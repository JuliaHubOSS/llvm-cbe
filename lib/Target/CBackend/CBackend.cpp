//===-- CBackend.cpp - Library for converting LLVM code to C --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This library converts LLVM code to C code, compilable by GCC and other C
// compilers.
//
//===----------------------------------------------------------------------===//

#include "CBackend.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/Host.h"
#include "llvm/Target/TargetLowering.h"
#include "llvm/Config/config.h"
#include <algorithm>
#include <cstdio>

#include <iostream>

//#include "Graph.h"
//#include "PHINodePass.h"

//Jackson Korba 9/29/14
#ifndef DEBUG_TYPE
#define DEBUG_TYPE ""
#endif
//End Modification

// Some ms header decided to define setjmp as _setjmp, undo this for this file
// since we don't need it
#ifdef setjmp
#undef setjmp
#endif
using namespace llvm;

extern "C" void LLVMInitializeCBackendTarget() {
  // Register the target.
  RegisterTargetMachine<CTargetMachine> X(TheCBackendTarget);
}

char CWriter::ID = 0;

// extra (invalid) Ops tags for tracking unary ops as a special case of the available binary ops
enum UnaryOps {
  BinaryNeg = Instruction::OtherOpsEnd + 1,
  BinaryNot,
};

/// isAddressExposed - Return true if the specified value's name needs to
/// have its address taken in order to get a C value of the correct type.
/// This happens for global variables, byval parameters, and direct allocas.
bool CWriter::isAddressExposed(Value *V) const {
  if (Argument *A = dyn_cast<Argument>(V))
    return ByValParams.count(A);
  return isa<GlobalVariable>(V) || isDirectAlloca(V);
}

// isInlinableInst - Attempt to inline instructions into their uses to build
// trees as much as possible.  To do this, we have to consistently decide
// what is acceptable to inline, so that variable declarations don't get
// printed and an extra copy of the expr is not emitted.
//
bool CWriter::isInlinableInst(Instruction &I) {
  // Always inline cmp instructions, even if they are shared by multiple
  // expressions.  GCC generates horrible code if we don't.
  if (isa<CmpInst>(I))
    return true;

  // Must be an expression, must be used exactly once.  If it is dead, we
  // emit it inline where it would go.
  if (I.getType() == Type::getVoidTy(I.getContext()) || !I.hasOneUse() ||
      isa<TerminatorInst>(I) || isa<CallInst>(I) || isa<PHINode>(I) ||
      isa<LoadInst>(I) || isa<VAArgInst>(I) || isa<InsertElementInst>(I) ||
      isa<InsertValueInst>(I))
    // Don't inline a load across a store or other bad things!
    return false;

  // Must not be used in inline asm, extractelement, or shufflevector.
  if (I.hasOneUse()) {
    Instruction &User = cast<Instruction>(*I.user_back());
    if (isInlineAsm(User) || isa<ExtractElementInst>(User) ||
        isa<ShuffleVectorInst>(User))
      return false;
  }

  // Only inline instruction it if it's use is in the same BB as the inst.
  return I.getParent() == cast<Instruction>(I.user_back())->getParent();
}

// isDirectAlloca - Define fixed sized allocas in the entry block as direct
// variables which are accessed with the & operator.  This causes GCC to
// generate significantly better code than to emit alloca calls directly.
//
AllocaInst *CWriter::isDirectAlloca(Value *V) const {
  AllocaInst *AI = dyn_cast<AllocaInst>(V);
  if (!AI) return 0;
  if (AI->isArrayAllocation())
    return 0;   // FIXME: we can also inline fixed size array allocas!
  if (AI->getParent() != &AI->getParent()->getParent()->getEntryBlock())
    return 0;
  return AI;
}

// isInlineAsm - Check if the instruction is a call to an inline asm chunk.
bool CWriter::isInlineAsm(Instruction& I) {
  if (CallInst *CI = dyn_cast<CallInst>(&I))
    return isa<InlineAsm>(CI->getCalledValue());
  return false;
}

bool CWriter::runOnFunction(Function &F) {
 // Do not codegen any 'available_externally' functions at all, they have
 // definitions outside the translation unit.
 if (F.hasAvailableExternallyLinkage())
   return false;

  LI = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();

  // Get rid of intrinsics we can't handle.
  lowerIntrinsics(F);

  // Output all floating point constants that cannot be printed accurately.
  printFloatingPointConstants(F);

  printFunction(F);

  LI = NULL;

  return true; // may have lowered an IntrinsicCall
}

static std::string CBEMangle(const std::string &S) {
  std::string Result;

  for (unsigned i = 0, e = S.size(); i != e; ++i)
    if (isalnum(S[i]) || S[i] == '_') {
      Result += S[i];
    } else {
      Result += '_';
      Result += 'A'+(S[i]&15);
      Result += 'A'+((S[i]>>4)&15);
      Result += '_';
    }
  return Result;
}

raw_ostream &
CWriter::printTypeString(raw_ostream &Out, Type *Ty, bool isSigned) {
  if (Ty->isStructTy()) {
    return Out << "l_struct_" << CBEMangle(Ty->getStructName());
  }

  if (Ty->isPointerTy()) {
    Out << "p";
    return printTypeString(Out, Ty->getPointerElementType(), isSigned);
  }

  switch (Ty->getTypeID()) {
  case Type::VoidTyID:   return Out << "void";
  case Type::IntegerTyID: {
    unsigned NumBits = cast<IntegerType>(Ty)->getBitWidth();
    if (NumBits == 1)
      return Out << "bool";
    else {
      assert(NumBits <= 128 && "Bit widths > 128 not implemented yet");
      return Out << (isSigned?"i":"u") << NumBits;
    }
  }
  case Type::FloatTyID:    return Out << "f32";
  case Type::DoubleTyID:   return Out << "f64";
  case Type::X86_FP80TyID: return Out << "f80";
  case Type::PPC_FP128TyID:
  case Type::FP128TyID:    return Out << "f128";

  case Type::X86_MMXTyID:
    return Out << (isSigned ? "i32y2" : "u32y2");

  case Type::VectorTyID: {
    VectorType *VTy = cast<VectorType>(Ty);
    printTypeString(Out, VTy->getElementType(), isSigned);
    return Out << "x" << VTy->getNumElements();
  }

  case Type::ArrayTyID: {
    ArrayType *ATy = cast<ArrayType>(Ty);
    printTypeString(Out, ATy->getElementType(), isSigned);
    return Out << "a" << ATy->getNumElements();
  }

  default:
#ifndef NDEBUG
    errs() << "Unknown primitive type: " << *Ty << "\n";
#endif
    llvm_unreachable(0);
  }
}

std::string CWriter::getStructName(StructType *ST) {
  if (!ST->isLiteral() && !ST->getName().empty())
    return "struct l_struct_" + CBEMangle(ST->getName().str());

  unsigned &id = UnnamedStructIDs[ST];
  if (id == 0)
      id = ++NextAnonStructNumber;
  return "struct l_unnamed_" + utostr(id);
}

std::string CWriter::getFunctionName(FunctionType *FT, AttributeSet PAL) {
  unsigned &id = UnnamedFunctionIDs[std::pair<FunctionType*, AttributeSet>(FT, PAL)];
  if (id == 0)
      id = ++NextFunctionNumber;
  return "l_fptr_" + utostr(id);
}

std::string CWriter::getArrayName(ArrayType *AT) {
    std::string astr;
    raw_string_ostream ArrayInnards(astr);
    // Arrays are wrapped in structs to allow them to have normal
    // value semantics (avoiding the array "decay").
    printTypeName(ArrayInnards, AT->getElementType(), false);
    return "struct l_array_" + utostr(AT->getNumElements()) + '_' + CBEMangle(ArrayInnards.str());
}

static const std::string getCmpPredicateName(CmpInst::Predicate P) {
  switch (P) {
  case FCmpInst::FCMP_FALSE: return "0";
  case FCmpInst::FCMP_OEQ: return "oeq";
  case FCmpInst::FCMP_OGT: return "ogt";
  case FCmpInst::FCMP_OGE: return "oge";
  case FCmpInst::FCMP_OLT: return "olt";
  case FCmpInst::FCMP_OLE: return "ole";
  case FCmpInst::FCMP_ONE: return "one";
  case FCmpInst::FCMP_ORD: return "ord";
  case FCmpInst::FCMP_UNO: return "uno";
  case FCmpInst::FCMP_UEQ: return "ueq";
  case FCmpInst::FCMP_UGT: return "ugt";
  case FCmpInst::FCMP_UGE: return "uge";
  case FCmpInst::FCMP_ULT: return "ult";
  case FCmpInst::FCMP_ULE: return "ule";
  case FCmpInst::FCMP_UNE: return "une";
  case FCmpInst::FCMP_TRUE: return "1";
  case ICmpInst::ICMP_EQ:  return "eq";
  case ICmpInst::ICMP_NE:  return "ne";
  case ICmpInst::ICMP_ULE: return "ule";
  case ICmpInst::ICMP_SLE: return "sle";
  case ICmpInst::ICMP_UGE: return "uge";
  case ICmpInst::ICMP_SGE: return "sge";
  case ICmpInst::ICMP_ULT: return "ult";
  case ICmpInst::ICMP_SLT: return "slt";
  case ICmpInst::ICMP_UGT: return "ugt";
  case ICmpInst::ICMP_SGT: return "sgt";
  default:
#ifndef NDEBUG
    errs() << "Invalid icmp predicate!" << P;
#endif
    llvm_unreachable(0);
  }
}


raw_ostream &
CWriter::printSimpleType(raw_ostream &Out, Type *Ty, bool isSigned) {
  assert((Ty->isSingleValueType() || Ty->isVoidTy()) &&
         "Invalid type for printSimpleType");
  switch (Ty->getTypeID()) {
  case Type::VoidTyID:   return Out << "void";
  case Type::IntegerTyID: {
    unsigned NumBits = cast<IntegerType>(Ty)->getBitWidth();
    if (NumBits == 1)
      return Out << " bool";
    else if (NumBits <= 8)
      return Out << (isSigned?"signed":"unsigned") << " char";
    else if (NumBits <= 16)
      return Out << (isSigned?"signed":"unsigned") << " short";
    else if (NumBits <= 32)
      return Out << (isSigned?"signed":"unsigned") << " int";
    else if (NumBits <= 64)
      return Out << (isSigned?"signed":"unsigned") << " long long";
    else {
      assert(NumBits <= 128 && "Bit widths > 128 not implemented yet");
      return Out << (isSigned?"llvmInt128":"llvmUInt128");
    }
  }
  case Type::FloatTyID:  return Out << "float";
  case Type::DoubleTyID: return Out << "double";
  // Lacking emulation of FP80 on PPC, etc., we assume whichever of these is
  // present matches host 'long double'.
  case Type::X86_FP80TyID:
  case Type::PPC_FP128TyID:
  case Type::FP128TyID:  return Out << "long double";

  case Type::X86_MMXTyID:
    return printSimpleType(Out, Type::getInt32Ty(Ty->getContext()), isSigned) << " __attribute__((vector_size(64)))";

  case Type::VectorTyID: {
    VectorType *VTy = cast<VectorType>(Ty);
    printSimpleType(Out, VTy->getElementType(), isSigned);
    return Out << " __attribute__((vector_size(" << utostr(TD->getTypeAllocSize(VTy)) << " )))";
  }

  default:
#ifndef NDEBUG
    errs() << "Unknown primitive type: " << *Ty << "\n";
#endif
    llvm_unreachable(0);
  }
}

// Pass the Type* and the variable name and this prints out the variable
// declaration.
//
raw_ostream &CWriter::printTypeName(raw_ostream &Out, Type *Ty, bool isSigned, AttributeSet PAL) {
  if (Ty->isSingleValueType() || Ty->isVoidTy()) {
    if (!Ty->isPointerTy())
      return printSimpleType(Out, Ty, isSigned);
  }

  switch (Ty->getTypeID()) {
  case Type::FunctionTyID: {
    FunctionType *FTy = cast<FunctionType>(Ty);
    return Out << getFunctionName(FTy, PAL);
  }
  case Type::StructTyID: {
    StructType *STy = cast<StructType>(Ty);
    StructTypes.insert(STy);
    return Out << getStructName(STy);
  }

  case Type::PointerTyID: {
    PointerType *PTy = cast<PointerType>(Ty);
    return printTypeName(Out, PTy->getElementType(), false) << '*';
  }

  case Type::ArrayTyID: {
    ArrayType *ATy = cast<ArrayType>(Ty);
    ArrayTypes.insert(ATy);
    return Out << getArrayName(ATy);
  }

  default:
#ifndef NDEBUG
    errs() << "Unexpected type: " << *Ty << "\n";
#endif
    llvm_unreachable(0);
  }
}


raw_ostream &CWriter::printStructDeclaration(raw_ostream &Out, StructType *STy) {
    Out << getStructName(STy) << " {\n";
    unsigned Idx = 0;
    for (StructType::element_iterator I = STy->element_begin(),
           E = STy->element_end(); I != E; ++I) {
      Out << "  ";
      printTypeName(Out, *I, false) << " field" << utostr(Idx++);
      Out << ";\n";
    }
    Out << '}';
    if (STy->isPacked())
      Out << " __attribute__ ((packed))";
    return Out << ";\n";
}

raw_ostream &CWriter::printFunctionDeclaration(raw_ostream &Out, FunctionType *Ty,
                                AttributeSet PAL, const std::string &Name) {
    Out << "typedef ";
    printFunctionProto(Out, Ty, PAL, "(" + Name + ")");
    return Out << ";\n";
}

raw_ostream &CWriter::printFunctionProto(raw_ostream &Out, FunctionType *FTy,
                                AttributeSet PAL, const std::string &Name) {
    // Should this function actually return a struct by-value?
    bool isStructReturn = PAL.hasAttribute(1, Attribute::StructRet) ||
                          PAL.hasAttribute(2, Attribute::StructRet);
    // Get the return type for the function.
    Type *RetTy;
    if (!isStructReturn)
      RetTy = FTy->getReturnType();
    else {
      // If this is a struct-return function, print the struct-return type.
      RetTy = cast<PointerType>(FTy->getParamType(0))->getElementType();
    }
    printTypeName(Out, RetTy,
      /*isSigned=*/PAL.hasAttribute(AttributeSet::ReturnIndex, Attribute::SExt));

    Out << ' ' << Name << '(';
    unsigned Idx = 1;
    bool PrintedArg = false;
    FunctionType::param_iterator I = FTy->param_begin(), E = FTy->param_end();
    // If this is a struct-return function, don't print the hidden
    // struct-return argument.
    if (isStructReturn) {
      assert(I != E && "Invalid struct return function!");
      ++I;
      ++Idx;
    }
    for (; I != E; ++I) {
      Type *ArgTy = *I;
      if (PAL.hasAttribute(Idx, Attribute::ByVal)) {
        assert(ArgTy->isPointerTy());
        ArgTy = cast<PointerType>(ArgTy)->getElementType();
      }
      if (PrintedArg)
        Out << ", ";
      printTypeName(Out, ArgTy,
        /*isSigned=*/PAL.hasAttribute(Idx, Attribute::SExt));
      PrintedArg = true;
      ++Idx;
    }

    if (FTy->isVarArg()) {
      if (!PrintedArg)
        Out << "int"; //dummy argument for empty vaarg functs
      Out << ", ...";
    } else if (!PrintedArg) {
      Out << "void";
    }
    Out << ")";
    return Out;
}

raw_ostream &CWriter::printArrayDeclaration(raw_ostream &Out, ArrayType *Ty) {
    ArrayType *ATy = cast<ArrayType>(Ty);
    // Arrays are wrapped in structs to allow them to have normal
    // value semantics (avoiding the array "decay").
    Out << getArrayName(Ty) << " {\n  ";
    printTypeName(Out, ATy->getElementType());
    Out << " array[" << utostr(ATy->getNumElements()) << "];\n};\n";
    return Out;
}

void CWriter::printConstantArray(ConstantArray *CPA) {
  Out << "{ ";
  printConstant(cast<Constant>(CPA->getOperand(0)), true);
  for (unsigned i = 1, e = CPA->getNumOperands(); i != e; ++i) {
    Out << ", ";
    printConstant(cast<Constant>(CPA->getOperand(i)), true);
  }
  Out << " }";
}

void CWriter::printConstantVector(ConstantVector *CP) {
  Out << "{ ";
  printConstant(cast<Constant>(CP->getOperand(0)), true);
  for (unsigned i = 1, e = CP->getNumOperands(); i != e; ++i) {
    Out << ", ";
    printConstant(cast<Constant>(CP->getOperand(i)), true);
  }
  Out << " }";
}

void CWriter::printConstantDataSequential(ConstantDataSequential *CDS) {
  // As a special case, print the array as a string if it is an array of
  // ubytes or an array of sbytes with positive values.
  //
  if (CDS->isCString()) {
    Out << '\"';
    // Keep track of whether the last number was a hexadecimal escape.
    bool LastWasHex = false;

    StringRef Bytes = CDS->getAsCString();

    // Do not include the last character, which we know is null
    for (unsigned i = 0, e = Bytes.size(); i != e; ++i) {
      unsigned char C = Bytes[i];

      // Print it out literally if it is a printable character.  The only thing
      // to be careful about is when the last letter output was a hex escape
      // code, in which case we have to be careful not to print out hex digits
      // explicitly (the C compiler thinks it is a continuation of the previous
      // character, sheesh...)
      //
      if (isprint(C) && (!LastWasHex || !isxdigit(C))) {
        LastWasHex = false;
        if (C == '"' || C == '\\')
          Out << "\\" << (char)C;
        else
          Out << (char)C;
      } else {
        LastWasHex = false;
        switch (C) {
          case '\n': Out << "\\n"; break;
          case '\t': Out << "\\t"; break;
          case '\r': Out << "\\r"; break;
          case '\v': Out << "\\v"; break;
          case '\a': Out << "\\a"; break;
          case '\"': Out << "\\\""; break;
          case '\'': Out << "\\\'"; break;
          default:
            Out << "\\x";
            Out << (char)(( C/16  < 10) ? ( C/16 +'0') : ( C/16 -10+'A'));
            Out << (char)(((C&15) < 10) ? ((C&15)+'0') : ((C&15)-10+'A'));
            LastWasHex = true;
            break;
        }
      }
    }
    Out << '\"';
  } else {
    Out << "{ ";
    printConstant(CDS->getElementAsConstant(0), true);
    for (unsigned i = 1, e = CDS->getNumElements(); i != e; ++i) {
      Out << ", ";
      printConstant(CDS->getElementAsConstant(i), true);
    }
    Out << " }";
  }
}


// isFPCSafeToPrint - Returns true if we may assume that CFP may be written out
// textually as a double (rather than as a reference to a stack-allocated
// variable). We decide this by converting CFP to a string and back into a
// double, and then checking whether the conversion results in a bit-equal
// double to the original value of CFP. This depends on us and the target C
// compiler agreeing on the conversion process (which is pretty likely since we
// only deal in IEEE FP).
//

// TODO copied from CppBackend, new code should use raw_ostream
static inline std::string ftostr(const APFloat& V) {
  std::string Buf;
  if (&V.getSemantics() == &APFloat::IEEEdouble) {
    raw_string_ostream(Buf) << V.convertToDouble();
    return Buf;
  } else if (&V.getSemantics() == &APFloat::IEEEsingle) {
    raw_string_ostream(Buf) << (double)V.convertToFloat();
    return Buf;
  }
  return "<unknown format in ftostr>"; // error
}

static bool isFPCSafeToPrint(const ConstantFP *CFP) {
  bool ignored;
  // Do long doubles in hex for now.
  if (CFP->getType() != Type::getFloatTy(CFP->getContext()) &&
      CFP->getType() != Type::getDoubleTy(CFP->getContext()))
    return false;
  APFloat APF = APFloat(CFP->getValueAPF());  // copy
  if (CFP->getType() == Type::getFloatTy(CFP->getContext()))
    APF.convert(APFloat::IEEEdouble, APFloat::rmNearestTiesToEven, &ignored);
#if HAVE_PRINTF_A && ENABLE_CBE_PRINTF_A
  char Buffer[100];
  sprintf(Buffer, "%a", APF.convertToDouble());
  if (!strncmp(Buffer, "0x", 2) ||
      !strncmp(Buffer, "-0x", 3) ||
      !strncmp(Buffer, "+0x", 3))
    return APF.bitwiseIsEqual(APFloat(atof(Buffer)));
  return false;
#else
  std::string StrVal = ftostr(APF);

  while (StrVal[0] == ' ')
    StrVal.erase(StrVal.begin());

  // Check to make sure that the stringized number is not some string like "Inf"
  // or NaN.  Check that the string matches the "[-+]?[0-9]" regex.
  if ((StrVal[0] >= '0' && StrVal[0] <= '9') ||
      ((StrVal[0] == '-' || StrVal[0] == '+') &&
       (StrVal[1] >= '0' && StrVal[1] <= '9')))
    // Reparse stringized version!
    return APF.bitwiseIsEqual(APFloat(atof(StrVal.c_str())));
  return false;
#endif
}

/// Print out the casting for a cast operation. This does the double casting
/// necessary for conversion to the destination type, if necessary.
/// @brief Print a cast
void CWriter::printCast(unsigned opc, Type *SrcTy, Type *DstTy) {
  // Print the destination type cast
  switch (opc) {
    case Instruction::UIToFP:
    case Instruction::SIToFP:
    case Instruction::IntToPtr:
    case Instruction::Trunc:
    case Instruction::BitCast:
    case Instruction::FPExt:
    case Instruction::FPTrunc: // For these the DstTy sign doesn't matter
      Out << '(';
      printTypeName(Out, DstTy);
      Out << ')';
      break;
    case Instruction::ZExt:
    case Instruction::PtrToInt:
    case Instruction::FPToUI: // For these, make sure we get an unsigned dest
      Out << '(';
      printSimpleType(Out, DstTy, false);
      Out << ')';
      break;
    case Instruction::SExt:
    case Instruction::FPToSI: // For these, make sure we get a signed dest
      Out << '(';
      printSimpleType(Out, DstTy, true);
      Out << ')';
      break;
    default:
      llvm_unreachable("Invalid cast opcode");
  }

  // Print the source type cast
  switch (opc) {
    case Instruction::UIToFP:
    case Instruction::ZExt:
      Out << '(';
      printSimpleType(Out, SrcTy, false);
      Out << ')';
      break;
    case Instruction::SIToFP:
    case Instruction::SExt:
      Out << '(';
      printSimpleType(Out, SrcTy, true);
      Out << ')';
      break;
    case Instruction::IntToPtr:
    case Instruction::PtrToInt:
      // Avoid "cast to pointer from integer of different size" warnings
      Out << "(uintptr_t)";
      break;
    case Instruction::Trunc:
    case Instruction::BitCast:
    case Instruction::FPExt:
    case Instruction::FPTrunc:
    case Instruction::FPToSI:
    case Instruction::FPToUI:
      break; // These don't need a source cast.
    default:
      llvm_unreachable("Invalid cast opcode");
  }
}

// printConstant - The LLVM Constant to C Constant converter.
// Static context means that it is being used in a context where the
// type cast is implicit, such as the RHS of a `type var = RHS;` expression
// or inside a struct initializer expression
void CWriter::printConstant(Constant *CPV, bool Static) {
  if (ConstantExpr *CE = dyn_cast<ConstantExpr>(CPV)) {
    switch (CE->getOpcode()) {
    case Instruction::Trunc:
    case Instruction::ZExt:
    case Instruction::SExt:
    case Instruction::FPTrunc:
    case Instruction::FPExt:
    case Instruction::UIToFP:
    case Instruction::SIToFP:
    case Instruction::FPToUI:
    case Instruction::FPToSI:
    case Instruction::PtrToInt:
    case Instruction::IntToPtr:
    case Instruction::BitCast:
      Out << "(";
      printCast(CE->getOpcode(), CE->getOperand(0)->getType(), CE->getType());
      if (CE->getOpcode() == Instruction::SExt &&
          CE->getOperand(0)->getType() == Type::getInt1Ty(CPV->getContext())) {
        // Make sure we really sext from bool here by subtracting from 0
        Out << "0-";
      }
      printConstant(CE->getOperand(0), false);
      if (CE->getType() == Type::getInt1Ty(CPV->getContext()) &&
          (CE->getOpcode() == Instruction::Trunc ||
           CE->getOpcode() == Instruction::FPToUI ||
           CE->getOpcode() == Instruction::FPToSI ||
           CE->getOpcode() == Instruction::PtrToInt)) {
        // Make sure we really truncate to bool here by anding with 1
        Out << "&1u";
      }
      Out << ')';
      return;

    case Instruction::GetElementPtr:
      Out << "(";
      printGEPExpression(CE->getOperand(0), gep_type_begin(CPV), gep_type_end(CPV));
      Out << ")";
      return;
    case Instruction::Select:
      Out << '(';
      printConstant(CE->getOperand(0), false);
      Out << '?';
      printConstant(CE->getOperand(1), false);
      Out << ':';
      printConstant(CE->getOperand(2), false);
      Out << ')';
      return;
    case Instruction::Add:
    case Instruction::FAdd:
    case Instruction::Sub:
    case Instruction::FSub:
    case Instruction::Mul:
    case Instruction::FMul:
    case Instruction::SDiv:
    case Instruction::UDiv:
    case Instruction::FDiv:
    case Instruction::URem:
    case Instruction::SRem:
    case Instruction::FRem:
    case Instruction::And:
    case Instruction::Or:
    case Instruction::Xor:
    case Instruction::ICmp:
    case Instruction::Shl:
    case Instruction::LShr:
    case Instruction::AShr:
    {
      Out << '(';
      bool NeedsClosingParens = printConstExprCast(CE);
      printConstantWithCast(CE->getOperand(0), CE->getOpcode());
      switch (CE->getOpcode()) {
      case Instruction::Add:
      case Instruction::FAdd: Out << " + "; break;
      case Instruction::Sub:
      case Instruction::FSub: Out << " - "; break;
      case Instruction::Mul:
      case Instruction::FMul: Out << " * "; break;
      case Instruction::URem:
      case Instruction::SRem:
      case Instruction::FRem: Out << " % "; break;
      case Instruction::UDiv:
      case Instruction::SDiv:
      case Instruction::FDiv: Out << " / "; break;
      case Instruction::And: Out << " & "; break;
      case Instruction::Or:  Out << " | "; break;
      case Instruction::Xor: Out << " ^ "; break;
      case Instruction::Shl: Out << " << "; break;
      case Instruction::LShr:
      case Instruction::AShr: Out << " >> "; break;
      case Instruction::ICmp:
        switch (CE->getPredicate()) {
          case ICmpInst::ICMP_EQ: Out << " == "; break;
          case ICmpInst::ICMP_NE: Out << " != "; break;
          case ICmpInst::ICMP_SLT:
          case ICmpInst::ICMP_ULT: Out << " < "; break;
          case ICmpInst::ICMP_SLE:
          case ICmpInst::ICMP_ULE: Out << " <= "; break;
          case ICmpInst::ICMP_SGT:
          case ICmpInst::ICMP_UGT: Out << " > "; break;
          case ICmpInst::ICMP_SGE:
          case ICmpInst::ICMP_UGE: Out << " >= "; break;
          default: llvm_unreachable("Illegal ICmp predicate");
        }
        break;
      default: llvm_unreachable("Illegal opcode here!");
      }
      printConstantWithCast(CE->getOperand(1), CE->getOpcode());
      if (NeedsClosingParens)
        Out << "))";
      Out << ')';
      return;
    }
    case Instruction::FCmp: {
      Out << '(';
      bool NeedsClosingParens = printConstExprCast(CE);
      if (CE->getPredicate() == FCmpInst::FCMP_FALSE)
        Out << "0";
      else if (CE->getPredicate() == FCmpInst::FCMP_TRUE)
        Out << "1";
      else {
        Out << "llvm_fcmp_" << getCmpPredicateName((CmpInst::Predicate)CE->getPredicate()) << "(";
        printConstantWithCast(CE->getOperand(0), CE->getOpcode());
        Out << ", ";
        printConstantWithCast(CE->getOperand(1), CE->getOpcode());
        Out << ")";
      }
      if (NeedsClosingParens)
        Out << "))";
      Out << ')';
      return;
    }
    default:
#ifndef NDEBUG
      errs() << "CWriter Error: Unhandled constant expression: "
           << *CE << "\n";
#endif
      llvm_unreachable(0);
    }
  } else if (isa<UndefValue>(CPV) && CPV->getType()->isSingleValueType()) {
    Out << "((";
    printTypeName(Out, CPV->getType()); // sign doesn't matter
    Out << ")/*UNDEF*/";
    if (!CPV->getType()->isVectorTy()) {
      Out << "0)";
    } else {
      Out << "{})";
    }
    return;
  }

  if (ConstantInt *CI = dyn_cast<ConstantInt>(CPV)) {
    Type* Ty = CI->getType();
    if (Ty == Type::getInt1Ty(CPV->getContext())) {
      Out << (CI->getZExtValue() ? '1' : '0');
    } else if (Ty->getPrimitiveSizeInBits() < 32 && !Static) {
      Out << "((";
      printSimpleType(Out, Ty, false) << ')';
      if (CI->isMinValue(true))
        Out << CI->getZExtValue() << 'u';
      else
        Out << CI->getSExtValue();
      Out << ')';
    }
    else if (Ty->getPrimitiveSizeInBits() <= 32)
      Out << CI->getZExtValue() << 'u';
    else if (Ty->getPrimitiveSizeInBits() <= 64)
      Out << CI->getZExtValue() << "ull";
    else if (Ty->getPrimitiveSizeInBits() <= 128) {
      const APInt &V = CI->getValue();
      const APInt &Vlo = V.getLoBits(64);
      const APInt &Vhi = V.getHiBits(64);
      Out << '(';
      if (Vhi != 0) {
          Out << "(((uint128_t)" << Vhi.getZExtValue() << "ull) << 64) | ";
      }
      Out << "(uint128_t)" << Vlo.getZExtValue() << "ull";
      Out << ')';
    }
    return;
  }

  switch (CPV->getType()->getTypeID()) {
  case Type::FloatTyID:
  case Type::DoubleTyID:
  case Type::X86_FP80TyID:
  case Type::PPC_FP128TyID:
  case Type::FP128TyID: {
    ConstantFP *FPC = cast<ConstantFP>(CPV);
    std::map<const ConstantFP*, unsigned>::iterator I = FPConstantMap.find(FPC);
    if (I != FPConstantMap.end()) {
      // Because of FP precision problems we must load from a stack allocated
      // value that holds the value in hex.
      Out << "(*(" << (FPC->getType() == Type::getFloatTy(CPV->getContext()) ?
                       "float" :
                       FPC->getType() == Type::getDoubleTy(CPV->getContext()) ?
                       "double" :
                       "long double")
          << "*)&FPConstant" << I->second << ')';
    } else {
      double V;
      if (FPC->getType() == Type::getFloatTy(CPV->getContext()))
        V = FPC->getValueAPF().convertToFloat();
      else if (FPC->getType() == Type::getDoubleTy(CPV->getContext()))
        V = FPC->getValueAPF().convertToDouble();
      else {
        // Long double.  Convert the number to double, discarding precision.
        // This is not awesome, but it at least makes the CBE output somewhat
        // useful.
        APFloat Tmp = FPC->getValueAPF();
        bool LosesInfo;
        Tmp.convert(APFloat::IEEEdouble, APFloat::rmTowardZero, &LosesInfo);
        V = Tmp.convertToDouble();
      }

      if (isnan(V)) {
        // The value is NaN

        // FIXME the actual NaN bits should be emitted.
        // The prefix for a quiet NaN is 0x7FF8. For a signalling NaN,
        // it's 0x7ff4.
        const unsigned long QuietNaN = 0x7ff8UL;
        //const unsigned long SignalNaN = 0x7ff4UL;

        // We need to grab the first part of the FP #
        char Buffer[100];

        uint64_t ll = DoubleToBits(V);
        sprintf(Buffer, "0x%llx", static_cast<long long>(ll));

        std::string Num(&Buffer[0], &Buffer[6]);
        unsigned long Val = strtoul(Num.c_str(), 0, 16);

        if (FPC->getType() == Type::getFloatTy(FPC->getContext()))
          Out << "LLVM_NAN" << (Val == QuietNaN ? "" : "S") << "F(\""
              << Buffer << "\") /*nan*/ ";
        else
          Out << "LLVM_NAN" << (Val == QuietNaN ? "" : "S") << "(\""
              << Buffer << "\") /*nan*/ ";
      } else if (isinf(V)) {
        // The value is Inf
        if (V < 0) Out << '-';
        Out << "LLVM_INF" <<
            (FPC->getType() == Type::getFloatTy(FPC->getContext()) ? "F" : "")
            << " /*inf*/ ";
      } else {
        std::string Num;
#if HAVE_PRINTF_A && ENABLE_CBE_PRINTF_A
        // Print out the constant as a floating point number.
        char Buffer[100];
        sprintf(Buffer, "%a", V);
        Num = Buffer;
#else
        Num = ftostr(FPC->getValueAPF());
#endif
       Out << Num;
      }
    }
    break;
  }

  case Type::ArrayTyID:
    // Use C99 compound expression literal initializer syntax.
    if (!Static) {
      Out << "(";
      printTypeName(Out, CPV->getType());
      Out << ")";
    }
    Out << "{ "; // Arrays are wrapped in struct types.
    if (ConstantArray *CA = dyn_cast<ConstantArray>(CPV)) {
      printConstantArray(CA);
    } else if (ConstantDataSequential *CDS =
                 dyn_cast<ConstantDataSequential>(CPV)) {
      printConstantDataSequential(CDS);
    } else {
      assert(isa<ConstantAggregateZero>(CPV) || isa<UndefValue>(CPV));
      ArrayType *AT = cast<ArrayType>(CPV->getType());
      Out << '{';
      if (AT->getNumElements()) {
        Out << ' ';
        Constant *CZ = Constant::getNullValue(AT->getElementType());
        printConstant(CZ, true);
        for (unsigned i = 1, e = AT->getNumElements(); i != e; ++i) {
          Out << ", ";
          printConstant(CZ, true);
        }
      }
      Out << " }";
    }
    Out << " }"; // Arrays are wrapped in struct types.
    break;

  case Type::VectorTyID:
    // Use C99 compound expression literal initializer syntax.
    if (!Static) {
      Out << "(";
      printTypeName(Out, CPV->getType());
      Out << ")";
    }
    if (ConstantVector *CV = dyn_cast<ConstantVector>(CPV)) {
      printConstantVector(CV);
    } else if (ConstantDataSequential *CDS =
               dyn_cast<ConstantDataSequential>(CPV)) {
      printConstantDataSequential(CDS);
    } else {
      assert(isa<ConstantAggregateZero>(CPV) || isa<UndefValue>(CPV));
      VectorType *VT = cast<VectorType>(CPV->getType());
      Out << "{ ";
      Constant *CZ = Constant::getNullValue(VT->getElementType());
      printConstant(CZ, true);
      for (unsigned i = 1, e = VT->getNumElements(); i != e; ++i) {
        Out << ", ";
        printConstant(CZ, true);
      }
      Out << " }";
    }
    break;

  case Type::StructTyID:
    // Use C99 compound expression literal initializer syntax.
    if (!Static) {
      Out << "(";
      printTypeName(Out, CPV->getType());
      Out << ")";
    }
    if (isa<ConstantAggregateZero>(CPV) || isa<UndefValue>(CPV)) {
      StructType *ST = cast<StructType>(CPV->getType());
      Out << '{';
      if (ST->getNumElements()) {
        Out << ' ';
        printConstant(Constant::getNullValue(ST->getElementType(0)), true);
        for (unsigned i = 1, e = ST->getNumElements(); i != e; ++i) {
          Out << ", ";
          printConstant(Constant::getNullValue(ST->getElementType(i)), true);
        }
      }
      Out << " }";
    } else {
      Out << '{';
      if (CPV->getNumOperands()) {
        Out << ' ';
        printConstant(cast<Constant>(CPV->getOperand(0)), true);
        for (unsigned i = 1, e = CPV->getNumOperands(); i != e; ++i) {
          Out << ", ";
          printConstant(cast<Constant>(CPV->getOperand(i)), true);
        }
      }
      Out << " }";
    }
    break;

  case Type::PointerTyID:
    if (isa<ConstantPointerNull>(CPV)) {
      Out << "((";
      printTypeName(Out, CPV->getType()); // sign doesn't matter
      Out << ")/*NULL*/0)";
      break;
    } else if (GlobalValue *GV = dyn_cast<GlobalValue>(CPV)) {
      writeOperand(GV);
      break;
    }
    // FALL THROUGH
  default:
#ifndef NDEBUG
    errs() << "Unknown constant type: " << *CPV << "\n";
#endif
    llvm_unreachable(0);
  }
}

// Some constant expressions need to be casted back to the original types
// because their operands were casted to the expected type. This function takes
// care of detecting that case and printing the cast for the ConstantExpr.
bool CWriter::printConstExprCast(ConstantExpr* CE) {
  bool NeedsExplicitCast = false;
  Type *Ty = CE->getOperand(0)->getType();
  bool TypeIsSigned = false;
  switch (CE->getOpcode()) {
  case Instruction::Add:
  case Instruction::Sub:
  case Instruction::Mul:
    // We need to cast integer arithmetic so that it is always performed
    // as unsigned, to avoid undefined behavior on overflow.
  case Instruction::LShr:
  case Instruction::URem:
  case Instruction::UDiv: NeedsExplicitCast = true; break;
  case Instruction::AShr:
  case Instruction::SRem:
  case Instruction::SDiv: NeedsExplicitCast = true; TypeIsSigned = true; break;
  case Instruction::SExt:
    Ty = CE->getType();
    NeedsExplicitCast = true;
    TypeIsSigned = true;
    break;
  case Instruction::ZExt:
  case Instruction::Trunc:
  case Instruction::FPTrunc:
  case Instruction::FPExt:
  case Instruction::UIToFP:
  case Instruction::SIToFP:
  case Instruction::FPToUI:
  case Instruction::FPToSI:
  case Instruction::PtrToInt:
  case Instruction::IntToPtr:
  case Instruction::BitCast:
    Ty = CE->getType();
    NeedsExplicitCast = true;
    break;
  default: break;
  }
  if (NeedsExplicitCast) {
    Out << "((";
      printTypeName(Out, Ty, TypeIsSigned); // not integer, sign doesn't matter
    Out << ")(";
  }
  return NeedsExplicitCast;
}

//  Print a constant assuming that it is the operand for a given Opcode. The
//  opcodes that care about sign need to cast their operands to the expected
//  type before the operation proceeds. This function does the casting.
void CWriter::printConstantWithCast(Constant* CPV, unsigned Opcode) {

  // Extract the operand's type, we'll need it.
  Type* OpTy = CPV->getType();

  // Indicate whether to do the cast or not.
  bool shouldCast = false;
  bool typeIsSigned = false;

  // Based on the Opcode for which this Constant is being written, determine
  // the new type to which the operand should be casted by setting the value
  // of OpTy. If we change OpTy, also set shouldCast to true so it gets
  // casted below.
  switch (Opcode) {
    default:
      // for most instructions, it doesn't matter
      break;
    case Instruction::Add:
    case Instruction::Sub:
    case Instruction::Mul:
      // We need to cast integer arithmetic so that it is always performed
      // as unsigned, to avoid undefined behavior on overflow.
    case Instruction::LShr:
    case Instruction::UDiv:
    case Instruction::URem:
      shouldCast = true;
      break;
    case Instruction::AShr:
    case Instruction::SDiv:
    case Instruction::SRem:
      shouldCast = true;
      typeIsSigned = true;
      break;
  }

  // Write out the casted constant if we should, otherwise just write the
  // operand.
  if (shouldCast) {
    Out << "((";
    printSimpleType(Out, OpTy, typeIsSigned);
    Out << ")";
    printConstant(CPV, false);
    Out << ")";
  } else
    printConstant(CPV, false);
}

std::string CWriter::GetValueName(Value *Operand) {

  // Resolve potential alias.
  if (GlobalAlias *GA = dyn_cast<GlobalAlias>(Operand)) {
    Operand = GA->getAliasee();
  }

  std::string Name = Operand->getName();
  if (Name.empty()) { // Assign unique names to local temporaries.
    unsigned &No = AnonValueNumbers[Operand];
    if (No == 0)
      No = ++NextAnonValueNumber;
    Name = "tmp__" + utostr(No);
  }

  // Mangle globals with the standard mangler interface for LLC compatibility.
  if (isa<GlobalValue>(Operand)) {
    return CBEMangle(Name);
  }

  std::string VarName;
  VarName.reserve(Name.capacity());

  for (std::string::iterator I = Name.begin(), E = Name.end();
       I != E; ++I) {
    unsigned char ch = *I;

    if (!((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') ||
          (ch >= '0' && ch <= '9') || ch == '_')) {
      char buffer[5];
      sprintf(buffer, "_%x_", ch);
      VarName += buffer;
    } else
      VarName += ch;
  }

  return "llvm_cbe_" + VarName;
}

/// writeInstComputationInline - Emit the computation for the specified
/// instruction inline, with no destination provided.
void CWriter::writeInstComputationInline(Instruction &I) {
  // C can't handle non-power-of-two integer types
  unsigned mask = 0;
  Type *Ty = I.getType();
  if (Ty->isIntegerTy()) {
     IntegerType *ITy = static_cast<IntegerType*>(Ty);
     if (!ITy->isPowerOf2ByteWidth())
       mask = ITy->getBitMask();
  }

  // If this is a non-trivial bool computation, make sure to truncate down to
  // a 1 bit value.  This is important because we want "add i1 x, y" to return
  // "0" when x and y are true, not "2" for example.
  // Also truncate odd bit sizes
  if (mask)
    Out << "((";

  visit(&I);

  if (mask)
    Out << ")&" << mask << ")";
}


void CWriter::writeOperandInternal(Value *Operand, bool Static) {
  if (Instruction *I = dyn_cast<Instruction>(Operand))
    // Should we inline this instruction to build a tree?
    if (isInlinableInst(*I) && !isDirectAlloca(I)) {
      Out << '(';
      writeInstComputationInline(*I);
      Out << ')';
      return;
    }

  Constant* CPV = dyn_cast<Constant>(Operand);

  if (CPV && !isa<GlobalValue>(CPV))
    printConstant(CPV, Static);
  else
    Out << GetValueName(Operand);
}

void CWriter::writeOperand(Value *Operand, bool Static) {
  bool isAddressImplicit = isAddressExposed(Operand);
  if (isAddressImplicit)
    Out << "(&";  // Global variables are referenced as their addresses by llvm

  writeOperandInternal(Operand, Static);

  if (isAddressImplicit)
    Out << ')';
}

/// writeOperandDeref - Print the result of dereferencing the specified
/// operand with '*'.  This is equivalent to printing '*' then using
/// writeOperand, but avoids excess syntax in some cases.
void CWriter::writeOperandDeref(Value *Operand) {
  if (isAddressExposed(Operand)) {
    // Already something with an address exposed.
    writeOperandInternal(Operand);
  } else {
    Out << "*(";
    writeOperand(Operand);
    Out << ")";
  }
}

// Some instructions need to have their result value casted back to the
// original types because their operands were casted to the expected type.
// This function takes care of detecting that case and printing the cast
// for the Instruction.
bool CWriter::writeInstructionCast(Instruction &I) {
  Type *Ty = I.getOperand(0)->getType();
  switch (I.getOpcode()) {
  case Instruction::Add:
  case Instruction::Sub:
  case Instruction::Mul:
    // We need to cast integer arithmetic so that it is always performed
    // as unsigned, to avoid undefined behavior on overflow.
  case Instruction::LShr:
  case Instruction::URem:
  case Instruction::UDiv:
    Out << "((";
    printSimpleType(Out, Ty, false);
    Out << ")(";
    return true;
  case Instruction::AShr:
  case Instruction::SRem:
  case Instruction::SDiv:
    Out << "((";
    printSimpleType(Out, Ty, true);
    Out << ")(";
    return true;
  default: break;
  }
  return false;
}

// Write the operand with a cast to another type based on the Opcode being used.
// This will be used in cases where an instruction has specific type
// requirements (usually signedness) for its operands.
void CWriter::opcodeNeedsCast(unsigned Opcode,
      // Indicate whether to do the cast or not.
      bool &shouldCast,
      // Indicate whether the cast should be to a signed type or not.
      bool &castIsSigned) {

  shouldCast = false;
  castIsSigned = false;

  // Based on the Opcode for which this Operand is being written, determine
  // the new type to which the operand should be casted by setting the value
  // of OpTy. If we change OpTy, also set shouldCast to true.
  switch (Opcode) {
    default:
      // for most instructions, it doesn't matter
      break;
    case Instruction::Add:
    case Instruction::Sub:
    case Instruction::Mul:
      // We need to cast integer arithmetic so that it is always performed
      // as unsigned, to avoid undefined behavior on overflow.
    case Instruction::LShr:
    case Instruction::UDiv:
    case Instruction::URem: // Cast to unsigned first
      shouldCast = true;
      castIsSigned = false;
      break;
    case Instruction::GetElementPtr:
    case Instruction::AShr:
    case Instruction::SDiv:
    case Instruction::SRem: // Cast to signed first
      shouldCast = true;
      castIsSigned = true;
      break;
  }
}

void CWriter::writeOperandWithCast(Value* Operand, unsigned Opcode) {
  // Write out the casted operand if we should, otherwise just write the
  // operand.

  // Extract the operand's type, we'll need it.
  bool shouldCast;
  bool castIsSigned;
  opcodeNeedsCast(Opcode, shouldCast, castIsSigned);

  Type* OpTy = Operand->getType();
  if (shouldCast) {
    Out << "((";
    printSimpleType(Out, OpTy, castIsSigned);
    Out << ")";
    writeOperand(Operand);
    Out << ")";
  } else
    writeOperand(Operand);
}

// Write the operand with a cast to another type based on the icmp predicate
// being used.
void CWriter::writeOperandWithCast(Value* Operand, ICmpInst &Cmp) {
  // This has to do a cast to ensure the operand has the right signedness.
  // Also, if the operand is a pointer, we make sure to cast to an integer when
  // doing the comparison both for signedness and so that the C compiler doesn't
  // optimize things like "p < NULL" to false (p may contain an integer value
  // f.e.).
  bool shouldCast = Cmp.isRelational();

  // Write out the casted operand if we should, otherwise just write the
  // operand.
  if (!shouldCast) {
    writeOperand(Operand);
    return;
  }

  // Should this be a signed comparison?  If so, convert to signed.
  bool castIsSigned = Cmp.isSigned();

  // If the operand was a pointer, convert to a large integer type.
  Type* OpTy = Operand->getType();
  if (OpTy->isPointerTy())
    OpTy = TD->getIntPtrType(Operand->getContext());

  Out << "((";
  printSimpleType(Out, OpTy, castIsSigned);
  Out << ")";
  writeOperand(Operand);
  Out << ")";
}

// generateCompilerSpecificCode - This is where we add conditional compilation
// directives to cater to specific compilers as need be.
//
static void generateCompilerSpecificCode(raw_ostream& Out,
                                         const DataLayout *TD) {
  // Alloca is hard to get, and we don't want to include stdlib.h here.
  Out << "/* get a declaration for alloca */\n"
      << "#if defined(__CYGWIN__) || defined(__MINGW32__)\n"
      << "#define  alloca(x) __builtin_alloca((x))\n"
      << "#define _alloca(x) __builtin_alloca((x))\n"
      << "#elif defined(__APPLE__)\n"
      << "extern void *__builtin_alloca(unsigned long);\n"
      << "#define alloca(x) __builtin_alloca(x)\n"
      << "#define longjmp _longjmp\n"
      << "#define setjmp _setjmp\n"
      << "#elif defined(__sun__)\n"
      << "#if defined(__sparcv9)\n"
      << "extern void *__builtin_alloca(unsigned long);\n"
      << "#else\n"
      << "extern void *__builtin_alloca(unsigned int);\n"
      << "#endif\n"
      << "#define alloca(x) __builtin_alloca(x)\n"
      << "#elif defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(__DragonFly__) || defined(__arm__)\n"
      << "#define alloca(x) __builtin_alloca(x)\n"
      << "#elif defined(_MSC_VER)\n"
      << "#define inline _inline\n"
      << "#define alloca(x) _alloca(x)\n"
      << "#else\n"
      << "#include <alloca.h>\n"
      << "#endif\n\n";

  // We output GCC specific attributes to preserve 'linkonce'ness on globals.
  // If we aren't being compiled with GCC, just drop these attributes.
  Out << "#ifndef __GNUC__  /* Can only support \"linkonce\" vars with GCC */\n"
      << "#define __attribute__(X)\n"
      << "#endif\n\n";

  // On Mac OS X, "external weak" is spelled "__attribute__((weak_import))".
  Out << "#if defined(__GNUC__) && defined(__APPLE_CC__)\n"
      << "#define __EXTERNAL_WEAK__ __attribute__((weak_import))\n"
      << "#elif defined(__GNUC__)\n"
      << "#define __EXTERNAL_WEAK__ __attribute__((weak))\n"
      << "#else\n"
      << "#define __EXTERNAL_WEAK__\n"
      << "#endif\n\n";

  // For now, turn off the weak linkage attribute on Mac OS X. (See above.)
  Out << "#if defined(__GNUC__) && defined(__APPLE_CC__)\n"
      << "#define __ATTRIBUTE_WEAK__\n"
      << "#elif defined(__GNUC__)\n"
      << "#define __ATTRIBUTE_WEAK__ __attribute__((weak))\n"
      << "#else\n"
      << "#define __ATTRIBUTE_WEAK__\n"
      << "#endif\n\n";

  // Add hidden visibility support. FIXME: APPLE_CC?
  Out << "#if defined(__GNUC__)\n"
      << "#define __HIDDEN__ __attribute__((visibility(\"hidden\")))\n"
      << "#endif\n\n";

  // Define NaN and Inf as GCC builtins if using GCC, as 0 otherwise
  // From the GCC documentation:
  //
  //   double __builtin_nan (const char *str)
  //
  // This is an implementation of the ISO C99 function nan.
  //
  // Since ISO C99 defines this function in terms of strtod, which we do
  // not implement, a description of the parsing is in order. The string is
  // parsed as by strtol; that is, the base is recognized by leading 0 or
  // 0x prefixes. The number parsed is placed in the significand such that
  // the least significant bit of the number is at the least significant
  // bit of the significand. The number is truncated to fit the significand
  // field provided. The significand is forced to be a quiet NaN.
  //
  // This function, if given a string literal, is evaluated early enough
  // that it is considered a compile-time constant.
  //
  //   float __builtin_nanf (const char *str)
  //
  // Similar to __builtin_nan, except the return type is float.
  //
  //   double __builtin_inf (void)
  //
  // Similar to __builtin_huge_val, except a warning is generated if the
  // target floating-point format does not support infinities. This
  // function is suitable for implementing the ISO C99 macro INFINITY.
  //
  //   float __builtin_inff (void)
  //
  // Similar to __builtin_inf, except the return type is float.
  Out << "#ifdef __GNUC__\n"
      << "#define LLVM_NAN(NanStr)   __builtin_nan(NanStr)   /* Double */\n"
      << "#define LLVM_NANF(NanStr)  __builtin_nanf(NanStr)  /* Float */\n"
      << "#define LLVM_NANS(NanStr)  __builtin_nans(NanStr)  /* Double */\n"
      << "#define LLVM_NANSF(NanStr) __builtin_nansf(NanStr) /* Float */\n"
      << "#define LLVM_INF           __builtin_inf()         /* Double */\n"
      << "#define LLVM_INFF          __builtin_inff()        /* Float */\n"
      << "#define LLVM_PREFETCH(addr,rw,locality) "
                              "__builtin_prefetch(addr,rw,locality)\n"
      << "#define __ATTRIBUTE_CTOR__ __attribute__((constructor))\n"
      << "#define __ATTRIBUTE_DTOR__ __attribute__((destructor))\n"
      << "#define LLVM_ASM           __asm__\n"
      << "#else\n"
      << "#define LLVM_NAN(NanStr)   ((double)0.0)           /* Double */\n"
      << "#define LLVM_NANF(NanStr)  0.0F                    /* Float */\n"
      << "#define LLVM_NANS(NanStr)  ((double)0.0)           /* Double */\n"
      << "#define LLVM_NANSF(NanStr) 0.0F                    /* Float */\n"
      << "#define LLVM_INF           ((double)0.0)           /* Double */\n"
      << "#define LLVM_INFF          0.0F                    /* Float */\n"
      << "#define LLVM_PREFETCH(addr,rw,locality)            /* PREFETCH */\n"
      << "#define __ATTRIBUTE_CTOR__\n"
      << "#define __ATTRIBUTE_DTOR__\n"
      << "#define LLVM_ASM(X)\n"
      << "#endif\n\n";

  Out << "#if __GNUC__ < 4 /* Old GCC's, or compilers not GCC */ \n"
      << "#define __builtin_stack_save() 0   /* not implemented */\n"
      << "#define __builtin_stack_restore(X) /* noop */\n"
      << "#endif\n\n";

  // Output typedefs for 128-bit integers. If these are needed with a
  // 32-bit target or with a C compiler that doesn't support mode(TI),
  // more drastic measures will be needed.
  Out << "#if __GNUC__ && __LP64__ /* 128-bit integer types */\n"
      << "typedef int __attribute__((mode(TI))) llvmInt128;\n"
      << "typedef unsigned __attribute__((mode(TI))) llvmUInt128;\n"
      << "#endif\n\n";

  // Output target-specific code that should be inserted into main.
  Out << "#define CODE_FOR_MAIN() /* Any target-specific code for main()*/\n";
}

/// FindStaticTors - Given a static ctor/dtor list, unpack its contents into
/// the StaticTors set.
static void FindStaticTors(GlobalVariable *GV, std::set<Function*> &StaticTors){
  ConstantArray *InitList = dyn_cast<ConstantArray>(GV->getInitializer());
  if (!InitList) return;

  for (unsigned i = 0, e = InitList->getNumOperands(); i != e; ++i)
    if (ConstantStruct *CS = dyn_cast<ConstantStruct>(InitList->getOperand(i))){
      if (CS->getNumOperands() != 2) return;  // Not array of 2-element structs.

      if (CS->getOperand(1)->isNullValue())
        return;  // Found a null terminator, exit printing.
      Constant *FP = CS->getOperand(1);
      if (ConstantExpr *CE = dyn_cast<ConstantExpr>(FP))
        if (CE->isCast())
          FP = CE->getOperand(0);
      if (Function *F = dyn_cast<Function>(FP))
        StaticTors.insert(F);
    }
}

enum SpecialGlobalClass {
  NotSpecial = 0,
  GlobalCtors, GlobalDtors,
  NotPrinted
};

/// getGlobalVariableClass - If this is a global that is specially recognized
/// by LLVM, return a code that indicates how we should handle it.
static SpecialGlobalClass getGlobalVariableClass(GlobalVariable *GV) {
  // If this is a global ctors/dtors list, handle it now.
  if (GV->hasAppendingLinkage() && GV->use_empty()) {
    if (GV->getName() == "llvm.global_ctors")
      return GlobalCtors;
    else if (GV->getName() == "llvm.global_dtors")
      return GlobalDtors;
  }

  // Otherwise, if it is other metadata, don't print it.  This catches things
  // like debug information.
  if (StringRef(GV->getSection()) == "llvm.metadata")
    return NotPrinted;

  return NotSpecial;
}

// PrintEscapedString - Print each character of the specified string, escaping
// it if it is not printable or if it is an escape char.
static void PrintEscapedString(const char *Str, unsigned Length,
                               raw_ostream &Out) {
  for (unsigned i = 0; i != Length; ++i) {
    unsigned char C = Str[i];
    if (isprint(C) && C != '\\' && C != '"')
      Out << C;
    else if (C == '\\')
      Out << "\\\\";
    else if (C == '\"')
      Out << "\\\"";
    else if (C == '\t')
      Out << "\\t";
    else
      Out << "\\x" << hexdigit(C >> 4) << hexdigit(C & 0x0F);
  }
}

// PrintEscapedString - Print each character of the specified string, escaping
// it if it is not printable or if it is an escape char.
static void PrintEscapedString(const std::string &Str, raw_ostream &Out) {
  PrintEscapedString(Str.c_str(), Str.size(), Out);
}

bool CWriter::doInitialization(Module &M) {
  TheModule = &M;

  TD = new DataLayout(&M);
  IL = new IntrinsicLowering(*TD);
  IL->AddPrototypes(M);

#if 0
  std::string Triple = TheModule->getTargetTriple();
  if (Triple.empty())
    Triple = llvm::sys::getDefaultTargetTriple();

  std::string E;
  if (const Target *Match = TargetRegistry::lookupTarget(Triple, E))
    TAsm = Match->createMCAsmInfo(Triple);
#endif
  TAsm = new CBEMCAsmInfo();
  MRI  = new MCRegisterInfo();
  TCtx = new MCContext(TAsm, MRI, NULL);
  return false;
}

bool CWriter::doFinalization(Module &M) {
  // Output all code to the file
  std::string methods = Out.str();
  _Out.clear();
  generateHeader(M);
  std::string header = Out.str();
  _Out.clear();
  FileOut << header << methods;

  // Free memory...
  delete IL;
  delete TD;
  delete TCtx;
  delete TAsm;
  delete MRI;
  delete MOFI;
  FPConstantMap.clear();
  ByValParams.clear();
  AnonValueNumbers.clear();
  UnnamedStructIDs.clear();
  UnnamedFunctionIDs.clear();
  StructTypes.clear();
  ArrayTypes.clear();
  SelectDeclTypes.clear();
  CmpDeclTypes.clear();
  InlineOpDeclTypes.clear();
  CastOpDeclTypes.clear();
  prototypesToGen.clear();

  // reset all state
  FPCounter = 0;
  OpaqueCounter = 0;
  NextAnonValueNumber = 0;
  NextAnonStructNumber = 0;
  NextFunctionNumber = 0;
  return true; // may have lowered an IntrinsicCall
}

void CWriter::generateHeader(Module &M) {
  // Keep track of which functions are static ctors/dtors so they can have
  // an attribute added to their prototypes.
  std::set<Function*> StaticCtors, StaticDtors;
  for (Module::global_iterator I = M.global_begin(), E = M.global_end();
       I != E; ++I) {
    switch (getGlobalVariableClass(I)) {
    default: break;
    case GlobalCtors:
      FindStaticTors(I, StaticCtors);
      break;
    case GlobalDtors:
      FindStaticTors(I, StaticDtors);
      break;
    }
  }

  // get declaration for alloca
  Out << "/* Provide Declarations */\n";
  Out << "#include <stdarg.h>\n";      // Varargs support
  Out << "#include <setjmp.h>\n";      // Unwind support
  Out << "#include <limits.h>\n";      // With overflow intrinsics support.
  Out << "#include <stdint.h>\n";      // Sized integer support
  Out << "#include <string.h>\n";      // built-in definitions of many c library functions
  Out << "#include <APInt-C.h>\n";     // Implementations of many llvm intrinsics
  generateCompilerSpecificCode(Out, TD);

  // Provide a definition for `bool' if not compiling with a C++ compiler.
  Out << "\n"
      << "#ifndef __cplusplus\ntypedef unsigned char bool;\n#endif\n"

      << "\n\n/* Support for floating point constants */\n"
      << "typedef unsigned long long ConstantDoubleTy;\n"
      << "typedef unsigned int        ConstantFloatTy;\n"
      << "typedef struct { unsigned long long f1; unsigned short f2; "
         "unsigned short pad[3]; } ConstantFP80Ty;\n"
      // This is used for both kinds of 128-bit long double; meaning differs.
      << "typedef struct { unsigned long long f1; unsigned long long f2; }"
         " ConstantFP128Ty;\n"
      << "typedef __uint128_t uint128_t;\n"
      << "typedef __int128_t int128_t;\n"
      << "\n\n/* Global Declarations */\n";

  // First output all the declarations for the program, because C requires
  // Functions & globals to be declared before they are used.
  //
  if (!M.getModuleInlineAsm().empty()) {
    Out << "\n/* Module asm statements */\n"
        << "asm(";

    // Split the string into lines, to make it easier to read the .ll file.
    std::string Asm = M.getModuleInlineAsm();
    size_t CurPos = 0;
    size_t NewLine = Asm.find_first_of('\n', CurPos);
    while (NewLine != std::string::npos) {
      // We found a newline, print the portion of the asm string from the
      // last newline up to this newline.
      Out << "\"";
      PrintEscapedString(std::string(Asm.begin()+CurPos, Asm.begin()+NewLine),
                         Out);
      Out << "\\n\"\n";
      CurPos = NewLine+1;
      NewLine = Asm.find_first_of('\n', CurPos);
    }
    Out << "\"";
    PrintEscapedString(std::string(Asm.begin()+CurPos, Asm.end()), Out);
    Out << "\");\n"
        << "/* End Module asm statements */\n";
  }

  // collect any remaining types
  raw_null_ostream NullOut;
  for (Module::global_iterator I = M.global_begin(), E = M.global_end();
       I != E; ++I) {
    // Ignore special globals, such as debug info.
    if (getGlobalVariableClass(I))
      continue;
    printTypeName(NullOut, I->getType()->getElementType(), false);
  }
  printModuleTypes(Out);

  // Global variable declarations...
  if (!M.global_empty()) {
    Out << "\n/* External Global Variable Declarations */\n";
    for (Module::global_iterator I = M.global_begin(), E = M.global_end();
         I != E; ++I) {
      if (!I->isDeclaration())
        continue;

      if (I->hasDLLImportStorageClass())
        Out << "__declspec(dllimport) ";
      else if (I->hasDLLExportStorageClass())
        Out << "__declspec(dllexport) ";

      if (I->hasExternalLinkage() || I->hasExternalWeakLinkage() ||
          I->hasCommonLinkage())
        Out << "extern ";
      else
        continue; // Internal Global

      // Thread Local Storage
      if (I->isThreadLocal())
        Out << "__thread ";

      printTypeName(Out, I->getType()->getElementType(), false) << ' ' << GetValueName(I);

      if (I->hasExternalWeakLinkage())
         Out << " __EXTERNAL_WEAK__";
      Out << ";\n";
    }
  }

  // Function declarations
  Out << "\n/* Function Declarations */\n";
  Out << "double fmod(double, double);\n";   // Support for FP rem
  Out << "float fmodf(float, float);\n";
  Out << "long double fmodl(long double, long double);\n";

  // Store the intrinsics which will be declared/defined below.
  SmallVector<Function*, 16> intrinsicsToDefine;

  for (Module::iterator I = M.begin(), E = M.end(); I != E; ++I) {
    // Don't print declarations for intrinsic functions.
    // Store the used intrinsics, which need to be explicitly defined.
    if (I->isIntrinsic()) {
      switch (I->getIntrinsicID()) {
        default:
          continue;
        case Intrinsic::uadd_with_overflow:
        case Intrinsic::sadd_with_overflow:
        case Intrinsic::usub_with_overflow:
        case Intrinsic::ssub_with_overflow:
        case Intrinsic::umul_with_overflow:
        case Intrinsic::smul_with_overflow:
        case Intrinsic::bswap:
        case Intrinsic::ceil:
        case Intrinsic::ctlz:
        case Intrinsic::ctpop:
        case Intrinsic::cttz:
        case Intrinsic::fabs:
        case Intrinsic::floor:
        case Intrinsic::fma:
        case Intrinsic::fmuladd:
        case Intrinsic::pow:
        case Intrinsic::rint:
        case Intrinsic::sqrt:
        case Intrinsic::trunc:
          intrinsicsToDefine.push_back(I);
          continue;
      }
    }

    // Skip a few functions that have already been defined in headers
    if (I->getName() == "setjmp" ||
        I->getName() == "longjmp" ||
        I->getName() == "_setjmp" ||
        I->getName() == "siglongjmp" ||
        I->getName() == "sigsetjmp" ||
        I->getName() == "memcmp" ||
        I->getName() == "memcpy")
      continue;

    if (I->hasDLLImportStorageClass())
      Out << "__declspec(dllimport) ";
    else if (I->hasDLLExportStorageClass())
      Out << "__declspec(dllexport) ";

    if (I->hasLocalLinkage())
      Out << "static ";
    if (I->hasExternalWeakLinkage())
      Out << "extern ";
    printFunctionProto(Out, I);
    if (I->hasWeakLinkage() || I->hasLinkOnceLinkage())
      Out << " __ATTRIBUTE_WEAK__";
    if (I->hasExternalWeakLinkage())
      Out << " __EXTERNAL_WEAK__";
    if (StaticCtors.count(I))
      Out << " __ATTRIBUTE_CTOR__";
    if (StaticDtors.count(I))
      Out << " __ATTRIBUTE_DTOR__";
    if (I->hasHiddenVisibility())
      Out << " __HIDDEN__";

    if (I->hasName() && I->getName()[0] == 1)
      Out << " LLVM_ASM(\"" << I->getName().substr(1) << "\")";

    Out << ";\n";
  }

  // Output the global variable definitions and contents...
  if (!M.global_empty()) {
    Out << "\n\n/* Global Variable Definitions and Initialization */\n";
    for (Module::global_iterator I = M.global_begin(), E = M.global_end();
         I != E; ++I) {
      if (!I->isDeclaration()) {
        // Ignore special globals, such as debug info.
        if (getGlobalVariableClass(I))
          continue;

        if (I->hasDLLImportStorageClass())
          Out << "__declspec(dllimport) ";
        else if (I->hasDLLExportStorageClass())
          Out << "__declspec(dllexport) ";

        if (I->hasLocalLinkage())
          Out << "static ";

        // Thread Local Storage
        if (I->isThreadLocal())
          Out << "__thread ";

        printTypeName(Out, I->getType()->getElementType(), false) << ' ' << GetValueName(I);
        if (I->hasLinkOnceLinkage())
          Out << " __attribute__((common))";
        else if (I->hasWeakLinkage())
          Out << " __ATTRIBUTE_WEAK__";
        else if (I->hasCommonLinkage())
          Out << " __ATTRIBUTE_WEAK__";

        if (I->hasHiddenVisibility())
          Out << " __HIDDEN__";

        // If the initializer is not null, emit the initializer.  If it is null,
        // we try to avoid emitting large amounts of zeros.  The problem with
        // this, however, occurs when the variable has weak linkage.  In this
        // case, the assembler will complain about the variable being both weak
        // and common, so we disable this optimization.
        // FIXME common linkage should avoid this problem.
        if (!I->getInitializer()->isNullValue()) {
          Out << " = " ;
          writeOperand(I->getInitializer(), true);
        } else if (I->hasWeakLinkage()) {
          // We have to specify an initializer, but it doesn't have to be
          // complete.  If the value is an aggregate, print out { 0 }, and let
          // the compiler figure out the rest of the zeros.
          Out << " = " ;
          if (I->getInitializer()->getType()->isStructTy() ||
              I->getInitializer()->getType()->isVectorTy()) {
            Out << "{ 0 }";
          } else if (I->getInitializer()->getType()->isArrayTy()) {
            // As with structs and vectors, but with an extra set of braces
            // because arrays are wrapped in structs.
            Out << "{ { 0 } }";
          } else {
            // Just print it out normally.
            writeOperand(I->getInitializer(), true);
          }
        }
        Out << ";\n";
      }
    }
  }

  // Alias declarations...
  if (!M.alias_empty()) {
    Out << "\n/* External Alias Declarations */\n";
    for (Module::alias_iterator I = M.alias_begin(), E = M.alias_end();
         I != E; ++I) {
      assert(!I->isDeclaration());
      if (I->hasLocalLinkage())
        continue; // Internal Global

      if (I->hasDLLImportStorageClass())
        Out << "__declspec(dllimport) ";
      else if (I->hasDLLExportStorageClass())
        Out << "__declspec(dllexport) ";

      // Thread Local Storage
      if (I->isThreadLocal())
        Out << "__thread ";

      printTypeName(Out, I->getType(), false) << ' ' << I->getName();

      if (I->hasExternalWeakLinkage())
         Out << " __EXTERNAL_WEAK__";
      Out << " = ";
      writeOperand(I->getAliasee(), true);
      Out << ";\n";
    }
  }

  Out << "\n\n/* LLVM Intrinsic Builtin Function Bodies */\n";

  // Emit some helper functions for dealing with FCMP instruction's
  // predicates
  Out << "static inline int llvm_fcmp_ord(double X, double Y) { ";
  Out << "return X == X && Y == Y; }\n";
  Out << "static inline int llvm_fcmp_uno(double X, double Y) { ";
  Out << "return X != X || Y != Y; }\n";
  Out << "static inline int llvm_fcmp_ueq(double X, double Y) { ";
  Out << "return X == Y || llvm_fcmp_uno(X, Y); }\n";
  Out << "static inline int llvm_fcmp_une(double X, double Y) { ";
  Out << "return X != Y; }\n";
  Out << "static inline int llvm_fcmp_ult(double X, double Y) { ";
  Out << "return X <  Y || llvm_fcmp_uno(X, Y); }\n";
  Out << "static inline int llvm_fcmp_ugt(double X, double Y) { ";
  Out << "return X >  Y || llvm_fcmp_uno(X, Y); }\n";
  Out << "static inline int llvm_fcmp_ule(double X, double Y) { ";
  Out << "return X <= Y || llvm_fcmp_uno(X, Y); }\n";
  Out << "static inline int llvm_fcmp_uge(double X, double Y) { ";
  Out << "return X >= Y || llvm_fcmp_uno(X, Y); }\n";
  Out << "static inline int llvm_fcmp_oeq(double X, double Y) { ";
  Out << "return X == Y ; }\n";
  Out << "static inline int llvm_fcmp_one(double X, double Y) { ";
  Out << "return X != Y && llvm_fcmp_ord(X, Y); }\n";
  Out << "static inline int llvm_fcmp_olt(double X, double Y) { ";
  Out << "return X <  Y ; }\n";
  Out << "static inline int llvm_fcmp_ogt(double X, double Y) { ";
  Out << "return X >  Y ; }\n";
  Out << "static inline int llvm_fcmp_ole(double X, double Y) { ";
  Out << "return X <= Y ; }\n";
  Out << "static inline int llvm_fcmp_oge(double X, double Y) { ";
  Out << "return X >= Y ; }\n";
  Out << "static inline int llvm_fcmp_0(double X, double Y) { ";
  Out << "return 0; }\n";
  Out << "static inline int llvm_fcmp_1(double X, double Y) { ";
  Out << "return 1; }\n";

  // Loop over all select operations
  for (std::set<Type*>::iterator it = SelectDeclTypes.begin(), end = SelectDeclTypes.end();
       it != end; ++it) {
    // static inline Rty llvm_select_u8x4(<bool x 4> condition, <u8 x 4> iftrue, <u8 x 4> ifnot) {
    //     Rty r = {
    //          condition[0] ? iftrue[0] : ifnot[0],
    //          condition[1] ? iftrue[1] : ifnot[1],
    //          condition[2] ? iftrue[2] : ifnot[2],
    //          condition[3] ? iftrue[3] : ifnot[3]
    //          };
    //     return r;
    // }
    Out << "static inline ";
    printTypeName(Out, *it, false);
    Out << " llvm_select_";
    printTypeString(Out, *it, false);
    Out << "(";
    if (isa<VectorType>(*it))
      printSimpleType(Out, VectorType::get(Type::getInt1Ty((*it)->getContext()), (*it)->getVectorNumElements()), false);
    else
      Out << "bool";
    Out << " condition, ";
    printTypeName(Out, *it, false);
    Out << " iftrue, ";
    printTypeName(Out, *it, false);
    Out << " ifnot) {\n  ";
    printTypeName(Out, *it, false);
    Out << " r = ";
    if (isa<VectorType>(*it)) {
      unsigned n, l = (*it)->getVectorNumElements();
      Out << "{\n";
      for (n = 0; n < l; n++) {
        Out << "    condition[" << n << "] ? iftrue[" << n << "] : ifnot[" << n << "]";
        if (n != l - 1) Out << ",\n";
      }
      Out << "\n    };\n";
    }
    else {
      Out << "condition ? iftrue : ifnot;\n";
    }
    Out << "  return r;\n}\n";
  }

  // Loop over all compare operations
  for (std::set< std::pair<CmpInst::Predicate, VectorType*> >::iterator it = CmpDeclTypes.begin(), end = CmpDeclTypes.end();
       it != end; ++it) {
    // static inline <bool x 4> llvm_icmp_ge_u8x4(<u8 x 4> l, <u8 x 4> r) {
    //     Rty r = {
    //          l[0] >= r[0],
    //          l[1] >= r[1],
    //          l[2] >= r[2],
    //          l[3] >= r[3],
    //          };
    //     return r;
    // }
    unsigned n, l = (*it).second->getVectorNumElements();
    VectorType *RTy = VectorType::get(Type::getInt1Ty((*it).second->getContext()), l);
    bool isSigned = CmpInst::isSigned((*it).first);
    Out << "static inline ";
    printTypeName(Out, RTy, isSigned);
    if (CmpInst::isFPPredicate((*it).first))
      Out << " llvm_fcmp_";
    else
      Out << " llvm_icmp_";
    Out << getCmpPredicateName((*it).first) << "_";
    printTypeString(Out, (*it).second, isSigned);
    Out << "(";
    printTypeName(Out, (*it).second, isSigned);
    Out << " l, ";
    printTypeName(Out, (*it).second, isSigned);
    Out << " r) {\n  ";
    printTypeName(Out, RTy, isSigned);
    Out << " c = {\n";
    for (n = 0; n < l; n++) {
      if (CmpInst::isFPPredicate((*it).first)) {
        Out << "    llvm_fcmp_" << getCmpPredicateName((*it).first) << "(l[" << n << "], r[" << n << "])"; break;
      } else {
        switch ((*it).first) {
        case CmpInst::ICMP_EQ:  Out << "    l[" << n << "] == r[" << n << "]"; break;
        case CmpInst::ICMP_NE:  Out << "    l[" << n << "] != r[" << n << "]"; break;
        case CmpInst::ICMP_ULE:
        case CmpInst::ICMP_SLE: Out << "    l[" << n << "] <= r[" << n << "]"; break;
        case CmpInst::ICMP_UGE:
        case CmpInst::ICMP_SGE: Out << "    l[" << n << "] >= r[" << n << "]"; break;
        case CmpInst::ICMP_ULT:
        case CmpInst::ICMP_SLT: Out << "    l[" << n << "] < r[" << n << "]"; break;
        case CmpInst::ICMP_UGT:
        case CmpInst::ICMP_SGT: Out << "    l[" << n << "] > r[" << n << "]"; break;
        default:
  #ifndef NDEBUG
          errs() << "Invalid icmp predicate!" << (*it).first;
  #endif
          llvm_unreachable(0);
        }
      }
      if (n != l - 1) Out << ",\n";
    }
    Out << "\n    };\n";
    Out << "  return c;\n}\n";
  }

  // Loop over all (vector) cast operations
  for (std::set<std::pair<CastInst::CastOps, std::pair<Type*, Type*>>>::iterator it = CastOpDeclTypes.begin(), end = CastOpDeclTypes.end();
       it != end; ++it) {
    // static inline <u32 x 4> llvm_ZExt_u8x4_u32x4(<u8 x 4> in) { // Src->isVector == Dst->isVector
    //     Rty out = {
    //          in[0],
    //          in[1],
    //          in[2],
    //          in[3]
    //          };
    //     return out;
    // }
    // static inline u32 llvm_BitCast_u8x4_u32(<u8 x 4> in) { // Src->bitsSize == Dst->bitsSize
    //     union {
    //       <u8 x 4> in;
    //       u32 out;
    //     } cast;
    //     cast.in = in;
    //     return cast.out;
    // }
    CastInst::CastOps opcode = (*it).first;
    Type *SrcTy = (*it).second.first;
    Type *DstTy = (*it).second.second;
    bool SrcSigned, DstSigned;
    switch (opcode) {
    default:
      SrcSigned = false;
      DstSigned = false;
    case Instruction::SIToFP:
      SrcSigned = true;
      DstSigned = false;
    case Instruction::FPToSI:
      SrcSigned = false;
      DstSigned = true;
    case Instruction::SExt:
      SrcSigned = true;
      DstSigned = true;
    }

    Out << "static inline ";
    printTypeName(Out, DstTy, DstSigned);
    Out << " llvm_" << Instruction::getOpcodeName(opcode) << "_";
    printTypeString(Out, SrcTy, false);
    Out << "_";
    printTypeString(Out, DstTy, false);
    Out << "(";
    printTypeName(Out, SrcTy, SrcSigned);
    Out << " in) {\n  ";
    if (opcode == Instruction::BitCast) {
      Out << "union {\n    ";
      printTypeName(Out, SrcTy, SrcSigned);
      Out << " in;\n    ";
      printTypeName(Out, DstTy, DstSigned);
      Out << " out;\n  } cast;\n";
      Out << "  cast.in = in;\n  return cast.out;\n}\n";
    }
    else {
      printTypeName(Out, DstTy, DstSigned);
      Out << " out = {\n";
      unsigned n, l = DstTy->getVectorNumElements();
      assert(SrcTy->getVectorNumElements() == l);
      for (n = 0; n < l; n++) {
        Out << "    in[" << n << "]";
        if (n != l - 1) Out << ",\n";
      }
      Out << "\n};\n  return out;\n}\n";
    }
  }

  // Loop over all simple vector operations
  for (std::set<std::pair<unsigned, Type*>>::iterator it = InlineOpDeclTypes.begin(), end = InlineOpDeclTypes.end();
       it != end; ++it) {
    // static inline <u32 x 4> llvm_BinOp_u32x4(<u32 x 4> a, <u32 x 4> b) {
    //     Rty r = {
    //          a[0] OP b[0],
    //          a[1] OP b[1],
    //          a[2] OP b[2],
    //          a[3] OP b[3],
    //          };
    //     return r;
    // }
    unsigned opcode = (*it).first;
    Type *OpTy = (*it).second;
    Type *ElemTy = isa<VectorType>(OpTy) ? OpTy->getVectorElementType() : OpTy;
    bool shouldCast;
    bool isSigned;
    opcodeNeedsCast(opcode, shouldCast, isSigned);

    Out << "static inline ";
    printTypeName(Out, OpTy);
    if (opcode == BinaryNeg) {
      Out << " llvm_neg_";
      printTypeString(Out, OpTy, false);
      Out << "(";
      printTypeName(Out, OpTy, isSigned);
      Out << " a) {\n  ";
    } else if (opcode == BinaryNot) {
      Out << " llvm_not_";
      printTypeString(Out, OpTy, false);
      Out << "(";
      printTypeName(Out, OpTy, isSigned);
      Out << " a) {\n  ";
    } else {
      Out << " llvm_" << Instruction::getOpcodeName(opcode) << "_";
      printTypeString(Out, OpTy, false);
      Out << "(";
      printTypeName(Out, OpTy, isSigned);
      Out << " a, ";
      printTypeName(Out, OpTy, isSigned);
      Out << " b) {\n  ";
    }

    printTypeName(Out, OpTy);
    // C can't handle non-power-of-two integer types
    unsigned mask = 0;
    if (ElemTy->isIntegerTy()) {
       IntegerType *ITy = static_cast<IntegerType*>(ElemTy);
       if (!ITy->isPowerOf2ByteWidth())
         mask = ITy->getBitMask();
    }

    if (isa<VectorType>(OpTy)) {
      Out << " r = {\n";
      unsigned n, l = OpTy->getVectorNumElements();
      for (n = 0; n < l; n++) {
        Out << "    ";
        if (mask)
          Out << "(";
        if (opcode == BinaryNeg) {
          Out << "    -a[" << n << "]";
        } else if (opcode == BinaryNot) {
          Out << "    ~a[" << n << "]";
        } else if (opcode == Instruction::FRem) {
          // Output a call to fmod/fmodf instead of emitting a%b
          if (ElemTy->isFloatTy())
            Out << "fmodf(a[" << n << "], b[" << n << "])";
          else if (ElemTy->isDoubleTy())
            Out << "fmod(a[" << n << "], b[" << n << "])";
          else  // all 3 flavors of long double
            Out << "fmodl(a[" << n << "], b[" << n << "])";
        } else {
          Out << "    a[" << n << "]";
          switch (opcode) {
          case Instruction::Add:
          case Instruction::FAdd: Out << " + "; break;
          case Instruction::Sub:
          case Instruction::FSub: Out << " - "; break;
          case Instruction::Mul:
          case Instruction::FMul: Out << " * "; break;
          case Instruction::URem:
          case Instruction::SRem:
          case Instruction::FRem: Out << " % "; break;
          case Instruction::UDiv:
          case Instruction::SDiv:
          case Instruction::FDiv: Out << " / "; break;
          case Instruction::And:  Out << " & "; break;
          case Instruction::Or:   Out << " | "; break;
          case Instruction::Xor:  Out << " ^ "; break;
          case Instruction::Shl : Out << " << "; break;
          case Instruction::LShr:
          case Instruction::AShr: Out << " >> "; break;
          default:
#ifndef NDEBUG
             errs() << "Invalid operator type!" << opcode;
#endif
             llvm_unreachable(0);
          }
          Out << "b[" << n << "]";
          if (mask)
            Out << ") & " << mask;
        }
        if (n != l - 1) Out << ",\n";
      }
      Out << "\n};\n";
    } else {
      Out << " r = ";
        if (mask)
          Out << "(";
        if (opcode == BinaryNeg) {
          Out << "-a";
        } else if (opcode == BinaryNot) {
          Out << "~a";
        } else if (opcode == Instruction::FRem) {
          // Output a call to fmod/fmodf instead of emitting a%b
          if (ElemTy->isFloatTy())
            Out << "fmodf(a, b)";
          else if (ElemTy->isDoubleTy())
            Out << "fmod(a, b)";
          else  // all 3 flavors of long double
            Out << "fmodl(a, b)";
        } else {
          Out << "a";
          switch (opcode) {
          case Instruction::Add:
          case Instruction::FAdd: Out << " + "; break;
          case Instruction::Sub:
          case Instruction::FSub: Out << " - "; break;
          case Instruction::Mul:
          case Instruction::FMul: Out << " * "; break;
          case Instruction::URem:
          case Instruction::SRem:
          case Instruction::FRem: Out << " % "; break;
          case Instruction::UDiv:
          case Instruction::SDiv:
          case Instruction::FDiv: Out << " / "; break;
          case Instruction::And:  Out << " & "; break;
          case Instruction::Or:   Out << " | "; break;
          case Instruction::Xor:  Out << " ^ "; break;
          case Instruction::Shl : Out << " << "; break;
          case Instruction::LShr:
          case Instruction::AShr: Out << " >> "; break;
          default:
#ifndef NDEBUG
             errs() << "Invalid operator type!" << opcode;
#endif
             llvm_unreachable(0);
          }
          Out << "b";
          if (mask)
            Out << ") & " << mask;
        }
        Out << ";\n";
    }
    Out << "  return r;\n}\n";
  }

  // Emit definitions of the intrinsics.
  for (SmallVector<Function*, 16>::iterator
       I = intrinsicsToDefine.begin(),
       E = intrinsicsToDefine.end(); I != E; ++I) {
    printIntrinsicDefinition(**I, Out);
  }

  if (!M.empty())
    Out << "\n\n/* Function Bodies */\n";
}


/// Output all floating point constants that cannot be printed accurately...
void CWriter::printFloatingPointConstants(Function &F) {
  // Scan the module for floating point constants.  If any FP constant is used
  // in the function, we want to redirect it here so that we do not depend on
  // the precision of the printed form, unless the printed form preserves
  // precision.
  //
  for (inst_iterator I = inst_begin(&F), E = inst_end(&F); I != E; ++I)
      for (Instruction::op_iterator I_Op = I->op_begin(), E_Op = I->op_end(); I_Op != E_Op; ++I_Op)
          if (const Constant *C = dyn_cast<Constant>(I_Op))
            printFloatingPointConstants(C);
  Out << '\n';
}

void CWriter::printFloatingPointConstants(const Constant *C) {
  // If this is a constant expression, recursively check for constant fp values.
  if (const ConstantExpr *CE = dyn_cast<ConstantExpr>(C)) {
    for (unsigned i = 0, e = CE->getNumOperands(); i != e; ++i)
      printFloatingPointConstants(CE->getOperand(i));
    return;
  }

  // Otherwise, check for a FP constant that we need to print.
  const ConstantFP *FPC = dyn_cast<ConstantFP>(C);
  if (FPC == 0 ||
      // Do not put in FPConstantMap if safe.
      isFPCSafeToPrint(FPC) ||
      // Already printed this constant?
      FPConstantMap.count(FPC))
    return;

  FPConstantMap[FPC] = FPCounter;  // Number the FP constants

  if (FPC->getType() == Type::getDoubleTy(FPC->getContext())) {
    double Val = FPC->getValueAPF().convertToDouble();
    uint64_t i = FPC->getValueAPF().bitcastToAPInt().getZExtValue();
    Out << "static const ConstantDoubleTy FPConstant" << FPCounter++
    << " = 0x" << utohexstr(i)
    << "ULL;    /* " << Val << " */\n";
  } else if (FPC->getType() == Type::getFloatTy(FPC->getContext())) {
    float Val = FPC->getValueAPF().convertToFloat();
    uint32_t i = (uint32_t)FPC->getValueAPF().bitcastToAPInt().
    getZExtValue();
    Out << "static const ConstantFloatTy FPConstant" << FPCounter++
    << " = 0x" << utohexstr(i)
    << "U;    /* " << Val << " */\n";
  } else if (FPC->getType() == Type::getX86_FP80Ty(FPC->getContext())) {
    // api needed to prevent premature destruction
    const APInt api = FPC->getValueAPF().bitcastToAPInt();
    const uint64_t *p = api.getRawData();
    Out << "static const ConstantFP80Ty FPConstant" << FPCounter++
    << " = { 0x" << utohexstr(p[0])
    << "ULL, 0x" << utohexstr((uint16_t)p[1]) << ",{0,0,0}"
    << "}; /* Long double constant */\n";
  } else if (FPC->getType() == Type::getPPC_FP128Ty(FPC->getContext()) ||
             FPC->getType() == Type::getFP128Ty(FPC->getContext())) {
    const APInt api = FPC->getValueAPF().bitcastToAPInt();
    const uint64_t *p = api.getRawData();
    Out << "static const ConstantFP128Ty FPConstant" << FPCounter++
    << " = { 0x"
    << utohexstr(p[0]) << ", 0x" << utohexstr(p[1])
    << "}; /* Long double constant */\n";

  } else {
    llvm_unreachable("Unknown float type!");
  }
}


/// printSymbolTable - Run through symbol table looking for type names.  If a
/// type name is found, emit its declaration...
///
void CWriter::printModuleTypes(raw_ostream &Out) {
  Out << "/* Helper union for bitcasts */\n";
  Out << "typedef union {\n";
  Out << "  unsigned int Int32;\n";
  Out << "  unsigned long long Int64;\n";
  Out << "  float Float;\n";
  Out << "  double Double;\n";
  Out << "} llvmBitCastUnion;\n";

  // Keep track of which structures have been printed so far.
  SmallPtrSet<Type *, 16> StructPrinted;

  // Loop over all structures then push them into the stack so they are
  // printed in the correct order.
  Out << "\n/* Structure and Array contents */\n";
  for (std::set<StructType*>::iterator it = StructTypes.begin(), end = StructTypes.end();
       it != end; ++it) {
    printContainedStructs(Out, *it, StructPrinted);
  }

  for (DenseMap<StructType*, unsigned>::iterator
       I = UnnamedStructIDs.begin(), E = UnnamedStructIDs.end();
       I != E; ++I) {
    printContainedStructs(Out, I->first, StructPrinted);
  }

  // Loop over all arrays
  for (std::set<ArrayType*>::iterator it = ArrayTypes.begin(), end = ArrayTypes.end();
       it != end; ++it) {
    printContainedStructs(Out, *it, StructPrinted);
  }

  // We may have collected some intrinsic prototypes to emit.
  // Emit them now, before the function that uses them is emitted
  for (std::vector<Function*>::iterator
       I = prototypesToGen.begin(), E = prototypesToGen.end();
       I != E; ++I) {
    Out << '\n';
    Function *F = *I;
    printFunctionProto(Out, F);
    Out << ";\n";
  }

  // Loop over all function prototypes
  for (DenseMap<std::pair<FunctionType*, AttributeSet>, unsigned>::iterator
       I = UnnamedFunctionIDs.begin(), E = UnnamedFunctionIDs.end();
       I != E; ++I) {
    Out << '\n';
    std::pair<FunctionType*, AttributeSet> F = I->first;
    printFunctionDeclaration(Out, F.first, F.second, getFunctionName(F.first, F.second));
  }
}

// Push the struct onto the stack and recursively push all structs
// this one depends on.
//
// TODO:  Make this work properly with vector types
//
void CWriter::printContainedStructs(raw_ostream &Out, Type *Ty,
                                    SmallPtrSet<Type *, 16> &StructPrinted) {
  // Don't walk through simple primitive types (including pointers)
  if (Ty->isPointerTy() && !Ty->isVectorTy())
    return;
  // Check to see if we have already printed this struct.
  if (!StructPrinted.insert(Ty).second) return;

  // Print all contained types first.
  for (Type::subtype_iterator I = Ty->subtype_begin(),
       E = Ty->subtype_end(); I != E; ++I)
    printContainedStructs(Out, *I, StructPrinted);

  if (StructType *ST = dyn_cast<StructType>(Ty)) {
    // Print structure type out.
    printStructDeclaration(Out, ST);
  } else if (ArrayType *AT = dyn_cast<ArrayType>(Ty)) {
    // Print array type out.
    printArrayDeclaration(Out, AT);
  }
}

void CWriter::printFunctionSignature(raw_ostream &Out, Function *F) {
  assert(!F->isDeclaration());
  /// isStructReturn - Should this function actually return a struct by-value?
  bool isStructReturn = F->hasStructRetAttr();

  if (F->hasDLLImportStorageClass()) Out << "__declspec(dllimport) ";
  if (F->hasDLLExportStorageClass()) Out << "__declspec(dllexport) ";
  if (F->hasLocalLinkage()) Out << "static ";

  switch (F->getCallingConv()) {
   case CallingConv::X86_StdCall:
    Out << "__attribute__((stdcall)) ";
    break;
   case CallingConv::X86_FastCall:
    Out << "__attribute__((fastcall)) ";
    break;
   case CallingConv::X86_ThisCall:
    Out << "__attribute__((thiscall)) ";
    break;
   default:
    break;
  }

  FunctionType *FTy = F->getFunctionType();
  const AttributeSet &PAL = F->getAttributes();

  // Get the return type for the function.
  Type *RetTy;
  if (!isStructReturn)
    RetTy = FTy->getReturnType();
  else {
    // If this is a struct-return function, print the struct-return type.
    RetTy = cast<PointerType>(FTy->getParamType(0))->getElementType();
  }
  printTypeName(Out, RetTy,
    /*isSigned=*/PAL.hasAttribute(AttributeSet::ReturnIndex, Attribute::SExt));

  // Print out the name...
  Out << ' ' << GetValueName(F) << '(';

  // Loop over the arguments, printing them...
  bool PrintedArg = false;
  Function::arg_iterator I = F->arg_begin(), E = F->arg_end();
  unsigned Idx = 1;

  // If this is a struct-return function, don't print the hidden
  // struct-return argument.
  if (isStructReturn) {
    assert(I != E && "Invalid struct return function!");
    ++I;
    ++Idx;
  }

  for (; I != E; ++I) {
    Type *ArgTy = I->getType();
    if (PAL.hasAttribute(Idx, Attribute::ByVal)) {
      ArgTy = cast<PointerType>(ArgTy)->getElementType();
      ByValParams.insert(I);
    }
    if (PrintedArg)
        Out << ", ";
    printTypeName(Out, ArgTy, /*isSigned=*/PAL.hasAttribute(Idx, Attribute::SExt));
    Out << ' ' << GetValueName(I);
    PrintedArg = true;
    ++Idx;
  }
  if (FTy->isVarArg()) {
    if (!PrintedArg)
      Out << "int vararg_dummy_arg";
    Out << ", ...";
  } else if (!PrintedArg) {
    Out << "void";
  }
  Out << ")";
}

static inline bool isFPIntBitCast(Instruction &I) {
  if (!isa<BitCastInst>(I))
    return false;
  Type *SrcTy = I.getOperand(0)->getType();
  Type *DstTy = I.getType();
  return (SrcTy->isFloatingPointTy() && DstTy->isIntegerTy()) ||
         (DstTy->isFloatingPointTy() && SrcTy->isIntegerTy());
}

void CWriter::printFunction(Function &F) {
  /// isStructReturn - Should this function actually return a struct by-value?
  bool isStructReturn = F.hasStructRetAttr();

  printFunctionSignature(Out, &F);
  Out << " {\n";

  // If this is a struct return function, handle the result with magic.
  if (isStructReturn) {
    Type *StructTy =
      cast<PointerType>(F.arg_begin()->getType())->getElementType();
    Out << "  ";
    printTypeName(Out, StructTy, false) << " StructReturn;  /* Struct return temporary */\n";

    Out << "  ";
    printTypeName(Out, F.arg_begin()->getType(), false);
    Out << GetValueName(F.arg_begin()) << " = &StructReturn;\n";
  }

  bool PrintedVar = false;

  // print local variable information for the function
  for (inst_iterator I = inst_begin(&F), E = inst_end(&F); I != E; ++I) {
    if (AllocaInst *AI = isDirectAlloca(&*I)) {
      unsigned Alignment = AI->getAlignment();
      bool IsOveraligned = Alignment &&
        Alignment > TD->getABITypeAlignment(AI->getAllocatedType());
      Out << "  ";
      printTypeName(Out, AI->getAllocatedType(), false) << ' ';
      if (IsOveraligned)
        Out << "__attribute__((aligned(" << Alignment << "))) ";
      Out << GetValueName(AI);
      Out << ";    /* Address-exposed local */\n";
      PrintedVar = true;
    } else if (I->getType() != Type::getVoidTy(F.getContext()) &&
               !isInlinableInst(*I)) {
      Out << "  ";
      printTypeName(Out, I->getType(), false) << ' ' << GetValueName(&*I);
      Out << ";\n";

      if (isa<PHINode>(*I)) {  // Print out PHI node temporaries as well...
        Out << "  ";
        printTypeName(Out, I->getType(), false) << ' ' << (GetValueName(&*I)+"__PHI_TEMPORARY");
        Out << ";\n";
      }
      PrintedVar = true;
    }
    // We need a temporary for the BitCast to use so it can pluck a value out
    // of a union to do the BitCast. This is separate from the need for a
    // variable to hold the result of the BitCast.
    if (isFPIntBitCast(*I)) {
      Out << "  llvmBitCastUnion " << GetValueName(&*I)
          << "__BITCAST_TEMPORARY;\n";
      PrintedVar = true;
    }
  }

  if (PrintedVar)
    Out << '\n';

  if (F.hasExternalLinkage() && F.getName() == "main")
    Out << "  CODE_FOR_MAIN();\n";

  // print the basic blocks
  for (Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB) {
    if (Loop *L = LI->getLoopFor(BB)) {
      if (L->getHeader() == BB && L->getParentLoop() == 0)
        printLoop(L);
    } else {
      printBasicBlock(BB);
    }
  }

  Out << "}\n\n";
}

void CWriter::printLoop(Loop *L) {
  Out << "  do {     /* Syntactic loop '" << L->getHeader()->getName()
      << "' to make GCC happy */\n";
  for (unsigned i = 0, e = L->getBlocks().size(); i != e; ++i) {
    BasicBlock *BB = L->getBlocks()[i];
    Loop *BBLoop = LI->getLoopFor(BB);
    if (BBLoop == L)
      printBasicBlock(BB);
    else if (BB == BBLoop->getHeader() && BBLoop->getParentLoop() == L)
      printLoop(BBLoop);
  }
  Out << "  } while (1); /* end of syntactic loop '"
      << L->getHeader()->getName() << "' */\n";
}

void CWriter::printBasicBlock(BasicBlock *BB) {

  // Don't print the label for the basic block if there are no uses, or if
  // the only terminator use is the predecessor basic block's terminator.
  // We have to scan the use list because PHI nodes use basic blocks too but
  // do not require a label to be generated.
  //
  bool NeedsLabel = false;
  for (pred_iterator PI = pred_begin(BB), E = pred_end(BB); PI != E; ++PI)
    if (isGotoCodeNecessary(*PI, BB)) {
      NeedsLabel = true;
      break;
    }

  if (NeedsLabel) Out << GetValueName(BB) << ":\n";

  // Output all of the instructions in the basic block...
  for (BasicBlock::iterator II = BB->begin(), E = --BB->end(); II != E;
       ++II) {
    if (!isInlinableInst(*II) && !isDirectAlloca(II)) {
      if (II->getType() != Type::getVoidTy(BB->getContext()) &&
          !isInlineAsm(*II))
        outputLValue(II);
      else
        Out << "  ";
      writeInstComputationInline(*II);
      Out << ";\n";
    }
  }

  // Don't emit prefix or suffix for the terminator.
  visit(*BB->getTerminator());
}


// Specific Instruction type classes... note that all of the casts are
// necessary because we use the instruction classes as opaque types...
//
void CWriter::visitReturnInst(ReturnInst &I) {
  // If this is a struct return function, return the temporary struct.
  bool isStructReturn = I.getParent()->getParent()->hasStructRetAttr();

  if (isStructReturn) {
    Out << "  return StructReturn;\n";
    return;
  }

  // Don't output a void return if this is the last basic block in the function
  // unless that would make the basic block empty
  if (I.getNumOperands() == 0 &&
      &*--I.getParent()->getParent()->end() == I.getParent() &&
      I.getParent()->begin() != I) {
    return;
  }

  Out << "  return";
  if (I.getNumOperands()) {
    Out << ' ';
    writeOperand(I.getOperand(0));
  }
  Out << ";\n";
}

void CWriter::visitSwitchInst(SwitchInst &SI) {

  Value* Cond = SI.getCondition();

  Out << "  switch (";
  writeOperand(Cond);
  Out << ") {\n  default:\n";
  printPHICopiesForSuccessor (SI.getParent(), SI.getDefaultDest(), 2);
  printBranchToBlock(SI.getParent(), SI.getDefaultDest(), 2);
  Out << ";\n";

  // Skip the first item since that's the default case.
  for (SwitchInst::CaseIt i = SI.case_begin(), e = SI.case_end(); i != e; ++i) {
    ConstantInt* CaseVal = i.getCaseValue();
    BasicBlock* Succ = i.getCaseSuccessor();
    Out << "  case ";
    writeOperand(CaseVal);
    Out << ":\n";
    printPHICopiesForSuccessor (SI.getParent(), Succ, 2);
    printBranchToBlock(SI.getParent(), Succ, 2);
    if (Function::iterator(Succ) ==
        std::next(Function::iterator(SI.getParent())))
      Out << "    break;\n";
  }

  Out << "  }\n";
}

void CWriter::visitIndirectBrInst(IndirectBrInst &IBI) {
  Out << "  goto *(void*)(";
  writeOperand(IBI.getOperand(0));
  Out << ");\n";
}

void CWriter::visitUnreachableInst(UnreachableInst &I) {
  Out << "__builtin_unreachable();\n"; // TODO: use __assume(0) on MSVC
}

bool CWriter::isGotoCodeNecessary(BasicBlock *From, BasicBlock *To) {
  /// FIXME: This should be reenabled, but loop reordering safe!!
  return true;

  if (std::next(Function::iterator(From)) != Function::iterator(To))
    return true;  // Not the direct successor, we need a goto.

  //isa<SwitchInst>(From->getTerminator())

  if (LI->getLoopFor(From) != LI->getLoopFor(To))
    return true;
  return false;
}

void CWriter::printPHICopiesForSuccessor (BasicBlock *CurBlock,
                                          BasicBlock *Successor,
                                          unsigned Indent) {
  for (BasicBlock::iterator I = Successor->begin(); isa<PHINode>(I); ++I) {
    PHINode *PN = cast<PHINode>(I);
    // Now we have to do the printing.
    Value *IV = PN->getIncomingValueForBlock(CurBlock);
    if (!isa<UndefValue>(IV)) {
      Out << std::string(Indent, ' ');
      Out << "  " << GetValueName(I) << "__PHI_TEMPORARY = ";
      writeOperand(IV);
      Out << ";   /* for PHI node */\n";
    }
  }
}

void CWriter::printBranchToBlock(BasicBlock *CurBB, BasicBlock *Succ,
                                 unsigned Indent) {
  if (isGotoCodeNecessary(CurBB, Succ)) {
    Out << std::string(Indent, ' ') << "  goto ";
    writeOperand(Succ);
    Out << ";\n";
  }
}

// Branch instruction printing - Avoid printing out a branch to a basic block
// that immediately succeeds the current one.
//
void CWriter::visitBranchInst(BranchInst &I) {

  if (I.isConditional()) {
    if (isGotoCodeNecessary(I.getParent(), I.getSuccessor(0))) {
      Out << "  if (";
      writeOperand(I.getCondition());
      Out << ") {\n";

      printPHICopiesForSuccessor (I.getParent(), I.getSuccessor(0), 2);
      printBranchToBlock(I.getParent(), I.getSuccessor(0), 2);

      if (isGotoCodeNecessary(I.getParent(), I.getSuccessor(1))) {
        Out << "  } else {\n";
        printPHICopiesForSuccessor (I.getParent(), I.getSuccessor(1), 2);
        printBranchToBlock(I.getParent(), I.getSuccessor(1), 2);
      }
    } else {
      // First goto not necessary, assume second one is...
      Out << "  if (!";
      writeOperand(I.getCondition());
      Out << ") {\n";

      printPHICopiesForSuccessor (I.getParent(), I.getSuccessor(1), 2);
      printBranchToBlock(I.getParent(), I.getSuccessor(1), 2);
    }

    Out << "  }\n";
  } else {
    printPHICopiesForSuccessor (I.getParent(), I.getSuccessor(0), 0);
    printBranchToBlock(I.getParent(), I.getSuccessor(0), 0);
  }
  Out << "\n";
}

// PHI nodes get copied into temporary values at the end of predecessor basic
// blocks.  We now need to copy these temporary values into the REAL value for
// the PHI.
void CWriter::visitPHINode(PHINode &I) {
  writeOperand(&I);
  Out << "__PHI_TEMPORARY";
}

void CWriter::visitBinaryOperator(BinaryOperator &I) {
  // binary instructions, shift instructions, setCond instructions.
  assert(!I.getType()->isPointerTy());

  // We must cast the results of binary operations which might be promoted.
  bool needsCast = false;
  if ((I.getType() == Type::getInt8Ty(I.getContext())) ||
      (I.getType() == Type::getInt16Ty(I.getContext()))
      || (I.getType() == Type::getFloatTy(I.getContext()))) {
    needsCast = true;
  }
  bool shouldCast;
  bool castIsSigned;
  opcodeNeedsCast(I.getOpcode(), shouldCast, castIsSigned);

  if (I.getType()->isVectorTy() || needsCast || shouldCast) {
    Type *VTy = I.getOperand(0)->getType();
    unsigned opcode;
    if (BinaryOperator::isNeg(&I)) {
      opcode = BinaryNeg;
      Out << "llvm_neg_";
      printTypeString(Out, VTy, false);
      Out << "(";
      writeOperand(BinaryOperator::getNegArgument(&I));
    } else if (BinaryOperator::isFNeg(&I)) {
      opcode = BinaryNeg;
      Out << "llvm_neg_";
      printTypeString(Out, VTy, false);
      Out << "(";
      writeOperand(BinaryOperator::getFNegArgument(&I));
    } else if (BinaryOperator::isNot(&I)) {
      opcode = BinaryNot;
      Out << "llvm_not_";
      printTypeString(Out, VTy, false);
      Out << "(";
      writeOperand(BinaryOperator::getNotArgument(&I));
    } else {
      opcode = I.getOpcode();
      Out << "llvm_" << Instruction::getOpcodeName(opcode) << "_";
      printTypeString(Out, VTy, false);
      Out << "(";
      writeOperand(I.getOperand(0));
      Out << ", ";
      writeOperand(I.getOperand(1));
    }
    Out << ")";
    InlineOpDeclTypes.insert(std::pair<unsigned, Type*>(opcode, VTy));
    return;
  }

  // If this is a negation operation, print it out as such.  For FP, we don't
  // want to print "-0.0 - X".
  if (BinaryOperator::isNeg(&I)) {
    Out << "-(";
    writeOperand(BinaryOperator::getNegArgument(&I));
    Out << ")";
  } else if (BinaryOperator::isFNeg(&I)) {
    Out << "-(";
    writeOperand(BinaryOperator::getFNegArgument(&I));
    Out << ")";
  } else if (BinaryOperator::isNot(&I)) {
    Out << "~(";
    writeOperand(BinaryOperator::getNotArgument(&I));
    Out << ")";
  } else if (I.getOpcode() == Instruction::FRem) {
    // Output a call to fmod/fmodf instead of emitting a%b
    if (I.getType() == Type::getFloatTy(I.getContext()))
      Out << "fmodf(";
    else if (I.getType() == Type::getDoubleTy(I.getContext()))
      Out << "fmod(";
    else  // all 3 flavors of long double
      Out << "fmodl(";
    writeOperand(I.getOperand(0));
    Out << ", ";
    writeOperand(I.getOperand(1));
    Out << ")";
  } else {

    // Write out the cast of the instruction's value back to the proper type
    // if necessary.
    bool NeedsClosingParens = writeInstructionCast(I);

    // Certain instructions require the operand to be forced to a specific type
    // so we use writeOperandWithCast here instead of writeOperand. Similarly
    // below for operand 1
    writeOperandWithCast(I.getOperand(0), I.getOpcode());

    switch (I.getOpcode()) {
    case Instruction::Add:
    case Instruction::FAdd: Out << " + "; break;
    case Instruction::Sub:
    case Instruction::FSub: Out << " - "; break;
    case Instruction::Mul:
    case Instruction::FMul: Out << " * "; break;
    case Instruction::URem:
    case Instruction::SRem:
    case Instruction::FRem: Out << " % "; break;
    case Instruction::UDiv:
    case Instruction::SDiv:
    case Instruction::FDiv: Out << " / "; break;
    case Instruction::And:  Out << " & "; break;
    case Instruction::Or:   Out << " | "; break;
    case Instruction::Xor:  Out << " ^ "; break;
    case Instruction::Shl : Out << " << "; break;
    case Instruction::LShr:
    case Instruction::AShr: Out << " >> "; break;
    default:
#ifndef NDEBUG
       errs() << "Invalid operator type!" << I;
#endif
       llvm_unreachable(0);
    }

    writeOperandWithCast(I.getOperand(1), I.getOpcode());
    if (NeedsClosingParens)
      Out << "))";
  }

  if (needsCast) {
    Out << "))";
  }
}

void CWriter::visitICmpInst(ICmpInst &I) {
  if (I.getType()->isVectorTy()) {
    VectorType *VTy = cast<VectorType>(I.getOperand(0)->getType());
    Out << "llvm_icmp_" << getCmpPredicateName(I.getPredicate()) << "_";
    printTypeString(Out, VTy, I.isSigned());
    Out << "(";
    writeOperand(I.getOperand(0));
    Out << ", ";
    writeOperand(I.getOperand(1));
    Out << ")";
    CmpDeclTypes.insert(std::pair<CmpInst::Predicate, VectorType*>(I.getPredicate(), VTy));
    return;
  }

  // We must cast the results of icmp which might be promoted.
  bool needsCast = false;

  // Write out the cast of the instruction's value back to the proper type
  // if necessary.
  bool NeedsClosingParens = writeInstructionCast(I);

  // Certain icmp predicate require the operand to be forced to a specific type
  // so we use writeOperandWithCast here instead of writeOperand. Similarly
  // below for operand 1
  writeOperandWithCast(I.getOperand(0), I);

  switch (I.getPredicate()) {
  case ICmpInst::ICMP_EQ:  Out << " == "; break;
  case ICmpInst::ICMP_NE:  Out << " != "; break;
  case ICmpInst::ICMP_ULE:
  case ICmpInst::ICMP_SLE: Out << " <= "; break;
  case ICmpInst::ICMP_UGE:
  case ICmpInst::ICMP_SGE: Out << " >= "; break;
  case ICmpInst::ICMP_ULT:
  case ICmpInst::ICMP_SLT: Out << " < "; break;
  case ICmpInst::ICMP_UGT:
  case ICmpInst::ICMP_SGT: Out << " > "; break;
  default:
#ifndef NDEBUG
    errs() << "Invalid icmp predicate!" << I;
#endif
    llvm_unreachable(0);
  }

  writeOperandWithCast(I.getOperand(1), I);
  if (NeedsClosingParens)
    Out << "))";

  if (needsCast) {
    Out << "))";
  }
}

void CWriter::visitFCmpInst(FCmpInst &I) {
  if (I.getType()->isVectorTy()) {
    VectorType *VTy = cast<VectorType>(I.getOperand(0)->getType());
    Out << "llvm_fcmp_" << getCmpPredicateName(I.getPredicate()) << "_";
    printTypeString(Out, VTy, I.isSigned());
    Out << "(";
    writeOperand(I.getOperand(0));
    Out << ", ";
    writeOperand(I.getOperand(1));
    Out << ")";
    CmpDeclTypes.insert(std::pair<CmpInst::Predicate, VectorType*>(I.getPredicate(), VTy));
    return;
  }

  Out << "llvm_fcmp_" << getCmpPredicateName(I.getPredicate()) << "(";
  // Write the first operand
  writeOperand(I.getOperand(0));
  Out << ", ";
  // Write the second operand
  writeOperand(I.getOperand(1));
  Out << ")";
}

static const char * getFloatBitCastField(Type *Ty) {
  switch (Ty->getTypeID()) {
    default: llvm_unreachable("Invalid Type");
    case Type::FloatTyID:  return "Float";
    case Type::DoubleTyID: return "Double";
    case Type::IntegerTyID: {
      unsigned NumBits = cast<IntegerType>(Ty)->getBitWidth();
      if (NumBits <= 32)
        return "Int32";
      else
        return "Int64";
    }
  }
}

void CWriter::visitCastInst(CastInst &I) {
  Type *DstTy = I.getType();
  Type *SrcTy = I.getOperand(0)->getType();

  if (DstTy->isVectorTy() || SrcTy->isVectorTy()) {
    Out << "llvm_" << I.getOpcodeName() << "_";
    printTypeString(Out, SrcTy, false);
    Out << "_";
    printTypeString(Out, DstTy, false);
    Out << "(";
    writeOperand(I.getOperand(0));
    Out << ")";
    CastOpDeclTypes.insert(std::pair<Instruction::CastOps, std::pair<Type*, Type*> >(I.getOpcode(), std::pair<Type*, Type*>(SrcTy, DstTy)));
    return;
  }

  if (isFPIntBitCast(I)) {
    Out << '(';
    // These int<->float and long<->double casts need to be handled specially
    Out << GetValueName(&I) << "__BITCAST_TEMPORARY."
        << getFloatBitCastField(I.getOperand(0)->getType()) << " = ";
    writeOperand(I.getOperand(0));
    Out << ", " << GetValueName(&I) << "__BITCAST_TEMPORARY."
        << getFloatBitCastField(I.getType());
    Out << ')';
    return;
  }

  Out << '(';
  printCast(I.getOpcode(), SrcTy, DstTy);

  // Make a sext from i1 work by subtracting the i1 from 0 (an int).
  if (SrcTy == Type::getInt1Ty(I.getContext()) &&
      I.getOpcode() == Instruction::SExt)
    Out << "0-";

  writeOperand(I.getOperand(0));

  if (DstTy == Type::getInt1Ty(I.getContext()) &&
      (I.getOpcode() == Instruction::Trunc ||
       I.getOpcode() == Instruction::FPToUI ||
       I.getOpcode() == Instruction::FPToSI ||
       I.getOpcode() == Instruction::PtrToInt)) {
    // Make sure we really get a trunc to bool by anding the operand with 1
    Out << "&1u";
  }
  Out << ')';
}

void CWriter::visitSelectInst(SelectInst &I) {
  Out << "llvm_select_";
  printTypeString(Out, I.getType(), false);
  Out << "(";
  writeOperand(I.getCondition());
  Out << ", ";
  writeOperand(I.getTrueValue());
  Out << ", ";
  writeOperand(I.getFalseValue());
  Out << ")";
  SelectDeclTypes.insert(I.getType());
}

// Returns the macro name or value of the max or min of an integer type
// (as defined in limits.h).
static void printLimitValue(IntegerType &Ty, bool isSigned, bool isMax,
                            raw_ostream &Out) {
  const char* type;
  const char* sprefix = "";

  unsigned NumBits = Ty.getBitWidth();
  if (NumBits <= 8) {
    type = "CHAR";
    sprefix = "S";
  } else if (NumBits <= 16) {
    type = "SHRT";
  } else if (NumBits <= 32) {
    type = "INT";
  } else if (NumBits <= 64) {
    type = "LLONG";
  } else {
    llvm_unreachable("Bit widths > 64 not implemented yet");
  }

  if (isSigned)
    Out << sprefix << type << (isMax ? "_MAX" : "_MIN");
  else
    Out << "U" << type << (isMax ? "_MAX" : "0");
}

#ifndef NDEBUG
static bool isSupportedIntegerSize(IntegerType &T) {
  return T.getBitWidth() == 8 || T.getBitWidth() == 16 ||
         T.getBitWidth() == 32 || T.getBitWidth() == 64 ||
         T.getBitWidth() == 128;
}
#endif

void CWriter::printIntrinsicDefinition(Function &F, raw_ostream &Out) {
  FunctionType *funT = F.getFunctionType();
  Type *retT = F.getReturnType();
  Type *elemT = funT->getParamType(0);
  IntegerType *elemIntT = dyn_cast<IntegerType>(funT->getParamType(0));
  char i, numParams = funT->getNumParams();
  bool isSigned;
  switch (F.getIntrinsicID()) {
  default:
    isSigned = false;
    break;
  case Intrinsic::sadd_with_overflow:
  case Intrinsic::ssub_with_overflow:
  case Intrinsic::smul_with_overflow:
    isSigned = true;
    break;
  }
  assert(numParams > 0 && numParams < 26);

  // static inline Rty _llvm_op_ixx(unsigned ixx a, unsigned ixx b) {
  //   Rty r;
  //   <opcode here>
  //   return r;
  // }
  Out << "static inline ";
  printTypeName(Out, retT);
  Out << " ";
  Out << GetValueName(&F);
  Out << "(";
  for (i = 0; i < numParams; i++) {
    switch (F.getIntrinsicID()) {
    // optional intrinsic validity assertion checks
    default:
      // default case: assume all parameters must have the same type
      assert(elemT == funT->getParamType(i));
      break;
    case Intrinsic::ctlz:
    case Intrinsic::cttz:
      break;
    }
    printSimpleType(Out, funT->getParamType(i), isSigned);
    Out << " " << (char)('a' + i);
    if (i != numParams - 1) Out << ", ";
  }
  Out << ") {\n  ";
  printTypeName(Out, retT);
  Out << " r;\n";

  if (elemIntT) {
    // handle integer ops
    assert(isSupportedIntegerSize(*elemIntT) &&
           "CBackend does not support arbitrary size integers.");
    switch (F.getIntrinsicID()) {
    default:
  #ifndef NDEBUG
      errs() << "Unsupported Intrinsic!" << F;
  #endif
      llvm_unreachable(0);

    case Intrinsic::uadd_with_overflow:
      //   r.field0 = a + b;
      //   r.field1 = (r.field0 < a);
      assert(cast<StructType>(retT)->getElementType(0) == elemT);
      Out << "  r.field0 = a + b;\n";
      Out << "  r.field1 = (a >= -b);\n";
      break;

    case Intrinsic::sadd_with_overflow:
      //   r.field0 = a + b;
      //   r.field1 = (b > 0 && a > XX_MAX - b) ||
      //              (b < 0 && a < XX_MIN - b);
      assert(cast<StructType>(retT)->getElementType(0) == elemT);
      Out << "  r.field0 = a + b;\n";
      Out << "  r.field1 = (b >= 0 ? a > ";
      printLimitValue(*elemIntT, true, true, Out);
      Out << " - b : a < ";
      printLimitValue(*elemIntT, true, false, Out);
      Out << " - b);\n";
      break;

    case Intrinsic::usub_with_overflow:
      assert(cast<StructType>(retT)->getElementType(0) == elemT);
      Out << "  r.field0 = a - b;\n";
      Out << "  r.field1 = (a < b);\n";
      break;

    case Intrinsic::ssub_with_overflow:
      assert(cast<StructType>(retT)->getElementType(0) == elemT);
      Out << "  r.field0 = a - b;\n";
      Out << "  r.field1 = (b <= 0 ? a > ";
      printLimitValue(*elemIntT, true, true, Out);
      Out << " + b : a < ";
      printLimitValue(*elemIntT, true, false, Out);
      Out << " + b);\n";
      break;

    case Intrinsic::umul_with_overflow:
      assert(cast<StructType>(retT)->getElementType(0) == elemT);
      Out << "  r.field1 = LLVMMul_sov(8 * sizeof(a), &a, &b, &r.field0);\n";
      break;

    case Intrinsic::smul_with_overflow:
      assert(cast<StructType>(retT)->getElementType(0) == elemT);
      Out << "  r.field1 = LLVMMul_uov(8 * sizeof(a), &a, &b, &r.field0);\n";
      break;

    case Intrinsic::bswap:
      assert(retT == elemT);
      Out << "  LLVMFlipAllBits(8 * sizeof(a), &a, &r);\n";
      break;

    case Intrinsic::ctpop:
      assert(retT == elemT);
      Out << "  r = LLVMCountPopulation(8 * sizeof(a), &a);\n";
      break;

    case Intrinsic::ctlz:
      assert(retT == elemT);
      Out << "  (void)b;\n  r = LLVMCountLeadingZeros(8 * sizeof(a), &a);\n";
      break;

    case Intrinsic::cttz:
      assert(retT == elemT);
      Out << "  (void)b;\n  r = LLVMCountTrailingZeros(8 * sizeof(a), &a);\n";
      break;
    }
  } else {
    // handle FP ops
    const char *suffix;
    assert(retT == elemT);
    if (elemT->isFloatTy() || elemT->isHalfTy()) {
        suffix = "f";
    } else if (elemT->isDoubleTy()) {
        suffix = "";
    } else if (elemT->isFP128Ty()) {
    } else if (elemT->isX86_FP80Ty()) {
    } else if (elemT->isPPC_FP128Ty()) {
        suffix = "l";
    } else {
  #ifndef NDEBUG
      errs() << "Unsupported Intrinsic!" << F;
  #endif
      llvm_unreachable(0);
    }

    switch (F.getIntrinsicID()) {
    default:
  #ifndef NDEBUG
      errs() << "Unsupported Intrinsic!" << F;
  #endif
      llvm_unreachable(0);

    case Intrinsic::ceil:
      Out << "  r = ceil" << suffix << "(a);\n";
      break;

    case Intrinsic::fabs:
      Out << "  r = fabs" << suffix << "(a);\n";
      break;

    case Intrinsic::floor:
      Out << "  r = floor" << suffix << "(a);\n";
      break;

    case Intrinsic::fma:
      Out << "  r = fma" << suffix << "(a, b, c);\n";
      break;

    case Intrinsic::fmuladd:
      Out << "  r = a * b + c;\n";
      break;

    case Intrinsic::pow:
      Out << "  r = pow" << suffix << "(a, b);\n";
      break;

    case Intrinsic::rint:
      Out << "  r = rint" << suffix << "(a);\n";
      break;

    case Intrinsic::sqrt:
      Out << "  r = sqrt" << suffix << "(a);\n";
      break;

    case Intrinsic::trunc:
      Out << "  r = trunc" << suffix << "(a);\n";
      break;

    }
  }

  Out << "  return r;\n}\n";
}

void CWriter::lowerIntrinsics(Function &F) {
  // Examine all the instructions in this function to find the intrinsics that
  // need to be lowered.
  for (Function::iterator BB = F.begin(), EE = F.end(); BB != EE; ++BB)
    for (BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; )
      if (CallInst *CI = dyn_cast<CallInst>(I++))
        if (Function *F = CI->getCalledFunction())
          switch (F->getIntrinsicID()) {
          case Intrinsic::not_intrinsic:
          case Intrinsic::vastart:
          case Intrinsic::vacopy:
          case Intrinsic::vaend:
          case Intrinsic::returnaddress:
          case Intrinsic::frameaddress:
          case Intrinsic::setjmp:
          case Intrinsic::longjmp:
          case Intrinsic::sigsetjmp:
          case Intrinsic::siglongjmp:
          case Intrinsic::prefetch:
          case Intrinsic::x86_sse_cmp_ss:
          case Intrinsic::x86_sse_cmp_ps:
          case Intrinsic::x86_sse2_cmp_sd:
          case Intrinsic::x86_sse2_cmp_pd:
          case Intrinsic::ppc_altivec_lvsl:
          case Intrinsic::uadd_with_overflow:
          case Intrinsic::sadd_with_overflow:
          case Intrinsic::usub_with_overflow:
          case Intrinsic::ssub_with_overflow:
          case Intrinsic::umul_with_overflow:
          case Intrinsic::smul_with_overflow:
          case Intrinsic::bswap:
          case Intrinsic::ceil:
          case Intrinsic::ctlz:
          case Intrinsic::ctpop:
          case Intrinsic::cttz:
          case Intrinsic::fabs:
          case Intrinsic::floor:
          case Intrinsic::fma:
          case Intrinsic::fmuladd:
          case Intrinsic::pow:
          case Intrinsic::powi:
          case Intrinsic::rint:
          case Intrinsic::sqrt:
          case Intrinsic::trunc:
          case Intrinsic::trap:
          case Intrinsic::stackprotector:
              // We directly implement these intrinsics
            break;
          default:
            // All other intrinsic calls we must lower.
            Instruction *Before = 0;
            if (CI != &BB->front())
              Before = std::prev(BasicBlock::iterator(CI));

            IL->LowerIntrinsicCall(CI);
            if (Before) {        // Move iterator to instruction after call
              I = Before; ++I;
            } else {
              I = BB->begin();
            }
            // If the intrinsic got lowered to another call, and that call has
            // a definition then we need to make sure its prototype is emitted
            // before any calls to it.
            if (CallInst *Call = dyn_cast<CallInst>(I))
              if (Function *NewF = Call->getCalledFunction())
                if (!NewF->isDeclaration())
                  prototypesToGen.push_back(NewF);

            break;
          }
}

void CWriter::visitCallInst(CallInst &I) {
  if (isa<InlineAsm>(I.getCalledValue()))
    return visitInlineAsm(I);

  // Handle intrinsic function calls first...
  if (Function *F = I.getCalledFunction())
    if (Intrinsic::ID ID = (Intrinsic::ID)F->getIntrinsicID())
      if (visitBuiltinCall(I, ID))
        return;

  Value *Callee = I.getCalledValue();

  PointerType  *PTy   = cast<PointerType>(Callee->getType());
  FunctionType *FTy   = cast<FunctionType>(PTy->getElementType());

  // If this is a call to a struct-return function, assign to the first
  // parameter instead of passing it to the call.
  const AttributeSet &PAL = I.getAttributes();
  bool hasByVal = I.hasByValArgument();
  bool isStructRet = I.hasStructRetAttr();
  if (isStructRet) {
    writeOperandDeref(I.getArgOperand(0));
    Out << " = ";
  }

  if (I.isTailCall()) Out << " /*tail*/ ";

  // If this is an indirect call to a struct return function, we need to cast
  // the pointer. Ditto for indirect calls with byval arguments.
  bool NeedsCast = (hasByVal || isStructRet) && !isa<Function>(Callee);

  // GCC is a real PITA.  It does not permit codegening casts of functions to
  // function pointers if they are in a call (it generates a trap instruction
  // instead!).  We work around this by inserting a cast to void* in between
  // the function and the function pointer cast.  Unfortunately, we can't just
  // form the constant expression here, because the folder will immediately
  // nuke it.
  //
  // Note finally, that this is completely unsafe.  ANSI C does not guarantee
  // that void* and function pointers have the same size. :( To deal with this
  // in the common case, we handle casts where the number of arguments passed
  // match exactly.
  //
  if (ConstantExpr *CE = dyn_cast<ConstantExpr>(Callee))
    if (CE->isCast())
      if (Function *RF = dyn_cast<Function>(CE->getOperand(0))) {
        NeedsCast = true;
        Callee = RF;
      }

  if (NeedsCast) {
    // Ok, just cast the pointer type.
    Out << "((";
      printTypeName(Out, I.getCalledValue()->getType()->getPointerElementType(), false, PAL);
    Out << "*)(void*)";
  }
  writeOperand(Callee);
  if (NeedsCast) Out << ')';

  Out << '(';

  bool PrintedArg = false;
  if (FTy->isVarArg() && !FTy->getNumParams()) {
    Out << "0 /*dummy arg*/";
    PrintedArg = true;
  }

  unsigned NumDeclaredParams = FTy->getNumParams();
  CallSite CS(&I);
  CallSite::arg_iterator AI = CS.arg_begin(), AE = CS.arg_end();
  unsigned ArgNo = 0;
  if (isStructRet) {   // Skip struct return argument.
    ++AI;
    ++ArgNo;
  }

  Function *F = I.getCalledFunction();
  if (F) {
    StringRef Name = F->getName();
    // emit cast for the first argument to type expected by header prototype
    // the jmp_buf type is an array, so the array-to-pointer decay adds the
    // strange extra *'s
    if (Name == "sigsetjmp")
        Out << "*(sigjmp_buf*)";
    else if (Name == "setjmp")
        Out << "*(jmp_buf*)";
  }

  for (; AI != AE; ++AI, ++ArgNo) {
    if (PrintedArg) Out << ", ";
    if (ArgNo < NumDeclaredParams &&
        (*AI)->getType() != FTy->getParamType(ArgNo)) {
      Out << '(';
      printTypeName(Out, FTy->getParamType(ArgNo),
            /*isSigned=*/PAL.hasAttribute(ArgNo+1, Attribute::SExt));
      Out << ')';
    }
    // Check if the argument is expected to be passed by value.
    if (I.getAttributes().hasAttribute(ArgNo+1, Attribute::ByVal))
      writeOperandDeref(*AI);
    else
      writeOperand(*AI);
    PrintedArg = true;
  }
  Out << ')';
}

/// visitBuiltinCall - Handle the call to the specified builtin.  Returns true
/// if the entire call is handled, return false if it wasn't handled
bool CWriter::visitBuiltinCall(CallInst &I, Intrinsic::ID ID) {
  switch (ID) {
  default: {
    I.dump();
    assert(0 && "Unknown LLVM intrinsic!");
    return false;
  }
  case Intrinsic::vastart:
    Out << "0; ";

    Out << "va_start(*(va_list*)";
    writeOperand(I.getArgOperand(0));
    Out << ", ";
    // Output the last argument to the enclosing function.
    if (I.getParent()->getParent()->arg_empty())
      Out << "vararg_dummy_arg";
    else
      writeOperand(--I.getParent()->getParent()->arg_end());
    Out << ')';
    return true;
  case Intrinsic::vaend:
    if (!isa<ConstantPointerNull>(I.getArgOperand(0))) {
      Out << "0; va_end(*(va_list*)";
      writeOperand(I.getArgOperand(0));
      Out << ')';
    } else {
      Out << "va_end(*(va_list*)0)";
    }
    return true;
  case Intrinsic::vacopy:
    Out << "0; ";
    Out << "va_copy(*(va_list*)";
    writeOperand(I.getArgOperand(0));
    Out << ", *(va_list*)";
    writeOperand(I.getArgOperand(1));
    Out << ')';
    return true;
  case Intrinsic::returnaddress:
    Out << "__builtin_return_address(";
    writeOperand(I.getArgOperand(0));
    Out << ')';
    return true;
  case Intrinsic::frameaddress:
    Out << "__builtin_frame_address(";
    writeOperand(I.getArgOperand(0));
    Out << ')';
    return true;
  case Intrinsic::setjmp:
    Out << "setjmp(*(jmp_buf*)";
    writeOperand(I.getArgOperand(0));
    Out << ')';
    return true;
  case Intrinsic::longjmp:
    Out << "longjmp(*(jmp_buf*)";
    writeOperand(I.getArgOperand(0));
    Out << ", ";
    writeOperand(I.getArgOperand(1));
    Out << ')';
    return true;
  case Intrinsic::sigsetjmp:
    Out << "sigsetjmp(*(sigjmp_buf*)";
    writeOperand(I.getArgOperand(0));
    Out << ',';
    writeOperand(I.getArgOperand(1));
    Out << ')';
    return true;
  case Intrinsic::siglongjmp:
    Out << "siglongjmp(*(sigjmp_buf*)";
    writeOperand(I.getArgOperand(0));
    Out << ", ";
    writeOperand(I.getArgOperand(1));
    Out << ')';
    return true;
  case Intrinsic::prefetch:
    Out << "LLVM_PREFETCH((const void *)";
    writeOperand(I.getArgOperand(0));
    Out << ", ";
    writeOperand(I.getArgOperand(1));
    Out << ", ";
    writeOperand(I.getArgOperand(2));
    Out << ")";
    return true;
  case Intrinsic::stacksave:
    // Emit this as: Val = 0; *((void**)&Val) = __builtin_stack_save()
    // to work around GCC bugs (see PR1809).
    Out << "0; *((void**)&" << GetValueName(&I)
        << ") = __builtin_stack_save()";
    return true;
  case Intrinsic::x86_sse_cmp_ss:
  case Intrinsic::x86_sse_cmp_ps:
  case Intrinsic::x86_sse2_cmp_sd:
  case Intrinsic::x86_sse2_cmp_pd:
    Out << '(';
    printTypeName(Out, I.getType());
    Out << ')';
    // Multiple GCC builtins multiplex onto this intrinsic.
    switch (cast<ConstantInt>(I.getArgOperand(2))->getZExtValue()) {
    default: llvm_unreachable("Invalid llvm.x86.sse.cmp!");
    case 0: Out << "__builtin_ia32_cmpeq"; break;
    case 1: Out << "__builtin_ia32_cmplt"; break;
    case 2: Out << "__builtin_ia32_cmple"; break;
    case 3: Out << "__builtin_ia32_cmpunord"; break;
    case 4: Out << "__builtin_ia32_cmpneq"; break;
    case 5: Out << "__builtin_ia32_cmpnlt"; break;
    case 6: Out << "__builtin_ia32_cmpnle"; break;
    case 7: Out << "__builtin_ia32_cmpord"; break;
    }
    if (ID == Intrinsic::x86_sse_cmp_ps || ID == Intrinsic::x86_sse2_cmp_pd)
      Out << 'p';
    else
      Out << 's';
    if (ID == Intrinsic::x86_sse_cmp_ss || ID == Intrinsic::x86_sse_cmp_ps)
      Out << 's';
    else
      Out << 'd';

    Out << "(";
    writeOperand(I.getArgOperand(0));
    Out << ", ";
    writeOperand(I.getArgOperand(1));
    Out << ")";
    return true;
  case Intrinsic::ppc_altivec_lvsl:
    Out << '(';
    printTypeName(Out, I.getType());
    Out << ')';
    Out << "__builtin_altivec_lvsl(0, (void*)";
    writeOperand(I.getArgOperand(0));
    Out << ")";
    return true;
  case Intrinsic::stackprotector:
    writeOperandDeref(I.getArgOperand(1));
    Out << " = ";
    writeOperand(I.getArgOperand(0));
    return true;
  case Intrinsic::uadd_with_overflow:
  case Intrinsic::sadd_with_overflow:
  case Intrinsic::usub_with_overflow:
  case Intrinsic::ssub_with_overflow:
  case Intrinsic::umul_with_overflow:
  case Intrinsic::smul_with_overflow:
  case Intrinsic::bswap:
  case Intrinsic::ceil:
  case Intrinsic::ctlz:
  case Intrinsic::ctpop:
  case Intrinsic::cttz:
  case Intrinsic::fabs:
  case Intrinsic::floor:
  case Intrinsic::fma:
  case Intrinsic::fmuladd:
  case Intrinsic::pow:
  case Intrinsic::powi:
  case Intrinsic::rint:
  case Intrinsic::sqrt:
  case Intrinsic::trap:
  case Intrinsic::trunc:
    return false; // these use the normal function call emission
  }
}

//This converts the llvm constraint string to something gcc is expecting.
//TODO: work out platform independent constraints and factor those out
//      of the per target tables
//      handle multiple constraint codes
std::string CWriter::InterpretASMConstraint(InlineAsm::ConstraintInfo& c) {
    return TargetLowering::AsmOperandInfo(c).ConstraintCode;
#if 0
  assert(c.Codes.size() == 1 && "Too many asm constraint codes to handle");

  // Grab the translation table from MCAsmInfo if it exists.
  const MCRegisterInfo *MRI;
  const MCAsmInfo *TargetAsm;
  std::string Triple = TheModule->getTargetTriple();
  if (Triple.empty())
    Triple = llvm::sys::getDefaultTargetTriple();

  std::string E;
  if (const Target *Match = TargetRegistry::lookupTarget(Triple, E)) {
    MRI = Match->createMCRegInfo(Triple);
    TargetAsm = Match->createMCAsmInfo(*MRI, Triple);
  } else {
    return c.Codes[0];
  }

  const char *const *table = TargetAsm->getAsmCBE();

  // Search the translation table if it exists.
  for (int i = 0; table && table[i]; i += 2)
    if (c.Codes[0] == table[i]) {
      delete TargetAsm;
      delete MRI;
      return table[i+1];
    }

  // Default is identity.
  delete TargetAsm;
  delete MRI;
  return c.Codes[0];
#endif
}

//TODO: import logic from AsmPrinter.cpp
static std::string gccifyAsm(std::string asmstr) {
  for (std::string::size_type i = 0; i != asmstr.size(); ++i)
    if (asmstr[i] == '\n')
      asmstr.replace(i, 1, "\\n");
    else if (asmstr[i] == '\t')
      asmstr.replace(i, 1, "\\t");
    else if (asmstr[i] == '$') {
      if (asmstr[i + 1] == '{') {
        std::string::size_type a = asmstr.find_first_of(':', i + 1);
        std::string::size_type b = asmstr.find_first_of('}', i + 1);
        std::string n = "%" +
          asmstr.substr(a + 1, b - a - 1) +
          asmstr.substr(i + 2, a - i - 2);
        asmstr.replace(i, b - i + 1, n);
        i += n.size() - 1;
      } else
        asmstr.replace(i, 1, "%");
    }
    else if (asmstr[i] == '%')//grr
      { asmstr.replace(i, 1, "%%"); ++i;}

  return asmstr;
}

//TODO: assumptions about what consume arguments from the call are likely wrong
//      handle communitivity
void CWriter::visitInlineAsm(CallInst &CI) {
  InlineAsm* as = cast<InlineAsm>(CI.getCalledValue());
  InlineAsm::ConstraintInfoVector Constraints = as->ParseConstraints();

  std::vector<std::pair<Value*, int> > ResultVals;
  if (CI.getType() == Type::getVoidTy(CI.getContext()))
    ;
  else if (StructType *ST = dyn_cast<StructType>(CI.getType())) {
    for (unsigned i = 0, e = ST->getNumElements(); i != e; ++i)
      ResultVals.push_back(std::make_pair(&CI, (int)i));
  } else {
    ResultVals.push_back(std::make_pair(&CI, -1));
  }

  // Fix up the asm string for gcc and emit it.
  Out << "__asm__ volatile (\"" << gccifyAsm(as->getAsmString()) << "\"\n";
  Out << "        :";

  unsigned ValueCount = 0;
  bool IsFirst = true;

  // Convert over all the output constraints.
  for (InlineAsm::ConstraintInfoVector::iterator I = Constraints.begin(),
       E = Constraints.end(); I != E; ++I) {

    if (I->Type != InlineAsm::isOutput) {
      ++ValueCount;
      continue;  // Ignore non-output constraints.
    }

    assert(I->Codes.size() == 1 && "Too many asm constraint codes to handle");
    std::string C = InterpretASMConstraint(*I);
    if (C.empty()) continue;

    if (!IsFirst) {
      Out << ", ";
      IsFirst = false;
    }

    // Unpack the dest.
    Value *DestVal;
    int DestValNo = -1;

    if (ValueCount < ResultVals.size()) {
      DestVal = ResultVals[ValueCount].first;
      DestValNo = ResultVals[ValueCount].second;
    } else
      DestVal = CI.getArgOperand(ValueCount-ResultVals.size());

    if (I->isEarlyClobber)
      C = "&"+C;

    Out << "\"=" << C << "\"(" << GetValueName(DestVal);
    if (DestValNo != -1)
      Out << ".field" << DestValNo; // Multiple retvals.
    Out << ")";
    ++ValueCount;
  }


  // Convert over all the input constraints.
  Out << "\n        :";
  IsFirst = true;
  ValueCount = 0;
  for (InlineAsm::ConstraintInfoVector::iterator I = Constraints.begin(),
       E = Constraints.end(); I != E; ++I) {
    if (I->Type != InlineAsm::isInput) {
      ++ValueCount;
      continue;  // Ignore non-input constraints.
    }

    assert(I->Codes.size() == 1 && "Too many asm constraint codes to handle");
    std::string C = InterpretASMConstraint(*I);
    if (C.empty()) continue;

    if (!IsFirst) {
      Out << ", ";
      IsFirst = false;
    }

    assert(ValueCount >= ResultVals.size() && "Input can't refer to result");
    Value *SrcVal = CI.getArgOperand(ValueCount-ResultVals.size());

    Out << "\"" << C << "\"(";
    if (!I->isIndirect)
      writeOperand(SrcVal);
    else
      writeOperandDeref(SrcVal);
    Out << ")";
  }

  // Convert over the clobber constraints.
  IsFirst = true;
  for (InlineAsm::ConstraintInfoVector::iterator I = Constraints.begin(),
       E = Constraints.end(); I != E; ++I) {
    if (I->Type != InlineAsm::isClobber)
      continue;  // Ignore non-input constraints.

    assert(I->Codes.size() == 1 && "Too many asm constraint codes to handle");
    std::string C = InterpretASMConstraint(*I);
    if (C.empty()) continue;

    if (!IsFirst) {
      Out << ", ";
      IsFirst = false;
    }

    Out << '\"' << C << '"';
  }

  Out << ")";
}

void CWriter::visitAllocaInst(AllocaInst &I) {
  Out << '(';
  printTypeName(Out, I.getType());
  Out << ") alloca(sizeof(";
  printTypeName(Out, I.getType()->getElementType());
  if (I.isArrayAllocation()) {
    Out << ") * (" ;
    writeOperand(I.getArraySize());
  }
  Out << "))";
}

void CWriter::printGEPExpression(Value *Ptr, gep_type_iterator I,
                                 gep_type_iterator E) {

  // If there are no indices, just print out the pointer.
  if (I == E) {
    writeOperand(Ptr);
    return;
  }

  // Find out if the last index is into a vector.  If so, we have to print this
  // specially.  Since vectors can't have elements of indexable type, only the
  // last index could possibly be of a vector element.
  VectorType *LastIndexIsVector = 0;
  {
    for (gep_type_iterator TmpI = I; TmpI != E; ++TmpI)
      LastIndexIsVector = dyn_cast<VectorType>(*TmpI);
  }

  Out << "(";

  // If the last index is into a vector, we can't print it as &a[i][j] because
  // we can't index into a vector with j in GCC.  Instead, emit this as
  // (((float*)&a[i])+j)
  if (LastIndexIsVector) {
    Out << "((";
    printTypeName(Out, PointerType::getUnqual(LastIndexIsVector->getElementType()));
    Out << ")(";
  }

  Out << '&';

  // If the first index is 0 (very typical) we can do a number of
  // simplifications to clean up the code.
  Value *FirstOp = I.getOperand();
  if (!isa<Constant>(FirstOp) || !cast<Constant>(FirstOp)->isNullValue()) {
    // First index isn't simple, print it the hard way.
    writeOperand(Ptr);
  } else {
    ++I;  // Skip the zero index.

    // Okay, emit the first operand. If Ptr is something that is already address
    // exposed, like a global, avoid emitting (&foo)[0], just emit foo instead.
    if (isAddressExposed(Ptr)) {
      writeOperandInternal(Ptr);
    } else if (I != E && (*I)->isStructTy()) {
      // If we didn't already emit the first operand, see if we can print it as
      // P->f instead of "P[0].f"
      writeOperand(Ptr);
      Out << "->field" << cast<ConstantInt>(I.getOperand())->getZExtValue();
      ++I;  // eat the struct index as well.
    } else {
      // Instead of emitting P[0][1], emit (*P)[1], which is more idiomatic.
      Out << "(*";
      writeOperand(Ptr);
      Out << ")";
    }
  }

  for (; I != E; ++I) {
    if ((*I)->isStructTy()) {
      Out << ".field" << cast<ConstantInt>(I.getOperand())->getZExtValue();
    } else if ((*I)->isArrayTy()) {
      Out << ".array[";
      writeOperandWithCast(I.getOperand(), Instruction::GetElementPtr);
      Out << ']';
    } else if (!(*I)->isVectorTy()) {
      Out << '[';
      writeOperandWithCast(I.getOperand(), Instruction::GetElementPtr);
      Out << ']';
    } else {
      // If the last index is into a vector, then print it out as "+j)".  This
      // works with the 'LastIndexIsVector' code above.
      if (isa<Constant>(I.getOperand()) &&
          cast<Constant>(I.getOperand())->isNullValue()) {
        Out << "))";  // avoid "+0".
      } else {
        Out << ")+(";
        writeOperandWithCast(I.getOperand(), Instruction::GetElementPtr);
        Out << "))";
      }
    }
  }
  Out << ")";
}

void CWriter::writeMemoryAccess(Value *Operand, Type *OperandType,
                                bool IsVolatile, unsigned Alignment /*bytes*/) {
  if (isAddressExposed(Operand)) {
    writeOperandInternal(Operand);
    return;
  }

  bool IsUnaligned = Alignment &&
    Alignment < TD->getABITypeAlignment(OperandType);

  if (!IsUnaligned)
    Out << '*';

  else if (IsVolatile || IsUnaligned) {
    Out << "(";
    if (IsUnaligned) Out << "(struct { ";
    printTypeName(Out, OperandType, false);
    if (IsUnaligned) Out << " data __attribute__ ((packed, aligned(" << Alignment << "))); } ";
    if (IsVolatile) Out << "volatile";
    Out << "*)";
  }

  writeOperand(Operand);

  if (IsUnaligned) {
    Out << ")->data";
  }
}

void CWriter::visitLoadInst(LoadInst &I) {
  writeMemoryAccess(I.getOperand(0), I.getType(), I.isVolatile(),
                    I.getAlignment());

}

void CWriter::visitStoreInst(StoreInst &I) {
  writeMemoryAccess(I.getPointerOperand(), I.getOperand(0)->getType(),
                    I.isVolatile(), I.getAlignment());
  Out << " = ";
  Value *Operand = I.getOperand(0);
  unsigned BitMask = 0;
  if (IntegerType* ITy = dyn_cast<IntegerType>(Operand->getType()))
    if (!ITy->isPowerOf2ByteWidth())
      // We have a bit width that doesn't match an even power-of-2 byte
      // size. Consequently we must & the value with the type's bit mask
      BitMask = ITy->getBitMask();
  if (BitMask)
    Out << "((";
  writeOperand(Operand);
  if (BitMask)
    Out << ") & " << BitMask << ")";
}

void CWriter::visitGetElementPtrInst(GetElementPtrInst &I) {
  printGEPExpression(I.getPointerOperand(), gep_type_begin(I),
                     gep_type_end(I));
}

void CWriter::visitVAArgInst(VAArgInst &I) {
  Out << "va_arg(*(va_list*)";
  writeOperand(I.getOperand(0));
  Out << ", ";
  printTypeName(Out, I.getType());
  Out << ");\n ";
}

void CWriter::visitInsertElementInst(InsertElementInst &I) {
  Type *EltTy = I.getType()->getElementType();
  writeOperand(I.getOperand(0));
  Out << ";\n  ";
  Out << "((";
  printTypeName(Out, PointerType::getUnqual(EltTy));
  Out << ")(&" << GetValueName(&I) << "))[";
  writeOperand(I.getOperand(2));
  Out << "] = (";
  writeOperand(I.getOperand(1));
  Out << ")";
}

void CWriter::visitExtractElementInst(ExtractElementInst &I) {
  // We know that our operand is not inlined.
  Out << "((";
  Type *EltTy =
    cast<VectorType>(I.getOperand(0)->getType())->getElementType();
  printTypeName(Out, PointerType::getUnqual(EltTy));
  Out << ")(&" << GetValueName(I.getOperand(0)) << "))[";
  writeOperand(I.getOperand(1));
  Out << "]";
}

// <result> = shufflevector <n x <ty>> <v1>, <n x <ty>> <v2>, <m x i32> <mask>
// ; yields <m x <ty>>
void CWriter::visitShuffleVectorInst(ShuffleVectorInst &SVI) {
  Out << "(";
  printTypeName(Out, SVI.getType());
  Out << "){ ";
  VectorType *VT = SVI.getType();
  unsigned NumElts = VT->getNumElements();
  VectorType *InputVT = cast<VectorType>(SVI.getOperand(0)->getType());
  unsigned NumInputElts = InputVT->getNumElements(); // n
  Type *EltTy = VT->getElementType();

  for (unsigned i = 0; i != NumElts; ++i) {
    if (i) Out << ", ";
    int SrcVal = SVI.getMaskValue(i);
    if ((unsigned)SrcVal >= NumInputElts * 2) {
      Out << " 0/*undef*/ ";
    } else {
      // If SrcVal belongs [0, n - 1], it extracts value from <v1>
      // If SrcVal belongs [n, 2 * n - 1], it extracts value from <v2>
      // In C++, the value false is converted to zero and the value true is
      // converted to one
      Value *Op = SVI.getOperand((unsigned)SrcVal >= NumInputElts);
      if (isa<Instruction>(Op)) {
        // Do an extractelement of this value from the appropriate input.
        Out << "((";
        printTypeName(Out, PointerType::getUnqual(EltTy));
        Out << ")(&" << GetValueName(Op)
            << "))[" << ((unsigned)SrcVal >= NumInputElts ? SrcVal - NumInputElts : SrcVal) << "]";
      } else if (isa<ConstantAggregateZero>(Op) || isa<UndefValue>(Op)) {
        Out << "0";
      } else {
        printConstant(cast<ConstantVector>(Op)->getOperand(SrcVal &
                                                           (NumElts-1)),
                      false);
      }
    }
  }
  Out << "}";
}

void CWriter::visitInsertValueInst(InsertValueInst &IVI) {
  // Start by copying the entire aggregate value into the result variable.
  writeOperand(IVI.getOperand(0));
  Out << ";\n  ";

  // Then do the insert to update the field.
  Out << GetValueName(&IVI);
  for (const unsigned *b = IVI.idx_begin(), *i = b, *e = IVI.idx_end();
       i != e; ++i) {
    Type *IndexedTy =
      ExtractValueInst::getIndexedType(IVI.getOperand(0)->getType(),
                                       makeArrayRef(b, i));
    assert(IndexedTy);
    if (IndexedTy->isArrayTy())
      Out << ".array[" << *i << "]";
    else
      Out << ".field" << *i;
  }
  Out << " = ";
  writeOperand(IVI.getOperand(1));
}

void CWriter::visitExtractValueInst(ExtractValueInst &EVI) {
  Out << "(";
  if (isa<UndefValue>(EVI.getOperand(0))) {
    Out << "(";
    printTypeName(Out, EVI.getType());
    Out << ") 0/*UNDEF*/";
  } else {
    writeOperand(EVI.getOperand(0));
    for (const unsigned *b = EVI.idx_begin(), *i = b, *e = EVI.idx_end();
         i != e; ++i) {
      Type *IndexedTy =
        ExtractValueInst::getIndexedType(EVI.getOperand(0)->getType(),
                                         makeArrayRef(b, i));
      if (IndexedTy->isArrayTy())
        Out << ".array[" << *i << "]";
      else
        Out << ".field" << *i;
    }
  }
  Out << ")";
}

//===----------------------------------------------------------------------===//
//                       External Interface declaration
//===----------------------------------------------------------------------===//

bool CTargetMachine::addPassesToEmitFile(
    PassManagerBase &PM, raw_pwrite_stream &Out, CodeGenFileType FileType,
    bool DisableVerify, AnalysisID StartBefore,
    AnalysisID StartAfter, AnalysisID StopAfter,
    MachineFunctionInitializer *MFInitializer) {

  if (FileType != TargetMachine::CGFT_AssemblyFile) return true;

  PM.add(createGCLoweringPass());
  PM.add(createLowerInvokePass());
  PM.add(createCFGSimplificationPass());   // clean up after lower invoke.
  PM.add(new CWriter(Out));
  return false;
}
