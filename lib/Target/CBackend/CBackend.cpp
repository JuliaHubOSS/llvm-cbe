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

#define	DEBUG	0	
#define PRINTSIGNED 0

#include "CWriter.h"
#if DEBUG
#include <iostream>
#endif


//#include "Graph.h"

#include <tr1/unordered_map>


// Some ms header decided to define setjmp as _setjmp, undo this for this file.
#ifdef _MSC_VER
#undef setjmp
#endif
using namespace llvm;

std::tr1::unordered_map<int, std::string> VarNamesMap;

extern "C" void LLVMInitializeCBackendTarget() {
 // Register the target.
 RegisterTargetMachine<CTargetMachine> X(TheCBackendTarget);
}

namespace {
 class CBEMCAsmInfo : public MCAsmInfo {
  public:
   CBEMCAsmInfo() {
    PrivateGlobalPrefix = "";
   }
 };
}

char CWriter::ID = 0;



static std::string CBEMangle(const std::string &S) {
 std::string Result;

 for (unsigned i = 0, e = S.size(); i != e; ++i)
  if (isalnum(S[i]) || S[i] == '_') {
   Result += S[i];
  } 
  else {
   Result += '_';
   Result += 'A'+(S[i]&15);
   Result += 'A'+((S[i]>>4)&15);
   Result += '_';
  }
 return Result;
}

std::string CWriter::getStructName(StructType *ST) {
 if (!ST->isLiteral() && !ST->getName().empty())
  return CBEMangle("l_"+ST->getName().str());

 return "l_unnamed_" + utostr(UnnamedStructIDs[ST]);
}


/// printStructReturnPointerFunctionType - This is like printType for a struct
/// return type, except, instead of printing the type as void (*)(Struct*, ...)
/// print it as "Struct (*)(...)", for struct return functions.
void CWriter::printStructReturnPointerFunctionType(raw_ostream &Out,
                                                   const AttributeSet &PAL,
                                                   PointerType *TheTy) {
 FunctionType *FTy = cast<FunctionType>(TheTy->getElementType());
 std::string tstr;
 raw_string_ostream FunctionInnards(tstr);
 FunctionInnards << " (*) (";
 bool PrintedType = false;

 FunctionType::param_iterator I = FTy->param_begin(), E = FTy->param_end();
 Type *RetTy = cast<PointerType>(*I)->getElementType();
 unsigned Idx = 1;
 for (++I, ++Idx; I != E; ++I, ++Idx) {
  if (PrintedType)
   FunctionInnards << ", ";
   Type *ArgTy = *I;
   if (PAL.hasAttribute(Idx, Attribute::ByVal)) {
    assert(ArgTy->isPointerTy());
    ArgTy = cast<PointerType>(ArgTy)->getElementType();
   }
   printType(FunctionInnards, ArgTy,
   !PAL.hasAttribute(Idx, Attribute::ZExt),
    "");
   PrintedType = true;
 }
 if (FTy->isVarArg()) {
  if (!PrintedType)
   FunctionInnards << " int"; //dummy argument for empty vararg functs
  FunctionInnards << ", ...";
 } else if (!PrintedType) {
    FunctionInnards << "void";
 }
 FunctionInnards << ')';
 printType(Out, RetTy,
    /*isSigned=*/!PAL.hasAttribute(AttributeSet::ReturnIndex, Attribute::ZExt),
    FunctionInnards.str());
}

raw_ostream &
CWriter::printSimpleType(raw_ostream &Out, Type *Ty, bool isSigned,
                         const std::string &NameSoFar) {
 assert(((Ty->getTypeID() >= 0 && Ty->getTypeID() <= 9) || Ty->isIntegerTy() || Ty->isVectorTy()) &&
        "Invalid type for printSimpleType");
 switch (Ty->getTypeID()) {
 case Type::VoidTyID:   return Out << "void " << NameSoFar;
 case Type::IntegerTyID: {
  unsigned NumBits = cast<IntegerType>(Ty)->getBitWidth();
  if (NumBits == 1)
   return Out << "bool " << NameSoFar;
  else if (NumBits <= 8)
   return Out << (isSigned?"":"unsigned ") << "char " << NameSoFar;
  else if (NumBits <= 16)
   return Out << (isSigned?"":"unsigned ") << "short " << NameSoFar;
  else if (NumBits <= 32)
   return Out << (isSigned?"":"unsigned ") << "int " << NameSoFar;
  else if (NumBits <= 64)
   return Out << (isSigned?"":"unsigned ") << "long long "<< NameSoFar;
  else {
   assert(NumBits <= 128 && "Bit widths > 128 not implemented yet");
   return Out << (isSigned?"llvmInt128":"llvmUInt128") << " " << NameSoFar;
  }
  }
  case Type::FloatTyID:  return Out << "float "   << NameSoFar;
  case Type::DoubleTyID: return Out << "double "  << NameSoFar;
  // Lacking emulation of FP80 on PPC, etc., we assume whichever of these is
  // present matches host 'long double'.
  case Type::X86_FP80TyID:
  case Type::PPC_FP128TyID:
  case Type::FP128TyID:  return Out << "long double " << NameSoFar;

  case Type::X86_MMXTyID:
   return printSimpleType(Out, Type::getInt32Ty(Ty->getContext()), isSigned,
                    " __attribute__((vector_size(64))) " + NameSoFar);

  case Type::VectorTyID: {
   VectorType *VTy = cast<VectorType>(Ty);
   return printSimpleType(Out, VTy->getElementType(), isSigned,
                    " __attribute__((vector_size(" +
                    utostr(TD->getTypeAllocSize(VTy)) + " ))) " + NameSoFar);
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
raw_ostream &CWriter::printType(raw_ostream &Out, Type *Ty,
                                bool isSigned, const std::string &NameSoFar,
                                bool IgnoreName, const AttributeSet &PAL) {
 if (PRINTSIGNED)  isSigned = true;
 if ((Ty->getTypeID() >= 0 && Ty->getTypeID() <= 9) || Ty->isIntegerTy() || Ty->isVectorTy()) {
  printSimpleType(Out, Ty, isSigned, NameSoFar);
  return Out;
 }

 switch (Ty->getTypeID()) {
 case Type::FunctionTyID: {
  FunctionType *FTy = cast<FunctionType>(Ty);
  std::string tstr;
  raw_string_ostream FunctionInnards(tstr);
  FunctionInnards << " (" << NameSoFar << ") (";
  unsigned Idx = 1;
  for (FunctionType::param_iterator I = FTy->param_begin(),
        E = FTy->param_end(); I != E; ++I) {
   Type *ArgTy = *I;
      
   if (PAL.hasAttribute(Idx, Attribute::ByVal)) {
    assert(ArgTy->isPointerTy());
    ArgTy = cast<PointerType>(ArgTy)->getElementType();
   }
   if (I != FTy->param_begin())
    FunctionInnards << ", ";
    printType(FunctionInnards, ArgTy,
       !PAL.hasAttribute(Idx, Attribute::ZExt),
       "");
    ++Idx;
   }
   if (FTy->isVarArg()) {
    if (!FTy->getNumParams())
     FunctionInnards << " int"; //dummy argument for empty vaarg functs
     FunctionInnards << ", ...";
   } else if (!FTy->getNumParams()) {
     FunctionInnards << "void";
   }
   FunctionInnards << ')';
   printType(Out, FTy->getReturnType(),
      !PAL.hasAttribute(AttributeSet::ReturnIndex, Attribute::ZExt),
      FunctionInnards.str());
   return Out;
 }
 case Type::StructTyID: {
  StructType *STy = cast<StructType>(Ty);

  // Check to see if the type is named.
  if (!IgnoreName)
   return Out << getStructName(STy) << ' ' << NameSoFar;

  Out << "struct " + NameSoFar + " {\n";
  unsigned Idx = 0;
  for (StructType::element_iterator I = STy->element_begin(),
          E = STy->element_end(); I != E; ++I) {
   Out << "  ";
   printType(Out, *I, false, "field" + utostr(Idx++));
   Out << ";\n";
  }
  Out << '}';
  if (STy->isPacked())
   Out << " __attribute__ ((packed))";
  return Out;
 }

 case Type::PointerTyID: {
  PointerType *PTy = cast<PointerType>(Ty);
  std::string ptrName = "*" + NameSoFar;
  std::string temp_str = "";

  if (PTy->getElementType()->isArrayTy() ||
        PTy->getElementType()->isVectorTy())
   ptrName = "(" + ptrName + ")";

  if (!PAL.isEmpty())
   // Must be a function ptr cast!
   return printType(Out, PTy->getElementType(), false, ptrName, true, PAL);
  if (PTy->getElementType()->isArrayTy() ||
        PTy->getElementType()->isVectorTy()) {
   printType(Out, PTy->getElementType(), false, temp_str);
   Out << temp_str << " " << ptrName;
   return Out;
  } else {
    return printType(Out, PTy->getElementType(), false, ptrName);
  }
 }

 case Type::ArrayTyID: {
  ArrayType *ATy = cast<ArrayType>(Ty);
  unsigned NumElements = ATy->getNumElements();
  if (NumElements == 0) NumElements = 1;
  // Arrays are wrapped in structs to allow them to have normal
  // value semantics (avoiding the array "decay").
  Out << "struct { ";
  printType(Out, ATy->getElementType(), false,
             "array[" + utostr(NumElements) + "]");
  return Out << "; } " << NameSoFar << " ";
 }

 default:
  llvm_unreachable("Unhandled case in getTypeProps!");
 }
}

void CWriter::printConstantArray(ConstantArray *CPA, bool Static) {
 Out << "{ ";
 printConstant(cast<Constant>(CPA->getOperand(0)), Static);
 for (unsigned i = 1, e = CPA->getNumOperands(); i != e; ++i) {
  Out << ", ";
  printConstant(cast<Constant>(CPA->getOperand(i)), Static);
 }
 Out << " }";
}

void CWriter::printConstantVector(ConstantVector *CP, bool Static) {
 Out << "{ ";
 printConstant(cast<Constant>(CP->getOperand(0)), Static);
 for (unsigned i = 1, e = CP->getNumOperands(); i != e; ++i) {
  Out << ", ";
  printConstant(cast<Constant>(CP->getOperand(i)), Static);
 }
 Out << " }";
}

void CWriter::printConstantDataSequential(ConstantDataSequential *CDS,
                                          bool Static) {
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
    printConstant(CDS->getElementAsConstant(0), Static);
    for (unsigned i = 1, e = CDS->getNumElements(); i != e; ++i) {
     Out << ", ";
     printConstant(CDS->getElementAsConstant(i), Static);
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
   printType(Out, DstTy);
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
   Out << "(unsigned long)";
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
void CWriter::printConstant(Constant *CPV, bool Static) {
 if (const ConstantExpr *CE = dyn_cast<ConstantExpr>(CPV)) {
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
    printConstant(CE->getOperand(0), Static);
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
    printGEPExpression(CE->getOperand(0), gep_type_begin(CPV),
                        gep_type_end(CPV), Static);
    Out << ")";
    return;
   case Instruction::Select:
    Out << '(';
    printConstant(CE->getOperand(0), Static);
    Out << '?';
    printConstant(CE->getOperand(1), Static);
    Out << ':';
    printConstant(CE->getOperand(2), Static);
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
    bool NeedsClosingParens = printConstExprCast(CE, Static);
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
    bool NeedsClosingParens = printConstExprCast(CE, Static);
    if (CE->getPredicate() == FCmpInst::FCMP_FALSE)
     Out << "0";
    else if (CE->getPredicate() == FCmpInst::FCMP_TRUE)
     Out << "1";
    else {
     const char* op = 0;
     switch (CE->getPredicate()) {
      default: llvm_unreachable("Illegal FCmp predicate");
      case FCmpInst::FCMP_ORD: op = "ord"; break;
      case FCmpInst::FCMP_UNO: op = "uno"; break;
      case FCmpInst::FCMP_UEQ: op = "ueq"; break;
      case FCmpInst::FCMP_UNE: op = "une"; break;
      case FCmpInst::FCMP_ULT: op = "ult"; break;
      case FCmpInst::FCMP_ULE: op = "ule"; break;
      case FCmpInst::FCMP_UGT: op = "ugt"; break;
      case FCmpInst::FCMP_UGE: op = "uge"; break;
      case FCmpInst::FCMP_OEQ: op = "oeq"; break;
      case FCmpInst::FCMP_ONE: op = "one"; break;
      case FCmpInst::FCMP_OLT: op = "olt"; break;
      case FCmpInst::FCMP_OLE: op = "ole"; break;
      case FCmpInst::FCMP_OGT: op = "ogt"; break;
      case FCmpInst::FCMP_OGE: op = "oge"; break;
     }
     Out << "llvm_fcmp_" << op << "(";
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
 errs() << "CWriter Error: Unhandled constant expression: " << *CE << "\n";
#endif
 llvm_unreachable(0);
 }
} else if (isa<UndefValue>(CPV) && CPV->getType()->isSingleValueType()) {
   Out << "((";
   printType(Out, CPV->getType()); // sign doesn't matter
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
  if (Ty == Type::getInt1Ty(CPV->getContext()))
   Out << (CI->getZExtValue() ? '1' : '0');
  else if (Ty == Type::getInt32Ty(CPV->getContext()))
  {
   if(!PRINTSIGNED) 
    Out << CI->getZExtValue() << 'u';
   else if(PRINTSIGNED && CI->isNegative()){
    int NegVal = CI->getZExtValue();
    Out << NegVal;
   }
   else
    Out << CI->getZExtValue();
  }
  else if (Ty->getPrimitiveSizeInBits() > 32)
   Out << CI->getZExtValue() << "ull";
  else {
   Out << "((";
   printSimpleType(Out, Ty, false) << ')';
   if (CI->isMinValue(true))
    Out << CI->getZExtValue() << 'u';
   else
    Out << CI->getSExtValue();
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

   if (IsNAN(V)) {
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
    } else if (IsInf(V)) {
       // The value is Inf
       if (V < 0) Out << '-';
       Out << "LLVM_INF" <<
          (FPC->getType() == Type::getFloatTy(FPC->getContext()) ? "F" : "")
          << " /*inf*/ ";
    } else {
       std::string Num;
       Num = ftostr(FPC->getValueAPF());
       Out << Num;
    }
   }
   break;
 }

 case Type::ArrayTyID:
  // Use C99 compound expression literal initializer syntax.
  if (!Static) {
   Out << "(";
   printType(Out, CPV->getType());
   Out << ")";
  }
  Out << "{ "; // Arrays are wrapped in struct types.
  if (ConstantArray *CA = dyn_cast<ConstantArray>(CPV)) {
   printConstantArray(CA, Static);
  } else if (ConstantDataSequential *CDS =
               dyn_cast<ConstantDataSequential>(CPV)) {
     printConstantDataSequential(CDS, Static);
  } else {
     assert(isa<ConstantAggregateZero>(CPV) || isa<UndefValue>(CPV));
     ArrayType *AT = cast<ArrayType>(CPV->getType());
     Out << '{';
     if (AT->getNumElements()) {
      Out << ' ';
      Constant *CZ = Constant::getNullValue(AT->getElementType());
      printConstant(CZ, Static);
      for (unsigned i = 1, e = AT->getNumElements(); i != e; ++i) {
       Out << ", ";
       printConstant(CZ, Static);
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
   printType(Out, CPV->getType());
   Out << ")";
  }
  if (ConstantVector *CV = dyn_cast<ConstantVector>(CPV)) {
   printConstantVector(CV, Static);
  } else if (ConstantDataSequential *CDS =
             dyn_cast<ConstantDataSequential>(CPV)) {
     printConstantDataSequential(CDS, Static);
  } else {
     assert(isa<ConstantAggregateZero>(CPV) || isa<UndefValue>(CPV));
     VectorType *VT = cast<VectorType>(CPV->getType());
     Out << "{ ";
     Constant *CZ = Constant::getNullValue(VT->getElementType());
     printConstant(CZ, Static);
     for (unsigned i = 1, e = VT->getNumElements(); i != e; ++i) {
      Out << ", ";
      printConstant(CZ, Static);
     }
     Out << " }";
  }
  break;

 case Type::StructTyID:
  // Use C99 compound expression literal initializer syntax.
  if (!Static) {
   Out << "(";
   printType(Out, CPV->getType());
   Out << ")";
  }
  if (isa<ConstantAggregateZero>(CPV) || isa<UndefValue>(CPV)) {
   StructType *ST = cast<StructType>(CPV->getType());
   Out << '{';
   if (ST->getNumElements()) {
    Out << ' ';
    printConstant(Constant::getNullValue(ST->getElementType(0)), Static);
    for (unsigned i = 1, e = ST->getNumElements(); i != e; ++i) {
     Out << ", ";
     printConstant(Constant::getNullValue(ST->getElementType(i)), Static);
    }
   }
   Out << " }";
  } else {
     Out << '{';
     if (CPV->getNumOperands()) {
      Out << ' ';
      printConstant(cast<Constant>(CPV->getOperand(0)), Static);
      for (unsigned i = 1, e = CPV->getNumOperands(); i != e; ++i) {
       Out << ", ";
       printConstant(cast<Constant>(CPV->getOperand(i)), Static);
      }
     }
     Out << " }";
   }
   break;

 case Type::PointerTyID:
  if (isa<ConstantPointerNull>(CPV)) {
   Out << "((";
   printType(Out, CPV->getType()); // sign doesn't matter
   Out << ")/*NULL*/0)";
   break;
  } else if (GlobalValue *GV = dyn_cast<GlobalValue>(CPV)) {
     writeOperand(GV, Static);
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
bool CWriter::printConstExprCast(const ConstantExpr* CE, bool Static) {
 bool NeedsExplicitCast = false;
 Type *Ty = CE->getOperand(0)->getType();
 bool TypeIsSigned = false;
 switch (CE->getOpcode()) {
  case Instruction::Add:
  case Instruction::Sub:
  case Instruction::Mul:
   break;
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
  if (Ty->isIntegerTy() && Ty != Type::getInt1Ty(Ty->getContext()))
   printSimpleType(Out, Ty, TypeIsSigned);
  else
   printType(Out, Ty); // not integer, sign doesn't matter
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
   break;
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

std::string CWriter::GetValueName(const Value *Operand) {

 // Resolve potential alias.
 if (const GlobalAlias *GA = dyn_cast<GlobalAlias>(Operand)) {
  if (const Value *V = GA->resolveAliasedGlobal(false))
   Operand = V;
 }

 // Mangle globals with the standard mangler interface for LLC compatibility.
 if (const GlobalValue *GV = dyn_cast<GlobalValue>(Operand)) {
  SmallString<128> Str;
  Mang->getNameWithPrefix(Str, GV, false);
  return CBEMangle(Str.str().str());
 }

 std::string Name = Operand->getName();
 size_t GlobalPos = 0;
 std::string Expression;
 if (Name.empty()) { // Assign unique names to local temporaries.
  raw_string_ostream S (Expression);
  S << *Operand;

  GlobalPos = Expression.find('@');

  if (GlobalPos > Expression.length())
   GlobalPos = Expression.find('%', 3);
  ++GlobalPos;
  for(; GlobalPos < Expression.length(); ++GlobalPos)
  {
   if(Expression.at(GlobalPos) == ',')
    break;
   else
    Name += Expression.at(GlobalPos);
  }
  if (Name[0] >= '0' && Name[0] <= '9'){
   int Num = (int) Name[0] - '0';
   Num++;
   Name = VarNamesMap[Num];
  }

  unsigned &No = AnonValueNumbers[Operand];

  if (No == 0)
   No = ++NextAnonValueNumber;
  VarNamesMap[No] = Name;
  Name = Name + "_"+utostr(No);
 }
 
 std::string VarName;
 VarName.reserve(Name.capacity());

 for (std::string::iterator I = Name.begin(), E = Name.end();
      I != E; ++I) {
  char ch = *I;

  if (!((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') ||
        (ch >= '0' && ch <= '9') || ch == '_')) {
   char buffer[5];
   sprintf(buffer, "_%x_", ch);
   VarName += buffer;
  } else
     VarName += ch;
 }

 return VarName;
}

/// writeInstComputationInline - Emit the computation for the specified
/// instruction inline, with no destination provided.
void CWriter::writeInstComputationInline(Instruction &I) {
 // We can't currently support integer types other than 1, 8, 16, 32, 64.
 // Validate this.
 Type *Ty = I.getType();
 if (Ty->isIntegerTy() && (Ty!=Type::getInt1Ty(I.getContext()) &&
       Ty!=Type::getInt8Ty(I.getContext()) &&
       Ty!=Type::getInt16Ty(I.getContext()) &&
       Ty!=Type::getInt32Ty(I.getContext()) &&
       Ty!=Type::getInt64Ty(I.getContext()))) {
  report_fatal_error("The C backend does not currently support integer "
                       "types of widths other than 1, 8, 16, 32, 64.\n"
                       "This is being tracked as PR 4158.");
 }

 // If this is a non-trivial bool computation, make sure to truncate down tof
 // a 1 bit value.  This is important because we want "add i1 x, y" to return
 // "0" when x and y are true, not "2" for example.
 bool NeedBoolTrunc = false;
 if (I.getType() == Type::getInt1Ty(I.getContext()) &&
     !isa<ICmpInst>(I) && !isa<FCmpInst>(I))
  NeedBoolTrunc = true;

 if (NeedBoolTrunc)
  Out << "((";

 visit(I);

 if (NeedBoolTrunc)
  Out << ")&1)";
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

// Some instructions need to have their result value casted back to the
// original types because their operands were casted to the expected type.
// This function takes care of detecting that case and printing the cast
// for the Instruction.
bool CWriter::writeInstructionCast(const Instruction &I) {
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
void CWriter::writeOperandWithCast(Value* Operand, unsigned Opcode) {

 // Extract the operand's type, we'll need it.
 Type* OpTy = Operand->getType();

 // Indicate whether to do the cast or not.
 bool shouldCast = false;

 // Indicate whether the cast should be to a signed type or not.
 bool castIsSigned = false;

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
   shouldCast = false;//true;
   castIsSigned = false;
   break;
  case Instruction::GetElementPtr:
  case Instruction::AShr:
  case Instruction::SDiv:
  case Instruction::SRem: // Cast to signed first
   shouldCast = false;//true;
   castIsSigned = true;
   break;
 }

 // Write out the casted operand if we should, otherwise just write the
 // operand.
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
void CWriter::writeOperandWithCast(Value* Operand, const ICmpInst &Cmp) {
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
static void generateCompilerSpecificCode(formatted_raw_ostream& Out,
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
 //Out << "#define CODE_FOR_MAIN() /* Any target-specific code for main()*/\n";
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
static SpecialGlobalClass getGlobalVariableClass(const GlobalVariable *GV) {
 // If this is a global ctors/dtors list, handle it now.
 if (GV->hasAppendingLinkage() && GV->use_empty()) {
  if (GV->getName() == "llvm.global_ctors")
   return GlobalCtors;
  else if (GV->getName() == "llvm.global_dtors")
   return GlobalDtors;
 }

 // Otherwise, if it is other metadata, don't print it.  This catches things
 // like debug information.
 if (GV->getSection() == "llvm.metadata")
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
 FunctionPass::doInitialization(M);

 // Initialize
 TheModule = &M;

 TD = new DataLayout(&M);
 IL = new IntrinsicLowering(*TD);
 IL->AddPrototypes(M);
 
 G = new Graph(&M);
 G->makeGraph();

#if DEBUG
 G->printGraphFromMod();
 G->printGraph();
 G->print();
#endif

 G->createMap();

 next = NULL;


 TAsm = new CBEMCAsmInfo();
 MRI  = new MCRegisterInfo();
 TCtx = new MCContext(TAsm, MRI, NULL);
 // NOTE: TargetMachine is not used in the Prefix function, which is the only
 // one the CBackend uses.

 Mang = new Mangler(TD);

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
     << "\n\n/* Global Declarations */\n";

 // First output all the declarations for the program, because C requires
 // Functions & globals to be declared before they are used.
 //
 if (!M.getModuleInlineAsm().empty()) {
  Out << "/* Module asm statements */\n"
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

 // Loop over the symbol table, emitting all named constants.
 printModuleTypes();

 // Global variable declarations...
 if (!M.global_empty()) {
  Out << "\n/* External Global Variable Declarations */\n";
  for (Module::global_iterator I = M.global_begin(), E = M.global_end();
       I != E; ++I) { 

   if (I->getType()->getElementType()->isArrayTy()) {
    if (nameToType.find(GetValueName(I)) == nameToType.end() ) {
     Out << "typedef ";
     printType(Out, I->getType()->getElementType(), false, GetValueName(I) + std::string("_t"));
     nameToType[GetValueName(I)] = GetValueName(I) + std::string("_t");
     Out << ";\n";
    }
   }

   if (I->hasExternalLinkage() || I->hasExternalWeakLinkage() || I->hasCommonLinkage())
    Out << "extern ";
   
    else
     continue; // Internal Global

    // Thread Local Storage
    if (I->isThreadLocal())
     Out << "__thread ";

    if (I->getType()->getElementType()->isArrayTy()) {
     if (nameToType.find(GetValueName(I)) != nameToType.end() ) {
         Out << nameToType[GetValueName(I)] << " " << GetValueName(I);
     }
    } else {
       printType(Out, I->getType()->getElementType(), false, GetValueName(I));
    }

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
 SmallVector<const Function*, 8> intrinsicsToDefine;

 for (Module::iterator I = M.begin(), E = M.end(); I != E; ++I) {
  // Don't print declarations for intrinsic functions.
  // Store the used intrinsics, which need to be explicitly defined.
  if (I->isIntrinsic()) {
   switch (I->getIntrinsicID()) {
    default:
     break;
    case Intrinsic::uadd_with_overflow:
    case Intrinsic::sadd_with_overflow:
     intrinsicsToDefine.push_back(I);
     break;
   }
   continue;
  }

 if (I->getName() == "setjmp" ||
     I->getName() == "longjmp" || I->getName() == "_setjmp")
  continue;

 
 Out << "extern ";
 printFunctionSignature(I, true);

 if (I->hasName() && I->getName()[0] == 1)
     Out << " LLVM_ASM(\"" << I->getName().substr(1) << "\")";

  Out << ";\n";
 }

 // Output the global variable declarations
 if (!M.global_empty()) {
  Out << "\n\n/* Global Variable Declarations */\n";
  for (Module::global_iterator I = M.global_begin(), E = M.global_end();
       I != E; ++I)
   if (!I->isDeclaration() && !I->hasLocalLinkage()) {
    // Ignore special globals, such as debug info.
    if (getGlobalVariableClass(I))
     continue;

    if ( I->getType()->getElementType()->isArrayTy()) {
     if (nameToType.find(GetValueName(I)) == nameToType.end() ) {
      Out << "typedef ";
      printType(Out, I->getType()->getElementType(), false, GetValueName(I) + std::string("_t"));
      nameToType[GetValueName(I)] = GetValueName(I) + std::string("_t");
      Out << ";\n";
     }
    }

    Out << "extern ";

    // Thread Local Storage
    if (I->isThreadLocal())
     Out << "__thread ";

    if ( I->getType()->getElementType()->isArrayTy()) {
     if (nameToType.find(GetValueName(I)) != nameToType.end() ) {
      Out << nameToType[GetValueName(I)] << " " << GetValueName(I);
     }
    } else {
       printType(Out, I->getType()->getElementType(), false, GetValueName(I));
    }

    if (I->hasLinkOnceLinkage())
     Out << " __attribute__((common))";
    else if (I->hasCommonLinkage())     // FIXME is this right?
     Out << " __ATTRIBUTE_WEAK__";
    else if (I->hasWeakLinkage())
     Out << " __ATTRIBUTE_WEAK__";
    else if (I->hasExternalWeakLinkage())
     Out << " __EXTERNAL_WEAK__";
    if (I->hasHiddenVisibility())
     Out << " __HIDDEN__";
    Out << ";\n";
   }
 }

 // Output the global variable definitions and contents...
 if (!M.global_empty()) {
  Out << "\n\n/* Global Variable Definitions and Initialization */\n";
  for (Module::global_iterator I = M.global_begin(), E = M.global_end();
       I != E; ++I)
   if (!I->isDeclaration()) {
    // Ignore special globals, such as debug info.
    if (getGlobalVariableClass(I))
     continue;

    if (I->getType()->getElementType()->isArrayTy()) {
     if (nameToType.find(GetValueName(I)) == nameToType.end() ) {
      Out << "typedef ";
      printType(Out, I->getType()->getElementType(), false, GetValueName(I) + std::string("_t"));
      nameToType[GetValueName(I)] = GetValueName(I) + std::string("_t");
      Out << ";\n";
     }
    }

    if (I->hasLocalLinkage())
     Out << "static ";

    // Thread Local Storage
    if (I->isThreadLocal())
     Out << "__thread ";

    if (I->getType()->getElementType()->isArrayTy()) {
     if (nameToType.find(GetValueName(I)) != nameToType.end() ) {
      Out << nameToType[GetValueName(I)] << " " << GetValueName(I);
     }
    } else {
       printType(Out, I->getType()->getElementType(), false,
       GetValueName(I));
    }

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

 if (!M.empty())
  Out << "\n\n/* Function Bodies */\n";

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

 // Emit definitions of the intrinsics.
 for (SmallVector<const Function*, 8>::const_iterator
      I = intrinsicsToDefine.begin(),
      E = intrinsicsToDefine.end(); I != E; ++I) {
  printIntrinsicDefinition(**I, Out);
 }

 return false;
}


/// Output all floating point constants that cannot be printed accurately...
void CWriter::printFloatingPointConstants(Function &F) {
 // Scan the module for floating point constants.  If any FP constant is used
 // in the function, we want to redirect it here so that we do not depend on
 // the precision of the printed form, unless the printed form preserves
 // precision.
 //
 for (constant_iterator I = constant_begin(&F), E = constant_end(&F);
      I != E; ++I)
  printFloatingPointConstants(*I);

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
    APInt api = FPC->getValueAPF().bitcastToAPInt();
    const uint64_t *p = api.getRawData();
    Out << "static const ConstantFP80Ty FPConstant" << FPCounter++
        << " = { 0x" << utohexstr(p[0])
        << "ULL, 0x" << utohexstr((uint16_t)p[1]) << ",{0,0,0}"
        << "}; /* Long double constant */\n";
 } else if (FPC->getType() == Type::getPPC_FP128Ty(FPC->getContext()) ||
            FPC->getType() == Type::getFP128Ty(FPC->getContext())) {
    APInt api = FPC->getValueAPF().bitcastToAPInt();
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
void CWriter::printModuleTypes() {


 // Get all of the struct types used in the module.
 TypeFinder StructTypes;
 StructTypes.run(*TheModule, false);

 if (StructTypes.empty()) return;

 Out << "/* Structure forward decls */\n";

 unsigned NextTypeID = 0;

 // If any of them are missing names, add a unique ID to UnnamedStructIDs.
 // Print out forward declarations for structure types.
 for (unsigned i = 0, e = StructTypes.size(); i != e; ++i) {
  StructType *ST = StructTypes[i];

  if (ST->isLiteral() || ST->getName().empty())
   UnnamedStructIDs[ST] = NextTypeID++;

  std::string Name = getStructName(ST);

  Out << "typedef struct " << Name << ' ' << Name << ";\n";
 }

 Out << '\n';

 // Keep track of which structures have been printed so far.
 SmallPtrSet<Type *, 16> StructPrinted;

 // Loop over all structures then push them into the stack so they are
 // printed in the correct order.
 //
 Out << "/* Structure contents */\n";
 for (unsigned i = 0, e = StructTypes.size(); i != e; ++i)
  if (StructTypes[i]->isStructTy())
   // Only print out used types!
   printContainedStructs(StructTypes[i], StructPrinted);
}

// Push the struct onto the stack and recursively push all structs
// this one depends on.
//
// TODO:  Make this work properly with vector types
//
void CWriter::printContainedStructs(Type *Ty,
                                SmallPtrSet<Type *, 16> &StructPrinted) {
 // Don't walk through pointers.

 if (Ty->isPointerTy() || (Ty->getTypeID() >= 0 && Ty->getTypeID() <= 9) || Ty->isIntegerTy())
  return;

 // Print all contained types first.
 for (Type::subtype_iterator I = Ty->subtype_begin(),
      E = Ty->subtype_end(); I != E; ++I)
  printContainedStructs(*I, StructPrinted);

 if (StructType *ST = dyn_cast<StructType>(Ty)) {
  // Check to see if we have already printed this struct.
  if (!StructPrinted.insert(Ty)) return;

   // Print structure type out.
   printType(Out, ST, false, getStructName(ST), true);
   Out << ";\n\n";
 }
}

void CWriter::printFunctionSignature(const Function *F, bool Prototype) {
 /// isStructReturn - Should this function actually return a struct by-value?
 bool isStructReturn = F->hasStructRetAttr();

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

 // Loop over the arguments, printing them...
 FunctionType *FT = cast<FunctionType>(F->getFunctionType());
 const AttributeSet &PAL = F->getAttributes();

 std::string tstr;
 raw_string_ostream FunctionInnards(tstr);

 // Print out the name...
 FunctionInnards << GetValueName(F) << '(';

 bool PrintedArg = false;
 if (!F->isDeclaration()) {
  if (!F->arg_empty()) {
   Function::const_arg_iterator I = F->arg_begin(), E = F->arg_end();
   unsigned Idx = 1;

   // If this is a struct-return function, don't print the hidden
   // struct-return argument.
   if (isStructReturn) {
    assert(I != E && "Invalid struct return function!");
    ++I;
    ++Idx;
   }

   std::string ArgName;
   for (I; I != E; ++I) {
    if (PrintedArg) FunctionInnards << ", ";
     if (I->hasName() || !Prototype)
      ArgName = GetValueName(I);
     else
      ArgName = "";
    Type *ArgTy = I->getType();
    if (PAL.hasAttribute(Idx, Attribute::ByVal)) {
     ArgTy = cast<PointerType>(ArgTy)->getElementType();
     ByValParams.insert(I);
    }
    printType(FunctionInnards, ArgTy,
           !PAL.hasAttribute(Idx, Attribute::ZExt), ArgName);
    PrintedArg = true;
    ++Idx;
   }
  }
 } else {
    // Loop over the arguments, printing them.
    FunctionType::param_iterator I = FT->param_begin(), E = FT->param_end();
    unsigned Idx = 1;

    // If this is a struct-return function, don't print the hidden
    // struct-return argument.
    if (isStructReturn) {
     assert(I != E && "Invalid struct return function!");
     ++I;
     ++Idx;
    }

    for (; I != E; ++I) {
     if (PrintedArg) FunctionInnards << ", ";
     Type *ArgTy = *I;
     if (PAL.hasAttribute(Idx, Attribute::ByVal)) {
      assert(ArgTy->isPointerTy());
      ArgTy = cast<PointerType>(ArgTy)->getElementType();
     }
     printType(FunctionInnards, ArgTy,
           !PAL.hasAttribute(Idx, Attribute::ZExt));
     PrintedArg = true;
     ++Idx;
   }
 }

 if (!PrintedArg && FT->isVarArg()) {
  FunctionInnards << "int vararg_dummy_arg";
  PrintedArg = true;
 }

 // Finish printing arguments... if this is a vararg function, print the ...,
 // unless there are no known types, in which case, we just emit ().
 //
 if (FT->isVarArg() && PrintedArg) {
  FunctionInnards << ",...";  // Output varargs portion of signature!
 } else if (!FT->isVarArg() && !PrintedArg) {
    FunctionInnards << "void"; // ret() -> ret(void) in C.
 }
 FunctionInnards << ')';

 // Get the return tpe for the function.
 Type *RetTy;
 if (!isStructReturn)
  RetTy = F->getReturnType();
 else {
  // If this is a struct-return function, print the struct-return type.
  RetTy = cast<PointerType>(FT->getParamType(0))->getElementType();
 }

 // Print out the return type and the signature built above.
 printType(Out, RetTy,
           !PAL.hasAttribute(AttributeSet::ReturnIndex, Attribute::ZExt),
           FunctionInnards.str());
}

static inline bool isFPIntBitCast(const Instruction &I) {
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

 printFunctionSignature(&F, false);
 Out << " {\n";

 // If this is a struct return function, handle the result with magic.
 if (isStructReturn) {
  Type *StructTy =
   cast<PointerType>(F.arg_begin()->getType())->getElementType();
  Out << "  ";
  printType(Out, StructTy, false, "StructReturn");
  Out << ";  /* Struct return temporary */\n";

  Out << "  ";
  printType(Out, F.arg_begin()->getType(), false,
            GetValueName(F.arg_begin()));
  Out << " = &StructReturn;\n";
 }

 bool PrintedVar = false;

 // print local variable information for the function
 for (inst_iterator I = inst_begin(&F), E = inst_end(&F); I != E; ++I) {
  if (const AllocaInst *AI = isDirectAlloca(&*I)) {
   Out << "  ";
   printType(Out, AI->getAllocatedType(), false, GetValueName(AI));
   Out << ";    /* Address-exposed local */\n";
   PrintedVar = true;
  } else if (I->getType() != Type::getVoidTy(F.getContext()) &&
               !isInlinableInst(*I)) {
     Out << "  ";
     printType(Out, I->getType(), false, GetValueName(&*I));
     Out << ";\n";
     
     if (isa<PHINode>(*I)) {  // Print out PHI node temporaries as well...
      Out << "  ";
      printType(Out, I->getType(), false,
                 GetValueName(&*I)+"__PHI_TEMPORARY");
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

 if (F.hasExternalLinkage() && F.getName() == "main"){}

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

#if DEBUG
 std::cout << "*******************Entering PrintLoop*************************\n";
#endif
 for (unsigned i = 0, e = L->getBlocks().size(); i != e; ++i) {
  BasicBlock *BB = L->getBlocks()[i];
  Loop *BBLoop = LI->getLoopFor(BB);
  int Tp = G->returnType(BB);

#if DEBUG
 std::cout << "_________________________________\n";
 std::cout << " In printLoop with BB " << BB << " with type " << Tp << "\n";
#endif
 switch (Tp){
  case STARTLOOP : 
   if(BBLoop == L){
    printBasicBlockLoop(BB);
   }
   else {
    printLoop(BBLoop);
   }

   break;
  case ENDLOOP :    printBasicBlock(BB); 
   break;
  case IF :          
  {
   Instruction *I = dynamic_cast<Instruction *>(BB->getTerminator());
   BranchInst *BI = static_cast<BranchInst *>(I);
   visitBranchInst(*BI);
  }
   break;
  default :         printBasicBlock(BB); 
   break;
  }
   
 }
#if DEBUG
 std::cout << "*************************Exit PrintLoop************************\n"; 
#endif
 Out << " }  /* end of printLoop */ \n";
}

void CWriter::printLoopInst(BasicBlock *BB){
 BasicBlock::iterator II = BB->begin();
 visit(II);
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


void CWriter::printBasicBlockLoop(BasicBlock *BB){
#if DEBGUG
 std::cout << "*************************Start of printBasicBlockLoop********************************"<< std::endl;
 Out << "/* We are in BasicBlock " << BB << " of size " << BB->size() << " */ \n";
#endif
 int x = 0;
 Instruction *I = dynamic_cast<Instruction *>(BB->getTerminator());
 BranchInst* BI = static_cast<BranchInst *>(I);
 bool IsConditional = BI->isConditional();
 bool InWhile = false;
 if(IsConditional){
  if(G->returnType(*BI) == 3){
   visitBranchInst(*BI);
   return;
  }
  else {
   Out << " for(";
   InWhile = true;
  }
 }
 else {
#if DEBUG
 std::cout << "\n\n\n\n\n\nnonConditional BasicBlock: " << BB << "\n\n\n\n\n\n\n\n\n";
#endif
 int tc = G->returnType(BB);
#if DEBUG
 std::cout << "Type is: " << tc << "n\n\n\n";
#endif
 if (tc == STARTLOOP){
  Out << " while(1) {\n";
  loopstack.push(BI);
 }
 }
 // FIXME: Remove label and goto
 label: 
  // Output all of the instructions in the basic block...
  for (BasicBlock::iterator II = BB->begin(), E = --BB->end(); II != E;
	    ++II) {
   if (!isInlinableInst(*II) && !isDirectAlloca(II)) {
	if (II->getType() != Type::getVoidTy(BB->getContext()) &&
	      !isInlineAsm(*II))
	 outputLValue(II);
    else
	 Out << " ";
	writeInstComputationInline(*II);
   if(!InWhile)
	Out << ";\n";
   else if(x == 0 && BB->size() <= 3) Out << "; ";
   }
   if(x == 0 && BB->size() <= 3) {
    if(IsConditional) {
	 writeOperand(BI->getCondition());
     Out << " ; ";
     loopstack.push(BI);
     BBstack.push(BB);
     II = BB->begin();
     x++;
     goto label;
    }
   }
   else if(x == 0 && BB->size() > 3){
    if(IsConditional) {
     Out << " , ";
     loopstack.push(BI);
     BBstack.push(BB);
    }
   }
   else if(x == 1 && BB->size() > 3){
    if(IsConditional) {
     Out << "; ";
     writeOperand(BI->getCondition());
     Out << " ; ";
     loopstack.push(BI);
     BBstack.push(BB);
     II = BB->begin();
     x++;
     goto label;
    }
   } 
   // / *
   if(x == 2 && IsConditional){
    // end of for loop
	Out << " ) { \n";
    InWhile = false;
            
   }
   // * /
   x++;
  }
        
  
#if DEBUG
 std::cout << "*********************End of printBasicBlockLoop********************************\n";
#endif
}

void CWriter::visitBasicBlockByType(BasicBlock *BB){
 int Ty = G->returnType(BB);

 switch(Ty) {
  case STARTLOOP :
   printLoop(LI->getLoopFor(BB));
   break;
  case IF :
  {
   Instruction *I = dynamic_cast<Instruction *>(BB->getTerminator());
   BranchInst *BI = static_cast<BranchInst *>(I);
   visitBranchInst(*BI); 
  }
   break;
  case ENDLOOP :
   printBasicBlock(BB);
   Out << "  } \n";
   break;
  default :
   printBasicBlock(BB);
   break;
  }

}

void CWriter::printBasicBlockNoLabel(BasicBlock *BB){

#if DEBUG
 std::cout << "\n\n\n\n\n\n******************Warning: In printBasicBlockNoLabel*************************\n\n\n\n\n\n\n";
#endif

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
 // visit(*BB->getTerminator());
}

// TODO: Stop printing out the BasicBlocks which contain the bodies that we
// 		have already visited in conditional statements
void CWriter::printBasicBlock(BasicBlock *BB) {


 if(gotoset.find(BB) == gotoset.end()){}
 else 	return;
#if DEBUG
 std::cout << "PrintBasicBlock for BasicBlock " << BB << std::endl;
#endif
 
 

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
 if (I.getNumOperands() == 0 &&
     &*--I.getParent()->getParent()->end() == I.getParent() &&
     !(I.getParent()->size() == 1)) {
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
 inSwitch = true;

 Value* Cond = SI.getCondition();

 Out << "  switch (";
 writeOperand(Cond);
 Out << ") {\n  default:\n  ";

 printBasicBlock(SI.getDefaultDest());
 Out << "  break;\n";
 // we already visited; need to check if block is still needed
 removeBlock(SI.getDefaultDest());
 
 // Skip the first item since that's the default case.
 for (SwitchInst::CaseIt i = SI.case_begin(), e = SI.case_end(); i != e; ++i) {
  ConstantInt* CaseVal = i.getCaseValue();
  BasicBlock* Succ = i.getCaseSuccessor();
  Out << "  case ";
  writeOperand(CaseVal);
  Out << ":\n  ";
  
  printBasicBlock(Succ);
  addToGoToSet(Succ);
  Out << "  break;\n";
  // we already visited; need to check if block is still needed
  removeBlock(Succ);

 }

 Out << "  }\n";
 inSwitch = false;
}

void CWriter::visitIndirectBrInst(IndirectBrInst &IBI) {
 Out << "  goto *(void*)(";
 writeOperand(IBI.getOperand(0));
 Out << ");\n";
}

void CWriter::visitUnreachableInst(UnreachableInst &I) {
 Out << "  /*UNREACHABLE*/;\n";
}

// Greg Simpson
void CWriter::removeBlock(BasicBlock * BB) {

 return;
 // Must have a predecessor
 BasicBlock * PSBB = BB->getSinglePredecessor();
 if(PSBB == NULL)
  return;
 else
  BB->removeFromParent();
}

bool CWriter::isGotoCodeNecessary(BasicBlock *From, BasicBlock *To) {
 gotoset.insert(To);
 if((G->branchCheck(From)) && inSwitch){
  next = To;
  return false;
 }


 /// FIXME: This should be reenabled, but loop reordering safe!!
 return true;

 if (llvm::next(Function::iterator(From)) != Function::iterator(To))
  return true;  // Not the direct successor, we need a goto.


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

void CWriter::addToGoToSet(BasicBlock *To){
 gotoset.insert(To);
}


void CWriter::visitBranchInst(BranchInst &I){

 if(I.isConditional())
 {
  printBasicBlockNoLabel(I.getParent());
  int type = G->returnType(I);

  
  if(type == IF || type == UNKNOWN || type == ELSEIF)
   Out << "if ( ";
  writeOperand(I.getCondition());
  Out << " ){ \n";
  printBasicBlock(I.getSuccessor(0));
  addToGoToSet(I.getSuccessor(0));
  Out << "  }\n";
  type = G->returnType(I.getSuccessor(1));
  if(type != UNKNOWN) return;
   Out << "  else { \n"; 
   printBasicBlock(I.getSuccessor(1));
   addToGoToSet(I.getSuccessor(1));
   Out << "  }\n";
 }
 
}


// PHI nodes get copied into temporary values at the end of predecessor basic
// blocks.  We now need to copy these temporary values into the REAL value for
// the PHI.
void CWriter::visitPHINode(PHINode &I) {
 writeOperand(&I);
 Out << "__PHI_TEMPORARY";
 
}


void CWriter::visitBinaryOperator(Instruction &I) {
 // binary instructions, shift instructions, setCond instructions.
 assert(!I.getType()->isPointerTy());

 // We must cast the results of binary operations which might be promoted.
 bool needsCast = false;
 if ((I.getType() == Type::getInt8Ty(I.getContext())) ||
     (I.getType() == Type::getInt16Ty(I.getContext()))
     || (I.getType() == Type::getFloatTy(I.getContext()))) {
  needsCast = true;
  Out << "((";
  printType(Out, I.getType(), false);
  Out << ")(";
 }

 // If this is a negation operation, print it out as such.  For FP, we don't
 // want to print "-0.0 - X".
 if (BinaryOperator::isNeg(&I)) {
  Out << "-(";
  writeOperand(BinaryOperator::getNegArgument(cast<BinaryOperator>(&I)));
  Out << ")";
 } else if (BinaryOperator::isFNeg(&I)) {
    Out << "-(";
    writeOperand(BinaryOperator::getFNegArgument(cast<BinaryOperator>(&I)));
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
    // bool NeedsClosingParens = writeInstructionCast(I);

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

 }

 if (needsCast) {
  Out << "))";
 }
}

void CWriter::visitICmpInst(ICmpInst &I) {
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
 if (I.getPredicate() == FCmpInst::FCMP_FALSE) {
  Out << "0";
  return;
 }
 if (I.getPredicate() == FCmpInst::FCMP_TRUE) {
  Out << "1";
  return;
 }

 const char* op = 0;
 switch (I.getPredicate()) {
  default: llvm_unreachable("Illegal FCmp predicate");
  case FCmpInst::FCMP_ORD: op = "ord"; break;
  case FCmpInst::FCMP_UNO: op = "uno"; break;
  case FCmpInst::FCMP_UEQ: op = "ueq"; break;
  case FCmpInst::FCMP_UNE: op = "une"; break;
  case FCmpInst::FCMP_ULT: op = "ult"; break;
  case FCmpInst::FCMP_ULE: op = "ule"; break;
  case FCmpInst::FCMP_UGT: op = "ugt"; break;
  case FCmpInst::FCMP_UGE: op = "uge"; break;
  case FCmpInst::FCMP_OEQ: op = "oeq"; break;
  case FCmpInst::FCMP_ONE: op = "one"; break;
  case FCmpInst::FCMP_OLT: op = "olt"; break;
  case FCmpInst::FCMP_OLE: op = "ole"; break;
  case FCmpInst::FCMP_OGT: op = "ogt"; break;
  case FCmpInst::FCMP_OGE: op = "oge"; break;
 }

 Out << "llvm_fcmp_" << op << "(";
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
 Out << "((";
 writeOperand(I.getCondition());
 Out << ") ? (";
 writeOperand(I.getTrueValue());
 Out << ") : (";
 writeOperand(I.getFalseValue());
 Out << "))";
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
        T.getBitWidth() == 32 || T.getBitWidth() == 64;
}
#endif

void CWriter::printIntrinsicDefinition(const Function &F, raw_ostream &Out) {
 FunctionType *funT = F.getFunctionType();
 Type *retT = F.getReturnType();
 IntegerType *elemT = cast<IntegerType>(funT->getParamType(1));

 assert(isSupportedIntegerSize(*elemT) &&
        "CBackend does not support arbitrary size integers.");
 assert(cast<StructType>(retT)->getElementType(0) == elemT &&
        elemT == funT->getParamType(0) && funT->getNumParams() == 2);

 switch (F.getIntrinsicID()) {
  default:
   llvm_unreachable("Unsupported Intrinsic.");
  case Intrinsic::uadd_with_overflow:
  
   Out << "static inline ";
   printType(Out, retT);
   Out << GetValueName(&F);
   Out << "(";
   printSimpleType(Out, elemT, false);
   Out << "a,";
   printSimpleType(Out, elemT, false);
   Out << "b) {\n  ";
   printType(Out, retT);
   Out << "r;\n";
   Out << "  r.field0 = a + b;\n";
   Out << "  r.field1 = (r.field0 < a);\n";
   Out << "  return r;\n}\n";
   break;
  case Intrinsic::sadd_with_overflow:
  
   Out << "static ";
   printType(Out, retT);
   Out << GetValueName(&F);
   Out << "(";
   printSimpleType(Out, elemT, true);
   Out << "a,";
   printSimpleType(Out, elemT, true);
   Out << "b) {\n  ";
   printType(Out, retT);
   Out << "r;\n";
   Out << "  r.field1 = (b > 0 && a > ";
   printLimitValue(*elemT, true, true, Out);
   Out << " - b) || (b < 0 && a < ";
   printLimitValue(*elemT, true, false, Out);
   Out << " - b);\n";
   Out << "  r.field0 = r.field1 ? 0 : a + b;\n";
   Out << "  return r;\n}\n";
   break;
 }
}

void CWriter::lowerIntrinsics(Function &F) {
 // This is used to keep track of intrinsics that get generated to a lowered
 // function. We must generate the prototypes before the function body which
 // will only be expanded on first use (by the loop below).
 std::vector<Function*> prototypesToGen;
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
      case Intrinsic::prefetch:
      case Intrinsic::powi:
      case Intrinsic::x86_sse_cmp_ss:
      case Intrinsic::x86_sse_cmp_ps:
      case Intrinsic::x86_sse2_cmp_sd:
      case Intrinsic::x86_sse2_cmp_pd:
      case Intrinsic::ppc_altivec_lvsl:
      case Intrinsic::uadd_with_overflow:
      case Intrinsic::sadd_with_overflow:
       // We directly implement these intrinsics
       break;
      default:
       // If this is an intrinsic that directly corresponds to a GCC
       // builtin, we handle it.
       const char *BuiltinName = "";
#define GET_GCC_BUILTIN_NAME
#include "llvm/IR/Intrinsics.gen"
#undef GET_GCC_BUILTIN_NAME
 // If we handle it, don't lower it.
 if (BuiltinName[0]) break;

 // All other intrinsic calls we must lower.
 Instruction *Before = 0;
 if (CI != &BB->front())
  Before = prior(BasicBlock::iterator(CI));

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
 // We may have collected some prototypes to emit in the loop above.
 // Emit them now, before the function that uses them is emitted. But,
 // be careful not to emit them twice.
 std::vector<Function*>::iterator I = prototypesToGen.begin();
 std::vector<Function*>::iterator E = prototypesToGen.end();
 for ( ; I != E; ++I) {
  if (intrinsicPrototypesAlreadyGenerated.insert(*I).second) {
   Out << '\n';
   printFunctionSignature(*I, true);
   Out << ";\n";
  }
 }
}

void CWriter::visitCallInst(CallInst &I) {
 if (isa<InlineAsm>(I.getCalledValue())) {
  Out << "/* Reached Inline asm which is not supported */ \n";
  std::string Instruc;
  std::string InstName;
  raw_string_ostream S (Instruc);
  S << I;
  size_t InstPos = 0;
  InstPos = Instruc.find("\"");
  for (; InstPos < Instruc.length(); ++InstPos)
  {	
   if (Instruc.at(InstPos) == ',')
    break;
   else
    InstName += Instruc.at(InstPos);
  }
  Out << "\tasm volatile (" << InstName << ")";
  return;
 }

 bool WroteCallee = false;

 // Handle intrinsic function calls first...
 if (Function *F = I.getCalledFunction())
  if (Intrinsic::ID ID = (Intrinsic::ID)F->getIntrinsicID())
   if (visitBuiltinCall(I, ID, WroteCallee))
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
  if (!WroteCallee) {
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
   // Cast the pointer type.
   Out << "((";
   if (isStructRet)
    printStructReturnPointerFunctionType(Out, PAL,
                        cast<PointerType>(I.getCalledValue()->getType()));
   else if (hasByVal)
    printType(Out, I.getCalledValue()->getType(), false, "", true, PAL);
   else
    printType(Out, I.getCalledValue()->getType());
   Out << ")(void*)";
  }
  writeOperand(Callee);
  if (NeedsCast) Out << ')';
  }

 Out << '(';

 bool PrintedArg = false;
 if(FTy->isVarArg() && !FTy->getNumParams()) {
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


 for (; AI != AE; ++AI, ++ArgNo) {
  if (PrintedArg) Out << ", ";
   if (ArgNo < NumDeclaredParams &&
       (*AI)->getType() != FTy->getParamType(ArgNo)) {
    Out << '(';
    printType(Out, FTy->getParamType(ArgNo),
          !PAL.hasAttribute(ArgNo+1, Attribute::ZExt));
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
/// if the entire call is handled, return false if it wasn't handled, and
/// optionally set 'WroteCallee' if the callee has already been printed out.
bool CWriter::visitBuiltinCall(CallInst &I, Intrinsic::ID ID,
                               bool &WroteCallee) {
 switch (ID) {
  default: {
   // If this is an intrinsic that directly corresponds to a GCC
   // builtin, we emit it here.
   const char *BuiltinName = "";
#define GET_GCC_BUILTIN_NAME
#include "llvm/IR/Intrinsics.gen"
#undef GET_GCC_BUILTIN_NAME
 assert(BuiltinName[0] && "Unknown LLVM intrinsic!");

  Out << BuiltinName;
  WroteCallee = true;
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
  case Intrinsic::powi:
   Out << "__builtin_powi(";
   writeOperand(I.getArgOperand(0));
   Out << ", ";
   writeOperand(I.getArgOperand(1));
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
   // to work around GCC bugs (see PR1809).
   Out << "0; *((void**)&" << GetValueName(&I)
       << ") = __builtin_stack_save()";
   return true;
  case Intrinsic::x86_sse_cmp_ss:
  case Intrinsic::x86_sse_cmp_ps:
  case Intrinsic::x86_sse2_cmp_sd:
  case Intrinsic::x86_sse2_cmp_pd:
   Out << '(';
   printType(Out, I.getType());
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
   printType(Out, I.getType());
   Out << ')';
   Out << "__builtin_altivec_lvsl(0, (void*)";
   writeOperand(I.getArgOperand(0));
   Out << ")";
   return true;
  case Intrinsic::uadd_with_overflow:
  case Intrinsic::sadd_with_overflow:
   Out << GetValueName(I.getCalledFunction()) << "(";
   writeOperand(I.getArgOperand(0));
   Out << ", ";
   writeOperand(I.getArgOperand(1));
   Out << ")";
   return true;
 }
}

void CWriter::visitAllocaInst(AllocaInst &I) {
 Out << '(';
 printType(Out, I.getType());
 Out << ") alloca(sizeof(";
 printType(Out, I.getType()->getElementType());
 Out << ')';
 if (I.isArrayAllocation()) {
  Out << " * " ;
  writeOperand(I.getOperand(0));
 }
 Out << ')';
}

void CWriter::printGEPExpression(Value *Ptr, gep_type_iterator I,
                                 gep_type_iterator E, bool Static) {

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
  printType(Out, PointerType::getUnqual(LastIndexIsVector->getElementType()));
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

    // Emit the first operand. If Ptr is something that is already address
    // exposed, like a global, avoid emitting (&foo)[0], just emit foo instead.
    if (isAddressExposed(Ptr)) {
     writeOperandInternal(Ptr, Static);
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
                                bool IsVolatile, unsigned Alignment) {

 bool IsUnaligned = Alignment &&
   Alignment < TD->getABITypeAlignment(OperandType);

 if (!IsUnaligned)
  Out << '*';
 if (IsVolatile || IsUnaligned) {
  Out << "((";
  if (IsUnaligned)
   Out << "struct __attribute__ ((packed, aligned(" << Alignment << "))) {";
  printType(Out, OperandType, false, IsUnaligned ? "data" : "volatile*");
  if (IsUnaligned) {
   Out << "; } ";
   if (IsVolatile) Out << "volatile ";
    Out << "*";
  }
  Out << ")";
 }

 writeOperand(Operand);
 
 if (IsVolatile || IsUnaligned) {
  Out << ')';
  if (IsUnaligned)
   Out << "->data";
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
 Constant *BitMask = 0;
 if (IntegerType* ITy = dyn_cast<IntegerType>(Operand->getType()))
  if (!ITy->isPowerOf2ByteWidth())
   // We have a bit width that doesn't match an even power-of-2 byte
   // size. Consequently we must & the value with the type's bit mask
   BitMask = ConstantInt::get(ITy, ITy->getBitMask());
 if (BitMask)
  Out << "((";
 writeOperand(Operand);
 if (BitMask) {
  Out << ") & ";
  printConstant(BitMask, false);
  Out << ")";
 }
}

void CWriter::visitGetElementPtrInst(GetElementPtrInst &I) {
 printGEPExpression(I.getPointerOperand(), gep_type_begin(I),
                    gep_type_end(I), false);
}

void CWriter::visitVAArgInst(VAArgInst &I) {
 Out << "va_arg(*(va_list*)";
 writeOperand(I.getOperand(0));
 Out << ", ";
 printType(Out, I.getType());
 Out << ");\n ";
}

void CWriter::visitInsertElementInst(InsertElementInst &I) {
 Type *EltTy = I.getType()->getElementType();
 writeOperand(I.getOperand(0));
 Out << ";\n  ";
 Out << "((";
 printType(Out, PointerType::getUnqual(EltTy));
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
 printType(Out, PointerType::getUnqual(EltTy));
 Out << ")(&" << GetValueName(I.getOperand(0)) << "))[";
 writeOperand(I.getOperand(1));
 Out << "]";
}

void CWriter::visitShuffleVectorInst(ShuffleVectorInst &SVI) {
 Out << "(";
 printType(Out, SVI.getType());
 Out << "){ ";
 VectorType *VT = SVI.getType();
 unsigned NumElts = VT->getNumElements();
 Type *EltTy = VT->getElementType();

 for (unsigned i = 0; i != NumElts; ++i) {
  if (i) Out << ", ";
   int SrcVal = SVI.getMaskValue(i);
  if ((unsigned)SrcVal >= NumElts*2) {
   Out << " 0/*undef*/ ";
  } else {
     Value *Op = SVI.getOperand((unsigned)SrcVal >= NumElts);
     if (isa<Instruction>(Op)) {
      // Do an extractelement of this value from the appropriate input.
      Out << "((";
      printType(Out, PointerType::getUnqual(EltTy));
      Out << ")(&" << GetValueName(Op)
          << "))[" << (SrcVal & (NumElts-1)) << "]";
      } else if (isa<ConstantAggregateZero>(Op) || isa<UndefValue>(Op)) {
         Out << "0";
      } else {
         printConstant(cast<ConstantVector>(Op)->getOperand(SrcVal &
                                                           (NumElts-1)),false);
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
                                     makeArrayRef(b, i+1));
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
  printType(Out, EVI.getType());
  Out << ") 0/*UNDEF*/";
 } else {
    Out << GetValueName(EVI.getOperand(0));
    for (const unsigned *b = EVI.idx_begin(), *i = b, *e = EVI.idx_end();
         i != e; ++i) {
     Type *IndexedTy =
      ExtractValueInst::getIndexedType(EVI.getOperand(0)->getType(),
                                       makeArrayRef(b, i+1));
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

bool CTargetMachine::addPassesToEmitFile(PassManagerBase &PM,
                                         formatted_raw_ostream &o,
                                         CodeGenFileType FileType,
                                         bool DisableVerify,
                                         AnalysisID StartAfter,
                                         AnalysisID StopAfter) {
 if (FileType != TargetMachine::CGFT_AssemblyFile) return true;

 PM.add(createGCLoweringPass());
 PM.add(createLowerInvokePass());
 PM.add(new CWriter(o));
 
 return false;
}
