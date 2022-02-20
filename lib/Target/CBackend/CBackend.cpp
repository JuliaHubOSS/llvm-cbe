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
#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/PatternMatch.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/TargetRegistry.h"

#include "TopologicalSorter.h"

#include <algorithm>
#include <cstdio>

#include <iostream>

// Jackson Korba 9/29/14
#ifndef DEBUG_TYPE
#define DEBUG_TYPE ""
#endif
// End Modification

// Some ms header decided to define setjmp as _setjmp, undo this for this file
// since we don't need it
#ifdef setjmp
#undef setjmp
#endif
#ifdef _MSC_VER
#define alloca _alloca
#endif
// On LLVM 10 and later, include intrinsics files.
#if LLVM_VERSION_MAJOR >= 10
#include "llvm/IR/IntrinsicsPowerPC.h"
#include "llvm/IR/IntrinsicsX86.h"
#endif

// Debug output helper
#ifndef NDEBUG
#define DBG_ERRS(x) errs() << x << " (#" << __LINE__ << ")\n"
#else
#define DBG_ERRS(x)
#endif

namespace llvm_cbe {

using namespace llvm;

static cl::opt<bool> DeclareLocalsLate(
    "cbe-declare-locals-late",
    cl::desc("C backend: Declare local variables at the point they're first "
             "assigned, "
             "if possible, rather than always at the start of the function. "
             "Note that "
             "this is not legal in standard C prior to C99."));

extern "C" void LLVMInitializeCBackendTarget() {
  // Register the target.
  RegisterTargetMachine<CTargetMachine> X(TheCBackendTarget);
}
#if LLVM_VERSION_MAJOR > 10
bool IsPowerOfTwo(unsigned long x)
{
  return (x & (x - 1)) == 0;
}
#endif

unsigned int NumberOfElements(VectorType *TheType) {
#if LLVM_VERSION_MAJOR >= 12
  return TheType->getElementCount().getValue();
#else
  return TheType->getNumElements();
#endif
}
char CWriter::ID = 0;

// extra (invalid) Ops tags for tracking unary ops as a special case of the
// available binary ops
enum UnaryOps {
  BinaryNeg = Instruction::OtherOpsEnd + 1,
  BinaryNot,
};

#ifdef NDEBUG
#define cwriter_assert(expr)                                                   \
  do {                                                                         \
  } while (0)
#else
#define cwriter_assert(expr)                                                   \
  if (!(expr)) {                                                               \
    this->errorWithMessage(#expr);                                             \
  }
#endif

static bool isConstantNull(Value *V) {
  if (Constant *C = dyn_cast<Constant>(V))
    return C->isNullValue();
  return false;
}

static bool isEmptyType(Type *Ty) {
  if (StructType *STy = dyn_cast<StructType>(Ty))
    return STy->getNumElements() == 0 ||
           std::all_of(STy->element_begin(), STy->element_end(), isEmptyType);

  if (VectorType *VTy = dyn_cast<VectorType>(Ty))
    return NumberOfElements(VTy) == 0 || isEmptyType(VTy->getElementType());
  if (ArrayType *ATy = dyn_cast<ArrayType>(Ty))
    return ATy->getNumElements() == 0 || isEmptyType(ATy->getElementType());

  return Ty->isVoidTy();
}

bool CWriter::isEmptyType(Type *Ty) const { return llvm_cbe::isEmptyType(Ty); }

/// Peel off outer array types which have zero elements.
/// This is useful for pointers types. It isn't reasonable for values.
Type *CWriter::skipEmptyArrayTypes(Type *Ty) const {
  while (Ty->isArrayTy() && Ty->getArrayNumElements() == 0)
    Ty = Ty->getArrayElementType();
  return Ty;
}

/// isAddressExposed - Return true if the specified value's name needs to
/// have its address taken in order to get a C value of the correct type.
/// This happens for global variables, byval parameters, and direct allocas.
bool CWriter::isAddressExposed(Value *V) const {
  if (Argument *A = dyn_cast<Argument>(V))
    return ByValParams.count(A) > 0;
  else
    return isa<GlobalVariable>(V) || isDirectAlloca(V);
}

// isInlinableInst - Attempt to inline instructions into their uses to build
// trees as much as possible.  To do this, we have to consistently decide
// what is acceptable to inline, so that variable declarations don't get
// printed and an extra copy of the expr is not emitted.
bool CWriter::isInlinableInst(Instruction &I) const {
  // Always inline cmp instructions, even if they are shared by multiple
  // expressions.  GCC generates horrible code if we don't.
  if (isa<CmpInst>(I))
    return true;

  // Must be an expression, must be used exactly once.  If it is dead, we
  // emit it inline where it would go.
  if (isEmptyType(I.getType()) || !I.hasOneUse() || I.isTerminator() ||
      isa<CallInst>(I) || isa<PHINode>(I) || isa<LoadInst>(I) ||
      isa<VAArgInst>(I) || isa<InsertElementInst>(I) || isa<InsertValueInst>(I))
    // Don't inline a load across a store or other bad things!
    return false;

  // Must not be used in inline asm, extractelement, or shufflevector.
  if (I.hasOneUse()) {
    Instruction &User = cast<Instruction>(*I.user_back());
    if (isInlineAsm(User))
      return false;
  }

  // Only inline instruction if its use is in the same BB as the inst.
  return I.getParent() == cast<Instruction>(I.user_back())->getParent();
}

// isDirectAlloca - Define fixed sized allocas in the entry block as direct
// variables which are accessed with the & operator.  This causes GCC to
// generate significantly better code than to emit alloca calls directly.
AllocaInst *CWriter::isDirectAlloca(Value *V) const {
  AllocaInst *AI = dyn_cast<AllocaInst>(V);
  if (!AI)
    return nullptr;
  if (AI->isArrayAllocation())
    return nullptr; // FIXME: we can also inline fixed size array allocas!
  if (AI->getParent() != &AI->getParent()->getParent()->getEntryBlock())
    return nullptr;
  return AI;
}

// isInlineAsm - Check if the instruction is a call to an inline asm chunk.
bool CWriter::isInlineAsm(Instruction &I) const {
  if (CallInst *CI = dyn_cast<CallInst>(&I)) {
    return isa<InlineAsm>(CI->getCalledOperand());
  } else
    return false;
}

bool CWriter::runOnFunction(Function &F) {
  // Do not codegen any 'available_externally' functions at all, they have
  // definitions outside the translation unit.
  if (F.hasAvailableExternallyLinkage())
    return false;

  LI = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();

  // Get rid of intrinsics we can't handle.
  bool Modified = lowerIntrinsics(F);

  // Output all floating point constants that cannot be printed accurately.
  printFloatingPointConstants(F);

  printFunction(F);

  LI = nullptr;

  return Modified;
}

static std::string CBEMangle(const std::string &S) {
  std::string Result;

  for (auto c : S) {
    if (isalnum(c) || c == '_') {
      Result += c;
    } else {
      Result += '_';
      Result += 'A' + (c & 15);
      Result += 'A' + ((c >> 4) & 15);
      Result += '_';
    }
  }

  return Result;
}

raw_ostream &CWriter::printTypeString(raw_ostream &Out, Type *Ty,
                                      bool isSigned) {
  if (StructType *ST = dyn_cast<StructType>(Ty)) {
    cwriter_assert(!isEmptyType(ST));
    TypedefDeclTypes.insert(Ty);

    if (!ST->isLiteral() && !ST->getName().empty()) {
      std::string Name{ST->getName()};
      return Out << "struct_" << CBEMangle(Name);
    }

    unsigned id = UnnamedStructIDs.getOrInsert(ST);
    return Out << "unnamed_" + utostr(id);
  }

  if (Ty->isPointerTy()) {
    Out << "p";
    return printTypeString(Out, Ty->getPointerElementType(), isSigned);
  }

  switch (Ty->getTypeID()) {
  case Type::VoidTyID:
    return Out << "void";
  case Type::IntegerTyID: {
    unsigned NumBits = cast<IntegerType>(Ty)->getBitWidth();
    if (NumBits == 1)
      return Out << "bool";
    else {
      cwriter_assert(NumBits <= 128 && "Bit widths > 128 not implemented yet");
      return Out << (isSigned ? "i" : "u") << NumBits;
    }
  }
  case Type::FloatTyID:
    return Out << "f32";
  case Type::DoubleTyID:
    return Out << "f64";
  case Type::X86_FP80TyID:
    return Out << "f80";
  case Type::PPC_FP128TyID:
  case Type::FP128TyID:
    return Out << "f128";

  case Type::X86_MMXTyID:
    return Out << (isSigned ? "i32y2" : "u32y2");
#if LLVM_VERSION_MAJOR > 10
  case Type::FixedVectorTyID:
  case Type::ScalableVectorTyID:
#else
  case Type::VectorTyID:
#endif
  {
    TypedefDeclTypes.insert(Ty);
#if LLVM_VERSION_MAJOR > 10
    FixedVectorType *VTy = cast<FixedVectorType>(Ty);
#else
    VectorType *VTy = cast<VectorType>(Ty);
#endif
    cwriter_assert(VTy->getNumElements() != 0);
    printTypeString(Out, VTy->getElementType(), isSigned);
    return Out << "x" << NumberOfElements(VTy);
  }

  case Type::ArrayTyID: {
    TypedefDeclTypes.insert(Ty);
    ArrayType *ATy = cast<ArrayType>(Ty);
    cwriter_assert(ATy->getNumElements() != 0);
    printTypeString(Out, ATy->getElementType(), isSigned);
    return Out << "a" << ATy->getNumElements();
  }

  default:
    DBG_ERRS("Unknown primitive type: " << *Ty);
    errorWithMessage("unknown primitive type");
  }
}

std::string CWriter::getStructName(StructType *ST) {
  cwriter_assert(ST->getNumElements() != 0);
  if (!ST->isLiteral() && !ST->getName().empty())
    return "struct l_struct_" + CBEMangle(ST->getName().str());

  unsigned id = UnnamedStructIDs.getOrInsert(ST);
  return "struct l_unnamed_" + utostr(id);
}

std::string
CWriter::getFunctionName(FunctionType *FT,
                         std::pair<AttributeList, CallingConv::ID> PAL) {
  unsigned id = UnnamedFunctionIDs.getOrInsert(std::make_pair(FT, PAL));
  return "l_fptr_" + utostr(id);
}

std::string CWriter::getArrayName(ArrayType *AT) {
  std::string astr;
  raw_string_ostream ArrayInnards(astr);
  // Arrays are wrapped in structs to allow them to have normal
  // value semantics (avoiding the array "decay").
  cwriter_assert(!isEmptyType(AT));
  printTypeName(ArrayInnards, AT->getElementType(), false);
  return "struct l_array_" + utostr(AT->getNumElements()) + '_' +
         CBEMangle(ArrayInnards.str());
}

std::string CWriter::getVectorName(VectorType *VT, bool Aligned) {
  std::string astr;
  raw_string_ostream VectorInnards(astr);
  // Vectors are handled like arrays
  cwriter_assert(!isEmptyType(VT));
  if (Aligned) {
    headerUseMsAlign();
    Out << "__MSALIGN__(" << TD->getABITypeAlignment(VT) << ") ";
  }
  printTypeName(VectorInnards, VT->getElementType(), false);
  return "struct l_vector_" + utostr(NumberOfElements(VT)) + '_' +
         CBEMangle(VectorInnards.str());
}

static const std::string getCmpPredicateName(CmpInst::Predicate P) {
  switch (P) {
  case FCmpInst::FCMP_FALSE:
    return "0";
  case FCmpInst::FCMP_OEQ:
    return "oeq";
  case FCmpInst::FCMP_OGT:
    return "ogt";
  case FCmpInst::FCMP_OGE:
    return "oge";
  case FCmpInst::FCMP_OLT:
    return "olt";
  case FCmpInst::FCMP_OLE:
    return "ole";
  case FCmpInst::FCMP_ONE:
    return "one";
  case FCmpInst::FCMP_ORD:
    return "ord";
  case FCmpInst::FCMP_UNO:
    return "uno";
  case FCmpInst::FCMP_UEQ:
    return "ueq";
  case FCmpInst::FCMP_UGT:
    return "ugt";
  case FCmpInst::FCMP_UGE:
    return "uge";
  case FCmpInst::FCMP_ULT:
    return "ult";
  case FCmpInst::FCMP_ULE:
    return "ule";
  case FCmpInst::FCMP_UNE:
    return "une";
  case FCmpInst::FCMP_TRUE:
    return "1";
  case ICmpInst::ICMP_EQ:
    return "eq";
  case ICmpInst::ICMP_NE:
    return "ne";
  case ICmpInst::ICMP_ULE:
    return "ule";
  case ICmpInst::ICMP_SLE:
    return "sle";
  case ICmpInst::ICMP_UGE:
    return "uge";
  case ICmpInst::ICMP_SGE:
    return "sge";
  case ICmpInst::ICMP_ULT:
    return "ult";
  case ICmpInst::ICMP_SLT:
    return "slt";
  case ICmpInst::ICMP_UGT:
    return "ugt";
  case ICmpInst::ICMP_SGT:
    return "sgt";
  default:
    DBG_ERRS("Invalid icmp predicate!" << P);
    // TODO: cwriter_assert
    llvm_unreachable(0);
  }
}

static const char *getFCmpImplem(CmpInst::Predicate P) {
  switch (P) {
  case FCmpInst::FCMP_FALSE:
    return "0";
  case FCmpInst::FCMP_OEQ:
    return "X == Y";
  case FCmpInst::FCMP_OGT:
    return "X >  Y";
  case FCmpInst::FCMP_OGE:
    return "X >= Y";
  case FCmpInst::FCMP_OLT:
    return "X <  Y";
  case FCmpInst::FCMP_OLE:
    return "X <= Y";
  case FCmpInst::FCMP_ONE:
    return "X != Y && llvm_fcmp_ord(X, Y);";
  case FCmpInst::FCMP_ORD:
    return "X == X && Y == Y";
  case FCmpInst::FCMP_UNO:
    return "X != X || Y != Y";
  case FCmpInst::FCMP_UEQ:
    return "X == Y || llvm_fcmp_uno(X, Y)";
  case FCmpInst::FCMP_UGT:
    return "X >  Y || llvm_fcmp_uno(X, Y)";
    return "ugt";
  case FCmpInst::FCMP_UGE:
    return "X >= Y || llvm_fcmp_uno(X, Y)";
  case FCmpInst::FCMP_ULT:
    return "X <  Y || llvm_fcmp_uno(X, Y)";
  case FCmpInst::FCMP_ULE:
    return "X <= Y || llvm_fcmp_uno(X, Y)";
  case FCmpInst::FCMP_UNE:
    return "X != Y";
  case FCmpInst::FCMP_TRUE:
    return "1";
  default:
    DBG_ERRS("Invalid fcmp predicate!" << P);
    // TODO: cwriter_assert
    llvm_unreachable(0);
  }
}

static void defineFCmpOp(raw_ostream &Out, CmpInst::Predicate const P) {
  Out << "static __forceinline int llvm_fcmp_" << getCmpPredicateName(P)
      << "(double X, double Y) { ";
  Out << "return " << getFCmpImplem(P) << "; }\n";
}

void CWriter::headerUseFCmpOp(CmpInst::Predicate P) {
  switch (P) {
  case FCmpInst::FCMP_ONE:
    FCmpOps.insert(CmpInst::FCMP_ORD);
    break;
  case FCmpInst::FCMP_UEQ:
  case FCmpInst::FCMP_UGT:
  case FCmpInst::FCMP_UGE:
  case FCmpInst::FCMP_ULT:
  case FCmpInst::FCMP_ULE:
    FCmpOps.insert(CmpInst::FCMP_UNO);
    break;
  default:
    break;
  }
  FCmpOps.insert(P);
}

raw_ostream &CWriter::printSimpleType(raw_ostream &Out, Type *Ty,
                                      bool isSigned) {
  cwriter_assert((Ty->isSingleValueType() || Ty->isVoidTy()) &&
                 "Invalid type for printSimpleType");
  switch (Ty->getTypeID()) {
  case Type::VoidTyID:
    return Out << "void";
  case Type::IntegerTyID: {
    unsigned NumBits = cast<IntegerType>(Ty)->getBitWidth();
    if (NumBits == 1)
      return Out << "bool";
    else if (NumBits <= 8)
      return Out << (isSigned ? "int8_t" : "uint8_t");
    else if (NumBits <= 16)
      return Out << (isSigned ? "int16_t" : "uint16_t");
    else if (NumBits <= 32)
      return Out << (isSigned ? "int32_t" : "uint32_t");
    else if (NumBits <= 64)
      return Out << (isSigned ? "int64_t" : "uint64_t");
    else {
      cwriter_assert(NumBits <= 128 && "Bit widths > 128 not implemented yet");
      return Out << (isSigned ? "int128_t" : "uint128_t");
    }
  }
  case Type::FloatTyID:
    return Out << "float";
  case Type::DoubleTyID:
    return Out << "double";
  // Lacking emulation of FP80 on PPC, etc., we assume whichever of these is
  // present matches host 'long double'.
  case Type::X86_FP80TyID:
  case Type::PPC_FP128TyID:
  case Type::FP128TyID:
    return Out << "long double";

  case Type::X86_MMXTyID:
    return Out << (isSigned ? "int32_t" : "uint32_t")
               << " __attribute__((vector_size(8)))";

  default:
    DBG_ERRS("Unknown primitive type: " << *Ty);
    errorWithMessage("unknown primitive type");
  }
}

raw_ostream &CWriter::printTypeNameForAddressableValue(raw_ostream &Out,
                                                       Type *Ty,
                                                       bool isSigned) {
  // We can't directly declare a zero-sized variable in C, so we have to
  // use a single-byte type instead, in case a pointer to it is taken.
  // We can then fix the pointer type in writeOperand.
  if (!isEmptyType(Ty))
    return printTypeName(Out, Ty, isSigned);
  else
    return Out << "char /* (empty) */";
}

// Pass the Type* and the variable name and this prints out the variable
// declaration.
raw_ostream &
CWriter::printTypeName(raw_ostream &Out, Type *Ty, bool isSigned,
                       std::pair<AttributeList, CallingConv::ID> PAL) {
  if (Ty->isSingleValueType() || Ty->isVoidTy()) {
    if (!Ty->isPointerTy() && !Ty->isVectorTy())
      return printSimpleType(Out, Ty, isSigned);
  }

  if (isEmptyType(Ty))
    return Out << "void";

  switch (Ty->getTypeID()) {
  case Type::FunctionTyID: {
    FunctionType *FTy = cast<FunctionType>(Ty);
    return Out << getFunctionName(FTy, PAL);
  }
  case Type::StructTyID: {
    TypedefDeclTypes.insert(Ty);
    return Out << getStructName(cast<StructType>(Ty));
  }

  case Type::PointerTyID: {
    Type *ElTy = Ty->getPointerElementType();
    ElTy = skipEmptyArrayTypes(ElTy);
    return printTypeName(Out, ElTy, false) << '*';
  }

  case Type::ArrayTyID: {
    TypedefDeclTypes.insert(Ty);
    return Out << getArrayName(cast<ArrayType>(Ty));
  }
#if LLVM_VERSION_MAJOR > 10
  case Type::FixedVectorTyID:
  case Type::ScalableVectorTyID:
#else
  case Type::VectorTyID:
#endif
  {
    TypedefDeclTypes.insert(Ty);
    return Out << getVectorName(cast<VectorType>(Ty), true);
  }

  default:
    DBG_ERRS("Unexpected type: " << *Ty);
    errorWithMessage("unexpected type");
  }
}

raw_ostream &CWriter::printTypeNameUnaligned(raw_ostream &Out, Type *Ty,
                                             bool isSigned) {
  if (VectorType *VTy = dyn_cast<VectorType>(Ty)) {
    // MSVC doesn't handle __declspec(align) on parameters,
    // but we specify it for Vector (hoping the compiler will vectorize it)
    // so we need to avoid it sometimes
    TypedefDeclTypes.insert(VTy);
    return Out << getVectorName(VTy, false);
  }
  return printTypeName(Out, Ty, isSigned);
}

raw_ostream &CWriter::printStructDeclaration(raw_ostream &Out,
                                             StructType *STy) {
  if (STy->isPacked())
    Out << "#ifdef _MSC_VER\n#pragma pack(push, 1)\n#endif\n";
  Out << getStructName(STy) << " {\n";
  unsigned Idx = 0;
  for (StructType::element_iterator I = STy->element_begin(),
                                    E = STy->element_end();
       I != E; ++I, Idx++) {
    Out << "  ";
    bool empty = isEmptyType(*I);
    if (empty)
      Out << "/* "; // skip zero-sized types
    printTypeName(Out, *I, false) << " field" << utostr(Idx);
    if (empty)
      Out << " */"; // skip zero-sized types
    else
      Out << ";\n";
  }
  Out << '}';
  if (STy->isPacked())
    Out << " __attribute__ ((packed))";
  Out << ";\n";
  if (STy->isPacked())
    Out << "#ifdef _MSC_VER\n#pragma pack(pop)\n#endif\n";
  return Out;
}

raw_ostream &CWriter::printFunctionAttributes(raw_ostream &Out,
                                              AttributeList Attrs) {
  SmallVector<std::string, 5> AttrsToPrint;
  for (const auto &FnAttr : Attrs.getFnAttributes()) {
    if (FnAttr.isEnumAttribute() || FnAttr.isIntAttribute()) {
      switch (FnAttr.getKindAsEnum()) {
      case Attribute::AttrKind::AlwaysInline:
        AttrsToPrint.push_back("always_inline");
        break;
      case Attribute::AttrKind::Cold:
        AttrsToPrint.push_back("cold");
        break;
      case Attribute::AttrKind::Naked:
        AttrsToPrint.push_back("naked");
        break;
      case Attribute::AttrKind::NoDuplicate:
        AttrsToPrint.push_back("noclone");
        break;
      case Attribute::AttrKind::NoInline:
        AttrsToPrint.push_back("noinline");
        break;
      case Attribute::AttrKind::NoUnwind:
        AttrsToPrint.push_back("nothrow");
        break;
      case Attribute::AttrKind::ReadOnly:
        AttrsToPrint.push_back("pure");
        break;
      case Attribute::AttrKind::ReadNone:
        AttrsToPrint.push_back("const");
        break;
      case Attribute::AttrKind::ReturnsTwice:
        AttrsToPrint.push_back("returns_twice");
        break;
      case Attribute::AttrKind::StackProtect:
      case Attribute::AttrKind::StackProtectReq:
      case Attribute::AttrKind::StackProtectStrong:
        AttrsToPrint.push_back("stack_protect");
        break;
      case Attribute::AttrKind::AllocSize: {
        const auto AllocSize = FnAttr.getAllocSizeArgs();
        if (AllocSize.second.hasValue()) {
          AttrsToPrint.push_back(
              "alloc_size(" + std::to_string(AllocSize.first) + "," +
              std::to_string(AllocSize.second.getValue()) + ")");
        } else {
          AttrsToPrint.push_back("alloc_size(" +
                                 std::to_string(AllocSize.first) + ")");
        }
      } break;

      default:
        break;
      }
    }
    if (FnAttr.isStringAttribute()) {
      if (FnAttr.getKindAsString() == "patchable-function" &&
          FnAttr.getValueAsString() == "prologue-short-redirect") {
        AttrsToPrint.push_back("ms_hook_prologue");
      }
    }
  }
  if (!AttrsToPrint.empty()) {
    headerUseAttributeList();
    Out << " __ATTRIBUTELIST__((";
    bool DidPrintAttr = false;
    for (const auto &Attr : AttrsToPrint) {
      if (DidPrintAttr)
        Out << ", ";
      Out << Attr;
      DidPrintAttr = true;
    }
    Out << "))";
  }
  return Out;
}

raw_ostream &CWriter::printFunctionDeclaration(
    raw_ostream &Out, FunctionType *Ty,
    std::pair<AttributeList, CallingConv::ID> PAL) {
  Out << "typedef ";
  printFunctionProto(Out, Ty, PAL, getFunctionName(Ty, PAL), nullptr);
  return Out << ";\n";
}

// Commonly accepted types and names for main()'s return type and arguments.
static const std::initializer_list<std::pair<StringRef, StringRef>> MainArgs = {
    // Standard C return type.
    {"int", ""},
    // Standard C.
    {"int", "argc"},
    // Standard C. The canonical form is `*argv[]`, but `**argv` is equivalent
    // and more convenient here.
    {"char **", "argv"},
    // De-facto UNIX standard (not POSIX!) extra argument `*envp[]`.
    // The C standard mentions this as a "common extension".
    {"char **", "envp"},
};
// Commonly accepted argument counts for the C main() function.
static const std::initializer_list<unsigned> MainArgCounts = {
    0, // Standard C `main(void)`
    2, // Standard C `main(argc, argv)`
    3, // De-facto UNIX standard `main(argc, argv, envp)`
};

// C compilers are pedantic about the exact type of main(), and this is
// usually an error rather than a warning. Since the C backend emits unsigned
// or explicitly-signed types, it would always get the type of main() wrong.
// Therefore, we use this function to detect common cases and special-case them.
bool CWriter::isStandardMain(const FunctionType *FTy) {
  if (std::find(MainArgCounts.begin(), MainArgCounts.end(),
                FTy->getNumParams()) == MainArgCounts.end())
    return false;

  cwriter_assert(FTy->getNumContainedTypes() <= MainArgs.size());

  for (unsigned i = 0; i < FTy->getNumContainedTypes(); i++) {
    const Type *T = FTy->getContainedType(i);
    const StringRef CType = MainArgs.begin()[i].first;

    if (CType.equals("int") && !T->isIntegerTy())
      return false;

    if (CType.equals("char **") &&
        (!T->isPointerTy() || !T->getPointerElementType()->isPointerTy() ||
         !T->getPointerElementType()->getPointerElementType()->isIntegerTy(8)))
      return false;
  }

  return true;
}

raw_ostream &
CWriter::printFunctionProto(raw_ostream &Out, FunctionType *FTy,
                            std::pair<AttributeList, CallingConv::ID> Attrs,
                            const std::string &Name,
                            iterator_range<Function::arg_iterator> *ArgList) {
  bool shouldFixMain = (Name == "main" && isStandardMain(FTy));

  AttributeList &PAL = Attrs.first;

  if (PAL.hasAttribute(AttributeList::FunctionIndex, Attribute::NoReturn)) {
    headerUseNoReturn();
    Out << "__noreturn ";
  }

  bool isStructReturn = false;
  if (shouldFixMain) {
    Out << MainArgs.begin()[0].first;
  } else {
    // Should this function actually return a struct by-value?
    isStructReturn = PAL.hasAttribute(1, Attribute::StructRet) ||
                     PAL.hasAttribute(2, Attribute::StructRet);
    // Get the return type for the function.
    Type *RetTy;
    if (!isStructReturn)
      RetTy = FTy->getReturnType();
    else {
      // If this is a struct-return function, print the struct-return type.
      RetTy = cast<PointerType>(FTy->getParamType(0))->getElementType();
    }
    printTypeName(
        Out, RetTy,
        /*isSigned=*/
        PAL.hasAttribute(AttributeList::ReturnIndex, Attribute::SExt));
  }

  switch (Attrs.second) {
  case CallingConv::C:
    break;
  // Consider the LLVM fast calling convention as the same as the C calling
  // convention for now.
  case CallingConv::Fast:
    break;
  case CallingConv::X86_StdCall:
    Out << " __stdcall";
    break;
  case CallingConv::X86_FastCall:
    Out << " __fastcall";
    break;
  case CallingConv::X86_ThisCall:
    Out << " __thiscall";
    break;
  default:
    DBG_ERRS("Unhandled calling convention " << Attrs.second);
    errorWithMessage("Encountered Unhandled Calling Convention");
    break;
  }
  Out << ' ' << Name << '(';

  unsigned Idx = 1;
  bool PrintedArg = false;
  FunctionType::param_iterator I = FTy->param_begin(), E = FTy->param_end();
  Function::arg_iterator ArgName =
      ArgList ? ArgList->begin() : Function::arg_iterator();

  // If this is a struct-return function, don't print the hidden
  // struct-return argument.
  if (isStructReturn) {
    cwriter_assert(!shouldFixMain);
    cwriter_assert(I != E && "Invalid struct return function!");
    ++I;
    ++Idx;
    if (ArgList)
      ++ArgName;
  }

  for (; I != E; ++I) {
    Type *ArgTy = *I;
    if (ArgTy->isMetadataTy())
      continue;

    if (PAL.hasAttribute(Idx, Attribute::ByVal)) {
      cwriter_assert(!shouldFixMain);
      cwriter_assert(ArgTy->isPointerTy());
      ArgTy = cast<PointerType>(ArgTy)->getElementType();
    }
    if (PrintedArg)
      Out << ", ";
    if (shouldFixMain)
      Out << MainArgs.begin()[Idx].first;
    else
      printTypeNameUnaligned(
          Out, ArgTy,
          /*isSigned=*/PAL.hasAttribute(Idx, Attribute::SExt));
    PrintedArg = true;
    if (ArgList) {
      Out << ' ';
      if (shouldFixMain)
        Out << MainArgs.begin()[Idx].second;
      else
        Out << GetValueName(ArgName);
      ++ArgName;
    }
    ++Idx;
  }

  if (FTy->isVarArg()) {
    cwriter_assert(!shouldFixMain);
    if (!PrintedArg) {
      Out << "int"; // dummy argument for empty vaarg functs
      if (ArgList)
        Out << " vararg_dummy_arg";
    }
    Out << ", ...";
  } else if (!PrintedArg) {
    Out << "void";
  }
  Out << ")";
  return Out;
}

raw_ostream &CWriter::printArrayDeclaration(raw_ostream &Out, ArrayType *ATy) {
  cwriter_assert(!isEmptyType(ATy));
  // Arrays are wrapped in structs to allow them to have normal
  // value semantics (avoiding the array "decay").
  Out << getArrayName(ATy) << " {\n  ";
  printTypeName(Out, ATy->getElementType());
  Out << " array[" << utostr(ATy->getNumElements()) << "];\n};\n";
  return Out;
}

raw_ostream &CWriter::printVectorDeclaration(raw_ostream &Out,
                                             VectorType *VTy) {
  cwriter_assert(!isEmptyType(VTy));
  // Vectors are printed like arrays
  Out << getVectorName(VTy, false) << " {\n  ";
  printTypeName(Out, VTy->getElementType());
  Out << " vector[" << utostr(NumberOfElements(VTy))
      << "];\n} __attribute__((aligned(" << TD->getABITypeAlignment(VTy)
      << ")));\n";
  return Out;
}

void CWriter::printConstantArray(ConstantArray *CPA,
                                 enum OperandContext Context) {
  printConstant(cast<Constant>(CPA->getOperand(0)), Context);
  for (unsigned i = 1, e = CPA->getNumOperands(); i != e; ++i) {
    Out << ", ";
    printConstant(cast<Constant>(CPA->getOperand(i)), Context);
  }
}

void CWriter::printConstantVector(ConstantVector *CP,
                                  enum OperandContext Context) {
  printConstant(cast<Constant>(CP->getOperand(0)), Context);
  for (unsigned i = 1, e = CP->getNumOperands(); i != e; ++i) {
    Out << ", ";
    printConstant(cast<Constant>(CP->getOperand(i)), Context);
  }
}

void CWriter::printConstantDataSequential(ConstantDataSequential *CDS,
                                          enum OperandContext Context) {
  printConstant(CDS->getElementAsConstant(0), Context);
  for (unsigned i = 1, e = CDS->getNumElements(); i != e; ++i) {
    Out << ", ";
    printConstant(CDS->getElementAsConstant(i), Context);
  }
}

bool CWriter::printConstantString(Constant *C, enum OperandContext Context) {
  // As a special case, print the array as a string if it is an array of
  // ubytes or an array of sbytes with positive values.
  ConstantDataSequential *CDS = dyn_cast<ConstantDataSequential>(C);
  if (!CDS || !CDS->isString())
    return false;
  if (Context != ContextStatic)
    return false; // TODO

  Out << "{ \"";
  // Keep track of whether the last number was a hexadecimal escape.
  bool LastWasHex = false;

  StringRef Bytes = CDS->getAsString();

  unsigned length = Bytes.size();
  // We can skip the last character only if it is an implied null.
  // Beware: C does not force character (i.e. (u)int8_t here) arrays to have a
  // null terminator, but if the length is not specified it will imply one!
  if (length >= 1 && Bytes[length - 1] == '\0')
    length--;

  for (unsigned i = 0; i < length; ++i) {
    unsigned char C = Bytes[i];

    // Print it out literally if it is a printable character.  The only thing
    // to be careful about is when the last letter output was a hex escape
    // code, in which case we have to be careful not to print out hex digits
    // explicitly (the C compiler thinks it is a continuation of the previous
    // character, sheesh...)
    if (isprint(C) && (!LastWasHex || !isxdigit(C))) {
      LastWasHex = false;
      if (C == '"' || C == '\\')
        Out << "\\" << (char)C;
      else
        Out << (char)C;
    } else {
      LastWasHex = false;
      switch (C) {
      case '\n':
        Out << "\\n";
        break;
      case '\t':
        Out << "\\t";
        break;
      case '\r':
        Out << "\\r";
        break;
      case '\v':
        Out << "\\v";
        break;
      case '\a':
        Out << "\\a";
        break;
      case '\"':
        Out << "\\\"";
        break;
      case '\'':
        Out << "\\\'";
        break;
      default:
        Out << "\\x";
        Out << (char)((C / 16 < 10) ? (C / 16 + '0') : (C / 16 - 10 + 'A'));
        Out << (char)(((C & 15) < 10) ? ((C & 15) + '0')
                                      : ((C & 15) - 10 + 'A'));
        LastWasHex = true;
        break;
      }
    }
  }
  Out << "\" }";
  return true;
}

// isFPCSafeToPrint - Returns true if we may assume that CFP may be written out
// textually as a double (rather than as a reference to a stack-allocated
// variable). We decide this by converting CFP to a string and back into a
// double, and then checking whether the conversion results in a bit-equal
// double to the original value of CFP. This depends on us and the target C
// compiler agreeing on the conversion process (which is pretty likely since we
// only deal in IEEE FP).

// TODO copied from CppBackend, new code should use raw_ostream
static inline std::string ftostr(const APFloat &V) {
  if (&V.getSemantics() != &APFloat::IEEEdouble() &&
      &V.getSemantics() != &APFloat::IEEEsingle()) {
    return "<unknown format in ftostr>"; // error
  }
  SmallVector<char, 32> Buffer;
  V.toString(Buffer);
  return std::string(Buffer.data(), Buffer.size());
}

static bool isFPCSafeToPrint(const ConstantFP *CFP) {
  bool ignored;
  // Do long doubles in hex for now.
  if (CFP->getType() != Type::getFloatTy(CFP->getContext()) &&
      CFP->getType() != Type::getDoubleTy(CFP->getContext()))
    return false;
  APFloat APF = APFloat(CFP->getValueAPF()); // copy
  if (CFP->getType() == Type::getFloatTy(CFP->getContext()))
    APF.convert(APFloat::IEEEdouble(), APFloat::rmNearestTiesToEven, &ignored);
#if HAVE_PRINTF_A && ENABLE_CBE_PRINTF_A
  char Buffer[100];
  sprintf(Buffer, "%a", APF.convertToDouble());
  if (!strncmp(Buffer, "0x", 2) || !strncmp(Buffer, "-0x", 3) ||
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
    errorWithMessage("Invalid cast opcode");
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
    errorWithMessage("Invalid cast opcode");
  }
}

// printConstant - The LLVM Constant to C Constant converter.
void CWriter::printConstant(Constant *CPV, enum OperandContext Context) {
  if (ConstantExpr *CE = dyn_cast<ConstantExpr>(CPV)) {
    // TODO: VectorType are valid here, but not supported
    if (!CE->getType()->isIntegerTy() && !CE->getType()->isFloatingPointTy() &&
        !CE->getType()->isPointerTy()) {
      DBG_ERRS("Unsupported constant type " << *CE->getType()
                                            << " in: " << *CE);
      errorWithMessage("Unsupported constant type");
    }
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
      printConstant(CE->getOperand(0), ContextCasted);
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
                         gep_type_end(CPV));
      Out << ")";
      return;
    case Instruction::Select:
      Out << '(';
      printConstant(CE->getOperand(0), ContextCasted);
      Out << '?';
      printConstant(CE->getOperand(1), ContextNormal);
      Out << ':';
      printConstant(CE->getOperand(2), ContextNormal);
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
    case Instruction::AShr: {
      Out << '(';
      bool NeedsClosingParens = printConstExprCast(CE);
      printConstantWithCast(CE->getOperand(0), CE->getOpcode());
      switch (CE->getOpcode()) {
      case Instruction::Add:
      case Instruction::FAdd:
        Out << " + ";
        break;
      case Instruction::Sub:
      case Instruction::FSub:
        Out << " - ";
        break;
      case Instruction::Mul:
      case Instruction::FMul:
        Out << " * ";
        break;
      case Instruction::URem:
      case Instruction::SRem:
      case Instruction::FRem:
        Out << " % ";
        break;
      case Instruction::UDiv:
      case Instruction::SDiv:
      case Instruction::FDiv:
        Out << " / ";
        break;
      case Instruction::And:
        Out << " & ";
        break;
      case Instruction::Or:
        Out << " | ";
        break;
      case Instruction::Xor:
        Out << " ^ ";
        break;
      case Instruction::Shl:
        Out << " << ";
        break;
      case Instruction::LShr:
      case Instruction::AShr:
        Out << " >> ";
        break;
      case Instruction::ICmp:
        switch (CE->getPredicate()) {
        case ICmpInst::ICMP_EQ:
          Out << " == ";
          break;
        case ICmpInst::ICMP_NE:
          Out << " != ";
          break;
        case ICmpInst::ICMP_SLT:
        case ICmpInst::ICMP_ULT:
          Out << " < ";
          break;
        case ICmpInst::ICMP_SLE:
        case ICmpInst::ICMP_ULE:
          Out << " <= ";
          break;
        case ICmpInst::ICMP_SGT:
        case ICmpInst::ICMP_UGT:
          Out << " > ";
          break;
        case ICmpInst::ICMP_SGE:
        case ICmpInst::ICMP_UGE:
          Out << " >= ";
          break;
        default:
          errorWithMessage("Illegal ICmp predicate");
        }
        break;
      default:
        errorWithMessage("Illegal opcode here!");
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
        const auto Pred = (CmpInst::Predicate)CE->getPredicate();
        headerUseFCmpOp(Pred);
        Out << "llvm_fcmp_" << getCmpPredicateName(Pred) << "(";
        printConstant(CE->getOperand(0), ContextCasted);
        Out << ", ";
        printConstant(CE->getOperand(1), ContextCasted);
        Out << ")";
      }
      if (NeedsClosingParens)
        Out << "))";
      Out << ')';
      return;
    }
    default:
      DBG_ERRS("CWriter Error: Unhandled constant expression: " << *CE);
      errorWithMessage("unhandled constant expression");
    }
  } else if (isa<UndefValue>(CPV) && CPV->getType()->isSingleValueType()) {
    if (CPV->getType()->isVectorTy()) {
      if (Context == ContextStatic) {
        Out << "{}";
        return;
      }
      VectorType *VT = cast<VectorType>(CPV->getType());
      cwriter_assert(!isEmptyType(VT));
      CtorDeclTypes.insert(VT);
      Out << "/*undef*/llvm_ctor_";
      printTypeString(Out, VT, false);
      Out << "(";
      Constant *Zero = Constant::getNullValue(VT->getElementType());

      unsigned NumElts = NumberOfElements(VT);
      for (unsigned i = 0; i != NumElts; ++i) {
        if (i)
          Out << ", ";
        printConstant(Zero, ContextCasted);
      }
      Out << ")";

    } else {
      Constant *Zero = Constant::getNullValue(CPV->getType());
      Out << "/*UNDEF*/";
      return printConstant(Zero, Context);
    }
    return;
  }

  if (ConstantInt *CI = dyn_cast<ConstantInt>(CPV)) {
    Type *Ty = CI->getType();
    unsigned ActiveBits = CI->getValue().getMinSignedBits();
    if (Ty == Type::getInt1Ty(CPV->getContext())) {
      Out << (CI->getZExtValue() ? '1' : '0');
    } else if (Context != ContextNormal && Ty->getPrimitiveSizeInBits() <= 64 &&
               ActiveBits < Ty->getPrimitiveSizeInBits()) {
      if (ActiveBits >= 32)
        Out << "INT64_C(";
      Out << CI->getSExtValue(); // most likely a shorter representation
      if (ActiveBits >= 32)
        Out << ")";
    } else if (Ty->getPrimitiveSizeInBits() < 32 && Context == ContextNormal) {
      Out << "((";
      printSimpleType(Out, Ty, false) << ')';
      if (CI->isMinValue(true))
        Out << CI->getZExtValue() << 'u';
      else
        Out << CI->getSExtValue();
      Out << ')';
    } else if (Ty->getPrimitiveSizeInBits() <= 32) {
      Out << CI->getZExtValue() << 'u';
    } else if (Ty->getPrimitiveSizeInBits() <= 64) {
      Out << "UINT64_C(" << CI->getZExtValue() << ")";
    } else if (Ty->getPrimitiveSizeInBits() <= 128) {
      headerUseInt128();
      const APInt &V = CI->getValue();
      const APInt &Vlo = V.getLoBits(64);
      const APInt &Vhi = V.getHiBits(64);
      Out << (Context == ContextStatic ? "UINT128_C" : "llvm_ctor_u128");
      Out << "(UINT64_C(" << Vhi.getZExtValue() << "), UINT64_C("
          << Vlo.getZExtValue() << "))";
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
    auto I = FPConstantMap.find(FPC);
    if (I != FPConstantMap.end()) {
      // Because of FP precision problems we must load from a stack allocated
      // value that holds the value in hex.
      Out << "(*("
          << (FPC->getType() == Type::getFloatTy(CPV->getContext()) ? "float"
              : FPC->getType() == Type::getDoubleTy(CPV->getContext())
                  ? "double"
                  : "long double")
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
        Tmp.convert(APFloat::IEEEdouble(), APFloat::rmTowardZero, &LosesInfo);
        V = Tmp.convertToDouble();
      }

      if (std::isnan(V)) {
        // The value is NaN

        // FIXME the actual NaN bits should be emitted.
        // The prefix for a quiet NaN is 0x7FF8. For a signalling NaN,
        // it's 0x7ff4.
        const unsigned long QuietNaN = 0x7ff8UL;
        // const unsigned long SignalNaN = 0x7ff4UL;

        // We need to grab the first part of the FP #
        char Buffer[100];

        uint64_t ll = DoubleToBits(V);
        sprintf(Buffer, "0x%llx", static_cast<long long>(ll));

        std::string Num(&Buffer[0], &Buffer[6]);
        unsigned long Val = strtoul(Num.c_str(), 0, 16);

        headerUseNanInf();
        if (FPC->getType() == Type::getFloatTy(FPC->getContext()))
          Out << "LLVM_NAN" << (Val == QuietNaN ? "" : "S") << "F(\"" << Buffer
              << "\") /*nan*/ ";
        else
          Out << "LLVM_NAN" << (Val == QuietNaN ? "" : "S") << "(\"" << Buffer
              << "\") /*nan*/ ";
      } else if (std::isinf(V)) {
        // The value is Inf
        if (V < 0)
          Out << '-';
        headerUseNanInf();
        Out << "LLVM_INF"
            << (FPC->getType() == Type::getFloatTy(FPC->getContext()) ? "F"
                                                                      : "")
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

  case Type::ArrayTyID: {
    if (printConstantString(CPV, Context))
      break;
    ArrayType *AT = cast<ArrayType>(CPV->getType());
    cwriter_assert(AT->getNumElements() != 0 && !isEmptyType(AT));
    if (Context != ContextStatic) {
      CtorDeclTypes.insert(AT);
      Out << "llvm_ctor_";
      printTypeString(Out, AT, false);
      Out << "(";
      Context = ContextCasted;
    } else {
      Out << "{ { "; // Arrays are wrapped in struct types.
    }
    if (ConstantArray *CA = dyn_cast<ConstantArray>(CPV)) {
      printConstantArray(CA, Context);
    } else if (ConstantDataSequential *CDS =
                   dyn_cast<ConstantDataSequential>(CPV)) {
      printConstantDataSequential(CDS, Context);
    } else {
      cwriter_assert(isa<ConstantAggregateZero>(CPV) || isa<UndefValue>(CPV));
      Constant *CZ = Constant::getNullValue(AT->getElementType());
      printConstant(CZ, Context);
      for (unsigned i = 1, e = AT->getNumElements(); i != e; ++i) {
        Out << ", ";
        printConstant(CZ, Context);
      }
    }
    Out << (Context == ContextStatic
                ? " } }"
                : ")"); // Arrays are wrapped in struct types.
    break;
  }
#if LLVM_VERSION_MAJOR > 10
  case Type::FixedVectorTyID:
  case Type::ScalableVectorTyID:
#else
  case Type::VectorTyID:
#endif
  {
#if LLVM_VERSION_MAJOR > 10
    FixedVectorType *VT = cast<FixedVectorType>(CPV->getType());
#else
    VectorType *VT = cast<VectorType>(CPV->getType());
#endif
    cwriter_assert(VT->getNumElements() != 0 && !isEmptyType(VT));
    if (Context != ContextStatic) {
      CtorDeclTypes.insert(VT);
      Out << "llvm_ctor_";
      printTypeString(Out, VT, false);
      Out << "(";
      Context = ContextCasted;
    } else {
      Out << "{ ";
    }
    if (ConstantVector *CV = dyn_cast<ConstantVector>(CPV)) {
      printConstantVector(CV, Context);
    } else if (ConstantDataSequential *CDS =
                   dyn_cast<ConstantDataSequential>(CPV)) {
      printConstantDataSequential(CDS, Context);
    } else {
      cwriter_assert(isa<ConstantAggregateZero>(CPV) || isa<UndefValue>(CPV));
      Constant *CZ = Constant::getNullValue(VT->getElementType());
      printConstant(CZ, Context);

      for (unsigned i = 1, e = NumberOfElements(VT); i != e; ++i) {
        Out << ", ";
        printConstant(CZ, Context);
      }
    }
    Out << (Context == ContextStatic ? " }" : ")");
    break;
  }

  case Type::StructTyID: {
    StructType *ST = cast<StructType>(CPV->getType());
    cwriter_assert(!isEmptyType(ST));
    if (Context != ContextStatic) {
      CtorDeclTypes.insert(ST);
      Out << "llvm_ctor_";
      printTypeString(Out, ST, false);
      Out << "(";
      Context = ContextCasted;
    } else {
      Out << "{ ";
    }

    if (isa<ConstantAggregateZero>(CPV) || isa<UndefValue>(CPV)) {
      bool printed = false;
      for (unsigned i = 0, e = ST->getNumElements(); i != e; ++i) {
        Type *ElTy = ST->getElementType(i);
        if (isEmptyType(ElTy))
          continue;
        if (printed)
          Out << ", ";
        printConstant(Constant::getNullValue(ElTy), Context);
        printed = true;
      }
      cwriter_assert(printed);
    } else {
      bool printed = false;
      for (unsigned i = 0, e = CPV->getNumOperands(); i != e; ++i) {
        Constant *C = cast<Constant>(CPV->getOperand(i));
        if (isEmptyType(C->getType()))
          continue;
        if (printed)
          Out << ", ";
        printConstant(C, Context);
        printed = true;
      }
      cwriter_assert(printed);
    }
    Out << (Context == ContextStatic ? " }" : ")");
    break;
  }

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
    DBG_ERRS("Unknown constant type: " << *CPV);
    errorWithMessage("unknown constant type");
  }
}

// Some constant expressions need to be casted back to the original types
// because their operands were casted to the expected type. This function takes
// care of detecting that case and printing the cast for the ConstantExpr.
bool CWriter::printConstExprCast(ConstantExpr *CE) {
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
  case Instruction::UDiv:
    NeedsExplicitCast = true;
    break;
  case Instruction::AShr:
  case Instruction::SRem:
  case Instruction::SDiv:
    NeedsExplicitCast = true;
    TypeIsSigned = true;
    break;
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
  default:
    break;
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
void CWriter::printConstantWithCast(Constant *CPV, unsigned Opcode) {

  // Extract the operand's type, we'll need it.
  Type *OpTy = CPV->getType();
  // TODO: VectorType are valid here, but not supported
  if (!OpTy->isIntegerTy() && !OpTy->isFloatingPointTy()) {
    DBG_ERRS("Unsupported 'constant with cast' type " << *OpTy
                                                      << " in: " << *CPV);
    errorWithMessage("Unsupported 'constant with cast' type");
  }

  // Indicate whether to do the cast or not.
  bool shouldCast;
  bool typeIsSigned;
  opcodeNeedsCast(Opcode, shouldCast, typeIsSigned);

  // Write out the casted constant if we should, otherwise just write the
  // operand.
  if (shouldCast) {
    Out << "((";
    printSimpleType(Out, OpTy, typeIsSigned);
    Out << ")";
    printConstant(CPV, ContextCasted);
    Out << ")";
  } else
    printConstant(CPV, ContextCasted);
}

std::string CWriter::GetValueName(Value *Operand) {

  // Resolve potential alias.
  if (GlobalAlias *GA = dyn_cast<GlobalAlias>(Operand)) {
    Operand = GA->getAliasee();
  }

  std::string Name{Operand->getName()};
  if (Name.empty()) { // Assign unique names to local temporaries.
    unsigned No = AnonValueNumbers.getOrInsert(Operand);

    Name = "_" + utostr(No);
    if (!TheModule->getNamedValue(Name)) {
      // Short name for the common case where there's no conflicting global.
      return Name;
    }

    Name = "tmp_" + Name;
  }

  // Mangle globals with the standard mangler interface for LLC compatibility.
  if (isa<GlobalValue>(Operand)) {
    return CBEMangle(Name);
  }

  std::string VarName;
  VarName.reserve(Name.capacity());

  for (std::string::iterator I = Name.begin(), E = Name.end(); I != E; ++I) {
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
    IntegerType *ITy = static_cast<IntegerType *>(Ty);
#if LLVM_VERSION_MAJOR <= 10
    if (!ITy->isPowerOf2ByteWidth())
#else
    if (!IsPowerOfTwo(ITy->getBitWidth()))
#endif
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

void CWriter::writeOperandInternal(Value *Operand,
                                   enum OperandContext Context) {
  if (Instruction *I = dyn_cast<Instruction>(Operand))
    // Should we inline this instruction to build a tree?
    if (isInlinableInst(*I) && !isDirectAlloca(I)) {
      Out << '(';
      writeInstComputationInline(*I);
      Out << ')';
      return;
    }

  Constant *CPV = dyn_cast<Constant>(Operand);

  if (CPV && !isa<GlobalValue>(CPV))
    printConstant(CPV, Context);
  else
    Out << GetValueName(Operand);
}

void CWriter::writeOperand(Value *Operand, enum OperandContext Context) {
  bool isAddressImplicit = isAddressExposed(Operand);
  // Global variables are referenced as their addresses by llvm
  if (isAddressImplicit) {
    // We can't directly declare a zero-sized variable in C, so
    // printTypeNameForAddressableValue uses a single-byte type instead.
    // We fix up the pointer type here.
    if (!isEmptyType(Operand->getType()->getPointerElementType()))
      Out << "(&";
    else
      Out << "((void*)&";
  }

  writeOperandInternal(Operand, Context);

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
  default:
    break;
  }
  return false;
}

// Write the operand with a cast to another type based on the Opcode being used.
// This will be used in cases where an instruction has specific type
// requirements (usually signedness) for its operands.
void CWriter::opcodeNeedsCast(
    unsigned Opcode,
    // Indicate whether to do the cast or not.
    bool &shouldCast,
    // Indicate whether the cast should be to a signed type or not.
    bool &castIsSigned) {

  // Based on the Opcode for which this Operand is being written, determine
  // the new type to which the operand should be casted by setting the value
  // of OpTy. If we change OpTy, also set shouldCast to true.
  switch (Opcode) {
  default:
    // for most instructions, it doesn't matter
    shouldCast = false;
    castIsSigned = false;
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

void CWriter::writeOperandWithCast(Value *Operand, unsigned Opcode) {
  // Write out the casted operand if we should, otherwise just write the
  // operand.

  // Extract the operand's type, we'll need it.
  bool shouldCast;
  bool castIsSigned;
  opcodeNeedsCast(Opcode, shouldCast, castIsSigned);

  Type *OpTy = Operand->getType();
  if (shouldCast) {
    Out << "((";
    printSimpleType(Out, OpTy, castIsSigned);
    Out << ")";
    writeOperand(Operand, ContextCasted);
    Out << ")";
  } else
    writeOperand(Operand, ContextCasted);
}

// Write the operand with a cast to another type based on the icmp predicate
// being used.
void CWriter::writeOperandWithCast(Value *Operand, ICmpInst &Cmp) {
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
  Type *OpTy = Operand->getType();
  if (OpTy->isPointerTy())
    OpTy = TD->getIntPtrType(Operand->getContext());

  Out << "((";
  printSimpleType(Out, OpTy, castIsSigned);
  Out << ")";
  writeOperand(Operand);
  Out << ")";
}

static void defineConstantDoubleTy(raw_ostream &Out) {
  Out << "typedef uint64_t ConstantDoubleTy;\n";
}

static void defineConstantFloatTy(raw_ostream &Out) {
  Out << "typedef uint32_t ConstantFloatTy;\n";
}

static void defineConstantFP80Ty(raw_ostream &Out) {
  Out << "typedef struct { uint64_t f1; uint16_t f2; "
         "uint16_t pad[3]; } ConstantFP80Ty;\n";
}

static void defineConstantFP128Ty(raw_ostream &Out) {
  // This is used for both kinds of 128-bit long double; meaning differs.
  Out << "typedef struct { uint64_t f1; uint64_t f2; }"
         " ConstantFP128Ty;\n";
}

static void defineBuiltinAlloca(raw_ostream &Out) {
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
      << "#elif defined(__FreeBSD__) || defined(__NetBSD__) || "
         "defined(__OpenBSD__) || defined(__DragonFly__) || defined(__arm__)\n"
      << "#define alloca(x) __builtin_alloca(x)\n"
      << "#elif defined(_MSC_VER)\n"
      << "#define alloca(x) _alloca(x)\n"
      << "#else\n"
      << "#include <alloca.h>\n"
      << "#endif\n\n";
}

static void defineExternalWeak(raw_ostream &Out) {
  // On Mac OS X, "external weak" is spelled "__attribute__((weak_import))".
  Out << "#if defined(__GNUC__) && defined(__APPLE_CC__)\n"
      << "#define __EXTERNAL_WEAK__ __attribute__((weak_import))\n"
      << "#elif defined(__GNUC__)\n"
      << "#define __EXTERNAL_WEAK__ __attribute__((weak))\n"
      << "#else\n"
      << "#define __EXTERNAL_WEAK__\n"
      << "#endif\n\n";
}

static void defineAttributeWeak(raw_ostream &Out) {
  // For now, turn off the weak linkage attribute on Mac OS X. (See above.)
  Out << "#if defined(__GNUC__) && defined(__APPLE_CC__)\n"
      << "#define __ATTRIBUTE_WEAK__\n"
      << "#elif defined(__GNUC__)\n"
      << "#define __ATTRIBUTE_WEAK__ __attribute__((weak))\n"
      << "#else\n"
      << "#define __ATTRIBUTE_WEAK__\n"
      << "#endif\n\n";
}

static void defineHidden(raw_ostream &Out) {
  // Add hidden visibility support. FIXME: APPLE_CC?
  Out << "#if defined(__GNUC__)\n"
      << "#define __HIDDEN__ __attribute__((visibility(\"hidden\")))\n"
      << "#endif\n\n";
}

static void defineAttributeList(raw_ostream &Out) {
  // gcc attributes
  Out << "#if defined(__GNUC__)\n"
      << "#define  __ATTRIBUTELIST__(x) __attribute__(x)\n"
      << "#else\n"
      << "#define  __ATTRIBUTELIST__(x)  \n"
      << "#endif\n\n";

  // We output GCC specific attributes to preserve 'linkonce'ness on globals.
  // If we aren't being compiled with GCC, just drop these attributes.
  Out << "#ifdef _MSC_VER  /* Can only support \"linkonce\" vars with GCC */\n"
      << "#define __attribute__(X)\n"
      << "#endif\n\n";
}

static void defineUnalignedLoad(raw_ostream &Out) {
  // Define unaligned-load helper macro
  Out << "#ifdef _MSC_VER\n";
  Out << "#define __UNALIGNED_LOAD__(type, align, op) *((type "
         "__unaligned*)op)\n";
  Out << "#else\n";
  Out << "#define __UNALIGNED_LOAD__(type, align, op) ((struct { type data "
         "__attribute__((packed, aligned(align))); }*)op)->data\n";
  Out << "#endif\n\n";
}

static void defineMsAlign(raw_ostream &Out) {
  Out << "#ifdef _MSC_VER\n";
  Out << "#define __MSALIGN__(X) __declspec(align(X))\n";
  Out << "#else\n";
  Out << "#define __MSALIGN__(X)\n";
  Out << "#endif\n\n";
}

static void defineUnreachable(raw_ostream &Out) {
  Out << "#ifdef _MSC_VER\n";
  Out << "#define __builtin_unreachable() __assume(0)\n";
  Out << "#endif\n";
}

static void defineNoReturn(raw_ostream &Out) {
  Out << "#ifdef _MSC_VER\n";
  Out << "#define __noreturn __declspec(noreturn)\n";
  Out << "#else\n";
  Out << "#define __noreturn __attribute__((noreturn))\n";
  Out << "#endif\n";
}

static void defineForceInline(raw_ostream &Out) {
  Out << "#ifndef _MSC_VER\n";
  Out << "#define __forceinline __attribute__((always_inline)) inline\n";
  Out << "#endif\n\n";
}

static void defineNanInf(raw_ostream &Out) {
  // Define NaN and Inf as GCC builtins if using GCC
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
      //<< "#define LLVM_NANS(NanStr)  __builtin_nans(NanStr)  /* Double */\n"
      //<< "#define LLVM_NANSF(NanStr) __builtin_nansf(NanStr) /* Float */\n"
      << "#define LLVM_INF           __builtin_inf()         /* Double */\n"
      << "#define LLVM_INFF          __builtin_inff()        /* Float */\n"
      << "#define LLVM_PREFETCH(addr,rw,locality) "
         "__builtin_prefetch(addr,rw,locality)\n"
      << "#define __ATTRIBUTE_CTOR__ __attribute__((constructor))\n"
      << "#define __ATTRIBUTE_DTOR__ __attribute__((destructor))\n"
      << "#else\n"
      << "#define LLVM_NAN(NanStr)   ((double)NAN)           /* Double */\n"
      << "#define LLVM_NANF(NanStr)  ((float)NAN))           /* Float */\n"
      //<< "#define LLVM_NANS(NanStr)  ((double)NAN)           /* Double */\n"
      //<< "#define LLVM_NANSF(NanStr) ((single)NAN)           /* Float */\n"
      << "#define LLVM_INF           ((double)INFINITY)      /* Double */\n"
      << "#define LLVM_INFF          ((float)INFINITY)       /* Float */\n"
      << "#define LLVM_PREFETCH(addr,rw,locality)            /* PREFETCH */\n"
      << "#define __ATTRIBUTE_CTOR__ \"__attribute__((constructor)) not "
         "supported on this compiler\"\n"
      << "#define __ATTRIBUTE_DTOR__ \"__attribute__((destructor)) not "
         "supported on this compiler\"\n"
      << "#endif\n\n";
}

static void defineStackSaveRestore(raw_ostream &Out) {
  Out << "#if !defined(__GNUC__) || __GNUC__ < 4 /* Old GCC's, or compilers "
         "not GCC */ \n"
      << "#define __builtin_stack_save() 0   /* not implemented */\n"
      << "#define __builtin_stack_restore(X) /* noop */\n"
      << "#endif\n\n";
}

static void defineInt128(raw_ostream &Out) {
  // Output typedefs for 128-bit integers
  Out << "#if defined(__GNUC__) && defined(__LP64__) /* 128-bit integer types "
         "*/\n"
      << "typedef int __attribute__((mode(TI))) int128_t;\n"
      << "typedef unsigned __attribute__((mode(TI))) uint128_t;\n"
      << "#define UINT128_C(hi, lo) (((uint128_t)(hi) << 64) | "
         "(uint128_t)(lo))\n"
      << "static __forceinline uint128_t llvm_ctor_u128(uint64_t hi, uint64_t "
         "lo) {"
      << " return UINT128_C(hi, lo); }\n"
      << "static __forceinline bool llvm_icmp_eq_u128(uint128_t l, uint128_t "
         "r) {"
      << " return l == r; }\n"
      << "static __forceinline bool llvm_icmp_ne_u128(uint128_t l, uint128_t "
         "r) {"
      << " return l != r; }\n"
      << "static __forceinline bool llvm_icmp_ule_u128(uint128_t l, uint128_t "
         "r) {"
      << " return l <= r; }\n"
      << "static __forceinline bool llvm_icmp_sle_i128(int128_t l, int128_t r) "
         "{"
      << " return l <= r; }\n"
      << "static __forceinline bool llvm_icmp_uge_u128(uint128_t l, uint128_t "
         "r) {"
      << " return l >= r; }\n"
      << "static __forceinline bool llvm_icmp_sge_i128(int128_t l, int128_t r) "
         "{"
      << " return l >= r; }\n"
      << "static __forceinline bool llvm_icmp_ult_u128(uint128_t l, uint128_t "
         "r) {"
      << " return l < r; }\n"
      << "static __forceinline bool llvm_icmp_slt_i128(int128_t l, int128_t r) "
         "{"
      << " return l < r; }\n"
      << "static __forceinline bool llvm_icmp_ugt_u128(uint128_t l, uint128_t "
         "r) {"
      << " return l > r; }\n"
      << "static __forceinline bool llvm_icmp_sgt_i128(int128_t l, int128_t r) "
         "{"
      << " return l > r; }\n"

      << "#else /* manual 128-bit types */\n"
      // TODO: field order should be reversed for big-endian
      << "typedef struct { uint64_t lo; uint64_t hi; } uint128_t;\n"
      << "typedef uint128_t int128_t;\n"
      << "#define UINT128_C(hi, lo) {(lo), (hi)}\n" // only use in Static
                                                    // context
      << "static __forceinline uint128_t llvm_ctor_u128(uint64_t hi, uint64_t "
         "lo) {"
      << " uint128_t r; r.lo = lo; r.hi = hi; return r; }\n"
      << "static __forceinline bool llvm_icmp_eq_u128(uint128_t l, uint128_t "
         "r) {"
      << " return l.hi == r.hi && l.lo == r.lo; }\n"
      << "static __forceinline bool llvm_icmp_ne_u128(uint128_t l, uint128_t "
         "r) {"
      << " return l.hi != r.hi || l.lo != r.lo; }\n"
      << "static __forceinline bool llvm_icmp_ule_u128(uint128_t l, uint128_t "
         "r) {"
      << " return l.hi < r.hi ? 1 : (l.hi == r.hi ? l.lo <= l.lo : 0); }\n"
      << "static __forceinline bool llvm_icmp_sle_i128(int128_t l, int128_t r) "
         "{"
      << " return (int64_t)l.hi < (int64_t)r.hi ? 1 : (l.hi == r.hi ? "
         "(int64_t)l.lo <= (int64_t)l.lo : 0); }\n"
      << "static __forceinline bool llvm_icmp_uge_u128(uint128_t l, uint128_t "
         "r) {"
      << " return l.hi > r.hi ? 1 : (l.hi == r.hi ? l.lo >= l.hi : 0); }\n"
      << "static __forceinline bool llvm_icmp_sge_i128(int128_t l, int128_t r) "
         "{"
      << " return (int64_t)l.hi > (int64_t)r.hi ? 1 : (l.hi == r.hi ? "
         "(int64_t)l.lo >= (int64_t)l.lo : 0); }\n"
      << "static __forceinline bool llvm_icmp_ult_u128(uint128_t l, uint128_t "
         "r) {"
      << " return l.hi < r.hi ? 1 : (l.hi == r.hi ? l.lo < l.hi : 0); }\n"
      << "static __forceinline bool llvm_icmp_slt_i128(int128_t l, int128_t r) "
         "{"
      << " return (int64_t)l.hi < (int64_t)r.hi ? 1 : (l.hi == r.hi ? "
         "(int64_t)l.lo < (int64_t)l.lo : 0); }\n"
      << "static __forceinline bool llvm_icmp_ugt_u128(uint128_t l, uint128_t "
         "r) {"
      << " return l.hi > r.hi ? 1 : (l.hi == r.hi ? l.lo > l.hi : 0); }\n"
      << "static __forceinline bool llvm_icmp_sgt_i128(int128_t l, int128_t r) "
         "{"
      << " return (int64_t)l.hi > (int64_t)r.hi ? 1 : (l.hi == r.hi ? "
         "(int64_t)l.lo > (int64_t)l.lo : 0); }\n"
      << "#define __emulate_i128\n"
      << "#endif\n\n";
}

static void defineThreadFence(raw_ostream &Out) {
  Out << "#ifdef _MSC_VER\n"
      << "#define __atomic_thread_fence(x) __faststorefence\n"
      << "#endif\n\n";
}

/// FindStaticTors - Given a static ctor/dtor list, unpack its contents into
/// the StaticTors set.
static void FindStaticTors(GlobalVariable *GV,
                           std::set<Function *> &StaticTors) {
  ConstantArray *InitList = dyn_cast<ConstantArray>(GV->getInitializer());
  if (!InitList)
    return;

  for (unsigned i = 0, e = InitList->getNumOperands(); i != e; ++i)
    if (ConstantStruct *CS =
            dyn_cast<ConstantStruct>(InitList->getOperand(i))) {
      if (CS->getNumOperands() != 2)
        return; // Not array of 2-element structs.

      if (CS->getOperand(1)->isNullValue())
        return; // Found a null terminator, exit printing.
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
  GlobalCtors,
  GlobalDtors,
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

// generateCompilerSpecificCode - This is where we add conditional compilation
// directives to cater to specific compilers as need be.
void CWriter::generateCompilerSpecificCode(raw_ostream &Out,
                                           const DataLayout *) const {
  if (headerIncConstantDoubleTy())
    defineConstantDoubleTy(Out);
  if (headerIncConstantFloatTy())
    defineConstantFloatTy(Out);
  if (headerIncConstantFP80Ty())
    defineConstantFP80Ty(Out);
  if (headerIncConstantFP128Ty())
    defineConstantFP128Ty(Out);
  if (headerIncBuiltinAlloca())
    defineBuiltinAlloca(Out);
  if (headerIncUnreachable())
    defineUnreachable(Out);
  if (headerIncNoReturn())
    defineNoReturn(Out);
  if (headerIncForceInline())
    defineForceInline(Out);
  if (headerIncExternalWeak())
    defineExternalWeak(Out);
  if (headerIncAttributeWeak())
    defineAttributeWeak(Out);
  if (headerIncHidden())
    defineHidden(Out);
  if (headerIncAttributeList())
    defineAttributeList(Out);
  if (headerIncUnalignedLoad())
    defineUnalignedLoad(Out);
  if (headerIncMsAlign())
    defineMsAlign(Out);
  if (headerIncNanInf())
    defineNanInf(Out);
  if (headerIncInt128())
    defineInt128(Out);
  if (headerIncThreadFence())
    defineThreadFence(Out);
  if (headerIncStackSaveRestore())
    defineStackSaveRestore(Out);
}

bool CWriter::doInitialization(Module &M) {
  TheModule = &M;

  TD = new DataLayout(&M);
  IL = new IntrinsicLowering(*TD);
#if LLVM_VERSION_MAJOR < 9
  IL->AddPrototypes(M);
#endif

#if 0
  std::string Triple = TheModule->getTargetTriple();
  if (Triple.empty())
    Triple = llvm::sys::getDefaultTargetTriple();

  std::string E;
  if (const Target *Match = TargetRegistry::lookupTarget(Triple, E))
    TAsm = Match->createMCAsmInfo(Triple);
#endif
  TAsm = new CBEMCAsmInfo();
  MRI = new MCRegisterInfo();
#if LLVM_VERSION_MAJOR > 12
  TCtx = new MCContext(llvm::Triple(TheModule->getTargetTriple()),TAsm, MRI, nullptr);
#else
  TCtx = new MCContext(TAsm, MRI, nullptr);
#endif
  return false;
}

bool CWriter::doFinalization(Module &M) {
  // Output all code to the file
  std::string methods = Out.str();
  _Out.clear();
  generateHeader(M);
  std::string header = OutHeaders.str() + Out.str();
  _Out.clear();
  _OutHeaders.clear();
  FileOut << header << methods;

  // Free memory...

  delete IL;
  IL = nullptr;

  delete TD;
  TD = nullptr;

  delete TCtx;
  TCtx = nullptr;

  delete TAsm;
  TAsm = nullptr;

  delete MRI;
  MRI = nullptr;

  delete MOFI;
  MOFI = nullptr;

  FPConstantMap.clear();
  ByValParams.clear();
  AnonValueNumbers.clear();
  UnnamedStructIDs.clear();
  UnnamedFunctionIDs.clear();
  TypedefDeclTypes.clear();
  SelectDeclTypes.clear();
  CmpDeclTypes.clear();
  CastOpDeclTypes.clear();
  InlineOpDeclTypes.clear();
  CtorDeclTypes.clear();
  prototypesToGen.clear();

  return true; // may have lowered an IntrinsicCall
}

void CWriter::generateHeader(Module &M) {
  // Keep track of which functions are static ctors/dtors so they can have
  // an attribute added to their prototypes.
  std::set<Function *> StaticCtors, StaticDtors;
  for (Module::global_iterator I = M.global_begin(), E = M.global_end(); I != E;
       ++I) {
    switch (getGlobalVariableClass(&*I)) {
    default:
      break;
    case GlobalCtors:
      FindStaticTors(&*I, StaticCtors);
      break;
    case GlobalDtors:
      FindStaticTors(&*I, StaticDtors);
      break;
    }
  }

  // Include required standard headers
  OutHeaders << "/* Provide Declarations */\n";
  if (headerIncStdarg())
    OutHeaders << "#include <stdarg.h>\n";
  if (headerIncSetjmp())
    OutHeaders << "#include <setjmp.h>\n";
  if (headerIncLimits())
    OutHeaders << "#include <limits.h>\n";
  // Support for integers with explicit sizes. This one isn't conditional
  // because virtually all CBE output will use it.
  OutHeaders << "#include <stdint.h>\n"; // Sized integer support
  if (headerIncMath())
    OutHeaders << "#include <math.h>\n";
  // Provide a definition for `bool' if not compiling with a C++ compiler.
  OutHeaders << "#ifndef __cplusplus\ntypedef unsigned char bool;\n#endif\n";
  OutHeaders << "\n";

  Out << "\n\n/* Global Declarations */\n";

  // First output all the declarations for the program, because C requires
  // Functions & globals to be declared before they are used.
  if (!M.getModuleInlineAsm().empty()) {
    Out << "\n/* Module asm statements */\n"
        << "__asm__ (";

    // Split the string into lines, to make it easier to read the .ll file.
    std::string Asm = M.getModuleInlineAsm();
    size_t CurPos = 0;
    size_t NewLine = Asm.find_first_of('\n', CurPos);
    while (NewLine != std::string::npos) {
      // We found a newline, print the portion of the asm string from the
      // last newline up to this newline.
      Out << "\"";
      PrintEscapedString(
          std::string(Asm.begin() + CurPos, Asm.begin() + NewLine), Out);
      Out << "\\n\"\n";
      CurPos = NewLine + 1;
      NewLine = Asm.find_first_of('\n', CurPos);
    }
    Out << "\"";
    PrintEscapedString(std::string(Asm.begin() + CurPos, Asm.end()), Out);
    Out << "\");\n"
        << "/* End Module asm statements */\n";
  }

  // collect any remaining types
  raw_null_ostream NullOut;
  for (Module::global_iterator I = M.global_begin(), E = M.global_end(); I != E;
       ++I) {
    // Ignore special globals, such as debug info.
    if (getGlobalVariableClass(&*I))
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

      Type *ElTy = I->getType()->getElementType();
      unsigned Alignment = I->getAlignment();
      bool IsOveraligned =
          Alignment && Alignment > TD->getABITypeAlignment(ElTy);
      if (IsOveraligned) {
        headerUseMsAlign();
        Out << "__MSALIGN__(" << Alignment << ") ";
      }
      printTypeNameForAddressableValue(Out, ElTy, false);
      Out << ' ' << GetValueName(&*I);
      if (IsOveraligned)
        Out << " __attribute__((aligned(" << Alignment << ")))";

      if (I->hasExternalWeakLinkage()) {
        headerUseExternalWeak();
        Out << " __EXTERNAL_WEAK__";
      }
      Out << ";\n";
    }
  }

  // Function declarations
  Out << "\n/* Function Declarations */\n";

  // Store the intrinsics which will be declared/defined below.
  SmallVector<Function *, 16> intrinsicsToDefine;

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
      case Intrinsic::powi:
      case Intrinsic::rint:
      case Intrinsic::sqrt:
      case Intrinsic::trunc:
        intrinsicsToDefine.push_back(&*I);
        continue;
      }
    }

    // Skip a few functions that have already been defined in headers
    if ((headerIncSetjmp() &&
         (I->getName() == "setjmp" || I->getName() == "longjmp" ||
          I->getName() == "_setjmp" || I->getName() == "siglongjmp" ||
          I->getName() == "sigsetjmp")) ||
        (headerIncMath() &&
         (I->getName() == "pow" || I->getName() == "powf" ||
          I->getName() == "sqrt" || I->getName() == "sqrtf" ||
          I->getName() == "trunc" || I->getName() == "truncf" ||
          I->getName() == "rint" || I->getName() == "rintf" ||
          I->getName() == "floor" || I->getName() == "floorf" ||
          I->getName() == "ceil" || I->getName() == "ceilf")) ||
        I->getName() == "alloca" || I->getName() == "_alloca" ||
        I->getName() == "_chkstk" || I->getName() == "__chkstk" ||
        I->getName() == "___chkstk_ms")
      continue;

    if (I->hasDLLImportStorageClass())
      Out << "__declspec(dllimport) ";
    else if (I->hasDLLExportStorageClass())
      Out << "__declspec(dllexport) ";

    if (I->hasLocalLinkage())
      Out << "static ";
    if (I->hasExternalWeakLinkage())
      Out << "extern ";
    printFunctionProto(Out, &*I);
    printFunctionAttributes(Out, I->getAttributes());
    if (I->hasWeakLinkage() || I->hasLinkOnceLinkage()) {
      headerUseAttributeWeak();
      Out << " __ATTRIBUTE_WEAK__";
    }
    if (I->hasExternalWeakLinkage()) {
      headerUseExternalWeak();
      Out << " __EXTERNAL_WEAK__";
    }
    if (StaticCtors.count(&*I))
      Out << " __ATTRIBUTE_CTOR__";
    if (StaticDtors.count(&*I))
      Out << " __ATTRIBUTE_DTOR__";
    if (I->hasHiddenVisibility()) {
      headerUseHidden();
      Out << " __HIDDEN__";
    }

    if (I->hasName() && I->getName()[0] == 1)
      Out << " __asm__ (\"" << I->getName().substr(1) << "\")";

    Out << ";\n";
  }

  // Output the global variable definitions and contents...
  if (!M.global_empty()) {
    Out << "\n\n/* Global Variable Definitions and Initialization */\n";
    for (Module::global_iterator I = M.global_begin(), E = M.global_end();
         I != E; ++I) {
      declareOneGlobalVariable(&*I);
    }
  }

  // Alias declarations...
  if (!M.alias_empty()) {
    Out << "\n/* External Alias Declarations */\n";
    for (Module::alias_iterator I = M.alias_begin(), E = M.alias_end(); I != E;
         ++I) {
      cwriter_assert(!I->isDeclaration() &&
                     !isEmptyType(I->getType()->getPointerElementType()));
      if (I->hasLocalLinkage())
        continue; // Internal Global

      if (I->hasDLLImportStorageClass())
        Out << "__declspec(dllimport) ";
      else if (I->hasDLLExportStorageClass())
        Out << "__declspec(dllexport) ";

      // Thread Local Storage
      if (I->isThreadLocal())
        Out << "__thread ";

      Type *ElTy = I->getType()->getElementType();
      unsigned Alignment = I->getBaseObject()->getAlignment();
      bool IsOveraligned =
          Alignment && Alignment > TD->getABITypeAlignment(ElTy);
      if (IsOveraligned) {
        headerUseMsAlign();
        Out << "__MSALIGN__(" << Alignment << ") ";
      }
      // GetValueName would resolve the alias, which is not what we want,
      // so use getName directly instead (assuming that the Alias has a name...)
      printTypeName(Out, ElTy, false) << " *" << I->getName();
      if (IsOveraligned)
        Out << " __attribute__((aligned(" << Alignment << ")))";

      if (I->hasExternalWeakLinkage()) {
        headerUseExternalWeak();
        Out << " __EXTERNAL_WEAK__";
      }
      Out << " = ";
      writeOperand(I->getAliasee(), ContextStatic);
      Out << ";\n";
    }
  }

  Out << "\n\n/* LLVM Intrinsic Builtin Function Bodies */\n";

  // Loop over all select operations
  if (!SelectDeclTypes.empty())
    headerUseForceInline();
  for (std::set<Type *>::iterator it = SelectDeclTypes.begin(),
                                  end = SelectDeclTypes.end();
       it != end; ++it) {
    // static __forceinline Rty llvm_select_u8x4(<bool x 4> condition, <u8 x 4>
    // iftrue, <u8 x 4> ifnot) {
    //   Rty r = {
    //     condition[0] ? iftrue[0] : ifnot[0],
    //     condition[1] ? iftrue[1] : ifnot[1],
    //     condition[2] ? iftrue[2] : ifnot[2],
    //     condition[3] ? iftrue[3] : ifnot[3]
    //   };
    //   return r;
    // }
    Out << "static __forceinline ";
    printTypeNameUnaligned(Out, *it, false);
    Out << " llvm_select_";
    printTypeString(Out, *it, false);
    Out << "(";
    if (isa<VectorType>(*it))
#if LLVM_VERSION_MAJOR >= 12
      printTypeNameUnaligned(
          Out,
          VectorType::get(Type::getInt1Ty((*it)->getContext()),
                          cast<VectorType>(*it)->getElementCount()),
          false);
#else
      printTypeNameUnaligned(
          Out,
          VectorType::get(Type::getInt1Ty((*it)->getContext()),
                          cast<VectorType>(*it)->getNumElements()),
          false);
#endif
    else
      Out << "bool";
    Out << " condition, ";
    printTypeNameUnaligned(Out, *it, false);
    Out << " iftrue, ";
    printTypeNameUnaligned(Out, *it, false);
    Out << " ifnot) {\n  ";
    printTypeNameUnaligned(Out, *it, false);
    Out << " r;\n";
    if (isa<VectorType>(*it)) {
      unsigned n, l = NumberOfElements(cast<VectorType>(*it));
      for (n = 0; n < l; n++) {
        Out << "  r.vector[" << n << "] = condition.vector[" << n
            << "] ? iftrue.vector[" << n << "] : ifnot.vector[" << n << "];\n";
      }
    } else {
      Out << "  r = condition ? iftrue : ifnot;\n";
    }
    Out << "  return r;\n}\n";
  }

  // Loop over all compare operations
  if (!CmpDeclTypes.empty())
    headerUseForceInline();
  for (std::set<std::pair<CmpInst::Predicate, VectorType *>>::iterator
           it = CmpDeclTypes.begin(),
           end = CmpDeclTypes.end();
       it != end; ++it) {
    // static __forceinline <bool x 4> llvm_icmp_ge_u8x4(<u8 x 4> l, <u8 x 4> r)
    // {
    //   Rty c = {
    //     l[0] >= r[0],
    //     l[1] >= r[1],
    //     l[2] >= r[2],
    //     l[3] >= r[3],
    //   };
    //   return c;
    // }
    unsigned n, l = NumberOfElements((*it).second);
    VectorType *RTy =
#if LLVM_VERSION_MAJOR >= 12
        VectorType::get(Type::getInt1Ty((*it).second->getContext()), l,(*it).second->getElementCount().isScalar());
#else
        VectorType::get(Type::getInt1Ty((*it).second->getContext()), l);
#endif
    bool isSigned = CmpInst::isSigned((*it).first);
    Out << "static __forceinline ";
    printTypeName(Out, RTy, isSigned);
    const auto Pred = (*it).first;
    if (CmpInst::isFPPredicate(Pred)) {
      headerUseFCmpOp(Pred);
      Out << " llvm_fcmp_";
    } else
      Out << " llvm_icmp_";
    Out << getCmpPredicateName(Pred) << "_";
    printTypeString(Out, (*it).second, isSigned);
    Out << "(";
    printTypeNameUnaligned(Out, (*it).second, isSigned);
    Out << " l, ";
    printTypeNameUnaligned(Out, (*it).second, isSigned);
    Out << " r) {\n  ";
    printTypeName(Out, RTy, isSigned);
    Out << " c;\n";
    for (n = 0; n < l; n++) {
      Out << "  c.vector[" << n << "] = ";
      if (CmpInst::isFPPredicate((*it).first)) {
        Out << "llvm_fcmp_" << getCmpPredicateName((*it).first) << "(l.vector["
            << n << "], r.vector[" << n << "]);\n";
      } else {
        Out << "l.vector[" << n << "]";
        switch ((*it).first) {
        case CmpInst::ICMP_EQ:
          Out << " == ";
          break;
        case CmpInst::ICMP_NE:
          Out << " != ";
          break;
        case CmpInst::ICMP_ULE:
        case CmpInst::ICMP_SLE:
          Out << " <= ";
          break;
        case CmpInst::ICMP_UGE:
        case CmpInst::ICMP_SGE:
          Out << " >= ";
          break;
        case CmpInst::ICMP_ULT:
        case CmpInst::ICMP_SLT:
          Out << " < ";
          break;
        case CmpInst::ICMP_UGT:
        case CmpInst::ICMP_SGT:
          Out << " > ";
          break;
        default:
          DBG_ERRS("Invalid icmp predicate!" << (*it).first);
          errorWithMessage("invalid icmp predicate");
        }
        Out << "r.vector[" << n << "];\n";
      }
    }
    Out << "  return c;\n}\n";
  }

  // Loop over all (vector) cast operations
  if (!CastOpDeclTypes.empty())
    headerUseForceInline();
  for (std::set<
           std::pair<CastInst::CastOps, std::pair<Type *, Type *>>>::iterator
           it = CastOpDeclTypes.begin(),
           end = CastOpDeclTypes.end();
       it != end; ++it) {
    // static __forceinline <u32 x 4> llvm_ZExt_u8x4_u32x4(<u8 x 4> in) { //
    // Src->isVector == Dst->isVector
    //   Rty out = {
    //     in[0],
    //     in[1],
    //     in[2],
    //     in[3]
    //   };
    //   return out;
    // }
    // static __forceinline u32 llvm_BitCast_u8x4_u32(<u8 x 4> in) { //
    // Src->bitsSize == Dst->bitsSize
    //   union {
    //     <u8 x 4> in;
    //     u32 out;
    //   } cast;
    //   cast.in = in;
    //   return cast.out;
    // }
    CastInst::CastOps opcode = (*it).first;
    Type *SrcTy = (*it).second.first;
    Type *DstTy = (*it).second.second;
    bool SrcSigned, DstSigned;
    switch (opcode) {
    default:
      SrcSigned = false;
      DstSigned = false;
      break;
    case Instruction::SIToFP:
      SrcSigned = true;
      DstSigned = false;
      break;
    case Instruction::FPToSI:
      SrcSigned = false;
      DstSigned = true;
      break;
    case Instruction::SExt:
      SrcSigned = true;
      DstSigned = true;
      break;
    }

    Out << "static __forceinline ";
    printTypeName(Out, DstTy, DstSigned);
    Out << " llvm_" << Instruction::getOpcodeName(opcode) << "_";
    printTypeString(Out, SrcTy, false);
    Out << "_";
    printTypeString(Out, DstTy, false);
    Out << "(";
    printTypeNameUnaligned(Out, SrcTy, SrcSigned);
    Out << " in) {\n";
    if (opcode == Instruction::BitCast) {
      Out << "  union {\n    ";
      printTypeName(Out, SrcTy, SrcSigned);
      Out << " in;\n    ";
      printTypeName(Out, DstTy, DstSigned);
      Out << " out;\n  } cast;\n";
      Out << "  cast.in = in;\n  return cast.out;\n}\n";
    } else if (isa<VectorType>(DstTy)) {
      Out << "  ";
      printTypeName(Out, DstTy, DstSigned);
      Out << " out;\n";
      unsigned n, l = NumberOfElements(cast<VectorType>(DstTy));
#if LLVM_VERSION_MAJOR > 10
      cwriter_assert(cast<FixedVectorType>(SrcTy)->getNumElements() == l);
#else
      cwriter_assert(cast<VectorType>(SrcTy)->getNumElements() == l);
#endif
      for (n = 0; n < l; n++) {
        Out << "  out.vector[" << n << "] = in.vector[" << n << "];\n";
      }
      Out << "  return out;\n}\n";
    } else {
      Out << "#ifndef __emulate_i128\n";
      // easy case first: compiler supports i128 natively
      Out << "  return in;\n";
      Out << "#else\n";
      Out << "  ";
      printTypeName(Out, DstTy, DstSigned);
      Out << " out;\n";
      Out << "  LLVM";
      switch (opcode) {
      case Instruction::UIToFP:
        Out << "UItoFP";
        break;
      case Instruction::SIToFP:
        Out << "SItoFP";
        break;
      case Instruction::Trunc:
        Out << "Trunc";
        break;
      case Instruction::FPExt:
        Out << "FPExt";
        break;
      case Instruction::FPTrunc:
        Out << "FPTrunc";
        break;
      case Instruction::ZExt:
        Out << "ZExt";
        break;
      case Instruction::FPToUI:
        Out << "FPtoUI";
        break;
      case Instruction::SExt:
        Out << "SExt";
        break;
      case Instruction::FPToSI:
        Out << "FPtoSI";
        break;
      default:
        DBG_ERRS("Invalid cast opcode: " << opcode);
        errorWithMessage("Invalid cast opcode for i128");
      }
      Out << "(" << SrcTy->getPrimitiveSizeInBits() << ", &in, "
          << DstTy->getPrimitiveSizeInBits() << ", &out);\n";
      Out << "  return out;\n";
      Out << "#endif\n";
      Out << "}\n";
    }
  }

  // Loop over all simple vector operations
  if (!InlineOpDeclTypes.empty())
    headerUseForceInline();
  for (std::set<std::pair<unsigned, Type *>>::iterator
           it = InlineOpDeclTypes.begin(),
           end = InlineOpDeclTypes.end();
       it != end; ++it) {
    // static __forceinline <u32 x 4> llvm_BinOp_u32x4(<u32 x 4> a, <u32 x 4> b)
    // {
    //   Rty r = {
    //      a[0] OP b[0],
    //      a[1] OP b[1],
    //      a[2] OP b[2],
    //      a[3] OP b[3],
    //   };
    //   return r;
    // }
    unsigned opcode = (*it).first;
    Type *OpTy = (*it).second;
    Type *ElemTy =
        isa<VectorType>(OpTy) ? cast<VectorType>(OpTy)->getElementType() : OpTy;
    bool shouldCast;
    bool isSigned;
    opcodeNeedsCast(opcode, shouldCast, isSigned);

    Out << "static __forceinline ";
    printTypeName(Out, OpTy);
    if (opcode == BinaryNeg) {
      Out << " llvm_neg_";
      printTypeString(Out, OpTy, false);
      Out << "(";
      printTypeNameUnaligned(Out, OpTy, isSigned);
      Out << " a) {\n  ";
    } else if (opcode == BinaryNot) {
      Out << " llvm_not_";
      printTypeString(Out, OpTy, false);
      Out << "(";
      printTypeNameUnaligned(Out, OpTy, isSigned);
      Out << " a) {\n  ";
    } else {
      Out << " llvm_" << Instruction::getOpcodeName(opcode) << "_";
      printTypeString(Out, OpTy, false);
      Out << "(";
      printTypeNameUnaligned(Out, OpTy, isSigned);
      Out << " a, ";
      printTypeNameUnaligned(Out, OpTy, isSigned);
      Out << " b) {\n  ";
    }

    printTypeName(Out, OpTy);
    // C can't handle non-power-of-two integer types
    unsigned mask = 0;
    if (ElemTy->isIntegerTy()) {
      IntegerType *ITy = static_cast<IntegerType *>(ElemTy);
#if LLVM_VERSION_MAJOR <= 10
      if (!ITy->isPowerOf2ByteWidth())
#else
      if (!IsPowerOfTwo(ITy->getBitWidth()))
#endif
        mask = ITy->getBitMask();
    }

    if (isa<VectorType>(OpTy)) {
      Out << " r;\n";
      unsigned n, l = NumberOfElements(cast<VectorType>(OpTy));
      for (n = 0; n < l; n++) {
        Out << "  r.vector[" << n << "] = ";
        if (mask)
          Out << "(";
        if (opcode == BinaryNeg) {
          Out << "-a.vector[" << n << "]";
        } else if (opcode == BinaryNot) {
          Out << "~a.vector[" << n << "]";
        } else if (opcode == Instruction::FRem) {
          // Output a call to fmod/fmodf instead of emitting a%b
          if (ElemTy->isFloatTy())
            Out << "fmodf(a.vector[" << n << "], b.vector[" << n << "])";
          else if (ElemTy->isDoubleTy())
            Out << "fmod(a.vector[" << n << "], b.vector[" << n << "])";
          else // all 3 flavors of long double
            Out << "fmodl(a.vector[" << n << "], b.vector[" << n << "])";
        } else {
          Out << "a.vector[" << n << "]";
          switch (opcode) {
          case Instruction::Add:
          case Instruction::FAdd:
            Out << " + ";
            break;
          case Instruction::Sub:
          case Instruction::FSub:
            Out << " - ";
            break;
          case Instruction::Mul:
          case Instruction::FMul:
            Out << " * ";
            break;
          case Instruction::URem:
          case Instruction::SRem:
          case Instruction::FRem:
            Out << " % ";
            break;
          case Instruction::UDiv:
          case Instruction::SDiv:
          case Instruction::FDiv:
            Out << " / ";
            break;
          case Instruction::And:
            Out << " & ";
            break;
          case Instruction::Or:
            Out << " | ";
            break;
          case Instruction::Xor:
            Out << " ^ ";
            break;
          case Instruction::Shl:
            Out << " << ";
            break;
          case Instruction::LShr:
          case Instruction::AShr:
            Out << " >> ";
            break;
          default:
            DBG_ERRS("Invalid operator type ! " << opcode);
            errorWithMessage("invalid operator type");
          }
          Out << "b.vector[" << n << "]";
        }
        if (mask)
          Out << ") & " << mask;
        Out << ";\n";
      }

    } else if (OpTy->getPrimitiveSizeInBits() > 64) {
      Out << " r;\n";
      Out << "#ifndef __emulate_i128\n";
      // easy case first: compiler supports i128 natively
      Out << "  r = ";
      if (opcode == BinaryNeg) {
        Out << "-a;\n";
      } else if (opcode == BinaryNot) {
        Out << "~a;\n";
      } else {
        Out << "a";
        switch (opcode) {
        case Instruction::Add:
        case Instruction::FAdd:
          Out << " + ";
          break;
        case Instruction::Sub:
        case Instruction::FSub:
          Out << " - ";
          break;
        case Instruction::Mul:
        case Instruction::FMul:
          Out << " * ";
          break;
        case Instruction::URem:
        case Instruction::SRem:
          Out << " % ";
          break;
        case Instruction::UDiv:
        case Instruction::SDiv:
        case Instruction::FDiv:
          Out << " / ";
          break;
        case Instruction::And:
          Out << " & ";
          break;
        case Instruction::Or:
          Out << " | ";
          break;
        case Instruction::Xor:
          Out << " ^ ";
          break;
        case Instruction::Shl:
          Out << " << ";
          break;
        case Instruction::LShr:
        case Instruction::AShr:
          Out << " >> ";
          break;
        default:
          DBG_ERRS("Invalid operator type !" << opcode);
          errorWithMessage("invalid operator type");
        }
        Out << "b;\n";
      }
      Out << "#else\n";
      // emulated twos-complement i128 math
      if (opcode == BinaryNeg) {
        Out << "  r.hi = ~a.hi;\n";
        Out << "  r.lo = ~a.lo + 1;\n";
        Out << "  if (r.lo == 0) r.hi += 1;\n"; // overflow: carry the one
      } else if (opcode == BinaryNot) {
        Out << "  r.hi = ~a.hi;\n";
        Out << "  r.lo = ~a.lo;\n";
      } else if (opcode == Instruction::And) {
        Out << "  r.hi = a.hi & b.hi;\n";
        Out << "  r.lo = a.lo & b.lo;\n";
      } else if (opcode == Instruction::Or) {
        Out << "  r.hi = a.hi | b.hi;\n";
        Out << "  r.lo = a.lo | b.lo;\n";
      } else if (opcode == Instruction::Xor) {
        Out << "  r.hi = a.hi ^ b.hi;\n";
        Out << "  r.lo = a.lo ^ b.lo;\n";
      } else if (opcode ==
                 Instruction::Shl) { // reminder: undef behavior if b >= 128
        Out << "  if (b.lo >= 64) {\n";
        Out << "    r.hi = (a.lo << (b.lo - 64));\n";
        Out << "    r.lo = 0;\n";
        Out << "  } else if (b.lo == 0) {\n";
        Out << "    r.hi = a.hi;\n";
        Out << "    r.lo = a.lo;\n";
        Out << "  } else {\n";
        Out << "    r.hi = (a.hi << b.lo) | (a.lo >> (64 - b.lo));\n";
        Out << "    r.lo = a.lo << b.lo;\n";
        Out << "  }\n";
      } else {
        // everything that hasn't been manually implemented above
        Out << "  LLVM";
        switch (opcode) {
        // case BinaryNeg: Out << "Neg"; break;
        // case BinaryNot: Out << "FlipAllBits"; break;
        case Instruction::Add:
          Out << "Add";
          break;
        case Instruction::FAdd:
          Out << "FAdd";
          break;
        case Instruction::Sub:
          Out << "Sub";
          break;
        case Instruction::FSub:
          Out << "FSub";
          break;
        case Instruction::Mul:
          Out << "Mul";
          break;
        case Instruction::FMul:
          Out << "FMul";
          break;
        case Instruction::URem:
          Out << "URem";
          break;
        case Instruction::SRem:
          Out << "SRem";
          break;
        case Instruction::UDiv:
          Out << "UDiv";
          break;
        case Instruction::SDiv:
          Out << "SDiv";
          break;
        case Instruction::FDiv:
          Out << "FDiv";
          break;
        // case Instruction::And:  Out << "And"; break;
        // case Instruction::Or:   Out << "Or"; break;
        // case Instruction::Xor:  Out << "Xor"; break;
        // case Instruction::Shl: Out << "Shl"; break;
        case Instruction::LShr:
          Out << "LShr";
          break;
        case Instruction::AShr:
          Out << "AShr";
          break;
        default:
          DBG_ERRS("Invalid operator type !" << opcode);
          errorWithMessage("invalid operator type");
        }
        Out << "(16, &a, &b, &r);\n";
      }
      Out << "#endif\n";

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
        else // all 3 flavors of long double
          Out << "fmodl(a, b)";
      } else {
        Out << "a";
        switch (opcode) {
        case Instruction::Add:
        case Instruction::FAdd:
          Out << " + ";
          break;
        case Instruction::Sub:
        case Instruction::FSub:
          Out << " - ";
          break;
        case Instruction::Mul:
        case Instruction::FMul:
          Out << " * ";
          break;
        case Instruction::URem:
        case Instruction::SRem:
        case Instruction::FRem:
          Out << " % ";
          break;
        case Instruction::UDiv:
        case Instruction::SDiv:
        case Instruction::FDiv:
          Out << " / ";
          break;
        case Instruction::And:
          Out << " & ";
          break;
        case Instruction::Or:
          Out << " | ";
          break;
        case Instruction::Xor:
          Out << " ^ ";
          break;
        case Instruction::Shl:
          Out << " << ";
          break;
        case Instruction::LShr:
        case Instruction::AShr:
          Out << " >> ";
          break;
        default:
          DBG_ERRS("Invalid operator type !" << opcode);
          errorWithMessage("invalid operator type");
        }
        Out << "b";
        if (mask)
          Out << ") & " << mask;
      }
      Out << ";\n";
    }
    Out << "  return r;\n}\n";
  }

  if (!CtorDeclTypes.empty())
    headerUseForceInline();
  // Loop over all inline constructors
  for (std::set<Type *>::iterator it = CtorDeclTypes.begin(),
                                  end = CtorDeclTypes.end();
       it != end; ++it) {
    // static __forceinline <u32 x 4> llvm_ctor_u32x4(u32 x1, u32 x2, u32 x3,
    // u32 x4) {
    //   Rty r = {
    //     x1, x2, x3, x4
    //   };
    //   return r;
    // }
    Out << "static __forceinline ";
    printTypeName(Out, *it);
    Out << " llvm_ctor_";
    printTypeString(Out, *it, false);
    Out << "(";
    StructType *STy = dyn_cast<StructType>(*it);
    ArrayType *ATy = dyn_cast<ArrayType>(*it);
    VectorType *VTy = dyn_cast<VectorType>(*it);
    unsigned e = (STy ? STy->getNumElements()
                      : (ATy ? ATy->getNumElements() : NumberOfElements(VTy)));
    bool printed = false;
    for (unsigned i = 0; i != e; ++i) {
      Type *ElTy = STy ? STy->getElementType(i) : nullptr;
      if (isEmptyType(ElTy))
        Out << " /* ";
      else if (printed)
        Out << ", ";
      printTypeNameUnaligned(Out, ElTy);
      Out << " x" << i;
      if (isEmptyType(ElTy))
        Out << " */";
      else
        printed = true;
    }
    Out << ") {\n  ";
    printTypeName(Out, *it);
    Out << " r;";
    for (unsigned i = 0; i != e; ++i) {
      Type *ElTy = STy ? STy->getElementType(i) : nullptr;
      if (isEmptyType(ElTy))
        continue;
      if (STy)
        Out << "\n  r.field" << i << " = x" << i << ";";
      else if (ATy)
        Out << "\n  r.array[" << i << "] = x" << i << ";";
      else if (VTy)
        Out << "\n  r.vector[" << i << "] = x" << i << ";";
      else
        cwriter_assert(0);
    }
    Out << "\n  return r;\n}\n";
  }

  // Emit definitions of the intrinsics.
  if (!intrinsicsToDefine.empty())
    headerUseForceInline();
  for (SmallVector<Function *, 16>::iterator I = intrinsicsToDefine.begin(),
                                             E = intrinsicsToDefine.end();
       I != E; ++I) {
    printIntrinsicDefinition(**I, Out);
  }

  if (!M.empty())
    Out << "\n\n/* Function Bodies */\n";

  if (!FCmpOps.empty())
    headerUseForceInline();

  generateCompilerSpecificCode(OutHeaders, TD);

  // Loop over all fcmp compare operations. We do that after
  // generateCompilerSpecificCode because we need __forceinline!
  if (FCmpOps.erase(FCmpInst::FCMP_ORD)) {
    defineFCmpOp(OutHeaders, FCmpInst::FCMP_ORD);
  }
  if (FCmpOps.erase(FCmpInst::FCMP_UNO)) {
    defineFCmpOp(OutHeaders, FCmpInst::FCMP_UNO);
  }
  for (auto Pred : FCmpOps) {
    defineFCmpOp(OutHeaders, Pred);
  }
  FCmpOps.clear();
}

void CWriter::declareOneGlobalVariable(GlobalVariable *I) {
  if (I->isDeclaration())
    return;

  // Ignore special globals, such as debug info.
  if (getGlobalVariableClass(&*I))
    return;

  if (I->hasDLLImportStorageClass())
    Out << "__declspec(dllimport) ";
  else if (I->hasDLLExportStorageClass())
    Out << "__declspec(dllexport) ";

  if (I->hasLocalLinkage())
    Out << "static ";

  // Thread Local Storage
  if (I->isThreadLocal())
    Out << "__thread ";

  Type *ElTy = I->getType()->getElementType();
  unsigned Alignment = I->getAlignment();
  bool IsOveraligned = Alignment && Alignment > TD->getABITypeAlignment(ElTy);
  if (IsOveraligned) {
    headerUseMsAlign();
    Out << "__MSALIGN__(" << Alignment << ") ";
  }
  printTypeNameForAddressableValue(Out, ElTy, false);
  Out << ' ' << GetValueName(I);
  if (IsOveraligned)
    Out << " __attribute__((aligned(" << Alignment << ")))";

  if (I->hasLinkOnceLinkage())
    Out << " __attribute__((common))";
  else if (I->hasWeakLinkage()) {
    headerUseAttributeWeak();
    Out << " __ATTRIBUTE_WEAK__";
  } else if (I->hasCommonLinkage()) {
    headerUseAttributeWeak();
    Out << " __ATTRIBUTE_WEAK__";
  }

  if (I->hasHiddenVisibility()) {
    headerUseHidden();
    Out << " __HIDDEN__";
  }

  // If the initializer is not null, emit the initializer.  If it is null,
  // we try to avoid emitting large amounts of zeros.  The problem with
  // this, however, occurs when the variable has weak linkage.  In this
  // case, the assembler will complain about the variable being both weak
  // and common, so we disable this optimization.
  // FIXME common linkage should avoid this problem.
  if (!I->getInitializer()->isNullValue()) {
    Out << " = ";
    writeOperand(I->getInitializer(), ContextStatic);
  } else if (I->hasWeakLinkage()) {
    // We have to specify an initializer, but it doesn't have to be
    // complete.  If the value is an aggregate, print out { 0 }, and let
    // the compiler figure out the rest of the zeros.
    Out << " = ";
    if (I->getInitializer()->getType()->isStructTy() ||
        I->getInitializer()->getType()->isVectorTy()) {
      Out << "{ 0 }";
    } else if (I->getInitializer()->getType()->isArrayTy()) {
      // As with structs and vectors, but with an extra set of braces
      // because arrays are wrapped in structs.
      Out << "{ { 0 } }";
    } else {
      // Just print it out normally.
      writeOperand(I->getInitializer(), ContextStatic);
    }
  }
  Out << ";\n";
}

/// Output all floating point constants that cannot be printed accurately...
void CWriter::printFloatingPointConstants(Function &F) {
  // Scan the module for floating point constants.  If any FP constant is used
  // in the function, we want to redirect it here so that we do not depend on
  // the precision of the printed form, unless the printed form preserves
  // precision.
  for (inst_iterator I = inst_begin(&F), E = inst_end(&F); I != E; ++I)
    for (Instruction::op_iterator I_Op = I->op_begin(), E_Op = I->op_end();
         I_Op != E_Op; ++I_Op)
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
  if (FPC == nullptr ||
      // Do not put in FPConstantMap if safe.
      isFPCSafeToPrint(FPC) ||
      // Already printed this constant?
      FPConstantMap.has(FPC))
    return;

  unsigned Counter = FPConstantMap.getOrInsert(FPC);

  if (FPC->getType() == Type::getDoubleTy(FPC->getContext())) {
    double Val = FPC->getValueAPF().convertToDouble();
    uint64_t i = FPC->getValueAPF().bitcastToAPInt().getZExtValue();
    headerUseConstantDoubleTy();
    Out << "static const ConstantDoubleTy FPConstant" << Counter << " = 0x"
        << utohexstr(i) << "ULL;    /* " << Val << " */\n";
  } else if (FPC->getType() == Type::getFloatTy(FPC->getContext())) {
    float Val = FPC->getValueAPF().convertToFloat();
    uint32_t i = (uint32_t)FPC->getValueAPF().bitcastToAPInt().getZExtValue();
    headerUseConstantFloatTy();
    Out << "static const ConstantFloatTy FPConstant" << Counter << " = 0x"
        << utohexstr(i) << "U;    /* " << Val << " */\n";
  } else if (FPC->getType() == Type::getX86_FP80Ty(FPC->getContext())) {
    // api needed to prevent premature destruction
    const APInt api = FPC->getValueAPF().bitcastToAPInt();
    const uint64_t *p = api.getRawData();
    headerUseConstantFP80Ty();
    Out << "static const ConstantFP80Ty FPConstant" << Counter << " = { 0x"
        << utohexstr(p[0]) << "ULL, 0x" << utohexstr((uint16_t)p[1])
        << ",{0,0,0}"
        << "}; /* Long double constant */\n";
  } else if (FPC->getType() == Type::getPPC_FP128Ty(FPC->getContext()) ||
             FPC->getType() == Type::getFP128Ty(FPC->getContext())) {
    const APInt api = FPC->getValueAPF().bitcastToAPInt();
    const uint64_t *p = api.getRawData();
    headerUseConstantFP128Ty();
    Out << "static const ConstantFP128Ty FPConstant" << Counter << " = { 0x"
        << utohexstr(p[0]) << ", 0x" << utohexstr(p[1])
        << "}; /* Long double constant */\n";

  } else {
    errorWithMessage("Unknown float type!");
  }
}

static void defineBitCastUnion(raw_ostream &Out) {
  Out << "/* Helper union for bitcasts */\n";
  Out << "typedef union {\n";
  Out << "  uint32_t Int32;\n";
  Out << "  uint64_t Int64;\n";
  Out << "  float Float;\n";
  Out << "  double Double;\n";
  Out << "} llvmBitCastUnion;\n";
}

/// printSymbolTable - Run through symbol table looking for type names.  If a
/// type name is found, emit its declaration...
void CWriter::printModuleTypes(raw_ostream &Out) {
  if (headerIncBitCastUnion()) {
    defineBitCastUnion(Out);
  }

  // Keep track of which types have been printed so far.
  std::set<Type *> TypesPrinted;

  // Loop over all structures then push them into the stack so they are
  // printed in the correct order.
  Out << "\n/* Types Declarations */\n";

  // Collect types referenced by structs and global functions, and
  // forward-declare the structs.
  {
    std::set<Type *> TypesPrinted;
    for (auto it = TypedefDeclTypes.begin(), end = TypedefDeclTypes.end();
         it != end; ++it) {
      forwardDeclareStructs(Out, *it, TypesPrinted);
    }
    for (const Function &F : *TheModule)
      forwardDeclareStructs(Out, F.getFunctionType(), TypesPrinted);
  }

  Out << "\n/* Function definitions */\n";

  struct FunctionDefinition {
    FunctionType *FT;
    std::vector<FunctionType *> Dependencies;
    std::string NameToPrint;
  };

  std::vector<FunctionDefinition> FunctionTypeDefinitions;
  // Copy Function Types into indexable container
  for (auto &I : UnnamedFunctionIDs) {
    const auto &F = I.first;
    FunctionType *FT = F.first;
    std::vector<FunctionType *> FDeps;
    for (const auto P : F.first->params()) {
      // Handle arbitrarily deep pointer indirection
      Type *PP = P;
      while (PP->isPointerTy())
        PP = PP->getPointerElementType();
      if (auto *PPF = dyn_cast<FunctionType>(PP))
        FDeps.push_back(PPF);
    }
    std::string DeclString;
    raw_string_ostream TmpOut(DeclString);
    printFunctionDeclaration(TmpOut, F.first, F.second);
    TmpOut.flush();
    FunctionTypeDefinitions.emplace_back(
        FunctionDefinition{FT, FDeps, DeclString});
  }

  // Sort function types
  TopologicalSorter Sorter(FunctionTypeDefinitions.size());
  DenseMap<FunctionType *, int> TopologicalSortMap;
  // Add Vertices
  for (unsigned I = 0; I < FunctionTypeDefinitions.size(); I++) {
    TopologicalSortMap[FunctionTypeDefinitions[I].FT] = I;
  }
  // Add Edges
  for (unsigned I = 0; I < FunctionTypeDefinitions.size(); I++) {
    const auto &Dependencies = FunctionTypeDefinitions[I].Dependencies;
    for (unsigned J = 0; J < Dependencies.size(); J++) {
      Sorter.addEdge(I, TopologicalSortMap[Dependencies[J]]);
    }
  }
  Optional<std::vector<int>> TopologicalSortResult = Sorter.sort();
  if (!TopologicalSortResult.hasValue()) {
    errorWithMessage("Cyclic dependencies in function definitions");
  }
  for (const auto I : TopologicalSortResult.getValue()) {
    Out << FunctionTypeDefinitions[I].NameToPrint << "\n";
  }

  // We may have collected some intrinsic prototypes to emit.
  // Emit them now, before the function that uses them is emitted
  for (auto &F : prototypesToGen) {
    Out << '\n';
    printFunctionProto(Out, F);
    Out << ";\n";
  }

  Out << "\n/* Types Definitions */\n";

  for (auto it = TypedefDeclTypes.begin(), end = TypedefDeclTypes.end();
       it != end; ++it) {
    printContainedTypes(Out, *it, TypesPrinted);
  }
}

void CWriter::forwardDeclareStructs(raw_ostream &Out, Type *Ty,
                                    std::set<Type *> &TypesPrinted) {
  if (!TypesPrinted.insert(Ty).second)
    return;
  if (isEmptyType(Ty))
    return;

  for (auto I = Ty->subtype_begin(); I != Ty->subtype_end(); ++I) {
    forwardDeclareStructs(Out, *I, TypesPrinted);
  }

  if (StructType *ST = dyn_cast<StructType>(Ty)) {
    Out << getStructName(ST) << ";\n";
  // Since function declarations come before the definitions of array-wrapper
  // structs, it is sometimes necessary to forward-declare those.
  } else if (auto *AT = dyn_cast<ArrayType>(Ty)) {
    Out << getArrayName(AT) << ";\n";
  } else if (auto *FT = dyn_cast<FunctionType>(Ty)) {
    // Ensure function types which are only directly used by struct types will
    // get declared.
    (void)getFunctionName(FT);
  }
}

// Push the struct onto the stack and recursively push all structs
// this one depends on.
void CWriter::printContainedTypes(raw_ostream &Out, Type *Ty,
                                  std::set<Type *> &TypesPrinted) {
  // Check to see if we have already printed this struct.
  if (!TypesPrinted.insert(Ty).second)
    return;
  // Skip empty structs
  if (isEmptyType(Ty))
    return;

  // Print all contained types first.
  for (Type::subtype_iterator I = Ty->subtype_begin(), E = Ty->subtype_end();
       I != E; ++I)
    printContainedTypes(Out, *I, TypesPrinted);

  if (StructType *ST = dyn_cast<StructType>(Ty)) {
    // Print structure type out.
    printStructDeclaration(Out, ST);
  } else if (ArrayType *AT = dyn_cast<ArrayType>(Ty)) {
    // Print array type out.
    printArrayDeclaration(Out, AT);
  } else if (VectorType *VT = dyn_cast<VectorType>(Ty)) {
    // Print vector type out.
    printVectorDeclaration(Out, VT);
  }
}

static inline bool isFPIntBitCast(Instruction &I) {
  if (!isa<BitCastInst>(I))
    return false;
  Type *SrcTy = I.getOperand(0)->getType();
  Type *DstTy = I.getType();
  return (SrcTy->isFloatingPointTy() && DstTy->isIntegerTy()) ||
         (DstTy->isFloatingPointTy() && SrcTy->isIntegerTy());
}

bool CWriter::canDeclareLocalLate(Instruction &I) {
  if (!DeclareLocalsLate) {
    return false;
  }

  // When a late declaration ends up inside a deeper scope than one of its uses,
  // the C compiler will reject it. That doesn't happen if we restrict to a
  // single block.
  if (I.isUsedOutsideOfBlock(I.getParent())) {
    return false;
  }

  return true;
}

void CWriter::printFunction(Function &F) {
  /// isStructReturn - Should this function actually return a struct by-value?
  bool isStructReturn = F.hasStructRetAttr();

  cwriter_assert(!F.isDeclaration());
  if (F.hasDLLImportStorageClass())
    Out << "__declspec(dllimport) ";
  if (F.hasDLLExportStorageClass())
    Out << "__declspec(dllexport) ";
  if (F.hasLocalLinkage())
    Out << "static ";

  std::string Name = GetValueName(&F);

  FunctionType *FTy = F.getFunctionType();

  bool shouldFixMain = false;
  if (Name == "main") {
    if (!isStandardMain(FTy)) {
      // Implementations are free to support non-standard signatures for main(),
      // so it would be unreasonable to make it an outright error.
      errs() << "CBackend warning: main() has an unrecognized signature. The "
                "types emitted will not be fixed to match the C standard.\n";
    } else {
      shouldFixMain = true;
    }
  }

  iterator_range<Function::arg_iterator> args = F.args();
  printFunctionProto(Out, FTy,
                     std::make_pair(F.getAttributes(), F.getCallingConv()),
                     Name, &args);

  Out << " {\n";

  if (shouldFixMain) {
    // Cast the arguments to main() to the expected LLVM IR types and names.
    unsigned Idx = 1;
    FunctionType::param_iterator I = FTy->param_begin(), E = FTy->param_end();
    Function::arg_iterator ArgName = args.begin();

    for (; I != E; ++I) {
      Type *ArgTy = *I;
      Out << "  ";
      printTypeName(Out, ArgTy);
      Out << ' ' << GetValueName(ArgName) << " = (";
      printTypeName(Out, ArgTy);
      Out << ")" << MainArgs.begin()[Idx].second << ";\n";

      ++Idx;
      ++ArgName;
    }
  }

  // If this is a struct return function, handle the result with magic.
  if (isStructReturn) {
    Type *StructTy =
        cast<PointerType>(F.arg_begin()->getType())->getElementType();
    Out << "  ";
    printTypeName(Out, StructTy, false)
        << " StructReturn;  /* Struct return temporary */\n";

    Out << "  ";
    printTypeName(Out, F.arg_begin()->getType(), false);
    Out << GetValueName(F.arg_begin()) << " = &StructReturn;\n";
  }

  bool PrintedVar = false;

  // print local variable information for the function
  for (inst_iterator I = inst_begin(&F), E = inst_end(&F); I != E; ++I) {
    if (AllocaInst *AI = isDirectAlloca(&*I)) {
      unsigned Alignment = AI->getAlignment();
      bool IsOveraligned = Alignment && Alignment > TD->getABITypeAlignment(
                                                        AI->getAllocatedType());
      Out << "  ";
      if (IsOveraligned) {
        headerUseMsAlign();
        Out << "__MSALIGN__(" << Alignment << ") ";
      }
      printTypeNameForAddressableValue(Out, AI->getAllocatedType(), false);
      Out << ' ' << GetValueName(AI);
      if (IsOveraligned)
        Out << " __attribute__((aligned(" << Alignment << ")))";
      Out << ";    /* Address-exposed local */\n";
      PrintedVar = true;
    } else if (!isEmptyType(I->getType()) && !isInlinableInst(*I)) {
      if (!canDeclareLocalLate(*I)) {
        Out << "  ";
        printTypeName(Out, I->getType(), false) << ' ' << GetValueName(&*I);
        Out << ";\n";
      }

      if (isa<PHINode>(*I)) { // Print out PHI node temporaries as well...
        Out << "  ";
        printTypeName(Out, I->getType(), false)
            << ' ' << (GetValueName(&*I) + "__PHI_TEMPORARY");
        Out << ";\n";
      }
      PrintedVar = true;
    }
    // We need a temporary for the BitCast to use so it can pluck a value out
    // of a union to do the BitCast. This is separate from the need for a
    // variable to hold the result of the BitCast.
    if (isFPIntBitCast(*I)) {
      headerUseBitCastUnion();
      Out << "  llvmBitCastUnion " << GetValueName(&*I)
          << "__BITCAST_TEMPORARY;\n";
      PrintedVar = true;
    }
  }

  if (PrintedVar)
    Out << '\n';

  // print the basic blocks
  for (Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB) {
    if (Loop *L = LI->getLoopFor(&*BB)) {
      if (L->getHeader() == &*BB && L->getParentLoop() == nullptr)
        printLoop(L);
    } else {
      printBasicBlock(&*BB);
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
  bool NeedsLabel = false;
  for (pred_iterator PI = pred_begin(BB), E = pred_end(BB); PI != E; ++PI)
    if (isGotoCodeNecessary(*PI, BB)) {
      NeedsLabel = true;
      break;
    }

  if (NeedsLabel) {
    Out << GetValueName(BB) << ":";
    // A label immediately before a late variable declaration is problematic,
    // because "a label can only be part of a statement and a declaration is not
    // a statement" (GCC). Adding a ";" is a simple workaround.
    if (DeclareLocalsLate) {
      Out << ";";
    }
    Out << "\n";
  }

  // Output all of the instructions in the basic block...
  for (BasicBlock::iterator II = BB->begin(), E = --BB->end(); II != E; ++II) {
    DILocation *Loc = (*II).getDebugLoc();
    if (Loc != nullptr && LastAnnotatedSourceLine != Loc->getLine()) {
      Out << "#line " << Loc->getLine() << " \"" << Loc->getDirectory() << "/"
          << Loc->getFilename() << "\""
          << "\n";
      LastAnnotatedSourceLine = Loc->getLine();
    }
    if (!isInlinableInst(*II) && !isDirectAlloca(&*II)) {
      Out << "  ";
      if (!isEmptyType(II->getType()) && !isInlineAsm(*II)) {
        if (canDeclareLocalLate(*II)) {
          printTypeName(Out, II->getType(), false) << ' ';
        }
        Out << GetValueName(&*II) << " = ";
      }
      writeInstComputationInline(*II);
      Out << ";\n";
    }
  }

  // Don't emit prefix or suffix for the terminator.
  visit(*BB->getTerminator());
}

// Specific Instruction type classes... note that all of the casts are
// necessary because we use the instruction classes as opaque types...
void CWriter::visitReturnInst(ReturnInst &I) {
  CurInstr = &I;

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
      &*I.getParent()->begin() != &I) {
    return;
  }

  Out << "  return";
  if (I.getNumOperands()) {
    Out << ' ';
    writeOperand(I.getOperand(0), ContextCasted);
  }
  Out << ";\n";
}

void CWriter::visitSwitchInst(SwitchInst &SI) {
  CurInstr = &SI;

  Value *Cond = SI.getCondition();
  unsigned NumBits = cast<IntegerType>(Cond->getType())->getBitWidth();

  if (SI.getNumCases() == 0) { // unconditional branch
    printPHICopiesForSuccessor(SI.getParent(), SI.getDefaultDest(), 2);
    printBranchToBlock(SI.getParent(), SI.getDefaultDest(), 2);
    Out << "\n";

  } else if (NumBits <= 64) { // model as a switch statement
    Out << "  switch (";
    writeOperand(Cond);
    Out << ") {\n  default:\n";
    printPHICopiesForSuccessor(SI.getParent(), SI.getDefaultDest(), 2);
    printBranchToBlock(SI.getParent(), SI.getDefaultDest(), 2);

    // Skip the first item since that's the default case.
    for (SwitchInst::CaseIt i = SI.case_begin(), e = SI.case_end(); i != e;
         ++i) {
      ConstantInt *CaseVal = i->getCaseValue();
      BasicBlock *Succ = i->getCaseSuccessor();
      Out << "  case ";
      writeOperand(CaseVal);
      Out << ":\n";
      printPHICopiesForSuccessor(SI.getParent(), Succ, 2);
      if (isGotoCodeNecessary(SI.getParent(), Succ))
        printBranchToBlock(SI.getParent(), Succ, 2);
      else
        Out << "    break;\n";
    }
    Out << "  }\n";

  } else { // model as a series of if statements
    Out << "  ";
    for (SwitchInst::CaseIt i = SI.case_begin(), e = SI.case_end(); i != e;
         ++i) {
      Out << "if (";
      ConstantInt *CaseVal = i->getCaseValue();
      BasicBlock *Succ = i->getCaseSuccessor();
      ICmpInst *icmp = new ICmpInst(CmpInst::ICMP_EQ, Cond, CaseVal);
      visitICmpInst(*icmp);
      delete icmp;
      Out << ") {\n";
      printPHICopiesForSuccessor(SI.getParent(), Succ, 2);
      printBranchToBlock(SI.getParent(), Succ, 2);
      Out << "  } else ";
    }
    Out << "{\n";
    printPHICopiesForSuccessor(SI.getParent(), SI.getDefaultDest(), 2);
    printBranchToBlock(SI.getParent(), SI.getDefaultDest(), 2);
    Out << "  }\n";
  }
  Out << "\n";
}

void CWriter::visitIndirectBrInst(IndirectBrInst &IBI) {
  CurInstr = &IBI;

  Out << "  goto *(void*)(";
  writeOperand(IBI.getOperand(0));
  Out << ");\n";
}

void CWriter::visitUnreachableInst(UnreachableInst &I) {
  CurInstr = &I;

  headerUseUnreachable();
  Out << "  __builtin_unreachable();\n\n";
}

bool CWriter::isGotoCodeNecessary(BasicBlock *From, BasicBlock *To) {
  /// FIXME: This should be reenabled, but loop reordering safe!!
  return true;

  if (std::next(Function::iterator(From)) != Function::iterator(To))
    return true; // Not the direct successor, we need a goto.

  // isa<SwitchInst>(From->getTerminator())

  if (LI->getLoopFor(From) != LI->getLoopFor(To))
    return true;
  return false;
}

void CWriter::printPHICopiesForSuccessor(BasicBlock *CurBlock,
                                         BasicBlock *Successor,
                                         unsigned Indent) {
  for (BasicBlock::iterator I = Successor->begin(); isa<PHINode>(I); ++I) {
    PHINode *PN = cast<PHINode>(I);
    // Now we have to do the printing.
    Value *IV = PN->getIncomingValueForBlock(CurBlock);
    if (!isa<UndefValue>(IV) && !isEmptyType(IV->getType())) {
      Out << std::string(Indent, ' ');
      Out << "  " << GetValueName(&*I) << "__PHI_TEMPORARY = ";
      writeOperand(IV, ContextCasted);
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
void CWriter::visitBranchInst(BranchInst &I) {
  CurInstr = &I;

  if (I.isConditional()) {
    if (isGotoCodeNecessary(I.getParent(), I.getSuccessor(0))) {
      Out << "  if (";
      writeOperand(I.getCondition(), ContextCasted);
      Out << ") {\n";

      printPHICopiesForSuccessor(I.getParent(), I.getSuccessor(0), 2);
      printBranchToBlock(I.getParent(), I.getSuccessor(0), 2);

      if (isGotoCodeNecessary(I.getParent(), I.getSuccessor(1))) {
        Out << "  } else {\n";
        printPHICopiesForSuccessor(I.getParent(), I.getSuccessor(1), 2);
        printBranchToBlock(I.getParent(), I.getSuccessor(1), 2);
      }
    } else {
      // First goto not necessary, assume second one is...
      Out << "  if (!";
      writeOperand(I.getCondition(), ContextCasted);
      Out << ") {\n";

      printPHICopiesForSuccessor(I.getParent(), I.getSuccessor(1), 2);
      printBranchToBlock(I.getParent(), I.getSuccessor(1), 2);
    }

    Out << "  }\n";
  } else {
    printPHICopiesForSuccessor(I.getParent(), I.getSuccessor(0), 0);
    printBranchToBlock(I.getParent(), I.getSuccessor(0), 0);
  }
  Out << "\n";
}

// PHI nodes get copied into temporary values at the end of predecessor basic
// blocks.  We now need to copy these temporary values into the REAL value for
// the PHI.
void CWriter::visitPHINode(PHINode &I) {
  CurInstr = &I;

  writeOperand(&I);
  Out << "__PHI_TEMPORARY";
}

void CWriter::visitUnaryOperator(UnaryOperator &I) {
  CurInstr = &I;

  // Currently the only unary operator supported is FNeg, which was introduced
  // in LLVM 8, although not fully exploited until later LLVM versions.
  // Older code uses a pseudo-FNeg pattern (-0.0 - x) which is matched in
  // visitBinaryOperator instead.
  if (I.getOpcode() != Instruction::FNeg) {
    DBG_ERRS("Invalid operator type !" << I);
    errorWithMessage("invalid operator type");
  }

  Value *X = I.getOperand(0);

  // We must cast the results of operations which might be promoted.
  bool needsCast = false;
  if ((I.getType() == Type::getInt8Ty(I.getContext())) ||
      (I.getType() == Type::getInt16Ty(I.getContext())) ||
      (I.getType() == Type::getFloatTy(I.getContext()))) {
    // types too small to work with directly
    needsCast = true;
  } else if (I.getType()->getPrimitiveSizeInBits() > 64) {
    // types too big to work with directly
    needsCast = true;
  }

  if (I.getType()->isVectorTy() || needsCast) {
    Type *VTy = I.getOperand(0)->getType();
    Out << "llvm_neg_";
    printTypeString(Out, VTy, false);
    Out << "(";
    writeOperand(X, ContextCasted);
    Out << ")";
    InlineOpDeclTypes.insert(std::pair<unsigned, Type *>(BinaryNeg, VTy));
  } else {
    Out << "-(";
    writeOperand(X);
    Out << ")";
  }
}

void CWriter::visitBinaryOperator(BinaryOperator &I) {
  using namespace PatternMatch;

  CurInstr = &I;

  // binary instructions, shift instructions, setCond instructions.
  cwriter_assert(!I.getType()->isPointerTy());

  // We must cast the results of binary operations which might be promoted.
  bool needsCast = false;
  if ((I.getType() == Type::getInt8Ty(I.getContext())) ||
      (I.getType() == Type::getInt16Ty(I.getContext())) ||
      (I.getType() == Type::getFloatTy(I.getContext()))) {
    // types too small to work with directly
    needsCast = true;
  } else if (I.getType()->getPrimitiveSizeInBits() > 64) {
    // types too big to work with directly
    needsCast = true;
  }
  bool shouldCast;
  bool castIsSigned;
  opcodeNeedsCast(I.getOpcode(), shouldCast, castIsSigned);

  if (I.getType()->isVectorTy() || needsCast || shouldCast) {
    Type *VTy = I.getOperand(0)->getType();
    unsigned opcode;
    Value *X;
    if (match(&I, m_Neg(m_Value(X)))) {
      opcode = BinaryNeg;
      Out << "llvm_neg_";
      printTypeString(Out, VTy, false);
      Out << "(";
      writeOperand(X, ContextCasted);
    } else if (match(&I, m_FNeg(m_Value(X)))) {
      opcode = BinaryNeg;
      Out << "llvm_neg_";
      printTypeString(Out, VTy, false);
      Out << "(";
      writeOperand(X, ContextCasted);
    } else if (match(&I, m_Not(m_Value(X)))) {
      opcode = BinaryNot;
      Out << "llvm_not_";
      printTypeString(Out, VTy, false);
      Out << "(";
      writeOperand(X, ContextCasted);
    } else {
      opcode = I.getOpcode();
      Out << "llvm_" << Instruction::getOpcodeName(opcode) << "_";
      printTypeString(Out, VTy, false);
      Out << "(";
      writeOperand(I.getOperand(0), ContextCasted);
      Out << ", ";
      writeOperand(I.getOperand(1), ContextCasted);
    }
    Out << ")";
    InlineOpDeclTypes.insert(std::pair<unsigned, Type *>(opcode, VTy));
    return;
  }

  // If this is a negation operation, print it out as such.  For FP, we don't
  // want to print "-0.0 - X".
  Value *X;
  if (match(&I, m_Neg(m_Value(X)))) {
    Out << "-(";
    writeOperand(X);
    Out << ")";
  } else if (match(&I, m_FNeg(m_Value(X)))) {
    Out << "-(";
    writeOperand(X);
    Out << ")";
  } else if (match(&I, m_Not(m_Value(X)))) {
    Out << "~(";
    writeOperand(X);
    Out << ")";
  } else if (I.getOpcode() == Instruction::FRem) {
    // Output a call to fmod/fmodf instead of emitting a%b
    if (I.getType() == Type::getFloatTy(I.getContext()))
      Out << "fmodf(";
    else if (I.getType() == Type::getDoubleTy(I.getContext()))
      Out << "fmod(";
    else // all 3 flavors of long double
      Out << "fmodl(";
    writeOperand(I.getOperand(0), ContextCasted);
    Out << ", ";
    writeOperand(I.getOperand(1), ContextCasted);
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
    case Instruction::FAdd:
      Out << " + ";
      break;
    case Instruction::Sub:
    case Instruction::FSub:
      Out << " - ";
      break;
    case Instruction::Mul:
    case Instruction::FMul:
      Out << " * ";
      break;
    case Instruction::URem:
    case Instruction::SRem:
    case Instruction::FRem:
      Out << " % ";
      break;
    case Instruction::UDiv:
    case Instruction::SDiv:
    case Instruction::FDiv:
      Out << " / ";
      break;
    case Instruction::And:
      Out << " & ";
      break;
    case Instruction::Or:
      Out << " | ";
      break;
    case Instruction::Xor:
      Out << " ^ ";
      break;
    case Instruction::Shl:
      Out << " << ";
      break;
    case Instruction::LShr:
    case Instruction::AShr:
      Out << " >> ";
      break;
    default:
      DBG_ERRS("Invalid operator type !" << I);
      errorWithMessage("invalid operator type");
    }

    writeOperandWithCast(I.getOperand(1), I.getOpcode());
    if (NeedsClosingParens)
      Out << "))";
  }
}

void CWriter::visitICmpInst(ICmpInst &I) {
  CurInstr = &I;

  if (I.getType()->isVectorTy() ||
      I.getOperand(0)->getType()->getPrimitiveSizeInBits() > 64) {
    Out << "llvm_icmp_" << getCmpPredicateName(I.getPredicate()) << "_";
    printTypeString(Out, I.getOperand(0)->getType(), I.isSigned());
    Out << "(";
    writeOperand(I.getOperand(0), ContextCasted);
    Out << ", ";
    writeOperand(I.getOperand(1), ContextCasted);
    Out << ")";
    if (VectorType *VTy = dyn_cast<VectorType>(I.getOperand(0)->getType())) {
      CmpDeclTypes.insert(
          std::pair<CmpInst::Predicate, VectorType *>(I.getPredicate(), VTy));
      TypedefDeclTypes.insert(
          I.getType()); // insert type not necessarily visible above
    }
    return;
  }

  // Write out the cast of the instruction's value back to the proper type
  // if necessary.
  bool NeedsClosingParens = writeInstructionCast(I);

  // Certain icmp predicate require the operand to be forced to a specific type
  // so we use writeOperandWithCast here instead of writeOperand. Similarly
  // below for operand 1
  writeOperandWithCast(I.getOperand(0), I);

  switch (I.getPredicate()) {
  case ICmpInst::ICMP_EQ:
    Out << " == ";
    break;
  case ICmpInst::ICMP_NE:
    Out << " != ";
    break;
  case ICmpInst::ICMP_ULE:
  case ICmpInst::ICMP_SLE:
    Out << " <= ";
    break;
  case ICmpInst::ICMP_UGE:
  case ICmpInst::ICMP_SGE:
    Out << " >= ";
    break;
  case ICmpInst::ICMP_ULT:
  case ICmpInst::ICMP_SLT:
    Out << " < ";
    break;
  case ICmpInst::ICMP_UGT:
  case ICmpInst::ICMP_SGT:
    Out << " > ";
    break;
  default:
    DBG_ERRS("Invalid icmp predicate!" << I);
    errorWithMessage("invalid icmp predicate");
  }

  writeOperandWithCast(I.getOperand(1), I);
  if (NeedsClosingParens)
    Out << "))";
}

void CWriter::visitFCmpInst(FCmpInst &I) {
  CurInstr = &I;

  if (I.getType()->isVectorTy()) {
    Out << "llvm_fcmp_" << getCmpPredicateName(I.getPredicate()) << "_";
    printTypeString(Out, I.getOperand(0)->getType(), I.isSigned());
    Out << "(";
    writeOperand(I.getOperand(0), ContextCasted);
    Out << ", ";
    writeOperand(I.getOperand(1), ContextCasted);
    Out << ")";
    if (VectorType *VTy = dyn_cast<VectorType>(I.getOperand(0)->getType())) {
      CmpDeclTypes.insert(
          std::pair<CmpInst::Predicate, VectorType *>(I.getPredicate(), VTy));
      TypedefDeclTypes.insert(
          I.getType()); // insert type not necessarily visible above
    }
    return;
  }

  const auto Pred = I.getPredicate();
  headerUseFCmpOp(Pred);
  Out << "llvm_fcmp_" << getCmpPredicateName(Pred) << "(";
  // Write the first operand
  writeOperand(I.getOperand(0), ContextCasted);
  Out << ", ";
  // Write the second operand
  writeOperand(I.getOperand(1), ContextCasted);
  Out << ")";
}

static const char *getFloatBitCastField(Type *Ty) {
  switch (Ty->getTypeID()) {
  default:
    llvm_unreachable("Invalid Type");
  case Type::FloatTyID:
    return "Float";
  case Type::DoubleTyID:
    return "Double";
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
  CurInstr = &I;

  Type *DstTy = I.getType();
  Type *SrcTy = I.getOperand(0)->getType();

  if (DstTy->isVectorTy() || SrcTy->isVectorTy() ||
      DstTy->getPrimitiveSizeInBits() > 64 ||
      SrcTy->getPrimitiveSizeInBits() > 64) {
    Out << "llvm_" << I.getOpcodeName() << "_";
    printTypeString(Out, SrcTy, false);
    Out << "_";
    printTypeString(Out, DstTy, false);
    Out << "(";
    writeOperand(I.getOperand(0), ContextCasted);
    Out << ")";
    CastOpDeclTypes.insert(
        std::pair<Instruction::CastOps, std::pair<Type *, Type *>>(
            I.getOpcode(), std::pair<Type *, Type *>(SrcTy, DstTy)));
    return;
  }

  if (isFPIntBitCast(I)) {
    Out << '(';
    // These int<->float and long<->double casts need to be handled specially
    Out << GetValueName(&I) << "__BITCAST_TEMPORARY."
        << getFloatBitCastField(I.getOperand(0)->getType()) << " = ";
    writeOperand(I.getOperand(0), ContextCasted);
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

  writeOperand(I.getOperand(0), ContextCasted);

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
  CurInstr = &I;

  Out << "llvm_select_";
  printTypeString(Out, I.getType(), false);
  Out << "(";
  writeOperand(I.getCondition(), ContextCasted);
  Out << ", ";
  writeOperand(I.getTrueValue(), ContextCasted);
  Out << ", ";
  writeOperand(I.getFalseValue(), ContextCasted);
  Out << ")";
  SelectDeclTypes.insert(I.getType());
  cwriter_assert(
      I.getCondition()->getType()->isVectorTy() ==
      I.getType()->isVectorTy()); // TODO: might be scalarty == vectorty
}

// Returns the macro name or value of the max or min of an integer type
// (as defined in limits.h).
static void printLimitValue(IntegerType &Ty, bool isSigned, bool isMax,
                            raw_ostream &Out) {
  const char *type;
  const char *sprefix = "";

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

void CWriter::printIntrinsicDefinition(FunctionType *funT, unsigned Opcode,
                                       std::string OpName, raw_ostream &Out) {
  Type *retT = funT->getReturnType();
  Type *elemT = funT->getParamType(0);
  IntegerType *elemIntT = dyn_cast<IntegerType>(elemT);
  char i, numParams = funT->getNumParams();
  bool isSigned;
  switch (Opcode) {
  default:
    isSigned = false;
    break;
  case Intrinsic::sadd_with_overflow:
  case Intrinsic::ssub_with_overflow:
  case Intrinsic::smul_with_overflow:
    isSigned = true;
    break;
  }
  cwriter_assert(numParams > 0 && numParams < 26);

  if (isa<VectorType>(retT)) {
    // this looks general, but is only actually used for ctpop, ctlz, cttz
    Type **devecFunParams = (Type **)alloca(sizeof(Type *) * numParams);
    for (i = 0; i < numParams; i++) {
      devecFunParams[(int)i] = funT->params()[(int)i]->getScalarType();
    }
    FunctionType *devecFunT = FunctionType::get(
        funT->getReturnType()->getScalarType(),
        makeArrayRef(devecFunParams, numParams), funT->isVarArg());
    printIntrinsicDefinition(devecFunT, Opcode, OpName + "_devec", Out);
  }

  // static __forceinline Rty _llvm_op_ixx(unsigned ixx a, unsigned ixx b) {
  //   Rty r;
  //   <opcode here>
  //   return r;
  // }
  Out << "static __forceinline ";
  printTypeName(Out, retT);
  Out << " ";
  Out << OpName;
  Out << "(";
  for (i = 0; i < numParams; i++) {
    switch (Opcode) {
    // optional intrinsic validity cwriter_assertion checks
    default:
      // default case: assume all parameters must have the same type
      cwriter_assert(elemT == funT->getParamType(i));
      break;
    case Intrinsic::ctlz:
    case Intrinsic::cttz:
    case Intrinsic::powi:
      break;
    }
    printTypeNameUnaligned(Out, funT->getParamType(i), isSigned);
    Out << " " << (char)('a' + i);
    if (i != numParams - 1)
      Out << ", ";
  }
  Out << ") {\n  ";
  printTypeName(Out, retT);
  Out << " r;\n";

  if (isa<VectorType>(retT)) {
    for (i = 0; i < numParams; i++) {
      Out << "  r.vector[" << (int)i << "] = " << OpName << "_devec(";
      for (char j = 0; j < numParams; j++) {
        Out << (char)('a' + j);
        if (isa<VectorType>(funT->params()[j]))
          Out << ".vector[" << (int)i << "]";
        if (j != numParams - 1)
          Out << ", ";
      }
      Out << ");\n";
    }
  } else if (elemIntT) {
    // handle integer ops
    cwriter_assert(isSupportedIntegerSize(*elemIntT) &&
                   "CBackend does not support arbitrary size integers.");
    switch (Opcode) {
    default:
      DBG_ERRS("Unsupported Intrinsic!" << Opcode);
      errorWithMessage("unsupported instrinsic");

    case Intrinsic::uadd_with_overflow:
      //   r.field0 = a + b;
      //   r.field1 = (r.field0 < a);
      cwriter_assert(cast<StructType>(retT)->getElementType(0) == elemT);
      Out << "  r.field0 = a + b;\n";
      Out << "  r.field1 = (a >= -b);\n";
      break;

    case Intrinsic::sadd_with_overflow:
      headerUseLimits(); // _MAX and _MIN definitions
      //   r.field0 = a + b;
      //   r.field1 = (b > 0 && a > XX_MAX - b) ||
      //              (b < 0 && a < XX_MIN - b);
      cwriter_assert(cast<StructType>(retT)->getElementType(0) == elemT);
      Out << "  r.field0 = a + b;\n";
      Out << "  r.field1 = (b >= 0 ? a > ";
      printLimitValue(*elemIntT, true, true, Out);
      Out << " - b : a < ";
      printLimitValue(*elemIntT, true, false, Out);
      Out << " - b);\n";
      break;

    case Intrinsic::usub_with_overflow:
      cwriter_assert(cast<StructType>(retT)->getElementType(0) == elemT);
      Out << "  r.field0 = a - b;\n";
      Out << "  r.field1 = (a < b);\n";
      break;

    case Intrinsic::ssub_with_overflow:
      headerUseLimits(); // _MAX and _MIN definitions
      cwriter_assert(cast<StructType>(retT)->getElementType(0) == elemT);
      Out << "  r.field0 = a - b;\n";
      Out << "  r.field1 = (b <= 0 ? a > ";
      printLimitValue(*elemIntT, true, true, Out);
      Out << " + b : a < ";
      printLimitValue(*elemIntT, true, false, Out);
      Out << " + b);\n";
      break;

    case Intrinsic::umul_with_overflow:
      cwriter_assert(cast<StructType>(retT)->getElementType(0) == elemT);
      Out << "  r.field1 = LLVMMul_uov(8 * sizeof(a), &a, &b, &r.field0);\n";
      break;

    case Intrinsic::smul_with_overflow:
      cwriter_assert(cast<StructType>(retT)->getElementType(0) == elemT);
      Out << "  r.field1 = LLVMMul_sov(8 * sizeof(a), &a, &b, &r.field0);\n";
      break;

    case Intrinsic::bswap:
      cwriter_assert(retT == elemT);
      Out << "  LLVMFlipAllBits(8 * sizeof(a), &a, &r);\n";
      break;

    case Intrinsic::ctpop:
      cwriter_assert(retT == elemT);
      Out << "  r = ";
      if (retT->getPrimitiveSizeInBits() > 64)
        Out << "llvm_ctor_u128(0, ";
      Out << "LLVMCountPopulation(8 * sizeof(a), &a)";
      if (retT->getPrimitiveSizeInBits() > 64)
        Out << ")";
      Out << ";\n";
      break;

    case Intrinsic::ctlz:
      cwriter_assert(retT == elemT);
      Out << "  (void)b;\n  r = ";
      if (retT->getPrimitiveSizeInBits() > 64)
        Out << "llvm_ctor_u128(0, ";
      Out << "LLVMCountLeadingZeros(8 * sizeof(a), &a)";
      if (retT->getPrimitiveSizeInBits() > 64)
        Out << ")";
      Out << ";\n";
      break;

    case Intrinsic::cttz:
      cwriter_assert(retT == elemT);
      Out << "  (void)b;\n  r = ";
      if (retT->getPrimitiveSizeInBits() > 64)
        Out << "llvm_ctor_u128(0, ";
      Out << "LLVMCountTrailingZeros(8 * sizeof(a), &a)";
      if (retT->getPrimitiveSizeInBits() > 64)
        Out << ")";
      Out << ";\n";
      break;
    }

  } else {
    // handle FP ops
    const char *suffix;
    cwriter_assert(retT == elemT);
    if (elemT->isFloatTy() || elemT->isHalfTy()) {
      suffix = "f";
    } else if (elemT->isDoubleTy()) {
      suffix = "";
    } else if (elemT->isFP128Ty()) {
    } else if (elemT->isX86_FP80Ty()) {
    } else if (elemT->isPPC_FP128Ty()) {
      suffix = "l";
    } else {
      DBG_ERRS("Unsupported Intrinsic!" << Opcode);
      errorWithMessage("unsupported instrinsic");
    }

    switch (Opcode) {
    default:
      DBG_ERRS("Unsupported Intrinsic!" << Opcode);
      errorWithMessage("unsupported instrinsic");

    case Intrinsic::ceil:
      headerUseMath();
      Out << "  r = ceil" << suffix << "(a);\n";
      break;

    case Intrinsic::fabs:
      headerUseMath();
      Out << "  r = fabs" << suffix << "(a);\n";
      break;

    case Intrinsic::floor:
      headerUseMath();
      Out << "  r = floor" << suffix << "(a);\n";
      break;

    case Intrinsic::fma:
      headerUseMath();
      Out << "  r = fma" << suffix << "(a, b, c);\n";
      break;

    case Intrinsic::fmuladd:
      Out << "  r = a * b + c;\n";
      break;

    case Intrinsic::pow:
    case Intrinsic::powi:
      headerUseMath();
      Out << "  r = pow" << suffix << "(a, b);\n";
      break;

    case Intrinsic::rint:
      headerUseMath();
      Out << "  r = rint" << suffix << "(a);\n";
      break;

    case Intrinsic::sqrt:
      headerUseMath();
      Out << "  r = sqrt" << suffix << "(a);\n";
      break;

    case Intrinsic::trunc:
      headerUseMath();
      Out << "  r = trunc" << suffix << "(a);\n";
      break;
    }
  }

  Out << "  return r;\n}\n";
}

void CWriter::printIntrinsicDefinition(Function &F, raw_ostream &Out) {
  FunctionType *funT = F.getFunctionType();
  unsigned Opcode = F.getIntrinsicID();
  std::string OpName = GetValueName(&F);
  printIntrinsicDefinition(funT, Opcode, OpName, Out);
}

bool CWriter::lowerIntrinsics(Function &F) {
  bool LoweredAny = false;

  // Examine all the instructions in this function to find the intrinsics that
  // need to be lowered.
  for (auto &BB : F) {
    for (BasicBlock::iterator I = BB.begin(), E = BB.end(); I != E;) {
      if (CallInst *CI = dyn_cast<CallInst>(I++)) {
        if (Function *F = CI->getCalledFunction()) {
          switch (F->getIntrinsicID()) {
          case Intrinsic::not_intrinsic:
          case Intrinsic::vastart:
          case Intrinsic::vacopy:
          case Intrinsic::vaend:
          case Intrinsic::returnaddress:
          case Intrinsic::frameaddress:
// LLVM 10 doesn't have setjmp/longjmp as intrinsics.
// TODO: figure this out.
#if LLVM_VERSION_MAJOR < 10
          case Intrinsic::setjmp:
          case Intrinsic::longjmp:
          case Intrinsic::sigsetjmp:
          case Intrinsic::siglongjmp:
#endif
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
          case Intrinsic::dbg_value:
          case Intrinsic::dbg_declare:
            // We directly implement these intrinsics
            break;

          default:
            // All other intrinsic calls we must lower.
            LoweredAny = true;

            Instruction *Before = (CI == &BB.front())
                                      ? nullptr
                                      : &*std::prev(BasicBlock::iterator(CI));

            IL->LowerIntrinsicCall(CI);
            if (Before) { // Move iterator to instruction after call
              I = BasicBlock::iterator(Before);
              ++I;
            } else {
              I = BB.begin();
            }

            // If the intrinsic got lowered to another call, and that call has
            // a definition, then we need to make sure its prototype is emitted
            // before any calls to it.
            if (CallInst *Call = dyn_cast<CallInst>(I))
              if (Function *NewF = Call->getCalledFunction())
                if (!NewF->isDeclaration())
                  prototypesToGen.push_back(NewF);

            break;
          }
        }
      }
    }
  }

  return LoweredAny;
}

void CWriter::visitCallInst(CallInst &I) {
  CurInstr = &I;

  if (isa<InlineAsm>(I.getCalledOperand()))
    return visitInlineAsm(I);

  // Handle intrinsic function calls first...
  if (Function *F = I.getCalledFunction()) {
    auto ID = F->getIntrinsicID();
    if (ID != Intrinsic::not_intrinsic && visitBuiltinCall(I, ID))
      return;
  }

  Value *Callee = I.getCalledOperand();

  PointerType *PTy = cast<PointerType>(Callee->getType());
  FunctionType *FTy = cast<FunctionType>(PTy->getElementType());

  // If this is a call to a struct-return function, assign to the first
  // parameter instead of passing it to the call.
  const AttributeList &PAL = I.getAttributes();
  bool hasByVal = I.hasByValArgument();
  bool isStructRet = I.hasStructRetAttr();
  if (isStructRet) {
    writeOperandDeref(I.getArgOperand(0));
    Out << " = ";
  }

  if (I.isTailCall())
    Out << " /*tail*/ ";

  // If this is an indirect call to a struct return function, we need to cast
  // the pointer. Ditto for indirect calls with byval arguments.
  bool NeedsCast =
      (hasByVal || isStructRet || I.getCallingConv() != CallingConv::C) &&
      !isa<Function>(Callee);

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
  if (ConstantExpr *CE = dyn_cast<ConstantExpr>(Callee))
    if (CE->isCast())
      if (Function *RF = dyn_cast<Function>(CE->getOperand(0))) {
        NeedsCast = true;
        Callee = RF;
      }

  if (NeedsCast) {
    // Ok, just cast the pointer type.
    Out << "((";
    printTypeName(Out, I.getCalledOperand()->getType()->getPointerElementType(),
                  false, std::make_pair(PAL, I.getCallingConv()));
    Out << "*)(void*)";
  }
  writeOperand(Callee, ContextCasted);
  if (NeedsCast)
    Out << ')';

  Out << '(';

  bool PrintedArg = false;
  if (FTy->isVarArg() && !FTy->getNumParams()) {
    Out << "0 /*dummy arg*/";
    PrintedArg = true;
  }

  unsigned NumDeclaredParams = FTy->getNumParams();
  auto CS(&I);
  auto AI = CS->arg_begin(), AE = CS->arg_end();
  unsigned ArgNo = 0;
  if (isStructRet) { // Skip struct return argument.
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
    if (PrintedArg)
      Out << ", ";
    if (ArgNo < NumDeclaredParams &&
        (*AI)->getType() != FTy->getParamType(ArgNo)) {
      Out << '(';
      printTypeNameUnaligned(
          Out, FTy->getParamType(ArgNo),
          /*isSigned=*/PAL.hasAttribute(ArgNo + 1, Attribute::SExt));
      Out << ')';
    }
    // Check if the argument is expected to be passed by value.
    if (I.getAttributes().hasAttribute(ArgNo + 1, Attribute::ByVal))
      writeOperandDeref(*AI);
    else
      writeOperand(*AI, ContextCasted);
    PrintedArg = true;
  }
  Out << ')';
}

/// visitBuiltinCall - Handle the call to the specified builtin.  Returns true
/// if the entire call is handled, return false if it wasn't handled
bool CWriter::visitBuiltinCall(CallInst &I, Intrinsic::ID ID) {
  CurInstr = &I;

  switch (ID) {
  default: {
    DBG_ERRS("Unknown LLVM intrinsic! " << I);
    errorWithMessage("unknown llvm instrinsic");
    return false;
  }
  case Intrinsic::dbg_value:
  case Intrinsic::dbg_declare:
    return true; // ignore these intrinsics
  case Intrinsic::vastart:
    headerUseStdarg();
    Out << "0; ";

    Out << "va_start(*(va_list*)";
    writeOperand(I.getArgOperand(0), ContextCasted);
    Out << ", ";
    // Output the last argument to the enclosing function.
    if (I.getParent()->getParent()->arg_empty())
      Out << "vararg_dummy_arg";
    else {
      Function::arg_iterator arg_end = I.getParent()->getParent()->arg_end();
      writeOperand(--arg_end);
    }
    Out << ')';
    return true;
  case Intrinsic::vaend:
    headerUseStdarg();
    if (!isa<ConstantPointerNull>(I.getArgOperand(0))) {
      Out << "0; va_end(*(va_list*)";
      writeOperand(I.getArgOperand(0), ContextCasted);
      Out << ')';
    } else {
      Out << "va_end(*(va_list*)0)";
    }
    return true;
  case Intrinsic::vacopy:
    headerUseStdarg();
    Out << "0; ";
    Out << "va_copy(*(va_list*)";
    writeOperand(I.getArgOperand(0), ContextCasted);
    Out << ", *(va_list*)";
    writeOperand(I.getArgOperand(1), ContextCasted);
    Out << ')';
    return true;
  case Intrinsic::returnaddress:
    Out << "__builtin_return_address(";
    writeOperand(I.getArgOperand(0), ContextCasted);
    Out << ')';
    return true;
  case Intrinsic::frameaddress:
    Out << "__builtin_frame_address(";
    writeOperand(I.getArgOperand(0), ContextCasted);
    Out << ')';
    return true;
// LLVM 10 doesn't have setjmp/longjmp as intrinsics.
// TODO: figure this out.
#if LLVM_VERSION_MAJOR < 10
  case Intrinsic::setjmp:
    headerUseSetjmp();
    Out << "setjmp(*(jmp_buf*)";
    writeOperand(I.getArgOperand(0), ContextCasted);
    Out << ')';
    return true;
  case Intrinsic::longjmp:
    headerUseSetjmp();
    Out << "longjmp(*(jmp_buf*)";
    writeOperand(I.getArgOperand(0), ContextCasted);
    Out << ", ";
    writeOperand(I.getArgOperand(1), ContextCasted);
    Out << ')';
    return true;
  case Intrinsic::sigsetjmp:
    headerUseSetjmp();
    Out << "sigsetjmp(*(sigjmp_buf*)";
    writeOperand(I.getArgOperand(0), ContextCasted);
    Out << ',';
    writeOperand(I.getArgOperand(1), ContextCasted);
    Out << ')';
    return true;
  case Intrinsic::siglongjmp:
    headerUseSetjmp();
    Out << "siglongjmp(*(sigjmp_buf*)";
    writeOperand(I.getArgOperand(0), ContextCasted);
    Out << ", ";
    writeOperand(I.getArgOperand(1), ContextCasted);
    Out << ')';
    return true;
#endif
  case Intrinsic::prefetch:
    Out << "LLVM_PREFETCH((const void *)";
    writeOperand(I.getArgOperand(0), ContextCasted);
    Out << ", ";
    writeOperand(I.getArgOperand(1), ContextCasted);
    Out << ", ";
    writeOperand(I.getArgOperand(2), ContextCasted);
    Out << ")";
    return true;
  case Intrinsic::stacksave:
    // Emit this as: Val = 0; *((void**)&Val) = __builtin_stack_save()
    // to work around GCC bugs (see PR1809).
    headerUseStackSaveRestore();
    Out << "0; *((void**)&" << GetValueName(&I) << ") = __builtin_stack_save()";
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
    default:
      errorWithMessage("Invalid llvm.x86.sse.cmp!");
    case 0:
      Out << "__builtin_ia32_cmpeq";
      break;
    case 1:
      Out << "__builtin_ia32_cmplt";
      break;
    case 2:
      Out << "__builtin_ia32_cmple";
      break;
    case 3:
      Out << "__builtin_ia32_cmpunord";
      break;
    case 4:
      Out << "__builtin_ia32_cmpneq";
      break;
    case 5:
      Out << "__builtin_ia32_cmpnlt";
      break;
    case 6:
      Out << "__builtin_ia32_cmpnle";
      break;
    case 7:
      Out << "__builtin_ia32_cmpord";
      break;
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
    writeOperand(I.getArgOperand(0), ContextCasted);
    Out << ", ";
    writeOperand(I.getArgOperand(1), ContextCasted);
    Out << ")";
    return true;
  case Intrinsic::ppc_altivec_lvsl:
    Out << '(';
    printTypeName(Out, I.getType());
    Out << ')';
    Out << "__builtin_altivec_lvsl(0, (void*)";
    writeOperand(I.getArgOperand(0), ContextCasted);
    Out << ")";
    return true;
  case Intrinsic::stackprotector:
    writeOperandDeref(I.getArgOperand(1));
    Out << " = ";
    writeOperand(I.getArgOperand(0), ContextCasted);
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

// This converts the llvm constraint string to something gcc is expecting.
// TODO: work out platform independent constraints and factor those out
//      of the per target tables
//      handle multiple constraint codes
std::string CWriter::InterpretASMConstraint(InlineAsm::ConstraintInfo &c) {
  return TargetLowering::AsmOperandInfo(c).ConstraintCode;
#if 0
  cwriter_assert(c.Codes.size() == 1 && "Too many asm constraint codes to handle");

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

// TODO: import logic from AsmPrinter.cpp
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
        std::string n = "%" + asmstr.substr(a + 1, b - a - 1) +
                        asmstr.substr(i + 2, a - i - 2);
        asmstr.replace(i, b - i + 1, n);
        i += n.size() - 1;
      } else
        asmstr.replace(i, 1, "%");
    } else if (asmstr[i] == '%') // grr
    {
      asmstr.replace(i, 1, "%%");
      ++i;
    }

  return asmstr;
}

// TODO: assumptions about what consume arguments from the call are likely wrong
//      handle communitivity
void CWriter::visitInlineAsm(CallInst &CI) {
  CurInstr = &CI;

  InlineAsm *as = cast<InlineAsm>(CI.getCalledOperand());
  InlineAsm::ConstraintInfoVector Constraints = as->ParseConstraints();

  std::vector<std::pair<Value *, int>> ResultVals;
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
                                                 E = Constraints.end();
       I != E; ++I) {

    if (I->Type != InlineAsm::isOutput) {
      ++ValueCount;
      continue; // Ignore non-output constraints.
    }

    cwriter_assert(I->Codes.size() == 1 &&
                   "Too many asm constraint codes to handle");
    std::string C = InterpretASMConstraint(*I);
    if (C.empty())
      continue;

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
      DestVal = CI.getArgOperand(ValueCount - ResultVals.size());

    if (I->isEarlyClobber)
      C = "&" + C;

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
                                                 E = Constraints.end();
       I != E; ++I) {
    if (I->Type != InlineAsm::isInput) {
      ++ValueCount;
      continue; // Ignore non-input constraints.
    }

    cwriter_assert(I->Codes.size() == 1 &&
                   "Too many asm constraint codes to handle");
    std::string C = InterpretASMConstraint(*I);
    if (C.empty())
      continue;

    if (!IsFirst) {
      Out << ", ";
      IsFirst = false;
    }

    cwriter_assert(ValueCount >= ResultVals.size() &&
                   "Input can't refer to result");
    Value *SrcVal = CI.getArgOperand(ValueCount - ResultVals.size());

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
                                                 E = Constraints.end();
       I != E; ++I) {
    if (I->Type != InlineAsm::isClobber)
      continue; // Ignore non-input constraints.

    cwriter_assert(I->Codes.size() == 1 &&
                   "Too many asm constraint codes to handle");
    std::string C = InterpretASMConstraint(*I);
    if (C.empty())
      continue;

    if (!IsFirst) {
      Out << ", ";
      IsFirst = false;
    }

    Out << '\"' << C << '"';
  }

  Out << ")";
}

void CWriter::visitAllocaInst(AllocaInst &I) {
  CurInstr = &I;

  headerUseBuiltinAlloca();

  Out << '(';
  printTypeName(Out, I.getType());
  Out << ") alloca(sizeof(";
  printTypeName(Out, I.getType()->getElementType());
  if (I.isArrayAllocation()) {
    Out << ") * (";
    writeOperand(I.getArraySize(), ContextCasted);
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
      LastIndexIsVector = dyn_cast<VectorType>(TmpI.getIndexedType());
  }

  Out << "(";

  // If the last index is into a vector, we can't print it as &a[i][j] because
  // we can't index into a vector with j in GCC.  Instead, emit this as
  // (((float*)&a[i])+j)
  // TODO: this is no longer true now that we don't represent vectors using
  // gcc-extentions
  if (LastIndexIsVector) {
    Out << "((";
    printTypeName(Out,
                  PointerType::getUnqual(LastIndexIsVector->getElementType()));
    Out << ")(";
  }

  Out << '&';

  Type *IntoT = I.getIndexedType();

  // The first index of a GEP is special. It does pointer arithmetic without
  // indexing into the element type.
  Value *FirstOp = I.getOperand();
  IntoT = I.getIndexedType();
  ++I;
  if (!isConstantNull(FirstOp)) {
    writeOperand(Ptr);
    Out << '[';
    writeOperandWithCast(FirstOp, Instruction::GetElementPtr);
    Out << ']';
  } else {
    // When the first index is 0 (very common) we can simplify it.
    if (isAddressExposed(Ptr)) {
      // Print P rather than (&P)[0]
      writeOperandInternal(Ptr);
    } else if (I != E && I.isStruct()) {
      // If the second index is a struct index, print P->f instead of P[0].f
      writeOperand(Ptr);
      Out << "->field" << cast<ConstantInt>(I.getOperand())->getZExtValue();
      // Eat the struct index
      IntoT = I.getIndexedType();
      ++I;
    } else {
      // Print (*P)[1] instead of P[0][1] (more idiomatic)
      Out << "(*";
      writeOperand(Ptr);
      Out << ")";
    }
  }

  for (; I != E; ++I) {
    Value *Opnd = I.getOperand();

    cwriter_assert(
        Opnd
            ->getType()
            ->isIntegerTy()); // TODO: indexing a Vector with a Vector is valid,
                              // but we don't support it here

    if (I.isStruct()) {
      Out << ".field" << cast<ConstantInt>(Opnd)->getZExtValue();
    } else if (IntoT->isArrayTy()) {
      // Zero-element array types are either skipped or, for pointers, peeled
      // off by skipEmptyArrayTypes. In this latter case, we can translate
      // zero-element array indexing as pointer arithmetic.
      if (IntoT->getArrayNumElements() == 0) {
        if (!isConstantNull(Opnd)) {
          // TODO: The operator precedence here is only correct if there are no
          //       subsequent indexable types other than zero-element arrays.
          cwriter_assert(skipEmptyArrayTypes(IntoT)->isSingleValueType());
          Out << " + (";
          writeOperandWithCast(Opnd, Instruction::GetElementPtr);
          Out << ')';
        }
      } else {
        Out << ".array[";
        writeOperandWithCast(Opnd, Instruction::GetElementPtr);
        Out << ']';
      }
    } else if (!IntoT->isVectorTy()) {
      Out << '[';
      writeOperandWithCast(Opnd, Instruction::GetElementPtr);
      Out << ']';
    } else {
      // If the last index is into a vector, then print it out as "+j)".  This
      // works with the 'LastIndexIsVector' code above.
      if (!isConstantNull(Opnd)) {
        Out << "))"; // avoid "+0".
      } else {
        Out << ")+(";
        writeOperandWithCast(I.getOperand(), Instruction::GetElementPtr);
        Out << "))";
      }
    }

    IntoT = I.getIndexedType();
  }
  Out << ")";
}

void CWriter::writeMemoryAccess(Value *Operand, Type *OperandType,
                                bool IsVolatile, unsigned Alignment /*bytes*/) {
  if (isAddressExposed(Operand) && !IsVolatile) {
    writeOperandInternal(Operand);
    return;
  }

  bool IsUnaligned =
      Alignment && Alignment < TD->getABITypeAlignment(OperandType);

  if (!IsUnaligned) {
    Out << '*';
    if (IsVolatile) {
      Out << "(volatile ";
      printTypeName(Out, OperandType, false);
      Out << "*)";
    }
  } else if (IsUnaligned) {
    headerUseUnalignedLoad();
    Out << "__UNALIGNED_LOAD__(";
    printTypeNameUnaligned(Out, OperandType, false);
    if (IsVolatile)
      Out << " volatile";
    Out << ", " << Alignment << ", ";
  }

  writeOperand(Operand);

  if (IsUnaligned) {
    Out << ")";
  }
}

void CWriter::visitLoadInst(LoadInst &I) {
  CurInstr = &I;

  writeMemoryAccess(I.getOperand(0), I.getType(), I.isVolatile(),
                    I.getAlignment());
}

void CWriter::visitStoreInst(StoreInst &I) {
  CurInstr = &I;

  writeMemoryAccess(I.getPointerOperand(), I.getOperand(0)->getType(),
                    I.isVolatile(), I.getAlignment());
  Out << " = ";
  Value *Operand = I.getOperand(0);
  unsigned BitMask = 0;
  if (IntegerType *ITy = dyn_cast<IntegerType>(Operand->getType()))
#if LLVM_VERSION_MAJOR <= 10
    if (!ITy->isPowerOf2ByteWidth())
#else
    if (!IsPowerOfTwo(ITy->getBitWidth()))
#endif
      // We have a bit width that doesn't match an even power-of-2 byte
      // size. Consequently we must & the value with the type's bit mask
      BitMask = ITy->getBitMask();
  if (BitMask)
    Out << "((";
  writeOperand(Operand, BitMask ? ContextNormal : ContextCasted);
  if (BitMask)
    Out << ") & " << BitMask << ")";
}

void CWriter::visitFenceInst(FenceInst &I) {
  headerUseThreadFence();
  Out << "__atomic_thread_fence(";
  switch (I.getOrdering()) {
  case AtomicOrdering::Acquire:
    Out << "__ATOMIC_ACQUIRE";
    break;
  case AtomicOrdering::Release:
    Out << "__ATOMIC_RELEASE";
    break;
  case AtomicOrdering::AcquireRelease:
    Out << "__ATOMIC_ACQ_REL";
    break;
  case AtomicOrdering::SequentiallyConsistent:
    Out << "__ATOMIC_SEQ_CST";
    break;
  case AtomicOrdering::NotAtomic:
  case AtomicOrdering::Unordered:
  case AtomicOrdering::Monotonic:
    Out << "__ATOMIC_RELAXED";
    break;
  default:
    errorWithMessage("Unhandled atomic ordering for fence instruction");
  }
  Out << ");\n";
}

void CWriter::visitGetElementPtrInst(GetElementPtrInst &I) {
  CurInstr = &I;

  printGEPExpression(I.getPointerOperand(), gep_type_begin(I), gep_type_end(I));
}

void CWriter::visitVAArgInst(VAArgInst &I) {
  CurInstr = &I;

  headerUseStdarg();

  Out << "va_arg(*(va_list*)";
  writeOperand(I.getOperand(0), ContextCasted);
  Out << ", ";
  printTypeName(Out, I.getType());
  Out << ");\n ";
}

void CWriter::visitInsertElementInst(InsertElementInst &I) {
  CurInstr = &I;

  // Start by copying the entire aggregate value into the result variable.
  writeOperand(I.getOperand(0));
  Type *EltTy = I.getType()->getElementType();
  cwriter_assert(I.getOperand(1)->getType() == EltTy);
  if (isEmptyType(EltTy))
    return;

  // Then do the insert to update the field.
  Out << ";\n  ";
  Out << GetValueName(&I) << ".vector[";
  writeOperand(I.getOperand(2));
  Out << "] = ";
  writeOperand(I.getOperand(1), ContextCasted);
}

void CWriter::visitExtractElementInst(ExtractElementInst &I) {
  CurInstr = &I;

  cwriter_assert(!isEmptyType(I.getType()));
  if (isa<UndefValue>(I.getOperand(0))) {
    Out << "(";
    printTypeName(Out, I.getType());
    Out << ") 0/*UNDEF*/";
  } else {
    Out << "(";
    writeOperand(I.getOperand(0));
    Out << ").vector[";
    writeOperand(I.getOperand(1));
    Out << "]";
  }
}

// <result> = shufflevector <n x <ty>> <v1>, <n x <ty>> <v2>, <m x i32> <mask>
// ; yields <m x <ty>>
void CWriter::visitShuffleVectorInst(ShuffleVectorInst &SVI) {
  CurInstr = &SVI;

  VectorType *VT = SVI.getType();
  Type *EltTy = VT->getElementType();
  VectorType *InputVT = cast<VectorType>(SVI.getOperand(0)->getType());
  cwriter_assert(!isEmptyType(VT));
  cwriter_assert(InputVT->getElementType() == VT->getElementType());

  CtorDeclTypes.insert(VT);
  Out << "llvm_ctor_";
  printTypeString(Out, VT, false);
  Out << "(";

  Constant *Zero = Constant::getNullValue(EltTy);
  unsigned NumElts = NumberOfElements(VT);
  unsigned NumInputElts = NumberOfElements(InputVT); // n
  for (unsigned i = 0; i != NumElts; ++i) {
    if (i)
      Out << ", ";
    int SrcVal = SVI.getMaskValue(i);
    if ((unsigned)SrcVal >= NumInputElts * 2) {
      Out << "/*undef*/";
      printConstant(Zero, ContextCasted);
    } else {
      // If SrcVal belongs [0, n - 1], it extracts value from <v1>
      // If SrcVal belongs [n, 2 * n - 1], it extracts value from <v2>
      // In C++, the value false is converted to zero and the value true is
      // converted to one
      Value *Op = SVI.getOperand((unsigned)SrcVal >= NumInputElts);
      if (isa<Instruction>(Op)) {
        // Do an extractelement of this value from the appropriate input.
        Out << "(";
        writeOperand(Op);
        Out << ").vector[";
        Out << ((unsigned)SrcVal >= NumInputElts ? SrcVal - NumInputElts
                                                 : SrcVal);
        Out << "]";
      } else if (isa<ConstantAggregateZero>(Op) || isa<UndefValue>(Op)) {
        printConstant(Zero, ContextCasted);
      } else {
        printConstant(
            cast<ConstantVector>(Op)->getOperand(SrcVal & (NumElts - 1)),
            ContextNormal);
      }
    }
  }
  Out << ")";
}

void CWriter::visitInsertValueInst(InsertValueInst &IVI) {
  CurInstr = &IVI;

  // Start by copying the entire aggregate value into the result variable.
  writeOperand(IVI.getOperand(0));
  Type *EltTy = IVI.getOperand(1)->getType();
  if (isEmptyType(EltTy))
    return;

  // Then do the insert to update the field.
  Out << ";\n  ";
  Out << GetValueName(&IVI);
  for (const unsigned *b = IVI.idx_begin(), *i = b, *e = IVI.idx_end(); i != e;
       ++i) {
    Type *IndexedTy = ExtractValueInst::getIndexedType(
        IVI.getOperand(0)->getType(), makeArrayRef(b, i));
    cwriter_assert(IndexedTy);
    if (IndexedTy->isArrayTy())
      Out << ".array[" << *i << "]";
    else
      Out << ".field" << *i;
  }
  Out << " = ";
  writeOperand(IVI.getOperand(1), ContextCasted);
}

void CWriter::visitExtractValueInst(ExtractValueInst &EVI) {
  CurInstr = &EVI;

  Out << "(";
  if (isa<UndefValue>(EVI.getOperand(0))) {
    Out << "(";
    printTypeName(Out, EVI.getType());
    Out << ") 0/*UNDEF*/";
  } else {
    writeOperand(EVI.getOperand(0));
    for (const unsigned *b = EVI.idx_begin(), *i = b, *e = EVI.idx_end();
         i != e; ++i) {
      Type *IndexedTy = ExtractValueInst::getIndexedType(
          EVI.getOperand(0)->getType(), makeArrayRef(b, i));
      if (IndexedTy->isArrayTy())
        Out << ".array[" << *i << "]";
      else
        Out << ".field" << *i;
    }
  }
  Out << ")";
}

LLVM_ATTRIBUTE_NORETURN void CWriter::errorWithMessage(const char *message) {
#ifndef NDEBUG
  errs() << message;
  errs() << " in: ";
  if (CurInstr != nullptr) {
    errs() << *CurInstr << " @ ";
    CurInstr->getDebugLoc().print(errs());
  } else {
    errs() << "<unknown instruction>";
  }
  errs() << "\n";
#endif

  llvm_unreachable(message);
}

} // namespace llvm_cbe
