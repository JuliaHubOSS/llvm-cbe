#include "CTargetMachine.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/CodeGen/IntrinsicLowering.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/IR/AbstractCallSite.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Pass.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Transforms/Scalar.h"

#include <optional>
#include <set>
#include <variant>

#include "IDMap.h"

namespace llvm_cbe {

using namespace llvm;

class CBEMCAsmInfo : public MCAsmInfo {
public:
  CBEMCAsmInfo() { PrivateGlobalPrefix = ""; }
};

using FunctionInfoVariant = std::variant<const Function *, const CallInst *>;

/// CWriter - This class is the main chunk of code that converts an LLVM
/// module to a C translation unit.
class CWriter : public FunctionPass, public InstVisitor<CWriter> {
  std::string _Out;
  std::string _OutHeaders;
  raw_string_ostream OutHeaders;
  raw_string_ostream Out;
  raw_ostream &FileOut;
  IntrinsicLowering *IL = nullptr;
  LoopInfo *LI = nullptr;
  const Module *TheModule = nullptr;
  const MCAsmInfo *TAsm = nullptr;
  const MCRegisterInfo *MRI = nullptr;
  const MCObjectFileInfo *MOFI = nullptr;
  MCContext *TCtx = nullptr;
  const DataLayout *TD = nullptr;
  const Instruction *CurInstr = nullptr;

  IDMap<const ConstantFP *> FPConstantMap;

  IDMap<const Value *> AnonValueNumbers;

  /// UnnamedStructIDs - This contains a unique ID for each struct that is
  /// either anonymous or has no name.
  IDMap<StructType *> UnnamedStructIDs;

  std::set<Type *> TypedefDeclTypes;
  std::set<Type *> SelectDeclTypes;
  std::set<std::pair<CmpInst::Predicate, VectorType *>> CmpDeclTypes;
  std::set<std::pair<CastInst::CastOps, std::pair<Type *, Type *>>>
      CastOpDeclTypes;
  std::set<std::pair<unsigned, Type *>> InlineOpDeclTypes;
  std::set<Type *> CtorDeclTypes;

  IDMap<FunctionInfoVariant> UnnamedFunctionIDs;

  // This is used to keep track of intrinsics that get generated to a lowered
  // function. We must generate the prototypes before the function body which
  // will only be expanded on first use
  std::vector<Function *> prototypesToGen;

  unsigned LastAnnotatedSourceLine = 0;

  struct {
    // Standard headers
    bool Stdarg : 1;
    bool Setjmp : 1;
    bool Limits : 1;
    bool Math : 1;

    // printModuleTypes()
    bool BitCastUnion : 1;

    // generateCompilerSpecificCode()
    bool BuiltinAlloca : 1;
    bool Unreachable : 1;
    bool NoReturn : 1;
    bool ExternalWeak : 1;
    bool AttributeWeak : 1;
    bool Hidden : 1;
    bool AttributeList : 1;
    bool UnalignedLoad : 1;
    bool Aligns : 1;
    bool FunctionAlign : 1;
    bool NanInf : 1;
    bool Int128 : 1;
    bool ThreadFence : 1;
    bool StackSaveRestore : 1;
    bool ConstantDoubleTy : 1;
    bool ConstantFloatTy : 1;
    bool ConstantFP80Ty : 1;
    bool ConstantFP128Ty : 1;
    bool ForceInline : 1;
    bool Trap : 1;
    bool ConstructorsDestructors : 1;
  } UsedHeaders;

#define USED_HEADERS_FLAG(Name)                                                \
  void headerUse##Name() { UsedHeaders.Name = true; }                          \
  bool headerInc##Name() const { return UsedHeaders.Name; }

  // Standard headers
  USED_HEADERS_FLAG(Stdarg)
  USED_HEADERS_FLAG(Setjmp)
  USED_HEADERS_FLAG(Limits)
  USED_HEADERS_FLAG(Math)

  // printModuleTypes()
  USED_HEADERS_FLAG(BitCastUnion)

  // generateCompilerSpecificCode()
  USED_HEADERS_FLAG(BuiltinAlloca)
  USED_HEADERS_FLAG(Unreachable)
  USED_HEADERS_FLAG(NoReturn)
  USED_HEADERS_FLAG(ExternalWeak)
  USED_HEADERS_FLAG(AttributeWeak)
  USED_HEADERS_FLAG(Hidden)
  USED_HEADERS_FLAG(AttributeList)
  USED_HEADERS_FLAG(UnalignedLoad)
  USED_HEADERS_FLAG(Aligns)
  USED_HEADERS_FLAG(FunctionAlign)
  USED_HEADERS_FLAG(NanInf)
  USED_HEADERS_FLAG(Int128)
  USED_HEADERS_FLAG(ThreadFence)
  USED_HEADERS_FLAG(StackSaveRestore)
  USED_HEADERS_FLAG(ConstantDoubleTy)
  USED_HEADERS_FLAG(ConstantFloatTy)
  USED_HEADERS_FLAG(ConstantFP80Ty)
  USED_HEADERS_FLAG(ConstantFP128Ty)
  USED_HEADERS_FLAG(ForceInline)
  USED_HEADERS_FLAG(Trap)
  USED_HEADERS_FLAG(ConstructorsDestructors)

  llvm::SmallSet<CmpInst::Predicate, 26> FCmpOps;
  void headerUseFCmpOp(CmpInst::Predicate P);

  void generateCompilerSpecificCode(raw_ostream &Out, const DataLayout *) const;

public:
  static char ID;
  explicit CWriter(raw_ostream &o)
      : FunctionPass(ID), OutHeaders(_OutHeaders), Out(_Out), FileOut(o) {
    memset(&UsedHeaders, 0, sizeof(UsedHeaders));
  }

  virtual StringRef getPassName() const { return "C backend"; }

  void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.addRequired<LoopInfoWrapperPass>();
    AU.setPreservesCFG();
  }

  virtual bool doInitialization(Module &M);
  virtual bool doFinalization(Module &M);
  virtual bool runOnFunction(Function &F);

private:
  void generateHeader(Module &M);
  void declareOneGlobalVariable(GlobalVariable *I);

  void forwardDeclareStructs(raw_ostream &Out, Type *Ty,
                             std::set<Type *> &TypesPrinted);

  raw_ostream &printFunctionAttributes(raw_ostream &Out, AttributeList Attrs);

  bool isStandardMain(const FunctionType *FTy);
  raw_ostream &printFunctionProto(raw_ostream &Out, FunctionInfoVariant FIV,
                                  const std::string_view Name);

  raw_ostream &printFunctionDeclaration(raw_ostream &Out,
                                        FunctionInfoVariant FIV,
                                        const std::string_view Name);
  raw_ostream &printStructDeclaration(raw_ostream &Out, StructType *Ty);
  raw_ostream &printArrayDeclaration(raw_ostream &Out, ArrayType *Ty);
  raw_ostream &printVectorDeclaration(raw_ostream &Out, VectorType *Ty);

  raw_ostream &printTypeName(raw_ostream &Out, Type *Ty, bool isSigned = false,
                             std::pair<AttributeList, CallingConv::ID> PAL =
                                 std::make_pair(AttributeList(),
                                                CallingConv::C));
  raw_ostream &printTypeNameForAddressableValue(raw_ostream &Out, Type *Ty,
                                                bool isSigned = false);
  raw_ostream &printSimpleType(raw_ostream &Out, Type *Ty, bool isSigned);
  raw_ostream &printTypeString(raw_ostream &Out, Type *Ty, bool isSigned);

  std::string getStructName(StructType *ST);
  std::string getFunctionName(FunctionInfoVariant FIV);
  std::string getArrayName(ArrayType *AT);
  std::string getVectorName(VectorType *VT);

  enum OperandContext {
    ContextNormal,
    ContextCasted,
    // Casted context means the type-cast will be implicit,
    // such as the RHS of a `var = RHS;` expression
    // or inside a struct initializer expression
    ContextStatic
    // Static context means that it is being used in as a static initializer
    // (also implies ContextCasted)
  };

  void writeOperandDeref(Value *Operand);
  void writeOperand(Value *Operand,
                    enum OperandContext Context = ContextNormal);
  void writeInstComputationInline(Instruction &I);
  void writeOperandInternal(Value *Operand,
                            enum OperandContext Context = ContextNormal);
  void writeOperandWithCast(Value *Operand, unsigned Opcode);
  void writeVectorOperandWithCast(Value *Operand, unsigned Index,
                                  unsigned Opcode);
  void opcodeNeedsCast(unsigned Opcode, bool &shouldCast, bool &castIsSigned);

  void writeOperandWithCast(Value *Operand, ICmpInst &I);
  bool writeInstructionCast(Instruction &I);
  void writeMemoryAccess(Value *Operand, Type *OperandType, bool IsVolatile,
                         unsigned Alignment);

  std::string InterpretASMConstraint(InlineAsm::ConstraintInfo &c);

  bool lowerIntrinsics(Function &F);
  /// Prints the definition of the intrinsic function F. Supports the
  /// intrinsics which need to be explicitly defined in the CBackend.
  void printIntrinsicDefinition(Function &F, raw_ostream &Out);
  void printIntrinsicDefinition(FunctionType *funT, unsigned Opcode,
                                std::string OpName, raw_ostream &Out);

  void printModuleTypes(raw_ostream &Out);
  void printContainedTypes(raw_ostream &Out, Type *Ty, std::set<Type *> &);

  void printFloatingPointConstants(Function &F);
  void printFloatingPointConstants(const Constant *C);

  void printFunction(Function &);
  void printBasicBlock(BasicBlock *BB);
  void printLoop(Loop *L);

  void printCast(unsigned opcode, Type *SrcTy, Type *DstTy);
  void printConstant(Constant *CPV, enum OperandContext Context);
  void printConstantWithCast(Constant *CPV, unsigned Opcode);
  bool printConstExprCast(ConstantExpr *CE);
  void printConstantArray(ConstantArray *CPA, enum OperandContext Context);
  void printConstantVector(ConstantVector *CV, enum OperandContext Context);
  void printConstantDataSequential(ConstantDataSequential *CDS,
                                   enum OperandContext Context);
  bool printConstantString(Constant *C, enum OperandContext Context);

  bool isEmptyType(Type *Ty) const;
  Type *skipEmptyArrayTypes(Type *Ty) const;
  std::optional<Type *> tryGetTypeOfAddressExposedValue(Value *V) const;
  bool isInlinableInst(Instruction &I) const;
  AllocaInst *isDirectAlloca(Value *V) const;
  bool isInlineAsm(Instruction &I) const;

  // Instruction visitation functions
  friend class InstVisitor<CWriter>;

  void visitReturnInst(ReturnInst &I);
  void visitBranchInst(BranchInst &I);
  void visitSwitchInst(SwitchInst &I);
  void visitIndirectBrInst(IndirectBrInst &I);
  void visitInvokeInst(InvokeInst &I) {
    llvm_unreachable("Lowerinvoke pass didn't work!");
  }
  void visitResumeInst(ResumeInst &I) {
    llvm_unreachable("DwarfEHPrepare pass didn't work!");
  }
  void visitUnreachableInst(UnreachableInst &I);

  void visitPHINode(PHINode &I);
  void visitUnaryOperator(UnaryOperator &I);
  void visitBinaryOperator(BinaryOperator &I);
  void visitICmpInst(ICmpInst &I);
  void visitFCmpInst(FCmpInst &I);

  void visitCastInst(CastInst &I);
  void visitSelectInst(SelectInst &I);
  void visitCallInst(CallInst &I);
  void visitInlineAsm(CallInst &I);
  bool visitBuiltinCall(CallInst &I, Intrinsic::ID ID);

  void visitAllocaInst(AllocaInst &I);
  void visitLoadInst(LoadInst &I);
  void visitStoreInst(StoreInst &I);
  void visitFenceInst(FenceInst &I);
  void visitGetElementPtrInst(GetElementPtrInst &I);
  void visitVAArgInst(VAArgInst &I);

  void visitInsertElementInst(InsertElementInst &I);
  void visitExtractElementInst(ExtractElementInst &I);
  void visitShuffleVectorInst(ShuffleVectorInst &SVI);

  void visitInsertValueInst(InsertValueInst &I);
  void visitExtractValueInst(ExtractValueInst &I);

  void visitInstruction(Instruction &I) {
    CurInstr = &I;
    errorWithMessage("unsupported LLVM instruction");
  }

  [[noreturn]] void errorWithMessage(const char *message);

  bool isGotoCodeNecessary(BasicBlock *From, BasicBlock *To);
  bool canDeclareLocalLate(Instruction &I);
  void printPHICopiesForSuccessor(BasicBlock *CurBlock, BasicBlock *Successor,
                                  unsigned Indent);
  void printBranchToBlock(BasicBlock *CurBlock, BasicBlock *SuccBlock,
                          unsigned Indent);
  void printGEPExpression(Value *Ptr, unsigned NumOperands, gep_type_iterator I,
                          gep_type_iterator E);

  std::string GetValueName(const Value *Operand);

  friend class CWriterTestHelper;
};

} // namespace llvm_cbe
