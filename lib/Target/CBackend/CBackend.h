
#include "CTargetMachine.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/CodeGen/IntrinsicLowering.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Pass.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Transforms/Scalar.h"

namespace {
  using namespace llvm;

  class CBEMCAsmInfo : public MCAsmInfo {
  public:
    CBEMCAsmInfo() {
      PrivateGlobalPrefix = "";
    }
  };

  /// CWriter - This class is the main chunk of code that converts an LLVM
  /// module to a C translation unit.
  class CWriter : public FunctionPass, public InstVisitor<CWriter> {
    std::string _Out;
    raw_string_ostream Out;
    raw_pwrite_stream &FileOut;
    IntrinsicLowering *IL;
    LoopInfo *LI;
    const Module *TheModule;
    const MCAsmInfo* TAsm;
    const MCRegisterInfo *MRI;
    const MCObjectFileInfo *MOFI;
    MCContext *TCtx;
    const DataLayout* TD;

    std::map<const ConstantFP *, unsigned> FPConstantMap;
    std::set<const Argument*> ByValParams;
    unsigned FPCounter;
    unsigned OpaqueCounter;

    DenseMap<const Value*, unsigned> AnonValueNumbers;
    unsigned NextAnonValueNumber;

    /// UnnamedStructIDs - This contains a unique ID for each struct that is
    /// either anonymous or has no name.
    DenseMap<StructType*, unsigned> UnnamedStructIDs;
    unsigned NextAnonStructNumber;

    std::set<StructType*> StructTypes;
    std::set<ArrayType*> ArrayTypes;
    std::set<Type*> SelectDeclTypes;
    std::set<std::pair<CmpInst::Predicate, VectorType*>> CmpDeclTypes;
    std::set<std::pair<CastInst::CastOps, std::pair<Type*, Type*>>> CastOpDeclTypes;
    std::set<std::pair<unsigned, Type*>> InlineOpDeclTypes;
    std::set<Type*> CtorDeclTypes;

    DenseMap<std::pair<FunctionType*, AttributeSet>, unsigned> UnnamedFunctionIDs;
    unsigned NextFunctionNumber;

    // This is used to keep track of intrinsics that get generated to a lowered
    // function. We must generate the prototypes before the function body which
    // will only be expanded on first use
    std::vector<Function*> prototypesToGen;

  public:
    static char ID;
    explicit CWriter(raw_pwrite_stream &o)
      : FunctionPass(ID), Out(_Out), FileOut(o), IL(0), LI(0),
        TheModule(0), TAsm(0), MRI(0), MOFI(0), TCtx(0), TD(0),
        OpaqueCounter(0), NextAnonValueNumber(0),
        NextAnonStructNumber(0), NextFunctionNumber(0) {
      FPCounter = 0;
    }

    virtual const char *getPassName() const { return "C backend"; }

    void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.addRequired<LoopInfoWrapperPass>();
      AU.setPreservesCFG();
    }

    virtual bool doInitialization(Module &M);
    virtual bool doFinalization(Module &M);
    virtual bool runOnFunction(Function &F);

  private:

    void generateHeader(Module &M);

    raw_ostream &printFunctionProto(raw_ostream &Out, FunctionType *Ty,
                           AttributeSet PAL, const std::string &Name);
    raw_ostream &printFunctionProto(raw_ostream &Out, Function *F) {
      return printFunctionProto(Out, F->getFunctionType(), F->getAttributes(), GetValueName(F));
    }

    raw_ostream &printFunctionDeclaration(raw_ostream &Out, FunctionType *Ty,
                           AttributeSet PAL, const std::string &Name);
    raw_ostream &printStructDeclaration(raw_ostream &Out, StructType *Ty);
    raw_ostream &printArrayDeclaration(raw_ostream &Out, ArrayType *Ty);

    raw_ostream &printTypeName(raw_ostream &Out, Type *Ty, bool isSigned = false, AttributeSet PAL = AttributeSet());
    raw_ostream &printSimpleType(raw_ostream &Out, Type *Ty, bool isSigned);
    raw_ostream &printTypeString(raw_ostream &Out, Type *Ty, bool isSigned);

    std::string getStructName(StructType *ST);
    std::string getFunctionName(FunctionType *FT, AttributeSet PAL);
    std::string getArrayName(ArrayType *AT);

    void writeOperandDeref(Value *Operand);
    void writeOperand(Value *Operand, bool Static = false);
    void writeInstComputationInline(Instruction &I);
    void writeOperandInternal(Value *Operand, bool Static = false);
    void writeOperandWithCast(Value* Operand, unsigned Opcode);
    void opcodeNeedsCast(unsigned Opcode, bool &shouldCast, bool &castIsSigned);

    void writeOperandWithCast(Value* Operand, ICmpInst &I);
    bool writeInstructionCast(Instruction &I);
    void writeMemoryAccess(Value *Operand, Type *OperandType,
                           bool IsVolatile, unsigned Alignment);

    std::string InterpretASMConstraint(InlineAsm::ConstraintInfo& c);

    void lowerIntrinsics(Function &F);
    /// Prints the definition of the intrinsic function F. Supports the
    /// intrinsics which need to be explicitly defined in the CBackend.
    void printIntrinsicDefinition(Function &F, raw_ostream &Out);

    void printModuleTypes(raw_ostream &Out);
    void printContainedStructs(raw_ostream &Out, Type *Ty, SmallPtrSet<Type *, 16> &);
    void printFunctionSignature(raw_ostream &Out, Function *F);

    void printFloatingPointConstants(Function &F);
    void printFloatingPointConstants(const Constant *C);

    void printFunction(Function &);
    void printBasicBlock(BasicBlock *BB);
    void printLoop(Loop *L);

    void printCast(unsigned opcode, Type *SrcTy, Type *DstTy);
    void printConstant(Constant *CPV, bool Static);
    void printConstantWithCast(Constant *CPV, unsigned Opcode);
    bool printConstExprCast(ConstantExpr *CE);
    void printConstantArray(ConstantArray *CPA, bool Static);
    void printConstantVector(ConstantVector *CV, bool Static);
    void printConstantDataSequential(ConstantDataSequential *CDS, bool Static);
    bool printConstantString(Constant *C, bool Static);

    bool isEmptyType(Type *Ty) const;
    bool isAddressExposed(Value *V) const;
    bool isInlinableInst(Instruction &I) const;
    AllocaInst *isDirectAlloca(Value *V) const;
    bool isInlineAsm(Instruction& I) const;

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
    void visitBinaryOperator(BinaryOperator &I);
    void visitICmpInst(ICmpInst &I);
    void visitFCmpInst(FCmpInst &I);

    void visitCastInst (CastInst &I);
    void visitSelectInst(SelectInst &I);
    void visitCallInst (CallInst &I);
    void visitInlineAsm(CallInst &I);
    bool visitBuiltinCall(CallInst &I, Intrinsic::ID ID);

    void visitAllocaInst(AllocaInst &I);
    void visitLoadInst  (LoadInst   &I);
    void visitStoreInst (StoreInst  &I);
    void visitGetElementPtrInst(GetElementPtrInst &I);
    void visitVAArgInst (VAArgInst &I);

    void visitInsertElementInst(InsertElementInst &I);
    void visitExtractElementInst(ExtractElementInst &I);
    void visitShuffleVectorInst(ShuffleVectorInst &SVI);

    void visitInsertValueInst(InsertValueInst &I);
    void visitExtractValueInst(ExtractValueInst &I);

    void visitInstruction(Instruction &I) {
#ifndef NDEBUG
      errs() << "C Writer does not know about " << I;
#endif
      llvm_unreachable(0);
    }

    void outputLValue(Instruction *I) {
      Out << "  " << GetValueName(I) << " = ";
    }

    bool isGotoCodeNecessary(BasicBlock *From, BasicBlock *To);
    void printPHICopiesForSuccessor(BasicBlock *CurBlock,
                                    BasicBlock *Successor, unsigned Indent);
    void printBranchToBlock(BasicBlock *CurBlock, BasicBlock *SuccBlock,
                            unsigned Indent);
    void printGEPExpression(Value *Ptr, gep_type_iterator I, gep_type_iterator E);

    std::string GetValueName(Value *Operand);
  };
}
