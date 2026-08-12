//===-- CBackend.cpp - C Backend ABI Plugin ------------------------------===//
//
// Loadable plugin that provides a type-preserving ABI for LLVM C backend.
// Load with: clang -Xclang -load -Xclang CBackendPlugin.so
//
// Triple format: llvm32-<os>[-<env>] or llvm64-<os>[-<env>]
//   e.g. llvm32-linux-gnu, llvm64-linux-gnu, llvm64-darwin
//
//===----------------------------------------------------------------------===//

#include "ABIInfo.h"
#include "ABIInfoImpl.h"
#include "TargetInfo.h"
#include "clang/CodeGen/CGFunctionInfo.h"
#include "llvm/Support/Compiler.h"
#include "llvm/TargetParser/Triple.h"

using namespace clang;
using namespace clang::CodeGen;

namespace {

class CBackendABIInfo : public ABIInfo {
public:
  CBackendABIInfo(CodeGenTypes &CGT) : ABIInfo(CGT) {}

  ABIArgInfo classifyReturnType(QualType RetTy) const {
    if (RetTy->isVoidType())
      return ABIArgInfo::getIgnore();
    if (RetTy->isSignedIntegerOrEnumerationType())
      return ABIArgInfo::getSignExtend(RetTy);
    else if (RetTy->isUnsignedIntegerOrEnumerationType())
      return ABIArgInfo::getZeroExtend(RetTy);
    return ABIArgInfo::getDirect(nullptr, 0, nullptr, /*CanBeFlattened*/ false);
  }

  ABIArgInfo classifyArgumentType(QualType Ty) const {
    Ty = useFirstFieldIfTransparentUnion(Ty);
    if (Ty->isVoidType())
      return ABIArgInfo::getIgnore();
    if (Ty->isSignedIntegerOrEnumerationType())
      return ABIArgInfo::getSignExtend(Ty);
    else if (Ty->isUnsignedIntegerOrEnumerationType())
      return ABIArgInfo::getZeroExtend(Ty);
    if (isAggregateTypeForABI(Ty)) {
      // Structures with either a non-trivial destructor or a non-trivial
      // copy constructor are always indirect.
      if (CGCXXABI::RecordArgABI RAA = getRecordArgABI(Ty, getCXXABI())) {
        return getNaturalAlignIndirect(Ty, /*ByVal=*/RAA ==
                                               CGCXXABI::RAA_DirectInMemory);
      }
    }
    return ABIArgInfo::getDirect(nullptr, 0, nullptr, /*CanBeFlattened*/ false);
  }

  void computeInfo(CGFunctionInfo &FI) const override {
    if (!::classifyReturnType(getCXXABI(), FI, *this))
      FI.getReturnInfo() = classifyReturnType(FI.getReturnType());
    for (auto &Arg : FI.arguments())
      Arg.info = classifyArgumentType(Arg.type);
  }

  RValue EmitVAArg(CodeGenFunction &CGF, Address VAListAddr, QualType Ty,
                   AggValueSlot Slot) const override {
    return emitVoidPtrVAArg(CGF, VAListAddr, Ty, /*IsIndirect=*/false,
                            CGF.getContext().getTypeInfoInChars(Ty),
                            CharUnits::fromQuantity(8),
                            /*AllowHigherAlign=*/true, Slot);
  }
};

class CBackendTargetCodeGenInfo : public TargetCodeGenInfo {
public:
  CBackendTargetCodeGenInfo(CodeGenTypes &CGT)
      : TargetCodeGenInfo(std::make_unique<CBackendABIInfo>(CGT)) {}
};
} // namespace

namespace clang {
namespace CodeGen {
inline std::unique_ptr<TargetCodeGenInfo>
createCBackendTargetCodeGenInfo(CodeGenModule &CGM) {
  return std::make_unique<CBackendTargetCodeGenInfo>(CGM.getTypes());
}
} // namespace CodeGen
} // namespace clang

extern "C" LLVM_ABI_EXPORT void *getCBackendABIForTarget(const llvm::Triple *T,
                                                         CodeGenModule *CGM) {
  return createCBackendTargetCodeGenInfo(*CGM).release();
}
