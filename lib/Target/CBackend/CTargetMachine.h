//===-- CTargetMachine.h - TargetMachine for the C backend ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the TargetMachine that is used by the C backend.
//
//===----------------------------------------------------------------------===//

#ifndef CTARGETMACHINE_H
#define CTARGETMACHINE_H

#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {

class CTargetLowering : public TargetLowering {
public:
  explicit CTargetLowering(const TargetMachine &TM) : TargetLowering(TM) {
    setMaxAtomicSizeInBitsSupported(0);
  }
};

class CTargetSubtargetInfo : public TargetSubtargetInfo {
public:
  CTargetSubtargetInfo(const TargetMachine &TM, const Triple &TT, StringRef CPU,
                       StringRef FS)
      : TargetSubtargetInfo(TT, CPU, FS, ArrayRef<SubtargetFeatureKV>(),
                            ArrayRef<SubtargetSubTypeKV>(), nullptr, nullptr,
                            nullptr, nullptr, nullptr, nullptr),
        Lowering(TM) {}
  bool enableAtomicExpand() const override;
  const TargetLowering *getTargetLowering() const override;
  const CTargetLowering Lowering;
};

class CTargetMachine : public LLVMTargetMachine {
public:
  CTargetMachine(const Target &T, const Triple &TT, StringRef CPU, StringRef FS,
                 const TargetOptions &Options, Optional<Reloc::Model> RM,
                 Optional<CodeModel::Model> CM, CodeGenOpt::Level OL,
                 bool /*JIT*/)
      : LLVMTargetMachine(T, "", TT, CPU, FS, Options,
                          RM.hasValue() ? RM.getValue() : Reloc::Static,
                          CM.hasValue() ? CM.getValue() : CodeModel::Small, OL),
        SubtargetInfo(*this, TT, CPU, FS) {}

  /// Add passes to the specified pass manager to get the specified file
  /// emitted.  Typically this will involve several steps of code generation.
  bool addPassesToEmitFile(PassManagerBase &PM, raw_pwrite_stream &Out,
#if LLVM_VERSION_MAJOR >= 7
                           raw_pwrite_stream *DwoOut,
#endif
                           CodeGenFileType FileType, bool DisableVerify = true,
                           MachineModuleInfo *MMI = nullptr) override;

  // TargetMachine interface
  const TargetSubtargetInfo *getSubtargetImpl(const Function &) const override;
  const CTargetSubtargetInfo SubtargetInfo;
};

extern Target TheCBackendTarget;

} // namespace llvm

#endif
