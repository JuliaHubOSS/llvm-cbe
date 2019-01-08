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

#include "llvm/IR/DataLayout.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {

struct CTargetMachine : public TargetMachine {
  CTargetMachine(const Target &T, const Triple &TT, StringRef CPU, StringRef FS,
                 const TargetOptions &Options, Optional<Reloc::Model> RM,
                 Optional<CodeModel::Model> CM, CodeGenOpt::Level OL, bool JIT)
      : TargetMachine(T, "", TT, CPU, FS, Options) {}

  /// Add passes to the specified pass manager to get the specified file
  /// emitted.  Typically this will involve several steps of code generation.
  bool addPassesToEmitFile(PassManagerBase &PM, raw_pwrite_stream &Out,
#if LLVM_VERSION_MAJOR >= 7
                           raw_pwrite_stream *DwoOut,
#endif
                           CodeGenFileType FileType, bool DisableVerify = true,
                           MachineModuleInfo *MMI = nullptr) override;
};

extern Target TheCBackendTarget;

} // namespace llvm

#endif
