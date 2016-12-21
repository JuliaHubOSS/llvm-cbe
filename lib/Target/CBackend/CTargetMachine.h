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

#include "llvm/Target/TargetMachine.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/ADT/Optional.h"

namespace llvm {

struct CTargetMachine : public TargetMachine {
  CTargetMachine(const Target &T, const Triple &TargetTriple, StringRef CPU, StringRef FS,
                 const TargetOptions &Options, Optional<Reloc::Model> RM,
                 CodeModel::Model CM, CodeGenOpt::Level OL)
    : TargetMachine(T, "", TargetTriple, CPU, FS, Options) { }

  /// Add passes to the specified pass manager to get the specified file
  /// emitted.  Typically this will involve several steps of code generation.
  bool addPassesToEmitFile(
    PassManagerBase &PM, raw_pwrite_stream &Out, CodeGenFileType FileType,
    bool DisableVerify = true, AnalysisID StartBefore = nullptr,
    AnalysisID StartAfter = nullptr, AnalysisID StopAfter = nullptr,
    MachineFunctionInitializer *MFInitializer = nullptr) override;

};

extern Target TheCBackendTarget;

} // End llvm namespace


#endif
