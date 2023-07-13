//===-- CBackendTargetInfo.cpp - CBackend Target Implementation -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "../CTargetMachine.h"
#include "llvm/IR/Module.h"
#if LLVM_VERSION_MAJOR >= 16
#include "llvm/MC/TargetRegistry.h"
#else
#include "llvm/Support/TargetRegistry.h"
#endif
using namespace llvm;

Target llvm::TheCBackendTarget;

extern "C" void LLVMInitializeCBackendTargetInfo() {
  RegisterTarget<> X(TheCBackendTarget, "c", "C backend", "C");
}

extern "C" void LLVMInitializeCBackendTargetMC() {}
