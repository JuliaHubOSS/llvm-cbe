//===-- CBackendTargetInfo.cpp - CBackend Target Implementation -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "../CTargetMachine.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/TargetRegistry.h"

using namespace llvm;

Target llvm::TheCBackendTarget;

extern "C" void LLVMInitializeCBackendTargetInfo() {
  RegisterTarget<> X(TheCBackendTarget, "c", "C backend", "C");
}

extern "C" void LLVMInitializeCBackendTargetMC() {}
