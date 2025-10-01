//===-- CTargetMachine.cpp - TargetMachine for the C backend ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the TargetMachine that is used by the C backend.
//
//===----------------------------------------------------------------------===//

#include "CTargetMachine.h"
#include "CBackend.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/Transforms/Utils.h"

namespace llvm {

bool CTargetMachine::addPassesToEmitFile(PassManagerBase &PM,
                                         raw_pwrite_stream &Out,
                                         raw_pwrite_stream *DwoOut,
                                         CodeGenFileType FileType,
                                         bool DisableVerify,
                                         MachineModuleInfoWrapperPass *MMI) {

  if (FileType != CodeGenFileType::AssemblyFile)
    return true;

  PM.add(new TargetPassConfig(*this, PM));
  PM.add(createGCLoweringPass());

  // Remove exception handling with LowerInvokePass. This would be done with
  // TargetPassConfig if TargetPassConfig supported TargetMachines that aren't
  // LLVMTargetMachines.
  PM.add(createLowerInvokePass());
  PM.add(createUnreachableBlockEliminationPass());

  // Lower atomic operations to libcalls
  PM.add(createAtomicExpandLegacyPass());

  // Lower vector operations into shuffle sequences
  PM.add(createExpandReductionsPass());

  PM.add(new llvm_cbe::CWriter(Out));
  return false;
}

const TargetSubtargetInfo *
CTargetMachine::getSubtargetImpl(const Function &) const {
  return &SubtargetInfo;
}

bool CTargetSubtargetInfo::enableAtomicExpand() const { return true; }

const TargetLowering *CTargetSubtargetInfo::getTargetLowering() const {
  return &Lowering;
}

} // namespace llvm
