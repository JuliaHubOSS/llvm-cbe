//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will execute a thread fence statement
// correctly.
//
//===----------------------------------------------------------------------===//

int main() {
  int x = 6;
  __atomic_thread_fence(__ATOMIC_SEQ_CST);
  return x;
}
