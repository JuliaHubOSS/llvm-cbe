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
// Test if CBE can handle register variables.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  register int counter = 0;
  counter += 6;

  return 6;
}
