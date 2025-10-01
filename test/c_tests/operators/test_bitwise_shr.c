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
// This code tests to see that the CBE can handle the
// Binary Shift Right (a>>b) bitwise operator.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  unsigned int a = 13; // 1100
  unsigned int b = 0;

  b = a >> 1; // 0110
  if (b == 6) {
    return 6;
  }
  return 1;
}
