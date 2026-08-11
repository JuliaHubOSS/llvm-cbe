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
// This code tests to see that the CBE can handle the unary minus (-a) operator.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  signed int a = 10;
  signed int b = -a;

  if (b == -10) {
    return 6;
  }
  return 1;
}
