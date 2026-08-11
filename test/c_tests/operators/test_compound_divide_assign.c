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
// Compound Division Assignment(a/=b) operator.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int a = 30;
  int b = 5;

  a /= b;
  if (a == 6) {
    return 6;
  }
  return 1;
}
