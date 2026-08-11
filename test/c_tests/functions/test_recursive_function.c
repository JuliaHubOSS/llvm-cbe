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
// Test if CBE can handle a recursive function.
// *TW
//
//===----------------------------------------------------------------------===//

int fibonaci(int i) {
  if (i == 0) {
    return 0;
  }
  if (i == 1) {
    return 1;
  }
  return fibonaci(i - 1) + fibonaci(i - 2);
}

int main() {
  int returnval;
  returnval = fibonaci(6) - 2;

  return returnval;
}
