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
// This code tests to see that the CBE will properly use the address-of value
// (&) variable and and return the value-at address (*) variable from integer
// 'num'. *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int *ptr;
  int num = 6;
  ptr = &num;
  int deref = *ptr;
  return deref;
}
