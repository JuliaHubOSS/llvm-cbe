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
// This code tests to see that the CBE can handle a simple addition function.
// *TW
//
//===----------------------------------------------------------------------===//

int addby2(int x);

int main() {
  int n;
  n = addby2(4);
  return n;
}

int addby2(int x) {
  int p;
  p = x + 2;
  return (p);
}
