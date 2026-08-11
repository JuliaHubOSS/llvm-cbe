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
// Test if CBE can handle static variables.
// *TW
//
//===----------------------------------------------------------------------===//

int subtrby2(int x);
static int eight = 8;
static int two = 2;

int main() {
  int n;
  n = subtrby2(eight);
  return n;
}

int subtrby2(int x) {
  int p;
  p = x - two;
  return (p);
}
