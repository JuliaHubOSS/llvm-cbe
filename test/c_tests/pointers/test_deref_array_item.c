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
// This code tests to see that the CBE will properly use a pointer to
// access an array.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int *ip;
  int a[2];
  a[0] = 1;
  a[1] = 6;
  ip = &a[1];

  return *ip;
}
