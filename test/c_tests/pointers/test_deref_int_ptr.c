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
// This code tests to see that the CBE will properly use an integer pointer and
// return the value located originally at the address of x.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int x = 6, y = 0, *ip = 0;
  ip = &x;
  y = *ip;
  return y;
}
