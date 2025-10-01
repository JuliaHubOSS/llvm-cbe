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
// This code tests to see that the CBE will properly use a double pointer and
// confirms the value located originally at the address of x.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  float x = 6, y = 0, *fp;
  fp = &x;
  y = *fp;
  if (y == 6) {
    return 6;
  }
  return 1;
}
