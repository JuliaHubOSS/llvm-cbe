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
// This code tests to see that the CBE can handle declaring and returning a
// float. *TW
//
//===----------------------------------------------------------------------===//

int main() {
  float a = 6.0;

  int ia = 0;
  ia = (int)a;

  return ia;
}
