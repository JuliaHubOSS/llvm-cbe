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
// This code tests to see that the CBE can handle multiplication between two
// variables. *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int i = 3, t = 2, x = 0;
  x = i * t;

  return x;
}
