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
// Tests if the CBE will execute Macro Substitution.
// *TW
//
//===----------------------------------------------------------------------===//
#define A 3
#define B 3
#define C A + B

int main() {
  int x = C;
  return x;
}
