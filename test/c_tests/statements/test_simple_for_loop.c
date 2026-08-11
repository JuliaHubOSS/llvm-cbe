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
// This code tests to see that the CBE will execute a for loop correctly.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int i, x = 0;
  for (i = 0; i < 6; i++)
    ++x;
  return x;
}
