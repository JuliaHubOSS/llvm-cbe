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
// This code tests to see that the CBE will execute a nested for loop correctly.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int i, j, x = 0;
  for (i = 0; i < 3; i++)
    for (j = 0; j < 2; j++)
      ++x;

  return x;
}
