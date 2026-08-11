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
// This code tests to see that the CBE will execute a nested while loop
// correctly. *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int i = 0, j = 0, x = 0;
  while (i < 6) {
    while (j < 6) {
      ++x;
      ++j;
    }
    ++i;
  }
  return x;
}
