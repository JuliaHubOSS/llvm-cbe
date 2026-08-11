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
// This code tests to see that the CBE will execute an array correctly.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int example[10];
  int i;
  for (i = 0; i < 10; ++i) {
    example[i] = i;
  }
  return example[6];
}
