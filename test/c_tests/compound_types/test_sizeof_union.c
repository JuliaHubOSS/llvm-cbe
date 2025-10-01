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
// This code tests to see that the CBE will execute a union and check the data
// size correctly. *TW
//
//===----------------------------------------------------------------------===//

union Data {
  int i;
  float f;
  char str[8];
};

int main() {
  union Data data;
  int datasize = sizeof(data) - 2;

  return datasize;
}
