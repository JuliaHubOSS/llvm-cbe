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
// This code tests to see that the CBE will access and return union members
// correctly. *TW
//
//===----------------------------------------------------------------------===//

union Data {
  char unit1[6];
  char unit2;
  char unit3;
};

int main() {
  union Data data;
  return sizeof(data);
}
