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
// This code tests to see that the CBE will pass a structure into a function
// correctly. *TW
//
//===----------------------------------------------------------------------===//

int k = 0;

struct test {
  int i;
  float f;
};

void funct(struct test example) { k = example.i; }

int main() {
  struct test example;

  example.i = 6;
  example.f = 6.0;
  funct(example);

  return k;
}
