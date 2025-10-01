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
// This code tests to see that the CBE will execute a struct correctly.
// *TW
//
//===----------------------------------------------------------------------===//

struct test {
  int var1;
  int var2;
  int var3;
};

int main() {
  struct test variable;

  variable.var2 = 5;
  variable.var3 = 6;
  variable.var1 = 9;

  return variable.var3;
}
