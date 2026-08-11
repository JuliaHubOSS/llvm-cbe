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
// Tests if the CBE will execute a function pointer.
// *TW
//
//===----------------------------------------------------------------------===//

int subtract(int x, int y) {
  int z = x - y;
  return z;
}

int main() {
  int (*sabPtr)(int, int) = subtract;
  return (*sabPtr)(10, 4);
}
