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
// This code tests to see that the CBE will execute a do-while statement
// correctly. *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int x = 0;
  do {
    x++;
  } while (x < 6);

  return x;
}
