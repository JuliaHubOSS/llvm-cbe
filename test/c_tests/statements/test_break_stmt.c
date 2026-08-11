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
// This code tests to see that the CBE will execute a break/continue statement
// correctly. *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int x;
  for (x = 0; x <= 25; x++) {
    if (x == 6)
      break;
    if (x < 15)
      continue;
  }
  return x;
}
