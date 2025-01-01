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
// This code tests to see that the CBE will execute an else-if statement
// correctly. *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int x = 6;
  if (x == 4) {
    return 2;
  } else if (x == 6) {
    return 6;
  } else {
    return 8;
  }
}
