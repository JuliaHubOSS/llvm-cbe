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
// This code tests to see that the CBE can handle the
// less than or equal to (=<) relational logic operator.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int x = 6;
  int y = 6;

  if (x <= y) {
    return x;
  }
  return 1;
}
