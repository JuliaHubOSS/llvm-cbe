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
// This code tests if the CBE will execute a tail recursion example correctly.
// *TW
//
//===----------------------------------------------------------------------===//

int tail(int n) {
  if (n == 6)
    return n;
  else
    return tail(n + 1);
}

int main() { return tail(0); }
