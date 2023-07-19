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
// This code tests if the CBE will execute a head recursion example correctly.
// *TW
//
//===----------------------------------------------------------------------===//

int head(int n) {
  if (n == 6)
    return n;
  else
    return head(n + 1);
}

int main() { return head(0); }
