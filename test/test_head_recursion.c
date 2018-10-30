//===-- test_head_recursion.c - LLVM C Backend test file ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
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
