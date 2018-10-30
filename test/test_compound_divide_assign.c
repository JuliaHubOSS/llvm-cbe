//===-- test_compound_divide_assign.c - LLVM C Backend test file ----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE can handle the
// Compound Division Assignment(a/=b) operator.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int a = 30;
  int b = 5;

  a /= b;
  if (a == 6) {
    return 6;
  }
  return 1;
}
