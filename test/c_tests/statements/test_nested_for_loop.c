//===-- test_nested_for_loop.c - LLVM C Backend test file -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will execute a nested for loop correctly.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int i, j, x = 0;
  for (i = 0; i < 3; i++)
    for (j = 0; j < 2; j++)
      ++x;

  return x;
}
