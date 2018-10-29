//===-- CBackend.cpp - Library for converting LLVM code to C --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will execute a Goto-Label statement
// correctly. *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int x = 0;
  goto label;

  for (;;) {
    x = 10;
    return x;
  }

label:
  x = 6;
  return x;
}
