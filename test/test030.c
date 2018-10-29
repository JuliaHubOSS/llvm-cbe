//===-- CBackend.cpp - Library for converting LLVM code to C --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE can handle the
// OR (a&b) bitwise operator.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  unsigned int a = 2;
  unsigned int b = 4;
  unsigned int c = 0;

  c = a | b;
  if (c == 6) {
    return 6;
  }
  return 1;
}
