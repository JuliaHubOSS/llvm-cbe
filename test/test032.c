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
// Binary Shift Left (a<<b) bitwise operator.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  unsigned int a = 3; // 0011
  unsigned int b = 0;

  b = a << 1; // 0110
  if (b == 6) {
    return 6;
  }
  return 1;
}
