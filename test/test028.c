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
// NOT (~a) bitwise operator.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  unsigned int a = -7;
  unsigned int b = 0;

  b = ~a;
  if (b == 6) {
    return 6;
  }
  return 1;
}
