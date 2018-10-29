//===-- CBackend.cpp - Library for converting LLVM code to C --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE can handle the unary minus (-a) operator.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  signed int a = 10;
  signed int b = -a;

  if (b == -10) {
    return 6;
  }
  return 1;
}
