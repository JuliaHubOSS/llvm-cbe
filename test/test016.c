//===-- CBackend.cpp - Library for converting LLVM code to C --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE can handle the unary plus (+a) operator.
// Increases the amount of space the char data type to 4.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  char ch;

  if (sizeof(+ch) == 4) {
    return 6;
  }
  return 1;
}
