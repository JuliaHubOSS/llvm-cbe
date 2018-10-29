//===-- CBackend.cpp - Library for converting LLVM code to C --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE can handle the decremental (--a)
// operator. *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int x = 7;

  --x;

  return x;
}
