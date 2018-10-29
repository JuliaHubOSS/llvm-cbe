//===-- CBackend.cpp - Library for converting LLVM code to C --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE can handle a simple addition function.
// *TW
//
//===----------------------------------------------------------------------===//

int addby2(int x);

int main() {
  int n;
  n = addby2(4);
  return n;
}

int addby2(int x) {
  int p;
  p = x + 2;
  return (p);
}
