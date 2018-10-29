//===-- CBackend.cpp - Library for converting LLVM code to C --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Test if CBE can handle static variables.
// *TW
//
//===----------------------------------------------------------------------===//

int subtrby2(int x);
static int eight = 8;
static int two = 2;

int main() {
  int n;
  n = subtrby2(eight);
  return n;
}

int subtrby2(int x) {
  int p;
  p = x - two;
  return (p);
}
