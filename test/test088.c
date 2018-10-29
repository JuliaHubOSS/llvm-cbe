//===-- CBackend.cpp - Library for converting LLVM code to C --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will properly declare a multi-dimension.
// This example works by using a pointer to access address[1][1] to return 6.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int a[2][2];
  int *ip;
  a[0][0] = 0;
  a[0][1] = 1;
  a[1][0] = 3;
  a[1][1] = 6;
  ip = &a[1][1];

  return *ip;
}
