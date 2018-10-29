//===-- CBackend.cpp - Library for converting LLVM code to C --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will properly increment a pointer via
// char. This example works by subtracting two mem. addresses and adding 2 to
// return 6. *TW
//
//===----------------------------------------------------------------------===//

int main() {
  char inc0 = 0, inc1 = 0;
  int diff = 0, a = 100;
  int *p = &a;
  inc0 = (long)p;
  ++(*p++); //++(*p++);
  inc1 = (long)p;
  diff = inc1 - inc0;
  diff += 2;
  return diff;
}
