//===-- CBackend.cpp - Library for converting LLVM code to C --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests if the CBE will execute a tail recursion example correctly.
// *TW
//
//===----------------------------------------------------------------------===//

int tail(int n) {
  if (n == 6)
    return n;
  else
    return tail(n + 1);
}

int main() { return tail(0); }
