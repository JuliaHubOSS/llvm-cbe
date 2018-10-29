//===-- CBackend.cpp - Library for converting LLVM code to C --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will execute an if statement correctly.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int x = 6;
  if (x == 6)
    return x;
}
