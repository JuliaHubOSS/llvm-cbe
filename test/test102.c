//===-- CBackend.cpp - Library for converting LLVM code to C --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Tests if the CBE will execute a function pointer.
// *TW
//
//===----------------------------------------------------------------------===//

int subtract(int x, int y) {
  int z = x - y;
  return z;
}

int main() {
  int (*sabPtr)(int, int) = subtract;
  return (*sabPtr)(10, 4);
}
