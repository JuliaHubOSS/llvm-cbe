//===-- CBackend.cpp - Library for converting LLVM code to C --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Test if CBE can handle register variables.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  register int counter = 0;
  counter += 6;

  return 6;
}
