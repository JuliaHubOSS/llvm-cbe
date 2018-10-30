//===-- test_declare_signed_short.c - LLVM C Backend test file ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE can handle declaring and returning a
// signed short. *TW
//
//===----------------------------------------------------------------------===//

int main() {
  signed short a = 6;

  int ia = 0;
  ia = (int)a;

  return ia;
}
