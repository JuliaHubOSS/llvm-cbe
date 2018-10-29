//===-- test055.c - LLVM C Backend test file ------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE can handle declaring and returning a
// short. *TW
//
//===----------------------------------------------------------------------===//

int main() {
  short a = 6;

  int ia = 0;
  ia = (int)a;

  return ia;
}
