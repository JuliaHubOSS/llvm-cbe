//===-- test014.c - LLVM C Backend test file ------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE can handle division between two
// variables. *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int i = 30, t = 5, x = 0;
  x = i / t;

  return x;
}
