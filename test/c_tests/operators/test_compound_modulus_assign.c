//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE can handle the
// Compound Modulus Assignment(a%=b) operator.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int a = 20;
  int b = 14;

  a %= b;
  if (a == 6) {
    return 6;
  }
  return 1;
}
