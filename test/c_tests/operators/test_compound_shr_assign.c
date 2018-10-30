//===-- test_compound_shr_assign.c - LLVM C Backend test file -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE can handle the
// Binary Shift Right (a>>=b) Compound Assignment bitwise operator.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  unsigned int a = 13; // 1100

  a >>= 1; // 0110
  if (a == 6) {
    return 6;
  }
  return 1;
}
