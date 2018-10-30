//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will properly use a pointer to
// access an array.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int *ip;
  int a[2];
  a[0] = 1;
  a[1] = 6;
  ip = &a[1];

  return *ip;
}
