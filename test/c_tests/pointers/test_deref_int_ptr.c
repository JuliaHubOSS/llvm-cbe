//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will properly use an integer pointer and
// return the value located originally at the address of x.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int x = 6, y = 0, *ip = 0;
  ip = &x;
  y = *ip;
  return y;
}
