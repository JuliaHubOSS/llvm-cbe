//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will properly use a double pointer and
// confirms the value located originally at the address of x.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  float x = 6, y = 0, *fp;
  fp = &x;
  y = *fp;
  if (y == 6) {
    return 6;
  }
  return 1;
}
