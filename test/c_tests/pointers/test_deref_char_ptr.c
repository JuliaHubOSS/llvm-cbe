//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will properly use a char pointer and
// return the value located originally at the address of x.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  char x = 'a', y = 'b', *cp;
  cp = &x;
  y = *cp;
  if (y == 'a') {
    return 6;
  }
  return 1;
}
