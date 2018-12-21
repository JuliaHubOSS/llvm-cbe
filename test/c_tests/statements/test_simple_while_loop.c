//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will execute a while loop correctly.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int i = 0, x = 0;
  while (i < 6) {
    ++x;
    ++i;
  }
  return x;
}
