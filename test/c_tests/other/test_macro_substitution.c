//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Tests if the CBE will execute Macro Substitution.
// *TW
//
//===----------------------------------------------------------------------===//
#define A 3
#define B 3
#define C A + B

int main() {
  int x = C;
  return x;
}
