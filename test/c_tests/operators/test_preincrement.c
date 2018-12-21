//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE can handle the incremental (++a)
// operator. *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int x = 5;

  ++x;

  return x;
}
