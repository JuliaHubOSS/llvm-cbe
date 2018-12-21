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
// AND (&&) relational logic operator.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int x = 6;
  int y = 6;
  int z = 6;

  if (x == y && x == z) {
    return 6;
  }
  return 1;
}
