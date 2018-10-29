//===-- test022.c - LLVM C Backend test file ------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE can handle the
// less than (<) relational logic operator.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int x = 6;
  int y = 12;

  if (x < y) {
    return x;
  }
  return 1;
}
