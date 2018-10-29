//===-- test072.c - LLVM C Backend test file ------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will execute an else-if statement
// correctly. *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int x = 6;
  if (x == 4) {
    return 2;
  } else if (x == 6) {
    return 6;
  } else {
    return 8;
  }
}
