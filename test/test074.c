//===-- CBackend.cpp - Library for converting LLVM code to C --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will execute a break/continue statement
// correctly. *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int x;
  for (x = 0; x <= 25; x++) {
    if (x == 6)
      break;
    if (x < 15)
      continue;
  }
  return x;
}
