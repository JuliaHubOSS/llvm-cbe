//===-- test073.c - LLVM C Backend test file ------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will execute a do-while statement
// correctly. *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int x = 0;
  do {
    x++;
  } while (x < 6);

  return x;
}
