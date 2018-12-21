//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will execute an if/else statement
// correctly. *TW
//
//===----------------------------------------------------------------------===//

int main() {
  int x = 3;
  x += 3;
  if (x == 6)
    return x;
  else
    return 0;
}
