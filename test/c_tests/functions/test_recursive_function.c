//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Test if CBE can handle a recursive function.
// *TW
//
//===----------------------------------------------------------------------===//

int fibonaci(int i) {
  if (i == 0) {
    return 0;
  }
  if (i == 1) {
    return 1;
  }
  return fibonaci(i - 1) + fibonaci(i - 2);
}

int main() {
  int returnval;
  returnval = fibonaci(6) - 2;

  return returnval;
}
