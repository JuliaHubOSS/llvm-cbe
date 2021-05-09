//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests that the CBE correctly handles pointers to two-dimensional
// arrays. Specifically, it is a test for GEPs where the first index is not zero
// and the element type is an array.
//
//===----------------------------------------------------------------------===//

int sumArray(int array[2][2]) {
  return array[0][0] + array[0][1] + array[1][0] + array[1][1];
}

int main(void) {
  int sumsToSix[2][2] = {{1, -2}, {3, 4}};
  return sumArray(sumsToSix);
}
