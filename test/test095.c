//===-- test095.c - LLVM C Backend test file ------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will execute an array of structures.
// *TW
//
//===----------------------------------------------------------------------===//

struct Shows {
  char show[20];
  int runlength;
  int rating;
};

int main() {
  struct Shows b1[3] = {
      {"Big Bang Theory", 22, 6},
      {"NCIS", 45, 9},
  };
  return b1[0].rating;
}
