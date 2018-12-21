//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will execute a union and check the data
// size correctly. *TW
//
//===----------------------------------------------------------------------===//

union Data {
  int i;
  float f;
  char str[8];
};

int main() {
  union Data data;
  int datasize = sizeof(data) - 2;

  return datasize;
}
