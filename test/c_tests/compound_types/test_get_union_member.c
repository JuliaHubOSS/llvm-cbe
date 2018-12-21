//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will access and return union members
// correctly. *TW
//
//===----------------------------------------------------------------------===//

union Data {
  char unit1[6];
  char unit2;
  char unit3;
};

int main() {
  union Data data;
  return sizeof(data);
}
