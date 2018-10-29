//===-- test089.c - LLVM C Backend test file ------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will execute data-packing in a structure
// correctly. *TW
//
//===----------------------------------------------------------------------===//

#pragma pack(push)
#pragma pack(1)

struct DataSize {
  char Data2;
  char Data3;
  int Data1;
};

int main() {
  struct DataSize example;
  return sizeof(example);
}

#pragma pack(pop)
