//===-- test093.c - LLVM C Backend test file ------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will execute a nested structure properly.
// *TW
//
//===----------------------------------------------------------------------===//

struct layer1 {
  int depth1;
  char name1[20];
};

struct layer2 {
  int depth2;
  char name2[20];
  struct layer1 layer_data;
} layer2_data;

int main() {
  struct layer2 layer2_data = {1, "test", {6, "test2"}};

  return layer2_data.layer_data.depth1;
}
