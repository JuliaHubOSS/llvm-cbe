//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will execute a self referencing
// structure. *TW
//
//===----------------------------------------------------------------------===//
#include <stdio.h> //for NULL

struct data {
  int a;
  struct data *ptr;
};

int main() {
  struct data p =
      (struct data){.a = 3, .ptr = &(struct data){.a = 6, .ptr = NULL}};
  return p.ptr->a;
}
