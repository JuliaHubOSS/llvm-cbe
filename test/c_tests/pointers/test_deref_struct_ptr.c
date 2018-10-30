//===-- test_deref_struct_ptr.c - LLVM C Backend test file ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will properly use a pointer to
// a structure (->).
// *TW
//
//===----------------------------------------------------------------------===//

struct Number {
  int price;
};

int main() {
  struct Number a;
  struct Number *ptr = &a;
  ptr->price = 6;
  return ptr->price;
}
