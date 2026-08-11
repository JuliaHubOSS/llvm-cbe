//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will properly decrement a pointer via
// char. This example works by subtracting two mem. addresses and adding 2 to
// return 6. *TW
//
//===----------------------------------------------------------------------===//

// Suppress "array subscript -1 is outside array bounds"
// gcc_extra_args: -Wno-array-bounds

int main() {
  unsigned char inc0 = 0, inc1 = 0;
  int diff = 0, a = 100;
  int *p = &a;
  inc0 = (long)p;
  --(*p--); //--*p--);
  inc1 = (long)p;
  // Truncate back to a char before widening: inc0 and inc1 only hold the low
  // byte of each address, so their difference borrows whenever the low byte of
  // &a is zero. Without this cast the test depends on the stack address.
  diff = (unsigned char)(inc0 - inc1);
  diff += 2;
  return diff;
}
