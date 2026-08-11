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
// This code tests to see that the CBE will handle zero-length arrays properly.
//
//===----------------------------------------------------------------------===//

// xfail: zero length arrays don't work

int main() {
  union {
    char zl_chars[0];
    struct {
      char first_char;
      char second_char;
    };
  } x;

  x.first_char = 2;
  x.second_char = 4;
  return x.zl_chars[0] + x.zl_chars[1];
}
