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
// This code tests to see that the CBE will handle zero-length arrays properly,
// when they are used as function arguments.
//
//===----------------------------------------------------------------------===//

// xfail: zero length arrays don't work

int foo(char some_array[0]) { return some_array[0] + some_array[1]; }

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
  return foo(x.zl_chars);
}
