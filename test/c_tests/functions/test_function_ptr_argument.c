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
// Tests if the CBE can handle a function pointer passed as an argument.
//
//===----------------------------------------------------------------------===//

static int do_op(int (*op)(int, int), int x, int y) { return op(x, y); }

static int subtract(int x, int y) {
  int z = x - y;
  return z;
}

int main() { return do_op(subtract, 10, 4); }
