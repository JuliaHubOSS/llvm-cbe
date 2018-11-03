//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE can handle a static function.
//
//===----------------------------------------------------------------------===//

static int foo(int x);

int main() {
  return foo(4);
}

static int foo(int x) {
  return x + 2;
}
