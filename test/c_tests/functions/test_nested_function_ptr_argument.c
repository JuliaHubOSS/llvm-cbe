//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Tests if the CBE can handle a function pointer with a second function pointer
// as one of its arguments.
//
//===----------------------------------------------------------------------===//

static int return_arg(int x) { return x; }

static int call1(int (*f)(int), int x) { return f(x); }

static int call2(int (*f2)(int (*)(int), int), int (*f)(int), int x) {
  return f2(f, x);
}

int main() { return call2(call1, return_arg, 6); }
