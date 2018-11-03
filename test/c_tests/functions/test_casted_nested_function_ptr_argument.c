//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Tests if the CBE can handle casting void* to and from a function pointer with
// a second function pointer as one of its arguments.
//
//===----------------------------------------------------------------------===//

// xfail: function pointer typedefs are sometimes in the wrong order

static int return_arg(int x) {
    return x;
}

static int call1(void *f, int x) {
    return ((int (*)(int))f)(x);
}

static int call2(void *f2, void *f, int x) {
    return ((int (*)(int (*)(int), int))f2)(f, x);
}

int main() {
  return call2(call1, return_arg, 6);
}
