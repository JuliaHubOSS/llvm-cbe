//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests if the CBE will execute a tail recursion example correctly.
// *TW
//
//===----------------------------------------------------------------------===//

// When a phi gets set to `poison` we don't initialize the PHI_TEMPORARY variable
// so suppress "uninitialized local variable '...' used".
// msvc_extra_args: /wd4700

int tail(int n) {
  if (n == 6)
    return n;
  else
    return tail(n + 1);
}

int main() { return tail(0); }
