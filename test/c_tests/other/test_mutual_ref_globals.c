//===---------------------- LLVM C Backend test file ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This code tests to see that the CBE will properly initialize global variables
// that reference each other. (Issue #4)
//
//===----------------------------------------------------------------------===//

// xfail: mutually referring globals don't work (#4)

extern void* pA;
extern void* pB;
void* pA = &pB;
void* pB = &pA;

int main() {
  if (**(void***)pA != pA) {
    return 1;
  }
  if (**(void***)pB != pB) {
    return 2;
  }
  return 6;
}
