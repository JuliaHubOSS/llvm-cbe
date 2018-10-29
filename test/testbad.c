//===-- testbad.c - LLVM C Backend test file ------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Does not return 6, will always fail. Exception for 25 so it will succeed.
//
//===----------------------------------------------------------------------===//

int main() { return 25; }
