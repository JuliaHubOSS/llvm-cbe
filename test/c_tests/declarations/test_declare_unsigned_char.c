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
// This code tests to see that the CBE can handle declaring
// and returning an unsigned char.
// *TW
//
//===----------------------------------------------------------------------===//

int main() {
  unsigned char a = 'A';
  int ia = 0;

  ia = a;
  ia -= 59;

  return ia;
}
