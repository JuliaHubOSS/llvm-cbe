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
// This code tests to see that the CBE will execute a nested switch statement
// correctly. *TW
//
//===----------------------------------------------------------------------===//

int main() {
  char var = 'x', var2;
  switch (var) {
  case 'z':
    return 0;
    break;
  case 'y':
    return 1;
    break;
  case 'x':
    var2 = 'b';

    switch (var2) {
    case 'a':
      return 10;
      break;
    case 'b':
      return 6;
      break;
    default:
      return 18;
    }

  case 'w':
    return 7;
    break;
  default:
    return 100;
  }
}
