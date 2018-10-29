//===-- CBackend.cpp - Library for converting LLVM code to C --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Tests if the CBE will execute a fibonacci numbers example.
// Credit: http://en.literateprograms.org/Fibonacci_numbers_(C)
// *TW
//
//===----------------------------------------------------------------------===//

unsigned int fastfib(unsigned int n);

int main() { return fastfib(6) - 2; }

unsigned int fastfib(unsigned int n) {
  unsigned int a[3];
  unsigned int *p = a;
  unsigned int i;

  for (i = 0; i <= n; ++i) {
    if (i < 2)
      *p = i;
    else {
      if (p == a)
        *p = *(a + 1) + *(a + 2);
      else if (p == a + 1)
        *p = *a + *(a + 2);
      else
        *p = *a + *(a + 1);
    }
    if (++p > a + 2)
      p = a;
  }
  return p == a ? *(p + 2) : *(p - 1);
}
