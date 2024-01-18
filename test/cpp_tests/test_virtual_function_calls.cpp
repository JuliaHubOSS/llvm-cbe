#ifndef _MSC_VER
// Suppress "dereferencing type-punned pointer will break strict-aliasing rules"
// gcc_extra_args: -Wno-strict-aliasing
struct A {
  int a;
  int b;
  int f1(int i) { return 1 + i; }
  int f2(int i) { return 1 + i; }
  typedef int (A::*Function)(int);
  int function_wrapper(Function f, int i) __attribute__((noinline)) {
    return (this->*f)(i);
  }
};

int main() {
  A a{};
  if (a.function_wrapper(&A::f1, 1) != 2)
    return 0;
  if (a.function_wrapper(&A::f2, 1) != 2)
    return 0;
  return 6;
}

#else

/* MSVC doesn't support adding alignment to functions, treat it as an expected
 * failure */

int main() { return 25; }

#endif
