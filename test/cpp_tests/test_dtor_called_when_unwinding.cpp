#include <exception>

// xfail: no landingpad support (#17)

class Foo {
public:
  Foo(int *x) : x(x) {}
  ~Foo() { *x = 6; }
  int *x;
};

int main() {
  int y = 1;

  try {
    Foo f(&y);
    // f's dtor should modify y to 6 during exception unwinding, when the
    // exception is thrown
    throw std::exception();
  } catch (const std::exception &e) {
  }

  return y;
}
