#include <exception>

// xfail: no landingpad support (#17)

class Foo {
public:
  Foo() { throw std::exception(); }
};

int main() {
  try {
    Foo f;
  } catch (const std::exception &e) {
    return 6;
  }

  return 0;
}
