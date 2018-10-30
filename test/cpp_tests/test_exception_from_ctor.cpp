#include <exception>

class Foo {
 public:
  Foo() { throw std::exception(); }
};

int main() {
  try {
    Foo f;
  } catch (const std::exception& e) {
    return 6;
  }

  return 0;
}
