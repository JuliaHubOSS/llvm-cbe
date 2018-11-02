#include <exception>

// xfail: no std::exception support

int main() {
  try {
    throw std::exception();
  } catch (const std::exception& e) {
    return 6;
  }

  return 0;
}
