#include <stdatomic.h>

// xfail: no atomic support (#10)

int main() {
  atomic_int x = 6;
  return atomic_load(&x);
}
