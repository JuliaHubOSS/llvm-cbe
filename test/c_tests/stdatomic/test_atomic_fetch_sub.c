#include <stdatomic.h>

// xfail: no atomic support (#10)

int main() {
  atomic_int x = 12;
  atomic_fetch_sub(&x, 3);
  atomic_fetch_sub(&x, 3);
  return atomic_load(&x);
}
