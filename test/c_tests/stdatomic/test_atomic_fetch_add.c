#include <stdatomic.h>

// xfail: no atomic support (#10)

int main() {
  atomic_int x = 0;
  atomic_fetch_add(&x, 3);
  atomic_fetch_add(&x, 3);
  return atomic_load(&x);
}
