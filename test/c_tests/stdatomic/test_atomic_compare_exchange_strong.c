#include <stdatomic.h>

// xfail: no atomic support (#10)

int main() {
  atomic_int x = 0;
  int y = 1;

  if (atomic_compare_exchange_strong(&x, &y, 6)) {
    return 1;
  }
  // compare exchange failed, so x shouldn't be 6 now
  if (atomic_load(&x) == 6) {
    return 1;
  }

  y = 0;
  if (!atomic_compare_exchange_strong(&x, &y, 6)) {
    return 1;
  }
  return atomic_load(&x);
}
