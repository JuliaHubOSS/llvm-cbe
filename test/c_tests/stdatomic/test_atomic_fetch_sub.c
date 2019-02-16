#include <stdatomic.h>

int main() {
  atomic_int x = 12;
  atomic_fetch_sub(&x, 3);
  atomic_fetch_sub(&x, 3);
  return atomic_load(&x);
}
