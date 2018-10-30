#include <stdatomic.h>

int main() {
  atomic_int x = 0;
  atomic_fetch_or(&x, 2);
  atomic_fetch_or(&x, 4);
  return atomic_load(&x);
}
