#include <stdatomic.h>

int main() {
  atomic_int x = 0;
  atomic_exchange(&x, 6);
  return atomic_load(&x);
}
