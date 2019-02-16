#include <stdatomic.h>

int main() {
  atomic_int x = 0;
  atomic_store(&x, 6);
  return atomic_load(&x);
}
