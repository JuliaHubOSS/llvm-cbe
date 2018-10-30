#include <stdatomic.h>

int main() {
  atomic_int x = 6;
  return atomic_load(&x);
}
