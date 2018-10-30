#include <stdatomic.h>

int main() {
  atomic_int x = 0;
  atomic_fetch_xor(&x, 7);
  atomic_fetch_xor(&x, 1);
  return atomic_load(&x);
}
