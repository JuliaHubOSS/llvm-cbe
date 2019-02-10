#include <stdatomic.h>

int main() {
  atomic_int x = 0b1111;
  atomic_fetch_and(&x, 0b1110);
  atomic_fetch_and(&x, 0b0111);
  return atomic_load(&x);
}
