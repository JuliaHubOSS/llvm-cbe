// Test file for C Backend target
// This demonstrates the type preservation features

#include <stdint.h>

// Test 1: Integer signedness preservation
int32_t signed_add(int32_t a, int32_t b) { return a + b; }

uint32_t unsigned_add(uint32_t a, uint32_t b) { return a + b; }

// Test 2: Mixed signedness
int32_t mixed_operation(int32_t signed_val, uint32_t unsigned_val) {
  return signed_val + unsigned_val;
}

// Test 3: Structure return (should not use sret)
struct Point {
  int x;
  int y;
};

struct Point make_point(int x, int y) {
  struct Point p;
  p.x = x;
  p.y = y;
  return p;
}

// Test 4: Structure argument (should not use byval)
int point_sum(struct Point p) { return p.x + p.y; }

// Test 5: Large structure (should still avoid sret/byval)
struct LargeStruct {
  int64_t data[8];
};

struct LargeStruct make_large(int64_t val) {
  struct LargeStruct s;
  for (int i = 0; i < 8; i++) {
    s.data[i] = val * i;
  }
  return s;
}

// Test 6: Enum types (preserve underlying signedness)
enum Color { RED = 0, GREEN = 1, BLUE = 2 };

enum Color get_color(int value) { return (enum Color)value; }

// Test 7: Character types (char should be signed, unsigned char should be
// unsigned)
char signed_char_func(char c) { return c + 1; }

unsigned char unsigned_char_func(unsigned char c) { return c + 1; }

// Test 8: Short types
short signed_short_func(short s) { return s * 2; }

unsigned short unsigned_short_func(unsigned short s) { return s * 2; }

// Test 9: Long types
long signed_long_func(long l) { return l - 1; }

unsigned long unsigned_long_func(unsigned long l) { return l - 1; }

// Test 10: Boolean type
_Bool bool_func(_Bool b) { return !b; }
