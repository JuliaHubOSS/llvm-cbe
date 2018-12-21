class Foo {
public:
  Foo(int *x) : x(x) {}
  ~Foo() { *x = 6; }
  int *x;
};

int main() {
  int y = 0;
  { Foo f(&y); }
  return y;
}
