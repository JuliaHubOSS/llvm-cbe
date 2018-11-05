class Foo {
public:
  Foo(int x) : x(x) {}
  int x;
};

int main() {
  Foo f(6);
  return f.x;
}
