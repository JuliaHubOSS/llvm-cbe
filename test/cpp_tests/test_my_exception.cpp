// xfail: no landingpad support (#17)

class my_exception {
public:
  my_exception() {}
  virtual ~my_exception() {}
};

int main() {
  try {
    throw my_exception();
  } catch (const my_exception &e) {
    return 6;
  }

  return 0;
}
