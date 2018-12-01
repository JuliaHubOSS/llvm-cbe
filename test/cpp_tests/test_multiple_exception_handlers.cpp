// xfail: no landingpad support (#17)

class exc1 {
public:
  exc1() {}
  virtual ~exc1() {}
};

class exc2 {
public:
  exc2() {}
  virtual ~exc2() {}
};

int main() {
  try {
    throw exc2();
  } catch (const exc1 &e) {
    return 0;
  } catch (const exc2 &e) {
    return 6;
  }
}
