#ifndef _MSC_VER
int global_var = 0;

__attribute__((constructor)) static void update_global_var_in_constructor() {
  global_var = 6;
}

int main() { return global_var; }
#else
int main() {
  return 25; /* constructor/destructor attributes not supported on MSVC */
}
#endif
