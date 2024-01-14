int f(int i) {
  if (__builtin_constant_p(i) &&
      i == 1) /* generates the llvm.is.constant.i32 intrinsic at -O0 */
    return i + 5;
  return 6;
}

int main() { return f(1); }
