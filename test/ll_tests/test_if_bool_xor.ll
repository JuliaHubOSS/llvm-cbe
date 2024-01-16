define dso_local i1 @a() {
  ret i1 1
}

define dso_local i32 @main() #0 {
  %ret  = call zeroext i1 @a()
  %cond = xor i1 %ret, true
  br i1 %cond, label %a_is_false, label %a_is_true

a_is_false:
  ret i32 -1

a_is_true:
  ret i32 6
}
