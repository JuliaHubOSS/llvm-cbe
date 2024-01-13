declare void @llvm.trap() cold noreturn nounwind

define dso_local i32 @main() #0 {
  ret i32 6
}

define dso_local void @this_should_not_happen() {
  call void @llvm.trap()
  unreachable
}
