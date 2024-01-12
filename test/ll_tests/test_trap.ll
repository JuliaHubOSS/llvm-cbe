declare void @llvm.trap() cold noreturn nounwind

define dso_local i32 @main() #0 {
  %should_program_exit_normally  = icmp eq i1 1, 1
  br i1 %should_program_exit_normally, label %ok, label %this_should_not_happen

ok:
  ret i32 6

this_should_not_happen:
  call void @llvm.trap()
  unreachable
}
