; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
  %a = alloca i3, align 4
  store i3 7, i3* %a, align 4
  %b = load i3, i3* %a, align 4 
  %c = sext i3 %b to i17
  %d = alloca i17, align 4
  store i17 6, i17* %d, align 4
  %e = load i17, i17* %d, align 4
  %f = and i17 %c, %e
  %g = zext i17 %f to i32
  ret i32 %g
}
