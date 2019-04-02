%TOpaque = type opaque
%TReal = type { i32 (%TOpaque*) *} 

; Function Attrs: nounwind uwtable
define i32 @call4(%TOpaque*) unnamed_addr {
  %bcast = bitcast %TOpaque* %0 to %TReal*
  %2 = getelementptr %TReal, %TReal* %bcast, i32 0, i32 0
  %3 = load i32 (%TOpaque*) *, i32 (%TOpaque*) ** %2
  %4 = call i32 %3(%TOpaque* %0)
  ret i32 %4
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
  ret i32 6
}
