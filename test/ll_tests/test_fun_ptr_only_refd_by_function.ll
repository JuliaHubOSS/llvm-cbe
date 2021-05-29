; This tests whether the CBE will emit a definition for a function type whose
; only direct reference is a function parameter list.

%funptr = type void ()*

declare i32 @someFun(%funptr)

define i32 @main() {
  ret i32 6
}
