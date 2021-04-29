; This tests whether the CBE will emit a definition for a function type whose
; only direct reference is a struct type. (Issue #101)

%struct.funptr_struct = type { void ()* }

define i32 @main() {
  %1 = alloca %struct.funptr_struct, align 8
  ret i32 6
}
