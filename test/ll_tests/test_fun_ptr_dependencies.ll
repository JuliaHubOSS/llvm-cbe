; This tests that CBE will emit function type definitions before other function
; type definitions that use them, even in the presence of more than one level of
; pointer indirection.

%struct.funptr_struct = type { void (void ()**)*, void ()* }

define i32 @main() {
  %1 = alloca %struct.funptr_struct, align 8
  ret i32 6
}
