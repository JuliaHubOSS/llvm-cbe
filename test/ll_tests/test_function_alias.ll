; Test function aliases
; It is inspired by LLVM IR produced by rustc 1.69 for code like
;
; #[no_mangle]
; unsafe extern "C" fn drop_vec_of_a(
;     data: *mut A,
;     len: usize,
;     capacity: usize,
; ) {
;     drop(Vec::from_raw_parts(data, len, capacity))
; }
; #[no_mangle]
; unsafe extern "C" fn drop_vec_of_b(
;     data: *mut B,
;     len: usize,
;     capacity: usize,
; ) {
;     drop(Vec::from_raw_parts(data, len, capacity))
; }
;
; but with some simplifications.

; Function Attrs: noinline nounwind optnone uwtable
define i32 @drop_vec_of_a(i32 %retVal) #0 {
  ret i32 %retVal
}

@drop_vec_of_b = unnamed_addr alias i32 (i32), ptr @drop_vec_of_a

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #1 {
  %a = call i32 @drop_vec_of_a(i32 2)
  %b = call i32 @drop_vec_of_b(i32 4)
  %sum = add i32 %a, %b
  ret i32 %sum
}