; This tests GEPs and Phi nodes for pointers to zero-sized arrays.
; It is inspired by LLVM IR produced by rustc 1.45.0 for code like
;
; match some_expression {
;   Case1 => "Test string",
;   Case2 => "Different test string",
; }.as_ptr()
;
; but with some simplifications.

; Suppress "array is too small to include a terminating null character"
; msvc_extra_args: /wd4295

@correctStringConstant = constant [3 x i8] c"\01\02\03"
@wrongStringConstant = constant [5 x i8] c"test\00"

define i32 @main(i32 %argc, i8** %argv) {
bb0:
  %alwaysTrue = icmp eq i32 %argc, 1
  br i1 %alwaysTrue, label %iftrue, label %iffalse

iftrue:
  br label %end

iffalse:
  br label %end

end:
  %stringConstant = phi [0 x i8]* [ bitcast ([3 x i8]* @correctStringConstant to [0 x i8]*), %iftrue ], [ bitcast ([5 x i8]* @wrongStringConstant to [0 x i8]*), %iffalse ]

  %char1ptr = getelementptr [0 x i8], [0 x i8]* %stringConstant, i64 0, i64 0
  %char2ptr = getelementptr [0 x i8], [0 x i8]* %stringConstant, i64 0, i64 1
  %char3ptr = getelementptr [0 x i8], [0 x i8]* %stringConstant, i64 0, i64 2

  %char1 = load i8, i8* %char1ptr
  %char2 = load i8, i8* %char2ptr
  %char3 = load i8, i8* %char3ptr

  %sum12 = add i8 %char1, %char2
  %sum = add i8 %sum12, %char3
  %res = zext i8 %sum to i32

  ret i32 %res
}
