; This tests GEPs being indexed via vectors.

@correctStringConstant = constant [3 x i8] c"\01\02\03"
@wrongStringConstant = constant [3 x i8] c"\04\05\06"

define i32 @main(i32 %argc, i8** %argv) {
bb0:
  %alwaysTrue = icmp eq i32 %argc, 1
  br i1 %alwaysTrue, label %iftrue, label %iffalse

iftrue:
  br label %end

iffalse:
  br label %end

end:
  %stringConstant = phi ptr [ @correctStringConstant, %iftrue ], [ @wrongStringConstant, %iffalse ]

  %charPtrVec = getelementptr i8, ptr %stringConstant, <3 x i64> <i64 0, i64 1, i64 2>

  %char1ptr = extractelement <3 x ptr> %charPtrVec, i64 0
  %char2ptr = extractelement <3 x ptr> %charPtrVec, i64 1
  %char3ptr = extractelement <3 x ptr> %charPtrVec, i64 2

  %char1 = load i8, i8* %char1ptr
  %char2 = load i8, i8* %char2ptr
  %char3 = load i8, i8* %char3ptr

  %sum12 = add i8 %char1, %char2
  %sum = add i8 %sum12, %char3
  %res = zext i8 %sum to i32

  ret i32 %res
}
