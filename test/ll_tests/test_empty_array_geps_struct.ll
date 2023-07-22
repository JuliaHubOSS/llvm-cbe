; This tests GEPs and Phi nodes for pointers to zero-sized arrays of structs.

%struct.intbox = type { i32 }

@correctConstant = constant [3 x %struct.intbox] [%struct.intbox { i32 1 }, %struct.intbox { i32 2 }, %struct.intbox { i32 3 }]
@wrongConstant = constant [5 x %struct.intbox] [%struct.intbox { i32 4 }, %struct.intbox { i32 5 }, %struct.intbox { i32 6 }, %struct.intbox { i32 7 }, %struct.intbox { i32 8 }]

define i32 @main(i32 %argc, i8** %argv) {
bb0:
  %alwaysTrue = icmp eq i32 %argc, 1
  br i1 %alwaysTrue, label %iftrue, label %iffalse

iftrue:
  br label %end

iffalse:
  br label %end

end:
  %theConstant = phi [0 x %struct.intbox]* [ bitcast ([3 x %struct.intbox]* @correctConstant to [0 x %struct.intbox]*), %iftrue ], [ bitcast ([5 x %struct.intbox]* @wrongConstant to [0 x %struct.intbox]*), %iffalse ]

  %int1ptr = getelementptr [0 x %struct.intbox], [0 x %struct.intbox]* %theConstant, i64 0, i64 0, i32 0
  %int2ptr = getelementptr [0 x %struct.intbox], [0 x %struct.intbox]* %theConstant, i64 0, i64 1, i32 0
  %int3ptr = getelementptr [0 x %struct.intbox], [0 x %struct.intbox]* %theConstant, i64 0, i64 2, i32 0

  %int1 = load i32, i32* %int1ptr, align 4
  %int2 = load i32, i32* %int2ptr, align 4
  %int3 = load i32, i32* %int3ptr, align 4

  %sum12 = add i32 %int1, %int2
  %res = add i32 %sum12, %int3

  ret i32 %res
}
