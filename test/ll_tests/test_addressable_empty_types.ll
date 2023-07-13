; This tests the handling of global variables, constants, and allocas which have
; zero-sized/empty types, and in particular taking pointers to them.
; Obviously these pointers can't be dereferenced, but it's legal to pass them
; around and compare them, so we must produce reasonable code for this.

%emptyStruct = type {}
%emptyArray = type [0 x i64]
%veryEmptyArray = type [0 x %emptyStruct]
%veryEmptyStruct = type { %emptyStruct }
%extremelyEmptyArray = type [0 x %veryEmptyStruct]

@constant1 = constant %emptyStruct zeroinitializer
@constant2 = constant %emptyArray zeroinitializer
@constant3 = constant %veryEmptyArray zeroinitializer
@constant4 = constant %veryEmptyStruct zeroinitializer
@constant5 = constant %extremelyEmptyArray zeroinitializer

@global1 = private global %emptyStruct zeroinitializer, align 16
@global2 = private global %emptyArray zeroinitializer, align 8
@global3 = private global %veryEmptyArray zeroinitializer, align 4
@global4 = private global %veryEmptyStruct zeroinitializer, align 2
@global5 = private global %extremelyEmptyArray zeroinitializer, align 1

define i32 @main() {
  %alloca1 = alloca %emptyStruct, align 1
  %alloca2 = alloca %emptyArray, align 2
  %alloca3 = alloca %veryEmptyArray, align 4
  %alloca4 = alloca %veryEmptyStruct, align 8
  %alloca5 = alloca %extremelyEmptyArray, align 16

  %eq1a = icmp eq %emptyStruct* @constant1, @global1
  %eq1b = icmp eq %emptyStruct* @constant1, %alloca1
  %eq2a = icmp eq %emptyArray* @constant2, @global2
  %eq2b = icmp eq %emptyArray* @constant2, %alloca2
  %eq3a = icmp eq %veryEmptyArray* @constant3, @global3
  %eq3b = icmp eq %veryEmptyArray* @constant3, %alloca3
  %eq4a = icmp eq %veryEmptyStruct* @constant4, @global4
  %eq4b = icmp eq %veryEmptyStruct* @constant4, %alloca4
  %eq5a = icmp eq %extremelyEmptyArray* @constant5, @global5
  %eq5b = icmp eq %extremelyEmptyArray* @constant5, %alloca5

  %zext1a = zext i1 %eq1a to i32
  %zext1b = zext i1 %eq1b to i32
  %zext2a = zext i1 %eq2a to i32
  %zext2b = zext i1 %eq2b to i32
  %zext3a = zext i1 %eq3a to i32
  %zext3b = zext i1 %eq3b to i32
  %zext4a = zext i1 %eq4a to i32
  %zext4b = zext i1 %eq4b to i32
  %zext5a = zext i1 %eq5a to i32
  %zext5b = zext i1 %eq5b to i32

  %sum1 = add i32 %zext1a, %zext1b
  %sum2 = add i32 %zext2a, %zext2b
  %sum3 = add i32 %zext3a, %zext3b
  %sum4 = add i32 %zext4a, %zext4b
  %sum5 = add i32 %zext5a, %zext5b

  %sum12 = add i32 %sum1, %sum2
  %sum34 = add i32 %sum3, %sum4
  %sum1234 = add i32 %sum12, %sum34
  %sum = add i32 %sum1234, %sum5

  %correct = icmp eq i32 %sum, 0

  %retCode = select i1 %correct, i32 6, i32 0

  ret i32 %retCode
}
