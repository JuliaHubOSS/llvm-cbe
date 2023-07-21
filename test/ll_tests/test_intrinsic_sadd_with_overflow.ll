declare {i8, i1} @llvm.sadd.with.overflow.i8(i8 %a, i8 %b)
declare {i16, i1} @llvm.sadd.with.overflow.i16(i16 %a, i16 %b)
declare {i32, i1} @llvm.sadd.with.overflow.i32(i32 %a, i32 %b)
declare {i64, i1} @llvm.sadd.with.overflow.i64(i64 %a, i64 %b)

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
basic_8:
  %basic_8_res = call { i8, i1 } @llvm.sadd.with.overflow.i8(i8 52, i8 -10)
  %basic_8_sum = extractvalue { i8, i1 } %basic_8_res, 0
  %basic_8_sum_ok = icmp eq i8 %basic_8_sum, 42
  br i1 %basic_8_sum_ok, label %basic_8_overflow_check, label %error

basic_8_overflow_check:
  %basic_8_overflowed = extractvalue { i8, i1 } %basic_8_res, 1
  br i1 %basic_8_overflowed, label %error, label %overflowing_8

overflowing_8:
  %overflowing_8_res = call { i8, i1 } @llvm.sadd.with.overflow.i8(i8 u0x7C, i8 u0x7C)
  %overflowing_8_overflowed = extractvalue { i8, i1 } %overflowing_8_res, 1
  br i1 %overflowing_8_overflowed, label %basic_16, label %error

basic_16:
  %basic_16_res = call { i16, i1 } @llvm.sadd.with.overflow.i16(i16 52, i16 -10)
  %basic_16_sum = extractvalue { i16, i1 } %basic_16_res, 0
  %basic_16_sum_ok = icmp eq i16 %basic_16_sum, 42
  br i1 %basic_16_sum_ok, label %basic_16_overflow_check, label %error

basic_16_overflow_check:
  %basic_16_overflowed = extractvalue { i16, i1 } %basic_16_res, 1
  br i1 %basic_16_overflowed, label %error, label %overflowing_16

overflowing_16:
  %overflowing_16_res = call { i16, i1 } @llvm.sadd.with.overflow.i16(i16 u0x7CCC, i16 u0x7CCC)
  %overflowing_16_overflowed = extractvalue { i16, i1 } %overflowing_16_res, 1
  br i1 %overflowing_16_overflowed, label %basic_32, label %error

basic_32:
  %basic_32_res = call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 52, i32 -10)
  %basic_32_sum = extractvalue { i32, i1 } %basic_32_res, 0
  %basic_32_sum_ok = icmp eq i32 %basic_32_sum, 42
  br i1 %basic_32_sum_ok, label %basic_32_overflow_check, label %error

basic_32_overflow_check:
  %basic_32_overflowed = extractvalue { i32, i1 } %basic_32_res, 1
  br i1 %basic_32_overflowed, label %error, label %overflowing_32

overflowing_32:
  %overflowing_32_res = call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 u0x7CCCCCCC, i32 u0x7CCCCCCC)
  %overflowing_32_overflowed = extractvalue { i32, i1 } %overflowing_32_res, 1
  br i1 %overflowing_32_overflowed, label %basic_64, label %error

basic_64:
  %basic_64_res = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 52, i64 -10)
  %basic_64_sum = extractvalue { i64, i1 } %basic_64_res, 0
  %basic_64_sum_ok = icmp eq i64 %basic_64_sum, 42
  br i1 %basic_64_sum_ok, label %basic_64_overflow_check, label %error

basic_64_overflow_check:
  %basic_64_overflowed = extractvalue { i64, i1 } %basic_64_res, 1
  br i1 %basic_64_overflowed, label %error, label %overflowing_64

overflowing_64:
  %overflowing_64_res = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 u0x7CCCCCCCCCCCCCCC, i64 u0x7CCCCCCCCCCCCCCC)
  %overflowing_64_overflowed = extractvalue { i64, i1 } %overflowing_64_res, 1
  br i1 %overflowing_64_overflowed, label %ok, label %error

ok:
  ret i32 6

error:
  %retVal = phi i32
    [ 81, %basic_8 ],
    [ 82, %basic_8_overflow_check ],
    [ 83, %overflowing_8 ],
    [ 161, %basic_16 ],
    [ 162, %basic_16_overflow_check ],
    [ 163, %overflowing_16 ],
    [ 321, %basic_32 ],
    [ 322, %basic_32_overflow_check ],
    [ 323, %overflowing_32 ],
    [ 641, %basic_64 ],
    [ 642, %basic_64_overflow_check ],
    [ 643, %overflowing_64 ]
  ret i32 %retVal
}
