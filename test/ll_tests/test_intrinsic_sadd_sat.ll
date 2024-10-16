declare i8 @llvm.sadd.sat.i8(i8 %a, i8 %b)
declare i16 @llvm.sadd.sat.i16(i16 %a, i16 %b)
declare i32 @llvm.sadd.sat.i32(i32 %a, i32 %b)
declare i64 @llvm.sadd.sat.i64(i64 %a, i64 %b)

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
basic_8:
  %basic_8_sum = call i8 @llvm.sadd.sat.i8(i8 52, i8 -10)
  %basic_8_ok = icmp eq i8 %basic_8_sum, 42
  br i1 %basic_8_ok, label %overflowing_8, label %error

overflowing_8:
  %overflowing_8_sum = call i8 @llvm.sadd.sat.i8(i8 u0x7C, i8 u0x7C)
  %overflowing_8_ok = icmp eq i8 %overflowing_8_sum, 127
  br i1 %overflowing_8_ok, label %underflowing_8, label %error

underflowing_8:
  %underflowing_8_sum = call i8 @llvm.sadd.sat.i8(i8 u0x8C, i8 u0x8C)
  %underflowing_8_ok = icmp eq i8 %underflowing_8_sum, -128
  br i1 %underflowing_8_ok, label %basic_16, label %error

basic_16:
  %basic_16_sum = call i16 @llvm.sadd.sat.i16(i16 52, i16 -10)
  %basic_16_ok = icmp eq i16 %basic_16_sum, 42
  br i1 %basic_16_ok, label %overflowing_16, label %error

overflowing_16:
  %overflowing_16_sum = call i16 @llvm.sadd.sat.i16(i16 u0x7CCC, i16 u0x7CCC)
  %overflowing_16_ok = icmp eq i16 %overflowing_16_sum, 32767
  br i1 %overflowing_16_ok, label %underflowing_16, label %error

underflowing_16:
  %underflowing_16_sum = call i16 @llvm.sadd.sat.i16(i16 u0x8CCC, i16 u0x8CCC)
  %underflowing_16_ok = icmp eq i16 %underflowing_16_sum, -32768
  br i1 %underflowing_16_ok, label %basic_32, label %error

basic_32:
  %basic_32_sum = call i32 @llvm.sadd.sat.i32(i32 52, i32 -10)
  %basic_32_ok = icmp eq i32 %basic_32_sum, 42
  br i1 %basic_32_ok, label %overflowing_32, label %error

overflowing_32:
  %overflowing_32_sum = call i32 @llvm.sadd.sat.i32(i32 u0x7CCCCCCC, i32 u0x7CCCCCCC)
  %overflowing_32_ok = icmp eq i32 %overflowing_32_sum, 2147483647
  br i1 %overflowing_32_ok, label %underflowing_32, label %error

underflowing_32:
  %underflowing_32_sum = call i32 @llvm.sadd.sat.i32(i32 u0x8CCCCCCC, i32 u0x8CCCCCCC)
  %underflowing_32_ok = icmp eq i32 %underflowing_32_sum, -2147483648
  br i1 %underflowing_32_ok, label %basic_64, label %error

basic_64:
  %basic_64_sum = call i64 @llvm.sadd.sat.i64(i64 52, i64 -10)
  %basic_64_ok = icmp eq i64 %basic_64_sum, 42
  br i1 %basic_64_ok, label %overflowing_64, label %error

overflowing_64:
  %overflowing_64_sum = call i64 @llvm.sadd.sat.i64(i64 u0x7CCCCCCCCCCCCCCC, i64 u0x7CCCCCCCCCCCCCCC)
  %overflowing_64_ok = icmp eq i64 %overflowing_64_sum, 9223372036854775807 
  br i1 %overflowing_64_ok, label %underflowing_64, label %error

underflowing_64:
  %underflowing_64_sum = call i64 @llvm.sadd.sat.i64(i64 u0x8CCCCCCCCCCCCCCC, i64 u0x8CCCCCCCCCCCCCCC)
  %underflowing_64_ok = icmp eq i64 %underflowing_64_sum, -9223372036854775808 
  br i1 %underflowing_64_ok, label %ok, label %error

ok:
  ret i32 6

error:
  %retVal = phi i32
    [ 81, %basic_8 ],
    [ 82, %overflowing_8 ],
    [ 83, %underflowing_8 ],
    [ 161, %basic_16 ],
    [ 162, %overflowing_16 ],
    [ 163, %underflowing_16 ],
    [ 321, %basic_32 ],
    [ 322, %overflowing_32 ],
    [ 323, %underflowing_32 ],
    [ 641, %basic_64 ],
    [ 642, %overflowing_64 ],
    [ 643, %underflowing_64 ]
  ret i32 %retVal
}
