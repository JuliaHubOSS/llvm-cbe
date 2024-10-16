declare i8 @llvm.sshl.sat.i8(i8 %a, i8 %b)
declare i16 @llvm.sshl.sat.i16(i16 %a, i16 %b)
declare i32 @llvm.sshl.sat.i32(i32 %a, i32 %b)
declare i64 @llvm.sshl.sat.i64(i64 %a, i64 %b)

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
basic_8:
  %basic_8_shl = call i8 @llvm.sshl.sat.i8(i8 21, i8 1)
  %basic_8_ok = icmp eq i8 %basic_8_shl, 42
  br i1 %basic_8_ok, label %overflowing_8, label %error

overflowing_8:
  %overflowing_8_shl = call i8 @llvm.sshl.sat.i8(i8 1, i8 7)
  %overflowing_8_ok = icmp eq i8 %overflowing_8_shl, 127
  br i1 %overflowing_8_ok, label %underflowing_8, label %error

underflowing_8:
  %underflowing_8_shl = call i8 @llvm.sshl.sat.i8(i8 -1, i8 7)
  %underflowing_8_ok = icmp eq i8 %underflowing_8_shl, -128
  br i1 %underflowing_8_ok, label %basic_16, label %error

basic_16:
  %basic_16_shl = call i16 @llvm.sshl.sat.i16(i16 21, i16 1)
  %basic_16_ok = icmp eq i16 %basic_16_shl, 42
  br i1 %basic_16_ok, label %overflowing_16, label %error

overflowing_16:
  %overflowing_16_shl = call i16 @llvm.sshl.sat.i16(i16 1, i16 15)
  %overflowing_16_ok = icmp eq i16 %overflowing_16_shl, 32767
  br i1 %overflowing_16_ok, label %underflowing_16, label %error

underflowing_16:
  %underflowing_16_shl = call i16 @llvm.sshl.sat.i16(i16 -1, i16 15)
  %underflowing_16_ok = icmp eq i16 %underflowing_16_shl, -32768
  br i1 %underflowing_16_ok, label %basic_32, label %error

basic_32:
  %basic_32_shl = call i32 @llvm.sshl.sat.i32(i32 21, i32 1)
  %basic_32_ok = icmp eq i32 %basic_32_shl, 42
  br i1 %basic_32_ok, label %overflowing_32, label %error

overflowing_32:
  %overflowing_32_shl = call i32 @llvm.sshl.sat.i32(i32 1, i32 31)
  %overflowing_32_ok = icmp eq i32 %overflowing_32_shl, 2147483647
  br i1 %overflowing_32_ok, label %underflowing_32, label %error

underflowing_32:
  %underflowing_32_shl = call i32 @llvm.sshl.sat.i32(i32 -1, i32 31)
  %underflowing_32_ok = icmp eq i32 %underflowing_32_shl, -2147483648
  br i1 %underflowing_32_ok, label %basic_64, label %error

basic_64:
  %basic_64_shl = call i64 @llvm.sshl.sat.i64(i64 21, i64 1)
  %basic_64_ok = icmp eq i64 %basic_64_shl, 42
  br i1 %basic_64_ok, label %overflowing_64, label %error

overflowing_64:
  %overflowing_64_shl = call i64 @llvm.sshl.sat.i64(i64 1, i64 63)
  %overflowing_64_ok = icmp eq i64 %overflowing_64_shl, 9223372036854775807 
  br i1 %overflowing_64_ok, label %underflowing_64, label %error

underflowing_64:
  %underflowing_64_shl = call i64 @llvm.sshl.sat.i64(i64 -1, i64 63)
  %underflowing_64_ok = icmp eq i64 %underflowing_64_shl, -9223372036854775808 
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
