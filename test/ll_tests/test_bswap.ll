declare i16 @llvm.bswap.i16(i16)
declare i32 @llvm.bswap.i32(i32)
declare i64 @llvm.bswap.i64(i64)

define dso_local i32 @main() #0 {
bswap.i16:
  %bswap.i16.val = call i16 @llvm.bswap.i16(i16 4660) ; 0x1234
  %bswap.i16.ok  = icmp eq i16 %bswap.i16.val, 13330 ; 0x3412
  br i1 %bswap.i16.ok, label %bswap.i32, label %error

bswap.i32:
  %bswap.i32.val = call i32 @llvm.bswap.i32(i32 305419896) ; 0x12345678
  %bswap.i32.ok  = icmp eq i32 %bswap.i32.val, 2018915346 ; 0x78563412
  br i1 %bswap.i32.ok, label %bswap.i64, label %error

bswap.i64:
  %bswap.i64.val = call i64 @llvm.bswap.i64(i64 1311768467463790320) ; 0x123456789abcdef0
  %bswap.i64.ok  = icmp eq i64 %bswap.i64.val, 17356517385562371090 ; 0xf0debc9a78563412
  br i1 %bswap.i64.ok, label %ok, label %error

ok:
  ret i32 6

error:
  %retVal = phi i32
    [ 20, %bswap.i16 ],
    [ 21, %bswap.i32 ],
    [ 22, %bswap.i64 ]
  ret i32 %retVal
}
