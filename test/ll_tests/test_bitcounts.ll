declare i64 @llvm.ctpop.i64(i64)
declare i64 @llvm.ctlz.i64(i64, i1) ; i1: is_zero_poison
declare i16 @llvm.ctlz.i16(i16, i1) ; i1: is_zero_poison
declare i64 @llvm.cttz.i64(i64, i1); i1: is_zero_poison>
declare i16 @llvm.cttz.i16(i16, i1); i1: is_zero_poison>


define dso_local i32 @main() #0 {
ctpop.1:
  %ctpop.1.val = call i64 @llvm.ctpop.i64(i64 18446744073709551615) ; 0xffffffffffffffff
  %ctpop.1.ok  = icmp eq i64 %ctpop.1.val, 64
  br i1 %ctpop.1.ok, label %ctpop.2, label %error

ctpop.2:
  %ctpop.2.val = call i64 @llvm.ctpop.i64(i64 0)
  %ctpop.2.ok  = icmp eq i64 %ctpop.2.val, 0
  br i1 %ctpop.2.ok, label %ctpop.3, label %error

ctpop.3:
  %ctpop.3.val = call i64 @llvm.ctpop.i64(i64 181) ; 0b10110101
  %ctpop.3.ok  = icmp eq i64 %ctpop.3.val, 5
  br i1 %ctpop.3.ok, label %ctlz.1, label %error

ctlz.1:
  %ctlz.1.val = call i64 @llvm.ctlz.i64(i64 18446744073709551615, i1 0) ; 0xffffffffffffffff
  %ctlz.1.ok  = icmp eq i64 %ctlz.1.val, 0
  br i1 %ctlz.1.ok, label %ctlz.2, label %error

ctlz.2:
  %ctlz.2.val = call i64 @llvm.ctlz.i64(i64 1, i1 0)
  %ctlz.2.ok  = icmp eq i64 %ctlz.2.val, 63
  br i1 %ctlz.2.ok, label %ctlz.3, label %error

ctlz.3:
  %ctlz.3.val = call i16 @llvm.ctlz.i16(i16 181, i1 0) ; 0b10110101
  %ctlz.3.ok  = icmp eq i16 %ctlz.3.val, 8
  br i1 %ctlz.3.ok, label %cttz.1, label %error

cttz.1:
  %cttz.1.val = call i64 @llvm.cttz.i64(i64 18446744073709551615, i1 0) ; 0xffffffffffffffff
  %cttz.1.ok  = icmp eq i64 %cttz.1.val, 0
  br i1 %cttz.1.ok, label %cttz.2, label %error

cttz.2:
  %cttz.2.val = call i64 @llvm.cttz.i64(i64 1, i1 0)
  %cttz.2.ok  = icmp eq i64 %cttz.2.val, 0
  br i1 %cttz.2.ok, label %cttz.3, label %error

cttz.3:
  %cttz.3.val = call i16 @llvm.cttz.i16(i16 180, i1 0) ; 0b10110100
  %cttz.3.ok  = icmp eq i16 %cttz.3.val, 2
  br i1 %cttz.3.ok, label %ok, label %error

ok:
  ret i32 6

error:
  %retVal = phi i32
    [ 20, %ctpop.1 ],
    [ 21, %ctpop.2 ],
    [ 22, %ctpop.3 ],
    [ 23, %ctlz.1 ],
    [ 24, %ctlz.2 ],
    [ 25, %ctlz.3 ],
    [ 26, %cttz.1 ],
    [ 27, %cttz.2 ],
    [ 28, %cttz.3 ]
  ret i32 %retVal
}
