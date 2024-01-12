declare i64 @llvm.smin.i64(i64, i64)
declare i64 @llvm.smax.i64(i64, i64)
declare i64 @llvm.umin.i64(i64, i64)
declare i64 @llvm.umax.i64(i64, i64)

define dso_local i32 @main() #0 {
smin.check:
  %smin.res = call i64 @llvm.smin.i64(i64 6, i64 -6)
  %smin.ok  = icmp eq i64 %smin.res, -6
  br i1 %smin.ok, label %smax.check, label %error

smax.check:
  %smax.res = call i64 @llvm.smax.i64(i64 6, i64 -6)
  %smax.ok  = icmp eq i64 %smax.res, 6
  br i1 %smax.ok, label %umin.check, label %error

umin.check:
  %umin.res = call i64 @llvm.umin.i64(i64 6, i64 -6)
  %umin.ok  = icmp eq i64 %umin.res, 6
  br i1 %umin.ok, label %umax.check, label %error

umax.check:
  %umax.res = call i64 @llvm.umax.i64(i64 6, i64 -6)
  %umax.ok  = icmp eq i64 %umax.res, -6
  br i1 %umax.ok, label %ok, label %error


ok:
  ret i32 6

error:
  %retVal = phi i32
    [ 20, %smin.check ],
    [ 21, %smax.check ],
    [ 22, %umin.check ],
    [ 23, %umax.check ]
  ret i32 %retVal
}
