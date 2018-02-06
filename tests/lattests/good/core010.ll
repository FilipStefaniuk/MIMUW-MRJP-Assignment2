declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)
declare void @llvm.memset.p0i8.i32(i8*, i8, i32, i1)



define i32 @main() {
  %1 = call i32 @fac(i32 5)
  call void @printInt(i32 %1)
  ret i32 0
}

define i32 @fac(i32 %a) {
  %1 = alloca i32
  %r = alloca i32
  %n = alloca i32
  store i32 %a, i32* %1
  store i32 0, i32* %r
  store i32 0, i32* %n
  store i32 1, i32* %r
  %2 = load i32, i32* %1
  store i32 %2, i32* %n
  br label %3

; <label>:3
  %4 = load i32, i32* %n
  %5 = icmp sgt i32 %4, 0
  br i1 %5, label %6, label %12

; <label>:6
  %7 = load i32, i32* %r
  %8 = load i32, i32* %n
  %9 = mul i32 %7, %8
  store i32 %9, i32* %r
  %10 = load i32, i32* %n
  %11 = sub i32 %10, 1
  store i32 %11, i32* %n
  br label %3

; <label>:12
  %13 = load i32, i32* %r
  ret i32 %13
}
