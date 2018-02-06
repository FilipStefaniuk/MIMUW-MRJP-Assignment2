declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)
declare void @llvm.memset.p0i8.i32(i8*, i8, i32, i1)


@.str = constant [4 x i8] c"foo\00"

define i32 @main() {
  %i = alloca i32
  %i1 = alloca i32
  %i2 = alloca i32
  %i3 = alloca i32
  store i32 78, i32* %i
  store i32 1, i32* %i1
  %1 = load i32, i32* %i1
  call void @printInt(i32 %1)
  %2 = load i32, i32* %i
  call void @printInt(i32 %2)
  br label %3

; <label>:3
  %4 = load i32, i32* %i
  %5 = icmp sgt i32 %4, 76
  br i1 %5, label %6, label %13

; <label>:6
  %7 = load i32, i32* %i
  %8 = sub i32 %7, 1
  store i32 %8, i32* %i
  %9 = load i32, i32* %i
  call void @printInt(i32 %9)
  %10 = load i32, i32* %i
  %11 = add i32 %10, 7
  store i32 %11, i32* %i2
  %12 = load i32, i32* %i2
  call void @printInt(i32 %12)
  br label %3

; <label>:13
  %14 = load i32, i32* %i
  call void @printInt(i32 %14)
  %15 = load i32, i32* %i
  %16 = icmp sgt i32 %15, 4
  br i1 %16, label %17, label %19

; <label>:17
  store i32 4, i32* %i3
  %18 = load i32, i32* %i3
  call void @printInt(i32 %18)
  br label %21

; <label>:19
  %20 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
  call void @printString(i8* %20)
  br label %21

; <label>:21
  %22 = load i32, i32* %i
  call void @printInt(i32 %22)
  ret i32 0
}
