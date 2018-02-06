declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)
declare void @llvm.memset.p0i8.i32(i8*, i8, i32, i1)


@.str.1 = constant [2 x i8] c" \00"
@.str.2 = constant [14 x i8] c"concatenation\00"
@.str.4 = constant [6 x i8] c"false\00"
@.str = constant [7 x i8] c"string\00"
@.str.3 = constant [5 x i8] c"true\00"

define i32 @main() {
  %x = alloca i32
  %y = alloca i32
  store i32 56, i32* %x
  store i32 -23, i32* %y
  %1 = load i32, i32* %x
  %2 = load i32, i32* %y
  %3 = add i32 %1, %2
  call void @printInt(i32 %3)
  %4 = load i32, i32* %x
  %5 = load i32, i32* %y
  %6 = sub i32 %4, %5
  call void @printInt(i32 %6)
  %7 = load i32, i32* %x
  %8 = load i32, i32* %y
  %9 = mul i32 %7, %8
  call void @printInt(i32 %9)
  call void @printInt(i32 22)
  call void @printInt(i32 0)
  %10 = load i32, i32* %x
  %11 = load i32, i32* %y
  %12 = sub i32 %10, %11
  %13 = load i32, i32* %x
  %14 = load i32, i32* %y
  %15 = add i32 %13, %14
  %16 = icmp sgt i32 %12, %15
  %17 = zext i1 %16 to i8
  call void @printBool(i8 %17)
  %18 = load i32, i32* %x
  %19 = load i32, i32* %y
  %20 = sdiv i32 %18, %19
  %21 = load i32, i32* %x
  %22 = load i32, i32* %y
  %23 = mul i32 %21, %22
  %24 = icmp sle i32 %20, %23
  %25 = zext i1 %24 to i8
  call void @printBool(i8 %25)
  %26 = getelementptr [7 x i8], [7 x i8]* @.str, i32 0, i32 0
  %27 = getelementptr [2 x i8], [2 x i8]* @.str.1, i32 0, i32 0
  %28 = call i8* @concat(i8* %26, i8* %27)
  %29 = getelementptr [14 x i8], [14 x i8]* @.str.2, i32 0, i32 0
  %30 = call i8* @concat(i8* %28, i8* %29)
  call void @printString(i8* %30)
  ret i32 0
}

define void @printBool(i8 %b) {
  %1 = alloca i8
  store i8 %b, i8* %1
  %2 = load i8, i8* %1
  %3 = trunc i8 %2 to i1
  br i1 %3, label %4, label %6

; <label>:4
  %5 = getelementptr [5 x i8], [5 x i8]* @.str.3, i32 0, i32 0
  call void @printString(i8* %5)
  ret void

; <label>:6
  %7 = getelementptr [6 x i8], [6 x i8]* @.str.4, i32 0, i32 0
  call void @printString(i8* %7)
  ret void
}
