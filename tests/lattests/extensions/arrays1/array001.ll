declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)
declare void @llvm.memset.p0i8.i32(i8*, i8, i32, i32, i1)



define i32 @main() {
  %a = alloca i8*
  %j = alloca i32
  %x = alloca i32
  %.t = alloca i32
  %x1 = alloca i32
  %1 = mul i32 10, 4
  %2 = add i32 %1, 4
  %3 = call i8* @malloc(i32 %2)
  call void @llvm.memset.p0i8.i32(i8* %3, i8 0, i32 %2, i32 1, i1 false)
  %4 = bitcast i8* %3 to i32*
  store i32 10, i32* %4
  store i8* %3, i8** %a
  store i32 0, i32* %j
  br label %5

; <label>:5
  %6 = load i32, i32* %j
  %7 = load i8*, i8** %a
  %8 = bitcast i8* %7 to i32*
  %9 = load i32, i32* %8
  %10 = icmp slt i32 %6, %9
  br i1 %10, label %11, label %20

; <label>:11
  %12 = load i32, i32* %j
  %13 = load i8*, i8** %a
  %14 = getelementptr i8, i8* %13, i32 4
  %15 = bitcast i8* %14 to i32*
  %16 = getelementptr i32, i32* %15, i32 %12
  %17 = load i32, i32* %j
  store i32 %17, i32* %16
  %18 = load i32, i32* %j
  %19 = add i32 %18, 1
  store i32 %19, i32* %j
  br label %5

; <label>:20
  store i32 0, i32* %x
  store i32 0, i32* %.t
  br label %21

; <label>:21
  %22 = load i32, i32* %.t
  %23 = load i8*, i8** %a
  %24 = bitcast i8* %23 to i32*
  %25 = load i32, i32* %24
  %26 = icmp slt i32 %22, %25
  br i1 %26, label %27, label %37

; <label>:27
  %28 = load i32, i32* %.t
  %29 = load i8*, i8** %a
  %30 = getelementptr i8, i8* %29, i32 4
  %31 = bitcast i8* %30 to i32*
  %32 = getelementptr i32, i32* %31, i32 %28
  %33 = load i32, i32* %32
  store i32 %33, i32* %x
  %34 = load i32, i32* %x
  call void @printInt(i32 %34)
  %35 = load i32, i32* %.t
  %36 = add i32 %35, 1
  store i32 %36, i32* %.t
  br label %21

; <label>:37
  store i32 45, i32* %x1
  %38 = load i32, i32* %x1
  call void @printInt(i32 %38)
  ret i32 0
}
