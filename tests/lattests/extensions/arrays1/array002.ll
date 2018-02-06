declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)
declare void @llvm.memset.p0i8.i32(i8*, i8, i32, i32, i1)



define i8* @doubleArray(i8* %a) {
  %1 = alloca i8*
  %res = alloca i8*
  %i = alloca i32
  %n = alloca i32
  %.t = alloca i32
  store i8* %a, i8** %1
  %2 = load i8*, i8** %1
  %3 = bitcast i8* %2 to i32*
  %4 = load i32, i32* %3
  %5 = mul i32 %4, 4
  %6 = add i32 %5, 4
  %7 = call i8* @malloc(i32 %6)
  call void @llvm.memset.p0i8.i32(i8* %7, i8 0, i32 %6, i32 1, i1 false)
  %8 = bitcast i8* %7 to i32*
  store i32 %4, i32* %8
  store i8* %7, i8** %res
  store i32 0, i32* %i
  store i32 0, i32* %n
  store i32 0, i32* %.t
  br label %9

; <label>:9
  %10 = load i32, i32* %.t
  %11 = load i8*, i8** %1
  %12 = bitcast i8* %11 to i32*
  %13 = load i32, i32* %12
  %14 = icmp slt i32 %10, %13
  br i1 %14, label %15, label %33

; <label>:15
  %16 = load i32, i32* %.t
  %17 = load i8*, i8** %1
  %18 = getelementptr i8, i8* %17, i32 4
  %19 = bitcast i8* %18 to i32*
  %20 = getelementptr i32, i32* %19, i32 %16
  %21 = load i32, i32* %20
  store i32 %21, i32* %n
  %22 = load i32, i32* %i
  %23 = load i8*, i8** %res
  %24 = getelementptr i8, i8* %23, i32 4
  %25 = bitcast i8* %24 to i32*
  %26 = getelementptr i32, i32* %25, i32 %22
  %27 = load i32, i32* %n
  %28 = mul i32 2, %27
  store i32 %28, i32* %26
  %29 = load i32, i32* %i
  %30 = add i32 %29, 1
  store i32 %30, i32* %i
  %31 = load i32, i32* %.t
  %32 = add i32 %31, 1
  store i32 %32, i32* %.t
  br label %9

; <label>:33
  %34 = load i8*, i8** %res
  ret i8* %34
}

define void @shiftLeft(i8* %a) {
  %1 = alloca i8*
  %x = alloca i32
  %i = alloca i32
  store i8* %a, i8** %1
  %2 = load i8*, i8** %1
  %3 = getelementptr i8, i8* %2, i32 4
  %4 = bitcast i8* %3 to i32*
  %5 = getelementptr i32, i32* %4, i32 0
  %6 = load i32, i32* %5
  store i32 %6, i32* %x
  store i32 0, i32* %i
  br label %7

; <label>:7
  %8 = load i32, i32* %i
  %9 = load i8*, i8** %1
  %10 = bitcast i8* %9 to i32*
  %11 = load i32, i32* %10
  %12 = sub i32 %11, 1
  %13 = icmp slt i32 %8, %12
  br i1 %13, label %14, label %29

; <label>:14
  %15 = load i32, i32* %i
  %16 = load i8*, i8** %1
  %17 = getelementptr i8, i8* %16, i32 4
  %18 = bitcast i8* %17 to i32*
  %19 = getelementptr i32, i32* %18, i32 %15
  %20 = load i32, i32* %i
  %21 = add i32 %20, 1
  %22 = load i8*, i8** %1
  %23 = getelementptr i8, i8* %22, i32 4
  %24 = bitcast i8* %23 to i32*
  %25 = getelementptr i32, i32* %24, i32 %21
  %26 = load i32, i32* %25
  store i32 %26, i32* %19
  %27 = load i32, i32* %i
  %28 = add i32 %27, 1
  store i32 %28, i32* %i
  br label %7

; <label>:29
  %30 = load i8*, i8** %1
  %31 = bitcast i8* %30 to i32*
  %32 = load i32, i32* %31
  %33 = sub i32 %32, 1
  %34 = load i8*, i8** %1
  %35 = getelementptr i8, i8* %34, i32 4
  %36 = bitcast i8* %35 to i32*
  %37 = getelementptr i32, i32* %36, i32 %33
  %38 = load i32, i32* %x
  store i32 %38, i32* %37
  ret void
}

define i32 @scalProd(i8* %a, i8* %b) {
  %1 = alloca i8*
  %2 = alloca i8*
  %res = alloca i32
  %i = alloca i32
  store i8* %a, i8** %1
  store i8* %b, i8** %2
  store i32 0, i32* %res
  store i32 0, i32* %i
  br label %3

; <label>:3
  %4 = load i32, i32* %i
  %5 = load i8*, i8** %1
  %6 = bitcast i8* %5 to i32*
  %7 = load i32, i32* %6
  %8 = icmp slt i32 %4, %7
  br i1 %8, label %9, label %27

; <label>:9
  %10 = load i32, i32* %res
  %11 = load i32, i32* %i
  %12 = load i8*, i8** %1
  %13 = getelementptr i8, i8* %12, i32 4
  %14 = bitcast i8* %13 to i32*
  %15 = getelementptr i32, i32* %14, i32 %11
  %16 = load i32, i32* %15
  %17 = load i32, i32* %i
  %18 = load i8*, i8** %2
  %19 = getelementptr i8, i8* %18, i32 4
  %20 = bitcast i8* %19 to i32*
  %21 = getelementptr i32, i32* %20, i32 %17
  %22 = load i32, i32* %21
  %23 = mul i32 %16, %22
  %24 = add i32 %10, %23
  store i32 %24, i32* %res
  %25 = load i32, i32* %i
  %26 = add i32 %25, 1
  store i32 %26, i32* %i
  br label %3

; <label>:27
  %28 = load i32, i32* %res
  ret i32 %28
}

define i32 @main() {
  %a = alloca i8*
  %i = alloca i32
  %b = alloca i8*
  %x = alloca i32
  %.t = alloca i32
  %x1 = alloca i32
  %.t1 = alloca i32
  %1 = mul i32 5, 4
  %2 = add i32 %1, 4
  %3 = call i8* @malloc(i32 %2)
  call void @llvm.memset.p0i8.i32(i8* %3, i8 0, i32 %2, i32 1, i1 false)
  %4 = bitcast i8* %3 to i32*
  store i32 5, i32* %4
  store i8* %3, i8** %a
  store i32 0, i32* %i
  br label %5

; <label>:5
  %6 = load i32, i32* %i
  %7 = load i8*, i8** %a
  %8 = bitcast i8* %7 to i32*
  %9 = load i32, i32* %8
  %10 = icmp slt i32 %6, %9
  br i1 %10, label %11, label %20

; <label>:11
  %12 = load i32, i32* %i
  %13 = load i8*, i8** %a
  %14 = getelementptr i8, i8* %13, i32 4
  %15 = bitcast i8* %14 to i32*
  %16 = getelementptr i32, i32* %15, i32 %12
  %17 = load i32, i32* %i
  store i32 %17, i32* %16
  %18 = load i32, i32* %i
  %19 = add i32 %18, 1
  store i32 %19, i32* %i
  br label %5

; <label>:20
  %21 = load i8*, i8** %a
  call void @shiftLeft(i8* %21)
  %22 = load i8*, i8** %a
  %23 = call i8* @doubleArray(i8* %22)
  store i8* %23, i8** %b
  store i32 0, i32* %x
  store i32 0, i32* %.t
  br label %24

; <label>:24
  %25 = load i32, i32* %.t
  %26 = load i8*, i8** %a
  %27 = bitcast i8* %26 to i32*
  %28 = load i32, i32* %27
  %29 = icmp slt i32 %25, %28
  br i1 %29, label %30, label %40

; <label>:30
  %31 = load i32, i32* %.t
  %32 = load i8*, i8** %a
  %33 = getelementptr i8, i8* %32, i32 4
  %34 = bitcast i8* %33 to i32*
  %35 = getelementptr i32, i32* %34, i32 %31
  %36 = load i32, i32* %35
  store i32 %36, i32* %x
  %37 = load i32, i32* %x
  call void @printInt(i32 %37)
  %38 = load i32, i32* %.t
  %39 = add i32 %38, 1
  store i32 %39, i32* %.t
  br label %24

; <label>:40
  store i32 0, i32* %x1
  store i32 0, i32* %.t1
  br label %41

; <label>:41
  %42 = load i32, i32* %.t1
  %43 = load i8*, i8** %b
  %44 = bitcast i8* %43 to i32*
  %45 = load i32, i32* %44
  %46 = icmp slt i32 %42, %45
  br i1 %46, label %47, label %57

; <label>:47
  %48 = load i32, i32* %.t1
  %49 = load i8*, i8** %b
  %50 = getelementptr i8, i8* %49, i32 4
  %51 = bitcast i8* %50 to i32*
  %52 = getelementptr i32, i32* %51, i32 %48
  %53 = load i32, i32* %52
  store i32 %53, i32* %x1
  %54 = load i32, i32* %x1
  call void @printInt(i32 %54)
  %55 = load i32, i32* %.t1
  %56 = add i32 %55, 1
  store i32 %56, i32* %.t1
  br label %41

; <label>:57
  %58 = load i8*, i8** %a
  %59 = load i8*, i8** %b
  %60 = call i32 @scalProd(i8* %58, i8* %59)
  call void @printInt(i32 %60)
  ret i32 0
}
