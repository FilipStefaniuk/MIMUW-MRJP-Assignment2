declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)
declare void @llvm.memset.p0i8.i32(i8*, i8, i32, i32, i1)


@.str.3 = constant [1 x i8] c"\00"
@.str.2 = constant [9 x i8] c"/* world\00"
@.str = constant [2 x i8] c"=\00"
@.str.1 = constant [9 x i8] c"hello */\00"

define i32 @main() {
  %r = alloca i8*
  %n = alloca i32
  %r1 = alloca i32
  %1 = call i32 @fac(i32 10)
  call void @printInt(i32 %1)
  %2 = call i32 @rfac(i32 10)
  call void @printInt(i32 %2)
  %3 = call i32 @mfac(i32 10)
  call void @printInt(i32 %3)
  %4 = call i32 @ifac(i32 10)
  call void @printInt(i32 %4)
  store i8* null, i8** %r
  store i32 10, i32* %n
  store i32 1, i32* %r1
  br label %5

; <label>:5
  %6 = load i32, i32* %n
  %7 = icmp sgt i32 %6, 0
  br i1 %7, label %8, label %14

; <label>:8
  %9 = load i32, i32* %r1
  %10 = load i32, i32* %n
  %11 = mul i32 %9, %10
  store i32 %11, i32* %r1
  %12 = load i32, i32* %n
  %13 = sub i32 %12, 1
  store i32 %13, i32* %n
  br label %5

; <label>:14
  %15 = load i32, i32* %r1
  call void @printInt(i32 %15)
  %16 = getelementptr [2 x i8], [2 x i8]* @.str, i32 0, i32 0
  %17 = call i8* @repStr(i8* %16, i32 60)
  call void @printString(i8* %17)
  %18 = getelementptr [9 x i8], [9 x i8]* @.str.1, i32 0, i32 0
  call void @printString(i8* %18)
  %19 = getelementptr [9 x i8], [9 x i8]* @.str.2, i32 0, i32 0
  call void @printString(i8* %19)
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

define i32 @rfac(i32 %n) {
  %1 = alloca i32
  store i32 %n, i32* %1
  %2 = load i32, i32* %1
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %4, label %5

; <label>:4
  ret i32 1

; <label>:5
  %6 = load i32, i32* %1
  %7 = load i32, i32* %1
  %8 = sub i32 %7, 1
  %9 = call i32 @rfac(i32 %8)
  %10 = mul i32 %6, %9
  ret i32 %10
}

define i32 @mfac(i32 %n) {
  %1 = alloca i32
  store i32 %n, i32* %1
  %2 = load i32, i32* %1
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %4, label %5

; <label>:4
  ret i32 1

; <label>:5
  %6 = load i32, i32* %1
  %7 = load i32, i32* %1
  %8 = sub i32 %7, 1
  %9 = call i32 @nfac(i32 %8)
  %10 = mul i32 %6, %9
  ret i32 %10
}

define i32 @nfac(i32 %n) {
  %1 = alloca i32
  store i32 %n, i32* %1
  %2 = load i32, i32* %1
  %3 = icmp ne i32 %2, 0
  br i1 %3, label %4, label %10

; <label>:4
  %5 = load i32, i32* %1
  %6 = sub i32 %5, 1
  %7 = call i32 @mfac(i32 %6)
  %8 = load i32, i32* %1
  %9 = mul i32 %7, %8
  ret i32 %9

; <label>:10
  ret i32 1
}

define i32 @ifac(i32 %n) {
  %1 = alloca i32
  store i32 %n, i32* %1
  %2 = load i32, i32* %1
  %3 = call i32 @ifac2f(i32 1, i32 %2)
  ret i32 %3
}

define i32 @ifac2f(i32 %l, i32 %h) {
  %1 = alloca i32
  %2 = alloca i32
  %m = alloca i32
  store i32 %l, i32* %1
  store i32 %h, i32* %2
  %3 = load i32, i32* %1
  %4 = load i32, i32* %2
  %5 = icmp eq i32 %3, %4
  br i1 %5, label %6, label %8

; <label>:6
  %7 = load i32, i32* %1
  ret i32 %7

; <label>:8
  %9 = load i32, i32* %1
  %10 = load i32, i32* %2
  %11 = icmp sgt i32 %9, %10
  br i1 %11, label %12, label %13

; <label>:12
  ret i32 1

; <label>:13
  store i32 0, i32* %m
  %14 = load i32, i32* %1
  %15 = load i32, i32* %2
  %16 = add i32 %14, %15
  %17 = sdiv i32 %16, 2
  store i32 %17, i32* %m
  %18 = load i32, i32* %1
  %19 = load i32, i32* %m
  %20 = call i32 @ifac2f(i32 %18, i32 %19)
  %21 = load i32, i32* %m
  %22 = add i32 %21, 1
  %23 = load i32, i32* %2
  %24 = call i32 @ifac2f(i32 %22, i32 %23)
  %25 = mul i32 %20, %24
  ret i32 %25
}

define i8* @repStr(i8* %s, i32 %n) {
  %1 = alloca i8*
  %2 = alloca i32
  %r = alloca i8*
  %i = alloca i32
  store i8* %s, i8** %1
  store i32 %n, i32* %2
  %3 = getelementptr [1 x i8], [1 x i8]* @.str.3, i32 0, i32 0
  store i8* %3, i8** %r
  store i32 0, i32* %i
  br label %4

; <label>:4
  %5 = load i32, i32* %i
  %6 = load i32, i32* %2
  %7 = icmp slt i32 %5, %6
  br i1 %7, label %8, label %14

; <label>:8
  %9 = load i8*, i8** %r
  %10 = load i8*, i8** %1
  %11 = call i8* @concat(i8* %9, i8* %10)
  store i8* %11, i8** %r
  %12 = load i32, i32* %i
  %13 = add i32 %12, 1
  store i32 %13, i32* %i
  br label %4

; <label>:14
  %15 = load i8*, i8** %r
  ret i8* %15
}
