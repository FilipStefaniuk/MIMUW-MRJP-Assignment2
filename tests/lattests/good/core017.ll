declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)
declare void @llvm.memset.p0i8.i32(i8*, i8, i32, i1)


@.str = constant [4 x i8] c"apa\00"
@.str.2 = constant [6 x i8] c"false\00"
@.str.1 = constant [5 x i8] c"true\00"

define i32 @main() {
  %x = alloca i32
  store i32 4, i32* %x
  %1 = load i32, i32* %x
  %2 = icmp sle i32 3, %1
  br i1 %2, label %3, label %4

; <label>:3
  br label %4

; <label>:4
  %5 = phi i1 [ false, %0 ], [ true, %3 ]
  br i1 %5, label %6, label %7

; <label>:6
  call void @printBool(i8 1)
  br label %9

; <label>:7
  %8 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
  call void @printString(i8* %8)
  br label %9

; <label>:9
  call void @printBool(i8 1)
  call void @printBool(i8 0)
  %10 = load i32, i32* %x
  %11 = icmp eq i32 4, %10
  br i1 %11, label %12, label %13

; <label>:12
  br label %13

; <label>:13
  %14 = phi i1 [ false, %9 ], [ true, %12 ]
  %15 = zext i1 %14 to i8
  call void @printBool(i8 %15)
  %16 = call i8 @implies(i8 0, i8 0)
  %17 = trunc i8 %16 to i1
  %18 = zext i1 %17 to i8
  call void @printBool(i8 %18)
  %19 = call i8 @implies(i8 0, i8 1)
  %20 = trunc i8 %19 to i1
  %21 = zext i1 %20 to i8
  call void @printBool(i8 %21)
  %22 = call i8 @implies(i8 1, i8 0)
  %23 = trunc i8 %22 to i1
  %24 = zext i1 %23 to i8
  call void @printBool(i8 %24)
  %25 = call i8 @implies(i8 1, i8 1)
  %26 = trunc i8 %25 to i1
  %27 = zext i1 %26 to i8
  call void @printBool(i8 %27)
  ret i32 0
}

define i8 @dontCallMe(i32 %x) {
  %1 = alloca i32
  store i32 %x, i32* %1
  %2 = load i32, i32* %1
  call void @printInt(i32 %2)
  ret i8 1
}

define void @printBool(i8 %b) {
  %1 = alloca i8
  store i8 %b, i8* %1
  %2 = load i8, i8* %1
  %3 = trunc i8 %2 to i1
  br i1 %3, label %4, label %6

; <label>:4
  %5 = getelementptr [5 x i8], [5 x i8]* @.str.1, i32 0, i32 0
  call void @printString(i8* %5)
  br label %8

; <label>:6
  %7 = getelementptr [6 x i8], [6 x i8]* @.str.2, i32 0, i32 0
  call void @printString(i8* %7)
  br label %8

; <label>:8
  ret void
}

define i8 @implies(i8 %x, i8 %y) {
  %1 = alloca i8
  %2 = alloca i8
  store i8 %x, i8* %1
  store i8 %y, i8* %2
  %3 = load i8, i8* %1
  %4 = trunc i8 %3 to i1
  %5 = icmp eq i1 %4, false
  br i1 %5, label %12, label %6

; <label>:6
  %7 = load i8, i8* %1
  %8 = trunc i8 %7 to i1
  %9 = load i8, i8* %2
  %10 = trunc i8 %9 to i1
  %11 = icmp eq i1 %8, %10
  br label %12

; <label>:12
  %13 = phi i1 [ true, %0 ], [ %11, %6 ]
  %14 = zext i1 %13 to i8
  ret i8 %14
}
