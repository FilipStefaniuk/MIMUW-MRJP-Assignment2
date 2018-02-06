declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)
declare void @llvm.memset.p0i8.i32(i8*, i8, i32, i1)


@.str.2 = constant [2 x i8] c"!\00"
@.str = constant [3 x i8] c"&&\00"
@.str.3 = constant [6 x i8] c"false\00"
@.str.4 = constant [5 x i8] c"true\00"
@.str.1 = constant [3 x i8] c"||\00"

define i32 @main() {
  %1 = getelementptr [3 x i8], [3 x i8]* @.str, i32 0, i32 0
  call void @printString(i8* %1)
  %2 = call i8 @test(i32 -1)
  %3 = trunc i8 %2 to i1
  br i1 %3, label %4, label %7

; <label>:4
  %5 = call i8 @test(i32 0)
  %6 = trunc i8 %5 to i1
  br label %7

; <label>:7
  %8 = phi i1 [ false, %0 ], [ %6, %4 ]
  %9 = zext i1 %8 to i8
  call void @printBool(i8 %9)
  %10 = call i8 @test(i32 -2)
  %11 = trunc i8 %10 to i1
  br i1 %11, label %12, label %15

; <label>:12
  %13 = call i8 @test(i32 1)
  %14 = trunc i8 %13 to i1
  br label %15

; <label>:15
  %16 = phi i1 [ false, %7 ], [ %14, %12 ]
  %17 = zext i1 %16 to i8
  call void @printBool(i8 %17)
  %18 = call i8 @test(i32 3)
  %19 = trunc i8 %18 to i1
  br i1 %19, label %20, label %23

; <label>:20
  %21 = call i8 @test(i32 -5)
  %22 = trunc i8 %21 to i1
  br label %23

; <label>:23
  %24 = phi i1 [ false, %15 ], [ %22, %20 ]
  %25 = zext i1 %24 to i8
  call void @printBool(i8 %25)
  %26 = call i8 @test(i32 234234)
  %27 = trunc i8 %26 to i1
  br i1 %27, label %28, label %31

; <label>:28
  %29 = call i8 @test(i32 21321)
  %30 = trunc i8 %29 to i1
  br label %31

; <label>:31
  %32 = phi i1 [ false, %23 ], [ %30, %28 ]
  %33 = zext i1 %32 to i8
  call void @printBool(i8 %33)
  %34 = getelementptr [3 x i8], [3 x i8]* @.str.1, i32 0, i32 0
  call void @printString(i8* %34)
  %35 = call i8 @test(i32 -1)
  %36 = trunc i8 %35 to i1
  br i1 %36, label %40, label %37

; <label>:37
  %38 = call i8 @test(i32 0)
  %39 = trunc i8 %38 to i1
  br label %40

; <label>:40
  %41 = phi i1 [ true, %31 ], [ %39, %37 ]
  %42 = zext i1 %41 to i8
  call void @printBool(i8 %42)
  %43 = call i8 @test(i32 -2)
  %44 = trunc i8 %43 to i1
  br i1 %44, label %48, label %45

; <label>:45
  %46 = call i8 @test(i32 1)
  %47 = trunc i8 %46 to i1
  br label %48

; <label>:48
  %49 = phi i1 [ true, %40 ], [ %47, %45 ]
  %50 = zext i1 %49 to i8
  call void @printBool(i8 %50)
  %51 = call i8 @test(i32 3)
  %52 = trunc i8 %51 to i1
  br i1 %52, label %56, label %53

; <label>:53
  %54 = call i8 @test(i32 -5)
  %55 = trunc i8 %54 to i1
  br label %56

; <label>:56
  %57 = phi i1 [ true, %48 ], [ %55, %53 ]
  %58 = zext i1 %57 to i8
  call void @printBool(i8 %58)
  %59 = call i8 @test(i32 234234)
  %60 = trunc i8 %59 to i1
  br i1 %60, label %64, label %61

; <label>:61
  %62 = call i8 @test(i32 21321)
  %63 = trunc i8 %62 to i1
  br label %64

; <label>:64
  %65 = phi i1 [ true, %56 ], [ %63, %61 ]
  %66 = zext i1 %65 to i8
  call void @printBool(i8 %66)
  %67 = getelementptr [2 x i8], [2 x i8]* @.str.2, i32 0, i32 0
  call void @printString(i8* %67)
  call void @printBool(i8 1)
  call void @printBool(i8 0)
  ret i32 0
}

define void @printBool(i8 %b) {
  %1 = alloca i8
  store i8 %b, i8* %1
  %2 = load i8, i8* %1
  %3 = trunc i8 %2 to i1
  %4 = icmp eq i1 %3, false
  br i1 %4, label %5, label %7

; <label>:5
  %6 = getelementptr [6 x i8], [6 x i8]* @.str.3, i32 0, i32 0
  call void @printString(i8* %6)
  br label %9

; <label>:7
  %8 = getelementptr [5 x i8], [5 x i8]* @.str.4, i32 0, i32 0
  call void @printString(i8* %8)
  br label %9

; <label>:9
  ret void
}

define i8 @test(i32 %i) {
  %1 = alloca i32
  store i32 %i, i32* %1
  %2 = load i32, i32* %1
  call void @printInt(i32 %2)
  %3 = load i32, i32* %1
  %4 = icmp sgt i32 %3, 0
  %5 = zext i1 %4 to i8
  ret i8 %5
}
