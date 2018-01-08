declare void @printInt (i32)
declare void @printString (i8*)
declare void @error ()
declare i32 @readInt ()
declare i8* @readString ()
@str.1 = private constant [3 x i8] c"&&\00"
@str.2 = private constant [3 x i8] c"||\00"
@str.3 = private constant [2 x i8] c"!\00"
@str.4 = private constant [6 x i8] c"false\00"
@str.5 = private constant [6 x i8] c"false\00"

define i32 @main () {
  %1 = bitcast [3 x i8]* @str.1 to i8*
  call void @printString(i8* %1)
  %2 = mul i32 1, -1
  %3 = call i1 @test(i32 %2)
  %4 = call i1 @test(i32 0)
  %5 = and i1 %3, %4
  call void @printBool(i1 %5)
  %6 = mul i32 2, -1
  %7 = call i1 @test(i32 %6)
  %8 = call i1 @test(i32 1)
  %9 = and i1 %7, %8
  call void @printBool(i1 %9)
  %10 = call i1 @test(i32 3)
  %11 = mul i32 5, -1
  %12 = call i1 @test(i32 %11)
  %13 = and i1 %10, %12
  call void @printBool(i1 %13)
  %14 = call i1 @test(i32 234234)
  %15 = call i1 @test(i32 21321)
  %16 = and i1 %14, %15
  call void @printBool(i1 %16)
  %17 = bitcast [3 x i8]* @str.2 to i8*
  call void @printString(i8* %17)
  %18 = mul i32 1, -1
  %19 = call i1 @test(i32 %18)
  %20 = call i1 @test(i32 0)
  %21 = or i1 %19, %20
  call void @printBool(i1 %21)
  %22 = mul i32 2, -1
  %23 = call i1 @test(i32 %22)
  %24 = call i1 @test(i32 1)
  %25 = or i1 %23, %24
  call void @printBool(i1 %25)
  %26 = call i1 @test(i32 3)
  %27 = mul i32 5, -1
  %28 = call i1 @test(i32 %27)
  %29 = or i1 %26, %28
  call void @printBool(i1 %29)
  %30 = call i1 @test(i32 234234)
  %31 = call i1 @test(i32 21321)
  %32 = or i1 %30, %31
  call void @printBool(i1 %32)
  %33 = bitcast [2 x i8]* @str.3 to i8*
  call void @printString(i8* %33)
  call void @printBool(i1 1)
  call void @printBool(i1 0)
  ret i32 0
}

define void @printBool (i1 %b) {
  %1 = alloca i1
  store i1 %b, i1* %1
  %2 = load i1, i1* %1
  %3 = icmp eq i1 %2, 0
  br i1 %3, label %4, label %6

; <label>:4
  %5 = bitcast [6 x i8]* @str.4 to i8*
  call void @printString(i8* %5)
  br label %8

; <label>:6
  %7 = bitcast [6 x i8]* @str.5 to i8*
  call void @printString(i8* %7)
  br label %8

; <label>:8
  ret void
}

define i1 @test (i32 %i) {
  %1 = alloca i32
  store i32 %i, i32* %1
  %2 = load i32, i32* %1
  call void @printInt(i32 %2)
  %3 = load i32, i32* %1
  %4 = icmp sgt i32 %3, 0
  ret i1 %4
}
