declare void @printInt (i32)
declare void @printString (i8*)
declare void @error ()
declare i32 @readInt ()
declare i8* @readString ()
@str.1 = private constant [4 x i8] c"true"
@str.2 = private constant [4 x i8] c"true"

define i32 @main () {
  %1 = alloca i32
  store i32 4, i32* %1
  %2 = load i32, i32* %1
  %3 = icmp sle i32 3, %2
  %4 = icmp ne i32 4, 2
  %5 = and i1 %4, 1
  %6 = and i1 %3, %5
  br i1 %6, label %7, label %8

; <label>:7
  call void @printBool(i1 1)
  br label %9

; <label>:8
  call void @printBool(i1 1)
  br label %9

; <label>:9
  %10 = icmp eq i1 1, 1
  %11 = call i1 @dontCallMe(i32 1)
  %12 = or i1 %10, %11
  call void @printBool(i1 %12)
  %13 = mul i32 5, -1
  %14 = icmp slt i32 4, %13
  %15 = call i1 @dontCallMe(i32 2)
  %16 = and i1 %14, %15
  call void @printBool(i1 %16)
  %17 = load i32, i32* %1
  %18 = icmp eq i32 4, %17
  %19 = add i1 0, 1
  %20 = srem i32 %19, 2
  %21 = icmp eq i1 1, %20
  %22 = and i1 %21, 1
  %23 = and i1 %18, %22
  call void @printBool(i1 %23)
  %24 = call i1 @implies(i1 0, i1 0)
  call void @printBool(i1 %24)
  %25 = call i1 @implies(i1 0, i1 1)
  call void @printBool(i1 %25)
  %26 = call i1 @implies(i1 1, i1 0)
  call void @printBool(i1 %26)
  %27 = call i1 @implies(i1 1, i1 1)
  call void @printBool(i1 %27)
  ret i32 0
}

define i1 @dontCallMe (i32 %x) {
  %1 = alloca i32
  store i32 %x, i32* %1
  %2 = load i32, i32* %1
  call void @printInt(i32 %2)
  ret i1 1
}

define void @printBool (i1 %b) {
  %1 = alloca i1
  store i1 %b, i1* %1
  %2 = load i1, i1* %1
  br i1 %2, label %3, label %5

; <label>:3
  %4 = bitcast [4 x i8]* @str.1 to i8*
  call void @printString(i8* %4)
  br label %7

; <label>:5
  %6 = bitcast [4 x i8]* @str.2 to i8*
  call void @printString(i8* %6)
  br label %7

; <label>:7
  ret void
}

define i1 @implies (i1 %x, i1 %y) {
  %1 = alloca i1
  %2 = alloca i1
  store i1 %x, i1* %1
  store i1 %y, i1* %2
  %3 = load i1, i1* %1
  %4 = add i1 %3, 1
  %5 = srem i32 %4, 2
  %6 = load i1, i1* %1
  %7 = load i1, i1* %2
  %8 = icmp eq i1 %6, %7
  %9 = or i1 %5, %8
  ret i1 %9
}
