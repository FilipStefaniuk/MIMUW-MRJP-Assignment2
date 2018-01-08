declare void @printInt (i32)
declare void @printString (i8*)
declare void @error ()
declare i32 @readInt ()
declare i8* @readString ()

define i32 @main () {
  %1 = alloca i32
  %2 = alloca i32
  %3 = alloca i32
  %4 = alloca i32
  %5 = alloca i32
  store i32 78, i32* %1
  store i32 1, i32* %2
  %6 = load i32, i32* %2
  call void @printInt(i32 %6)
  %7 = load i32, i32* %2
  call void @printInt(i32 %7)
  br label %8

; <label>:8
  %9 = load i32, i32* %2
  %10 = icmp sgt i32 %9, 76
  br i1 %10, label %11, label %18

; <label>:11
  %12 = load i32, i32* %2
  %13 = sub i32 %12, 1
  store i32 %13, i32* %2
  %14 = load i32, i32* %2
  call void @printInt(i32 %14)
  %15 = load i32, i32* %2
  %16 = add i32 %15, 7
  store i32 %16, i32* %3
  %17 = load i32, i32* %3
  call void @printInt(i32 %17)
  br label %8

; <label>:18
  %19 = load i32, i32* %2
  call void @printInt(i32 %19)
  %20 = load i32, i32* %2
  %21 = icmp sgt i32 %20, 4
  br i1 %21, label %22, label %24

; <label>:22
  store i32 4, i32* %4
  %23 = load i32, i32* %4
  call void @printInt(i32 %23)
  br label %26

; <label>:24
  store i32 4, i32* %5
  %25 = load i32, i32* %5
  call void @printInt(i32 %25)
  br label %26

; <label>:26
  %27 = load i32, i32* %2
  call void @printInt(i32 %27)
  ret i32 0
}
