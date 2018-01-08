declare void @printInt (i32)
declare void @printString (i8*)
declare void @error ()
declare i32 @readInt ()
declare i8* @readString ()

define i32 @main () {
  %1 = alloca i32
  %2 = alloca i32
  %3 = alloca i32
  store i32 0, i32* %1
  store i32 0, i32* %2
  store i32 0, i32* %3
  store i32 1, i32* %1
  %4 = load i32, i32* %1
  store i32 %4, i32* %2
  store i32 5000000, i32* %3
  %5 = load i32, i32* %1
  call void @printInt(i32 %5)
  br label %6

; <label>:6
  %7 = load i32, i32* %2
  %8 = load i32, i32* %3
  %9 = icmp slt i32 %7, %8
  br i1 %9, label %10, label %18

; <label>:10
  %11 = load i32, i32* %2
  call void @printInt(i32 %11)
  %12 = load i32, i32* %1
  %13 = load i32, i32* %2
  %14 = add i32 %12, %13
  store i32 %14, i32* %2
  %15 = load i32, i32* %2
  %16 = load i32, i32* %1
  %17 = sub i32 %15, %16
  store i32 %17, i32* %1
  br label %6

; <label>:18
  ret i32 0
}
