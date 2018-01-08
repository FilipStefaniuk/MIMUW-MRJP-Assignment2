declare void @printInt (i32)
declare void @printString (i8*)
declare void @error ()
declare i32 @readInt ()
declare i8* @readString ()

define i32 @main () {
  %1 = alloca i32
  store i32 17, i32* %1
  br label %2

; <label>:2
  %3 = load i32, i32* %1
  %4 = icmp sgt i32 %3, 0
  br i1 %4, label %5, label %8

; <label>:5
  %6 = load i32, i32* %1
  %7 = sub i32 %6, 2
  store i32 %7, i32* %1
  br label %2

; <label>:8
  %9 = load i32, i32* %1
  %10 = icmp slt i32 %9, 0
  br i1 %10, label %11, label %12

; <label>:11
  call void @printInt(i32 0)
  ret i32 0

; <label>:12
  call void @printInt(i32 0)
  ret i32 0
}
