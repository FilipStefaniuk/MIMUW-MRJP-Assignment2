declare void @printInt (i32)
declare void @printString (i8*)
declare void @error ()
declare i32 @readInt ()
declare i8* @readString ()

define i32 @f (i32 %x) {
  %1 = alloca i32
  store i32 %x, i32* %1
  br label %2

; <label>:2
  %3 = load i32, i32* %1
  %4 = icmp slt i32 %3, 10
  br i1 %4, label %5, label %8

; <label>:5
  %6 = load i32, i32* %1
  %7 = add i32 %6, 1
  store i32 %7, i32* %1
  br label %2

; <label>:8
  %9 = load i32, i32* %1
  %10 = mul i32 %9, 10
  store i32 %10, i32* %1
  %11 = load i32, i32* %1
  ret i32 %11
}

define i32 @main () {
  %1 = call i32 @f(i32 0)
  call void @printInt(i32 %1)
  ret i32 0
}
