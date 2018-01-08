declare void @printInt (i32)
declare void @printString (i8*)
declare void @error ()
declare i32 @readInt ()
declare i8* @readString ()

define i32 @main () {
  %1 = call i32 @fac(i32 5)
  call void @printInt(i32 %1)
  ret i32 0
}

define i32 @fac (i32 %a) {
  %1 = alloca i32
  %2 = alloca i32
  %3 = alloca i32
  store i32 %a, i32* %1
  store i32 0, i32* %2
  store i32 0, i32* %3
  store i32 1, i32* %2
  %4 = load i32, i32* %1
  store i32 %4, i32* %3
  br label %5

; <label>:5
  %6 = load i32, i32* %3
  %7 = icmp sgt i32 %6, 0
  br i1 %7, label %8, label %14

; <label>:8
  %9 = load i32, i32* %2
  %10 = load i32, i32* %3
  %11 = mul i32 %9, %10
  store i32 %11, i32* %2
  %12 = load i32, i32* %3
  %13 = sub i32 %12, 1
  store i32 %13, i32* %3
  br label %5

; <label>:14
  %15 = load i32, i32* %2
  ret i32 %15
}
