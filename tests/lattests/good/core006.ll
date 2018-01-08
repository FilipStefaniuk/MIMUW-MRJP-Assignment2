declare void @printInt (i32)
declare void @printString (i8*)
declare void @error ()
declare i32 @readInt ()
declare i8* @readString ()

define i32 @main () {
  %1 = alloca i32
  %2 = alloca i32
  store i32 0, i32* %1
  store i32 0, i32* %2
  store i32 45, i32* %1
  %3 = mul i32 36, -1
  store i32 %3, i32* %2
  %4 = load i32, i32* %1
  call void @printInt(i32 %4)
  %5 = load i32, i32* %2
  call void @printInt(i32 %5)
  ret i32 0
}
