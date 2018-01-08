declare void @printInt (i32)
declare void @printString (i8*)
declare void @error ()
declare i32 @readInt ()
declare i8* @readString ()

define i32 @main () {
  %1 = alloca i32
  store i32 0, i32* %1
  %2 = load i32, i32* %1
  call void @printInt(i32 %2)
  ret i32 0
}
