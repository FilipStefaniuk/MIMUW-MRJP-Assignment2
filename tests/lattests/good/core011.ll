declare void @printInt (i32)
declare void @printString (i8*)
declare void @error ()
declare i32 @readInt ()
declare i8* @readString ()

define i32 @main () {
  %1 = mul i32 1, -1
  call void @printInt(i32 %1)
  ret i32 0
}