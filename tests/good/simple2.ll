declare void @printInt (i32)
declare void @printString (i8*)
declare void @error ()
declare i32 @readInt ()
declare i8* @readString ()
@str.1 = private constant [12 x i8] c"Hello world!"

define i32 @main () {
  %1 = bitcast [12 x i8]* @str.1 to i8*
  call void @printString(i8* %1)
  ret i32 0
}
