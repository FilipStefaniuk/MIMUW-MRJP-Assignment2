declare void @printInt (i32)
declare void @printString (i8*)
declare void @error ()
declare i32 @readInt ()
declare i8* @readString ()
@str.1 = private constant [3 x i8] c"foo"

define i32 @main () {
  call void @foo()
  ret i32 0
}

define void @foo () {
  %1 = bitcast [3 x i8]* @str.1 to i8*
  call void @printString(i8* %1)
  ret void
}
