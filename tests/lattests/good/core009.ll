declare void @printInt (i32)
declare void @printString (i8*)
declare void @error ()
declare i32 @readInt ()
declare i8* @readString ()

define i32 @main () {
  %1 = alloca i32
  %2 = call i32 @foo()
  store i32 %2, i32* %1
  %3 = load i32, i32* %1
  call void @printInt(i32 %3)
  ret i32 0
}

define i32 @foo () {
  ret i32 10
}
