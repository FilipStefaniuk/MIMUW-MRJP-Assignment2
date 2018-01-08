declare void @printInt (i32)
declare void @printString (i8*)
declare void @error ()
declare i32 @readInt ()
declare i8* @readString ()

define i32 @main () {
  %1 = icmp eq i1 1, 1
  br i1 %1, label %2, label %3

; <label>:2
  call void @printInt(i32 42)
  br label %4

; <label>:3
  call void @printInt(i32 42)
  br label %4

; <label>:4
  ret i32 0
}
