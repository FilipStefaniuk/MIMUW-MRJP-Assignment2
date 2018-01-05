declare void @printString (i8)
 declare void @error () 
 declare i32 @readInt () 
 declare i8 * @readString () 
 declare i8 * @_appendString () 
 declare i8 * @malloc () 
 define i32 @main () {
  %1 = alloca i32 
 %2 = alloca i32 
 %3 = alloca i32 
 ret i32 0 
}

