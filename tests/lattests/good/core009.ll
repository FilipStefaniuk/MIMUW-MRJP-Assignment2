declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)
declare void @llvm.memset.p0i8.i32(i8*, i8, i32, i1)



define i32 @main() {
  %x = alloca i32
  %1 = call i32 @foo()
  store i32 %1, i32* %x
  %2 = load i32, i32* %x
  call void @printInt(i32 %2)
  ret i32 0
}

define i32 @foo() {
  ret i32 10
}
