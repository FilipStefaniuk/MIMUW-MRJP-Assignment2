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
  %y = alloca i8*
  %z = alloca i8*
  %1 = call i32 @readInt()
  store i32 %1, i32* %x
  %2 = call i8* @readString()
  store i8* %2, i8** %y
  %3 = call i8* @readString()
  store i8* %3, i8** %z
  %4 = load i32, i32* %x
  %5 = sub i32 %4, 5
  call void @printInt(i32 %5)
  %6 = load i8*, i8** %y
  %7 = load i8*, i8** %z
  %8 = call i8* @concat(i8* %6, i8* %7)
  call void @printString(i8* %8)
  ret i32 0
}
