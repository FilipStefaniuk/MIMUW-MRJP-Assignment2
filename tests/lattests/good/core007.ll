declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)

define i32 @main() {
	%x = alloca i32
	store i32 7, i32* %x
	%1 = load i32, i32* %x
	call void @printInt(i32 %1)
	ret i32 0
}
