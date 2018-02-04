declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)

define i32 @main() {
	%x = alloca i32*
	%y = alloca i32
	%1 = mul i32 20, 4
	%2 = call i8* @malloc(i32 %1)
	%3 = bitcast i8* %2 to i32*
	store i32* %3, i32** %x
	store i32 2, i32* %y
	%4 = load i32, i32* %y
	%5 = load i32*, i32** %x
	%6 = getelementptr inbounds i32, i32* %5, i32 %4
	store i32 20, i32* %6
	%7 = load i32, i32* %y
	%8 = load i32*, i32** %x
	%9 = getelementptr inbounds i32, i32* %8, i32 %7
	%10 = load i32, i32* %9
	call void @printInt(i32 %10)
	ret i32 0
}
