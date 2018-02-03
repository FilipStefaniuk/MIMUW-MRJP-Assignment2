declare void @printInt(i32)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @_appendString(i8*, i8*)
declare i8* @malloc(i32)
@.str = constant [10 x i8] c"Ala ma "
@.str.1 = constant [8 x i8] c"kota."

define i32 @main() {
	%1 = alloca i8
	%2 = alloca i8
	%3 = getelementptr [10 x i8], [10 x i8]*
	store i8* %3, i8* %1
	%4 = getelementptr [8 x i8], [8 x i8]*
	store i8* %4, i8* %2
	ret i32 0
}
