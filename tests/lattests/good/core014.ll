declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)

define i32 @main() {
	%lo = alloca i32
	%hi = alloca i32
	%mx = alloca i32
	store i32 0, i32* %lo
	store i32 0, i32* %hi
	store i32 0, i32* %mx
	store i32 1, i32* %lo
	%1 = load i32, i32* %lo
	store i32 %1, i32* %hi
	store i32 5000000, i32* %mx
	%2 = load i32, i32* %lo
	call void @printInt(i32 %2)
	br label %3

; <label>: 3
	%4 = load i32, i32* %hi
	%5 = load i32, i32* %mx
	%6 = icmp slt i32 %4, %5
	br i1 %6, label %7, label %15

; <label>: 7
	%8 = load i32, i32* %hi
	call void @printInt(i32 %8)
	%9 = load i32, i32* %lo
	%10 = load i32, i32* %hi
	%11 = add i32 %9, %10
	store i32 %11, i32* %hi
	%12 = load i32, i32* %hi
	%13 = load i32, i32* %lo
	%14 = sub i32 %12, %13
	store i32 %14, i32* %lo
	br label %3

; <label>: 15
	ret i32 0
}
