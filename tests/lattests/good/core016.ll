declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)

define i32 @main() {
	%y = alloca i32
	store i32 17, i32* %y
	br label %1

; <label>: 1
	%2 = load i32, i32* %y
	%3 = icmp sgt i32 %2, 0
	br i1 %3, label %4, label %7

; <label>: 4
	%5 = load i32, i32* %y
	%6 = sub i32 %5, 2
	store i32 %6, i32* %y
	br label %1

; <label>: 7
	%8 = load i32, i32* %y
	%9 = icmp slt i32 %8, 0
	br i1 %9, label %10, label %11

; <label>: 10
	call void @printInt(i32 0)
	ret i32 0

; <label>: 11
	call void @printInt(i32 1)
	ret i32 0

; <label>: 12

}
