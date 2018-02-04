declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)

define i32 @main() {
	%1 = call i32 @ev(i32 17)
	call void @printInt(i32 %1)
	ret i32 0
}

define i32 @ev(i32 %y) {
	%1 = alloca i32
	store i32 %y, i32* %1
	%2 = load i32, i32* %1
	%3 = icmp sgt i32 %2, 0
	br i1 %3, label %4, label %8

; <label>: 4
	%5 = load i32, i32* %1
	%6 = sub i32 %5, 2
	%7 = call i32 @ev(i32 %6)
	ret i32 %7

; <label>: 8
	%9 = load i32, i32* %1
	%10 = icmp slt i32 %9, 0
	br i1 %10, label %11, label %12

; <label>: 11
	ret i32 0

; <label>: 12
	ret i32 1
}
