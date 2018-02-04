declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)

define i32 @main() {
	%x = alloca i32
	%y = alloca i32
	store i32 0, i32* %x
	store i32 56, i32* %y
	%1 = load i32, i32* %y
	%2 = add i32 %1, 45
	%3 = icmp sle i32 %2, 2
	br i1 %3, label %4, label %5

; <label>: 4
	store i32 1, i32* %x
	br label %6

; <label>: 5
	store i32 2, i32* %x
	br label %6

; <label>: 6
	%7 = load i32, i32* %x
	call void @printInt(i32 %7)
	ret i32 0
}
