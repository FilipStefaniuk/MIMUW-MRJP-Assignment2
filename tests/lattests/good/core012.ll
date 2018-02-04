declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)
@.str.1 = constant [2 x i8] c" \00"
@.str.2 = constant [14 x i8] c"concatenation\00"
@.str.4 = constant [6 x i8] c"false\00"
@.str = constant [7 x i8] c"string\00"
@.str.3 = constant [5 x i8] c"true\00"

define i32 @main() {
	%x = alloca i32
	%y = alloca i32
	store i32 56, i32* %x
	store i32 -23, i32* %y
	%1 = load i32, i32* %x
	%2 = load i32, i32* %y
	%3 = add i32 %1, %2
	call void @printInt(i32 %3)
	%4 = load i32, i32* %x
	%5 = load i32, i32* %y
	%6 = sub i32 %4, %5
	call void @printInt(i32 %6)
	%7 = load i32, i32* %x
	%8 = load i32, i32* %y
	%9 = mul i32 %7, %8
	call void @printInt(i32 %9)
	call void @printInt(i32 22)
	call void @printInt(i32 0)
	%10 = load i32, i32* %x
	%11 = load i32, i32* %y
	%12 = sub i32 %10, %11
	%13 = load i32, i32* %x
	%14 = load i32, i32* %y
	%15 = add i32 %13, %14
	%16 = icmp sgt i32 %12, %15
	call void @printBool(i1 %16)
	%17 = load i32, i32* %x
	%18 = load i32, i32* %y
	%19 = sdiv i32 %17, %18
	%20 = load i32, i32* %x
	%21 = load i32, i32* %y
	%22 = mul i32 %20, %21
	%23 = icmp sle i32 %19, %22
	call void @printBool(i1 %23)
	%24 = getelementptr inbounds [7 x i8], [7 x i8]* @.str, i32 0, i32 0
	%25 = getelementptr inbounds [2 x i8], [2 x i8]* @.str.1, i32 0, i32 0
	%26 = call i8* @concat(i8* %24, i8* %25)
	%27 = getelementptr inbounds [14 x i8], [14 x i8]* @.str.2, i32 0, i32 0
	%28 = call i8* @concat(i8* %26, i8* %27)
	call void @printString(i8* %28)
	ret i32 0
}

define void @printBool(i1 %b) {
	%1 = alloca i1
	store i1 %b, i1* %1
	%2 = load i1, i1* %1
	br i1 %2, label %3, label %5

; <label>: 3
	%4 = getelementptr inbounds [5 x i8], [5 x i8]* @.str.3, i32 0, i32 0
	call void @printString(i8* %4)
	ret void

; <label>: 5
	%6 = getelementptr inbounds [6 x i8], [6 x i8]* @.str.4, i32 0, i32 0
	call void @printString(i8* %6)
	ret void

; <label>: 7
	ret void
}
