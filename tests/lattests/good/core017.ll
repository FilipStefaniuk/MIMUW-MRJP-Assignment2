declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)
@.str = constant [4 x i8] c"apa\00"
@.str.2 = constant [6 x i8] c"false\00"
@.str.1 = constant [5 x i8] c"true\00"

define i32 @main() {
	%x = alloca i32
	store i32 4, i32* %x
	%1 = load i32, i32* %x
	%2 = icmp sle i32 3, %1
	br i1 %2, label %3, label %4

; <label>: 3
	br label %4

; <label>: 4
	%5 = phi i1 [ false, %0 ], [ true, %3 ]
	br i1 %5, label %6, label %7

; <label>: 6
	call void @printBool(i1 true)
	br label %9

; <label>: 7
	%8 = getelementptr inbounds [4 x i8], [4 x i8]* @.str, i32 0, i32 0
	call void @printString(i8* %8)
	br label %9

; <label>: 9
	call void @printBool(i1 true)
	call void @printBool(i1 false)
	%10 = load i32, i32* %x
	%11 = icmp eq i32 4, %10
	br i1 %11, label %12, label %13

; <label>: 12
	br label %13

; <label>: 13
	%14 = phi i1 [ false, %9 ], [ true, %12 ]
	call void @printBool(i1 %14)
	%15 = call i1 @implies(i1 false, i1 false)
	call void @printBool(i1 %15)
	%16 = call i1 @implies(i1 false, i1 true)
	call void @printBool(i1 %16)
	%17 = call i1 @implies(i1 true, i1 false)
	call void @printBool(i1 %17)
	%18 = call i1 @implies(i1 true, i1 true)
	call void @printBool(i1 %18)
	ret i32 0
}

define i1 @dontCallMe(i32 %x) {
	%1 = alloca i32
	store i32 %x, i32* %1
	%2 = load i32, i32* %1
	call void @printInt(i32 %2)
	ret i1 true
}

define void @printBool(i1 %b) {
	%1 = alloca i1
	store i1 %b, i1* %1
	%2 = load i1, i1* %1
	br i1 %2, label %3, label %5

; <label>: 3
	%4 = getelementptr inbounds [5 x i8], [5 x i8]* @.str.1, i32 0, i32 0
	call void @printString(i8* %4)
	br label %7

; <label>: 5
	%6 = getelementptr inbounds [6 x i8], [6 x i8]* @.str.2, i32 0, i32 0
	call void @printString(i8* %6)
	br label %7

; <label>: 7
	ret void
}

define i1 @implies(i1 %x, i1 %y) {
	%1 = alloca i1
	%2 = alloca i1
	store i1 %x, i1* %1
	store i1 %y, i1* %2
	%3 = load i1, i1* %1
	%4 = icmp eq i1 %3, false
	br i1 %4, label %9, label %5

; <label>: 5
	%6 = load i1, i1* %1
	%7 = load i1, i1* %2
	%8 = icmp eq i1 %6, %7
	br label %9

; <label>: 9
	%10 = phi i1 [ true, %0 ], [ %8, %5 ]
	ret i1 %10
}
