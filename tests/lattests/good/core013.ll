declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)
@.str.2 = constant [2 x i8] c"!\00"
@.str = constant [3 x i8] c"&&\00"
@.str.3 = constant [6 x i8] c"false\00"
@.str.4 = constant [5 x i8] c"true\00"
@.str.1 = constant [3 x i8] c"||\00"

define i32 @main() {
	%1 = getelementptr inbounds [3 x i8], [3 x i8]* @.str, i32 0, i32 0
	call void @printString(i8* %1)
	%2 = call i1 @test(i32 -1)
	br i1 %2, label %3, label %5

; <label>: 3
	%4 = call i1 @test(i32 0)
	br label %5

; <label>: 5
	%6 = phi i1 [ false, %0 ], [ %4, %3 ]
	call void @printBool(i1 %6)
	%7 = call i1 @test(i32 -2)
	br i1 %7, label %8, label %10

; <label>: 8
	%9 = call i1 @test(i32 1)
	br label %10

; <label>: 10
	%11 = phi i1 [ false, %5 ], [ %9, %8 ]
	call void @printBool(i1 %11)
	%12 = call i1 @test(i32 3)
	br i1 %12, label %13, label %15

; <label>: 13
	%14 = call i1 @test(i32 -5)
	br label %15

; <label>: 15
	%16 = phi i1 [ false, %10 ], [ %14, %13 ]
	call void @printBool(i1 %16)
	%17 = call i1 @test(i32 234234)
	br i1 %17, label %18, label %20

; <label>: 18
	%19 = call i1 @test(i32 21321)
	br label %20

; <label>: 20
	%21 = phi i1 [ false, %15 ], [ %19, %18 ]
	call void @printBool(i1 %21)
	%22 = getelementptr inbounds [3 x i8], [3 x i8]* @.str.1, i32 0, i32 0
	call void @printString(i8* %22)
	%23 = call i1 @test(i32 -1)
	br i1 %23, label %26, label %24

; <label>: 24
	%25 = call i1 @test(i32 0)
	br label %26

; <label>: 26
	%27 = phi i1 [ true, %20 ], [ %25, %24 ]
	call void @printBool(i1 %27)
	%28 = call i1 @test(i32 -2)
	br i1 %28, label %31, label %29

; <label>: 29
	%30 = call i1 @test(i32 1)
	br label %31

; <label>: 31
	%32 = phi i1 [ true, %26 ], [ %30, %29 ]
	call void @printBool(i1 %32)
	%33 = call i1 @test(i32 3)
	br i1 %33, label %36, label %34

; <label>: 34
	%35 = call i1 @test(i32 -5)
	br label %36

; <label>: 36
	%37 = phi i1 [ true, %31 ], [ %35, %34 ]
	call void @printBool(i1 %37)
	%38 = call i1 @test(i32 234234)
	br i1 %38, label %41, label %39

; <label>: 39
	%40 = call i1 @test(i32 21321)
	br label %41

; <label>: 41
	%42 = phi i1 [ true, %36 ], [ %40, %39 ]
	call void @printBool(i1 %42)
	%43 = getelementptr inbounds [2 x i8], [2 x i8]* @.str.2, i32 0, i32 0
	call void @printString(i8* %43)
	call void @printBool(i1 true)
	call void @printBool(i1 false)
	ret i32 0
}

define void @printBool(i1 %b) {
	%1 = alloca i1
	store i1 %b, i1* %1
	%2 = load i1, i1* %1
	%3 = icmp eq i1 %2, false
	br i1 %3, label %4, label %6

; <label>: 4
	%5 = getelementptr inbounds [6 x i8], [6 x i8]* @.str.3, i32 0, i32 0
	call void @printString(i8* %5)
	br label %8

; <label>: 6
	%7 = getelementptr inbounds [5 x i8], [5 x i8]* @.str.4, i32 0, i32 0
	call void @printString(i8* %7)
	br label %8

; <label>: 8
	ret void
}

define i1 @test(i32 %i) {
	%1 = alloca i32
	store i32 %i, i32* %1
	%2 = load i32, i32* %1
	call void @printInt(i32 %2)
	%3 = load i32, i32* %1
	%4 = icmp sgt i32 %3, 0
	ret i1 %4
}
