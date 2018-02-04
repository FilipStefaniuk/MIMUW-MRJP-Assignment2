declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)
%class.A = type { i32, i32 }
%class.C = type { %class.A, i32, i32 }


define i32 @main() {
	%a = alloca %class.C*
	%1 = call i8* @malloc(i32 16)
	%2 = bitcast i8* %1 to %class.C*
	store %class.C* %2, %class.C** %a
	%3 = load %class.C*, %class.C** %a
	%4 = bitcast %class.C* %3 to %class.A*
	%5 = getelementptr inbounds %class.A, %class.A* %4, i32 0, i32 0
	store i32 12, i32* %5
	%6 = load %class.C*, %class.C** %a
	%7 = bitcast %class.C* %6 to %class.A*
	%8 = getelementptr inbounds %class.A, %class.A* %7, i32 0, i32 0
	%9 = load i32, i32* %8
	call void @printInt(i32 %9)
	ret i32 0
}
