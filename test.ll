declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)
%class.A = type {  }
%class.B = type { %class.A }


define i32 @A.f(%class.A* %this) {
	%1 = alloca %class.A*
	store %class.A* %this, %class.A** %1
	call void @printInt(i32 10)
	ret i32 0
}

define i32 @B.g(%class.B* %this) {
	%1 = alloca %class.B*
	store %class.B* %this, %class.B** %1
	%2 = load %class.B*, %class.B** %1
	%3 = bitcast %class.B* %2 to %class.A*
	%4 = call i32 @A.f(%class.A* %3)
	ret i32 0
}

define i32 @f() {
	call void @printInt(i32 30)
	ret i32 0
}

define i32 @main() {
	%b = alloca %class.B*
	%1 = call i8* @malloc(i32 0)
	%2 = bitcast i8* %1 to %class.B*
	store %class.B* %2, %class.B** %b
	%3 = load %class.B*, %class.B** %b
	%4 = call i32 @B.g(%class.B* %3)
	ret i32 0
}
