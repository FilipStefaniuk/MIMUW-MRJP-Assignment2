declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)
declare void @llvm.memset.p0i8.i32(i8*, i8, i32, i32, i1)

%class.list = type { i32, %class.list* }


define i32 @main() {
  %1 = call %class.list* @fromTo(i32 1, i32 50)
  %2 = bitcast %class.list* %1 to %class.list*
  %3 = call i32 @length(%class.list* %2)
  call void @printInt(i32 %3)
  %4 = call %class.list* @fromTo(i32 1, i32 100)
  %5 = bitcast %class.list* %4 to %class.list*
  %6 = call i32 @length2(%class.list* %5)
  call void @printInt(i32 %6)
  ret i32 0
}

define i32 @head(%class.list* %xs) {
  %1 = alloca %class.list*
  store %class.list* %xs, %class.list** %1
  %2 = load %class.list*, %class.list** %1
  %3 = getelementptr %class.list, %class.list* %2, i32 0, i32 0
  %4 = load i32, i32* %3
  ret i32 %4
}

define %class.list* @cons(i32 %x, %class.list* %xs) {
  %1 = alloca i32
  %2 = alloca %class.list*
  %n = alloca %class.list*
  store i32 %x, i32* %1
  store %class.list* %xs, %class.list** %2
  store %class.list* null, %class.list** %n
  %3 = call i8* @malloc(i32 5)
  call void @llvm.memset.p0i8.i32(i8* %3, i8 0, i32 5, i32 1, i1 false)
  %4 = bitcast i8* %3 to %class.list*
  %5 = bitcast %class.list* %4 to %class.list*
  store %class.list* %5, %class.list** %n
  %6 = load %class.list*, %class.list** %n
  %7 = getelementptr %class.list, %class.list* %6, i32 0, i32 0
  %8 = load i32, i32* %1
  store i32 %8, i32* %7
  %9 = load %class.list*, %class.list** %n
  %10 = getelementptr %class.list, %class.list* %9, i32 0, i32 1
  %11 = load %class.list*, %class.list** %2
  %12 = bitcast %class.list* %11 to %class.list*
  store %class.list* %12, %class.list** %10
  %13 = load %class.list*, %class.list** %n
  %14 = bitcast %class.list* %13 to %class.list*
  ret %class.list* %14
}

define i32 @length(%class.list* %xs) {
  %1 = alloca %class.list*
  store %class.list* %xs, %class.list** %1
  %2 = load %class.list*, %class.list** %1
  %3 = icmp eq %class.list* %2, null
  br i1 %3, label %4, label %5

; <label>:4
  ret i32 0

; <label>:5
  %6 = load %class.list*, %class.list** %1
  %7 = getelementptr %class.list, %class.list* %6, i32 0, i32 1
  %8 = load %class.list*, %class.list** %7
  %9 = bitcast %class.list* %8 to %class.list*
  %10 = call i32 @length(%class.list* %9)
  %11 = add i32 1, %10
  ret i32 %11
}

define %class.list* @fromTo(i32 %m, i32 %n) {
  %1 = alloca i32
  %2 = alloca i32
  store i32 %m, i32* %1
  store i32 %n, i32* %2
  %3 = load i32, i32* %1
  %4 = load i32, i32* %2
  %5 = icmp sgt i32 %3, %4
  br i1 %5, label %6, label %8

; <label>:6
  %7 = bitcast %class.list* null to %class.list*
  ret %class.list* %7

; <label>:8
  %9 = load i32, i32* %1
  %10 = load i32, i32* %1
  %11 = add i32 %10, 1
  %12 = load i32, i32* %2
  %13 = call %class.list* @fromTo(i32 %11, i32 %12)
  %14 = bitcast %class.list* %13 to %class.list*
  %15 = call %class.list* @cons(i32 %9, %class.list* %14)
  %16 = bitcast %class.list* %15 to %class.list*
  ret %class.list* %16
}

define i32 @length2(%class.list* %xs) {
  %1 = alloca %class.list*
  %res = alloca i32
  store %class.list* %xs, %class.list** %1
  store i32 0, i32* %res
  br label %2

; <label>:2
  %3 = load %class.list*, %class.list** %1
  %4 = icmp ne %class.list* %3, null
  br i1 %4, label %5, label %12

; <label>:5
  %6 = load i32, i32* %res
  %7 = add i32 %6, 1
  store i32 %7, i32* %res
  %8 = load %class.list*, %class.list** %1
  %9 = getelementptr %class.list, %class.list* %8, i32 0, i32 1
  %10 = load %class.list*, %class.list** %9
  %11 = bitcast %class.list* %10 to %class.list*
  store %class.list* %11, %class.list** %1
  br label %2

; <label>:12
  %13 = load i32, i32* %res
  ret i32 %13
}
