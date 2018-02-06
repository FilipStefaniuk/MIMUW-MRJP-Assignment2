declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)
declare void @llvm.memset.p0i8.i32(i8*, i8, i32, i32, i1)

%class.A = type { %class.A* }

@.str = constant [6 x i8] c"empty\00"

define i32 @main() {
  %a = alloca %class.A*
  %1 = call i8* @malloc(i32 1)
  call void @llvm.memset.p0i8.i32(i8* %1, i8 0, i32 1, i32 1, i1 false)
  %2 = bitcast i8* %1 to %class.A*
  %3 = bitcast %class.A* %2 to %class.A*
  store %class.A* %3, %class.A** %a
  %4 = load %class.A*, %class.A** %a
  %5 = getelementptr %class.A, %class.A* %4, i32 0, i32 0
  %6 = load %class.A*, %class.A** %5
  %7 = icmp eq %class.A* %6, null
  br i1 %7, label %8, label %10

; <label>:8
  %9 = getelementptr [6 x i8], [6 x i8]* @.str, i32 0, i32 0
  call void @printString(i8* %9)
  br label %10

; <label>:10
  ret i32 0
}
