declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)
declare void @llvm.memset.p0i8.i32(i8*, i8, i32, i32, i1)

%class.Node = type { i32, %class.Node* }
%class.Stack = type { %class.Node* }


define void @Node.setElem(%class.Node* %this, i32 %c) {
  %1 = alloca %class.Node*
  %2 = alloca i32
  store %class.Node* %this, %class.Node** %1
  store i32 %c, i32* %2
  %3 = load %class.Node*, %class.Node** %1
  %4 = getelementptr %class.Node, %class.Node* %3, i32 0, i32 0
  %5 = load i32, i32* %2
  store i32 %5, i32* %4
  ret void
}

define void @Node.setNext(%class.Node* %this, %class.Node* %n) {
  %1 = alloca %class.Node*
  %2 = alloca %class.Node*
  store %class.Node* %this, %class.Node** %1
  store %class.Node* %n, %class.Node** %2
  %3 = load %class.Node*, %class.Node** %1
  %4 = getelementptr %class.Node, %class.Node* %3, i32 0, i32 1
  %5 = load %class.Node*, %class.Node** %2
  %6 = bitcast %class.Node* %5 to %class.Node*
  store %class.Node* %6, %class.Node** %4
  ret void
}

define i32 @Node.getElem(%class.Node* %this) {
  %1 = alloca %class.Node*
  store %class.Node* %this, %class.Node** %1
  %2 = load %class.Node*, %class.Node** %1
  %3 = getelementptr %class.Node, %class.Node* %2, i32 0, i32 0
  %4 = load i32, i32* %3
  ret i32 %4
}

define %class.Node* @Node.getNext(%class.Node* %this) {
  %1 = alloca %class.Node*
  store %class.Node* %this, %class.Node** %1
  %2 = load %class.Node*, %class.Node** %1
  %3 = getelementptr %class.Node, %class.Node* %2, i32 0, i32 1
  %4 = load %class.Node*, %class.Node** %3
  %5 = bitcast %class.Node* %4 to %class.Node*
  ret %class.Node* %5
}

define void @Stack.push(%class.Stack* %this, i32 %c) {
  %1 = alloca %class.Stack*
  %2 = alloca i32
  %newHead = alloca %class.Node*
  store %class.Stack* %this, %class.Stack** %1
  store i32 %c, i32* %2
  %3 = call i8* @malloc(i32 5)
  call void @llvm.memset.p0i8.i32(i8* %3, i8 0, i32 5, i32 1, i1 false)
  %4 = bitcast i8* %3 to %class.Node*
  %5 = bitcast %class.Node* %4 to %class.Node*
  store %class.Node* %5, %class.Node** %newHead
  %6 = load %class.Node*, %class.Node** %newHead
  %7 = load %class.Node*, %class.Node** %newHead
  %8 = bitcast %class.Node* %7 to %class.Node*
  %9 = load i32, i32* %2
  call void @Node.setElem(%class.Node* %8, i32 %9)
  %10 = load %class.Node*, %class.Node** %newHead
  %11 = load %class.Node*, %class.Node** %newHead
  %12 = bitcast %class.Node* %11 to %class.Node*
  %13 = load %class.Stack*, %class.Stack** %1
  %14 = getelementptr %class.Stack, %class.Stack* %13, i32 0, i32 0
  %15 = load %class.Node*, %class.Node** %14
  %16 = bitcast %class.Node* %15 to %class.Node*
  call void @Node.setNext(%class.Node* %12, %class.Node* %16)
  %17 = load %class.Stack*, %class.Stack** %1
  %18 = getelementptr %class.Stack, %class.Stack* %17, i32 0, i32 0
  %19 = load %class.Node*, %class.Node** %newHead
  %20 = bitcast %class.Node* %19 to %class.Node*
  store %class.Node* %20, %class.Node** %18
  ret void
}

define i8 @Stack.isEmpty(%class.Stack* %this) {
  %1 = alloca %class.Stack*
  store %class.Stack* %this, %class.Stack** %1
  %2 = load %class.Stack*, %class.Stack** %1
  %3 = getelementptr %class.Stack, %class.Stack* %2, i32 0, i32 0
  %4 = load %class.Node*, %class.Node** %3
  %5 = icmp eq %class.Node* %4, null
  %6 = zext i1 %5 to i8
  ret i8 %6
}

define i32 @Stack.top(%class.Stack* %this) {
  %1 = alloca %class.Stack*
  store %class.Stack* %this, %class.Stack** %1
  %2 = load %class.Stack*, %class.Stack** %1
  %3 = getelementptr %class.Stack, %class.Stack* %2, i32 0, i32 0
  %4 = load %class.Node*, %class.Node** %3
  %5 = load %class.Stack*, %class.Stack** %1
  %6 = getelementptr %class.Stack, %class.Stack* %5, i32 0, i32 0
  %7 = load %class.Node*, %class.Node** %6
  %8 = bitcast %class.Node* %7 to %class.Node*
  %9 = call i32 @Node.getElem(%class.Node* %8)
  ret i32 %9
}

define void @Stack.pop(%class.Stack* %this) {
  %1 = alloca %class.Stack*
  store %class.Stack* %this, %class.Stack** %1
  %2 = load %class.Stack*, %class.Stack** %1
  %3 = getelementptr %class.Stack, %class.Stack* %2, i32 0, i32 0
  %4 = load %class.Stack*, %class.Stack** %1
  %5 = getelementptr %class.Stack, %class.Stack* %4, i32 0, i32 0
  %6 = load %class.Node*, %class.Node** %5
  %7 = load %class.Stack*, %class.Stack** %1
  %8 = getelementptr %class.Stack, %class.Stack* %7, i32 0, i32 0
  %9 = load %class.Node*, %class.Node** %8
  %10 = bitcast %class.Node* %9 to %class.Node*
  %11 = call %class.Node* @Node.getNext(%class.Node* %10)
  %12 = bitcast %class.Node* %11 to %class.Node*
  store %class.Node* %12, %class.Node** %3
  ret void
}

define i32 @main() {
  %s = alloca %class.Stack*
  %i = alloca i32
  %1 = call i8* @malloc(i32 1)
  call void @llvm.memset.p0i8.i32(i8* %1, i8 0, i32 1, i32 1, i1 false)
  %2 = bitcast i8* %1 to %class.Stack*
  %3 = bitcast %class.Stack* %2 to %class.Stack*
  store %class.Stack* %3, %class.Stack** %s
  store i32 0, i32* %i
  br label %4

; <label>:4
  %5 = load i32, i32* %i
  %6 = icmp slt i32 %5, 10
  br i1 %6, label %7, label %14

; <label>:7
  %8 = load %class.Stack*, %class.Stack** %s
  %9 = load %class.Stack*, %class.Stack** %s
  %10 = bitcast %class.Stack* %9 to %class.Stack*
  %11 = load i32, i32* %i
  call void @Stack.push(%class.Stack* %10, i32 %11)
  %12 = load i32, i32* %i
  %13 = add i32 %12, 1
  store i32 %13, i32* %i
  br label %4

; <label>:14
  br label %15

; <label>:15
  %16 = load %class.Stack*, %class.Stack** %s
  %17 = load %class.Stack*, %class.Stack** %s
  %18 = bitcast %class.Stack* %17 to %class.Stack*
  %19 = call i8 @Stack.isEmpty(%class.Stack* %18)
  %20 = trunc i8 %19 to i1
  %21 = icmp eq i1 %20, false
  br i1 %21, label %22, label %30

; <label>:22
  %23 = load %class.Stack*, %class.Stack** %s
  %24 = load %class.Stack*, %class.Stack** %s
  %25 = bitcast %class.Stack* %24 to %class.Stack*
  %26 = call i32 @Stack.top(%class.Stack* %25)
  call void @printInt(i32 %26)
  %27 = load %class.Stack*, %class.Stack** %s
  %28 = load %class.Stack*, %class.Stack** %s
  %29 = bitcast %class.Stack* %28 to %class.Stack*
  call void @Stack.pop(%class.Stack* %29)
  br label %15

; <label>:30
  ret i32 0
}
