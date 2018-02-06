declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)
declare void @llvm.memset.p0i8.i32(i8*, i8, i32, i32, i1)

%class.Node = type { i32, %class.Node* }
%class.IntQueue = type { %class.Node*, %class.Node* }


define void @Node.setElem(%class.Node* %self, i32 %e) {
  %1 = alloca %class.Node*
  %2 = alloca i32
  store %class.Node* %self, %class.Node** %1
  store i32 %e, i32* %2
  %3 = load %class.Node*, %class.Node** %1
  %4 = getelementptr %class.Node, %class.Node* %3, i32 0, i32 0
  %5 = load i32, i32* %2
  store i32 %5, i32* %4
  ret void
}

define void @Node.setNext(%class.Node* %self, %class.Node* %n) {
  %1 = alloca %class.Node*
  %2 = alloca %class.Node*
  store %class.Node* %self, %class.Node** %1
  store %class.Node* %n, %class.Node** %2
  %3 = load %class.Node*, %class.Node** %1
  %4 = getelementptr %class.Node, %class.Node* %3, i32 0, i32 1
  %5 = load %class.Node*, %class.Node** %2
  %6 = bitcast %class.Node* %5 to %class.Node*
  store %class.Node* %6, %class.Node** %4
  ret void
}

define i32 @Node.getElem(%class.Node* %self) {
  %1 = alloca %class.Node*
  store %class.Node* %self, %class.Node** %1
  %2 = load %class.Node*, %class.Node** %1
  %3 = getelementptr %class.Node, %class.Node* %2, i32 0, i32 0
  %4 = load i32, i32* %3
  ret i32 %4
}

define %class.Node* @Node.getNext(%class.Node* %self) {
  %1 = alloca %class.Node*
  store %class.Node* %self, %class.Node** %1
  %2 = load %class.Node*, %class.Node** %1
  %3 = getelementptr %class.Node, %class.Node* %2, i32 0, i32 1
  %4 = load %class.Node*, %class.Node** %3
  %5 = bitcast %class.Node* %4 to %class.Node*
  ret %class.Node* %5
}

define i8 @IntQueue.isEmpty(%class.IntQueue* %self) {
  %1 = alloca %class.IntQueue*
  store %class.IntQueue* %self, %class.IntQueue** %1
  %2 = load %class.IntQueue*, %class.IntQueue** %1
  %3 = getelementptr %class.IntQueue, %class.IntQueue* %2, i32 0, i32 0
  %4 = load %class.Node*, %class.Node** %3
  %5 = icmp eq %class.Node* %4, null
  %6 = zext i1 %5 to i8
  ret i8 %6
}

define void @IntQueue.insert(%class.IntQueue* %self, i32 %x) {
  %1 = alloca %class.IntQueue*
  %2 = alloca i32
  %last = alloca %class.Node*
  store %class.IntQueue* %self, %class.IntQueue** %1
  store i32 %x, i32* %2
  %3 = call i8* @malloc(i32 5)
  call void @llvm.memset.p0i8.i32(i8* %3, i8 0, i32 5, i32 1, i1 false)
  %4 = bitcast i8* %3 to %class.Node*
  %5 = bitcast %class.Node* %4 to %class.Node*
  store %class.Node* %5, %class.Node** %last
  %6 = load %class.Node*, %class.Node** %last
  %7 = load %class.Node*, %class.Node** %last
  %8 = bitcast %class.Node* %7 to %class.Node*
  %9 = load i32, i32* %2
  call void @Node.setElem(%class.Node* %8, i32 %9)
  %10 = load %class.IntQueue*, %class.IntQueue** %1
  %11 = load %class.IntQueue*, %class.IntQueue** %1
  %12 = bitcast %class.IntQueue* %11 to %class.IntQueue*
  %13 = call i8 @IntQueue.isEmpty(%class.IntQueue* %12)
  %14 = trunc i8 %13 to i1
  br i1 %14, label %15, label %20

; <label>:15
  %16 = load %class.IntQueue*, %class.IntQueue** %1
  %17 = getelementptr %class.IntQueue, %class.IntQueue* %16, i32 0, i32 0
  %18 = load %class.Node*, %class.Node** %last
  %19 = bitcast %class.Node* %18 to %class.Node*
  store %class.Node* %19, %class.Node** %17
  br label %30

; <label>:20
  %21 = load %class.IntQueue*, %class.IntQueue** %1
  %22 = getelementptr %class.IntQueue, %class.IntQueue* %21, i32 0, i32 1
  %23 = load %class.Node*, %class.Node** %22
  %24 = load %class.IntQueue*, %class.IntQueue** %1
  %25 = getelementptr %class.IntQueue, %class.IntQueue* %24, i32 0, i32 1
  %26 = load %class.Node*, %class.Node** %25
  %27 = bitcast %class.Node* %26 to %class.Node*
  %28 = load %class.Node*, %class.Node** %last
  %29 = bitcast %class.Node* %28 to %class.Node*
  call void @Node.setNext(%class.Node* %27, %class.Node* %29)
  br label %30

; <label>:30
  %31 = load %class.IntQueue*, %class.IntQueue** %1
  %32 = getelementptr %class.IntQueue, %class.IntQueue* %31, i32 0, i32 1
  %33 = load %class.Node*, %class.Node** %last
  %34 = bitcast %class.Node* %33 to %class.Node*
  store %class.Node* %34, %class.Node** %32
  ret void
}

define i32 @IntQueue.first(%class.IntQueue* %self) {
  %1 = alloca %class.IntQueue*
  store %class.IntQueue* %self, %class.IntQueue** %1
  %2 = load %class.IntQueue*, %class.IntQueue** %1
  %3 = getelementptr %class.IntQueue, %class.IntQueue* %2, i32 0, i32 0
  %4 = load %class.Node*, %class.Node** %3
  %5 = load %class.IntQueue*, %class.IntQueue** %1
  %6 = getelementptr %class.IntQueue, %class.IntQueue* %5, i32 0, i32 0
  %7 = load %class.Node*, %class.Node** %6
  %8 = bitcast %class.Node* %7 to %class.Node*
  %9 = call i32 @Node.getElem(%class.Node* %8)
  ret i32 %9
}

define void @IntQueue.rmFirst(%class.IntQueue* %self) {
  %1 = alloca %class.IntQueue*
  store %class.IntQueue* %self, %class.IntQueue** %1
  %2 = load %class.IntQueue*, %class.IntQueue** %1
  %3 = getelementptr %class.IntQueue, %class.IntQueue* %2, i32 0, i32 0
  %4 = load %class.IntQueue*, %class.IntQueue** %1
  %5 = getelementptr %class.IntQueue, %class.IntQueue* %4, i32 0, i32 0
  %6 = load %class.Node*, %class.Node** %5
  %7 = load %class.IntQueue*, %class.IntQueue** %1
  %8 = getelementptr %class.IntQueue, %class.IntQueue* %7, i32 0, i32 0
  %9 = load %class.Node*, %class.Node** %8
  %10 = bitcast %class.Node* %9 to %class.Node*
  %11 = call %class.Node* @Node.getNext(%class.Node* %10)
  %12 = bitcast %class.Node* %11 to %class.Node*
  store %class.Node* %12, %class.Node** %3
  ret void
}

define i32 @IntQueue.size(%class.IntQueue* %self) {
  %1 = alloca %class.IntQueue*
  %n = alloca %class.Node*
  %res = alloca i32
  store %class.IntQueue* %self, %class.IntQueue** %1
  %2 = load %class.IntQueue*, %class.IntQueue** %1
  %3 = getelementptr %class.IntQueue, %class.IntQueue* %2, i32 0, i32 0
  %4 = load %class.Node*, %class.Node** %3
  %5 = bitcast %class.Node* %4 to %class.Node*
  store %class.Node* %5, %class.Node** %n
  store i32 0, i32* %res
  br label %6

; <label>:6
  %7 = load %class.Node*, %class.Node** %n
  %8 = icmp ne %class.Node* %7, null
  br i1 %8, label %9, label %17

; <label>:9
  %10 = load %class.Node*, %class.Node** %n
  %11 = load %class.Node*, %class.Node** %n
  %12 = bitcast %class.Node* %11 to %class.Node*
  %13 = call %class.Node* @Node.getNext(%class.Node* %12)
  %14 = bitcast %class.Node* %13 to %class.Node*
  store %class.Node* %14, %class.Node** %n
  %15 = load i32, i32* %res
  %16 = add i32 %15, 1
  store i32 %16, i32* %res
  br label %6

; <label>:17
  %18 = load i32, i32* %res
  ret i32 %18
}

define i32 @f(i32 %x) {
  %1 = alloca i32
  store i32 %x, i32* %1
  %2 = load i32, i32* %1
  %3 = load i32, i32* %1
  %4 = mul i32 %2, %3
  %5 = add i32 %4, 3
  ret i32 %5
}

define i32 @main() {
  %q = alloca %class.IntQueue*
  %1 = call i8* @malloc(i32 2)
  call void @llvm.memset.p0i8.i32(i8* %1, i8 0, i32 2, i32 1, i1 false)
  %2 = bitcast i8* %1 to %class.IntQueue*
  %3 = bitcast %class.IntQueue* %2 to %class.IntQueue*
  store %class.IntQueue* %3, %class.IntQueue** %q
  %4 = load %class.IntQueue*, %class.IntQueue** %q
  %5 = load %class.IntQueue*, %class.IntQueue** %q
  %6 = bitcast %class.IntQueue* %5 to %class.IntQueue*
  %7 = call i32 @f(i32 3)
  call void @IntQueue.insert(%class.IntQueue* %6, i32 %7)
  %8 = load %class.IntQueue*, %class.IntQueue** %q
  %9 = load %class.IntQueue*, %class.IntQueue** %q
  %10 = bitcast %class.IntQueue* %9 to %class.IntQueue*
  call void @IntQueue.insert(%class.IntQueue* %10, i32 5)
  %11 = load %class.IntQueue*, %class.IntQueue** %q
  %12 = load %class.IntQueue*, %class.IntQueue** %q
  %13 = bitcast %class.IntQueue* %12 to %class.IntQueue*
  call void @IntQueue.insert(%class.IntQueue* %13, i32 7)
  %14 = load %class.IntQueue*, %class.IntQueue** %q
  %15 = load %class.IntQueue*, %class.IntQueue** %q
  %16 = bitcast %class.IntQueue* %15 to %class.IntQueue*
  %17 = call i32 @IntQueue.first(%class.IntQueue* %16)
  call void @printInt(i32 %17)
  %18 = load %class.IntQueue*, %class.IntQueue** %q
  %19 = load %class.IntQueue*, %class.IntQueue** %q
  %20 = bitcast %class.IntQueue* %19 to %class.IntQueue*
  call void @IntQueue.rmFirst(%class.IntQueue* %20)
  %21 = load %class.IntQueue*, %class.IntQueue** %q
  %22 = load %class.IntQueue*, %class.IntQueue** %q
  %23 = bitcast %class.IntQueue* %22 to %class.IntQueue*
  %24 = call i32 @IntQueue.size(%class.IntQueue* %23)
  call void @printInt(i32 %24)
  ret i32 0
}
