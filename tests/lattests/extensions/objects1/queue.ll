declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @calloc(i32, i32)

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
  store %class.Node* %5, %class.Node** %4
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
  ret %class.Node* %4
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
  %3 = call i8* @calloc(i32 1, i32 5)
  %4 = bitcast i8* %3 to %class.Node*
  store %class.Node* %4, %class.Node** %last
  %5 = load %class.Node*, %class.Node** %last
  %6 = load %class.Node*, %class.Node** %last
  %7 = load i32, i32* %2
  call void @Node.setElem(%class.Node* %6, i32 %7)
  %8 = load %class.IntQueue*, %class.IntQueue** %1
  %9 = load %class.IntQueue*, %class.IntQueue** %1
  %10 = call i8 @IntQueue.isEmpty(%class.IntQueue* %9)
  %11 = trunc i8 %10 to i1
  br i1 %11, label %12, label %16

; <label>:12
  %13 = load %class.IntQueue*, %class.IntQueue** %1
  %14 = getelementptr %class.IntQueue, %class.IntQueue* %13, i32 0, i32 0
  %15 = load %class.Node*, %class.Node** %last
  store %class.Node* %15, %class.Node** %14
  br label %24

; <label>:16
  %17 = load %class.IntQueue*, %class.IntQueue** %1
  %18 = getelementptr %class.IntQueue, %class.IntQueue* %17, i32 0, i32 1
  %19 = load %class.Node*, %class.Node** %18
  %20 = load %class.IntQueue*, %class.IntQueue** %1
  %21 = getelementptr %class.IntQueue, %class.IntQueue* %20, i32 0, i32 1
  %22 = load %class.Node*, %class.Node** %21
  %23 = load %class.Node*, %class.Node** %last
  call void @Node.setNext(%class.Node* %22, %class.Node* %23)
  br label %24

; <label>:24
  %25 = load %class.IntQueue*, %class.IntQueue** %1
  %26 = getelementptr %class.IntQueue, %class.IntQueue* %25, i32 0, i32 1
  %27 = load %class.Node*, %class.Node** %last
  store %class.Node* %27, %class.Node** %26
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
  %8 = call i32 @Node.getElem(%class.Node* %7)
  ret i32 %8
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
  %10 = call %class.Node* @Node.getNext(%class.Node* %9)
  store %class.Node* %10, %class.Node** %3
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
  store %class.Node* %4, %class.Node** %n
  store i32 0, i32* %res
  br label %5

; <label>:5
  %6 = load %class.Node*, %class.Node** %n
  %7 = icmp ne %class.Node* %6, null
  br i1 %7, label %8, label %14

; <label>:8
  %9 = load %class.Node*, %class.Node** %n
  %10 = load %class.Node*, %class.Node** %n
  %11 = call %class.Node* @Node.getNext(%class.Node* %10)
  store %class.Node* %11, %class.Node** %n
  %12 = load i32, i32* %res
  %13 = add i32 %12, 1
  store i32 %13, i32* %res
  br label %5

; <label>:14
  %15 = load i32, i32* %res
  ret i32 %15
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
  %1 = call i8* @calloc(i32 1, i32 2)
  %2 = bitcast i8* %1 to %class.IntQueue*
  store %class.IntQueue* %2, %class.IntQueue** %q
  %3 = load %class.IntQueue*, %class.IntQueue** %q
  %4 = load %class.IntQueue*, %class.IntQueue** %q
  %5 = call i32 @f(i32 3)
  call void @IntQueue.insert(%class.IntQueue* %4, i32 %5)
  %6 = load %class.IntQueue*, %class.IntQueue** %q
  %7 = load %class.IntQueue*, %class.IntQueue** %q
  call void @IntQueue.insert(%class.IntQueue* %7, i32 5)
  %8 = load %class.IntQueue*, %class.IntQueue** %q
  %9 = load %class.IntQueue*, %class.IntQueue** %q
  call void @IntQueue.insert(%class.IntQueue* %9, i32 7)
  %10 = load %class.IntQueue*, %class.IntQueue** %q
  %11 = load %class.IntQueue*, %class.IntQueue** %q
  %12 = call i32 @IntQueue.first(%class.IntQueue* %11)
  call void @printInt(i32 %12)
  %13 = load %class.IntQueue*, %class.IntQueue** %q
  %14 = load %class.IntQueue*, %class.IntQueue** %q
  call void @IntQueue.rmFirst(%class.IntQueue* %14)
  %15 = load %class.IntQueue*, %class.IntQueue** %q
  %16 = load %class.IntQueue*, %class.IntQueue** %q
  %17 = call i32 @IntQueue.size(%class.IntQueue* %16)
  call void @printInt(i32 %17)
  ret i32 0
}
