declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
declare i8* @malloc(i32)
declare void @llvm.memset.p0i8.i32(i8*, i8, i32, i32, i1)

%class.Point2 = type { i32, i32 }
%class.Point3 = type { %class.Point2, i32 }
%class.Point4 = type { %class.Point3, i32 }


define void @Point2.move(%class.Point2* %this, i32 %dx, i32 %dy) {
  %1 = alloca %class.Point2*
  %2 = alloca i32
  %3 = alloca i32
  store %class.Point2* %this, %class.Point2** %1
  store i32 %dx, i32* %2
  store i32 %dy, i32* %3
  %4 = load %class.Point2*, %class.Point2** %1
  %5 = getelementptr %class.Point2, %class.Point2* %4, i32 0, i32 0
  %6 = load %class.Point2*, %class.Point2** %1
  %7 = getelementptr %class.Point2, %class.Point2* %6, i32 0, i32 0
  %8 = load i32, i32* %7
  %9 = load i32, i32* %2
  %10 = add i32 %8, %9
  store i32 %10, i32* %5
  %11 = load %class.Point2*, %class.Point2** %1
  %12 = getelementptr %class.Point2, %class.Point2* %11, i32 0, i32 1
  %13 = load %class.Point2*, %class.Point2** %1
  %14 = getelementptr %class.Point2, %class.Point2* %13, i32 0, i32 1
  %15 = load i32, i32* %14
  %16 = load i32, i32* %3
  %17 = add i32 %15, %16
  store i32 %17, i32* %12
  ret void
}

define i32 @Point2.getX(%class.Point2* %this) {
  %1 = alloca %class.Point2*
  store %class.Point2* %this, %class.Point2** %1
  %2 = load %class.Point2*, %class.Point2** %1
  %3 = getelementptr %class.Point2, %class.Point2* %2, i32 0, i32 0
  %4 = load i32, i32* %3
  ret i32 %4
}

define i32 @Point2.getY(%class.Point2* %this) {
  %1 = alloca %class.Point2*
  store %class.Point2* %this, %class.Point2** %1
  %2 = load %class.Point2*, %class.Point2** %1
  %3 = getelementptr %class.Point2, %class.Point2* %2, i32 0, i32 1
  %4 = load i32, i32* %3
  ret i32 %4
}

define void @Point3.moveZ(%class.Point3* %this, i32 %dz) {
  %1 = alloca %class.Point3*
  %2 = alloca i32
  store %class.Point3* %this, %class.Point3** %1
  store i32 %dz, i32* %2
  %3 = load %class.Point3*, %class.Point3** %1
  %4 = getelementptr %class.Point3, %class.Point3* %3, i32 0, i32 1
  %5 = load %class.Point3*, %class.Point3** %1
  %6 = getelementptr %class.Point3, %class.Point3* %5, i32 0, i32 1
  %7 = load i32, i32* %6
  %8 = load i32, i32* %2
  %9 = add i32 %7, %8
  store i32 %9, i32* %4
  ret void
}

define i32 @Point3.getZ(%class.Point3* %this) {
  %1 = alloca %class.Point3*
  store %class.Point3* %this, %class.Point3** %1
  %2 = load %class.Point3*, %class.Point3** %1
  %3 = getelementptr %class.Point3, %class.Point3* %2, i32 0, i32 1
  %4 = load i32, i32* %3
  ret i32 %4
}

define void @Point4.moveW(%class.Point4* %this, i32 %dw) {
  %1 = alloca %class.Point4*
  %2 = alloca i32
  store %class.Point4* %this, %class.Point4** %1
  store i32 %dw, i32* %2
  %3 = load %class.Point4*, %class.Point4** %1
  %4 = getelementptr %class.Point4, %class.Point4* %3, i32 0, i32 1
  %5 = load %class.Point4*, %class.Point4** %1
  %6 = getelementptr %class.Point4, %class.Point4* %5, i32 0, i32 1
  %7 = load i32, i32* %6
  %8 = load i32, i32* %2
  %9 = add i32 %7, %8
  store i32 %9, i32* %4
  ret void
}

define i32 @Point4.getW(%class.Point4* %this) {
  %1 = alloca %class.Point4*
  store %class.Point4* %this, %class.Point4** %1
  %2 = load %class.Point4*, %class.Point4** %1
  %3 = getelementptr %class.Point4, %class.Point4* %2, i32 0, i32 1
  %4 = load i32, i32* %3
  ret i32 %4
}

define i32 @main() {
  %p = alloca %class.Point2*
  %q = alloca %class.Point3*
  %r = alloca %class.Point4*
  %1 = call i8* @malloc(i32 12)
  call void @llvm.memset.p0i8.i32(i8* %1, i8 0, i32 12, i32 1, i1 false)
  %2 = bitcast i8* %1 to %class.Point3*
  %3 = bitcast %class.Point3* %2 to %class.Point2*
  store %class.Point2* %3, %class.Point2** %p
  %4 = call i8* @malloc(i32 12)
  call void @llvm.memset.p0i8.i32(i8* %4, i8 0, i32 12, i32 1, i1 false)
  %5 = bitcast i8* %4 to %class.Point3*
  %6 = bitcast %class.Point3* %5 to %class.Point3*
  store %class.Point3* %6, %class.Point3** %q
  %7 = call i8* @malloc(i32 16)
  call void @llvm.memset.p0i8.i32(i8* %7, i8 0, i32 16, i32 1, i1 false)
  %8 = bitcast i8* %7 to %class.Point4*
  %9 = bitcast %class.Point4* %8 to %class.Point4*
  store %class.Point4* %9, %class.Point4** %r
  %10 = load %class.Point3*, %class.Point3** %q
  %11 = load %class.Point3*, %class.Point3** %q
  %12 = bitcast %class.Point3* %11 to %class.Point2*
  call void @Point2.move(%class.Point2* %12, i32 2, i32 4)
  %13 = load %class.Point3*, %class.Point3** %q
  %14 = load %class.Point3*, %class.Point3** %q
  %15 = bitcast %class.Point3* %14 to %class.Point3*
  call void @Point3.moveZ(%class.Point3* %15, i32 7)
  %16 = load %class.Point3*, %class.Point3** %q
  %17 = bitcast %class.Point3* %16 to %class.Point2*
  store %class.Point2* %17, %class.Point2** %p
  %18 = load %class.Point2*, %class.Point2** %p
  %19 = load %class.Point2*, %class.Point2** %p
  %20 = bitcast %class.Point2* %19 to %class.Point2*
  call void @Point2.move(%class.Point2* %20, i32 3, i32 5)
  %21 = load %class.Point4*, %class.Point4** %r
  %22 = load %class.Point4*, %class.Point4** %r
  %23 = bitcast %class.Point4* %22 to %class.Point2*
  call void @Point2.move(%class.Point2* %23, i32 1, i32 3)
  %24 = load %class.Point4*, %class.Point4** %r
  %25 = load %class.Point4*, %class.Point4** %r
  %26 = bitcast %class.Point4* %25 to %class.Point3*
  call void @Point3.moveZ(%class.Point3* %26, i32 6)
  %27 = load %class.Point4*, %class.Point4** %r
  %28 = load %class.Point4*, %class.Point4** %r
  %29 = bitcast %class.Point4* %28 to %class.Point4*
  call void @Point4.moveW(%class.Point4* %29, i32 2)
  %30 = load %class.Point2*, %class.Point2** %p
  %31 = load %class.Point2*, %class.Point2** %p
  %32 = bitcast %class.Point2* %31 to %class.Point2*
  %33 = call i32 @Point2.getX(%class.Point2* %32)
  call void @printInt(i32 %33)
  %34 = load %class.Point2*, %class.Point2** %p
  %35 = load %class.Point2*, %class.Point2** %p
  %36 = bitcast %class.Point2* %35 to %class.Point2*
  %37 = call i32 @Point2.getY(%class.Point2* %36)
  call void @printInt(i32 %37)
  %38 = load %class.Point3*, %class.Point3** %q
  %39 = load %class.Point3*, %class.Point3** %q
  %40 = bitcast %class.Point3* %39 to %class.Point3*
  %41 = call i32 @Point3.getZ(%class.Point3* %40)
  call void @printInt(i32 %41)
  %42 = load %class.Point4*, %class.Point4** %r
  %43 = load %class.Point4*, %class.Point4** %r
  %44 = bitcast %class.Point4* %43 to %class.Point4*
  %45 = call i32 @Point4.getW(%class.Point4* %44)
  call void @printInt(i32 %45)
  ret i32 0
}
