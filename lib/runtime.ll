; ModuleID = 'runtime.c'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i8*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type { %struct._IO_marker*, %struct._IO_FILE*, i32 }

@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@stdin = external global %struct._IO_FILE*, align 8
@str = private unnamed_addr constant [14 x i8] c"runtime error\00"

; Function Attrs: nounwind uwtable
define void @printInt(i32 %x) #0 {
  %1 = tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i64 0, i64 0), i32 %x)
  ret void
}

; Function Attrs: nounwind
declare i32 @printf(i8* nocapture readonly, ...) #1

; Function Attrs: nounwind uwtable
define void @printString(i8* nocapture %string) #0 {
  %puts = tail call i32 @puts(i8* %string)
  ret void
}

; Function Attrs: noreturn nounwind uwtable
define void @error() #2 {
  %puts = tail call i32 @puts(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @str, i64 0, i64 0))
  tail call void @exit(i32 1) #8
  unreachable
}

; Function Attrs: noreturn nounwind
declare void @exit(i32) #3

; Function Attrs: nounwind uwtable
define i32 @readInt() #0 {
  %x = alloca i32, align 4
  %1 = bitcast i32* %x to i8*
  call void @llvm.lifetime.start(i64 4, i8* %1) #7
  %2 = call i32 (i8*, ...) @__isoc99_scanf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i64 0, i64 0), i32* nonnull %x)
  %3 = load i32, i32* %x, align 4, !tbaa !1
  call void @llvm.lifetime.end(i64 4, i8* %1) #7
  ret i32 %3
}

; Function Attrs: argmemonly nounwind
declare void @llvm.lifetime.start(i64, i8* nocapture) #4

; Function Attrs: nounwind
declare i32 @__isoc99_scanf(i8* nocapture readonly, ...) #1

; Function Attrs: argmemonly nounwind
declare void @llvm.lifetime.end(i64, i8* nocapture) #4

; Function Attrs: nounwind uwtable
define noalias i8* @readString() #0 {
  %line = alloca i8*, align 8
  %len = alloca i64, align 8
  %1 = bitcast i8** %line to i8*
  call void @llvm.lifetime.start(i64 8, i8* %1) #7
  store i8* null, i8** %line, align 8, !tbaa !5
  %2 = bitcast i64* %len to i8*
  call void @llvm.lifetime.start(i64 8, i8* %2) #7
  store i64 0, i64* %len, align 8, !tbaa !7
  %3 = load %struct._IO_FILE*, %struct._IO_FILE** @stdin, align 8, !tbaa !5
  %4 = call i64 @getline(i8** nonnull %line, i64* nonnull %len, %struct._IO_FILE* %3) #7
  %5 = icmp eq i64 %4, -1
  br i1 %5, label %16, label %6

; <label>:6                                       ; preds = %0
  %7 = load i8*, i8** %line, align 8, !tbaa !5
  %8 = call i64 @strlen(i8* %7) #9
  %9 = call noalias i8* @malloc(i64 %8) #7
  %strlenfirst = load i8, i8* %7, align 1
  %10 = icmp eq i8 %strlenfirst, 0
  br i1 %10, label %._crit_edge, label %.lr.ph.preheader

.lr.ph.preheader:                                 ; preds = %6
  br label %.lr.ph

.lr.ph:                                           ; preds = %.lr.ph.preheader, %.lr.ph
  %indvars.iv = phi i64 [ %indvars.iv.next, %.lr.ph ], [ 0, %.lr.ph.preheader ]
  %11 = getelementptr inbounds i8, i8* %7, i64 %indvars.iv
  %12 = load i8, i8* %11, align 1, !tbaa !9
  %13 = getelementptr inbounds i8, i8* %9, i64 %indvars.iv
  store i8 %12, i8* %13, align 1, !tbaa !9
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  %14 = call i64 @strlen(i8* %7) #9
  %15 = icmp ult i64 %indvars.iv.next, %14
  br i1 %15, label %.lr.ph, label %._crit_edge.loopexit

; <label>:16                                      ; preds = %0
  call void @error()
  unreachable

._crit_edge.loopexit:                             ; preds = %.lr.ph
  %.lcssa6 = phi i64 [ %14, %.lr.ph ]
  %phitmp = add i64 %.lcssa6, -1
  br label %._crit_edge

._crit_edge:                                      ; preds = %._crit_edge.loopexit, %6
  %.lcssa = phi i64 [ -1, %6 ], [ %phitmp, %._crit_edge.loopexit ]
  %17 = getelementptr inbounds i8, i8* %9, i64 %.lcssa
  store i8 0, i8* %17, align 1, !tbaa !9
  call void @llvm.lifetime.end(i64 8, i8* %2) #7
  call void @llvm.lifetime.end(i64 8, i8* nonnull %1) #7
  ret i8* %9
}

declare i64 @getline(i8**, i64*, %struct._IO_FILE*) #5

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #1

; Function Attrs: nounwind readonly
declare i64 @strlen(i8* nocapture) #6

; Function Attrs: nounwind uwtable
define i8* @concat(i8* nocapture readonly %s1, i8* nocapture readonly %s2) #0 {
  %1 = tail call i64 @strlen(i8* %s1) #9
  %2 = tail call i64 @strlen(i8* %s2) #9
  %3 = add i64 %1, 1
  %4 = add i64 %3, %2
  %5 = tail call noalias i8* @malloc(i64 %4) #7
  %6 = tail call i8* @strcpy(i8* %5, i8* %s1) #7
  %7 = tail call i8* @strcat(i8* %6, i8* %s2) #7
  ret i8* %7
}

; Function Attrs: nounwind
declare i8* @strcat(i8*, i8* nocapture readonly) #1

; Function Attrs: nounwind
declare i8* @strcpy(i8*, i8* nocapture readonly) #1

; Function Attrs: nounwind
declare i32 @puts(i8* nocapture) #7

attributes #0 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { noreturn nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { noreturn nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { argmemonly nounwind }
attributes #5 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { nounwind readonly "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #7 = { nounwind }
attributes #8 = { noreturn nounwind }
attributes #9 = { nounwind readonly }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.1-24 (tags/RELEASE_381/final)"}
!1 = !{!2, !2, i64 0}
!2 = !{!"int", !3, i64 0}
!3 = !{!"omnipotent char", !4, i64 0}
!4 = !{!"Simple C/C++ TBAA"}
!5 = !{!6, !6, i64 0}
!6 = !{!"any pointer", !3, i64 0}
!7 = !{!8, !8, i64 0}
!8 = !{!"long", !3, i64 0}
!9 = !{!3, !3, i64 0}
