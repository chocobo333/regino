; ModuleID = 'main'
source_filename = "main"

%string.1 = type { i8*, i32, i32 }

@"hello world" = constant [11 x i8] c"hello world"

; Function Attrs: nofree nounwind
define i32 @main() local_unnamed_addr #0 {
entry:
  %0 = tail call i32 @fib(i32 11)
  %1 = tail call i32 @puts(i8* nonnull dereferenceable(1) getelementptr inbounds ([11 x i8], [11 x i8]* @"hello world", i64 0, i64 0)) #4
  ret i32 11
}

; Function Attrs: norecurse nounwind readnone willreturn
define i32 @f(i32 returned %0) local_unnamed_addr #1 {
entry:
  ret i32 %0
}

; Function Attrs: norecurse nounwind readnone willreturn
define float @f.1(float returned %0) local_unnamed_addr #1 {
entry:
  ret float %0
}

; Function Attrs: norecurse nounwind readnone willreturn
define i1 @f.2(i1 returned %0) local_unnamed_addr #1 {
entry:
  ret i1 %0
}

; Function Attrs: norecurse nounwind readnone willreturn
define i32 @"+"(i32 %0, i32 %1) local_unnamed_addr #1 {
entry:
  %ret.i = add i32 %1, %0
  ret i32 %ret.i
}

; Function Attrs: norecurse nounwind readnone willreturn
define i32 @-(i32 %0, i32 %1) local_unnamed_addr #1 {
entry:
  %ret.i = sub i32 %0, %1
  ret i32 %ret.i
}

; Function Attrs: norecurse nounwind readnone willreturn
define i1 @"=="(i32 %0, i32 %1) local_unnamed_addr #1 {
entry:
  %ret.i = icmp eq i32 %0, %1
  ret i1 %ret.i
}

; Function Attrs: nounwind readnone
define i32 @fib(i32 %0) local_unnamed_addr #2 {
entry:
  %switch13 = icmp ult i32 %0, 2
  br i1 %switch13, label %ifcont, label %else

else:                                             ; preds = %entry, %else
  %.tr15 = phi i32 [ %ret.i.i12, %else ], [ %0, %entry ]
  %accumulator.tr14 = phi i32 [ %ret.i.i11, %else ], [ 0, %entry ]
  %ret.i.i9 = add i32 %.tr15, -1
  %1 = tail call i32 @fib(i32 %ret.i.i9)
  %ret.i.i12 = add i32 %.tr15, -2
  %ret.i.i11 = add i32 %1, %accumulator.tr14
  %switch = icmp ult i32 %ret.i.i12, 2
  br i1 %switch, label %ifcont.loopexit, label %else

ifcont.loopexit:                                  ; preds = %else
  %phi.bo = add i32 %ret.i.i11, 1
  br label %ifcont

ifcont:                                           ; preds = %ifcont.loopexit, %entry
  %accumulator.tr.lcssa = phi i32 [ 1, %entry ], [ %phi.bo, %ifcont.loopexit ]
  ret i32 %accumulator.tr.lcssa
}

; Function Attrs: norecurse nounwind readnone willreturn
define i32 @"add@int32/int32int32"(i32 %0, i32 %1) local_unnamed_addr #1 {
entry:
  %ret = add i32 %1, %0
  ret i32 %ret
}

; Function Attrs: norecurse nounwind readnone willreturn
define i32 @"sub@int32/int32int32"(i32 %0, i32 %1) local_unnamed_addr #1 {
entry:
  %ret = sub i32 %0, %1
  ret i32 %ret
}

; Function Attrs: norecurse nounwind readnone willreturn
define i1 @"eq@bool/int32int32"(i32 %0, i32 %1) local_unnamed_addr #1 {
entry:
  %ret = icmp eq i32 %0, %1
  ret i1 %ret
}

; Function Attrs: nofree nounwind
define void @echo(%string.1 %0) local_unnamed_addr #3 {
  %2 = extractvalue %string.1 %0, 0
  %3 = tail call i32 @puts(i8* nonnull dereferenceable(1) %2)
  ret void
}

; Function Attrs: nofree nounwind
declare noundef i32 @puts(i8* nocapture noundef readonly) local_unnamed_addr #0

; Function Attrs: norecurse nounwind readnone willreturn
define i32 @len(%string.1 %0) local_unnamed_addr #1 {
  %2 = extractvalue %string.1 %0, 1
  ret i32 %2
}

attributes #0 = { nofree nounwind }
attributes #1 = { norecurse nounwind readnone willreturn }
attributes #2 = { nounwind readnone }
attributes #3 = { nofree nounwind }
attributes #4 = { nounwind }
