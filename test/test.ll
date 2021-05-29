; ModuleID = 'main'
source_filename = "main"

; Function Attrs: norecurse nounwind readnone willreturn
define i32 @main() local_unnamed_addr #0 {
entry:
  ret i32 4
}

; Function Attrs: norecurse nounwind readnone willreturn
define i32 @f(i32 returned %0) local_unnamed_addr #0 {
entry:
  ret i32 %0
}

; Function Attrs: norecurse nounwind readnone willreturn
define i32 @"+"(i32 %0, i32 %1) local_unnamed_addr #0 {
entry:
  %ret.i = add i32 %1, %0
  ret i32 %ret.i
}

; Function Attrs: norecurse nounwind readnone willreturn
define i32 @"add@int32/int32int32"(i32 %0, i32 %1) local_unnamed_addr #0 {
entry:
  %ret = add i32 %1, %0
  ret i32 %ret
}

; Function Attrs: norecurse nounwind readnone willreturn
define i32 @"sub@int32/int32int32"(i32 %0, i32 %1) local_unnamed_addr #0 {
entry:
  %ret = sub i32 %0, %1
  ret i32 %ret
}

attributes #0 = { norecurse nounwind readnone willreturn }
