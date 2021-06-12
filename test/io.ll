
%string = type { i8*, i32, i32 }

declare i32 @puts(i8* nocapture) nounwind

define void @"echo"(%string) nounwind {
    %2 = extractvalue %string %0, 0
    call i32 @puts(i8* %2)
    ret void
}