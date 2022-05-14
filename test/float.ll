
%string = type { i8*, i32, i32 }

declare i8* @malloc(i32)
declare float @log10f(float)
declare i32 @sprintf(i8*, i8*, ...)

@fmt_float = constant [3 x i8] c"%f\00"

define void @"tos@string/float"(float, %string*) {
    %pointer = call i8* @malloc(i32 64)
    %fmtstr = getelementptr [3 x i8], [3 x i8]* @fmt_float, i32 0, i32 0
    call i32 (i8*, i8*, ...) @sprintf(i8* %pointer, i8* %fmtstr, float %0)
    %4 = getelementptr %string, %string* %1, i32 0, i32 0
    store i8* %pointer, i8** %4
    %5 = getelementptr %string, %string* %1, i32 0, i32 1
    store i32 64, i32* %5
    %6 = getelementptr %string, %string* %1, i32 0, i32 2
    store i32 64, i32* %6
    ret void
}