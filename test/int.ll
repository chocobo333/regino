%string = type { i8*, i32, i32 }

declare i8* @malloc(i32)
declare float @log10f(float)
declare i32 @sprintf(i8*, i8*, ...)

@fmt = constant [2 x i8] c"%d"

define void @"tos@string/i32"(i32, %string*) {
    %len = sitofp i32 %0 to float
    %len2 = call float @log10f(float %len)
    %len3 = fptosi float %len2 to i32
    %len4 = add i32 1, %len3
    %pointer = call i8* @malloc(i32 %len4)
    %fmtstr = getelementptr [2 x i8], [2 x i8]* @fmt, i32 0, i32 0
    call i32 (i8*, i8*, ...) @sprintf(i8* %pointer, i8* %fmtstr, i32 %0)
    %4 = getelementptr %string, %string* %1, i32 0, i32 0
    store i8* %pointer, i8** %4
    %5 = getelementptr %string, %string* %1, i32 0, i32 1
    store i32 %len4, i32* %5
    %6 = getelementptr %string, %string* %1, i32 0, i32 2
    store i32 %len4, i32* %6
    ret void
}