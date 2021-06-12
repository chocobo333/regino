
%string = type { i8*, i32, i32 }

define i32 @len(%string) {
    %2 = extractvalue %string %0, 1
    ret i32 %2
}