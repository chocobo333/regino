define i32 @"add@int32/int32int32" (i32, i32) {
    entry:
        %ret = add i32 %0, %1
        ret i32 %ret
}
define i32 @"sub@int32/int32int32" (i32, i32) {
    entry:
        %ret = sub i32 %0, %1
        ret i32 %ret
}

define i1 @"eq@bool/int32int32" (i32, i32) {
    entry:
        %ret = icmp eq i32 %0, %1
        ret i1 %ret
}