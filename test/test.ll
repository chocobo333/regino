; ModuleID = 'main'
source_filename = "main"

define i32 @main() {
entry:
  %a = alloca i32, align 4
  store i32 2, i32* %a, align 4
  %a1 = alloca i32, align 4
  store i32 3, i32* %a1, align 4
  %b = alloca i32, align 4
  %f = load i32 (i32)*, i32 (i32)* @f, align 8
  %a2 = load i32*, i32* %a1, align 8
  %"f(a)" = call i32 %f(i32* %a2)
  store i32 %"f(a)", i32* %b, align 4
  %c = alloca i32, align 4
  %"+" = load i32 (i32, i32)*, i32 (i32, i32)* @"+", align 8
  %a3 = load i32*, i32* %a1, align 8
  %b4 = load i32*, i32* %b, align 8
  %"+(a, b)" = call i32 %"+"(i32* %a3, i32* %b4)
  store i32 %"+(a, b)", i32* %c, align 4
  %c5 = load i32*, i32* %c, align 8
  ret i32* %c5
}

define i32 @f(i32 %0) {
entry:
  %a = alloca i32, align 4
  store i32 %0, i32* %a, align 4
  %a1 = load i32*, i32* %a, align 8
  ret i32* %a1
}

define i32 @"+"(i32 %0, i32 %1) {
entry:
  %a = alloca i32, align 4
  store i32 %0, i32* %a, align 4
  %b = alloca i32, align 4
  store i32 %1, i32* %b, align 4
  %"add@int32/int32int32" = load i32 (i32, i32)*, i32 (i32, i32)* @"add@int32/int32int32", align 8
  %a1 = load i32*, i32* %a, align 8
  %b2 = load i32*, i32* %b, align 8
  %"add@int32/int32int32(a, b)" = call i32 %"add@int32/int32int32"(i32* %a1, i32* %b2)
  ret i32 %"add@int32/int32int32(a, b)"
}

define i32 @"add@int32/int32int32"(i32 %0, i32 %1) {
entry:
  %ret = add i32 %0, %1
  ret i32 %ret
}

define i32 @"sub@int32/int32int32"(i32 %0, i32 %1) {
entry:
  %ret = sub i32 %0, %1
  ret i32 %ret
}
