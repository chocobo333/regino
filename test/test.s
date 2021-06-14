	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 11, 0
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
## %bb.0:                               ## %entry
	pushq	%rax
	movl	$11, %edi
	callq	_fib
	leaq	"_hello world"(%rip), %rdi
	callq	_puts
	movl	$11, %eax
	popq	%rcx
	retq
                                        ## -- End function
	.globl	_f                              ## -- Begin function f
	.p2align	4, 0x90
_f:                                     ## @f
## %bb.0:                               ## %entry
	movl	%edi, %eax
	retq
                                        ## -- End function
	.globl	_f.1                            ## -- Begin function f.1
	.p2align	4, 0x90
_f.1:                                   ## @f.1
## %bb.0:                               ## %entry
	retq
                                        ## -- End function
	.globl	_f.2                            ## -- Begin function f.2
	.p2align	4, 0x90
_f.2:                                   ## @f.2
## %bb.0:                               ## %entry
	movl	%edi, %eax
                                        ## kill: def $al killed $al killed $eax
	retq
                                        ## -- End function
	.globl	"_+"                            ## -- Begin function +
	.p2align	4, 0x90
"_+":                                   ## @"+"
## %bb.0:                               ## %entry
                                        ## kill: def $esi killed $esi def $rsi
                                        ## kill: def $edi killed $edi def $rdi
	leal	(%rdi,%rsi), %eax
	retq
                                        ## -- End function
	.globl	"_-"                            ## -- Begin function -
	.p2align	4, 0x90
"_-":                                   ## @-
## %bb.0:                               ## %entry
	movl	%edi, %eax
	subl	%esi, %eax
	retq
                                        ## -- End function
	.globl	"_=="                           ## -- Begin function ==
	.p2align	4, 0x90
"_==":                                  ## @"=="
## %bb.0:                               ## %entry
	cmpl	%esi, %edi
	sete	%al
	retq
                                        ## -- End function
	.globl	_fib                            ## -- Begin function fib
	.p2align	4, 0x90
_fib:                                   ## @fib
## %bb.0:                               ## %entry
	pushq	%rbp
	pushq	%rbx
	pushq	%rax
	movl	$1, %ebp
	cmpl	$2, %edi
	jb	LBB7_4
## %bb.1:                               ## %else.preheader
	movl	%edi, %ebx
	xorl	%ebp, %ebp
	.p2align	4, 0x90
LBB7_2:                                 ## %else
                                        ## =>This Inner Loop Header: Depth=1
	leal	-1(%rbx), %edi
	callq	_fib
	addl	$-2, %ebx
	addl	%eax, %ebp
	cmpl	$1, %ebx
	ja	LBB7_2
## %bb.3:                               ## %ifcont.loopexit
	incl	%ebp
LBB7_4:                                 ## %ifcont
	movl	%ebp, %eax
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	retq
                                        ## -- End function
	.globl	"_add@int32/int32int32"         ## -- Begin function add@int32/int32int32
	.p2align	4, 0x90
"_add@int32/int32int32":                ## @"add@int32/int32int32"
## %bb.0:                               ## %entry
                                        ## kill: def $esi killed $esi def $rsi
                                        ## kill: def $edi killed $edi def $rdi
	leal	(%rdi,%rsi), %eax
	retq
                                        ## -- End function
	.globl	"_sub@int32/int32int32"         ## -- Begin function sub@int32/int32int32
	.p2align	4, 0x90
"_sub@int32/int32int32":                ## @"sub@int32/int32int32"
## %bb.0:                               ## %entry
	movl	%edi, %eax
	subl	%esi, %eax
	retq
                                        ## -- End function
	.globl	"_eq@bool/int32int32"           ## -- Begin function eq@bool/int32int32
	.p2align	4, 0x90
"_eq@bool/int32int32":                  ## @"eq@bool/int32int32"
## %bb.0:                               ## %entry
	cmpl	%esi, %edi
	sete	%al
	retq
                                        ## -- End function
	.globl	_echo                           ## -- Begin function echo
	.p2align	4, 0x90
_echo:                                  ## @echo
## %bb.0:
	jmp	_puts                           ## TAILCALL
                                        ## -- End function
	.globl	"_tos@string/i32"               ## -- Begin function tos@string/i32
	.p2align	4, 0x90
"_tos@string/i32":                      ## @"tos@string/i32"
## %bb.0:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	pushq	%rax
	movq	%rsi, %r15
	movl	%edi, %r14d
	cvtsi2ss	%edi, %xmm0
	callq	_log10f
	cvttss2si	%xmm0, %ebp
	incl	%ebp
	movl	%ebp, %edi
	callq	_malloc
	movq	%rax, %rbx
	leaq	_fmt(%rip), %rsi
	movq	%rax, %rdi
	movl	%r14d, %edx
	xorl	%eax, %eax
	callq	_sprintf
	movq	%rbx, (%r15)
	movl	%ebp, 8(%r15)
	movl	%ebp, 12(%r15)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
                                        ## -- End function
	.globl	_len                            ## -- Begin function len
	.p2align	4, 0x90
_len:                                   ## @len
## %bb.0:
	movl	%esi, %eax
	retq
                                        ## -- End function
	.section	__TEXT,__const
	.globl	"_hello world"                  ## @"hello world"
"_hello world":
	.ascii	"hello world"

	.globl	_fmt                            ## @fmt
_fmt:
	.ascii	"%d"

.subsections_via_symbols
