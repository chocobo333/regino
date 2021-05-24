	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 11, 0
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
## %bb.0:                               ## %entry
	movl	$6, %eax
	retq
                                        ## -- End function
	.globl	_f                              ## -- Begin function f
	.p2align	4, 0x90
_f:                                     ## @f
## %bb.0:                               ## %entry
	movl	%edi, %eax
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
.subsections_via_symbols
