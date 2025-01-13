	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$42, %edi
	callq	_putchar
	popq	%rax
	retq
	.cfi_endproc
                                        ## -- End function
.subsections_via_symbols
