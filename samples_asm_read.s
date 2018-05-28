	.text
	.file	"samples_asm_read.ll"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	movq	$2, -8(%rsp)
	movq	$1, -16(%rsp)
	movq	-8(%rsp), %rax
	movq	-16(%rsp), %rcx
	#APP
	addq	%rcx, %rax
	#NO_APP
	movq	%rax, -16(%rsp)
	movq	-16(%rsp), %rax
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function

	.section	".note.GNU-stack","",@progbits
