	.text
	.file	"stdlib_io.ll"
	.globl	stdlib_io_print         # -- Begin function stdlib_io_print
	.p2align	4, 0x90
	.type	stdlib_io_print,@function
stdlib_io_print:                        # @stdlib_io_print
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)          # 8-byte Spill
	jmp	.LBB0_1
.LBB0_1:                                # %foreach.decl
	movq	%rsp, %rax
	addq	$-16, %rax
	movq	%rax, %rsp
	movq	%rsp, %rcx
	addq	$-16, %rcx
	movq	%rcx, %rsp
	movq	$0, (%rcx)
	movq	-8(%rbp), %rdx          # 8-byte Reload
	movq	(%rdx), %rsi
	movq	%rax, -16(%rbp)         # 8-byte Spill
	movq	%rcx, -24(%rbp)         # 8-byte Spill
	movq	%rsi, -32(%rbp)         # 8-byte Spill
.LBB0_2:                                # %foreach.test
                                        # =>This Inner Loop Header: Depth=1
	movq	-24(%rbp), %rax         # 8-byte Reload
	movq	(%rax), %rcx
	movq	-32(%rbp), %rdx         # 8-byte Reload
	cmpq	%rcx, %rdx
	movq	%rcx, -40(%rbp)         # 8-byte Spill
	je	.LBB0_4
# %bb.3:                                # %foreach.loop
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	-8(%rbp), %rax          # 8-byte Reload
	movq	-40(%rbp), %rcx         # 8-byte Reload
	movb	8(%rax,%rcx), %dl
	movq	-16(%rbp), %rsi         # 8-byte Reload
	movb	%dl, (%rsi)
	movzbl	(%rsi), %edi
	callq	stdlib_io_printc@PLT
	movq	-40(%rbp), %rcx         # 8-byte Reload
	addq	$1, %rcx
	movq	-24(%rbp), %rsi         # 8-byte Reload
	movq	%rcx, (%rsi)
	movq	%rax, -48(%rbp)         # 8-byte Spill
	jmp	.LBB0_2
.LBB0_4:                                # %foreach.exit
	xorl	%eax, %eax
                                        # kill: def %rax killed %eax
	movq	%rbp, %rsp
	popq	%rbp
	retq
.Lfunc_end0:
	.size	stdlib_io_print, .Lfunc_end0-stdlib_io_print
	.cfi_endproc
                                        # -- End function
	.globl	stdlib_io_printc        # -- Begin function stdlib_io_printc
	.p2align	4, 0x90
	.type	stdlib_io_printc,@function
stdlib_io_printc:                       # @stdlib_io_printc
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movb	%dil, %al
	movzbl	%al, %edi
	callq	putchar@PLT
	xorl	%edi, %edi
	movl	%edi, %ecx
	movl	%eax, 4(%rsp)           # 4-byte Spill
	movq	%rcx, %rax
	popq	%rcx
	retq
.Lfunc_end1:
	.size	stdlib_io_printc, .Lfunc_end1-stdlib_io_printc
	.cfi_endproc
                                        # -- End function

	.section	".note.GNU-stack","",@progbits
