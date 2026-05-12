	.globl	runtime::tape::checked_u32
	.p2align	2
runtime::tape::checked_u32:
Lfunc_begin44:
	.cfi_startproc
	lsr x8, x0, #32
	cbnz x8, LBB44_2
	ret
LBB44_2:
	sub sp, sp, #32
	.cfi_def_cfa_offset 32
	stp x29, x30, [sp, #16]
	add x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
Lloh582:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.36@PAGE
Lloh583:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.36@PAGEOFF
Lloh584:
	adrp x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGE
Lloh585:
	add x3, x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGEOFF
Lloh586:
	adrp x4, l_anon.10e61d2d870443aea44842dd9d471920.37@PAGE
Lloh587:
	add x4, x4, l_anon.10e61d2d870443aea44842dd9d471920.37@PAGEOFF
	sub x2, x29, #1
	mov w1, #52
	bl core::result::unwrap_failed
	.loh AdrpAdd	Lloh586, Lloh587
	.loh AdrpAdd	Lloh584, Lloh585
	.loh AdrpAdd	Lloh582, Lloh583
