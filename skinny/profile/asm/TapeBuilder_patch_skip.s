	.globl	<runtime::tape::TapeBuilder>::patch_skip
	.p2align	2
<runtime::tape::TapeBuilder>::patch_skip:
Lfunc_begin29:
	.cfi_startproc
	sub sp, sp, #32
	.cfi_def_cfa_offset 32
	stp x29, x30, [sp, #16]
	add x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_remember_state
	lsr x9, x2, #32
	cbnz x9, LBB29_3
	mov x8, x0
	mov w0, w1
	ldr x1, [x8, #16]
	cmp x1, x0
	b.ls LBB29_4
	ldr x8, [x8, #8]
	add x8, x8, x0, lsl #4
	str w2, [x8, #12]
	.cfi_def_cfa wsp, 32
	ldp x29, x30, [sp, #16]
	add sp, sp, #32
	.cfi_def_cfa_offset 0
	.cfi_restore w30
	.cfi_restore w29
	ret
LBB29_3:
	.cfi_restore_state
Lloh484:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.36@PAGE
Lloh485:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.36@PAGEOFF
Lloh486:
	adrp x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGE
Lloh487:
	add x3, x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGEOFF
Lloh488:
	adrp x4, l_anon.10e61d2d870443aea44842dd9d471920.37@PAGE
Lloh489:
	add x4, x4, l_anon.10e61d2d870443aea44842dd9d471920.37@PAGEOFF
	sub x2, x29, #1
	mov w1, #52
	bl core::result::unwrap_failed
LBB29_4:
Lloh490:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.24@PAGE
Lloh491:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.24@PAGEOFF
	bl core::panicking::panic_bounds_check
	.loh AdrpAdd	Lloh488, Lloh489
	.loh AdrpAdd	Lloh486, Lloh487
	.loh AdrpAdd	Lloh484, Lloh485
	.loh AdrpAdd	Lloh490, Lloh491
