	.globl	<runtime::tape::TapeBuilder>::patch_skip_to_current_len
	.p2align	2
<runtime::tape::TapeBuilder>::patch_skip_to_current_len:
Lfunc_begin32:
	.cfi_startproc
	sub sp, sp, #32
	.cfi_def_cfa_offset 32
	stp x29, x30, [sp, #16]
	add x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_remember_state
	mov x9, x1
	mov x8, x0
	ldr x1, [x0, #16]
	mov w0, w9
	subs x9, x1, x0
	lsr x10, x9, #32
	cbnz x10, LBB32_3
	b.ls LBB32_4
	ldr x8, [x8, #8]
	add x8, x8, x0, lsl #4
	str w9, [x8, #12]
	.cfi_def_cfa wsp, 32
	ldp x29, x30, [sp, #16]
	add sp, sp, #32
	.cfi_def_cfa_offset 0
	.cfi_restore w30
	.cfi_restore w29
	ret
LBB32_3:
	.cfi_restore_state
Lloh492:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.36@PAGE
Lloh493:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.36@PAGEOFF
Lloh494:
	adrp x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGE
Lloh495:
	add x3, x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGEOFF
Lloh496:
	adrp x4, l_anon.10e61d2d870443aea44842dd9d471920.37@PAGE
Lloh497:
	add x4, x4, l_anon.10e61d2d870443aea44842dd9d471920.37@PAGEOFF
	sub x2, x29, #1
	mov w1, #52
	bl core::result::unwrap_failed
LBB32_4:
Lloh498:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.25@PAGE
Lloh499:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.25@PAGEOFF
	bl core::panicking::panic_bounds_check
	.loh AdrpAdd	Lloh496, Lloh497
	.loh AdrpAdd	Lloh494, Lloh495
	.loh AdrpAdd	Lloh492, Lloh493
	.loh AdrpAdd	Lloh498, Lloh499
