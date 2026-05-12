	.globl	<runtime::tape::TapeBuilder>::emit
	.p2align	2
<runtime::tape::TapeBuilder>::emit:
Lfunc_begin33:
	.cfi_startproc
	sub sp, sp, #96
	.cfi_def_cfa_offset 96
	stp x26, x25, [sp, #16]
	stp x24, x23, [sp, #32]
	stp x22, x21, [sp, #48]
	stp x20, x19, [sp, #64]
	stp x29, x30, [sp, #80]
	add x29, sp, #80
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_offset w23, -56
	.cfi_offset w24, -64
	.cfi_offset w25, -72
	.cfi_offset w26, -80
	.cfi_remember_state
	mov x19, x0
	ldr x0, [x0, #16]
	lsr x9, x0, #32
	cbnz x9, LBB33_6
	lsr x9, x3, #32
	cbnz x9, LBB33_6
	lsr x9, x4, #32
	cbnz x9, LBB33_6
	ldr x9, [x19]
	cmp x0, x9
	b.eq LBB33_5
LBB33_4:
	ldr x9, [x19, #8]
	add x9, x9, x0, lsl #4
	strh w1, [x9]
	strh w2, [x9, #2]
	stp w3, w4, [x9, #4]
	str w5, [x9, #12]
	add x9, x0, #1
	str x9, [x19, #16]
	.cfi_def_cfa wsp, 96
	ldp x29, x30, [sp, #80]
	ldp x20, x19, [sp, #64]
	ldp x22, x21, [sp, #48]
	ldp x24, x23, [sp, #32]
	ldp x26, x25, [sp, #16]
	add sp, sp, #96
	.cfi_def_cfa_offset 0
	.cfi_restore w30
	.cfi_restore w29
	.cfi_restore w19
	.cfi_restore w20
	.cfi_restore w21
	.cfi_restore w22
	.cfi_restore w23
	.cfi_restore w24
	.cfi_restore w25
	.cfi_restore w26
	ret
LBB33_5:
	.cfi_restore_state
	mov x20, x0
	mov x0, x19
	mov x21, x5
	mov x24, x4
	mov x22, x3
	mov x25, x2
	mov x23, x1
	bl <alloc::raw_vec::RawVec<runtime::tape::TapeToken>>::grow_one
	mov x1, x23
	mov x2, x25
	mov x3, x22
	mov x0, x20
	mov x4, x24
	mov x5, x21
	b LBB33_4
LBB33_6:
Lloh500:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.36@PAGE
Lloh501:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.36@PAGEOFF
Lloh502:
	adrp x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGE
Lloh503:
	add x3, x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGEOFF
Lloh504:
	adrp x4, l_anon.10e61d2d870443aea44842dd9d471920.37@PAGE
Lloh505:
	add x4, x4, l_anon.10e61d2d870443aea44842dd9d471920.37@PAGEOFF
	add x2, sp, #15
	mov w1, #52
	bl core::result::unwrap_failed
	.loh AdrpAdd	Lloh504, Lloh505
	.loh AdrpAdd	Lloh502, Lloh503
	.loh AdrpAdd	Lloh500, Lloh501
