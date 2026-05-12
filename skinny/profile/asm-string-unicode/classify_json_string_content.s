.section __TEXT,__text,regular,pure_instructions
	.globl	parse_that_regex::classify_json_string_content
	.p2align	2
parse_that_regex::classify_json_string_content:
Lfunc_begin4:
	.cfi_startproc
	stp x29, x30, [sp, #-16]!
	.cfi_def_cfa_offset 16
	mov x29, sp
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_remember_state
	movi.16b v0, #32
Lloh8:
	adrp x9, lCPI4_0@PAGE
Lloh9:
	ldr q1, [x9, lCPI4_0@PAGEOFF]
	movi.16b v2, #92
LBB4_1:
	add x9, x2, #16
	cmp x9, x3
	b.hi LBB4_5
	ldr q3, [x0, x2]
	cmhi.16b v4, v0, v3
	and.16b v4, v4, v1
	addv.8b b5, v4
	fmov w10, s5
	ext.16b v4, v4, v4, #8
	addv.8b b4, v4
	fmov w11, s4
	bfi w10, w11, #8, #24
	tst w10, #0xffff
	b.ne LBB4_12
	cmeq.16b v3, v3, v2
	and.16b v3, v3, v1
	addv.8b b4, v3
	fmov w10, s4
	ext.16b v3, v3, v3, #8
	addv.8b b3, v3
	fmov w11, s3
	orr w10, w11, w10
	mov x2, x9
	tst w10, #0xff
	b.eq LBB4_1
LBB4_4:
	mov w9, #1
	strb w9, [x8]
	b LBB4_15
LBB4_5:
	cmp x3, x1
	b.hi LBB4_17
	cmp x3, x2
	b.lo LBB4_17
	sub x9, x3, x2
	mov w10, #1
	sub x10, x10, x2
	add x11, x0, x2
LBB4_8:
	cbz x9, LBB4_14
	ldrb w12, [x11], #1
	cmp w12, #92
	b.eq LBB4_4
	sub x10, x10, #1
	sub x9, x9, #1
	cmp w12, #31
	b.hi LBB4_8
	neg x9, x10
	b LBB4_13
LBB4_12:
	rbit w9, w10
	clz w9, w9
	add x9, x9, x2
LBB4_13:
	str x9, [x8]
	mov w9, #2
	b LBB4_16
LBB4_14:
	strb wzr, [x8]
LBB4_15:
	mov w9, #6
LBB4_16:
	strb w9, [x8, #8]
	.cfi_def_cfa wsp, 16
	ldp x29, x30, [sp], #16
	.cfi_def_cfa_offset 0
	.cfi_restore w30
	.cfi_restore w29
	ret
LBB4_17:
	.cfi_restore_state
Lloh10:
	adrp x8, l_anon.028aad4ed9a93f18e57e62edb80815c2.3@PAGE
Lloh11:
	add x8, x8, l_anon.028aad4ed9a93f18e57e62edb80815c2.3@PAGEOFF
	mov x0, x2
	mov x2, x1
	mov x1, x3
	mov x3, x8
	bl core::slice::index::slice_index_fail
	.loh AdrpLdr	Lloh8, Lloh9
	.loh AdrpAdd	Lloh10, Lloh11
