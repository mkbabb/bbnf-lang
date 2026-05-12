.section __TEXT,__text,regular,pure_instructions
	.globl	simd_scan::scan_json_structurals
	.p2align	2
simd_scan::scan_json_structurals:
Lfunc_begin9:
	.cfi_startproc
	.cfi_personality 155, _rust_eh_personality
	.cfi_lsda 16, Lexception3
	sub sp, sp, #160
	.cfi_def_cfa_offset 160
	stp x28, x27, [sp, #64]
	stp x26, x25, [sp, #80]
	stp x24, x23, [sp, #96]
	stp x22, x21, [sp, #112]
	stp x20, x19, [sp, #128]
	stp x29, x30, [sp, #144]
	add x29, sp, #144
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
	.cfi_offset w27, -88
	.cfi_offset w28, -96
	.cfi_remember_state
	mov x20, x1
	mov x21, x0
	mov x23, x8
	lsr x8, x1, #3
	add x19, x8, #8
	lsl x22, x19, #2
	bl __rustc::__rust_no_alloc_shim_is_unstable_v2
	mov x0, x22
	mov w1, #4
	bl __rustc::__rust_alloc
	cbz x0, LBB9_18
	str x23, [sp, #8]
	stp x19, x0, [sp, #32]
	str xzr, [sp, #48]
	cmp x20, #64
	b.hs LBB9_5
	mov w25, #0
	mov w22, #0
	mov x23, #0
LBB9_3:
	add x3, sp, #32
	and w5, w25, #0x1
	mov x0, x21
	mov x1, x20
	mov x2, x23
	mov x4, x22
	bl simd_scan::scan_json_tail
	ldr q0, [sp, #32]
	ldr x9, [sp, #8]
	str q0, [x9]
	ldr x8, [sp, #48]
	str x8, [x9, #16]
	mov w8, #1
	strb w8, [x9, #24]
	.cfi_def_cfa wsp, 160
	ldp x29, x30, [sp, #144]
	ldp x20, x19, [sp, #128]
	ldp x22, x21, [sp, #112]
	ldp x24, x23, [sp, #96]
	ldp x26, x25, [sp, #80]
	ldp x28, x27, [sp, #64]
	add sp, sp, #160
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
	.cfi_restore w27
	.cfi_restore w28
	ret
LBB9_5:
	.cfi_restore_state
	mov x24, #0
	mov w22, #0
	mov w25, #0
	mov w8, #64
Lloh28:
	adrp x9, lCPI9_0@PAGE
Lloh29:
	ldr q0, [x9, lCPI9_0@PAGEOFF]
	str q0, [sp, #16]
	mov x26, #-6148914691236517206
	movk x26, #43689
	mov x27, #6148914691236517205
	movk x27, #21844
	b LBB9_7
LBB9_6:
	add x8, x23, #64
	mov x24, x23
	cmp x8, x20
	b.hi LBB9_3
LBB9_7:
	mov x23, x8
	add x8, x21, x24
	ldp q0, q1, [x8]
	movi.16b v7, #223
	and.16b v2, v0, v7
	movi.16b v16, #91
	cmeq.16b v3, v2, v16
	movi.16b v17, #93
	cmeq.16b v2, v2, v17
	orr.16b v2, v3, v2
	movi.16b v18, #58
	cmeq.16b v3, v0, v18
	movi.16b v19, #44
	cmeq.16b v4, v0, v19
	orr.16b v3, v4, v3
	orr.16b v2, v3, v2
	ldr q21, [sp, #16]
	and.16b v2, v2, v21
	addv.8b b3, v2
	umov.b w9, v3[0]
	movi.16b v20, #34
	cmeq.16b v3, v0, v20
	ext.16b v2, v2, v2, #8
	addv.8b b2, v2
	fmov w10, s2
	bfi x9, x10, #8, #8
	and.16b v2, v3, v21
	addv.8b b3, v2
	umov.b w10, v3[0]
	ext.16b v2, v2, v2, #8
	addv.8b b2, v2
	fmov w11, s2
	bfi x10, x11, #8, #8
	and.16b v2, v1, v7
	cmeq.16b v3, v2, v16
	cmeq.16b v2, v2, v17
	orr.16b v2, v3, v2
	cmeq.16b v3, v1, v18
	cmeq.16b v4, v1, v19
	orr.16b v3, v4, v3
	orr.16b v2, v3, v2
	cmeq.16b v3, v1, v20
	and.16b v2, v2, v21
	addv.8b b4, v2
	fmov w11, s4
	ext.16b v2, v2, v2, #8
	addv.8b b2, v2
	fmov w12, s2
	and.16b v2, v3, v21
	addv.8b b3, v2
	fmov w13, s3
	ext.16b v2, v2, v2, #8
	addv.8b b2, v2
	fmov w14, s2
	lsl w12, w12, #24
	bfi x12, x11, #16, #8
	orr x9, x12, x9
	lsl w11, w14, #24
	bfi x11, x13, #16, #8
	orr x10, x11, x10
	ldp q2, q3, [x8, #32]
	and.16b v4, v2, v7
	cmeq.16b v5, v4, v16
	cmeq.16b v4, v4, v17
	orr.16b v4, v5, v4
	cmeq.16b v5, v2, v18
	cmeq.16b v6, v2, v19
	orr.16b v5, v6, v5
	orr.16b v4, v5, v4
	cmeq.16b v5, v2, v20
	and.16b v4, v4, v21
	addv.8b b6, v4
	fmov w8, s6
	ext.16b v4, v4, v4, #8
	addv.8b b4, v4
	fmov w11, s4
	and.16b v4, v5, v21
	addv.8b b5, v4
	fmov w12, s5
	ext.16b v4, v4, v4, #8
	addv.8b b4, v4
	fmov w13, s4
	lsl x8, x8, #32
	orr x8, x8, x11, lsl #40
	lsl x11, x12, #32
	orr x11, x11, x13, lsl #40
	and.16b v4, v3, v7
	cmeq.16b v5, v4, v16
	cmeq.16b v4, v4, v17
	orr.16b v4, v5, v4
	cmeq.16b v5, v3, v18
	cmeq.16b v6, v3, v19
	orr.16b v5, v6, v5
	orr.16b v4, v5, v4
	cmeq.16b v5, v3, v20
	and.16b v4, v4, v21
	addv.8b b6, v4
	fmov w12, s6
	ext.16b v4, v4, v4, #8
	addv.8b b4, v4
	fmov w13, s4
	and.16b v4, v5, v21
	addv.8b b5, v4
	fmov w14, s5
	ext.16b v4, v4, v4, #8
	addv.8b b4, v4
	fmov w15, s4
	lsl x12, x12, #48
	orr x12, x12, x13, lsl #56
	orr x8, x12, x8
	orr x28, x8, x9
	lsl x8, x14, #48
	orr x8, x8, x15, lsl #56
	orr x8, x8, x11
	orr x8, x8, x10
	tbnz w22, #0, LBB9_10
	cbnz x8, LBB9_10
	mov w22, #0
	cbnz x28, LBB9_11
	b LBB9_6
LBB9_10:
	movi.16b v6, #92
	cmeq.16b v0, v0, v6
	ldr q5, [sp, #16]
	and.16b v0, v0, v5
	addv.8b b4, v0
	umov.b w9, v4[0]
	ext.16b v0, v0, v0, #8
	addv.8b b0, v0
	fmov w10, s0
	mov x11, x9
	bfi x11, x10, #8, #8
	cmeq.16b v0, v1, v6
	and.16b v0, v0, v5
	addv.8b b1, v0
	fmov w10, s1
	ext.16b v0, v0, v0, #8
	addv.8b b0, v0
	fmov w12, s0
	lsl w12, w12, #24
	bfi x12, x10, #16, #8
	orr x10, x12, x11
	cmeq.16b v0, v2, v6
	and.16b v0, v0, v5
	addv.8b b1, v0
	fmov w11, s1
	ext.16b v0, v0, v0, #8
	addv.8b b0, v0
	fmov w12, s0
	lsl x11, x11, #32
	orr x11, x11, x12, lsl #40
	cmeq.16b v0, v3, v6
	and.16b v0, v0, v5
	addv.8b b1, v0
	fmov w12, s1
	ext.16b v0, v0, v0, #8
	addv.8b b0, v0
	fmov w13, s0
	lsl x14, x13, #56
	orr x11, x11, x12, lsl #48
	orr x11, x11, x10
	bfi x11, x13, #56, #8
	mvn x12, x11
	cmn x11, #1
	clz x10, x12
	csel w10, w25, w10, eq
	tst x14, #0x8000000000000000
	csel w10, wzr, w10, eq
	and w13, w25, w9
	bic x14, x11, x11, lsl #1
	tst x9, #0x1
	cset w9, eq
	tst w13, #0x1
	cinc x13, x26, eq
	and x13, x14, x13
	and w9, w25, w9
	cinc x15, x27, eq
	and x14, x14, x15
	add x14, x14, x11
	and x14, x12, x14
	and x14, x14, #0xaaaaaaaaaaaaaaaa
	add x11, x13, x11
	and x11, x12, x11
	and x11, x11, #0x5555555555555555
	orr x9, x11, x9
	orr x9, x9, x14
	bic x8, x8, x9
	eor x9, x8, x8, lsl #1
	eor x9, x9, x9, lsl #2
	eor x9, x9, x9, lsl #4
	eor x9, x9, x9, lsl #8
	eor x9, x9, x9, lsl #16
	sbfx x11, x22, #0, #1
	eor x11, x11, x9, lsl #32
	eor x9, x11, x9
	lsr x22, x9, #63
	bic x9, x28, x9
	orr x28, x9, x8
	mov x25, x10
	cbz x28, LBB9_6
LBB9_11:
	lsr x8, x24, #32
	cbnz x8, LBB9_16
	ldr x19, [sp, #48]
	b LBB9_14
LBB9_13:
	rbit x8, x28
	clz x8, x8
	orr w8, w8, w24
	ldr x9, [sp, #40]
	str w8, [x9, x19, lsl #2]
	add x19, x19, #1
	str x19, [sp, #48]
	sub x8, x28, #1
	ands x28, x8, x28
	b.eq LBB9_6
LBB9_14:
	ldr x8, [sp, #32]
	cmp x19, x8
	b.ne LBB9_13
	add x0, sp, #32
	bl <alloc::raw_vec::RawVec<u32>>::grow_one
	b LBB9_13
LBB9_16:
Lloh30:
	adrp x0, l_anon.d7d05db1deb039881e193d033e31fb2c.4@PAGE
Lloh31:
	add x0, x0, l_anon.d7d05db1deb039881e193d033e31fb2c.4@PAGEOFF
Lloh32:
	adrp x3, l_anon.d7d05db1deb039881e193d033e31fb2c.8@PAGE
Lloh33:
	add x3, x3, l_anon.d7d05db1deb039881e193d033e31fb2c.8@PAGEOFF
Lloh34:
	adrp x4, l_anon.d7d05db1deb039881e193d033e31fb2c.5@PAGE
Lloh35:
	add x4, x4, l_anon.d7d05db1deb039881e193d033e31fb2c.5@PAGEOFF
	add x2, sp, #63
	mov w1, #52
	bl core::result::unwrap_failed
	brk #0x1
LBB9_18:
	mov w0, #4
	mov x1, x22
	bl alloc::raw_vec::handle_error
	b LBB9_21
LBB9_21:
	ldr x9, [sp, #32]
	cbz x9, LBB9_23
	ldr x8, [sp, #40]
	lsl x1, x9, #2
	mov x19, x0
	mov x0, x8
	mov w2, #4
	bl __rustc::__rust_dealloc
	mov x0, x19
LBB9_23:
	bl __Unwind_Resume
	.loh AdrpLdr	Lloh28, Lloh29
	.loh AdrpAdd	Lloh34, Lloh35
	.loh AdrpAdd	Lloh32, Lloh33
	.loh AdrpAdd	Lloh30, Lloh31
