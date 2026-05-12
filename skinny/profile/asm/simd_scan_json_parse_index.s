.section __TEXT,__text,regular,pure_instructions
	.globl	simd_scan::scan_json_parse_index
	.p2align	2
simd_scan::scan_json_parse_index:
Lfunc_begin8:
	.cfi_startproc
	.cfi_personality 155, _rust_eh_personality
	.cfi_lsda 16, Lexception2
	sub sp, sp, #320
	.cfi_def_cfa_offset 320
	stp x28, x27, [sp, #224]
	stp x26, x25, [sp, #240]
	stp x24, x23, [sp, #256]
	stp x22, x21, [sp, #272]
	stp x20, x19, [sp, #288]
	stp x29, x30, [sp, #304]
	add x29, sp, #304
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
	mov x21, x1
	str x0, [sp, #32]
	mov x20, x8
	lsr x8, x1, #3
	add x19, x8, #8
	lsl x23, x19, #2
	bl __rustc::__rust_no_alloc_shim_is_unstable_v2
	mov x0, x23
	mov w1, #4
	bl __rustc::__rust_alloc
	cbz x0, LBB8_63
	mov x22, x0
	stp x19, x0, [sp, #136]
	str xzr, [sp, #152]
	lsr x8, x21, #9
	add x19, x8, #1
	lsl x23, x19, #2
	bl __rustc::__rust_no_alloc_shim_is_unstable_v2
	mov x0, x23
	mov w1, #4
	bl __rustc::__rust_alloc
	cbz x0, LBB8_64
	str x20, [sp, #8]
	stp x19, x0, [x29, #-144]
	stp xzr, xzr, [x29, #-128]
	mov w8, #4
	stp x8, xzr, [x29, #-112]
	str x21, [sp, #40]
	cmp x21, #64
	b.hs LBB8_40
	mov x23, #0
	mov w10, #0
	mov w19, #0
	mov x25, #0
LBB8_4:
	ldr x20, [sp, #40]
	cmp x25, x20
	b.hs LBB8_39
	mov x21, x10
	mov w22, #1
	mov x24, #1024
	movk x24, #256, lsl #16
	movk x24, #2560, lsl #48
	b LBB8_9
LBB8_6:
	mov w19, #0
	ldr x20, [sp, #40]
	mov x25, x9
LBB8_7:
	ldr x8, [sp, #144]
	str w25, [x8, x23, lsl #2]
	add x23, x23, #1
	str x23, [sp, #152]
LBB8_8:
	add x25, x25, #1
	cmp x25, x20
	b.hs LBB8_39
LBB8_9:
	ldr x8, [sp, #32]
	ldrb w8, [x8, x25]
	tbnz w19, #0, LBB8_22
	sub w9, w8, #34
	cmp w9, #59
	b.hi LBB8_19
	lsl x10, x22, x9
	tst x10, x24
	b.eq LBB8_16
LBB8_12:
	lsr x8, x25, #32
	cbnz x8, LBB8_62
	mov x9, x25
	ldr x8, [sp, #136]
	cmp x23, x8
	b.ne LBB8_6
	mov w19, #0
	ldr x20, [sp, #40]
	mov x25, x9
LBB8_15:
	add x0, sp, #136
	bl <alloc::raw_vec::RawVec<u32>>::grow_one
	b LBB8_7
LBB8_16:
	cbnz x9, LBB8_19
	lsr x8, x25, #32
	cbnz x8, LBB8_62
	ldr x8, [sp, #136]
	mov w19, #1
	cmp x23, x8
	ldr x20, [sp, #40]
	b.ne LBB8_7
	b LBB8_15
LBB8_19:
	cmp w8, #123
	b.eq LBB8_12
	cmp w8, #125
	b.eq LBB8_12
	mov w19, #0
	b LBB8_8
LBB8_22:
	tbz w21, #0, LBB8_24
LBB8_23:
	mov w21, #0
	mov w19, #1
	b LBB8_8
LBB8_24:
	cmp w8, #34
	b.eq LBB8_30
	cmp w8, #92
	b.ne LBB8_34
	lsr x8, x25, #32
	cbnz x8, LBB8_62
	mov x20, x25
	ldur x19, [x29, #-128]
	ldur x8, [x29, #-144]
	cmp x19, x8
	b.ne LBB8_29
	sub x0, x29, #144
	bl <alloc::raw_vec::RawVec<u32>>::grow_one
LBB8_29:
	ldur x8, [x29, #-136]
	mov x25, x20
	str w25, [x8, x19, lsl #2]
	add x8, x19, #1
	stur x8, [x29, #-128]
	mov w21, #1
	mov w19, #1
	ldr x20, [sp, #40]
	b LBB8_8
LBB8_30:
	lsr x8, x25, #32
	cbnz x8, LBB8_62
	mov x20, x25
	ldr x8, [sp, #136]
	cmp x23, x8
	b.ne LBB8_33
	add x0, sp, #136
	bl <alloc::raw_vec::RawVec<u32>>::grow_one
LBB8_33:
	mov w21, #0
	mov w19, #0
	ldr x8, [sp, #144]
	mov x25, x20
	str w25, [x8, x23, lsl #2]
	add x23, x23, #1
	str x23, [sp, #152]
	ldr x20, [sp, #40]
	b LBB8_8
LBB8_34:
	cmp w8, #32
	b.hs LBB8_23
	lsr x8, x25, #32
	cbnz x8, LBB8_62
	mov x20, x25
	ldur x19, [x29, #-104]
	ldur x8, [x29, #-120]
	cmp x19, x8
	b.ne LBB8_38
	sub x0, x29, #120
	bl <alloc::raw_vec::RawVec<u32>>::grow_one
LBB8_38:
	mov w21, #0
	ldur x8, [x29, #-112]
	mov x25, x20
	str w25, [x8, x19, lsl #2]
	add x8, x19, #1
	stur x8, [x29, #-104]
	mov w19, #1
	ldr x20, [sp, #40]
	b LBB8_8
LBB8_39:
	ldur q0, [sp, #136]
	str q0, [sp, #64]
	ldr x8, [sp, #152]
	str x8, [sp, #80]
	ldur q1, [x29, #-144]
	stur q1, [sp, #88]
	ldur x9, [x29, #-128]
	str x9, [sp, #104]
	ldur q2, [x29, #-120]
	str q2, [sp, #112]
	ldur x10, [x29, #-104]
	str x10, [sp, #128]
	ldr x10, [sp, #8]
	str q0, [x10]
	str x8, [x10, #16]
	stur q1, [x10, #24]
	str x9, [x10, #40]
	ldr q0, [sp, #112]
	str q0, [x10, #48]
	ldr x8, [sp, #128]
	str x8, [x10, #64]
	.cfi_def_cfa wsp, 320
	ldp x29, x30, [sp, #304]
	ldp x20, x19, [sp, #288]
	ldp x22, x21, [sp, #272]
	ldp x24, x23, [sp, #256]
	ldp x26, x25, [sp, #240]
	ldp x28, x27, [sp, #224]
	add sp, sp, #320
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
LBB8_40:
	.cfi_restore_state
	mov x23, #0
	mov x25, #0
	mov w19, #0
	mov w10, #0
	mov w12, #64
Lloh20:
	adrp x8, lCPI8_0@PAGE
Lloh21:
	ldr q0, [x8, lCPI8_0@PAGEOFF]
	str q0, [sp, #16]
	ldr x28, [sp, #32]
	b LBB8_42
LBB8_41:
	lsr x19, x19, #63
	add x12, x25, #64
	add x28, x28, #64
	ldr x8, [sp, #40]
	cmp x12, x8
	ldr w10, [sp, #52]
	b.hi LBB8_4
LBB8_42:
	mov x11, #0
	mov x8, #0
	mov x9, #0
	mov x21, #0
	mov x27, #0
	mov x24, x25
	str x12, [sp, #56]
	movi.16b v5, #223
	movi.16b v6, #91
	movi.16b v7, #93
	movi.16b v16, #58
	movi.16b v17, #44
	movi.16b v18, #34
	movi.16b v19, #92
	ldr q20, [sp, #16]
	movi.16b v21, #32
LBB8_43:
	ldr q0, [x28, x11]
	and.16b v1, v0, v5
	cmeq.16b v2, v1, v6
	cmeq.16b v1, v1, v7
	orr.16b v1, v2, v1
	cmeq.16b v2, v0, v16
	cmeq.16b v3, v0, v17
	orr.16b v2, v3, v2
	orr.16b v1, v2, v1
	cmeq.16b v2, v0, v18
	cmeq.16b v3, v0, v19
	cmhi.16b v0, v21, v0
	and.16b v1, v1, v20
	addv.8b b4, v1
	umov.b w12, v4[0]
	ext.16b v1, v1, v1, #8
	addv.8b b1, v1
	fmov w13, s1
	bfi x12, x13, #8, #8
	and.16b v1, v2, v20
	addv.8b b2, v1
	umov.b w13, v2[0]
	ext.16b v1, v1, v1, #8
	addv.8b b1, v1
	and.16b v2, v3, v20
	addv.8b b3, v2
	umov.b w14, v3[0]
	fmov w15, s1
	ext.16b v1, v2, v2, #8
	addv.8b b1, v1
	fmov w16, s1
	and.16b v0, v0, v20
	addv.8b b1, v0
	umov.b w17, v1[0]
	ext.16b v0, v0, v0, #8
	addv.8b b0, v0
	fmov w0, s0
	bfi x17, x0, #8, #8
	bfi x14, x16, #8, #8
	bfi x13, x15, #8, #8
	lsl x12, x12, x11
	orr x8, x12, x8
	lsl x12, x13, x11
	orr x9, x12, x9
	lsl x12, x14, x11
	orr x21, x12, x21
	lsl x12, x17, x11
	orr x27, x12, x27
	add x11, x11, #16
	cmp x11, #64
	b.ne LBB8_43
	mvn x11, x21
	cmn x21, #1
	clz x12, x11
	csel w12, w10, w12, eq
	tst x21, #0x8000000000000000
	csel w12, wzr, w12, eq
	str w12, [sp, #52]
	and w12, w10, w21
	bic x13, x21, x21, lsl #1
	tst x21, #0x1
	cset w14, eq
	tst w12, #0x1
	mov x12, #-6148914691236517206
	movk x12, #43689
	cinc x12, x12, eq
	and x12, x12, x13
	and w10, w10, w14
	mov x14, #6148914691236517205
	movk x14, #21844
	cinc x14, x14, eq
	and x13, x14, x13
	add x13, x13, x21
	and x13, x11, x13
	and x13, x13, #0xaaaaaaaaaaaaaaaa
	add x12, x12, x21
	and x11, x11, x12
	and x11, x11, #0x5555555555555555
	orr x10, x11, x10
	orr x10, x10, x13
	bic x25, x9, x10
	eor x9, x25, x25, lsl #1
	eor x9, x9, x9, lsl #2
	eor x9, x9, x9, lsl #4
	eor x9, x9, x9, lsl #8
	eor x9, x9, x9, lsl #16
	sbfx x10, x19, #0, #1
	eor x10, x10, x9, lsl #32
	eor x19, x10, x9
	bic x8, x8, x19
	lsr x20, x24, #32
	orr x26, x8, x25
	cbz x26, LBB8_50
	cbz x20, LBB8_48
	b LBB8_62
LBB8_46:
	ldr x22, [sp, #144]
LBB8_47:
	rbit x8, x26
	clz x8, x8
	orr w8, w8, w24
	str w8, [x22, x23, lsl #2]
	add x23, x23, #1
	str x23, [sp, #152]
	sub x8, x26, #1
	ands x26, x8, x26
	b.eq LBB8_50
LBB8_48:
	ldr x8, [sp, #136]
	cmp x23, x8
	b.ne LBB8_47
	add x0, sp, #136
	bl <alloc::raw_vec::RawVec<u32>>::grow_one
	b LBB8_46
LBB8_50:
	eor x25, x19, x25
	ands x21, x25, x21
	b.eq LBB8_56
	cbnz x20, LBB8_62
	ldur x26, [x29, #-128]
	b LBB8_54
LBB8_53:
	rbit x8, x21
	clz x8, x8
	orr w8, w8, w24
	ldur x9, [x29, #-136]
	str w8, [x9, x26, lsl #2]
	add x26, x26, #1
	stur x26, [x29, #-128]
	sub x8, x21, #1
	ands x21, x8, x21
	b.eq LBB8_56
LBB8_54:
	ldur x8, [x29, #-144]
	cmp x26, x8
	b.ne LBB8_53
	sub x0, x29, #144
	bl <alloc::raw_vec::RawVec<u32>>::grow_one
	b LBB8_53
LBB8_56:
	ands x21, x25, x27
	ldr x25, [sp, #56]
	b.eq LBB8_41
	cbnz x20, LBB8_62
	ldur x20, [x29, #-104]
	ldr x25, [sp, #56]
	b LBB8_60
LBB8_59:
	rbit x8, x21
	clz x8, x8
	orr w8, w8, w24
	ldur x9, [x29, #-112]
	str w8, [x9, x20, lsl #2]
	add x20, x20, #1
	stur x20, [x29, #-104]
	sub x8, x21, #1
	ands x21, x8, x21
	b.eq LBB8_41
LBB8_60:
	ldur x8, [x29, #-120]
	cmp x20, x8
	b.ne LBB8_59
	sub x0, x29, #120
	bl <alloc::raw_vec::RawVec<u32>>::grow_one
	b LBB8_59
LBB8_62:
Lloh22:
	adrp x0, l_anon.d7d05db1deb039881e193d033e31fb2c.4@PAGE
Lloh23:
	add x0, x0, l_anon.d7d05db1deb039881e193d033e31fb2c.4@PAGEOFF
Lloh24:
	adrp x3, l_anon.d7d05db1deb039881e193d033e31fb2c.8@PAGE
Lloh25:
	add x3, x3, l_anon.d7d05db1deb039881e193d033e31fb2c.8@PAGEOFF
Lloh26:
	adrp x4, l_anon.d7d05db1deb039881e193d033e31fb2c.5@PAGE
Lloh27:
	add x4, x4, l_anon.d7d05db1deb039881e193d033e31fb2c.5@PAGEOFF
	sub x2, x29, #89
	mov w1, #52
	bl core::result::unwrap_failed
	b LBB8_65
LBB8_63:
	mov w0, #4
	mov x1, x23
	bl alloc::raw_vec::handle_error
LBB8_64:
	mov w0, #4
	mov x1, x23
	bl alloc::raw_vec::handle_error
LBB8_65:
	brk #0x1
	b LBB8_73
	mov x19, x0
	ldr x8, [sp, #136]
	cbnz x8, LBB8_76
	b LBB8_77
	b LBB8_73
	b LBB8_73
	b LBB8_73
	b LBB8_73
LBB8_73:
	mov x19, x0
	ldur x8, [x29, #-120]
	cbnz x8, LBB8_78
	ldur x8, [x29, #-144]
	cbnz x8, LBB8_79
LBB8_75:
	ldr x8, [sp, #136]
	cbz x8, LBB8_77
LBB8_76:
	ldr x0, [sp, #144]
	lsl x1, x8, #2
	mov w2, #4
	bl __rustc::__rust_dealloc
LBB8_77:
	mov x0, x19
	bl __Unwind_Resume
LBB8_78:
	ldur x0, [x29, #-112]
	lsl x1, x8, #2
	mov w2, #4
	bl __rustc::__rust_dealloc
	ldur x8, [x29, #-144]
	cbz x8, LBB8_75
LBB8_79:
	ldur x0, [x29, #-136]
	lsl x1, x8, #2
	mov w2, #4
	bl __rustc::__rust_dealloc
	ldr x8, [sp, #136]
	cbnz x8, LBB8_76
	b LBB8_77
	.loh AdrpLdr	Lloh20, Lloh21
	.loh AdrpAdd	Lloh26, Lloh27
	.loh AdrpAdd	Lloh24, Lloh25
	.loh AdrpAdd	Lloh22, Lloh23
