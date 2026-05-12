runtime::generated_json::generated::parse_literal:
Lfunc_begin53:
	.cfi_startproc
	sub sp, sp, #128
	.cfi_def_cfa_offset 128
	stp x24, x23, [sp, #64]
	stp x22, x21, [sp, #80]
	stp x20, x19, [sp, #96]
	stp x29, x30, [sp, #112]
	add x29, sp, #112
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_offset w23, -56
	.cfi_offset w24, -64
	.cfi_remember_state
	mov x21, x3
	mov x22, x2
	mov x20, x1
	mov x19, x0
	ldr x23, [x1, #168]
	adds x8, x23, x3
	b.hs LBB53_3
	ldr x9, [x20, #160]
	cmp x8, x9
	b.hi LBB53_3
	ldr x8, [x20, #152]
	add x0, x8, x23
	mov x1, x22
	mov x2, x21
	bl _memcmp
	cbz w0, LBB53_6
LBB53_3:
	add x8, sp, #8
	mov x0, x22
	mov x1, x21
	bl core::str::converts::from_utf8
	ldr x8, [sp, #8]
	cmp x8, #1
	b.eq LBB53_11
	ldp x8, x9, [sp, #16]
	ldp x10, x11, [x20, #136]
	mov w12, #9
	stp x12, x8, [x19]
	stp x9, x10, [x19, #16]
	stp x11, x23, [x19, #32]
LBB53_5:
	.cfi_def_cfa wsp, 128
	ldp x29, x30, [sp, #112]
	ldp x20, x19, [sp, #96]
	ldp x22, x21, [sp, #80]
	ldp x24, x23, [sp, #64]
	add sp, sp, #128
	.cfi_def_cfa_offset 0
	.cfi_restore w30
	.cfi_restore w29
	.cfi_restore w19
	.cfi_restore w20
	.cfi_restore w21
	.cfi_restore w22
	.cfi_restore w23
	.cfi_restore w24
	ret
LBB53_6:
	.cfi_restore_state
	ldr x22, [x20, #88]
	lsr x8, x22, #32
	cbnz x8, LBB53_12
	lsr x8, x23, #32
	cbnz x8, LBB53_12
	ldur x8, [x20, #72]
	cmp x22, x8
	b.ne LBB53_10
	add x0, x20, #72
	bl <alloc::raw_vec::RawVec<u32>>::grow_one
LBB53_10:
	ldr x8, [x20, #80]
	str w23, [x8, x22, lsl #2]
	add x8, x22, #1
	str x8, [x20, #88]
	ldr x8, [x20, #168]
	add x8, x8, x21
	str x8, [x20, #168]
	str w23, [x19, #8]
	mov w8, #10
	str x8, [x19]
	b LBB53_5
LBB53_11:
	ldur q0, [sp, #16]
	str q0, [sp, #32]
Lloh694:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.63@PAGE
Lloh695:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.63@PAGEOFF
Lloh696:
	adrp x3, l_anon.10e61d2d870443aea44842dd9d471920.8@PAGE
Lloh697:
	add x3, x3, l_anon.10e61d2d870443aea44842dd9d471920.8@PAGEOFF
Lloh698:
	adrp x4, l_anon.10e61d2d870443aea44842dd9d471920.64@PAGE
Lloh699:
	add x4, x4, l_anon.10e61d2d870443aea44842dd9d471920.64@PAGEOFF
	add x2, sp, #32
	mov w1, #16
	bl core::result::unwrap_failed
LBB53_12:
Lloh700:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.36@PAGE
Lloh701:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.36@PAGEOFF
Lloh702:
	adrp x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGE
Lloh703:
	add x3, x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGEOFF
Lloh704:
	adrp x4, l_anon.10e61d2d870443aea44842dd9d471920.37@PAGE
Lloh705:
	add x4, x4, l_anon.10e61d2d870443aea44842dd9d471920.37@PAGEOFF
	sub x2, x29, #49
	mov w1, #52
	bl core::result::unwrap_failed
	.loh AdrpAdd	Lloh698, Lloh699
	.loh AdrpAdd	Lloh696, Lloh697
	.loh AdrpAdd	Lloh694, Lloh695
	.loh AdrpAdd	Lloh704, Lloh705
	.loh AdrpAdd	Lloh702, Lloh703
	.loh AdrpAdd	Lloh700, Lloh701
