simd_scan::scan_json_tail:
Lfunc_begin5:
	.cfi_startproc
	sub sp, sp, #112
	.cfi_def_cfa_offset 112
	stp x28, x27, [sp, #16]
	stp x26, x25, [sp, #32]
	stp x24, x23, [sp, #48]
	stp x22, x21, [sp, #64]
	stp x20, x19, [sp, #80]
	stp x29, x30, [sp, #96]
	add x29, sp, #96
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
	cmp x2, x1
	b.hs LBB5_26
	mov x23, x5
	mov x24, x4
	mov x19, x3
	mov x20, x2
	mov x21, x1
	mov x22, x0
	mov w25, #1
	mov x26, #1024
	movk x26, #256, lsl #16
	movk x26, #2560, lsl #48
	b LBB5_6
LBB5_2:
	mov w23, #0
LBB5_3:
	mov w24, #0
LBB5_4:
	ldr x8, [x19, #8]
	str w20, [x8, x27, lsl #2]
	add x8, x27, #1
	str x8, [x19, #16]
LBB5_5:
	add x20, x20, #1
	cmp x20, x21
	b.hs LBB5_26
LBB5_6:
	ldrb w8, [x22, x20]
	tbnz w24, #0, LBB5_19
	sub w9, w8, #34
	cmp w9, #59
	b.hi LBB5_16
	lsl x10, x25, x9
	tst x10, x26
	b.eq LBB5_13
LBB5_9:
	lsr x8, x20, #32
	cbnz x8, LBB5_27
	ldr x27, [x19, #16]
	ldr x8, [x19]
	cmp x27, x8
	b.ne LBB5_3
	mov w24, #0
LBB5_12:
	mov x0, x19
	bl <alloc::raw_vec::RawVec<u32>>::grow_one
	b LBB5_4
LBB5_13:
	cbnz x9, LBB5_16
	lsr x8, x20, #32
	cbnz x8, LBB5_27
	ldr x27, [x19, #16]
	ldr x8, [x19]
	mov w24, #1
	cmp x27, x8
	b.ne LBB5_4
	b LBB5_12
LBB5_16:
	cmp w8, #123
	b.eq LBB5_9
	cmp w8, #125
	b.eq LBB5_9
	mov w24, #0
	b LBB5_5
LBB5_19:
	tbz w23, #0, LBB5_21
	mov w23, #0
	mov w24, #1
	b LBB5_5
LBB5_21:
	cmp w8, #34
	b.eq LBB5_23
	cmp w8, #92
	cset w23, eq
	csinc w24, w25, wzr, eq
	b LBB5_5
LBB5_23:
	lsr x8, x20, #32
	cbnz x8, LBB5_27
	ldr x27, [x19, #16]
	ldr x8, [x19]
	cmp x27, x8
	b.ne LBB5_2
	mov x0, x19
	bl <alloc::raw_vec::RawVec<u32>>::grow_one
	b LBB5_2
LBB5_26:
	.cfi_def_cfa wsp, 112
	ldp x29, x30, [sp, #96]
	ldp x20, x19, [sp, #80]
	ldp x22, x21, [sp, #64]
	ldp x24, x23, [sp, #48]
	ldp x26, x25, [sp, #32]
	ldp x28, x27, [sp, #16]
	add sp, sp, #112
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
LBB5_27:
	.cfi_restore_state
Lloh6:
	adrp x0, l_anon.d7d05db1deb039881e193d033e31fb2c.4@PAGE
Lloh7:
	add x0, x0, l_anon.d7d05db1deb039881e193d033e31fb2c.4@PAGEOFF
Lloh8:
	adrp x3, l_anon.d7d05db1deb039881e193d033e31fb2c.8@PAGE
Lloh9:
	add x3, x3, l_anon.d7d05db1deb039881e193d033e31fb2c.8@PAGEOFF
Lloh10:
	adrp x4, l_anon.d7d05db1deb039881e193d033e31fb2c.5@PAGE
Lloh11:
	add x4, x4, l_anon.d7d05db1deb039881e193d033e31fb2c.5@PAGEOFF
	add x2, sp, #15
	mov w1, #52
	bl core::result::unwrap_failed
	.loh AdrpAdd	Lloh10, Lloh11
	.loh AdrpAdd	Lloh8, Lloh9
	.loh AdrpAdd	Lloh6, Lloh7
