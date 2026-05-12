runtime::generated_json::generated::consume_structural:
Lfunc_begin54:
	.cfi_startproc
	sub sp, sp, #64
	.cfi_def_cfa_offset 64
	stp x22, x21, [sp, #16]
	stp x20, x19, [sp, #32]
	stp x29, x30, [sp, #48]
	add x29, sp, #48
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_remember_state
	ldr x8, [x0, #16]
	ldr x9, [x0, #176]
	cmp x9, x8
	b.hs LBB54_4
	ldr x10, [x0, #8]
	ldr x11, [x0, #168]
LBB54_2:
	ldr w12, [x10, x9, lsl #2]
	cmp x11, x12
	b.ls LBB54_4
	add x9, x9, #1
	str x9, [x0, #176]
	cmp x8, x9
	b.ne LBB54_2
	b LBB54_23
LBB54_4:
	cmp x9, x8
	b.hs LBB54_23
	ldr x8, [x0, #8]
	ldr w19, [x8, x9, lsl #2]
	ldr x11, [x0, #168]
	cmp x11, x19
	b.ne LBB54_8
	ldr x8, [x0, #160]
	cmp x8, x19
	b.ls LBB54_23
	ldr x10, [x0, #152]
	b LBB54_15
LBB54_8:
	ldr x12, [x0, #160]
	cmp x11, x12
	b.hs LBB54_23
	ldr x10, [x0, #152]
	mov w8, #1
	mov x13, #9728
	movk x13, #1, lsl #32
LBB54_10:
	ldrb w14, [x10, x11]
	cmp w14, #32
	lsl x14, x8, x14
	and x14, x14, x13
	ccmp x14, #0, #4, ls
	b.eq LBB54_13
	add x11, x11, #1
	cmp x12, x11
	b.ne LBB54_10
	mov x11, x12
LBB54_13:
	mov w8, #0
	cmp x12, x19
	b.ls LBB54_24
	cmp x11, x19
	b.ne LBB54_24
LBB54_15:
	and w8, w1, #0xff
	ldrb w10, [x10, x19]
	cmp w10, w8
	b.ne LBB54_23
	add x9, x9, #1
	str x9, [x0, #176]
	cmp w8, #44
	b.eq LBB54_22
	cmp w8, #58
	b.eq LBB54_22
	ldr x20, [x0, #88]
	lsr x8, x20, #32
	cbnz x8, LBB54_25
	ldur x8, [x0, #72]
	cmp x20, x8
	b.ne LBB54_21
	mov x21, x0
	add x0, x0, #72
	bl <alloc::raw_vec::RawVec<u32>>::grow_one
	mov x0, x21
LBB54_21:
	ldr x8, [x0, #80]
	str w19, [x8, x20, lsl #2]
	add x8, x20, #1
	str x8, [x0, #88]
LBB54_22:
	add x8, x19, #1
	str x8, [x0, #168]
	mov w8, #1
	b LBB54_24
LBB54_23:
	mov w8, #0
LBB54_24:
	mov x0, x8
	.cfi_def_cfa wsp, 64
	ldp x29, x30, [sp, #48]
	ldp x20, x19, [sp, #32]
	ldp x22, x21, [sp, #16]
	add sp, sp, #64
	.cfi_def_cfa_offset 0
	.cfi_restore w30
	.cfi_restore w29
	.cfi_restore w19
	.cfi_restore w20
	.cfi_restore w21
	.cfi_restore w22
	ret
LBB54_25:
	.cfi_restore_state
Lloh706:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.36@PAGE
Lloh707:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.36@PAGEOFF
Lloh708:
	adrp x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGE
Lloh709:
	add x3, x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGEOFF
Lloh710:
	adrp x4, l_anon.10e61d2d870443aea44842dd9d471920.37@PAGE
Lloh711:
	add x4, x4, l_anon.10e61d2d870443aea44842dd9d471920.37@PAGEOFF
	add x2, sp, #15
	mov w1, #52
	bl core::result::unwrap_failed
	.loh AdrpAdd	Lloh710, Lloh711
	.loh AdrpAdd	Lloh708, Lloh709
	.loh AdrpAdd	Lloh706, Lloh707
