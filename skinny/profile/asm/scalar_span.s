runtime::generated_json::view::scalar_span:
Lfunc_begin47:
	.cfi_startproc
	stp x22, x21, [sp, #-48]!
	.cfi_def_cfa_offset 48
	stp x20, x19, [sp, #16]
	stp x29, x30, [sp, #32]
	add x29, sp, #32
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_remember_state
	ldr x9, [x0, #48]
	mov w8, w1
	cmp x9, x8
	b.ls LBB47_17
	ldp x20, x9, [x0, #32]
	ldr w19, [x9, x8, lsl #2]
	ldr x21, [x0, #24]
	bl <runtime::generated_json::value::JsonNodeKind>::at_cursor
	and w8, w0, #0xffff
	cmp w8, #8
	b.gt LBB47_5
	cmp w8, #7
	b.eq LBB47_8
	cmp w8, #8
	b.eq LBB47_7
LBB47_4:
Lloh588:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.40@PAGE
Lloh589:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.40@PAGEOFF
Lloh590:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.41@PAGE
Lloh591:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.41@PAGEOFF
	mov w1, #155
	bl core::panicking::panic_fmt
LBB47_5:
	cmp w8, #9
	b.eq LBB47_15
	cmp w8, #10
	b.ne LBB47_4
LBB47_7:
	add x1, x19, #4
	b LBB47_16
LBB47_8:
	mov x1, x19
	cmp x20, x19
	b.ls LBB47_16
	mov w8, #1
	mov x9, #9728
	movk x9, #4097, lsl #32
	mov x1, x19
LBB47_10:
	ldrb w10, [x21, x1]
	cmp w10, #44
	lsl x11, x8, x10
	and x11, x11, x9
	ccmp x11, #0, #4, ls
	b.ne LBB47_16
	cmp w10, #93
	b.eq LBB47_16
	cmp w10, #125
	b.eq LBB47_16
	add x1, x1, #1
	cmp x20, x1
	b.ne LBB47_10
	mov x1, x20
	b LBB47_16
LBB47_15:
	add x1, x19, #5
LBB47_16:
	mov x0, x19
	.cfi_def_cfa wsp, 48
	ldp x29, x30, [sp, #32]
	ldp x20, x19, [sp, #16]
	ldp x22, x21, [sp], #48
	.cfi_def_cfa_offset 0
	.cfi_restore w30
	.cfi_restore w29
	.cfi_restore w19
	.cfi_restore w20
	.cfi_restore w21
	.cfi_restore w22
	ret
LBB47_17:
	.cfi_restore_state
Lloh592:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.38@PAGE
Lloh593:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.38@PAGEOFF
Lloh594:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.39@PAGE
Lloh595:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.39@PAGEOFF
	mov w1, #13
	bl core::option::expect_failed
	.loh AdrpAdd	Lloh590, Lloh591
	.loh AdrpAdd	Lloh588, Lloh589
	.loh AdrpAdd	Lloh594, Lloh595
	.loh AdrpAdd	Lloh592, Lloh593
