runtime::generated_json::generated::parse_string:
Lfunc_begin52:
	.cfi_startproc
	sub sp, sp, #112
	.cfi_def_cfa_offset 112
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
	.cfi_remember_state
	mov x20, x1
	mov x19, x0
	ldr x21, [x1, #168]
	mov x0, x1
	mov w1, #34
	bl runtime::generated_json::generated::consume_structural
	tbz w0, #0, LBB52_5
	ldr x9, [x20, #16]
	ldr x8, [x20, #176]
	cmp x8, x9
	b.hs LBB52_6
	ldr x10, [x20, #8]
	ldr x11, [x20, #168]
LBB52_3:
	ldr w12, [x10, x8, lsl #2]
	cmp x11, x12
	b.ls LBB52_6
	add x8, x8, #1
	str x8, [x20, #176]
	cmp x9, x8
	b.ne LBB52_3
	b LBB52_16
LBB52_5:
	ldur q0, [x20, #136]
	ldr x8, [x20, #168]
	str xzr, [x19]
	b LBB52_17
LBB52_6:
	cmp x8, x9
	b.hs LBB52_16
	ldr x9, [x20, #8]
	ldr w22, [x9, x8, lsl #2]
	ldr x9, [x20, #160]
	cmp x9, x22
	b.ls LBB52_16
	ldr x9, [x20, #152]
	ldrb w9, [x9, x22]
	cmp w9, #34
	b.ne LBB52_16
	add x8, x8, #1
	str x8, [x20, #176]
	ldr x23, [x20, #88]
	lsr x8, x23, #32
	cbnz x8, LBB52_34
	ldur x8, [x20, #72]
	cmp x23, x8
	b.ne LBB52_12
	add x0, x20, #72
	bl <alloc::raw_vec::RawVec<u32>>::grow_one
LBB52_12:
	ldr x8, [x20, #80]
	str w22, [x8, x23, lsl #2]
	add x8, x23, #1
	str x8, [x20, #88]
	add x8, x22, #1
	str x8, [x20, #168]
	add x8, x21, #1
	ldr x1, [x20, #64]
	ldr x0, [x20, #192]
	cmp x0, x1
	b.hs LBB52_22
	ldr x10, [x20, #56]
LBB52_14:
	ldr w9, [x10, x0, lsl #2]
	cmp x8, x9
	b.ls LBB52_19
	add x0, x0, #1
	str x0, [x20, #192]
	cmp x1, x0
	b.ne LBB52_14
	b LBB52_22
LBB52_16:
	ldur q0, [x20, #136]
	ldr x8, [x20, #168]
	mov w9, #8
	str x9, [x19]
LBB52_17:
	stur q0, [x19, #24]
	str x8, [x19, #40]
LBB52_18:
	.cfi_def_cfa wsp, 112
	ldp x29, x30, [sp, #96]
	ldp x20, x19, [sp, #80]
	ldp x22, x21, [sp, #64]
	ldp x24, x23, [sp, #48]
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
	ret
LBB52_19:
	.cfi_restore_state
	cmp w22, w9
	b.ls LBB52_22
	cmp x0, x1
	b.hs LBB52_36
	ldp x10, x8, [x20, #136]
	mov w11, #8
	str x11, [x19]
	stp x10, x8, [x19, #24]
	str x9, [x19, #40]
	b LBB52_18
LBB52_22:
	ldr x9, [x20, #40]
	ldr x11, [x20, #184]
	subs x9, x9, x11
	b.ls LBB52_30
	ldr x12, [x20, #32]
	add x10, x11, #1
	add x11, x12, x11, lsl #2
LBB52_24:
	ldr w12, [x11], #4
	cmp x8, x12
	b.ls LBB52_26
	str x10, [x20, #184]
	add x10, x10, #1
	subs x9, x9, #1
	b.ne LBB52_24
	b LBB52_30
LBB52_26:
	cmp w22, w12
	b.ls LBB52_30
	ldp x0, x1, [x20, #152]
	mov x8, sp
	mov x2, x21
	bl parse_that_regex::match_json_string
	ldp x9, x8, [x20, #136]
	ldrb w10, [sp, #32]
	cmp w10, #2
	b.ne LBB52_29
	ldr x10, [sp]
	ldrb w11, [sp, #8]
	cmp w11, #0
	mov w11, #8
	csel x11, xzr, x11, eq
	b LBB52_33
LBB52_29:
	ldr x11, [sp, #8]
	ldr x10, [x20, #168]
	cmp x11, x10
	b.ne LBB52_32
LBB52_30:
	lsr x8, x21, #32
	cbnz x8, LBB52_35
	str w21, [x19, #8]
	mov w8, #10
	str x8, [x19]
	b LBB52_18
LBB52_32:
	mov w11, #8
LBB52_33:
	str x11, [x19]
	stp x9, x8, [x19, #24]
	str x10, [x19, #40]
	b LBB52_18
LBB52_34:
Lloh680:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.36@PAGE
Lloh681:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.36@PAGEOFF
Lloh682:
	adrp x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGE
Lloh683:
	add x3, x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGEOFF
Lloh684:
	adrp x4, l_anon.10e61d2d870443aea44842dd9d471920.37@PAGE
Lloh685:
	add x4, x4, l_anon.10e61d2d870443aea44842dd9d471920.37@PAGEOFF
	add x2, sp, #47
	mov w1, #52
	bl core::result::unwrap_failed
LBB52_35:
Lloh686:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.54@PAGE
Lloh687:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.54@PAGEOFF
Lloh688:
	adrp x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGE
Lloh689:
	add x3, x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGEOFF
Lloh690:
	adrp x4, l_anon.10e61d2d870443aea44842dd9d471920.61@PAGE
Lloh691:
	add x4, x4, l_anon.10e61d2d870443aea44842dd9d471920.61@PAGEOFF
	add x2, sp, #47
	mov w1, #21
	bl core::result::unwrap_failed
LBB52_36:
Lloh692:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.62@PAGE
Lloh693:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.62@PAGEOFF
	bl core::panicking::panic_bounds_check
	.loh AdrpAdd	Lloh684, Lloh685
	.loh AdrpAdd	Lloh682, Lloh683
	.loh AdrpAdd	Lloh680, Lloh681
	.loh AdrpAdd	Lloh690, Lloh691
	.loh AdrpAdd	Lloh688, Lloh689
	.loh AdrpAdd	Lloh686, Lloh687
	.loh AdrpAdd	Lloh692, Lloh693
Lfunc_end52:
	.cfi_endproc

	.p2align	2
