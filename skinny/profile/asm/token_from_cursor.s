	.globl	runtime::generated_json::view::token_from_cursor
	.p2align	2
runtime::generated_json::view::token_from_cursor:
Lfunc_begin49:
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
	mov x22, x1
	mov x21, x0
	mov x19, x8
	bl <runtime::generated_json::value::JsonNodeKind>::at_cursor
	mov x20, x0
	and w8, w0, #0xffff
	cmp w8, #5
	b.gt LBB49_5
	cmp w8, #2
	b.le LBB49_11
	cmp w8, #3
	b.eq LBB49_12
	cmp w8, #4
	b.eq LBB49_15
	mov x0, #0
	mov w8, #0
	ldr x1, [x21, #32]
	b LBB49_22
LBB49_5:
	sub w9, w8, #7
	cmp w9, #4
	b.hs LBB49_7
	mov x0, x21
	mov x1, x22
	bl runtime::generated_json::view::scalar_span
	and w8, w20, #0xffff
	cmp w8, #3
	b.gt LBB49_13
	b LBB49_17
LBB49_7:
	sub w8, w8, #11
	cmp w8, #2
	b.lo LBB49_15
	ldr x10, [x21, #48]
	mov w8, w22
	cmp x10, x8
	b.ls LBB49_25
	add w9, w22, #1
	cmp x10, x9
	b.ls LBB49_26
	ldr x10, [x21, #40]
	ldr w8, [x10, x8, lsl #2]
	add x0, x8, #1
	ldr w1, [x10, x9, lsl #2]
	mov w8, #66
	b LBB49_22
LBB49_11:
	cmp w8, #1
	b.ne LBB49_15
LBB49_12:
	mov x0, x21
	mov x1, x22
	bl runtime::generated_json::view::span_for_value
	and w8, w20, #0xffff
	cmp w8, #3
	b.le LBB49_17
LBB49_13:
	cmp w8, #4
	b.eq LBB49_21
	mov w9, #66
	cmp w8, #7
	cset w10, eq
	cmp w8, #6
	csel w8, w9, w10, eq
	b LBB49_22
LBB49_15:
	ldr x9, [x21, #48]
	mov w8, w22
	cmp x9, x8
	b.ls LBB49_24
	ldr x9, [x21, #40]
	ldr w0, [x9, x8, lsl #2]
	add x1, x0, #1
	and w8, w20, #0xffff
	cmp w8, #3
	b.gt LBB49_13
LBB49_17:
	cmp w8, #1
	b.eq LBB49_20
	cmp w8, #2
	b.eq LBB49_21
	cmp w8, #3
	b.ne LBB49_23
LBB49_20:
	mov w8, #132
	b LBB49_22
LBB49_21:
	mov w8, #256
LBB49_22:
	strh w20, [x19, #14]
	strh w8, [x19, #12]
	stp w0, w1, [x19]
	str wzr, [x19, #8]
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
LBB49_23:
	.cfi_restore_state
	mov w8, #0
	b LBB49_22
LBB49_24:
Lloh624:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.50@PAGE
Lloh625:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.50@PAGEOFF
Lloh626:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.51@PAGE
Lloh627:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.51@PAGEOFF
	mov w1, #14
	bl core::option::expect_failed
LBB49_25:
Lloh628:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.46@PAGE
Lloh629:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.46@PAGEOFF
Lloh630:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.48@PAGE
Lloh631:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.48@PAGEOFF
	mov w1, #18
	bl core::option::expect_failed
LBB49_26:
Lloh632:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.47@PAGE
Lloh633:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.47@PAGEOFF
Lloh634:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.49@PAGE
Lloh635:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.49@PAGEOFF
	mov w1, #19
	bl core::option::expect_failed
	.loh AdrpAdd	Lloh626, Lloh627
	.loh AdrpAdd	Lloh624, Lloh625
	.loh AdrpAdd	Lloh630, Lloh631
	.loh AdrpAdd	Lloh628, Lloh629
	.loh AdrpAdd	Lloh634, Lloh635
	.loh AdrpAdd	Lloh632, Lloh633
