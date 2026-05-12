	.globl	<runtime::generated_json::value::JsonNodeKind>::at_cursor
	.p2align	2
<runtime::generated_json::value::JsonNodeKind>::at_cursor:
Lfunc_begin16:
	.cfi_startproc
	stp x29, x30, [sp, #-16]!
	.cfi_def_cfa_offset 16
	mov x29, sp
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_remember_state
	ldr x9, [x0, #48]
	mov w8, w1
	cmp x9, x8
	b.ls LBB16_16
	ldp x1, x9, [x0, #32]
	ldr w8, [x9, x8, lsl #2]
	cmp x1, x8
	b.ls LBB16_18
	ldr x9, [x0, #24]
	ldrb w8, [x9, x8]
	sub w9, w8, #34
	cmp w9, #91
	b.hi LBB16_13
	mov w0, #1
Lloh426:
	adrp x10, LJTI16_0@PAGE
Lloh427:
	add x10, x10, LJTI16_0@PAGEOFF
	adr x11, LBB16_4
	ldrb w12, [x10, x9]
	add x11, x11, x12, lsl #2
	br x11
LBB16_4:
	mov w0, #6
	b LBB16_15
	mov w0, #2
	b LBB16_15
	mov w0, #12
	b LBB16_15
	mov w0, #3
	b LBB16_15
	mov w0, #11
	b LBB16_15
	mov w0, #4
	b LBB16_15
	mov w0, #9
	b LBB16_15
	mov w0, #8
	b LBB16_15
	mov w0, #10
	b LBB16_15
LBB16_13:
	sub w8, w8, #48
	cmp w8, #10
	b.hs LBB16_17
	mov w0, #7
LBB16_15:
	.cfi_def_cfa wsp, 16
	ldp x29, x30, [sp], #16
	.cfi_def_cfa_offset 0
	.cfi_restore w30
	.cfi_restore w29
	ret
LBB16_16:
	.cfi_restore_state
Lloh428:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.10@PAGE
Lloh429:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.10@PAGEOFF
Lloh430:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.12@PAGE
Lloh431:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.12@PAGEOFF
	mov w1, #46
	bl core::option::expect_failed
LBB16_17:
Lloh432:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.14@PAGE
Lloh433:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.14@PAGEOFF
Lloh434:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.15@PAGE
Lloh435:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.15@PAGEOFF
	mov w1, #169
	bl core::panicking::panic_fmt
LBB16_18:
Lloh436:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.13@PAGE
Lloh437:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.13@PAGEOFF
	mov x0, x8
	bl core::panicking::panic_bounds_check
	.loh AdrpAdd	Lloh426, Lloh427
	.loh AdrpAdd	Lloh430, Lloh431
	.loh AdrpAdd	Lloh428, Lloh429
	.loh AdrpAdd	Lloh434, Lloh435
	.loh AdrpAdd	Lloh432, Lloh433
	.loh AdrpAdd	Lloh436, Lloh437
