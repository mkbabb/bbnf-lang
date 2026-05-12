parse_that_regex::read_hex_unit:
Lfunc_begin1:
	.cfi_startproc
	stp x29, x30, [sp, #-16]!
	.cfi_def_cfa_offset 16
	mov x29, sp
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_remember_state
	mov x8, x1
	add x1, x3, #4
	cmp x1, x2
	b.hi LBB1_8
	cmn x3, #5
	b.hi LBB1_31
	add x11, x8, x3
	ldrb w15, [x11]
	sub w8, w15, #48
	and w9, w15, #0xffffffdf
	sub w9, w9, #65
	cmp w8, #10
	and w9, w9, #0xff
	ccmp w9, #6, #0, hs
	b.hs LBB1_8
	ldrb w14, [x11, #1]
	sub w9, w14, #48
	and w10, w14, #0xffffffdf
	sub w10, w10, #65
	cmp w9, #10
	and w10, w10, #0xff
	ccmp w10, #5, #0, hs
	b.hi LBB1_8
	ldrb w13, [x11, #2]
	sub w10, w13, #48
	cmp w10, #10
	b.lo LBB1_6
	and w12, w13, #0xffffffdf
	sub w12, w12, #65
	and w12, w12, #0xff
	cmp w12, #5
	b.hi LBB1_8
LBB1_6:
	ldrb w12, [x11, #3]
	sub w11, w12, #48
	cmp w11, #10
	b.lo LBB1_10
	and w16, w12, #0xffffffdf
	sub w16, w16, #65
	and w16, w16, #0xff
	cmp w16, #6
	b.lo LBB1_10
LBB1_8:
	str x3, [x0]
	mov w8, #4
LBB1_9:
	strb w8, [x0, #8]
	.cfi_def_cfa wsp, 16
	ldp x29, x30, [sp], #16
	.cfi_def_cfa_offset 0
	.cfi_restore w30
	.cfi_restore w29
	ret
LBB1_10:
	.cfi_restore_state
	and w16, w8, #0xff
	cmp w16, #10
	b.lo LBB1_15
	sub w8, w15, #97
	cmp w8, #6
	b.hs LBB1_13
	sub w8, w15, #87
	b LBB1_15
LBB1_13:
	sub w8, w15, #65
	cmp w8, #6
	b.hs LBB1_32
	sub w8, w15, #55
LBB1_15:
	and w15, w9, #0xff
	cmp w15, #10
	b.lo LBB1_20
	sub w9, w14, #97
	cmp w9, #6
	b.hs LBB1_18
	sub w9, w14, #87
	b LBB1_20
LBB1_18:
	sub w9, w14, #65
	cmp w9, #5
	b.hi LBB1_32
	sub w9, w14, #55
LBB1_20:
	and w14, w10, #0xff
	cmp w14, #10
	b.lo LBB1_25
	sub w10, w13, #97
	cmp w10, #6
	b.hs LBB1_23
	sub w10, w13, #87
	b LBB1_25
LBB1_23:
	sub w10, w13, #65
	cmp w10, #5
	b.hi LBB1_32
	sub w10, w13, #55
LBB1_25:
	and w13, w11, #0xff
	cmp w13, #10
	b.lo LBB1_30
	sub w11, w12, #97
	cmp w11, #6
	b.hs LBB1_28
	sub w11, w12, #87
	b LBB1_30
LBB1_28:
	sub w11, w12, #65
	cmp w11, #5
	b.hi LBB1_32
	sub w11, w12, #55
LBB1_30:
	and w8, w8, #0xff
	lsl w8, w8, #8
	add w8, w8, w9, uxtb #4
	add w8, w8, w10, uxtb
	lsl w8, w8, #4
	add w8, w8, w11, uxtb
	strh w8, [x0]
	mov w8, #6
	b LBB1_9
LBB1_31:
Lloh0:
	adrp x8, l_anon.028aad4ed9a93f18e57e62edb80815c2.1@PAGE
Lloh1:
	add x8, x8, l_anon.028aad4ed9a93f18e57e62edb80815c2.1@PAGEOFF
	mov x0, x3
	mov x3, x8
	bl core::slice::index::slice_index_fail
LBB1_32:
Lloh2:
	adrp x0, l_anon.028aad4ed9a93f18e57e62edb80815c2.4@PAGE
Lloh3:
	add x0, x0, l_anon.028aad4ed9a93f18e57e62edb80815c2.4@PAGEOFF
Lloh4:
	adrp x2, l_anon.028aad4ed9a93f18e57e62edb80815c2.5@PAGE
Lloh5:
	add x2, x2, l_anon.028aad4ed9a93f18e57e62edb80815c2.5@PAGEOFF
	mov w1, #139
	bl core::panicking::panic_fmt
	.loh AdrpAdd	Lloh0, Lloh1
	.loh AdrpAdd	Lloh4, Lloh5
	.loh AdrpAdd	Lloh2, Lloh3
