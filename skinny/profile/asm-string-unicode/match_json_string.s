	.globl	parse_that_regex::match_json_string
	.p2align	2
parse_that_regex::match_json_string:
Lfunc_begin2:
	.cfi_startproc
	cmp x2, x1
	b.hs LBB2_4
	ldrb w9, [x0, x2]
	cmp w9, #34
	b.ne LBB2_4
	add x9, x2, #1
	cmp x9, x1
	b.hs LBB2_26
	mov w16, #0
	mov w10, #1
	mov x11, #8193
	movk x11, #1024, lsl #48
	mov w12, #4113
	movk w12, #5, lsl #16
	add x13, x2, #1
	b LBB2_8
LBB2_4:
	str x2, [x8]
	strb wzr, [x8, #8]
	mov w10, #2
	strb w10, [x8, #32]
	ret
LBB2_5:
	sub w14, w14, #34
	cmp w14, #58
	lsl x14, x10, x14
	and x14, x14, x11
	ccmp x14, #0, #4, ls
	b.eq LBB2_27
LBB2_6:
	add x14, x13, #2
	mov w16, #1
LBB2_7:
	mov x13, x14
	cmp x14, x1
	b.hs LBB2_26
LBB2_8:
	add x15, x0, x13
	ldrb w14, [x15]
	cmp w14, #92
	b.eq LBB2_12
	cmp w14, #34
	b.eq LBB2_28
	cmp w14, #32
	b.lo LBB2_29
	add x14, x13, #1
	b LBB2_7
LBB2_12:
	add x14, x13, #1
	cmp x14, x1
	b.hs LBB2_27
	ldrb w14, [x0, x14]
	sub w16, w14, #98
	cmp w16, #19
	b.hi LBB2_5
	lsl w17, w10, w16
	tst w17, w12
	b.ne LBB2_6
	cmp w16, #19
	b.ne LBB2_5
	add x14, x13, #6
	cmp x14, x1
	b.hi LBB2_25
	ldrb w16, [x15, #2]
	sub w17, w16, #48
	cmp w17, #10
	b.lo LBB2_19
	and w16, w16, #0xffffffdf
	sub w16, w16, #65
	cmp w16, #5
	b.hi LBB2_25
LBB2_19:
	ldrb w16, [x15, #3]
	sub w17, w16, #48
	cmp w17, #10
	b.lo LBB2_21
	and w16, w16, #0xffffffdf
	sub w16, w16, #65
	cmp w16, #5
	b.hi LBB2_25
LBB2_21:
	ldrb w16, [x15, #4]
	sub w17, w16, #48
	cmp w17, #10
	b.lo LBB2_23
	and w16, w16, #0xffffffdf
	sub w16, w16, #65
	cmp w16, #5
	b.hi LBB2_25
LBB2_23:
	ldrb w15, [x15, #5]
	sub w17, w15, #48
	mov w16, #1
	cmp w17, #10
	b.lo LBB2_7
	and w15, w15, #0xffffffdf
	sub w15, w15, #65
	cmp w15, #6
	b.lo LBB2_7
LBB2_25:
	str x13, [x8]
	mov w9, #4
	strb w9, [x8, #8]
	mov w10, #2
	strb w10, [x8, #32]
	ret
LBB2_26:
	str x2, [x8]
	mov w9, #1
	strb w9, [x8, #8]
	mov w10, #2
	strb w10, [x8, #32]
	ret
LBB2_27:
	str x13, [x8]
	mov w9, #3
	strb w9, [x8, #8]
	mov w10, #2
	strb w10, [x8, #32]
	ret
LBB2_28:
	add x11, x13, #1
	and w10, w16, #0x1
	stp x2, x11, [x8]
	stp x9, x13, [x8, #16]
	strb w10, [x8, #32]
	ret
LBB2_29:
	str x13, [x8]
	mov w10, #2
	strb w10, [x8, #8]
	strb w10, [x8, #32]
	ret
