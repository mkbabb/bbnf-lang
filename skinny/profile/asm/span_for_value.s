runtime::generated_json::view::span_for_value:
Lfunc_begin48:
	.cfi_startproc
	sub sp, sp, #80
	.cfi_def_cfa_offset 80
	stp x22, x21, [sp, #32]
	stp x20, x19, [sp, #48]
	stp x29, x30, [sp, #64]
	add x29, sp, #64
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_remember_state
	mov x20, x1
	mov x19, x0
	bl <runtime::generated_json::value::JsonNodeKind>::at_cursor
	and w8, w0, #0xffff
	cmp w8, #6
	b.le LBB48_3
	sub w8, w8, #7
	cmp w8, #4
	b.hs LBB48_33
	mov x0, x19
	mov x1, x20
	.cfi_def_cfa wsp, 80
	ldp x29, x30, [sp, #64]
	ldp x20, x19, [sp, #48]
	ldp x22, x21, [sp, #32]
	add sp, sp, #80
	.cfi_def_cfa_offset 0
	.cfi_restore w30
	.cfi_restore w29
	.cfi_restore w19
	.cfi_restore w20
	.cfi_restore w21
	.cfi_restore w22
	b runtime::generated_json::view::scalar_span
LBB48_3:
	.cfi_restore_state
	.cfi_remember_state
	cmp w8, #1
	b.eq LBB48_9
	cmp w8, #3
	b.eq LBB48_9
	cmp w8, #6
	b.ne LBB48_33
	ldr x10, [x19, #48]
	mov w8, w20
	cmp x10, x8
	b.ls LBB48_34
	add w9, w20, #1
	cmp x10, x9
	b.ls LBB48_35
	ldr x10, [x19, #40]
	ldr w8, [x10, x8, lsl #2]
	add x0, x8, #1
	ldr w1, [x10, x9, lsl #2]
	b LBB48_15
LBB48_9:
	mov x0, x19
	mov x1, x20
	bl <runtime::generated_json::value::JsonNodeKind>::at_cursor
	and w8, w0, #0xffff
	cmp w8, #6
	b.le LBB48_16
	sub w8, w8, #7
	cmp w8, #4
	b.hs LBB48_36
	add w21, w20, #1
LBB48_12:
	ldr x10, [x19, #48]
	mov w8, w20
	cmp x10, x8
	b.ls LBB48_31
	sub w9, w21, #1
	cmp x10, x9
	b.ls LBB48_32
	ldr x10, [x19, #40]
	ldr w0, [x10, x8, lsl #2]
	ldr w8, [x10, x9, lsl #2]
	add x1, x8, #1
LBB48_15:
	.cfi_def_cfa wsp, 80
	ldp x29, x30, [sp, #64]
	ldp x20, x19, [sp, #48]
	ldp x22, x21, [sp, #32]
	add sp, sp, #80
	.cfi_def_cfa_offset 0
	.cfi_restore w30
	.cfi_restore w29
	.cfi_restore w19
	.cfi_restore w20
	.cfi_restore w21
	.cfi_restore w22
	ret
LBB48_16:
	.cfi_restore_state
	cmp w8, #1
	b.eq LBB48_20
	cmp w8, #3
	b.eq LBB48_20
	cmp w8, #6
	b.ne LBB48_36
	add w21, w20, #2
	b LBB48_12
LBB48_20:
	add w21, w20, #1
	mov w22, #1
	b LBB48_25
LBB48_21:
	cmp w9, #1
	b.eq LBB48_29
	cmp w9, #2
	b.ne LBB48_24
LBB48_23:
	sub w22, w22, #1
LBB48_24:
	add w21, w8, w21
	cbz w22, LBB48_12
LBB48_25:
	mov x0, x19
	mov x1, x21
	bl <runtime::generated_json::value::JsonNodeKind>::at_cursor
	and w9, w0, #0xffff
	mov w8, #1
	cmp w9, #2
	b.le LBB48_21
	cmp w9, #6
	b.eq LBB48_30
	cmp w9, #4
	b.eq LBB48_23
	cmp w9, #3
	b.ne LBB48_24
LBB48_29:
	add w22, w22, #1
	b LBB48_24
LBB48_30:
	mov w8, #2
	b LBB48_24
LBB48_31:
Lloh596:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.34@PAGE
Lloh597:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.34@PAGEOFF
Lloh598:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.35@PAGE
Lloh599:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.35@PAGEOFF
	mov w1, #50
	bl core::option::expect_failed
LBB48_32:
Lloh600:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.42@PAGE
Lloh601:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.42@PAGEOFF
Lloh602:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.44@PAGE
Lloh603:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.44@PAGEOFF
	mov w1, #33
	bl core::option::expect_failed
LBB48_33:
	strh w0, [sp, #14]
	add x8, sp, #14
Lloh604:
	adrp x9, <runtime::generated_json::value::JsonNodeKind as core::fmt::Debug>::fmt@PAGE
Lloh605:
	add x9, x9, <runtime::generated_json::value::JsonNodeKind as core::fmt::Debug>::fmt@PAGEOFF
	stp x8, x9, [sp, #16]
Lloh606:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.43@PAGE
Lloh607:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.43@PAGEOFF
Lloh608:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.45@PAGE
Lloh609:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.45@PAGEOFF
	add x1, sp, #16
	bl core::panicking::panic_fmt
LBB48_34:
Lloh610:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.46@PAGE
Lloh611:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.46@PAGEOFF
Lloh612:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.48@PAGE
Lloh613:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.48@PAGEOFF
	mov w1, #18
	bl core::option::expect_failed
LBB48_35:
Lloh614:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.47@PAGE
Lloh615:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.47@PAGEOFF
Lloh616:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.49@PAGE
Lloh617:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.49@PAGEOFF
	mov w1, #19
	bl core::option::expect_failed
LBB48_36:
	strh w0, [sp, #14]
	add x8, sp, #14
Lloh618:
	adrp x9, <runtime::generated_json::value::JsonNodeKind as core::fmt::Debug>::fmt@PAGE
Lloh619:
	add x9, x9, <runtime::generated_json::value::JsonNodeKind as core::fmt::Debug>::fmt@PAGEOFF
	stp x8, x9, [sp, #16]
Lloh620:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.43@PAGE
Lloh621:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.43@PAGEOFF
Lloh622:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.52@PAGE
Lloh623:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.52@PAGEOFF
	add x1, sp, #16
	bl core::panicking::panic_fmt
	.loh AdrpAdd	Lloh598, Lloh599
	.loh AdrpAdd	Lloh596, Lloh597
	.loh AdrpAdd	Lloh602, Lloh603
	.loh AdrpAdd	Lloh600, Lloh601
	.loh AdrpAdd	Lloh608, Lloh609
	.loh AdrpAdd	Lloh606, Lloh607
	.loh AdrpAdd	Lloh604, Lloh605
	.loh AdrpAdd	Lloh612, Lloh613
	.loh AdrpAdd	Lloh610, Lloh611
	.loh AdrpAdd	Lloh616, Lloh617
	.loh AdrpAdd	Lloh614, Lloh615
	.loh AdrpAdd	Lloh622, Lloh623
	.loh AdrpAdd	Lloh620, Lloh621
	.loh AdrpAdd	Lloh618, Lloh619
