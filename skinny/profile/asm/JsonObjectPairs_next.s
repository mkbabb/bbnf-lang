	.globl	<runtime::generated_json::view::JsonObjectPairs as core::iter::traits::iterator::Iterator>::next
	.p2align	2
<runtime::generated_json::view::JsonObjectPairs as core::iter::traits::iterator::Iterator>::next:
Lfunc_begin63:
	.cfi_startproc
	sub sp, sp, #112
	.cfi_def_cfa_offset 112
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
	.cfi_remember_state
	ldrb w9, [x0, #12]
	tbnz w9, #0, LBB63_4
	ldr w19, [x0, #8]
	ldr x20, [x0]
	ldr x9, [x20, #48]
	cmp x9, x19
	b.ls LBB63_4
	mov x23, x0
	mov x22, x8
	mov x0, x20
	mov x1, x19
	bl <runtime::generated_json::value::JsonNodeKind>::at_cursor
	and w8, w0, #0xffff
	cmp w8, #2
	b.ne LBB63_6
	mov w8, #1
	strb w8, [x23, #12]
	str xzr, [x22]
	b LBB63_5
LBB63_4:
	str xzr, [x8]
LBB63_5:
	.cfi_def_cfa wsp, 112
	ldp x29, x30, [sp, #96]
	ldp x20, x19, [sp, #80]
	ldp x22, x21, [sp, #64]
	ldp x24, x23, [sp, #48]
	ldp x26, x25, [sp, #32]
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
	ret
LBB63_6:
	.cfi_restore_state
	add w24, w19, #2
	add w1, w19, #2
	mov x0, x20
	bl <runtime::generated_json::value::JsonNodeKind>::at_cursor
	and w8, w0, #0xffff
	cmp w8, #6
	b.le LBB63_14
	sub w8, w8, #7
	cmp w8, #4
	b.hs LBB63_29
	add w21, w19, #3
LBB63_9:
	mov x0, x20
	mov x1, x21
	bl <runtime::generated_json::value::JsonNodeKind>::at_cursor
	and w8, w0, #0xffff
	cmp w8, #2
	b.eq LBB63_12
	cmp w8, #6
	b.ne LBB63_28
	str w21, [x23, #8]
	b LBB63_13
LBB63_12:
	str w21, [x23, #8]
	mov w8, #1
	strb w8, [x23, #12]
LBB63_13:
	str x20, [x22]
	stp w19, w24, [x22, #8]
	b LBB63_5
LBB63_14:
	cmp w8, #1
	ccmp w8, #3, #4, ne
	b.ne LBB63_26
	add w21, w19, #3
	mov w25, #1
	b LBB63_20
LBB63_16:
	cmp w9, #1
	b.eq LBB63_24
	cmp w9, #2
	b.ne LBB63_19
LBB63_18:
	sub w25, w25, #1
LBB63_19:
	add w21, w8, w21
	cbz w25, LBB63_9
LBB63_20:
	mov x0, x20
	mov x1, x21
	bl <runtime::generated_json::value::JsonNodeKind>::at_cursor
	and w9, w0, #0xffff
	mov w8, #1
	cmp w9, #2
	b.le LBB63_16
	cmp w9, #6
	b.eq LBB63_25
	cmp w9, #4
	b.eq LBB63_18
	cmp w9, #3
	b.ne LBB63_19
LBB63_24:
	add w25, w25, #1
	b LBB63_19
LBB63_25:
	mov w8, #2
	b LBB63_19
LBB63_26:
	cmp w8, #6
	b.ne LBB63_29
	add w21, w19, #4
	b LBB63_9
LBB63_28:
Lloh744:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.79@PAGE
Lloh745:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.79@PAGEOFF
Lloh746:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.80@PAGE
Lloh747:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.80@PAGEOFF
	mov w1, #169
	bl core::panicking::panic_fmt
LBB63_29:
	strh w0, [sp, #14]
	add x8, sp, #14
Lloh748:
	adrp x9, <runtime::generated_json::value::JsonNodeKind as core::fmt::Debug>::fmt@PAGE
Lloh749:
	add x9, x9, <runtime::generated_json::value::JsonNodeKind as core::fmt::Debug>::fmt@PAGEOFF
	stp x8, x9, [sp, #16]
Lloh750:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.43@PAGE
Lloh751:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.43@PAGEOFF
Lloh752:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.52@PAGE
Lloh753:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.52@PAGEOFF
	add x1, sp, #16
	bl core::panicking::panic_fmt
	.loh AdrpAdd	Lloh746, Lloh747
	.loh AdrpAdd	Lloh744, Lloh745
	.loh AdrpAdd	Lloh752, Lloh753
	.loh AdrpAdd	Lloh750, Lloh751
	.loh AdrpAdd	Lloh748, Lloh749
