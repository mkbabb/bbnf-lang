	.globl	<runtime::generated_json::view::JsonArrayValues as core::iter::traits::iterator::Iterator>::next
	.p2align	2
<runtime::generated_json::view::JsonArrayValues as core::iter::traits::iterator::Iterator>::next:
Lfunc_begin64:
	.cfi_startproc
	sub sp, sp, #96
	.cfi_def_cfa_offset 96
	stp x24, x23, [sp, #32]
	stp x22, x21, [sp, #48]
	stp x20, x19, [sp, #64]
	stp x29, x30, [sp, #80]
	add x29, sp, #80
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
	ldrb w9, [x0, #12]
	tbnz w9, #0, LBB64_4
	ldr w19, [x0, #8]
	ldr x20, [x0]
	ldr x9, [x20, #48]
	cmp x9, x19
	b.ls LBB64_4
	mov x23, x0
	mov x22, x8
	mov x0, x20
	mov x1, x19
	bl <runtime::generated_json::value::JsonNodeKind>::at_cursor
	and w8, w0, #0xffff
	cmp w8, #4
	b.ne LBB64_6
	mov w8, #1
	strb w8, [x23, #12]
	mov w9, #8
	mov x8, x22
	b LBB64_5
LBB64_4:
	mov w9, #8
LBB64_5:
	strb w9, [x8, #16]
	.cfi_def_cfa wsp, 96
	ldp x29, x30, [sp, #80]
	ldp x20, x19, [sp, #64]
	ldp x22, x21, [sp, #48]
	ldp x24, x23, [sp, #32]
	add sp, sp, #96
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
LBB64_6:
	.cfi_restore_state
	mov x0, x20
	mov x1, x19
	bl <runtime::generated_json::value::JsonNodeKind>::at_cursor
	and w8, w0, #0xffff
	cmp w8, #6
	b.le LBB64_16
	sub w8, w8, #7
	cmp w8, #4
	b.hs LBB64_32
	add w21, w19, #1
LBB64_9:
	mov x0, x20
	mov x1, x21
	bl <runtime::generated_json::value::JsonNodeKind>::at_cursor
	and w8, w0, #0xffff
	cmp w8, #10
	b.hi LBB64_33
	mov w9, #1
	lsl w9, w9, w8
	mov w10, #1994
	tst w9, w10
	b.eq LBB64_30
	str w21, [x23, #8]
LBB64_12:
	mov x0, x20
	mov x1, x19
	bl <runtime::generated_json::value::JsonNodeKind>::at_cursor
	sub w8, w0, #1
	and w9, w8, #0xffff
	cmp w9, #9
	b.hi LBB64_15
	mov w9, #997
	lsr w9, w9, w8
	tbz w9, #0, LBB64_15
	and x8, x8, #0xffff
Lloh754:
	adrp x9, l_switch.table.<runtime::generated_json::view::JsonArrayValues as core::iter::traits::iterator::Iterator>::next@PAGE
Lloh755:
	add x9, x9, l_switch.table.<runtime::generated_json::view::JsonArrayValues as core::iter::traits::iterator::Iterator>::next@PAGEOFF
	ldrb w9, [x9, x8]
	mov x8, x22
	str x20, [x22]
	str w19, [x22, #8]
	b LBB64_5
LBB64_15:
	strh w0, [sp, #14]
	add x8, sp, #14
Lloh756:
	adrp x9, <runtime::generated_json::value::JsonNodeKind as core::fmt::Debug>::fmt@PAGE
Lloh757:
	add x9, x9, <runtime::generated_json::value::JsonNodeKind as core::fmt::Debug>::fmt@PAGEOFF
	stp x8, x9, [sp, #16]
Lloh758:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.43@PAGE
Lloh759:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.43@PAGEOFF
Lloh760:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.53@PAGE
Lloh761:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.53@PAGEOFF
	add x1, sp, #16
	bl core::panicking::panic_fmt
LBB64_16:
	cmp w8, #1
	ccmp w8, #3, #4, ne
	b.ne LBB64_28
	add w21, w19, #1
	mov w24, #1
	b LBB64_22
LBB64_18:
	cmp w9, #1
	b.eq LBB64_26
	cmp w9, #2
	b.ne LBB64_21
LBB64_20:
	sub w24, w24, #1
LBB64_21:
	add w21, w8, w21
	cbz w24, LBB64_9
LBB64_22:
	mov x0, x20
	mov x1, x21
	bl <runtime::generated_json::value::JsonNodeKind>::at_cursor
	and w9, w0, #0xffff
	mov w8, #1
	cmp w9, #2
	b.le LBB64_18
	cmp w9, #6
	b.eq LBB64_27
	cmp w9, #4
	b.eq LBB64_20
	cmp w9, #3
	b.ne LBB64_21
LBB64_26:
	add w24, w24, #1
	b LBB64_21
LBB64_27:
	mov w8, #2
	b LBB64_21
LBB64_28:
	cmp w8, #6
	b.ne LBB64_32
	add w21, w19, #2
	b LBB64_9
LBB64_30:
	cmp w8, #4
	b.ne LBB64_33
	str w21, [x23, #8]
	mov w8, #1
	strb w8, [x23, #12]
	b LBB64_12
LBB64_32:
	strh w0, [sp, #14]
	add x8, sp, #14
Lloh762:
	adrp x9, <runtime::generated_json::value::JsonNodeKind as core::fmt::Debug>::fmt@PAGE
Lloh763:
	add x9, x9, <runtime::generated_json::value::JsonNodeKind as core::fmt::Debug>::fmt@PAGEOFF
	stp x8, x9, [sp, #16]
Lloh764:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.43@PAGE
Lloh765:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.43@PAGEOFF
Lloh766:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.52@PAGE
Lloh767:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.52@PAGEOFF
	add x1, sp, #16
	bl core::panicking::panic_fmt
LBB64_33:
Lloh768:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.81@PAGE
Lloh769:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.81@PAGEOFF
Lloh770:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.82@PAGE
Lloh771:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.82@PAGEOFF
	mov w1, #167
	bl core::panicking::panic_fmt
	.loh AdrpAdd	Lloh754, Lloh755
	.loh AdrpAdd	Lloh760, Lloh761
	.loh AdrpAdd	Lloh758, Lloh759
	.loh AdrpAdd	Lloh756, Lloh757
	.loh AdrpAdd	Lloh766, Lloh767
	.loh AdrpAdd	Lloh764, Lloh765
	.loh AdrpAdd	Lloh762, Lloh763
	.loh AdrpAdd	Lloh770, Lloh771
	.loh AdrpAdd	Lloh768, Lloh769
