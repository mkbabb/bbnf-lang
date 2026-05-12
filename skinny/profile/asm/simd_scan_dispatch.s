	.globl	simd_scan::scan_dispatch
	.p2align	2
simd_scan::scan_dispatch:
Lfunc_begin3:
	.cfi_startproc
	mov x3, x2
	mov x2, x1
	mov x1, x0
	mov w10, #0
Lloh0:
	adrp x9, l_anon.d7d05db1deb039881e193d033e31fb2c.2@PAGE
Lloh1:
	add x9, x9, l_anon.d7d05db1deb039881e193d033e31fb2c.2@PAGEOFF
LBB3_1:
	cmp w10, #255
	b.eq LBB3_3
	and x11, x10, #0xff
	add w10, w10, #1
	ldrb w12, [x3, x11]
	ldrb w11, [x9, x11]
	cmp w12, w11
	b.eq LBB3_1
	b LBB3_4
LBB3_3:
	ldrb w9, [x3, #255]
	cbz w9, LBB3_5
LBB3_4:
	stp x20, x19, [sp, #-32]!
	.cfi_def_cfa_offset 32
	stp x29, x30, [sp, #16]
	add x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov x0, x8
	mov x19, x8
	bl simd_scan::scalar_positions
	strb wzr, [x19, #24]
	.cfi_def_cfa wsp, 32
	ldp x29, x30, [sp, #16]
	ldp x20, x19, [sp], #32
	.cfi_def_cfa_offset 0
	.cfi_restore w30
	.cfi_restore w29
	.cfi_restore w19
	.cfi_restore w20
	ret
LBB3_5:
	mov x0, x1
	mov x1, x2
	b simd_scan::scan_json_structurals
	.loh AdrpAdd	Lloh0, Lloh1
