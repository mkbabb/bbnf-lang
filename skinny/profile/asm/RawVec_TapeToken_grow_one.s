	.globl	<alloc::raw_vec::RawVec<runtime::tape::TapeToken>>::grow_one
	.p2align	2
<alloc::raw_vec::RawVec<runtime::tape::TapeToken>>::grow_one:
Lfunc_begin36:
	.cfi_startproc
	sub sp, sp, #64
	.cfi_def_cfa_offset 64
	stp x20, x19, [sp, #32]
	stp x29, x30, [sp, #48]
	add x29, sp, #48
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_remember_state
	mov x19, x0
	ldp x1, x2, [x0]
	lsl x8, x1, #1
	mov w9, #4
	cmp x8, #4
	csel x20, x8, x9, hi
	add x0, sp, #8
	mov x3, x20
	mov w4, #16
	mov w5, #16
	bl <alloc::raw_vec::RawVecInner>::finish_grow
	ldr x8, [sp, #8]
	cmp x8, #1
	b.eq LBB36_2
	ldr x8, [sp, #16]
	stp x20, x8, [x19]
	.cfi_def_cfa wsp, 64
	ldp x29, x30, [sp, #48]
	ldp x20, x19, [sp, #32]
	add sp, sp, #64
	.cfi_def_cfa_offset 0
	.cfi_restore w30
	.cfi_restore w29
	.cfi_restore w19
	.cfi_restore w20
	ret
LBB36_2:
	.cfi_restore_state
	ldp x0, x1, [sp, #16]
	bl alloc::raw_vec::handle_error
