.section __TEXT,__text,regular,pure_instructions
	.globl	<runtime::tape::assembler::TapeAssembler>::new
	.p2align	2
<runtime::tape::assembler::TapeAssembler>::new:
Lfunc_begin17:
	.cfi_startproc
	stp x24, x23, [sp, #-64]!
	.cfi_def_cfa_offset 64
	stp x22, x21, [sp, #16]
	stp x20, x19, [sp, #32]
	stp x29, x30, [sp, #48]
	add x29, sp, #48
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_offset w23, -56
	.cfi_offset w24, -64
	mov x20, #0
	lsl x19, x2, #2
	lsr x9, x2, #62
	cbnz x9, LBB17_5
	mov x9, #9223372036854775804
	cmp x19, x9
	b.hi LBB17_5
	cbz x19, LBB17_6
	mov x22, x8
	mov x23, x2
	mov x24, x1
	mov x21, x0
	bl __rustc::__rust_no_alloc_shim_is_unstable_v2
	mov w20, #4
	mov x0, x19
	mov w1, #4
	bl __rustc::__rust_alloc
	mov x9, x0
	cbz x0, LBB17_5
	mov x1, x24
	mov x10, x23
	mov x8, x22
	mov x0, x21
	b LBB17_7
LBB17_5:
	mov x0, x20
	mov x1, x19
	bl alloc::raw_vec::handle_error
LBB17_6:
	mov x10, #0
	mov w9, #4
LBB17_7:
	stp x0, x1, [x8, #48]
	stp x10, x9, [x8]
	stp xzr, xzr, [x8, #16]
	mov w9, #1
	stp x9, xzr, [x8, #32]
	.cfi_def_cfa wsp, 64
	ldp x29, x30, [sp, #48]
	ldp x20, x19, [sp, #32]
	ldp x22, x21, [sp, #16]
	ldp x24, x23, [sp], #64
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
