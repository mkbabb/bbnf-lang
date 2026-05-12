	.globl	<runtime::tape::assembler::TapeAssembler>::finish
	.p2align	2
<runtime::tape::assembler::TapeAssembler>::finish:
Lfunc_begin18:
	.cfi_startproc
	.cfi_personality 155, _rust_eh_personality
	.cfi_lsda 16, Lexception9
	sub sp, sp, #160
	.cfi_def_cfa_offset 160
	stp x28, x27, [sp, #64]
	stp x26, x25, [sp, #80]
	stp x24, x23, [sp, #96]
	stp x22, x21, [sp, #112]
	stp x20, x19, [sp, #128]
	stp x29, x30, [sp, #144]
	add x29, sp, #144
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
	.cfi_offset w27, -88
	.cfi_offset w28, -96
	.cfi_remember_state
	mov x20, x1
	mov x24, x8
	ldp x22, x8, [x0, #48]
	str x8, [sp, #24]
	ldp x8, x25, [x0]
	ldp x21, x9, [x0, #16]
	stp x2, x9, [sp, #48]
	ldp x9, x28, [x0, #32]
	str x9, [sp, #40]
	cmp x8, x21
	b.ls LBB18_4
	lsl x26, x8, #2
	cbz x21, LBB18_9
	lsl x27, x21, #2
	mov x0, x25
	mov x1, x26
	mov w2, #4
	mov x3, x27
	bl __rustc::__rust_realloc
	mov x19, x0
	cbnz x0, LBB18_5
	str x20, [sp, #16]
	mov w0, #4
	mov x1, x27
	bl alloc::raw_vec::handle_error
	b LBB18_15
LBB18_4:
	mov x19, x25
LBB18_5:
	ldp x8, x26, [x20]
	ldr x23, [x20, #16]
	cmp x8, x23
	str x19, [sp, #32]
	mov x27, x28
	b.ls LBB18_10
LBB18_6:
	lsl x28, x8, #2
	cbz x23, LBB18_16
	lsl x19, x23, #2
	mov x0, x26
	mov x1, x28
	mov w2, #4
	mov x3, x19
	bl __rustc::__rust_realloc
	mov x25, x0
	cbnz x0, LBB18_11
	str x20, [sp, #16]
	mov w0, #4
	mov x1, x19
	bl alloc::raw_vec::handle_error
	b LBB18_15
LBB18_9:
	mov w19, #4
	mov x0, x25
	mov x1, x26
	mov w2, #4
	bl __rustc::__rust_dealloc
	ldp x8, x26, [x20]
	ldr x23, [x20, #16]
	cmp x8, x23
	str x19, [sp, #32]
	mov x27, x28
	b.hi LBB18_6
LBB18_10:
	mov x25, x26
LBB18_11:
	ldr x9, [sp, #48]
	ldp x8, x26, [x9]
	ldr x19, [x9, #16]
	cmp x8, x19
	b.ls LBB18_17
LBB18_12:
	lsl x1, x8, #2
	cbz x19, LBB18_18
	str x20, [sp, #16]
	lsl x3, x19, #2
	mov x0, x26
	mov x20, x1
	mov w2, #4
	str x3, [sp, #8]
	bl __rustc::__rust_realloc
	mov x28, x0
	cbnz x0, LBB18_19
	mov w0, #4
	ldr x1, [sp, #8]
	bl alloc::raw_vec::handle_error
LBB18_15:
	brk #0x1
LBB18_16:
	mov w25, #4
	mov x0, x26
	mov x1, x28
	mov w2, #4
	bl __rustc::__rust_dealloc
	ldr x9, [sp, #48]
	ldp x8, x26, [x9]
	ldr x19, [x9, #16]
	cmp x8, x19
	b.hi LBB18_12
LBB18_17:
	mov x28, x26
	b LBB18_19
LBB18_18:
	mov w28, #4
	mov x0, x26
	mov w2, #4
	bl __rustc::__rust_dealloc
LBB18_19:
Lloh438:
	adrp x8, runtime::tape::NEXT_TAPE_ID@PAGE
Lloh439:
	add x8, x8, runtime::tape::NEXT_TAPE_ID@PAGEOFF
	mov w9, #1
	ldadd x9, x8, [x8]
	ldp x10, x9, [sp, #24]
	stp x10, x9, [x24, #32]
	stp x21, x25, [x24, #48]
	stp x23, x28, [x24, #64]
	ldr x10, [sp, #56]
	ldr x9, [sp, #40]
	stp x10, x9, [x24]
	stp x27, x22, [x24, #16]
	stp x19, x8, [x24, #80]
	.cfi_def_cfa wsp, 160
	ldp x29, x30, [sp, #144]
	ldp x20, x19, [sp, #128]
	ldp x22, x21, [sp, #112]
	ldp x24, x23, [sp, #96]
	ldp x26, x25, [sp, #80]
	ldp x28, x27, [sp, #64]
	add sp, sp, #160
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
	.cfi_restore w27
	.cfi_restore w28
	ret
	.cfi_restore_state
	mov x24, x0
	mov x0, x26
	mov x1, x20
	mov w2, #4
	bl __rustc::__rust_dealloc
	cbnz x23, LBB18_22
	mov w19, #0
	b LBB18_24
LBB18_22:
	lsl x1, x23, #2
	mov x0, x25
	mov w2, #4
	bl __rustc::__rust_dealloc
	mov w19, #0
	b LBB18_24
	mov x24, x0
	mov x0, x26
	mov x1, x28
	mov w2, #4
	bl __rustc::__rust_dealloc
	mov w19, #1
LBB18_24:
	cbnz x21, LBB18_26
	mov w20, #0
	b LBB18_28
LBB18_26:
	lsl x1, x21, #2
	ldr x0, [sp, #32]
	mov w2, #4
	bl __rustc::__rust_dealloc
	mov w20, #0
	b LBB18_28
	mov x24, x0
	mov x0, x25
	mov x1, x26
	mov w2, #4
	bl __rustc::__rust_dealloc
	mov w20, #1
	mov w19, #1
LBB18_28:
	ldr x8, [sp, #56]
	cbz x8, LBB18_30
	ldr x0, [sp, #40]
	ldr x1, [sp, #56]
	mov w2, #1
	bl __rustc::__rust_dealloc
LBB18_30:
	tbz w19, #0, LBB18_33
	ldr x8, [sp, #48]
	ldr x8, [x8]
	cbz x8, LBB18_33
	ldr x9, [sp, #48]
	ldr x0, [x9, #8]
	lsl x1, x8, #2
	mov w2, #4
	bl __rustc::__rust_dealloc
LBB18_33:
	tbz w20, #0, LBB18_35
	ldr x8, [sp, #16]
	ldr x8, [x8]
	cbnz x8, LBB18_36
LBB18_35:
	mov x0, x24
	bl __Unwind_Resume
LBB18_36:
	ldr x9, [sp, #16]
	ldr x0, [x9, #8]
	lsl x1, x8, #2
	mov w2, #4
	bl __rustc::__rust_dealloc
	mov x0, x24
	bl __Unwind_Resume
	.loh AdrpAdd	Lloh438, Lloh439
