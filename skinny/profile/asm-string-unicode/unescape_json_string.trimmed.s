	.globl	parse_that_regex::unescape_json_string
	.p2align	2
parse_that_regex::unescape_json_string:
Lfunc_begin3:
	.cfi_startproc
	.cfi_personality 155, _rust_eh_personality
	.cfi_lsda 16, Lexception0
	sub sp, sp, #144
	.cfi_def_cfa_offset 144
	stp x28, x27, [sp, #48]
	stp x26, x25, [sp, #64]
	stp x24, x23, [sp, #80]
	stp x22, x21, [sp, #96]
	stp x20, x19, [sp, #112]
	stp x29, x30, [sp, #128]
	add x29, sp, #128
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
	mov x21, x0
	mov x19, x8
	cmp x1, #15
	b.hi LBB3_5
	cbz x20, LBB3_10
	mov x8, x21
	mov x9, x20
LBB3_3:
	ldrb w10, [x8], #1
	cmp w10, #92
	b.eq LBB3_6
	subs x9, x9, #1
	b.ne LBB3_3
	b LBB3_10
LBB3_5:
	mov w0, #92
	mov x1, x21
	mov x2, x20
	bl core::slice::memchr::memchr_aligned
	cmp x0, #1
	b.ne LBB3_10
LBB3_6:
	tbnz x20, #63, LBB3_124
	cbz x20, LBB3_110
	bl __rustc::__rust_no_alloc_shim_is_unstable_v2
	mov w22, #1
	mov x0, x20
	mov w1, #1
	bl __rustc::__rust_alloc
	cbz x0, LBB3_125
	mov x22, #0
	mov x23, #0
	stp x20, x0, [sp, #8]
	str xzr, [sp, #24]
	mov w8, #-1114112
	orr w8, w8, #0x800
	str w8, [sp, #4]
	mov w27, #2
	mov w28, #3
	b LBB3_14
LBB3_10:
	mov x8, #-9223372036854775808
	stp x8, x21, [x19]
	str x20, [x19, #16]
	b LBB3_116
LBB3_11:
	ldr x0, [sp, #16]
	mov w9, #8
LBB3_12:
	strb w9, [x0, x8]
	add x22, x22, #1
	str x22, [sp, #24]
	add x24, x23, #2
LBB3_13:
	mov x23, x24
	cmp x24, x20
	b.hs LBB3_111
LBB3_14:
	add x8, x21, x23
	ldrb w25, [x8]
	cmp w25, #92
	b.ne LBB3_25
	add x8, x23, #1
	cmp x8, x20
	b.hs LBB3_112
	ldrb w9, [x21, x8]
	mov w8, #3
	cmp w9, #101
	b.le LBB3_32
	cmp w9, #113
	b.le LBB3_45
	cmp w9, #114
	b.eq LBB3_56
	cmp w9, #116
	b.eq LBB3_50
	cmp w9, #117
	b.ne LBB3_114
	add x0, sp, #32
	add x3, x23, #2
	mov x1, x21
	mov x2, x20
	bl parse_that_regex::read_hex_unit
	ldrb w8, [sp, #40]
	cmp w8, #6
	b.ne LBB3_117
	ldrh w25, [sp, #32]
	add x24, x23, #6
	and w8, w25, #0xfc00
	mov w9, #55296
	cmp w8, w9
	b.eq LBB3_71
	mov w9, #56320
	cmp w8, w9
	b.ne LBB3_79
	b LBB3_119
LBB3_25:
	cmp w25, #32
	b.lo LBB3_113
	cbz x23, LBB3_28
	sxtb w9, w25
	cmn w9, #65
	b.le LBB3_122
LBB3_28:
	tbnz w25, #7, LBB3_37
	mov w26, #1
	mov w24, #1
	ldr x8, [sp, #8]
	sub x8, x8, x22
	cmp x24, x8
	mov x8, x22
	b.hi LBB3_62
LBB3_30:
	add x8, x0, x8
	tbz w26, #0, LBB3_64
LBB3_31:
	strb w25, [x8]
	mov w10, #1
	b LBB3_70
LBB3_32:
	cmp w9, #91
	b.gt LBB3_40
	cmp w9, #34
	b.eq LBB3_52
	cmp w9, #47
	b.ne LBB3_114
	ldr x9, [sp, #8]
	mov x8, x22
	cmp x9, x22
	b.eq LBB3_88
LBB3_36:
	ldr x0, [sp, #16]
	mov w9, #47
	b LBB3_12
LBB3_37:
	ldrb w9, [x8, #1]
	and w10, w9, #0x3f
	cmp w25, #224
	b.lo LBB3_49
	and w9, w25, #0x1f
	ldrb w11, [x8, #2]
	and w11, w11, #0x3f
	orr w10, w11, w10, lsl #6
	cmp w25, #240
	b.lo LBB3_60
	ldrb w8, [x8, #3]
	and w8, w8, #0x3f
	orr w25, w8, w10, lsl #6
	bfi w25, w9, #18, #3
	b LBB3_61
LBB3_40:
	cmp w9, #92
	b.eq LBB3_54
	cmp w9, #98
	b.ne LBB3_114
	ldr x9, [sp, #8]
	mov x8, x22
	cmp x9, x22
	b.ne LBB3_11
	add x0, sp, #8
	mov x1, x22
	mov w2, #1
	bl <alloc::raw_vec::RawVecInner<_>>::reserve::do_reserve_and_handle::<alloc::alloc::Global>
	ldr x8, [sp, #24]
	b LBB3_11
LBB3_45:
	cmp w9, #102
	b.eq LBB3_58
	cmp w9, #110
	b.ne LBB3_114
	ldr x9, [sp, #8]
	mov x8, x22
	cmp x9, x22
	b.eq LBB3_98
LBB3_48:
	ldr x0, [sp, #16]
	mov w9, #10
	b LBB3_12
LBB3_49:
	bfi w10, w25, #6, #5
	mov x25, x10
	b LBB3_61
LBB3_50:
	ldr x9, [sp, #8]
	mov x8, x22
	cmp x9, x22
	b.eq LBB3_90
LBB3_51:
	ldr x0, [sp, #16]
	mov w9, #9
	b LBB3_12
LBB3_52:
	ldr x9, [sp, #8]
	mov x8, x22
	cmp x9, x22
	b.eq LBB3_92
LBB3_53:
	ldr x0, [sp, #16]
	mov w9, #34
	b LBB3_12
LBB3_54:
	ldr x9, [sp, #8]
	mov x8, x22
	cmp x9, x22
	b.eq LBB3_94
LBB3_55:
	ldr x0, [sp, #16]
	mov w9, #92
	b LBB3_12
LBB3_56:
	ldr x9, [sp, #8]
	mov x8, x22
	cmp x9, x22
	b.eq LBB3_96
LBB3_57:
	ldr x0, [sp, #16]
	mov w9, #13
	b LBB3_12
LBB3_58:
	ldr x9, [sp, #8]
	mov x8, x22
	cmp x9, x22
	b.eq LBB3_100
LBB3_59:
	ldr x0, [sp, #16]
	mov w9, #12
	b LBB3_12
LBB3_60:
	orr w25, w10, w9, lsl #12
LBB3_61:
	cmp w25, #16, lsl #12
	cinc x8, x28, hs
	cmp w25, #2048
	csel x8, x27, x8, lo
	cmp w25, #128
	cset w26, lo
	csinc x24, x8, xzr, hs
	ldr x8, [sp, #8]
	sub x8, x8, x22
	cmp x24, x8
	mov x8, x22
	b.ls LBB3_30
LBB3_62:
	add x0, sp, #8
	mov x1, x22
	mov x2, x24
	bl <alloc::raw_vec::RawVecInner<_>>::reserve::do_reserve_and_handle::<alloc::alloc::Global>
	ldp x0, x8, [sp, #16]
	add x8, x0, x8
	tbnz w26, #0, LBB3_31
LBB3_64:
	mov w9, #-128
	bfxil w9, w25, #0, #6
	cmp w25, #2048
	b.hs LBB3_66
	lsr w10, w25, #6
	orr w10, w10, #0xc0
	strb w10, [x8]
	strb w9, [x8, #1]
	mov w10, #2
	b LBB3_70
LBB3_66:
	mov w11, #-128
	bfxil w11, w25, #6, #6
	lsr w10, w25, #16
	cbnz w10, LBB3_68
	lsr w10, w25, #12
	orr w12, w10, #0xffffffe0
	mov w10, #3
	mov x13, x11
	b LBB3_69
LBB3_68:
	mov w13, #-128
	bfxil w13, w25, #12, #6
	lsr w10, w25, #18
	orr w12, w10, #0xfffffff0
	strb w9, [x8, #3]
	mov w10, #4
	mov x9, x11
LBB3_69:
	strb w12, [x8]
	strb w13, [x8, #1]
	strb w9, [x8, #2]
LBB3_70:
	add x22, x24, x22
	str x22, [sp, #24]
	add x24, x10, x23
	b LBB3_13
LBB3_71:
	cmp x24, x20
	b.hs LBB3_120
	ldrb w8, [x21, x24]
	cmp w8, #92
	b.ne LBB3_120
	add x8, x23, #7
	cmp x8, x20
	b.hs LBB3_120
	ldrb w8, [x21, x8]
	cmp w8, #117
	b.ne LBB3_120
	add x0, sp, #32
	add x3, x23, #8
	mov x1, x21
	mov x2, x20
	bl parse_that_regex::read_hex_unit
	ldrb w8, [sp, #40]
	cmp w8, #6
	b.ne LBB3_117
	ldrh w8, [sp, #32]
	and w9, w8, #0xfc00
	mov w10, #56320
	cmp w9, w10
	b.ne LBB3_121
	add x24, x23, #12
	add w8, w8, w25, lsl #10
	mov w9, #9216
	movk w9, #64672, lsl #16
	add w25, w8, w9
LBB3_79:
	mov w8, #55296
	eor w8, w25, w8
	sub w8, w8, #272, lsl #12
	ldr w9, [sp, #4]
	cmp w8, w9
	b.lo LBB3_118
	cmp w25, #128
	b.hs LBB3_82
	mov w23, #1
	b LBB3_85
LBB3_82:
	cmp w25, #2048
	b.hs LBB3_84
	mov w23, #2
	b LBB3_85
LBB3_84:
	cmp w25, #16, lsl #12
	cinc x23, x28, hs
LBB3_85:
	ldr x8, [sp, #8]
	sub x8, x8, x22
	cmp x23, x8
	mov x8, x22
	b.hi LBB3_102
	ldr x0, [sp, #16]
	add x8, x0, x8
	cmp w25, #128
	b.hs LBB3_104
LBB3_87:
	strb w25, [x8]
	b LBB3_109
LBB3_88:
	add x0, sp, #8
	mov x1, x22
	mov w2, #1
	bl <alloc::raw_vec::RawVecInner<_>>::reserve::do_reserve_and_handle::<alloc::alloc::Global>
	ldr x8, [sp, #24]
	b LBB3_36
LBB3_90:
	add x0, sp, #8
	mov x1, x22
	mov w2, #1
	bl <alloc::raw_vec::RawVecInner<_>>::reserve::do_reserve_and_handle::<alloc::alloc::Global>
	ldr x8, [sp, #24]
	b LBB3_51
LBB3_92:
	add x0, sp, #8
	mov x1, x22
	mov w2, #1
	bl <alloc::raw_vec::RawVecInner<_>>::reserve::do_reserve_and_handle::<alloc::alloc::Global>
	ldr x8, [sp, #24]
	b LBB3_53
LBB3_94:
	add x0, sp, #8
	mov x1, x22
	mov w2, #1
	bl <alloc::raw_vec::RawVecInner<_>>::reserve::do_reserve_and_handle::<alloc::alloc::Global>
	ldr x8, [sp, #24]
	b LBB3_55
LBB3_96:
	add x0, sp, #8
	mov x1, x22
	mov w2, #1
	bl <alloc::raw_vec::RawVecInner<_>>::reserve::do_reserve_and_handle::<alloc::alloc::Global>
	ldr x8, [sp, #24]
	b LBB3_57
LBB3_98:
	add x0, sp, #8
	mov x1, x22
	mov w2, #1
	bl <alloc::raw_vec::RawVecInner<_>>::reserve::do_reserve_and_handle::<alloc::alloc::Global>
	ldr x8, [sp, #24]
	b LBB3_48
LBB3_100:
	add x0, sp, #8
	mov x1, x22
	mov w2, #1
	bl <alloc::raw_vec::RawVecInner<_>>::reserve::do_reserve_and_handle::<alloc::alloc::Global>
	ldr x8, [sp, #24]
	b LBB3_59
LBB3_102:
	add x0, sp, #8
	mov x1, x22
	mov x2, x23
	bl <alloc::raw_vec::RawVecInner<_>>::reserve::do_reserve_and_handle::<alloc::alloc::Global>
	ldp x0, x8, [sp, #16]
	add x8, x0, x8
	cmp w25, #128
	b.lo LBB3_87
LBB3_104:
	mov w9, #-128
	bfxil w9, w25, #0, #6
	cmp w25, #2048
	b.hs LBB3_106
	lsr w10, w25, #6
	orr w10, w10, #0xc0
	strb w10, [x8]
	strb w9, [x8, #1]
	b LBB3_109
LBB3_106:
	mov w10, #-128
	bfxil w10, w25, #6, #6
	lsr w11, w25, #16
	cbnz w11, LBB3_108
	lsr w11, w25, #12
	orr w11, w11, #0xe0
	strb w11, [x8]
	strb w10, [x8, #1]
	strb w9, [x8, #2]
	b LBB3_109
LBB3_108:
	mov w11, #-128
	lsr w12, w25, #18
	orr w12, w12, #0xfffffff0
	strb w12, [x8]
	bfxil w11, w25, #12, #6
	strb w11, [x8, #1]
	strb w10, [x8, #2]
	strb w9, [x8, #3]
LBB3_109:
	add x22, x23, x22
	str x22, [sp, #24]
	b LBB3_13
LBB3_110:
	mov w8, #1
	stp x20, x8, [sp, #8]
	str xzr, [sp, #24]
LBB3_111:
	ldur q0, [sp, #8]
	str q0, [x19]
	ldr x8, [sp, #24]
	str x8, [x19, #16]
	b LBB3_116
LBB3_112:
	mov w8, #3
	b LBB3_114
LBB3_113:
	mov w8, #2
LBB3_114:
	strb w8, [x19, #16]
	mov x8, #-9223372036854775807
	stp x8, x23, [x19]
	ldr x1, [sp, #8]
	cbz x1, LBB3_116
	ldr x0, [sp, #16]
	mov w2, #1
	bl __rustc::__rust_dealloc
LBB3_116:
	.cfi_def_cfa wsp, 144
	ldp x29, x30, [sp, #128]
	ldp x20, x19, [sp, #112]
	ldp x22, x21, [sp, #96]
	ldp x24, x23, [sp, #80]
	ldp x26, x25, [sp, #64]
	ldp x28, x27, [sp, #48]
	add sp, sp, #144
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
LBB3_117:
	.cfi_restore_state
	ldr x23, [sp, #32]
	b LBB3_114
LBB3_118:
	sub x23, x24, #4
	mov w8, #4
	b LBB3_114
LBB3_119:
	add x23, x23, #2
LBB3_120:
	mov w8, #5
	b LBB3_114
LBB3_121:
	add x23, x23, #8
	mov w8, #5
	b LBB3_114
LBB3_122:
Lloh6:
	adrp x4, l_anon.028aad4ed9a93f18e57e62edb80815c2.2@PAGE
Lloh7:
	add x4, x4, l_anon.028aad4ed9a93f18e57e62edb80815c2.2@PAGEOFF
	mov x0, x21
	mov x1, x20
	mov x2, x23
	mov x3, x20
	bl core::str::slice_error_fail
	brk #0x1
LBB3_124:
	mov x22, #0
LBB3_125:
	mov x0, x22
	mov x1, x20
	bl alloc::raw_vec::handle_error
	b LBB3_128
LBB3_128:
	mov x19, x0
	ldr x1, [sp, #8]
	cbz x1, LBB3_130
	ldr x0, [sp, #16]
	mov w2, #1
	bl __rustc::__rust_dealloc
LBB3_130:
	mov x0, x19
	bl __Unwind_Resume
	.loh AdrpAdd	Lloh6, Lloh7
