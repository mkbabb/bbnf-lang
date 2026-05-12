.section __TEXT,__text,regular,pure_instructions
	.p2align	2
runtime::generated_json::generated::parse_value:
Lfunc_begin51:
	.cfi_startproc
	sub sp, sp, #176
	.cfi_def_cfa_offset 176
	stp x26, x25, [sp, #96]
	stp x24, x23, [sp, #112]
	stp x22, x21, [sp, #128]
	stp x20, x19, [sp, #144]
	stp x29, x30, [sp, #160]
	add x29, sp, #160
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
	ldp x19, x21, [x1, #160]
	cmp x21, x19
	b.hs LBB51_5
	ldr x9, [x1, #152]
	mov w8, #1
	mov x10, #9728
	movk x10, #1, lsl #32
LBB51_2:
	ldrb w11, [x9, x21]
	cmp w11, #32
	lsl x11, x8, x11
	and x11, x11, x10
	ccmp x11, #0, #4, ls
	b.eq LBB51_9
	add x21, x21, #1
	cmp x19, x21
	b.ne LBB51_2
	mov x21, x19
LBB51_5:
	str x21, [x1, #168]
LBB51_6:
	ldp x8, x9, [x1, #136]
	str xzr, [x0]
LBB51_7:
	stp x8, x9, [x0, #24]
	str x21, [x0, #40]
LBB51_8:
	.cfi_def_cfa wsp, 176
	ldp x29, x30, [sp, #160]
	ldp x20, x19, [sp, #144]
	ldp x22, x21, [sp, #128]
	ldp x24, x23, [sp, #112]
	ldp x26, x25, [sp, #96]
	add sp, sp, #176
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
LBB51_9:
	.cfi_restore_state
	str x21, [x1, #168]
	ldrb w8, [x9, x21]
	cmp w8, #101
	mov x20, x0
	b.le LBB51_14
	cmp w8, #115
	b.gt LBB51_26
	cmp w8, #102
	b.eq LBB51_39
	cmp w8, #110
	b.ne LBB51_38
Lloh636:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.19@PAGE
Lloh637:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.19@PAGEOFF
	b LBB51_41
LBB51_14:
	cmp w8, #34
	b.eq LBB51_37
	cmp w8, #45
	b.eq LBB51_46
	cmp w8, #91
	b.ne LBB51_38
	mov x19, x1
	mov x0, x1
	mov w1, #91
	bl runtime::generated_json::generated::consume_structural
	tbz w0, #0, LBB51_72
	mov x0, x19
	ldp x9, x8, [x19, #160]
	cmp x8, x9
	b.hs LBB51_23
	ldr x10, [x0, #152]
	mov w11, #1
	mov x12, #9728
	movk x12, #1, lsl #32
LBB51_20:
	ldrb w13, [x10, x8]
	cmp w13, #32
	lsl x13, x11, x13
	and x13, x13, x12
	ccmp x13, #0, #4, ls
	b.eq LBB51_23
	add x8, x8, #1
	cmp x9, x8
	b.ne LBB51_20
	mov x8, x9
LBB51_23:
	str x8, [x0, #168]
	mov w1, #93
	bl runtime::generated_json::generated::consume_structural
	tbz w0, #0, LBB51_134
	lsr x8, x21, #32
	mov x0, x20
	cbz x8, LBB51_85
Lloh638:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.54@PAGE
Lloh639:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.54@PAGEOFF
Lloh640:
	adrp x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGE
Lloh641:
	add x3, x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGEOFF
Lloh642:
	adrp x4, l_anon.10e61d2d870443aea44842dd9d471920.58@PAGE
Lloh643:
	add x4, x4, l_anon.10e61d2d870443aea44842dd9d471920.58@PAGEOFF
	sub x2, x29, #65
	mov w1, #21
	bl core::result::unwrap_failed
LBB51_26:
	cmp w8, #116
	b.eq LBB51_40
	cmp w8, #123
	b.ne LBB51_38
	mov x19, x1
	mov x0, x1
	mov w1, #123
	bl runtime::generated_json::generated::consume_structural
	tbz w0, #0, LBB51_69
	mov x0, x19
	ldp x9, x8, [x19, #160]
	cmp x8, x9
	b.hs LBB51_34
	ldr x10, [x0, #152]
	mov w11, #1
	mov x12, #9728
	movk x12, #1, lsl #32
LBB51_31:
	ldrb w13, [x10, x8]
	cmp w13, #32
	lsl x13, x11, x13
	and x13, x13, x12
	ccmp x13, #0, #4, ls
	b.eq LBB51_34
	add x8, x8, #1
	cmp x9, x8
	b.ne LBB51_31
	mov x8, x9
LBB51_34:
	str x8, [x0, #168]
	mov w1, #125
	bl runtime::generated_json::generated::consume_structural
	tbz w0, #0, LBB51_86
	lsr x8, x21, #32
	mov x0, x20
	cbz x8, LBB51_85
Lloh644:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.54@PAGE
Lloh645:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.54@PAGEOFF
Lloh646:
	adrp x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGE
Lloh647:
	add x3, x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGEOFF
Lloh648:
	adrp x4, l_anon.10e61d2d870443aea44842dd9d471920.60@PAGE
Lloh649:
	add x4, x4, l_anon.10e61d2d870443aea44842dd9d471920.60@PAGEOFF
	sub x2, x29, #65
	mov w1, #21
	bl core::result::unwrap_failed
LBB51_37:
	add x0, sp, #40
	bl runtime::generated_json::generated::parse_string
	b LBB51_43
LBB51_38:
	sub w10, w8, #48
	mov x8, x21
	cmp w10, #10
	mov x0, x20
	b.hs LBB51_6
	b LBB51_47
LBB51_39:
Lloh650:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.21@PAGE
Lloh651:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.21@PAGEOFF
	add x0, sp, #40
	mov w3, #5
	b LBB51_42
LBB51_40:
Lloh652:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.20@PAGE
Lloh653:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.20@PAGEOFF
LBB51_41:
	add x0, sp, #40
	mov w3, #4
LBB51_42:
	bl runtime::generated_json::generated::parse_literal
LBB51_43:
	ldr x8, [sp, #40]
	ldr w21, [sp, #48]
	cmp x8, #10
	b.ne LBB51_45
	mov x0, x20
	b LBB51_85
LBB51_45:
	ldur q0, [sp, #52]
	stur q0, [x20, #12]
	ldur q0, [sp, #68]
	stur q0, [x20, #28]
	ldr w9, [sp, #84]
	str w9, [x20, #44]
	str x8, [x20]
	str w21, [x20, #8]
	b LBB51_8
LBB51_46:
	add x8, x21, #1
	cmp x8, x19
	mov x0, x20
	b.hs LBB51_171
LBB51_47:
	cmp x8, x19
	b.hs LBB51_181
	ldrb w10, [x9, x8]
	cmp w10, #48
	b.ne LBB51_75
	add x10, x8, #1
LBB51_50:
	cmp x10, x19
	b.hs LBB51_58
	ldrb w8, [x9, x10]
	cmp w8, #46
	b.ne LBB51_58
	add x8, x10, #1
	cmp x8, x19
	b.hs LBB51_171
	mov x10, x8
LBB51_54:
	ldrb w11, [x9, x10]
	sub w11, w11, #48
	cmp w11, #9
	b.hi LBB51_57
	add x10, x10, #1
	cmp x19, x10
	b.ne LBB51_54
	mov x10, x19
LBB51_57:
	cmp x10, x8
	b.eq LBB51_171
LBB51_58:
	cmp x10, x19
	b.hs LBB51_79
	ldrb w8, [x9, x10]
	orr w8, w8, #0x20
	cmp w8, #101
	b.ne LBB51_79
	add x8, x10, #1
	cmp x8, x19
	b.hs LBB51_64
	ldrb w11, [x9, x8]
	cmp w11, #45
	b.eq LBB51_63
	cmp w11, #43
	b.ne LBB51_64
LBB51_63:
	add x8, x10, #2
LBB51_64:
	cmp x8, x19
	b.hs LBB51_171
	mov x10, x8
LBB51_66:
	ldrb w11, [x9, x10]
	sub w11, w11, #48
	cmp w11, #9
	b.hi LBB51_169
	add x10, x10, #1
	cmp x19, x10
	b.ne LBB51_66
	mov x10, x19
	b LBB51_170
LBB51_69:
	mov x8, #0
	ldp x11, x12, [x19, #136]
LBB51_70:
	ldr x10, [x19, #168]
LBB51_71:
	mov x9, x20
	b LBB51_168
LBB51_72:
	mov x8, #0
	ldp x9, x10, [x19, #136]
LBB51_73:
	ldr x11, [x19, #168]
LBB51_74:
	str x8, [x20]
	str w12, [x20, #8]
	ldr x8, [sp, #8]
	stur x8, [x20, #12]
	ldr w8, [sp, #16]
	str w8, [x20, #20]
	stp x9, x10, [x20, #24]
	str x11, [x20, #40]
	b LBB51_8
LBB51_75:
	sub w10, w10, #49
	cmp w10, #8
	b.hi LBB51_171
	add x11, x9, #1
	sub x12, x19, #1
LBB51_77:
	cmp x12, x8
	b.eq LBB51_80
	add x10, x8, #1
	ldrb w8, [x11, x8]
	sub w13, w8, #48
	mov x8, x10
	cmp w13, #10
	b.lo LBB51_77
	b LBB51_50
LBB51_79:
	mov x19, x10
LBB51_80:
	ldr x22, [x1, #88]
	lsr x8, x22, #32
	cbnz x8, LBB51_180
	lsr x8, x21, #32
	cbnz x8, LBB51_180
	ldur x8, [x1, #72]
	cmp x22, x8
	b.ne LBB51_84
	add x0, x1, #72
	mov x23, x1
	bl <alloc::raw_vec::RawVec<u32>>::grow_one
	mov x1, x23
	mov x0, x20
LBB51_84:
	ldr x8, [x1, #80]
	str w21, [x8, x22, lsl #2]
	add x8, x22, #1
	str x8, [x1, #88]
	str x19, [x1, #168]
LBB51_85:
	str w21, [x0, #8]
	mov w8, #10
	str x8, [x0]
	b LBB51_8
LBB51_86:
	mov x8, x19
	ldp x9, x22, [x19, #160]
	cmp x22, x9
	mov x9, x20
	b.hs LBB51_164
	ldr x11, [x8, #152]
	mov w23, #1
	mov x24, #9728
	movk x24, #1, lsl #32
LBB51_88:
	ldrb w10, [x11, x22]
	cmp w10, #34
	b.ne LBB51_164
	add x0, sp, #40
	mov x1, x8
	bl runtime::generated_json::generated::parse_string
	ldr x8, [sp, #40]
	cmp x8, #10
	b.ne LBB51_179
	mov x1, x19
	ldp x8, x11, [x19, #152]
	ldr x10, [x19, #168]
	cmp x10, x11
	mov x9, x20
	b.hs LBB51_94
LBB51_91:
	ldrb w12, [x8, x10]
	cmp w12, #32
	lsl x12, x23, x12
	and x12, x12, x24
	ccmp x12, #0, #4, ls
	b.eq LBB51_98
	add x10, x10, #1
	cmp x11, x10
	b.ne LBB51_91
	mov w13, #0
	mov x10, x11
	str x11, [x1, #168]
	ldr x14, [x1, #16]
	ldr x12, [x1, #176]
	cmp x12, x14
	b.lo LBB51_95
	b LBB51_99
LBB51_94:
	mov w13, #0
	str x10, [x1, #168]
	ldr x14, [x1, #16]
	ldr x12, [x1, #176]
	cmp x12, x14
	b.hs LBB51_99
LBB51_95:
	ldr x15, [x1, #8]
LBB51_96:
	ldr w16, [x15, x12, lsl #2]
	cmp x10, x16
	b.ls LBB51_99
	add x12, x12, #1
	str x12, [x1, #176]
	cmp x14, x12
	b.ne LBB51_96
	b LBB51_167
LBB51_98:
	mov w13, #1
	str x10, [x1, #168]
	ldr x14, [x1, #16]
	ldr x12, [x1, #176]
	cmp x12, x14
	b.lo LBB51_95
LBB51_99:
	cmp x12, x14
	b.hs LBB51_167
	ldr x14, [x1, #8]
	ldr w14, [x14, x12, lsl #2]
	cmp x10, x14
	b.ne LBB51_102
	cmp x11, x10
	b.hi LBB51_108
	b LBB51_167
LBB51_102:
	mov x15, x10
	cbz w13, LBB51_167
LBB51_103:
	ldrb w13, [x8, x15]
	cmp w13, #32
	lsl x13, x23, x13
	and x13, x13, x24
	ccmp x13, #0, #4, ls
	b.eq LBB51_106
	add x15, x15, #1
	cmp x11, x15
	b.ne LBB51_103
	mov x15, x11
LBB51_106:
	cmp x11, x14
	b.ls LBB51_167
	cmp x15, x14
	b.ne LBB51_167
LBB51_108:
	ldrb w8, [x8, x14]
	cmp w8, #58
	b.ne LBB51_167
	add x8, x12, #1
	add x9, x14, #1
	stp x9, x8, [x1, #168]
	add x0, sp, #40
	bl runtime::generated_json::generated::parse_value
	ldr x8, [sp, #40]
	cmp x8, #10
	b.ne LBB51_179
	lsr x8, x22, #32
	cbnz x8, LBB51_182
	mov x8, x19
	ldp x11, x12, [x19, #152]
	ldr x10, [x19, #168]
	cmp x10, x12
	mov x9, x20
	b.hs LBB51_115
LBB51_112:
	ldrb w13, [x11, x10]
	cmp w13, #32
	lsl x13, x23, x13
	and x13, x13, x24
	ccmp x13, #0, #4, ls
	b.eq LBB51_119
	add x10, x10, #1
	cmp x12, x10
	b.ne LBB51_112
	mov w14, #0
	mov x10, x12
	str x12, [x8, #168]
	ldr x15, [x8, #16]
	ldr x13, [x8, #176]
	cmp x13, x15
	b.lo LBB51_116
	b LBB51_120
LBB51_115:
	mov w14, #0
	str x10, [x8, #168]
	ldr x15, [x8, #16]
	ldr x13, [x8, #176]
	cmp x13, x15
	b.hs LBB51_120
LBB51_116:
	ldr x16, [x8, #8]
LBB51_117:
	ldr w17, [x16, x13, lsl #2]
	cmp x10, x17
	b.ls LBB51_120
	add x13, x13, #1
	str x13, [x8, #176]
	cmp x15, x13
	b.ne LBB51_117
	b LBB51_172
LBB51_119:
	mov w14, #1
	str x10, [x8, #168]
	ldr x15, [x8, #16]
	ldr x13, [x8, #176]
	cmp x13, x15
	b.lo LBB51_116
LBB51_120:
	cmp x13, x15
	b.hs LBB51_172
	ldr x15, [x8, #8]
	ldr w15, [x15, x13, lsl #2]
	cmp x10, x15
	b.ne LBB51_123
	cmp x12, x10
	b.hi LBB51_129
	b LBB51_172
LBB51_123:
	cbz w14, LBB51_172
LBB51_124:
	ldrb w14, [x11, x10]
	cmp w14, #32
	lsl x14, x23, x14
	and x14, x14, x24
	ccmp x14, #0, #4, ls
	b.eq LBB51_127
	add x10, x10, #1
	cmp x12, x10
	b.ne LBB51_124
	mov x10, x12
LBB51_127:
	cmp x12, x15
	b.ls LBB51_172
	cmp x10, x15
	b.ne LBB51_172
LBB51_129:
	ldrb w10, [x11, x15]
	cmp w10, #44
	b.ne LBB51_172
	add x13, x13, #1
	add x10, x15, #1
	stp x10, x13, [x8, #168]
	cmp x10, x12
	b.hs LBB51_133
LBB51_131:
	ldrb w13, [x11, x10]
	cmp w13, #32
	lsl x13, x23, x13
	and x13, x13, x24
	ccmp x13, #0, #4, ls
	b.eq LBB51_133
	add x10, x10, #1
	cmp x12, x10
	b.ne LBB51_131
	b LBB51_175
LBB51_133:
	str x10, [x8, #168]
	mov x25, x22
	mov x22, x10
	cmp x10, x12
	b.lo LBB51_88
	b LBB51_165
LBB51_134:
	mov x8, x19
	ldp x9, x11, [x19, #160]
	mov w22, #1
	mov x23, #9728
	movk x23, #1, lsl #32
	cmp x11, x9
	b.hs LBB51_137
LBB51_135:
	ldr x9, [x8, #152]
	ldrb w9, [x9, x11]
	cmp w9, #93
	b.ne LBB51_137
	b LBB51_178
LBB51_136:
	str x11, [x8, #168]
	cmp x11, x9
	b.lo LBB51_135
LBB51_137:
	add x0, sp, #40
	mov x1, x8
	bl runtime::generated_json::generated::parse_value
	ldr x8, [sp, #40]
	cmp x8, #10
	b.ne LBB51_176
	mov x8, x19
	ldp x10, x9, [x19, #152]
	ldr x11, [x19, #168]
	cmp x11, x9
	b.hs LBB51_142
LBB51_139:
	ldrb w12, [x10, x11]
	cmp w12, #32
	lsl x12, x22, x12
	and x12, x12, x23
	ccmp x12, #0, #4, ls
	b.eq LBB51_146
	add x11, x11, #1
	cmp x9, x11
	b.ne LBB51_139
	mov w13, #0
	mov x11, x9
	str x9, [x8, #168]
	ldr x14, [x8, #16]
	ldr x12, [x8, #176]
	cmp x12, x14
	b.lo LBB51_143
	b LBB51_147
LBB51_142:
	mov w13, #0
	str x11, [x8, #168]
	ldr x14, [x8, #16]
	ldr x12, [x8, #176]
	cmp x12, x14
	b.hs LBB51_147
LBB51_143:
	ldr x15, [x8, #8]
LBB51_144:
	ldr w16, [x15, x12, lsl #2]
	cmp x11, x16
	b.ls LBB51_147
	add x12, x12, #1
	str x12, [x8, #176]
	cmp x14, x12
	b.ne LBB51_144
	b LBB51_161
LBB51_146:
	mov w13, #1
	str x11, [x8, #168]
	ldr x14, [x8, #16]
	ldr x12, [x8, #176]
	cmp x12, x14
	b.lo LBB51_143
LBB51_147:
	cmp x12, x14
	b.hs LBB51_161
	ldr x14, [x8, #8]
	ldr w14, [x14, x12, lsl #2]
	cmp x11, x14
	b.ne LBB51_150
	cmp x9, x11
	b.hi LBB51_156
	b LBB51_161
LBB51_150:
	cbz w13, LBB51_161
LBB51_151:
	ldrb w13, [x10, x11]
	cmp w13, #32
	lsl x13, x22, x13
	and x13, x13, x23
	ccmp x13, #0, #4, ls
	b.eq LBB51_154
	add x11, x11, #1
	cmp x9, x11
	b.ne LBB51_151
	mov x11, x9
LBB51_154:
	cmp x9, x14
	b.ls LBB51_161
	cmp x11, x14
	b.ne LBB51_161
LBB51_156:
	ldrb w11, [x10, x14]
	cmp w11, #44
	b.ne LBB51_161
	add x12, x12, #1
	add x11, x14, #1
	stp x11, x12, [x8, #168]
	cmp x11, x9
	b.hs LBB51_136
LBB51_158:
	ldrb w12, [x10, x11]
	cmp w12, #32
	lsl x12, x22, x12
	and x12, x12, x23
	ccmp x12, #0, #4, ls
	b.eq LBB51_136
	add x11, x11, #1
	cmp x9, x11
	b.ne LBB51_158
	mov x11, x9
	str x9, [x8, #168]
	cmp x9, x9
	b.lo LBB51_135
	b LBB51_137
LBB51_161:
	mov x0, x8
	mov w1, #93
	bl runtime::generated_json::generated::consume_structural
	tbz w0, #0, LBB51_166
	lsr x8, x21, #32
	mov x0, x20
	cbz x8, LBB51_85
Lloh654:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.54@PAGE
Lloh655:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.54@PAGEOFF
Lloh656:
	adrp x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGE
Lloh657:
	add x3, x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGEOFF
Lloh658:
	adrp x4, l_anon.10e61d2d870443aea44842dd9d471920.57@PAGE
Lloh659:
	add x4, x4, l_anon.10e61d2d870443aea44842dd9d471920.57@PAGEOFF
	sub x2, x29, #65
	mov w1, #21
	bl core::result::unwrap_failed
LBB51_164:
	mov x10, x22
LBB51_165:
	ldp x11, x12, [x8, #136]
	mov w8, #1
	b LBB51_168
LBB51_166:
	ldp x9, x10, [x19, #136]
	mov w8, #5
	b LBB51_73
LBB51_167:
	ldp x11, x12, [x1, #136]
	mov w8, #3
LBB51_168:
	str x8, [x9]
	str w25, [x9, #8]
	ldr x8, [sp, #24]
	stur x8, [x9, #12]
	ldr w8, [sp, #32]
	str w8, [x9, #20]
	stp x11, x12, [x9, #24]
	str x10, [x9, #40]
	b LBB51_8
LBB51_169:
	mov x19, x10
LBB51_170:
	cmp x10, x8
	b.ne LBB51_80
LBB51_171:
	ldp x8, x9, [x1, #136]
	mov w10, #7
	str x10, [x0]
	b LBB51_7
LBB51_172:
	mov x0, x8
	mov w1, #125
	bl runtime::generated_json::generated::consume_structural
	tbz w0, #0, LBB51_177
	lsr x8, x21, #32
	mov x0, x20
	cbz x8, LBB51_85
Lloh660:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.54@PAGE
Lloh661:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.54@PAGEOFF
Lloh662:
	adrp x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGE
Lloh663:
	add x3, x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGEOFF
Lloh664:
	adrp x4, l_anon.10e61d2d870443aea44842dd9d471920.59@PAGE
Lloh665:
	add x4, x4, l_anon.10e61d2d870443aea44842dd9d471920.59@PAGEOFF
	sub x2, x29, #65
	mov w1, #21
	bl core::result::unwrap_failed
LBB51_175:
	str x12, [x8, #168]
	mov x10, x12
	b LBB51_165
LBB51_176:
	ldr w12, [sp, #48]
	ldur x9, [sp, #52]
	str x9, [sp, #8]
	ldr w9, [sp, #60]
	str w9, [sp, #16]
	ldp x9, x10, [sp, #64]
	ldr x11, [sp, #80]
	b LBB51_74
LBB51_177:
	ldp x11, x12, [x19, #136]
	mov w8, #4
	b LBB51_70
LBB51_178:
	ldp x9, x10, [x8, #136]
	mov w8, #2
	b LBB51_74
LBB51_179:
	ldr w25, [sp, #48]
	ldur x9, [sp, #52]
	str x9, [sp, #24]
	ldr w9, [sp, #60]
	str w9, [sp, #32]
	ldp x11, x12, [sp, #64]
	ldr x10, [sp, #80]
	b LBB51_71
LBB51_180:
Lloh666:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.36@PAGE
Lloh667:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.36@PAGEOFF
Lloh668:
	adrp x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGE
Lloh669:
	add x3, x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGEOFF
Lloh670:
	adrp x4, l_anon.10e61d2d870443aea44842dd9d471920.37@PAGE
Lloh671:
	add x4, x4, l_anon.10e61d2d870443aea44842dd9d471920.37@PAGEOFF
	sub x2, x29, #65
	mov w1, #52
	bl core::result::unwrap_failed
LBB51_181:
Lloh672:
	adrp x2, l_anon.10e61d2d870443aea44842dd9d471920.4@PAGE
Lloh673:
	add x2, x2, l_anon.10e61d2d870443aea44842dd9d471920.4@PAGEOFF
	mov x0, x8
	mov x1, x19
	bl core::panicking::panic_bounds_check
LBB51_182:
Lloh674:
	adrp x0, l_anon.10e61d2d870443aea44842dd9d471920.54@PAGE
Lloh675:
	add x0, x0, l_anon.10e61d2d870443aea44842dd9d471920.54@PAGEOFF
Lloh676:
	adrp x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGE
Lloh677:
	add x3, x3, l_anon.10e61d2d870443aea44842dd9d471920.9@PAGEOFF
Lloh678:
	adrp x4, l_anon.10e61d2d870443aea44842dd9d471920.56@PAGE
Lloh679:
	add x4, x4, l_anon.10e61d2d870443aea44842dd9d471920.56@PAGEOFF
	sub x2, x29, #65
	mov w1, #21
	bl core::result::unwrap_failed
	.loh AdrpAdd	Lloh636, Lloh637
	.loh AdrpAdd	Lloh642, Lloh643
	.loh AdrpAdd	Lloh640, Lloh641
	.loh AdrpAdd	Lloh638, Lloh639
	.loh AdrpAdd	Lloh648, Lloh649
	.loh AdrpAdd	Lloh646, Lloh647
	.loh AdrpAdd	Lloh644, Lloh645
	.loh AdrpAdd	Lloh650, Lloh651
	.loh AdrpAdd	Lloh652, Lloh653
	.loh AdrpAdd	Lloh658, Lloh659
	.loh AdrpAdd	Lloh656, Lloh657
	.loh AdrpAdd	Lloh654, Lloh655
	.loh AdrpAdd	Lloh664, Lloh665
	.loh AdrpAdd	Lloh662, Lloh663
	.loh AdrpAdd	Lloh660, Lloh661
	.loh AdrpAdd	Lloh670, Lloh671
	.loh AdrpAdd	Lloh668, Lloh669
	.loh AdrpAdd	Lloh666, Lloh667
	.loh AdrpAdd	Lloh672, Lloh673
	.loh AdrpAdd	Lloh678, Lloh679
	.loh AdrpAdd	Lloh676, Lloh677
	.loh AdrpAdd	Lloh674, Lloh675
Lfunc_end51:
	.cfi_endproc

	.p2align	2
