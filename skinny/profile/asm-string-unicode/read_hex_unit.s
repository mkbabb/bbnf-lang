warning: patch `bbnf v0.2.11 (/Users/mkbabb/Programming/bbnf-lang/crates/core)` was not used in the crate graph
  |
  = help: perhaps you meant one of the following:
          	/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf
warning: patch `bbnf-ir v0.1.6 (/Users/mkbabb/Programming/bbnf-lang/crates/ir)` was not used in the crate graph
warning: patch `bbnf-regex v0.1.0 (/Users/mkbabb/Programming/parse-that/rust/regex)` was not used in the crate graph
warning: patch `bbnf-ser v0.1.0 (/Users/mkbabb/Programming/bbnf-lang/crates/ser)` was not used in the crate graph
warning: patch `csp-solver v0.1.0 (/Users/mkbabb/Programming/bbnf-lang/crates/csp-solver)` was not used in the crate graph
warning: patch `egraph v0.1.0 (/Users/mkbabb/Programming/bbnf-lang/crates/egraph)` was not used in the crate graph
warning: patch `egraph-derive v0.1.0 (/Users/mkbabb/Programming/bbnf-lang/crates/egraph-derive)` was not used in the crate graph
warning: patch `gorgeous v0.1.10 (/Users/mkbabb/Programming/bbnf-lang/crates/gorgeous)` was not used in the crate graph
warning: patch `parse_that v0.4.0 (/Users/mkbabb/Programming/parse-that/rust/parse_that)` was not used in the crate graph
warning: patch `pprint v0.3.6 (/Users/mkbabb/Programming/pprint/rust)` was not used in the crate graph
warning: patch `pprint_derive v0.2.2 (/Users/mkbabb/Programming/pprint/rust/derive)` was not used in the crate graph
help: Check that the patched package version and available features are compatible
      with the dependency requirements. If the patch has a different version from
      what is locked in the Cargo.lock file, run `cargo update` to use the new
      version. This may also occur with an optional dependency that is not enabled.
    Finished `release` profile [optimized + debuginfo] target(s) in 0.08s

.section __TEXT,__text,regular,pure_instructions
	.p2align	2
<alloc::raw_vec::RawVecInner<_>>::reserve::do_reserve_and_handle::<alloc::alloc::Global>:
Lfunc_begin0:
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
	adds x8, x2, x1
	b.hs LBB0_3
	mov x19, x0
	ldp x1, x2, [x0]
	lsl x9, x1, #1
	cmp x8, x9
	csel x8, x8, x9, hi
	mov w9, #8
	cmp x8, #8
	csel x20, x8, x9, hi
	add x0, sp, #8
	mov x3, x20
	bl <alloc::raw_vec::RawVecInner>::finish_grow
	ldr x8, [sp, #8]
	cmp x8, #1
	b.eq LBB0_4
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
LBB0_3:
	.cfi_restore_state
	mov x0, #0
	bl alloc::raw_vec::handle_error
LBB0_4:
	ldp x0, x1, [sp, #16]
	bl alloc::raw_vec::handle_error
Lfunc_end0:
	.cfi_endproc

	.p2align	2
parse_that_regex::read_hex_unit:
Lfunc_begin1:
	.cfi_startproc
	stp x29, x30, [sp, #-16]!
	.cfi_def_cfa_offset 16
	mov x29, sp
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_remember_state
	mov x8, x1
	add x1, x3, #4
	cmp x1, x2
	b.hi LBB1_8
	cmn x3, #5
	b.hi LBB1_31
	add x11, x8, x3
	ldrb w15, [x11]
	sub w8, w15, #48
	and w9, w15, #0xffffffdf
	sub w9, w9, #65
	cmp w8, #10
	and w9, w9, #0xff
	ccmp w9, #6, #0, hs
	b.hs LBB1_8
	ldrb w14, [x11, #1]
	sub w9, w14, #48
	and w10, w14, #0xffffffdf
	sub w10, w10, #65
	cmp w9, #10
	and w10, w10, #0xff
	ccmp w10, #5, #0, hs
	b.hi LBB1_8
	ldrb w13, [x11, #2]
	sub w10, w13, #48
	cmp w10, #10
	b.lo LBB1_6
	and w12, w13, #0xffffffdf
	sub w12, w12, #65
	and w12, w12, #0xff
	cmp w12, #5
	b.hi LBB1_8
LBB1_6:
	ldrb w12, [x11, #3]
	sub w11, w12, #48
	cmp w11, #10
	b.lo LBB1_10
	and w16, w12, #0xffffffdf
	sub w16, w16, #65
	and w16, w16, #0xff
	cmp w16, #6
	b.lo LBB1_10
LBB1_8:
	str x3, [x0]
	mov w8, #4
LBB1_9:
	strb w8, [x0, #8]
	.cfi_def_cfa wsp, 16
	ldp x29, x30, [sp], #16
	.cfi_def_cfa_offset 0
	.cfi_restore w30
	.cfi_restore w29
	ret
LBB1_10:
	.cfi_restore_state
	and w16, w8, #0xff
	cmp w16, #10
	b.lo LBB1_15
	sub w8, w15, #97
	cmp w8, #6
	b.hs LBB1_13
	sub w8, w15, #87
	b LBB1_15
LBB1_13:
	sub w8, w15, #65
	cmp w8, #6
	b.hs LBB1_32
	sub w8, w15, #55
LBB1_15:
	and w15, w9, #0xff
	cmp w15, #10
	b.lo LBB1_20
	sub w9, w14, #97
	cmp w9, #6
	b.hs LBB1_18
	sub w9, w14, #87
	b LBB1_20
LBB1_18:
	sub w9, w14, #65
	cmp w9, #5
	b.hi LBB1_32
	sub w9, w14, #55
LBB1_20:
	and w14, w10, #0xff
	cmp w14, #10
	b.lo LBB1_25
	sub w10, w13, #97
	cmp w10, #6
	b.hs LBB1_23
	sub w10, w13, #87
	b LBB1_25
LBB1_23:
	sub w10, w13, #65
	cmp w10, #5
	b.hi LBB1_32
	sub w10, w13, #55
LBB1_25:
	and w13, w11, #0xff
	cmp w13, #10
	b.lo LBB1_30
	sub w11, w12, #97
	cmp w11, #6
	b.hs LBB1_28
	sub w11, w12, #87
	b LBB1_30
LBB1_28:
	sub w11, w12, #65
	cmp w11, #5
	b.hi LBB1_32
	sub w11, w12, #55
LBB1_30:
	and w8, w8, #0xff
	lsl w8, w8, #8
	add w8, w8, w9, uxtb #4
	add w8, w8, w10, uxtb
	lsl w8, w8, #4
	add w8, w8, w11, uxtb
	strh w8, [x0]
	mov w8, #6
	b LBB1_9
LBB1_31:
Lloh0:
	adrp x8, l_anon.028aad4ed9a93f18e57e62edb80815c2.1@PAGE
Lloh1:
	add x8, x8, l_anon.028aad4ed9a93f18e57e62edb80815c2.1@PAGEOFF
	mov x0, x3
	mov x3, x8
	bl core::slice::index::slice_index_fail
LBB1_32:
Lloh2:
	adrp x0, l_anon.028aad4ed9a93f18e57e62edb80815c2.4@PAGE
Lloh3:
	add x0, x0, l_anon.028aad4ed9a93f18e57e62edb80815c2.4@PAGEOFF
Lloh4:
	adrp x2, l_anon.028aad4ed9a93f18e57e62edb80815c2.5@PAGE
Lloh5:
	add x2, x2, l_anon.028aad4ed9a93f18e57e62edb80815c2.5@PAGEOFF
	mov w1, #139
	bl core::panicking::panic_fmt
	.loh AdrpAdd	Lloh0, Lloh1
	.loh AdrpAdd	Lloh4, Lloh5
	.loh AdrpAdd	Lloh2, Lloh3
