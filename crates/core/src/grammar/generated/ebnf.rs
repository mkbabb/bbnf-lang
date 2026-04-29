//! AUTO-GENERATED from `[workspace.metadata.bbnf.grammars]` — do not edit manually.
//! Regenerate: cargo xtask regen --grammar ebnf

#![allow(
    dead_code,
    unused_variables,
    unused_mut,
    unused_parens,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    clippy::all
)]

use crate::runtime::tape::*;
use crate::runtime::{Parsed, ParseErr, Root};
use ::parse_that::*;

pub struct EbnfParser;
mod __ebnfparser_emit_impl {
    #![allow(
        dead_code,
        unused_variables,
        unused_mut,
        unused_parens,
        unused_assignments,
        non_camel_case_types,
        non_snake_case,
        non_upper_case_globals,
        clippy::all,
    )]
    use super::*;
    use ::parse_that::*;
    pub const GRAMMAR_EbnfParser: [&'static str; 1usize] = [
        include_str!(
            concat!(env!("CARGO_MANIFEST_DIR"), "/../../grammar/ebnf/ebnf.bbnf")
        ),
    ];
    static __GRAMMAR_PROFILE_ALPHABET: [u8; 89usize] = [
        8, 9, 10, 12, 13, 32, 34, 39, 40, 41, 42, 43, 44, 45, 46, 48, 49, 50, 51, 52, 53,
        54, 55, 56, 57, 59, 60, 61, 62, 63, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
        76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 95, 97,
        98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113,
        114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125,
    ];
    /// Per-grammar codegen fingerprint — consolidated static
    /// profile emitted by Tranche AV Phase 1. Every downstream
    /// consumer (tape capacity, scanner dispatch) reads the
    /// matching field.
    pub const GRAMMAR_PROFILE: crate::runtime::tape::GrammarProfile = crate::runtime::tape::GrammarProfile {
        compounds_per_input_byte: 0.5f32,
        leaves_per_input_byte: 0f32,
        parallel_break_even_bytes: 1048576u32,
        structural_alphabet: &__GRAMMAR_PROFILE_ALPHABET,
        structural_digraphs: &[],
        structural_digraph_mask: [0, 0, 0, 0],
        structural_quote_classes: &[],
    };
    /// AW-III.W6.2 — PHF keyword table.
    ///
    /// Mined literal-led Alt branches, sorted lexicographically.
    /// Binary search dispatches in O(log N) compares; LLVM lowers
    /// the fixed-size table to a balanced compare tree.
    static __PHF_EbnfParser_0_KW: [&[u8]; 52usize] = [
        b"A",
        b"B",
        b"C",
        b"D",
        b"E",
        b"F",
        b"G",
        b"H",
        b"I",
        b"J",
        b"K",
        b"L",
        b"M",
        b"N",
        b"O",
        b"P",
        b"Q",
        b"R",
        b"S",
        b"T",
        b"U",
        b"V",
        b"W",
        b"X",
        b"Y",
        b"Z",
        b"a",
        b"b",
        b"c",
        b"d",
        b"e",
        b"f",
        b"g",
        b"h",
        b"i",
        b"j",
        b"k",
        b"l",
        b"m",
        b"n",
        b"o",
        b"p",
        b"q",
        b"r",
        b"s",
        b"t",
        b"u",
        b"v",
        b"w",
        b"x",
        b"y",
        b"z",
    ];
    /// Per-entry branch discriminant — parallel to [`#kw_ident`].
    /// Entry `i`'s keyword bytes at `#kw_ident[i]` route to the
    /// branch with discriminant `#idx_ident[i]`.
    static __PHF_EbnfParser_0_IDX: [u8; 52usize] = [
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
        23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42,
        43, 44, 45, 46, 47, 48, 49, 50, 51,
    ];
    /// AW-III.W6.2 — dispatch the mined keyword table for rule
    /// `#rule_id`.
    ///
    /// Returns `Some(branch_idx)` when `bytes` matches a mined
    /// keyword, `None` otherwise. Called from the walker's
    /// AltLinear / ClassifyByte arm to short-circuit the branch
    /// scan to a single binary search.
    #[allow(dead_code)]
    #[inline]
    fn __phf_EbnfParser_dispatch_0(bytes: &[u8]) -> ::core::option::Option<u8> {
        match __PHF_EbnfParser_0_KW.binary_search(&bytes) {
            ::core::result::Result::Ok(idx) => {
                ::core::option::Option::Some(__PHF_EbnfParser_0_IDX[idx])
            }
            ::core::result::Result::Err(_) => ::core::option::Option::None,
        }
    }
    /// AW-III.W6.2 — PHF keyword table.
    ///
    /// Mined literal-led Alt branches, sorted lexicographically.
    /// Binary search dispatches in O(log N) compares; LLVM lowers
    /// the fixed-size table to a balanced compare tree.
    static __PHF_EbnfParser_1_KW: [&[u8]; 10usize] = [
        b"0",
        b"1",
        b"2",
        b"3",
        b"4",
        b"5",
        b"6",
        b"7",
        b"8",
        b"9",
    ];
    /// Per-entry branch discriminant — parallel to [`#kw_ident`].
    /// Entry `i`'s keyword bytes at `#kw_ident[i]` route to the
    /// branch with discriminant `#idx_ident[i]`.
    static __PHF_EbnfParser_1_IDX: [u8; 10usize] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
    /// AW-III.W6.2 — dispatch the mined keyword table for rule
    /// `#rule_id`.
    ///
    /// Returns `Some(branch_idx)` when `bytes` matches a mined
    /// keyword, `None` otherwise. Called from the walker's
    /// AltLinear / ClassifyByte arm to short-circuit the branch
    /// scan to a single binary search.
    #[allow(dead_code)]
    #[inline]
    fn __phf_EbnfParser_dispatch_1(bytes: &[u8]) -> ::core::option::Option<u8> {
        match __PHF_EbnfParser_1_KW.binary_search(&bytes) {
            ::core::result::Result::Ok(idx) => {
                ::core::option::Option::Some(__PHF_EbnfParser_1_IDX[idx])
            }
            ::core::result::Result::Err(_) => ::core::option::Option::None,
        }
    }
    /// AW-III.W6.2 — PHF keyword table.
    ///
    /// Mined literal-led Alt branches, sorted lexicographically.
    /// Binary search dispatches in O(log N) compares; LLVM lowers
    /// the fixed-size table to a balanced compare tree.
    static __PHF_EbnfParser_2_KW: [&[u8]; 25usize] = [
        b"\x08",
        b"\t",
        b"\n",
        b"\x0C",
        b"\r",
        b"\"",
        b"'",
        b"(",
        b")",
        b"*",
        b"+",
        b",",
        b"-",
        b".",
        b";",
        b"<",
        b"=",
        b">",
        b"?",
        b"[",
        b"\\",
        b"]",
        b"{",
        b"|",
        b"}",
    ];
    /// Per-entry branch discriminant — parallel to [`#kw_ident`].
    /// Entry `i`'s keyword bytes at `#kw_ident[i]` route to the
    /// branch with discriminant `#idx_ident[i]`.
    static __PHF_EbnfParser_2_IDX: [u8; 25usize] = [
        23, 20, 19, 22, 21, 9, 8, 4, 5, 17, 16, 13, 15, 12, 14, 6, 10, 7, 18, 0, 24, 1,
        2, 11, 3,
    ];
    /// AW-III.W6.2 — dispatch the mined keyword table for rule
    /// `#rule_id`.
    ///
    /// Returns `Some(branch_idx)` when `bytes` matches a mined
    /// keyword, `None` otherwise. Called from the walker's
    /// AltLinear / ClassifyByte arm to short-circuit the branch
    /// scan to a single binary search.
    #[allow(dead_code)]
    #[inline]
    fn __phf_EbnfParser_dispatch_2(bytes: &[u8]) -> ::core::option::Option<u8> {
        match __PHF_EbnfParser_2_KW.binary_search(&bytes) {
            ::core::result::Result::Ok(idx) => {
                ::core::option::Option::Some(__PHF_EbnfParser_2_IDX[idx])
            }
            ::core::result::Result::Err(_) => ::core::option::Option::None,
        }
    }
    /// AW-III.W6.2 — PHF keyword table.
    ///
    /// Mined literal-led Alt branches, sorted lexicographically.
    /// Binary search dispatches in O(log N) compares; LLVM lowers
    /// the fixed-size table to a balanced compare tree.
    static __PHF_EbnfParser_9_KW: [&[u8]; 3usize] = [b"(", b"[", b"{"];
    /// Per-entry branch discriminant — parallel to [`#kw_ident`].
    /// Entry `i`'s keyword bytes at `#kw_ident[i]` route to the
    /// branch with discriminant `#idx_ident[i]`.
    static __PHF_EbnfParser_9_IDX: [u8; 3usize] = [0, 1, 2];
    /// AW-III.W6.2 — dispatch the mined keyword table for rule
    /// `#rule_id`.
    ///
    /// Returns `Some(branch_idx)` when `bytes` matches a mined
    /// keyword, `None` otherwise. Called from the walker's
    /// AltLinear / ClassifyByte arm to short-circuit the branch
    /// scan to a single binary search.
    #[allow(dead_code)]
    #[inline]
    fn __phf_EbnfParser_dispatch_9(bytes: &[u8]) -> ::core::option::Option<u8> {
        match __PHF_EbnfParser_9_KW.binary_search(&bytes) {
            ::core::result::Result::Ok(idx) => {
                ::core::option::Option::Some(__PHF_EbnfParser_9_IDX[idx])
            }
            ::core::result::Result::Err(_) => ::core::option::Option::None,
        }
    }
    /// AW-III.W6.5 — aggregate dense Pratt precedence LUT.
    ///
    /// Union of every Pratt rule's packed LUT (last-write-wins
    /// per byte). Consulted by the walker cold-path's
    /// `ShuntingYard` arm until W0b retires the walker. See
    /// `bbnf::backend::rust::emitter::precedence` for the bit
    /// layout.
    pub const PRECEDENCE_LUT: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
    ];
    /// AW-III.W6.5 — aggregate sparse Pratt metadata slice.
    ///
    /// Flat union of every rule's mined operator entries.
    /// Consulted by the walker cold-path until W0b retires it.
    pub const PRECEDENCE_ENTRIES: &[crate::runtime::tape::DtaPrecedenceEntry] = &[];
    /// AW-III.W6.5 — total mined operator count for this
    /// grammar. Non-zero iff the lift admitted ≥ 1 chain OR the
    /// shape classifier admitted ≥ 1 single-rung Pratt rule.
    pub const PRECEDENCE_OPERATOR_COUNT: usize = 0usize;
    static __DTA_REGEX_124: &str = "[ \\t\\n\\r\\f]*";
    #[inline]
    #[cold]
    fn __regex_scan_EbnfParser(
        pattern: &str,
        input: &[u8],
        pos: usize,
    ) -> ::core::option::Option<u32> {
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_124.as_ptr())
            || pattern == __DTA_REGEX_124
        {
            return '__dfa: {
                let mut __dfa_state: u32 = 0;
                let mut __dfa_p: usize = pos;
                let mut __dfa_last_match: ::core::option::Option<u32> = ::core::option::Option::Some(
                    pos as u32,
                );
                loop {
                    let b = match input.get(__dfa_p) {
                        ::core::option::Option::Some(&b) => b,
                        ::core::option::Option::None => break,
                    };
                    match __dfa_state {
                        0 => {
                            match b {
                                9 | 10 | 12 | 13 | 32 => __dfa_state = 0,
                                _ => break,
                            }
                        }
                        _ => unsafe { ::core::hint::unreachable_unchecked() }
                    }
                    __dfa_p += 1;
                    match __dfa_state {
                        0 => {
                            __dfa_last_match = ::core::option::Option::Some(
                                __dfa_p as u32,
                            );
                        }
                        _ => {}
                    }
                }
                break '__dfa __dfa_last_match.map(|end| end - pos as u32);
            };
        }
        ::core::option::Option::None
    }
    /// AW-V.W3.2 — per-grammar shape-dispatch support.
    ///
    /// Inlined by every `parse_<shape>_<grammar>_<rule>` emitted
    /// sibling; carries the SIMD whitespace bitmap cache + the
    /// quoted-string scanner primitive. The module is private to
    /// the generated code — downstream consumers route through the
    /// top-level `parse_<grammar>_<root>` which inlines every
    /// helper under workspace LTO.
    #[allow(dead_code, non_snake_case)]
    pub(crate) mod __shape_support_EbnfParser {
        /// Per-parse SIMD scratch — 64-byte whitespace-bitmap
        /// cache mirroring `json-prototype::simd::ScanState`.
        ///
        /// AY.W4.3 — for grammars whose `structural_alphabet` is
        /// non-empty, ScanState additionally carries a lazy
        /// `OnceCell<StructuralIndex>` consumed by CTNS-style
        /// probes. Lazy-init keeps the O(N) scan cost amortised
        /// rather than paid eagerly at parse entry — see
        /// `AYW1-twitter-regression-diag` for the eager-init
        /// regression that motivates the OnceCell discipline.
        #[derive(Debug, Default)]
        pub struct ScanState {
            pub(crate) nospace_bits: u64,
            pub(crate) nospace_start: isize,
            /// AY.W4.3 — lazy structural-byte index. Populated on
            /// first consumer query via `ensure_structural_index`;
            /// `OnceCell` discipline keeps the O(N) scan cost
            /// amortised across the parse rather than paid eagerly
            /// at parse-entry (AY.W1-fix demonstrated eager scans
            /// regress JSON twitter -64%).
            pub(crate) structural_index: ::core::cell::OnceCell<
                crate::runtime::tape::StructuralIndex,
            >,
        }
        impl ScanState {
            #[inline]
            pub fn new() -> Self {
                Self {
                    nospace_bits: 0,
                    nospace_start: -1,
                    structural_index: ::core::cell::OnceCell::new(),
                }
            }
        }
        /// AY.W4.3 — lazy-init the per-parse structural index
        /// against the grammar's mined `structural_alphabet`.
        /// Idempotent; consumers may call freely.
        #[inline]
        pub(crate) fn ensure_structural_index<'a>(
            state: &'a mut ScanState,
            input: &[u8],
        ) -> &'a crate::runtime::tape::StructuralIndex {
            state
                .structural_index
                .get_or_init(|| {
                    crate::runtime::tape::scan_structural(
                        input,
                        super::GRAMMAR_PROFILE.structural_alphabet,
                    )
                })
        }
        /// Skip JSON whitespace at `*p`, returning the first
        /// non-whitespace byte (or `None` on EOF). Hot-path fast-
        /// exit when the next byte is non-whitespace.
        #[inline(always)]
        pub fn skip_space(
            input: &[u8],
            p: &mut usize,
            state: &mut ScanState,
        ) -> Option<u8> {
            match input.get(*p) {
                Some(&b) if b != b' ' && b != b'\t' && b != b'\n' && b != b'\r' => {
                    Some(b)
                }
                None => None,
                _ => {
                    skip_space_slow(input, p, state);
                    input.get(*p).copied()
                }
            }
        }
        #[inline(always)]
        pub(crate) fn skip_space_slow(
            input: &[u8],
            p: &mut usize,
            state: &mut ScanState,
        ) {
            loop {
                let cache_base = state.nospace_start;
                if cache_base >= 0 && (*p as isize) >= cache_base {
                    let rel = *p - cache_base as usize;
                    if rel < 64 {
                        let masked = state.nospace_bits & (!0u64 << rel);
                        if masked != 0 {
                            let bit = masked.trailing_zeros() as usize;
                            *p = cache_base as usize + bit;
                            return;
                        }
                        *p = cache_base as usize + 64;
                    }
                }
                if *p + 64 > input.len() {
                    while let Some(&b) = input.get(*p) {
                        if b != b' ' && b != b'\t' && b != b'\n' && b != b'\r' {
                            return;
                        }
                        *p += 1;
                    }
                    return;
                }
                let stripe = unsafe {
                    ::core::slice::from_raw_parts(input.as_ptr().add(*p), 64)
                };
                let mask = nospace_bitmap_64(stripe);
                state.nospace_bits = mask;
                state.nospace_start = *p as isize;
                if mask != 0 {
                    let bit = mask.trailing_zeros() as usize;
                    *p += bit;
                    return;
                }
                *p += 64;
            }
        }
        /// Compute the 64-bit "non-whitespace" bitmap for a 64-byte
        /// stripe. Bit `i` is `1` iff `stripe[i]` is NOT in
        /// `{b' ', b'\t', b'\n', b'\r'}`.
        #[inline(always)]
        pub(crate) fn nospace_bitmap_64(stripe: &[u8]) -> u64 {
            #[cfg(target_arch = "aarch64")]
            unsafe {
                return nospace_bitmap_64_neon(stripe);
            }
            #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
            unsafe {
                return nospace_bitmap_64_avx2(stripe);
            }
            #[allow(unreachable_code)] nospace_bitmap_64_scalar(stripe)
        }
        #[cfg(target_arch = "aarch64")]
        #[inline(always)]
        unsafe fn nospace_bitmap_64_neon(stripe: &[u8]) -> u64 {
            use core::arch::aarch64::*;
            unsafe {
                let ptr = stripe.as_ptr();
                let space = vdupq_n_u8(b' ');
                let tab = vdupq_n_u8(b'\t');
                let nl = vdupq_n_u8(b'\n');
                let cr = vdupq_n_u8(b'\r');
                let bits_lo: [u8; 16] = [
                    1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128,
                ];
                let bit_vec = vld1q_u8(bits_lo.as_ptr());
                let m0 = chunk_ns_mask16(ptr, 0, space, tab, nl, cr, bit_vec);
                let m1 = chunk_ns_mask16(ptr, 16, space, tab, nl, cr, bit_vec);
                let m2 = chunk_ns_mask16(ptr, 32, space, tab, nl, cr, bit_vec);
                let m3 = chunk_ns_mask16(ptr, 48, space, tab, nl, cr, bit_vec);
                (m0 as u64) | ((m1 as u64) << 16) | ((m2 as u64) << 32)
                    | ((m3 as u64) << 48)
            }
        }
        #[cfg(target_arch = "aarch64")]
        #[inline(always)]
        unsafe fn chunk_ns_mask16(
            ptr: *const u8,
            off: usize,
            space: core::arch::aarch64::uint8x16_t,
            tab: core::arch::aarch64::uint8x16_t,
            nl: core::arch::aarch64::uint8x16_t,
            cr: core::arch::aarch64::uint8x16_t,
            bit_vec: core::arch::aarch64::uint8x16_t,
        ) -> u16 {
            use core::arch::aarch64::*;
            unsafe {
                let chunk = vld1q_u8(ptr.add(off));
                let ws = vorrq_u8(
                    vorrq_u8(vceqq_u8(chunk, space), vceqq_u8(chunk, tab)),
                    vorrq_u8(vceqq_u8(chunk, nl), vceqq_u8(chunk, cr)),
                );
                let ns = vmvnq_u8(ws);
                let weighted = vandq_u8(ns, bit_vec);
                let low = vaddv_u8(vget_low_u8(weighted)) as u16;
                let high = vaddv_u8(vget_high_u8(weighted)) as u16;
                low | (high << 8)
            }
        }
        #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
        #[inline(always)]
        unsafe fn nospace_bitmap_64_avx2(stripe: &[u8]) -> u64 {
            use core::arch::x86_64::*;
            unsafe {
                let ptr = stripe.as_ptr();
                let space = _mm256_set1_epi8(b' ' as i8);
                let tab = _mm256_set1_epi8(b'\t' as i8);
                let nl = _mm256_set1_epi8(b'\n' as i8);
                let cr = _mm256_set1_epi8(b'\r' as i8);
                let mut out = 0u64;
                for i in 0..2 {
                    let v = _mm256_loadu_si256(ptr.add(i * 32) as *const __m256i);
                    let ws = _mm256_or_si256(
                        _mm256_or_si256(
                            _mm256_cmpeq_epi8(v, space),
                            _mm256_cmpeq_epi8(v, tab),
                        ),
                        _mm256_or_si256(
                            _mm256_cmpeq_epi8(v, nl),
                            _mm256_cmpeq_epi8(v, cr),
                        ),
                    );
                    let ws_mask = _mm256_movemask_epi8(ws) as u32;
                    let ns_mask = !ws_mask as u64;
                    out |= (ns_mask & 0xFFFF_FFFF) << (i * 32);
                }
                out
            }
        }
        #[inline(always)]
        pub(crate) fn nospace_bitmap_64_scalar(stripe: &[u8]) -> u64 {
            let mut out = 0u64;
            for (i, &b) in stripe.iter().enumerate() {
                if b != b' ' && b != b'\t' && b != b'\n' && b != b'\r' {
                    out |= 1u64 << i;
                }
            }
            out
        }
        /// Find the first `b'"'` or `b'\\'` byte in `bytes`.
        /// Mirrors `json-prototype::simd::first_quote_or_backslash`.
        #[inline(always)]
        pub fn first_quote_or_backslash(bytes: &[u8]) -> Option<(usize, u8)> {
            #[cfg(target_arch = "aarch64")]
            unsafe {
                return first_quote_or_backslash_neon(bytes);
            }
            #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
            unsafe {
                return first_quote_or_backslash_avx2(bytes);
            }
            #[allow(unreachable_code)] first_quote_or_backslash_scalar(bytes)
        }
        #[cfg(target_arch = "aarch64")]
        #[inline(always)]
        unsafe fn first_quote_or_backslash_neon(bytes: &[u8]) -> Option<(usize, u8)> {
            use core::arch::aarch64::*;
            unsafe {
                let quote = vdupq_n_u8(b'"');
                let backslash = vdupq_n_u8(b'\\');
                let ptr = bytes.as_ptr();
                let len = bytes.len();
                let mut i = 0usize;
                while i + 16 <= len {
                    let v = vld1q_u8(ptr.add(i));
                    let hit = vorrq_u8(vceqq_u8(v, quote), vceqq_u8(v, backslash));
                    let packed = vshrn_n_u16::<4>(vreinterpretq_u16_u8(hit));
                    let bits = vget_lane_u64::<0>(vreinterpret_u64_u8(packed));
                    if bits != 0 {
                        let off = (bits.trailing_zeros() >> 2) as usize;
                        let byte = *ptr.add(i + off);
                        return Some((i + off, byte));
                    }
                    i += 16;
                }
                while i < len {
                    let b = *ptr.add(i);
                    if b == b'"' || b == b'\\' {
                        return Some((i, b));
                    }
                    i += 1;
                }
                None
            }
        }
        #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
        #[inline(always)]
        unsafe fn first_quote_or_backslash_avx2(bytes: &[u8]) -> Option<(usize, u8)> {
            use core::arch::x86_64::*;
            unsafe {
                let quote = _mm256_set1_epi8(b'"' as i8);
                let backslash = _mm256_set1_epi8(b'\\' as i8);
                let ptr = bytes.as_ptr();
                let len = bytes.len();
                let mut i = 0usize;
                while i + 32 <= len {
                    let v = _mm256_loadu_si256(ptr.add(i) as *const __m256i);
                    let hit = _mm256_or_si256(
                        _mm256_cmpeq_epi8(v, quote),
                        _mm256_cmpeq_epi8(v, backslash),
                    );
                    let mask = _mm256_movemask_epi8(hit) as u32;
                    if mask != 0 {
                        let off = mask.trailing_zeros() as usize;
                        return Some((i + off, *ptr.add(i + off)));
                    }
                    i += 32;
                }
                while i < len {
                    let b = *ptr.add(i);
                    if b == b'"' || b == b'\\' {
                        return Some((i, b));
                    }
                    i += 1;
                }
                None
            }
        }
        #[inline(always)]
        pub(crate) fn first_quote_or_backslash_scalar(
            bytes: &[u8],
        ) -> Option<(usize, u8)> {
            for (i, &b) in bytes.iter().enumerate() {
                if b == b'"' || b == b'\\' {
                    return Some((i, b));
                }
            }
            None
        }
        /// Map a byte into one of six arms: object `{` → 0,
        /// array `[` → 1, string `"` → 2, digit/`-` (number) → 3,
        /// keyword-led `t` / `f` / `n` → 4, else → 5.
        ///
        /// The emitter's dispatcher inlines this to compile-time
        /// byte matches; kept here as a reference helper for tests.
        #[inline(always)]
        pub(crate) fn shape_byte_arm(b: u8) -> u8 {
            match b {
                b'{' => 0,
                b'[' => 1,
                b'"' => 2,
                b'-' | b'0'..=b'9' => 3,
                b't' | b'f' | b'n' => 4,
                _ => 5,
            }
        }
        /// Expect an exact keyword sequence at `*p` and advance
        /// past it on match.
        #[inline(always)]
        pub fn expect_keyword(input: &[u8], p: &mut usize, word: &[u8]) -> bool {
            let at = *p;
            let end = at + word.len();
            if input.len() < end || &input[at..end] != word {
                return false;
            }
            *p = end;
            true
        }
    }
    /// AZ-I.W2.RB — per-grammar AltDispatch-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / Sheets / CSS L4 per the resolver's
    /// `SubstrateBinding`).
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_variables,
        unused_mut,
        unused_assignments,
        unreachable_code
    )]
    pub fn parse_altdispatch_EbnfParser_letter<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::tape::DtaError> {
        use crate::runtime::builder::StructBuilder;
        let first = __shape_support_EbnfParser::skip_space(input, p, state)
            .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 0u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("letter"),
            kind: ::bbnf_ir::registry::LayoutKind::TaggedEnum,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __dispatch_checkpoint = builder.checkpoint();
        let __handle = builder.begin_compound(&__layout);
        let __dispatch_result: ::core::result::Result<
            (),
            crate::runtime::tape::DtaError,
        > = (|| {
            'try_branches: loop {
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [65u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(0u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [66u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(1u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [67u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(2u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [68u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(3u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [69u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(4u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [70u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(5u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [71u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(6u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [72u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(7u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [73u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(8u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [74u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(9u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [75u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(10u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [76u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(11u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [77u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(12u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [78u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(13u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [79u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(14u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [80u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(15u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [81u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(16u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [82u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(17u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [83u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(18u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [84u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(19u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [85u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(20u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [86u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(21u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [87u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(22u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [88u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(23u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [89u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(24u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [90u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(25u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [97u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(26u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [98u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(27u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [99u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(28u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [100u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(29u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [101u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(30u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [102u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(31u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [103u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(32u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [104u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(33u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [105u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(34u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [106u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(35u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [107u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(36u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [108u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(37u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [109u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(38u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [110u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(39u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [111u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(40u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [112u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(41u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [113u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(42u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [114u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(43u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [115u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(44u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [116u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(45u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [117u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(46u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [118u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(47u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [119u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(48u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [120u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(49u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [121u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(50u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [122u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(51u32);
                        break 'try_branches;
                    }
                }
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            ::core::result::Result::Ok(())
        })();
        match __dispatch_result {
            ::core::result::Result::Ok(()) => {
                builder.end_compound(__handle);
                Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__dispatch_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RD — struct-direct Keyword-shape parse fn
    /// (Alt of literal-led, Ref-led, or Seq-led branches).
    ///
    /// Literal branches push leaves through
    /// `builder.push_leaf_with_bool` (TypeDesc::Bool) or
    /// `builder.push_leaf_with_unit` (TypeDesc::U8 /
    /// untyped). Ref branches delegate to the target shape
    /// fn so the target's records bubble up unchanged.
    /// Returns `TapeOffset::NONE` for compositional
    /// uniformity.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_keyword_EbnfParser_digit<'p>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        use crate::runtime::builder::StructBuilder as _;
        let _ = state;
        match first_byte {
            48u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [48u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(
                        crate::runtime::tape::TapeOffset::NONE,
                    );
                }
                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            49u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [49u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(
                        crate::runtime::tape::TapeOffset::NONE,
                    );
                }
                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            50u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [50u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(
                        crate::runtime::tape::TapeOffset::NONE,
                    );
                }
                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            51u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [51u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(
                        crate::runtime::tape::TapeOffset::NONE,
                    );
                }
                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            52u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [52u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(
                        crate::runtime::tape::TapeOffset::NONE,
                    );
                }
                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            53u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [53u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(
                        crate::runtime::tape::TapeOffset::NONE,
                    );
                }
                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            54u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [54u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(
                        crate::runtime::tape::TapeOffset::NONE,
                    );
                }
                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            55u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [55u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(
                        crate::runtime::tape::TapeOffset::NONE,
                    );
                }
                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            56u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [56u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(
                        crate::runtime::tape::TapeOffset::NONE,
                    );
                }
                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            57u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [57u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(
                        crate::runtime::tape::TapeOffset::NONE,
                    );
                }
                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            _ => {
                ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                })
            }
        }
    }
    /// AZ-I.W2.RB — per-grammar AltDispatch-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / Sheets / CSS L4 per the resolver's
    /// `SubstrateBinding`).
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_variables,
        unused_mut,
        unused_assignments,
        unreachable_code
    )]
    pub fn parse_altdispatch_EbnfParser_symbol<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::tape::DtaError> {
        use crate::runtime::builder::StructBuilder;
        let first = __shape_support_EbnfParser::skip_space(input, p, state)
            .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 2u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("symbol"),
            kind: ::bbnf_ir::registry::LayoutKind::TaggedEnum,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __dispatch_checkpoint = builder.checkpoint();
        let __handle = builder.begin_compound(&__layout);
        let __dispatch_result: ::core::result::Result<
            (),
            crate::runtime::tape::DtaError,
        > = (|| {
            'try_branches: loop {
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [91u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(0u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [93u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(1u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [123u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(2u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [125u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(3u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [40u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(4u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [41u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(5u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [60u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(6u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [62u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(7u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [39u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(8u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [34u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(9u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [61u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(10u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [124u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(11u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [46u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(12u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [44u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(13u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [59u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(14u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [45u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(15u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [43u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(16u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [42u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(17u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [63u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(18u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [10u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(19u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [9u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(20u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [13u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(21u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [12u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(22u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [8u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(23u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [92u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(24u32);
                        break 'try_branches;
                    }
                }
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            ::core::result::Result::Ok(())
        })();
        match __dispatch_result {
            ::core::result::Result::Ok(()) => {
                builder.end_compound(__handle);
                Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__dispatch_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RF — per-grammar Flat-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / CSS L4 / Sheets per the
    /// resolver's `SubstrateBinding`).
    ///
    /// Walker-tape compound emission is replaced by typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns `TapeOffset::NONE` for compositional uniformity
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_EbnfParser_identifier<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __identifier_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 3u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("identifier"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __identifier_handle = <crate::runtime::ebnf::EbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__identifier_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::tape::DtaError> = (||
        {
            {
                let _ = ({
                    let _ = __shape_support_EbnfParser::skip_space(input, p, state);
                    parse_altdispatch_EbnfParser_letter(input, p, state, builder)
                })?;
            }
            {
                {
                    let mut __iter_count: u32 = 0;
                    loop {
                        if __iter_count >= 4294967295u32 {
                            break;
                        }
                        let __iter_save_p = *p;
                        if input.get(*p).is_none() {
                            break;
                        }
                        let __iter_builder_checkpoint = builder.checkpoint();
                        let __iter_result: ::core::result::Result<
                            (),
                            crate::runtime::tape::DtaError,
                        > = (|| {
                            'try_branches: loop {
                                {
                                    let __alt_save_p = *p;
                                    let __alt_builder_checkpoint = builder.checkpoint();
                                    let __alt_result: ::core::result::Result<
                                        (),
                                        crate::runtime::tape::DtaError,
                                    > = (|| {
                                        let at = *p;
                                        let end = at + 1usize;
                                        if input.len() < end || input[at..end] != [95u8] {
                                            return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                                offset: at as u32,
                                                failing_state: crate::runtime::tape::DtaStateId::NONE,
                                                failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                            });
                                        }
                                        *p = end;
                                        Ok(())
                                    })();
                                    match __alt_result {
                                        Ok(()) => {
                                            builder.commit(__alt_builder_checkpoint);
                                            break 'try_branches;
                                        }
                                        Err(_) => {
                                            *p = __alt_save_p;
                                            builder.rollback(__alt_builder_checkpoint);
                                        }
                                    }
                                }
                                {
                                    let __alt_save_p = *p;
                                    let __alt_builder_checkpoint = builder.checkpoint();
                                    let __alt_result: ::core::result::Result<
                                        (),
                                        crate::runtime::tape::DtaError,
                                    > = (|| {
                                        let _ = ({
                                            let __first = __shape_support_EbnfParser::skip_space(
                                                    input,
                                                    p,
                                                    state,
                                                )
                                                .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                                                    offset: *p as u32,
                                                })?;
                                            parse_keyword_EbnfParser_digit(
                                                input,
                                                p,
                                                __first,
                                                state,
                                                builder,
                                            )
                                        })?;
                                        Ok(())
                                    })();
                                    match __alt_result {
                                        Ok(()) => {
                                            builder.commit(__alt_builder_checkpoint);
                                            break 'try_branches;
                                        }
                                        Err(_) => {
                                            *p = __alt_save_p;
                                            builder.rollback(__alt_builder_checkpoint);
                                        }
                                    }
                                }
                                {
                                    let __alt_save_p = *p;
                                    let __alt_builder_checkpoint = builder.checkpoint();
                                    let __alt_result: ::core::result::Result<
                                        (),
                                        crate::runtime::tape::DtaError,
                                    > = (|| {
                                        let _ = ({
                                            let _ = __shape_support_EbnfParser::skip_space(
                                                input,
                                                p,
                                                state,
                                            );
                                            parse_altdispatch_EbnfParser_letter(
                                                input,
                                                p,
                                                state,
                                                builder,
                                            )
                                        })?;
                                        Ok(())
                                    })();
                                    match __alt_result {
                                        Ok(()) => {
                                            builder.commit(__alt_builder_checkpoint);
                                            break 'try_branches;
                                        }
                                        Err(_) => {
                                            *p = __alt_save_p;
                                            builder.rollback(__alt_builder_checkpoint);
                                        }
                                    }
                                }
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: *p as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            Ok(())
                        })();
                        match __iter_result {
                            Ok(()) => {
                                if *p == __iter_save_p {
                                    builder.rollback(__iter_builder_checkpoint);
                                    break;
                                }
                                builder.commit(__iter_builder_checkpoint);
                                __iter_count += 1;
                            }
                            Err(_) => {
                                *p = __iter_save_p;
                                builder.rollback(__iter_builder_checkpoint);
                                break;
                            }
                        }
                    }
                    if __iter_count < 0u32 {
                        return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                            offset: *p as u32,
                            failing_state: crate::runtime::tape::DtaStateId::NONE,
                            failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                        });
                    }
                }
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::ebnf::EbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __identifier_handle,
                );
                ::core::result::Result::Ok(crate::runtime::tape::TapeOffset::NONE)
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RB — per-grammar AltDispatch-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / Sheets / CSS L4 per the resolver's
    /// `SubstrateBinding`).
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_variables,
        unused_mut,
        unused_assignments,
        unreachable_code
    )]
    pub fn parse_altdispatch_EbnfParser_character<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::tape::DtaError> {
        use crate::runtime::builder::StructBuilder;
        let first = __shape_support_EbnfParser::skip_space(input, p, state)
            .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 4u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("character"),
            kind: ::bbnf_ir::registry::LayoutKind::TaggedEnum,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __dispatch_checkpoint = builder.checkpoint();
        let __handle = builder.begin_compound(&__layout);
        let __dispatch_result: ::core::result::Result<
            (),
            crate::runtime::tape::DtaError,
        > = (|| {
            'try_branches: loop {
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [95u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(0u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [32u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(1u32);
                        break 'try_branches;
                    }
                }
                {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match {
                        let __first = __shape_support_EbnfParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                                offset: *p as u32,
                            })?;
                        parse_keyword_EbnfParser_digit(input, p, __first, state, builder)
                    } {
                        Ok(_) => {
                            builder.push_branch_tag(2u32);
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match {
                        parse_altdispatch_EbnfParser_symbol(input, p, state, builder)
                    } {
                        Ok(_) => {
                            builder.push_branch_tag(3u32);
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match {
                        let _ = __shape_support_EbnfParser::skip_space(input, p, state);
                        parse_altdispatch_EbnfParser_letter(input, p, state, builder)
                    } {
                        Ok(_) => {
                            builder.push_branch_tag(4u32);
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            ::core::result::Result::Ok(())
        })();
        match __dispatch_result {
            ::core::result::Result::Ok(()) => {
                builder.end_compound(__handle);
                Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__dispatch_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RD — struct-direct Keyword-shape parse fn
    /// (Alt of literal-led, Ref-led, or Seq-led branches).
    ///
    /// Literal branches push leaves through
    /// `builder.push_leaf_with_bool` (TypeDesc::Bool) or
    /// `builder.push_leaf_with_unit` (TypeDesc::U8 /
    /// untyped). Ref branches delegate to the target shape
    /// fn so the target's records bubble up unchanged.
    /// Returns `TapeOffset::NONE` for compositional
    /// uniformity.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_keyword_EbnfParser_terminal<'p>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        use crate::runtime::builder::StructBuilder as _;
        let _ = state;
        match first_byte {
            34u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [34u8] {
                    let __seq_span_lo = *p;
                    let __seq_builder_checkpoint = builder.checkpoint();
                    let __seq_result: ::core::result::Result<
                        (),
                        crate::runtime::tape::DtaError,
                    > = (|| {
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [34u8] {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: at as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            *p = end;
                        }
                        {
                            let __minus_save_p = *p;
                            let __minus_builder_checkpoint = builder.checkpoint();
                            let __minus_excl: ::core::result::Result<
                                (),
                                crate::runtime::tape::DtaError,
                            > = (|| {
                                {
                                    let at = *p;
                                    let end = at + 1usize;
                                    if input.len() < end || input[at..end] != [34u8] {
                                        return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                            offset: at as u32,
                                            failing_state: crate::runtime::tape::DtaStateId::NONE,
                                            failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                        });
                                    }
                                    *p = end;
                                }
                                ::core::result::Result::Ok(())
                            })();
                            *p = __minus_save_p;
                            builder.rollback(__minus_builder_checkpoint);
                            if __minus_excl.is_ok() {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: *p as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            let _ = ({
                                parse_altdispatch_EbnfParser_character(
                                    input,
                                    p,
                                    state,
                                    builder,
                                )
                            })?;
                        }
                        {
                            let mut __iter_count: u32 = 0;
                            loop {
                                let __iter_save_p = *p;
                                if input.get(*p).is_none() {
                                    break;
                                }
                                let __iter_builder_checkpoint = builder.checkpoint();
                                let __iter_result: ::core::result::Result<
                                    (),
                                    crate::runtime::tape::DtaError,
                                > = (|| {
                                    {
                                        let __minus_save_p = *p;
                                        let __minus_builder_checkpoint = builder.checkpoint();
                                        let __minus_excl: ::core::result::Result<
                                            (),
                                            crate::runtime::tape::DtaError,
                                        > = (|| {
                                            {
                                                let at = *p;
                                                let end = at + 1usize;
                                                if input.len() < end || input[at..end] != [34u8] {
                                                    return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                                        offset: at as u32,
                                                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                                                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                                    });
                                                }
                                                *p = end;
                                            }
                                            ::core::result::Result::Ok(())
                                        })();
                                        *p = __minus_save_p;
                                        builder.rollback(__minus_builder_checkpoint);
                                        if __minus_excl.is_ok() {
                                            return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                                offset: *p as u32,
                                                failing_state: crate::runtime::tape::DtaStateId::NONE,
                                                failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                            });
                                        }
                                        let _ = ({
                                            parse_altdispatch_EbnfParser_character(
                                                input,
                                                p,
                                                state,
                                                builder,
                                            )
                                        })?;
                                    }
                                    ::core::result::Result::Ok(())
                                })();
                                match __iter_result {
                                    ::core::result::Result::Ok(()) => {
                                        if *p == __iter_save_p {
                                            builder.rollback(__iter_builder_checkpoint);
                                            break;
                                        }
                                        builder.commit(__iter_builder_checkpoint);
                                        __iter_count += 1;
                                    }
                                    ::core::result::Result::Err(_) => {
                                        *p = __iter_save_p;
                                        builder.rollback(__iter_builder_checkpoint);
                                        break;
                                    }
                                }
                            }
                            if __iter_count < 0u32 {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: *p as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                        }
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [34u8] {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: at as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            *p = end;
                        }
                        ::core::result::Result::Ok(())
                    })();
                    match __seq_result {
                        ::core::result::Result::Ok(()) => {
                            let __seq_span_hi = *p;
                            builder.rollback(__seq_builder_checkpoint);
                            let __seq_text = unsafe {
                                ::core::str::from_utf8_unchecked(
                                    &input[__seq_span_lo..__seq_span_hi],
                                )
                            };
                            builder.push_leaf_with_str(__seq_text);
                            return ::core::result::Result::Ok(
                                crate::runtime::tape::TapeOffset::NONE,
                            );
                        }
                        ::core::result::Result::Err(__err) => {
                            *p = __seq_span_lo;
                            builder.rollback(__seq_builder_checkpoint);
                            return ::core::result::Result::Err(__err);
                        }
                    }
                }
                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            39u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [39u8] {
                    let __seq_span_lo = *p;
                    let __seq_builder_checkpoint = builder.checkpoint();
                    let __seq_result: ::core::result::Result<
                        (),
                        crate::runtime::tape::DtaError,
                    > = (|| {
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [39u8] {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: at as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            *p = end;
                        }
                        {
                            let __minus_save_p = *p;
                            let __minus_builder_checkpoint = builder.checkpoint();
                            let __minus_excl: ::core::result::Result<
                                (),
                                crate::runtime::tape::DtaError,
                            > = (|| {
                                {
                                    let at = *p;
                                    let end = at + 1usize;
                                    if input.len() < end || input[at..end] != [39u8] {
                                        return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                            offset: at as u32,
                                            failing_state: crate::runtime::tape::DtaStateId::NONE,
                                            failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                        });
                                    }
                                    *p = end;
                                }
                                ::core::result::Result::Ok(())
                            })();
                            *p = __minus_save_p;
                            builder.rollback(__minus_builder_checkpoint);
                            if __minus_excl.is_ok() {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: *p as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            let _ = ({
                                parse_altdispatch_EbnfParser_character(
                                    input,
                                    p,
                                    state,
                                    builder,
                                )
                            })?;
                        }
                        {
                            let mut __iter_count: u32 = 0;
                            loop {
                                let __iter_save_p = *p;
                                if input.get(*p).is_none() {
                                    break;
                                }
                                let __iter_builder_checkpoint = builder.checkpoint();
                                let __iter_result: ::core::result::Result<
                                    (),
                                    crate::runtime::tape::DtaError,
                                > = (|| {
                                    {
                                        let __minus_save_p = *p;
                                        let __minus_builder_checkpoint = builder.checkpoint();
                                        let __minus_excl: ::core::result::Result<
                                            (),
                                            crate::runtime::tape::DtaError,
                                        > = (|| {
                                            {
                                                let at = *p;
                                                let end = at + 1usize;
                                                if input.len() < end || input[at..end] != [39u8] {
                                                    return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                                        offset: at as u32,
                                                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                                                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                                    });
                                                }
                                                *p = end;
                                            }
                                            ::core::result::Result::Ok(())
                                        })();
                                        *p = __minus_save_p;
                                        builder.rollback(__minus_builder_checkpoint);
                                        if __minus_excl.is_ok() {
                                            return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                                offset: *p as u32,
                                                failing_state: crate::runtime::tape::DtaStateId::NONE,
                                                failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                            });
                                        }
                                        let _ = ({
                                            parse_altdispatch_EbnfParser_character(
                                                input,
                                                p,
                                                state,
                                                builder,
                                            )
                                        })?;
                                    }
                                    ::core::result::Result::Ok(())
                                })();
                                match __iter_result {
                                    ::core::result::Result::Ok(()) => {
                                        if *p == __iter_save_p {
                                            builder.rollback(__iter_builder_checkpoint);
                                            break;
                                        }
                                        builder.commit(__iter_builder_checkpoint);
                                        __iter_count += 1;
                                    }
                                    ::core::result::Result::Err(_) => {
                                        *p = __iter_save_p;
                                        builder.rollback(__iter_builder_checkpoint);
                                        break;
                                    }
                                }
                            }
                            if __iter_count < 0u32 {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: *p as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                        }
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [39u8] {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: at as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            *p = end;
                        }
                        ::core::result::Result::Ok(())
                    })();
                    match __seq_result {
                        ::core::result::Result::Ok(()) => {
                            let __seq_span_hi = *p;
                            builder.rollback(__seq_builder_checkpoint);
                            let __seq_text = unsafe {
                                ::core::str::from_utf8_unchecked(
                                    &input[__seq_span_lo..__seq_span_hi],
                                )
                            };
                            builder.push_leaf_with_str(__seq_text);
                            return ::core::result::Result::Ok(
                                crate::runtime::tape::TapeOffset::NONE,
                            );
                        }
                        ::core::result::Result::Err(__err) => {
                            *p = __seq_span_lo;
                            builder.rollback(__seq_builder_checkpoint);
                            return ::core::result::Result::Err(__err);
                        }
                    }
                }
                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            _ => {
                ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                })
            }
        }
    }
    /// AZ-I.W2.RF — per-grammar Flat-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / CSS L4 / Sheets per the
    /// resolver's `SubstrateBinding`).
    ///
    /// Walker-tape compound emission is replaced by typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns `TapeOffset::NONE` for compositional uniformity
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_EbnfParser_concatenation<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __concatenation_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 6u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("concatenation"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __concatenation_handle = <crate::runtime::ebnf::EbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__concatenation_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::tape::DtaError> = (||
        {
            {
                {
                    let mut __iter_count: u32 = 0;
                    loop {
                        if __iter_count >= 4294967295u32 {
                            break;
                        }
                        let __iter_save_p = *p;
                        if input.get(*p).is_none() {
                            break;
                        }
                        let __iter_builder_checkpoint = builder.checkpoint();
                        let __iter_result: ::core::result::Result<
                            (),
                            crate::runtime::tape::DtaError,
                        > = (|| {
                            {
                                let __scan_start = *p;
                                let Some(match_len) = __regex_scan_EbnfParser(
                                    "[ \\t\\n\\r\\f]*",
                                    input,
                                    *p,
                                ) else {
                                    return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                        offset: __scan_start as u32,
                                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                    });
                                };
                                *p += match_len as usize;
                            }
                            let _ = ({
                                let _ = __shape_support_EbnfParser::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_flat_EbnfParser_factor(input, p, state, builder)
                            })?;
                            {
                                let __scan_start = *p;
                                let Some(match_len) = __regex_scan_EbnfParser(
                                    "[ \\t\\n\\r\\f]*",
                                    input,
                                    *p,
                                ) else {
                                    return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                        offset: __scan_start as u32,
                                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                    });
                                };
                                *p += match_len as usize;
                            }
                            {
                                let mut __iter_count: u32 = 0;
                                loop {
                                    if __iter_count >= 1u32 {
                                        break;
                                    }
                                    let __iter_save_p = *p;
                                    if input.get(*p).is_none() {
                                        break;
                                    }
                                    let __iter_builder_checkpoint = builder.checkpoint();
                                    let __iter_result: ::core::result::Result<
                                        (),
                                        crate::runtime::tape::DtaError,
                                    > = (|| {
                                        let at = *p;
                                        let end = at + 1usize;
                                        if input.len() < end || input[at..end] != [44u8] {
                                            return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                                offset: at as u32,
                                                failing_state: crate::runtime::tape::DtaStateId::NONE,
                                                failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                            });
                                        }
                                        *p = end;
                                        Ok(())
                                    })();
                                    match __iter_result {
                                        Ok(()) => {
                                            if *p == __iter_save_p {
                                                builder.rollback(__iter_builder_checkpoint);
                                                break;
                                            }
                                            builder.commit(__iter_builder_checkpoint);
                                            __iter_count += 1;
                                        }
                                        Err(_) => {
                                            *p = __iter_save_p;
                                            builder.rollback(__iter_builder_checkpoint);
                                            break;
                                        }
                                    }
                                }
                                if __iter_count < 0u32 {
                                    return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                        offset: *p as u32,
                                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                    });
                                }
                            }
                            Ok(())
                        })();
                        match __iter_result {
                            Ok(()) => {
                                if *p == __iter_save_p {
                                    builder.rollback(__iter_builder_checkpoint);
                                    break;
                                }
                                builder.commit(__iter_builder_checkpoint);
                                __iter_count += 1;
                            }
                            Err(_) => {
                                *p = __iter_save_p;
                                builder.rollback(__iter_builder_checkpoint);
                                break;
                            }
                        }
                    }
                    if __iter_count < 1u32 {
                        return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                            offset: *p as u32,
                            failing_state: crate::runtime::tape::DtaStateId::NONE,
                            failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                        });
                    }
                }
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::ebnf::EbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __concatenation_handle,
                );
                ::core::result::Result::Ok(crate::runtime::tape::TapeOffset::NONE)
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RF — per-grammar Flat-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / CSS L4 / Sheets per the
    /// resolver's `SubstrateBinding`).
    ///
    /// Walker-tape compound emission is replaced by typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns `TapeOffset::NONE` for compositional uniformity
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_EbnfParser_alternation<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __alternation_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 7u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("alternation"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __alternation_handle = <crate::runtime::ebnf::EbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__alternation_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::tape::DtaError> = (||
        {
            {
                {
                    let mut __iter_count: u32 = 0;
                    loop {
                        if __iter_count >= 4294967295u32 {
                            break;
                        }
                        let __iter_save_p = *p;
                        if input.get(*p).is_none() {
                            break;
                        }
                        let __iter_builder_checkpoint = builder.checkpoint();
                        let __iter_result: ::core::result::Result<
                            (),
                            crate::runtime::tape::DtaError,
                        > = (|| {
                            {
                                let __scan_start = *p;
                                let Some(match_len) = __regex_scan_EbnfParser(
                                    "[ \\t\\n\\r\\f]*",
                                    input,
                                    *p,
                                ) else {
                                    return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                        offset: __scan_start as u32,
                                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                    });
                                };
                                *p += match_len as usize;
                            }
                            let _ = ({
                                parse_flat_EbnfParser_concatenation(
                                    input,
                                    p,
                                    state,
                                    builder,
                                )
                            })?;
                            {
                                let __scan_start = *p;
                                let Some(match_len) = __regex_scan_EbnfParser(
                                    "[ \\t\\n\\r\\f]*",
                                    input,
                                    *p,
                                ) else {
                                    return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                        offset: __scan_start as u32,
                                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                    });
                                };
                                *p += match_len as usize;
                            }
                            {
                                let mut __iter_count: u32 = 0;
                                loop {
                                    if __iter_count >= 1u32 {
                                        break;
                                    }
                                    let __iter_save_p = *p;
                                    if input.get(*p).is_none() {
                                        break;
                                    }
                                    let __iter_builder_checkpoint = builder.checkpoint();
                                    let __iter_result: ::core::result::Result<
                                        (),
                                        crate::runtime::tape::DtaError,
                                    > = (|| {
                                        let at = *p;
                                        let end = at + 1usize;
                                        if input.len() < end || input[at..end] != [124u8] {
                                            return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                                offset: at as u32,
                                                failing_state: crate::runtime::tape::DtaStateId::NONE,
                                                failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                            });
                                        }
                                        *p = end;
                                        Ok(())
                                    })();
                                    match __iter_result {
                                        Ok(()) => {
                                            if *p == __iter_save_p {
                                                builder.rollback(__iter_builder_checkpoint);
                                                break;
                                            }
                                            builder.commit(__iter_builder_checkpoint);
                                            __iter_count += 1;
                                        }
                                        Err(_) => {
                                            *p = __iter_save_p;
                                            builder.rollback(__iter_builder_checkpoint);
                                            break;
                                        }
                                    }
                                }
                                if __iter_count < 0u32 {
                                    return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                        offset: *p as u32,
                                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                    });
                                }
                            }
                            Ok(())
                        })();
                        match __iter_result {
                            Ok(()) => {
                                if *p == __iter_save_p {
                                    builder.rollback(__iter_builder_checkpoint);
                                    break;
                                }
                                builder.commit(__iter_builder_checkpoint);
                                __iter_count += 1;
                            }
                            Err(_) => {
                                *p = __iter_save_p;
                                builder.rollback(__iter_builder_checkpoint);
                                break;
                            }
                        }
                    }
                    if __iter_count < 1u32 {
                        return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                            offset: *p as u32,
                            failing_state: crate::runtime::tape::DtaStateId::NONE,
                            failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                        });
                    }
                }
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::ebnf::EbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __alternation_handle,
                );
                ::core::result::Result::Ok(crate::runtime::tape::TapeOffset::NONE)
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RC — per-grammar Scalar-shape parse
    /// function (transparent-Ref body, struct-direct
    /// substrate). Delegates to the target's
    /// strategy-resolved shape fn; the inner call
    /// expression names `builder` against the
    /// concrete struct-builder.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_scalar_EbnfParser_rhs<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        { parse_flat_EbnfParser_alternation(input, p, state, builder) }
    }
    /// AZ-I.W2.RB — per-grammar AltDispatch-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / Sheets / CSS L4 per the resolver's
    /// `SubstrateBinding`).
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_variables,
        unused_mut,
        unused_assignments,
        unreachable_code
    )]
    pub fn parse_altdispatch_EbnfParser_term<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::tape::DtaError> {
        use crate::runtime::builder::StructBuilder;
        let first = __shape_support_EbnfParser::skip_space(input, p, state)
            .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 9u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("term"),
            kind: ::bbnf_ir::registry::LayoutKind::TaggedEnum,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __dispatch_checkpoint = builder.checkpoint();
        let __handle = builder.begin_compound(&__layout);
        let __dispatch_result: ::core::result::Result<
            (),
            crate::runtime::tape::DtaError,
        > = (|| {
            'try_branches: loop {
                {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    let attempt: ::core::result::Result<
                        (),
                        crate::runtime::tape::DtaError,
                    > = (|| {
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [40u8] {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: at as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            *p = end;
                        }
                        {
                            let __scan_start = *p;
                            let Some(match_len) = __regex_scan_EbnfParser(
                                "[ \\t\\n\\r\\f]*",
                                input,
                                *p,
                            ) else {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: __scan_start as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            };
                            *p += match_len as usize;
                        }
                        let _ = ({
                            parse_scalar_EbnfParser_rhs(input, p, state, builder)
                        })?;
                        {
                            let __scan_start = *p;
                            let Some(match_len) = __regex_scan_EbnfParser(
                                "[ \\t\\n\\r\\f]*",
                                input,
                                *p,
                            ) else {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: __scan_start as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            };
                            *p += match_len as usize;
                        }
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [41u8] {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: at as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            *p = end;
                        }
                        ::core::result::Result::Ok(())
                    })();
                    match attempt {
                        ::core::result::Result::Ok(()) => {
                            builder.push_branch_tag(0u32);
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    let attempt: ::core::result::Result<
                        (),
                        crate::runtime::tape::DtaError,
                    > = (|| {
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [91u8] {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: at as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            *p = end;
                        }
                        {
                            let __scan_start = *p;
                            let Some(match_len) = __regex_scan_EbnfParser(
                                "[ \\t\\n\\r\\f]*",
                                input,
                                *p,
                            ) else {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: __scan_start as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            };
                            *p += match_len as usize;
                        }
                        let _ = ({
                            parse_scalar_EbnfParser_rhs(input, p, state, builder)
                        })?;
                        {
                            let __scan_start = *p;
                            let Some(match_len) = __regex_scan_EbnfParser(
                                "[ \\t\\n\\r\\f]*",
                                input,
                                *p,
                            ) else {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: __scan_start as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            };
                            *p += match_len as usize;
                        }
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [93u8] {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: at as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            *p = end;
                        }
                        ::core::result::Result::Ok(())
                    })();
                    match attempt {
                        ::core::result::Result::Ok(()) => {
                            builder.push_branch_tag(1u32);
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    let attempt: ::core::result::Result<
                        (),
                        crate::runtime::tape::DtaError,
                    > = (|| {
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [123u8] {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: at as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            *p = end;
                        }
                        {
                            let __scan_start = *p;
                            let Some(match_len) = __regex_scan_EbnfParser(
                                "[ \\t\\n\\r\\f]*",
                                input,
                                *p,
                            ) else {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: __scan_start as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            };
                            *p += match_len as usize;
                        }
                        let _ = ({
                            parse_scalar_EbnfParser_rhs(input, p, state, builder)
                        })?;
                        {
                            let __scan_start = *p;
                            let Some(match_len) = __regex_scan_EbnfParser(
                                "[ \\t\\n\\r\\f]*",
                                input,
                                *p,
                            ) else {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: __scan_start as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            };
                            *p += match_len as usize;
                        }
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [125u8] {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: at as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            *p = end;
                        }
                        ::core::result::Result::Ok(())
                    })();
                    match attempt {
                        ::core::result::Result::Ok(()) => {
                            builder.push_branch_tag(2u32);
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match {
                        let __first = __shape_support_EbnfParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                                offset: *p as u32,
                            })?;
                        parse_keyword_EbnfParser_terminal(
                            input,
                            p,
                            __first,
                            state,
                            builder,
                        )
                    } {
                        Ok(_) => {
                            builder.push_branch_tag(3u32);
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match {
                        let _ = __shape_support_EbnfParser::skip_space(input, p, state);
                        parse_flat_EbnfParser_identifier(input, p, state, builder)
                    } {
                        Ok(_) => {
                            builder.push_branch_tag(4u32);
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            ::core::result::Result::Ok(())
        })();
        match __dispatch_result {
            ::core::result::Result::Ok(()) => {
                builder.end_compound(__handle);
                Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__dispatch_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RF — per-grammar Flat-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / CSS L4 / Sheets per the
    /// resolver's `SubstrateBinding`).
    ///
    /// Walker-tape compound emission is replaced by typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns `TapeOffset::NONE` for compositional uniformity
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_EbnfParser_factor<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __factor_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 10u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("factor"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __factor_handle = <crate::runtime::ebnf::EbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__factor_layout);
        let __body_result: ::core::result::Result<(), crate::runtime::tape::DtaError> = (||
        {
            {
                let _ = ({
                    let _ = __shape_support_EbnfParser::skip_space(input, p, state);
                    parse_altdispatch_EbnfParser_term(input, p, state, builder)
                })?;
            }
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_EbnfParser(
                        "[ \\t\\n\\r\\f]*",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                            offset: __scan_start as u32,
                            failing_state: crate::runtime::tape::DtaStateId::NONE,
                            failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                'try_branches: loop {
                    {
                        let __alt_save_p = *p;
                        let __alt_builder_checkpoint = builder.checkpoint();
                        let __alt_result: ::core::result::Result<
                            (),
                            crate::runtime::tape::DtaError,
                        > = (|| {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [63u8] {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: at as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            *p = end;
                            Ok(())
                        })();
                        match __alt_result {
                            Ok(()) => {
                                builder.commit(__alt_builder_checkpoint);
                                break 'try_branches;
                            }
                            Err(_) => {
                                *p = __alt_save_p;
                                builder.rollback(__alt_builder_checkpoint);
                            }
                        }
                    }
                    {
                        let __alt_save_p = *p;
                        let __alt_builder_checkpoint = builder.checkpoint();
                        let __alt_result: ::core::result::Result<
                            (),
                            crate::runtime::tape::DtaError,
                        > = (|| {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [42u8] {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: at as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            *p = end;
                            Ok(())
                        })();
                        match __alt_result {
                            Ok(()) => {
                                builder.commit(__alt_builder_checkpoint);
                                break 'try_branches;
                            }
                            Err(_) => {
                                *p = __alt_save_p;
                                builder.rollback(__alt_builder_checkpoint);
                            }
                        }
                    }
                    {
                        let __alt_save_p = *p;
                        let __alt_builder_checkpoint = builder.checkpoint();
                        let __alt_result: ::core::result::Result<
                            (),
                            crate::runtime::tape::DtaError,
                        > = (|| {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [43u8] {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: at as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            *p = end;
                            Ok(())
                        })();
                        match __alt_result {
                            Ok(()) => {
                                builder.commit(__alt_builder_checkpoint);
                                break 'try_branches;
                            }
                            Err(_) => {
                                *p = __alt_save_p;
                                builder.rollback(__alt_builder_checkpoint);
                            }
                        }
                    }
                    {
                        let __alt_save_p = *p;
                        let __alt_builder_checkpoint = builder.checkpoint();
                        let __alt_result: ::core::result::Result<
                            (),
                            crate::runtime::tape::DtaError,
                        > = (|| {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [45u8] {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: at as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            *p = end;
                            {
                                let __scan_start = *p;
                                let Some(match_len) = __regex_scan_EbnfParser(
                                    "[ \\t\\n\\r\\f]*",
                                    input,
                                    *p,
                                ) else {
                                    return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                        offset: __scan_start as u32,
                                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                    });
                                };
                                *p += match_len as usize;
                            }
                            let _ = ({
                                let _ = __shape_support_EbnfParser::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_altdispatch_EbnfParser_term(input, p, state, builder)
                            })?;
                            Ok(())
                        })();
                        match __alt_result {
                            Ok(()) => {
                                builder.commit(__alt_builder_checkpoint);
                                break 'try_branches;
                            }
                            Err(_) => {
                                *p = __alt_save_p;
                                builder.rollback(__alt_builder_checkpoint);
                            }
                        }
                    }
                    {
                        let __alt_save_p = *p;
                        let __alt_builder_checkpoint = builder.checkpoint();
                        let __alt_result: ::core::result::Result<
                            (),
                            crate::runtime::tape::DtaError,
                        > = (|| { Ok(()) })();
                        match __alt_result {
                            Ok(()) => {
                                builder.commit(__alt_builder_checkpoint);
                                break 'try_branches;
                            }
                            Err(_) => {
                                *p = __alt_save_p;
                                builder.rollback(__alt_builder_checkpoint);
                            }
                        }
                    }
                    return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    });
                }
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::ebnf::EbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __factor_handle,
                );
                ::core::result::Result::Ok(crate::runtime::tape::TapeOffset::NONE)
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RF — per-grammar Flat-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / CSS L4 / Sheets per the
    /// resolver's `SubstrateBinding`).
    ///
    /// Walker-tape compound emission is replaced by typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns `TapeOffset::NONE` for compositional uniformity
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_EbnfParser_rule<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __rule_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 11u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("rule"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __rule_handle = <crate::runtime::ebnf::EbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__rule_layout);
        let __body_result: ::core::result::Result<(), crate::runtime::tape::DtaError> = (||
        {
            {
                let _ = ({
                    let _ = __shape_support_EbnfParser::skip_space(input, p, state);
                    parse_flat_EbnfParser_identifier(input, p, state, builder)
                })?;
            }
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_EbnfParser(
                        "[ \\t\\n\\r\\f]*",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                            offset: __scan_start as u32,
                            failing_state: crate::runtime::tape::DtaStateId::NONE,
                            failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [61u8] {
                    return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                        offset: at as u32,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    });
                }
                *p = end;
            }
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_EbnfParser(
                        "[ \\t\\n\\r\\f]*",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                            offset: __scan_start as u32,
                            failing_state: crate::runtime::tape::DtaStateId::NONE,
                            failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                let _ = ({ parse_scalar_EbnfParser_rhs(input, p, state, builder) })?;
            }
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_EbnfParser(
                        "[ \\t\\n\\r\\f]*",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                            offset: __scan_start as u32,
                            failing_state: crate::runtime::tape::DtaStateId::NONE,
                            failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                'try_branches: loop {
                    {
                        let __alt_save_p = *p;
                        let __alt_builder_checkpoint = builder.checkpoint();
                        let __alt_result: ::core::result::Result<
                            (),
                            crate::runtime::tape::DtaError,
                        > = (|| {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [59u8] {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: at as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            *p = end;
                            Ok(())
                        })();
                        match __alt_result {
                            Ok(()) => {
                                builder.commit(__alt_builder_checkpoint);
                                break 'try_branches;
                            }
                            Err(_) => {
                                *p = __alt_save_p;
                                builder.rollback(__alt_builder_checkpoint);
                            }
                        }
                    }
                    {
                        let __alt_save_p = *p;
                        let __alt_builder_checkpoint = builder.checkpoint();
                        let __alt_result: ::core::result::Result<
                            (),
                            crate::runtime::tape::DtaError,
                        > = (|| {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [46u8] {
                                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                    offset: at as u32,
                                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            *p = end;
                            Ok(())
                        })();
                        match __alt_result {
                            Ok(()) => {
                                builder.commit(__alt_builder_checkpoint);
                                break 'try_branches;
                            }
                            Err(_) => {
                                *p = __alt_save_p;
                                builder.rollback(__alt_builder_checkpoint);
                            }
                        }
                    }
                    return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    });
                }
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::ebnf::EbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __rule_handle,
                );
                ::core::result::Result::Ok(crate::runtime::tape::TapeOffset::NONE)
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-II.cutover.F — per-grammar Array-shape parse function
    /// (Shape 2 — entry-rule list, **struct-direct body**).
    ///
    /// Opens the rule's compound frame on the StructBuilder,
    /// iterates the inner Repeat with savepoint rollback, and
    /// closes the frame on first-byte rejection or EOF. NO
    /// bracket-delimiter literals — termination is driven by
    /// the inner dispatcher's first-set check.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_array_EbnfParser_grammar<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        use crate::runtime::builder::StructBuilder;
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 12u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("grammar"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __handle = builder.begin_compound(&__layout);
        loop {
            let __iter_save_p = *p;
            if input.get(*p).is_none() {
                break;
            }
            let __iter_builder_checkpoint = builder.checkpoint();
            let __iter_result: ::core::result::Result<
                (),
                crate::runtime::tape::DtaError,
            > = (|| {
                let _ = __shape_support_EbnfParser::skip_space(input, p, state);
                ({
                    let _ = __shape_support_EbnfParser::skip_space(input, p, state);
                    parse_flat_EbnfParser_rule(input, p, state, builder)
                })?;
                let _ = __shape_support_EbnfParser::skip_space(input, p, state);
                Ok(())
            })();
            match __iter_result {
                Ok(()) => {
                    if *p == __iter_save_p {
                        builder.rollback(__iter_builder_checkpoint);
                        break;
                    }
                    builder.commit(__iter_builder_checkpoint);
                }
                Err(_) => {
                    *p = __iter_save_p;
                    builder.rollback(__iter_builder_checkpoint);
                    break;
                }
            }
        }
        builder.end_compound(__handle);
        Ok(crate::runtime::tape::TapeOffset::NONE)
    }
    /// AX.W0a.2.b — visitor-path AltDispatch-shape parse function.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_variables,
        unused_mut,
        unused_assignments,
        unreachable_code
    )]
    pub fn parse_altdispatch_visitor_EbnfParser_letter<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        let first = __shape_support_EbnfParser::skip_space(input, p, state)
            .ok_or(crate::runtime::ParseErr::Syntax {
                offset: *p as u32,
                rule: None,
            })?;
        'try_branches: loop {
            match first {
                65u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [65u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                66u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [66u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                67u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [67u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                68u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [68u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                69u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [69u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                70u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [70u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                71u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [71u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                72u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [72u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                73u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [73u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                74u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [74u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                75u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [75u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                76u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [76u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                77u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [77u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                78u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [78u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                79u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [79u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                80u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [80u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                81u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [81u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                82u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [82u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                83u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [83u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                84u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [84u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                85u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [85u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                86u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [86u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                87u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [87u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                88u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [88u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                89u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [89u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                90u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [90u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                97u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [97u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                98u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [98u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                99u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [99u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                100u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [100u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                101u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [101u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                102u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [102u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                103u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [103u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                104u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [104u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                105u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [105u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                106u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [106u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                107u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [107u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                108u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [108u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                109u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [109u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                110u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [110u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                111u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [111u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                112u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [112u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                113u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [113u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                114u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [114u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                115u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [115u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                116u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [116u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                117u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [117u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                118u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [118u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                119u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [119u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                120u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [120u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                121u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [121u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                122u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [122u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                _ => {}
            }
            return Err(crate::runtime::ParseErr::Syntax {
                offset: *p as u32,
                rule: None,
            });
        }
        Ok(())
    }
    /// AW-V.W3-bench-fix — visitor-path Keyword-shape parse
    /// function (Alt of literal-led or Ref-led branches).
    ///
    /// AX.W0a.2.g — admits Ref-led branches; threads `state`
    /// for downstream visitor-path Ref calls.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_keyword_visitor_EbnfParser_digit<V>(
        input: &[u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_EbnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::KeywordVisitor + crate::runtime::tape::ObjectVisitor
            + crate::runtime::tape::ArrayVisitor + crate::runtime::tape::StringVisitor
            + crate::runtime::tape::NumberVisitor,
    {
        let _ = state;
        match first_byte {
            48u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [48u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    return visitor
                        .null()
                        .map_err(|_| crate::runtime::ParseErr::Syntax {
                            offset: at as u32,
                            rule: None,
                        });
                }
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                });
            }
            49u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [49u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    return visitor
                        .null()
                        .map_err(|_| crate::runtime::ParseErr::Syntax {
                            offset: at as u32,
                            rule: None,
                        });
                }
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                });
            }
            50u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [50u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    return visitor
                        .null()
                        .map_err(|_| crate::runtime::ParseErr::Syntax {
                            offset: at as u32,
                            rule: None,
                        });
                }
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                });
            }
            51u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [51u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    return visitor
                        .null()
                        .map_err(|_| crate::runtime::ParseErr::Syntax {
                            offset: at as u32,
                            rule: None,
                        });
                }
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                });
            }
            52u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [52u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    return visitor
                        .null()
                        .map_err(|_| crate::runtime::ParseErr::Syntax {
                            offset: at as u32,
                            rule: None,
                        });
                }
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                });
            }
            53u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [53u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    return visitor
                        .null()
                        .map_err(|_| crate::runtime::ParseErr::Syntax {
                            offset: at as u32,
                            rule: None,
                        });
                }
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                });
            }
            54u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [54u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    return visitor
                        .null()
                        .map_err(|_| crate::runtime::ParseErr::Syntax {
                            offset: at as u32,
                            rule: None,
                        });
                }
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                });
            }
            55u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [55u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    return visitor
                        .null()
                        .map_err(|_| crate::runtime::ParseErr::Syntax {
                            offset: at as u32,
                            rule: None,
                        });
                }
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                });
            }
            56u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [56u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    return visitor
                        .null()
                        .map_err(|_| crate::runtime::ParseErr::Syntax {
                            offset: at as u32,
                            rule: None,
                        });
                }
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                });
            }
            57u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [57u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    return visitor
                        .null()
                        .map_err(|_| crate::runtime::ParseErr::Syntax {
                            offset: at as u32,
                            rule: None,
                        });
                }
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                });
            }
            _ => {
                Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                })
            }
        }
    }
    /// AX.W0a.2.b — visitor-path AltDispatch-shape parse function.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_variables,
        unused_mut,
        unused_assignments,
        unreachable_code
    )]
    pub fn parse_altdispatch_visitor_EbnfParser_symbol<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        let first = __shape_support_EbnfParser::skip_space(input, p, state)
            .ok_or(crate::runtime::ParseErr::Syntax {
                offset: *p as u32,
                rule: None,
            })?;
        'try_branches: loop {
            match first {
                8u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [8u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                9u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [9u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                10u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [10u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                12u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [12u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                13u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [13u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                34u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [34u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                39u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [39u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                40u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [40u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                41u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [41u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                42u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [42u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                43u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [43u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                44u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [44u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                45u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [45u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                46u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [46u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                59u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [59u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                60u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [60u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                61u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [61u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                62u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [62u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                63u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [63u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                91u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [91u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                92u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [92u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                93u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [93u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                123u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [123u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                124u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [124u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                125u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [125u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                _ => {}
            }
            return Err(crate::runtime::ParseErr::Syntax {
                offset: *p as u32,
                rule: None,
            });
        }
        Ok(())
    }
    /// AW-V.W4-fix — visitor-path Flat-shape parse function.
    ///
    /// Mirrors the tape-path emitter structure. Literal positions
    /// byte-match without emitting a visitor event; Ref / Regex /
    /// Alt positions recurse through the visitor dispatcher.
    ///
    /// AX.W0a.2.f — compound; see tape-path comment for the
    /// `#[inline]` downgrade rationale.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_visitor_EbnfParser_identifier<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        {
            ({
                let _ = __shape_support_EbnfParser::skip_space(input, p, state);
                parse_altdispatch_visitor_EbnfParser_letter(input, p, state, visitor)
            })?;
        }
        {
            let mut iter_count: u32 = 0;
            loop {
                let save_p = *p;
                let res = (|| -> ::core::result::Result<(), crate::runtime::ParseErr> {
                    {
                        let first = __shape_support_EbnfParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(crate::runtime::ParseErr::Syntax {
                                offset: *p as u32,
                                rule: None,
                            })?;
                        'try_branches: loop {
                            match first {
                                48u8 => {
                                    let attempt_p = *p;
                                    match {
                                        let __first = __shape_support_EbnfParser::skip_space(
                                                input,
                                                p,
                                                state,
                                            )
                                            .ok_or(crate::runtime::ParseErr::Syntax {
                                                offset: *p as u32,
                                                rule: None,
                                            })?;
                                        parse_keyword_visitor_EbnfParser_digit(
                                            input,
                                            p,
                                            __first,
                                            state,
                                            visitor,
                                        )
                                    } {
                                        Ok(_) => break 'try_branches,
                                        Err(_) => {
                                            *p = attempt_p;
                                        }
                                    }
                                }
                                49u8 => {
                                    let attempt_p = *p;
                                    match {
                                        let __first = __shape_support_EbnfParser::skip_space(
                                                input,
                                                p,
                                                state,
                                            )
                                            .ok_or(crate::runtime::ParseErr::Syntax {
                                                offset: *p as u32,
                                                rule: None,
                                            })?;
                                        parse_keyword_visitor_EbnfParser_digit(
                                            input,
                                            p,
                                            __first,
                                            state,
                                            visitor,
                                        )
                                    } {
                                        Ok(_) => break 'try_branches,
                                        Err(_) => {
                                            *p = attempt_p;
                                        }
                                    }
                                }
                                50u8 => {
                                    let attempt_p = *p;
                                    match {
                                        let __first = __shape_support_EbnfParser::skip_space(
                                                input,
                                                p,
                                                state,
                                            )
                                            .ok_or(crate::runtime::ParseErr::Syntax {
                                                offset: *p as u32,
                                                rule: None,
                                            })?;
                                        parse_keyword_visitor_EbnfParser_digit(
                                            input,
                                            p,
                                            __first,
                                            state,
                                            visitor,
                                        )
                                    } {
                                        Ok(_) => break 'try_branches,
                                        Err(_) => {
                                            *p = attempt_p;
                                        }
                                    }
                                }
                                51u8 => {
                                    let attempt_p = *p;
                                    match {
                                        let __first = __shape_support_EbnfParser::skip_space(
                                                input,
                                                p,
                                                state,
                                            )
                                            .ok_or(crate::runtime::ParseErr::Syntax {
                                                offset: *p as u32,
                                                rule: None,
                                            })?;
                                        parse_keyword_visitor_EbnfParser_digit(
                                            input,
                                            p,
                                            __first,
                                            state,
                                            visitor,
                                        )
                                    } {
                                        Ok(_) => break 'try_branches,
                                        Err(_) => {
                                            *p = attempt_p;
                                        }
                                    }
                                }
                                52u8 => {
                                    let attempt_p = *p;
                                    match {
                                        let __first = __shape_support_EbnfParser::skip_space(
                                                input,
                                                p,
                                                state,
                                            )
                                            .ok_or(crate::runtime::ParseErr::Syntax {
                                                offset: *p as u32,
                                                rule: None,
                                            })?;
                                        parse_keyword_visitor_EbnfParser_digit(
                                            input,
                                            p,
                                            __first,
                                            state,
                                            visitor,
                                        )
                                    } {
                                        Ok(_) => break 'try_branches,
                                        Err(_) => {
                                            *p = attempt_p;
                                        }
                                    }
                                }
                                53u8 => {
                                    let attempt_p = *p;
                                    match {
                                        let __first = __shape_support_EbnfParser::skip_space(
                                                input,
                                                p,
                                                state,
                                            )
                                            .ok_or(crate::runtime::ParseErr::Syntax {
                                                offset: *p as u32,
                                                rule: None,
                                            })?;
                                        parse_keyword_visitor_EbnfParser_digit(
                                            input,
                                            p,
                                            __first,
                                            state,
                                            visitor,
                                        )
                                    } {
                                        Ok(_) => break 'try_branches,
                                        Err(_) => {
                                            *p = attempt_p;
                                        }
                                    }
                                }
                                54u8 => {
                                    let attempt_p = *p;
                                    match {
                                        let __first = __shape_support_EbnfParser::skip_space(
                                                input,
                                                p,
                                                state,
                                            )
                                            .ok_or(crate::runtime::ParseErr::Syntax {
                                                offset: *p as u32,
                                                rule: None,
                                            })?;
                                        parse_keyword_visitor_EbnfParser_digit(
                                            input,
                                            p,
                                            __first,
                                            state,
                                            visitor,
                                        )
                                    } {
                                        Ok(_) => break 'try_branches,
                                        Err(_) => {
                                            *p = attempt_p;
                                        }
                                    }
                                }
                                55u8 => {
                                    let attempt_p = *p;
                                    match {
                                        let __first = __shape_support_EbnfParser::skip_space(
                                                input,
                                                p,
                                                state,
                                            )
                                            .ok_or(crate::runtime::ParseErr::Syntax {
                                                offset: *p as u32,
                                                rule: None,
                                            })?;
                                        parse_keyword_visitor_EbnfParser_digit(
                                            input,
                                            p,
                                            __first,
                                            state,
                                            visitor,
                                        )
                                    } {
                                        Ok(_) => break 'try_branches,
                                        Err(_) => {
                                            *p = attempt_p;
                                        }
                                    }
                                }
                                56u8 => {
                                    let attempt_p = *p;
                                    match {
                                        let __first = __shape_support_EbnfParser::skip_space(
                                                input,
                                                p,
                                                state,
                                            )
                                            .ok_or(crate::runtime::ParseErr::Syntax {
                                                offset: *p as u32,
                                                rule: None,
                                            })?;
                                        parse_keyword_visitor_EbnfParser_digit(
                                            input,
                                            p,
                                            __first,
                                            state,
                                            visitor,
                                        )
                                    } {
                                        Ok(_) => break 'try_branches,
                                        Err(_) => {
                                            *p = attempt_p;
                                        }
                                    }
                                }
                                57u8 => {
                                    let attempt_p = *p;
                                    match {
                                        let __first = __shape_support_EbnfParser::skip_space(
                                                input,
                                                p,
                                                state,
                                            )
                                            .ok_or(crate::runtime::ParseErr::Syntax {
                                                offset: *p as u32,
                                                rule: None,
                                            })?;
                                        parse_keyword_visitor_EbnfParser_digit(
                                            input,
                                            p,
                                            __first,
                                            state,
                                            visitor,
                                        )
                                    } {
                                        Ok(_) => break 'try_branches,
                                        Err(_) => {
                                            *p = attempt_p;
                                        }
                                    }
                                }
                                95u8 => {
                                    let at = *p;
                                    let end = at + 1usize;
                                    if input.len() >= end && input[at..end] == [95u8] {
                                        *p = end;
                                        break 'try_branches;
                                    }
                                }
                                _ => {}
                            }
                            {
                                let attempt_p = *p;
                                match {
                                    let _ = __shape_support_EbnfParser::skip_space(
                                        input,
                                        p,
                                        state,
                                    );
                                    parse_altdispatch_visitor_EbnfParser_letter(
                                        input,
                                        p,
                                        state,
                                        visitor,
                                    )
                                } {
                                    Ok(_) => break 'try_branches,
                                    Err(_) => {
                                        *p = attempt_p;
                                    }
                                }
                            }
                            return ::core::result::Result::Err(crate::runtime::ParseErr::Syntax {
                                offset: *p as u32,
                                rule: None,
                            });
                        }
                    }
                    Ok(())
                })();
                if res.is_err() {
                    *p = save_p;
                    break;
                }
                if *p == save_p {
                    break;
                }
                iter_count = iter_count.saturating_add(1);
            }
            if iter_count < (0usize as u32) {
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                });
            }
        }
        Ok(())
    }
    /// AX.W0a.2.b — visitor-path AltDispatch-shape parse function.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_variables,
        unused_mut,
        unused_assignments,
        unreachable_code
    )]
    pub fn parse_altdispatch_visitor_EbnfParser_character<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        let first = __shape_support_EbnfParser::skip_space(input, p, state)
            .ok_or(crate::runtime::ParseErr::Syntax {
                offset: *p as u32,
                rule: None,
            })?;
        'try_branches: loop {
            match first {
                32u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [32u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                48u8 => {
                    let attempt_p = *p;
                    match {
                        let __first = __shape_support_EbnfParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(crate::runtime::ParseErr::Syntax {
                                offset: *p as u32,
                                rule: None,
                            })?;
                        parse_keyword_visitor_EbnfParser_digit(
                            input,
                            p,
                            __first,
                            state,
                            visitor,
                        )
                    } {
                        Ok(_) => break 'try_branches,
                        Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                49u8 => {
                    let attempt_p = *p;
                    match {
                        let __first = __shape_support_EbnfParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(crate::runtime::ParseErr::Syntax {
                                offset: *p as u32,
                                rule: None,
                            })?;
                        parse_keyword_visitor_EbnfParser_digit(
                            input,
                            p,
                            __first,
                            state,
                            visitor,
                        )
                    } {
                        Ok(_) => break 'try_branches,
                        Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                50u8 => {
                    let attempt_p = *p;
                    match {
                        let __first = __shape_support_EbnfParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(crate::runtime::ParseErr::Syntax {
                                offset: *p as u32,
                                rule: None,
                            })?;
                        parse_keyword_visitor_EbnfParser_digit(
                            input,
                            p,
                            __first,
                            state,
                            visitor,
                        )
                    } {
                        Ok(_) => break 'try_branches,
                        Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                51u8 => {
                    let attempt_p = *p;
                    match {
                        let __first = __shape_support_EbnfParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(crate::runtime::ParseErr::Syntax {
                                offset: *p as u32,
                                rule: None,
                            })?;
                        parse_keyword_visitor_EbnfParser_digit(
                            input,
                            p,
                            __first,
                            state,
                            visitor,
                        )
                    } {
                        Ok(_) => break 'try_branches,
                        Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                52u8 => {
                    let attempt_p = *p;
                    match {
                        let __first = __shape_support_EbnfParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(crate::runtime::ParseErr::Syntax {
                                offset: *p as u32,
                                rule: None,
                            })?;
                        parse_keyword_visitor_EbnfParser_digit(
                            input,
                            p,
                            __first,
                            state,
                            visitor,
                        )
                    } {
                        Ok(_) => break 'try_branches,
                        Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                53u8 => {
                    let attempt_p = *p;
                    match {
                        let __first = __shape_support_EbnfParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(crate::runtime::ParseErr::Syntax {
                                offset: *p as u32,
                                rule: None,
                            })?;
                        parse_keyword_visitor_EbnfParser_digit(
                            input,
                            p,
                            __first,
                            state,
                            visitor,
                        )
                    } {
                        Ok(_) => break 'try_branches,
                        Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                54u8 => {
                    let attempt_p = *p;
                    match {
                        let __first = __shape_support_EbnfParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(crate::runtime::ParseErr::Syntax {
                                offset: *p as u32,
                                rule: None,
                            })?;
                        parse_keyword_visitor_EbnfParser_digit(
                            input,
                            p,
                            __first,
                            state,
                            visitor,
                        )
                    } {
                        Ok(_) => break 'try_branches,
                        Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                55u8 => {
                    let attempt_p = *p;
                    match {
                        let __first = __shape_support_EbnfParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(crate::runtime::ParseErr::Syntax {
                                offset: *p as u32,
                                rule: None,
                            })?;
                        parse_keyword_visitor_EbnfParser_digit(
                            input,
                            p,
                            __first,
                            state,
                            visitor,
                        )
                    } {
                        Ok(_) => break 'try_branches,
                        Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                56u8 => {
                    let attempt_p = *p;
                    match {
                        let __first = __shape_support_EbnfParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(crate::runtime::ParseErr::Syntax {
                                offset: *p as u32,
                                rule: None,
                            })?;
                        parse_keyword_visitor_EbnfParser_digit(
                            input,
                            p,
                            __first,
                            state,
                            visitor,
                        )
                    } {
                        Ok(_) => break 'try_branches,
                        Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                57u8 => {
                    let attempt_p = *p;
                    match {
                        let __first = __shape_support_EbnfParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(crate::runtime::ParseErr::Syntax {
                                offset: *p as u32,
                                rule: None,
                            })?;
                        parse_keyword_visitor_EbnfParser_digit(
                            input,
                            p,
                            __first,
                            state,
                            visitor,
                        )
                    } {
                        Ok(_) => break 'try_branches,
                        Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                95u8 => {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [95u8] {
                        *p = end;
                        break 'try_branches;
                    }
                }
                _ => {}
            }
            {
                let attempt_p = *p;
                match {
                    let _ = __shape_support_EbnfParser::skip_space(input, p, state);
                    parse_altdispatch_visitor_EbnfParser_symbol(input, p, state, visitor)
                } {
                    Ok(_) => break 'try_branches,
                    Err(_) => {
                        *p = attempt_p;
                    }
                }
            }
            {
                let attempt_p = *p;
                match {
                    let _ = __shape_support_EbnfParser::skip_space(input, p, state);
                    parse_altdispatch_visitor_EbnfParser_letter(input, p, state, visitor)
                } {
                    Ok(_) => break 'try_branches,
                    Err(_) => {
                        *p = attempt_p;
                    }
                }
            }
            return Err(crate::runtime::ParseErr::Syntax {
                offset: *p as u32,
                rule: None,
            });
        }
        Ok(())
    }
    /// AW-V.W3-bench-fix — visitor-path Keyword-shape parse
    /// function (Alt of literal-led or Ref-led branches).
    ///
    /// AX.W0a.2.g — admits Ref-led branches; threads `state`
    /// for downstream visitor-path Ref calls.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_keyword_visitor_EbnfParser_terminal<V>(
        input: &[u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_EbnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::KeywordVisitor + crate::runtime::tape::ObjectVisitor
            + crate::runtime::tape::ArrayVisitor + crate::runtime::tape::StringVisitor
            + crate::runtime::tape::NumberVisitor,
    {
        let _ = state;
        match first_byte {
            _ => {
                Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                })
            }
        }
    }
    /// AW-V.W4-fix — visitor-path Flat-shape parse function.
    ///
    /// Mirrors the tape-path emitter structure. Literal positions
    /// byte-match without emitting a visitor event; Ref / Regex /
    /// Alt positions recurse through the visitor dispatcher.
    ///
    /// AX.W0a.2.f — compound; see tape-path comment for the
    /// `#[inline]` downgrade rationale.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_visitor_EbnfParser_concatenation<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        {
            let mut iter_count: u32 = 0;
            loop {
                let save_p = *p;
                let res = (|| -> ::core::result::Result<(), crate::runtime::ParseErr> {
                    {
                        let span_lo = *p;
                        let Some(match_len) = __regex_scan_EbnfParser(
                            "[ \\t\\n\\r\\f]*",
                            input,
                            *p,
                        ) else {
                            return ::core::result::Result::Err(crate::runtime::ParseErr::Syntax {
                                offset: span_lo as u32,
                                rule: None,
                            });
                        };
                        *p = span_lo + match_len as usize;
                    }
                    ({
                        let _ = __shape_support_EbnfParser::skip_space(input, p, state);
                        parse_flat_visitor_EbnfParser_factor(input, p, state, visitor)
                    })?;
                    {
                        let span_lo = *p;
                        let Some(match_len) = __regex_scan_EbnfParser(
                            "[ \\t\\n\\r\\f]*",
                            input,
                            *p,
                        ) else {
                            return ::core::result::Result::Err(crate::runtime::ParseErr::Syntax {
                                offset: span_lo as u32,
                                rule: None,
                            });
                        };
                        *p = span_lo + match_len as usize;
                    }
                    let save_p = *p;
                    let res = (|| -> ::core::result::Result<
                        (),
                        crate::runtime::ParseErr,
                    > {
                        let at = *p;
                        let end = at + 1usize;
                        if input.len() < end || input[at..end] != [44u8] {
                            return Err(crate::runtime::ParseErr::Syntax {
                                offset: at as u32,
                                rule: None,
                            });
                        }
                        *p = end;
                        Ok(())
                    })();
                    if res.is_err() {
                        *p = save_p;
                    }
                    Ok(())
                })();
                if res.is_err() {
                    *p = save_p;
                    break;
                }
                if *p == save_p {
                    break;
                }
                iter_count = iter_count.saturating_add(1);
            }
            if iter_count < (1usize as u32) {
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                });
            }
        }
        Ok(())
    }
    /// AW-V.W4-fix — visitor-path Flat-shape parse function.
    ///
    /// Mirrors the tape-path emitter structure. Literal positions
    /// byte-match without emitting a visitor event; Ref / Regex /
    /// Alt positions recurse through the visitor dispatcher.
    ///
    /// AX.W0a.2.f — compound; see tape-path comment for the
    /// `#[inline]` downgrade rationale.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_visitor_EbnfParser_alternation<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        {
            let mut iter_count: u32 = 0;
            loop {
                let save_p = *p;
                let res = (|| -> ::core::result::Result<(), crate::runtime::ParseErr> {
                    {
                        let span_lo = *p;
                        let Some(match_len) = __regex_scan_EbnfParser(
                            "[ \\t\\n\\r\\f]*",
                            input,
                            *p,
                        ) else {
                            return ::core::result::Result::Err(crate::runtime::ParseErr::Syntax {
                                offset: span_lo as u32,
                                rule: None,
                            });
                        };
                        *p = span_lo + match_len as usize;
                    }
                    ({
                        let _ = __shape_support_EbnfParser::skip_space(input, p, state);
                        parse_flat_visitor_EbnfParser_concatenation(
                            input,
                            p,
                            state,
                            visitor,
                        )
                    })?;
                    {
                        let span_lo = *p;
                        let Some(match_len) = __regex_scan_EbnfParser(
                            "[ \\t\\n\\r\\f]*",
                            input,
                            *p,
                        ) else {
                            return ::core::result::Result::Err(crate::runtime::ParseErr::Syntax {
                                offset: span_lo as u32,
                                rule: None,
                            });
                        };
                        *p = span_lo + match_len as usize;
                    }
                    let save_p = *p;
                    let res = (|| -> ::core::result::Result<
                        (),
                        crate::runtime::ParseErr,
                    > {
                        let at = *p;
                        let end = at + 1usize;
                        if input.len() < end || input[at..end] != [124u8] {
                            return Err(crate::runtime::ParseErr::Syntax {
                                offset: at as u32,
                                rule: None,
                            });
                        }
                        *p = end;
                        Ok(())
                    })();
                    if res.is_err() {
                        *p = save_p;
                    }
                    Ok(())
                })();
                if res.is_err() {
                    *p = save_p;
                    break;
                }
                if *p == save_p {
                    break;
                }
                iter_count = iter_count.saturating_add(1);
            }
            if iter_count < (1usize as u32) {
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                });
            }
        }
        Ok(())
    }
    /// AX.W0a.2.b — visitor-path Scalar-shape parse function
    /// (transparent-Ref body).
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_scalar_visitor_EbnfParser_rhs<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        {
            let _ = __shape_support_EbnfParser::skip_space(input, p, state);
            parse_flat_visitor_EbnfParser_alternation(input, p, state, visitor)
        }
            .map(|_| ())
    }
    /// AX.W0a.2.b — visitor-path AltDispatch-shape parse function.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_variables,
        unused_mut,
        unused_assignments,
        unreachable_code
    )]
    pub fn parse_altdispatch_visitor_EbnfParser_term<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        let first = __shape_support_EbnfParser::skip_space(input, p, state)
            .ok_or(crate::runtime::ParseErr::Syntax {
                offset: *p as u32,
                rule: None,
            })?;
        'try_branches: loop {
            match first {
                34u8 => {
                    let attempt_p = *p;
                    match {
                        let __first = __shape_support_EbnfParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(crate::runtime::ParseErr::Syntax {
                                offset: *p as u32,
                                rule: None,
                            })?;
                        parse_keyword_visitor_EbnfParser_terminal(
                            input,
                            p,
                            __first,
                            state,
                            visitor,
                        )
                    } {
                        Ok(_) => break 'try_branches,
                        Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                39u8 => {
                    let attempt_p = *p;
                    match {
                        let __first = __shape_support_EbnfParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(crate::runtime::ParseErr::Syntax {
                                offset: *p as u32,
                                rule: None,
                            })?;
                        parse_keyword_visitor_EbnfParser_terminal(
                            input,
                            p,
                            __first,
                            state,
                            visitor,
                        )
                    } {
                        Ok(_) => break 'try_branches,
                        Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                40u8 => {
                    let save_p = *p;
                    let attempt = (|| -> ::core::result::Result<(), ()> {
                        let at = *p;
                        let end = at + 1usize;
                        if input.len() < end || input[at..end] != [40u8] {
                            return Err(());
                        }
                        *p = end;
                        let at = *p;
                        let mut q = at;
                        while q < input.len() {
                            let b = input[q];
                            if b.is_ascii_alphanumeric() || b == b'_' {
                                q += 1;
                            } else {
                                break;
                            }
                        }
                        if q == at {
                            return Err(());
                        }
                        *p = q;
                        return Err(());
                        let at = *p;
                        let mut q = at;
                        while q < input.len() {
                            let b = input[q];
                            if b.is_ascii_alphanumeric() || b == b'_' {
                                q += 1;
                            } else {
                                break;
                            }
                        }
                        if q == at {
                            return Err(());
                        }
                        *p = q;
                        let at = *p;
                        let end = at + 1usize;
                        if input.len() < end || input[at..end] != [41u8] {
                            return Err(());
                        }
                        *p = end;
                        Ok(())
                    })();
                    match attempt {
                        Ok(_) => break 'try_branches,
                        Err(_) => {
                            *p = save_p;
                        }
                    }
                }
                91u8 => {
                    let save_p = *p;
                    let attempt = (|| -> ::core::result::Result<(), ()> {
                        let at = *p;
                        let end = at + 1usize;
                        if input.len() < end || input[at..end] != [91u8] {
                            return Err(());
                        }
                        *p = end;
                        let at = *p;
                        let mut q = at;
                        while q < input.len() {
                            let b = input[q];
                            if b.is_ascii_alphanumeric() || b == b'_' {
                                q += 1;
                            } else {
                                break;
                            }
                        }
                        if q == at {
                            return Err(());
                        }
                        *p = q;
                        return Err(());
                        let at = *p;
                        let mut q = at;
                        while q < input.len() {
                            let b = input[q];
                            if b.is_ascii_alphanumeric() || b == b'_' {
                                q += 1;
                            } else {
                                break;
                            }
                        }
                        if q == at {
                            return Err(());
                        }
                        *p = q;
                        let at = *p;
                        let end = at + 1usize;
                        if input.len() < end || input[at..end] != [93u8] {
                            return Err(());
                        }
                        *p = end;
                        Ok(())
                    })();
                    match attempt {
                        Ok(_) => break 'try_branches,
                        Err(_) => {
                            *p = save_p;
                        }
                    }
                }
                123u8 => {
                    let save_p = *p;
                    let attempt = (|| -> ::core::result::Result<(), ()> {
                        let at = *p;
                        let end = at + 1usize;
                        if input.len() < end || input[at..end] != [123u8] {
                            return Err(());
                        }
                        *p = end;
                        let at = *p;
                        let mut q = at;
                        while q < input.len() {
                            let b = input[q];
                            if b.is_ascii_alphanumeric() || b == b'_' {
                                q += 1;
                            } else {
                                break;
                            }
                        }
                        if q == at {
                            return Err(());
                        }
                        *p = q;
                        return Err(());
                        let at = *p;
                        let mut q = at;
                        while q < input.len() {
                            let b = input[q];
                            if b.is_ascii_alphanumeric() || b == b'_' {
                                q += 1;
                            } else {
                                break;
                            }
                        }
                        if q == at {
                            return Err(());
                        }
                        *p = q;
                        let at = *p;
                        let end = at + 1usize;
                        if input.len() < end || input[at..end] != [125u8] {
                            return Err(());
                        }
                        *p = end;
                        Ok(())
                    })();
                    match attempt {
                        Ok(_) => break 'try_branches,
                        Err(_) => {
                            *p = save_p;
                        }
                    }
                }
                _ => {}
            }
            {
                let attempt_p = *p;
                match {
                    let _ = __shape_support_EbnfParser::skip_space(input, p, state);
                    parse_flat_visitor_EbnfParser_identifier(input, p, state, visitor)
                } {
                    Ok(_) => break 'try_branches,
                    Err(_) => {
                        *p = attempt_p;
                    }
                }
            }
            return Err(crate::runtime::ParseErr::Syntax {
                offset: *p as u32,
                rule: None,
            });
        }
        Ok(())
    }
    /// AW-V.W4-fix — visitor-path Flat-shape parse function.
    ///
    /// Mirrors the tape-path emitter structure. Literal positions
    /// byte-match without emitting a visitor event; Ref / Regex /
    /// Alt positions recurse through the visitor dispatcher.
    ///
    /// AX.W0a.2.f — compound; see tape-path comment for the
    /// `#[inline]` downgrade rationale.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_visitor_EbnfParser_factor<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        {
            ({
                let _ = __shape_support_EbnfParser::skip_space(input, p, state);
                parse_altdispatch_visitor_EbnfParser_term(input, p, state, visitor)
            })?;
        }
        {
            {
                let span_lo = *p;
                let Some(match_len) = __regex_scan_EbnfParser(
                    "[ \\t\\n\\r\\f]*",
                    input,
                    *p,
                ) else {
                    return ::core::result::Result::Err(crate::runtime::ParseErr::Syntax {
                        offset: span_lo as u32,
                        rule: None,
                    });
                };
                *p = span_lo + match_len as usize;
            }
        }
        {
            {
                let first = __shape_support_EbnfParser::skip_space(input, p, state)
                    .ok_or(crate::runtime::ParseErr::Syntax {
                        offset: *p as u32,
                        rule: None,
                    })?;
                'try_branches: loop {
                    match first {
                        42u8 => {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() >= end && input[at..end] == [42u8] {
                                *p = end;
                                break 'try_branches;
                            }
                        }
                        43u8 => {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() >= end && input[at..end] == [43u8] {
                                *p = end;
                                break 'try_branches;
                            }
                        }
                        45u8 => {
                            let save_p = *p;
                            let attempt = (|| -> ::core::result::Result<(), ()> {
                                let at = *p;
                                let end = at + 1usize;
                                if input.len() < end || input[at..end] != [45u8] {
                                    return Err(());
                                }
                                *p = end;
                                let at = *p;
                                let mut q = at;
                                while q < input.len() {
                                    let b = input[q];
                                    if b.is_ascii_alphanumeric() || b == b'_' {
                                        q += 1;
                                    } else {
                                        break;
                                    }
                                }
                                if q == at {
                                    return Err(());
                                }
                                *p = q;
                                return Err(());
                                Ok(())
                            })();
                            match attempt {
                                Ok(_) => break 'try_branches,
                                Err(_) => {
                                    *p = save_p;
                                }
                            }
                        }
                        63u8 => {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() >= end && input[at..end] == [63u8] {
                                *p = end;
                                break 'try_branches;
                            }
                        }
                        _ => {}
                    }
                    return ::core::result::Result::Err(crate::runtime::ParseErr::Syntax {
                        offset: *p as u32,
                        rule: None,
                    });
                }
            }
        }
        Ok(())
    }
    /// AW-V.W4-fix — visitor-path Flat-shape parse function.
    ///
    /// Mirrors the tape-path emitter structure. Literal positions
    /// byte-match without emitting a visitor event; Ref / Regex /
    /// Alt positions recurse through the visitor dispatcher.
    ///
    /// AX.W0a.2.f — compound; see tape-path comment for the
    /// `#[inline]` downgrade rationale.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_visitor_EbnfParser_rule<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        {
            ({
                let _ = __shape_support_EbnfParser::skip_space(input, p, state);
                parse_flat_visitor_EbnfParser_identifier(input, p, state, visitor)
            })?;
        }
        {
            {
                let span_lo = *p;
                let Some(match_len) = __regex_scan_EbnfParser(
                    "[ \\t\\n\\r\\f]*",
                    input,
                    *p,
                ) else {
                    return ::core::result::Result::Err(crate::runtime::ParseErr::Syntax {
                        offset: span_lo as u32,
                        rule: None,
                    });
                };
                *p = span_lo + match_len as usize;
            }
        }
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [61u8] {
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: at as u32,
                    rule: None,
                });
            }
            *p = end;
        }
        {
            {
                let span_lo = *p;
                let Some(match_len) = __regex_scan_EbnfParser(
                    "[ \\t\\n\\r\\f]*",
                    input,
                    *p,
                ) else {
                    return ::core::result::Result::Err(crate::runtime::ParseErr::Syntax {
                        offset: span_lo as u32,
                        rule: None,
                    });
                };
                *p = span_lo + match_len as usize;
            }
        }
        {
            ({
                let _ = __shape_support_EbnfParser::skip_space(input, p, state);
                parse_scalar_visitor_EbnfParser_rhs(input, p, state, visitor)
            })?;
        }
        {
            {
                let span_lo = *p;
                let Some(match_len) = __regex_scan_EbnfParser(
                    "[ \\t\\n\\r\\f]*",
                    input,
                    *p,
                ) else {
                    return ::core::result::Result::Err(crate::runtime::ParseErr::Syntax {
                        offset: span_lo as u32,
                        rule: None,
                    });
                };
                *p = span_lo + match_len as usize;
            }
        }
        {
            {
                let first = __shape_support_EbnfParser::skip_space(input, p, state)
                    .ok_or(crate::runtime::ParseErr::Syntax {
                        offset: *p as u32,
                        rule: None,
                    })?;
                'try_branches: loop {
                    match first {
                        46u8 => {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() >= end && input[at..end] == [46u8] {
                                *p = end;
                                break 'try_branches;
                            }
                        }
                        59u8 => {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() >= end && input[at..end] == [59u8] {
                                *p = end;
                                break 'try_branches;
                            }
                        }
                        _ => {}
                    }
                    return ::core::result::Result::Err(crate::runtime::ParseErr::Syntax {
                        offset: *p as u32,
                        rule: None,
                    });
                }
            }
        }
        Ok(())
    }
    /// AW-V.W3-bench-fix — visitor-path Array-shape parse function.
    ///
    /// Mirrors `json_prototype::parse_array::<V>`. Bypasses
    /// the tape entirely.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_array_visitor_EbnfParser_grammar<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        let begin_at = *p;
        if input.get(*p).copied() != Some(b'[') {
            return Err(crate::runtime::ParseErr::Syntax {
                offset: begin_at as u32,
                rule: None,
            });
        }
        *p += 1;
        visitor
            .begin_array()
            .map_err(|_| crate::runtime::ParseErr::Syntax {
                offset: begin_at as u32,
                rule: None,
            })?;
        if let Some(b) = __shape_support_EbnfParser::skip_space(input, p, state) {
            if b == b']' {
                *p += 1;
                return visitor
                    .end_array()
                    .map_err(|_| crate::runtime::ParseErr::Syntax {
                        offset: *p as u32,
                        rule: None,
                    });
            }
        } else {
            return Err(crate::runtime::ParseErr::Syntax {
                offset: *p as u32,
                rule: None,
            });
        }
        loop {
            ({
                let _ = __shape_support_EbnfParser::skip_space(input, p, state);
                parse_flat_visitor_EbnfParser_rule(input, p, state, visitor)
            })?;
            match __shape_support_EbnfParser::skip_space(input, p, state) {
                Some(b']') => {
                    *p += 1;
                    return visitor
                        .end_array()
                        .map_err(|_| crate::runtime::ParseErr::Syntax {
                            offset: *p as u32,
                            rule: None,
                        });
                }
                Some(b',') => {
                    *p += 1;
                    let _ = __shape_support_EbnfParser::skip_space(input, p, state);
                }
                _ => {
                    return Err(crate::runtime::ParseErr::Syntax {
                        offset: *p as u32,
                        rule: None,
                    });
                }
            }
        }
    }
    /// AY-II.W0.e — Grammar-activated structural-scan policy table.
    ///
    /// One entry per non-transparent rule, derived at codegen from
    /// CSP-inferred FIRST-set facts intersected with the grammar's
    /// mined `structural_alphabet` + `structural_digraph_mask`.
    /// Consumed at emission time by `emit_path_query_impls` in
    /// `backend::rust::view::value`, which inlines the matching
    /// cursor primitive in `__path_walk`'s per-`rule_kind()`
    /// dispatch:
    /// [`crate::runtime::tape::TapeCursor::object_key_seek`] /
    /// [`crate::runtime::tape::TapeCursor::bounded_lookahead`] /
    /// [`crate::runtime::tape::TapeCursor::scan_structural_bounded`]
    /// per the entry's `activation` bitmap.
    ///
    /// No runtime flag; no hand-routed grammar specialisation.
    /// AY-II.W0'.c retires the `#[allow(dead_code)]` that
    /// previously guarded this surface — the emitted grammar now
    /// carries a same-translation-unit consumer through
    /// `__path_walk`'s dispatch.
    pub const STRUCTURAL_SCAN_POLICY: &[crate::runtime::tape::ScanPolicyEntry] = &[
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 0u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Dense,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(7),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 1u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Dense,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(7),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 2u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Dense,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(7),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 3u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Dense,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(7),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 4u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Dense,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(7),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 5u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Sparse,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(2),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 6u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Dense,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(7),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 7u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Dense,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(7),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 8u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Dense,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 9u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Dense,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(7),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 10u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Dense,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(7),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 11u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Dense,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(7),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 12u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Dense,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(7),
        },
    ];
    /// AW-V.W3.2 — top-level shape dispatcher.
    ///
    /// Mirrors the walker's `value` rule ByteDispatch: skip leading
    /// whitespace, dispatch on the first byte to the chosen branch
    /// shape fn, return its `TapeOffset` unchanged. No outer Rule /
    /// Alt compound is pushed — the DTA's ByteDispatch state for
    /// `value` emits no compound either, and the target rule's Ref
    /// overwrites any `pending_variant_idx` en route, so the chosen
    /// rule's own compound carries the final root variant.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
    /// recursion rationale.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_EbnfParser_grammar<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        parse_EbnfParser_grammar__value(input, p, state, builder)
    }
    /// AW-V.W3.2 — value-position shape dispatcher. Called both at
    /// the grammar root and from Object / Array compound bodies.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_EbnfParser_grammar__value<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let _ = __shape_support_EbnfParser::skip_space(input, p, state);
        parse_array_EbnfParser_grammar(input, p, state, builder)
    }
    /// AW-V.W3-bench-fix — top-level visitor-path dispatcher.
    ///
    /// Generic over the visitor type; `V: JsonVisitor` composes all
    /// per-shape sub-trait bounds (`ObjectVisitor`, `ArrayVisitor`,
    /// `StringVisitor`, `NumberVisitor`, `KeywordVisitor`) so every
    /// per-shape method invocation resolves statically at the
    /// monomorphisation site. Bypasses the tape entirely.
    ///
    /// The dispatcher's bounds are narrow by design: emitted only
    /// for grammars whose classified rules use W3-pure shapes
    /// (Object / Array / String / Number / Keyword / Scalar).
    /// Grammars carrying W4-classified rules (Pratt / Unordered /
    /// ArgList / Flat / Wrap / HRegex) skip visitor dispatcher
    /// emission entirely — the tape-path dispatcher still emits,
    /// the per-shape fns still compile, but the generic `V`
    /// visitor bound can't union W4 visitor sub-traits without
    /// rippling into callers that don't have those bounds. Visitor
    /// activation for W4-carrying grammars lands in a follow-on
    /// wave alongside the per-Ref `__value` dispatcher refactor.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_EbnfParser_grammar_visitor<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        parse_EbnfParser_grammar_visitor__value(input, p, state, visitor)
    }
    /// AW-V.W3-bench-fix — value-position visitor-path dispatcher.
    /// Called both at the grammar root and from the object / array
    /// shape fns' value-position recursion.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_EbnfParser_grammar_visitor__value<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        let _ = __shape_support_EbnfParser::skip_space(input, p, state);
        parse_array_visitor_EbnfParser_grammar(input, p, state, visitor)
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct letterView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> letterView<'p> {
        #[inline]
        pub fn new(
            tape: &'p crate::runtime::tape::Tape,
            input: &'p str,
            offset: crate::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: crate::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: crate::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> crate::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> crate::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> EbnfParserRuleKind {
            match self.variant_idx() {
                0u8 => EbnfParserRuleKind::letter,
                1u8 => EbnfParserRuleKind::digit,
                2u8 => EbnfParserRuleKind::symbol,
                3u8 => EbnfParserRuleKind::identifier,
                4u8 => EbnfParserRuleKind::character,
                5u8 => EbnfParserRuleKind::terminal,
                6u8 => EbnfParserRuleKind::concatenation,
                7u8 => EbnfParserRuleKind::alternation,
                8u8 => EbnfParserRuleKind::rhs,
                9u8 => EbnfParserRuleKind::term,
                10u8 => EbnfParserRuleKind::factor,
                11u8 => EbnfParserRuleKind::rule,
                12u8 => EbnfParserRuleKind::grammar,
                13u8 => EbnfParserRuleKind::term_0,
                14u8 => EbnfParserRuleKind::term_1,
                15u8 => EbnfParserRuleKind::factor_0,
                16u8 => EbnfParserRuleKind::factor_1,
                _ => EbnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = EbnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| EbnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> letterView<'p> {
        /// The source text matched by this rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// The packed scalar fields decoded from the tape's
        /// aggregate payload buffer.
        ///
        /// Returns the layout-zeroed tuple if no payload was
        /// written for this record (e.g. an alternative branch
        /// path that never set any fields).
        #[inline]
        pub fn value(&self) -> ((u32, u32)) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 8usize) {
                Some(__bytes) => {
                    ({
                        let __raw = u64::from_le_bytes(
                            <[u8; 8]>::try_from(&__bytes[0usize..8usize])
                                .expect("aggregate slice is 8 bytes"),
                        );
                        (__raw as u32, (__raw >> 32) as u32)
                    })
                }
                None => ((0_u32, 0_u32)),
            }
        }
        /// The matched byte range as a `Range<usize>`, suitable
        /// for slicing the input string directly.
        #[inline]
        pub fn byte_range(&self) -> ::core::ops::Range<usize> {
            let (lo, hi) = self.span();
            lo as usize..hi as usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct digitView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> digitView<'p> {
        #[inline]
        pub fn new(
            tape: &'p crate::runtime::tape::Tape,
            input: &'p str,
            offset: crate::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: crate::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: crate::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> crate::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> crate::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> EbnfParserRuleKind {
            match self.variant_idx() {
                0u8 => EbnfParserRuleKind::letter,
                1u8 => EbnfParserRuleKind::digit,
                2u8 => EbnfParserRuleKind::symbol,
                3u8 => EbnfParserRuleKind::identifier,
                4u8 => EbnfParserRuleKind::character,
                5u8 => EbnfParserRuleKind::terminal,
                6u8 => EbnfParserRuleKind::concatenation,
                7u8 => EbnfParserRuleKind::alternation,
                8u8 => EbnfParserRuleKind::rhs,
                9u8 => EbnfParserRuleKind::term,
                10u8 => EbnfParserRuleKind::factor,
                11u8 => EbnfParserRuleKind::rule,
                12u8 => EbnfParserRuleKind::grammar,
                13u8 => EbnfParserRuleKind::term_0,
                14u8 => EbnfParserRuleKind::term_1,
                15u8 => EbnfParserRuleKind::factor_0,
                16u8 => EbnfParserRuleKind::factor_1,
                _ => EbnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = EbnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| EbnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> digitView<'p> {
        /// The source text matched by this rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// The packed scalar fields decoded from the tape's
        /// aggregate payload buffer.
        ///
        /// Returns the layout-zeroed tuple if no payload was
        /// written for this record (e.g. an alternative branch
        /// path that never set any fields).
        #[inline]
        pub fn value(&self) -> ((u32, u32)) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 8usize) {
                Some(__bytes) => {
                    ({
                        let __raw = u64::from_le_bytes(
                            <[u8; 8]>::try_from(&__bytes[0usize..8usize])
                                .expect("aggregate slice is 8 bytes"),
                        );
                        (__raw as u32, (__raw >> 32) as u32)
                    })
                }
                None => ((0_u32, 0_u32)),
            }
        }
        /// The matched byte range as a `Range<usize>`, suitable
        /// for slicing the input string directly.
        #[inline]
        pub fn byte_range(&self) -> ::core::ops::Range<usize> {
            let (lo, hi) = self.span();
            lo as usize..hi as usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct symbolView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> symbolView<'p> {
        #[inline]
        pub fn new(
            tape: &'p crate::runtime::tape::Tape,
            input: &'p str,
            offset: crate::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: crate::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: crate::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> crate::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> crate::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> EbnfParserRuleKind {
            match self.variant_idx() {
                0u8 => EbnfParserRuleKind::letter,
                1u8 => EbnfParserRuleKind::digit,
                2u8 => EbnfParserRuleKind::symbol,
                3u8 => EbnfParserRuleKind::identifier,
                4u8 => EbnfParserRuleKind::character,
                5u8 => EbnfParserRuleKind::terminal,
                6u8 => EbnfParserRuleKind::concatenation,
                7u8 => EbnfParserRuleKind::alternation,
                8u8 => EbnfParserRuleKind::rhs,
                9u8 => EbnfParserRuleKind::term,
                10u8 => EbnfParserRuleKind::factor,
                11u8 => EbnfParserRuleKind::rule,
                12u8 => EbnfParserRuleKind::grammar,
                13u8 => EbnfParserRuleKind::term_0,
                14u8 => EbnfParserRuleKind::term_1,
                15u8 => EbnfParserRuleKind::factor_0,
                16u8 => EbnfParserRuleKind::factor_1,
                _ => EbnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = EbnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| EbnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> symbolView<'p> {
        /// The source text matched by this rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// The packed scalar fields decoded from the tape's
        /// aggregate payload buffer.
        ///
        /// Returns the layout-zeroed tuple if no payload was
        /// written for this record (e.g. an alternative branch
        /// path that never set any fields).
        #[inline]
        pub fn value(&self) -> ((u32, u32)) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 8usize) {
                Some(__bytes) => {
                    ({
                        let __raw = u64::from_le_bytes(
                            <[u8; 8]>::try_from(&__bytes[0usize..8usize])
                                .expect("aggregate slice is 8 bytes"),
                        );
                        (__raw as u32, (__raw >> 32) as u32)
                    })
                }
                None => ((0_u32, 0_u32)),
            }
        }
        /// The matched byte range as a `Range<usize>`, suitable
        /// for slicing the input string directly.
        #[inline]
        pub fn byte_range(&self) -> ::core::ops::Range<usize> {
            let (lo, hi) = self.span();
            lo as usize..hi as usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct identifierView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> identifierView<'p> {
        #[inline]
        pub fn new(
            tape: &'p crate::runtime::tape::Tape,
            input: &'p str,
            offset: crate::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: crate::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: crate::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> crate::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> crate::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> EbnfParserRuleKind {
            match self.variant_idx() {
                0u8 => EbnfParserRuleKind::letter,
                1u8 => EbnfParserRuleKind::digit,
                2u8 => EbnfParserRuleKind::symbol,
                3u8 => EbnfParserRuleKind::identifier,
                4u8 => EbnfParserRuleKind::character,
                5u8 => EbnfParserRuleKind::terminal,
                6u8 => EbnfParserRuleKind::concatenation,
                7u8 => EbnfParserRuleKind::alternation,
                8u8 => EbnfParserRuleKind::rhs,
                9u8 => EbnfParserRuleKind::term,
                10u8 => EbnfParserRuleKind::factor,
                11u8 => EbnfParserRuleKind::rule,
                12u8 => EbnfParserRuleKind::grammar,
                13u8 => EbnfParserRuleKind::term_0,
                14u8 => EbnfParserRuleKind::term_1,
                15u8 => EbnfParserRuleKind::factor_0,
                16u8 => EbnfParserRuleKind::factor_1,
                _ => EbnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = EbnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| EbnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> identifierView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<letterView<'p>> {
            self.cursor.child(0usize).map(|c| letterView::from_cursor(c, self.input))
        }
        ///The `letter` child as a typed view.
        #[inline]
        pub fn letter(&self) -> ::core::option::Option<letterView<'p>> {
            self.cursor.child(0usize).map(|c| letterView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct characterView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> characterView<'p> {
        #[inline]
        pub fn new(
            tape: &'p crate::runtime::tape::Tape,
            input: &'p str,
            offset: crate::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: crate::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: crate::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> crate::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> crate::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> EbnfParserRuleKind {
            match self.variant_idx() {
                0u8 => EbnfParserRuleKind::letter,
                1u8 => EbnfParserRuleKind::digit,
                2u8 => EbnfParserRuleKind::symbol,
                3u8 => EbnfParserRuleKind::identifier,
                4u8 => EbnfParserRuleKind::character,
                5u8 => EbnfParserRuleKind::terminal,
                6u8 => EbnfParserRuleKind::concatenation,
                7u8 => EbnfParserRuleKind::alternation,
                8u8 => EbnfParserRuleKind::rhs,
                9u8 => EbnfParserRuleKind::term,
                10u8 => EbnfParserRuleKind::factor,
                11u8 => EbnfParserRuleKind::rule,
                12u8 => EbnfParserRuleKind::grammar,
                13u8 => EbnfParserRuleKind::term_0,
                14u8 => EbnfParserRuleKind::term_1,
                15u8 => EbnfParserRuleKind::factor_0,
                16u8 => EbnfParserRuleKind::factor_1,
                _ => EbnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = EbnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| EbnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> characterView<'p> {
        ///If variant `branch_0` (branch 0) was chosen, return its child view.
        #[inline]
        pub fn as_branch_0(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor
                    .child(0)
                    .map(|c| EbnfParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `branch_0` (branch 0) was chosen.
        #[inline]
        pub fn is_branch_0(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If variant `branch_1` (branch 1) was chosen, return its child view.
        #[inline]
        pub fn as_branch_1(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor
                    .child(0)
                    .map(|c| EbnfParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `branch_1` (branch 1) was chosen.
        #[inline]
        pub fn is_branch_1(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        ///If variant `digit` (branch 2) was chosen, return its child view.
        #[inline]
        pub fn as_digit(&self) -> ::core::option::Option<digitView<'p>> {
            if self.cursor.meta_idx() == 2u8 {
                self.cursor.child(0).map(|c| digitView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `digit` (branch 2) was chosen.
        #[inline]
        pub fn is_digit(&self) -> bool {
            self.cursor.meta_idx() == 2u8
        }
        ///If variant `symbol` (branch 3) was chosen, return its child view.
        #[inline]
        pub fn as_symbol(&self) -> ::core::option::Option<symbolView<'p>> {
            if self.cursor.meta_idx() == 3u8 {
                self.cursor.child(0).map(|c| symbolView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `symbol` (branch 3) was chosen.
        #[inline]
        pub fn is_symbol(&self) -> bool {
            self.cursor.meta_idx() == 3u8
        }
        ///If variant `letter` (branch 4) was chosen, return its child view.
        #[inline]
        pub fn as_letter(&self) -> ::core::option::Option<letterView<'p>> {
            if self.cursor.meta_idx() == 4u8 {
                self.cursor.child(0).map(|c| letterView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `letter` (branch 4) was chosen.
        #[inline]
        pub fn is_letter(&self) -> bool {
            self.cursor.meta_idx() == 4u8
        }
        /// The chosen branch's child as a generic node view,
        /// regardless of which variant was selected.
        #[inline]
        pub fn chosen(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(0).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
    }
    /// Typed value enum — payload-eligible branches carry typed
    /// values directly; non-eligible branches wrap a cursor view.
    #[derive(Clone, Debug)]
    pub enum characterValue<'p> {
        branch_0(EbnfParserNodeView<'p>),
        branch_1(EbnfParserNodeView<'p>),
        digit(((u32, u32))),
        symbol(((u32, u32))),
        letter(((u32, u32))),
    }
    impl<'p> characterView<'p> {
        /// Decode the chosen branch's value. Payload-eligible
        /// branches return typed scalars/aggregates; other
        /// branches return cursor-wrapped sub-views.
        #[inline]
        pub fn value(&self) -> ::core::option::Option<characterValue<'p>> {
            match self.cursor.meta_idx() {
                0u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        characterValue::branch_0(
                            EbnfParserNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                1u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        characterValue::branch_1(
                            EbnfParserNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                2u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_bytes(__rec, 8usize) {
                        Some(__bytes) => {
                            ({
                                let __raw = u64::from_le_bytes(
                                    <[u8; 8]>::try_from(&__bytes[0usize..8usize]).unwrap(),
                                );
                                (__raw as u32, (__raw >> 32) as u32)
                            })
                        }
                        None => ((0_u32, 0_u32)),
                    };
                    Some(characterValue::digit(__value))
                }
                3u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_bytes(__rec, 8usize) {
                        Some(__bytes) => {
                            ({
                                let __raw = u64::from_le_bytes(
                                    <[u8; 8]>::try_from(&__bytes[0usize..8usize]).unwrap(),
                                );
                                (__raw as u32, (__raw >> 32) as u32)
                            })
                        }
                        None => ((0_u32, 0_u32)),
                    };
                    Some(characterValue::symbol(__value))
                }
                4u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_bytes(__rec, 8usize) {
                        Some(__bytes) => {
                            ({
                                let __raw = u64::from_le_bytes(
                                    <[u8; 8]>::try_from(&__bytes[0usize..8usize]).unwrap(),
                                );
                                (__raw as u32, (__raw >> 32) as u32)
                            })
                        }
                        None => ((0_u32, 0_u32)),
                    };
                    Some(characterValue::letter(__value))
                }
                _ => None,
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct terminalView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> terminalView<'p> {
        #[inline]
        pub fn new(
            tape: &'p crate::runtime::tape::Tape,
            input: &'p str,
            offset: crate::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: crate::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: crate::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> crate::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> crate::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> EbnfParserRuleKind {
            match self.variant_idx() {
                0u8 => EbnfParserRuleKind::letter,
                1u8 => EbnfParserRuleKind::digit,
                2u8 => EbnfParserRuleKind::symbol,
                3u8 => EbnfParserRuleKind::identifier,
                4u8 => EbnfParserRuleKind::character,
                5u8 => EbnfParserRuleKind::terminal,
                6u8 => EbnfParserRuleKind::concatenation,
                7u8 => EbnfParserRuleKind::alternation,
                8u8 => EbnfParserRuleKind::rhs,
                9u8 => EbnfParserRuleKind::term,
                10u8 => EbnfParserRuleKind::factor,
                11u8 => EbnfParserRuleKind::rule,
                12u8 => EbnfParserRuleKind::grammar,
                13u8 => EbnfParserRuleKind::term_0,
                14u8 => EbnfParserRuleKind::term_1,
                15u8 => EbnfParserRuleKind::factor_0,
                16u8 => EbnfParserRuleKind::factor_1,
                _ => EbnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = EbnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| EbnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> terminalView<'p> {
        ///If variant `branch_0` (branch 0) was chosen, return its child view.
        #[inline]
        pub fn as_branch_0(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor
                    .child(0)
                    .map(|c| EbnfParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `branch_0` (branch 0) was chosen.
        #[inline]
        pub fn is_branch_0(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If variant `branch_1` (branch 1) was chosen, return its child view.
        #[inline]
        pub fn as_branch_1(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor
                    .child(0)
                    .map(|c| EbnfParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `branch_1` (branch 1) was chosen.
        #[inline]
        pub fn is_branch_1(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        /// The chosen branch's child as a generic node view,
        /// regardless of which variant was selected.
        #[inline]
        pub fn chosen(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(0).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct concatenationView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> concatenationView<'p> {
        #[inline]
        pub fn new(
            tape: &'p crate::runtime::tape::Tape,
            input: &'p str,
            offset: crate::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: crate::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: crate::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> crate::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> crate::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> EbnfParserRuleKind {
            match self.variant_idx() {
                0u8 => EbnfParserRuleKind::letter,
                1u8 => EbnfParserRuleKind::digit,
                2u8 => EbnfParserRuleKind::symbol,
                3u8 => EbnfParserRuleKind::identifier,
                4u8 => EbnfParserRuleKind::character,
                5u8 => EbnfParserRuleKind::terminal,
                6u8 => EbnfParserRuleKind::concatenation,
                7u8 => EbnfParserRuleKind::alternation,
                8u8 => EbnfParserRuleKind::rhs,
                9u8 => EbnfParserRuleKind::term,
                10u8 => EbnfParserRuleKind::factor,
                11u8 => EbnfParserRuleKind::rule,
                12u8 => EbnfParserRuleKind::grammar,
                13u8 => EbnfParserRuleKind::term_0,
                14u8 => EbnfParserRuleKind::term_1,
                15u8 => EbnfParserRuleKind::factor_0,
                16u8 => EbnfParserRuleKind::factor_1,
                _ => EbnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = EbnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| EbnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> concatenationView<'p> {
        /// Iterator over each repetition element as a typed view.
        #[inline]
        pub fn iter(
            &self,
        ) -> impl ::core::iter::Iterator<Item = EbnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| EbnfParserNodeView::from_cursor(c, input))
        }
        /// The number of elements in this repetition.
        #[inline]
        pub fn len(&self) -> usize {
            self.cursor.child_count()
        }
        /// Whether this repetition matched zero elements.
        #[inline]
        pub fn is_empty(&self) -> bool {
            self.len() == 0
        }
        /// The i-th element as a typed view, if present.
        #[inline]
        pub fn get(&self, i: usize) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct alternationView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> alternationView<'p> {
        #[inline]
        pub fn new(
            tape: &'p crate::runtime::tape::Tape,
            input: &'p str,
            offset: crate::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: crate::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: crate::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> crate::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> crate::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> EbnfParserRuleKind {
            match self.variant_idx() {
                0u8 => EbnfParserRuleKind::letter,
                1u8 => EbnfParserRuleKind::digit,
                2u8 => EbnfParserRuleKind::symbol,
                3u8 => EbnfParserRuleKind::identifier,
                4u8 => EbnfParserRuleKind::character,
                5u8 => EbnfParserRuleKind::terminal,
                6u8 => EbnfParserRuleKind::concatenation,
                7u8 => EbnfParserRuleKind::alternation,
                8u8 => EbnfParserRuleKind::rhs,
                9u8 => EbnfParserRuleKind::term,
                10u8 => EbnfParserRuleKind::factor,
                11u8 => EbnfParserRuleKind::rule,
                12u8 => EbnfParserRuleKind::grammar,
                13u8 => EbnfParserRuleKind::term_0,
                14u8 => EbnfParserRuleKind::term_1,
                15u8 => EbnfParserRuleKind::factor_0,
                16u8 => EbnfParserRuleKind::factor_1,
                _ => EbnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = EbnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| EbnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> alternationView<'p> {
        /// Iterator over each repetition element as a typed view.
        #[inline]
        pub fn iter(
            &self,
        ) -> impl ::core::iter::Iterator<Item = EbnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| EbnfParserNodeView::from_cursor(c, input))
        }
        /// The number of elements in this repetition.
        #[inline]
        pub fn len(&self) -> usize {
            self.cursor.child_count()
        }
        /// Whether this repetition matched zero elements.
        #[inline]
        pub fn is_empty(&self) -> bool {
            self.len() == 0
        }
        /// The i-th element as a typed view, if present.
        #[inline]
        pub fn get(&self, i: usize) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct rhsView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> rhsView<'p> {
        #[inline]
        pub fn new(
            tape: &'p crate::runtime::tape::Tape,
            input: &'p str,
            offset: crate::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: crate::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: crate::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> crate::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> crate::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> EbnfParserRuleKind {
            match self.variant_idx() {
                0u8 => EbnfParserRuleKind::letter,
                1u8 => EbnfParserRuleKind::digit,
                2u8 => EbnfParserRuleKind::symbol,
                3u8 => EbnfParserRuleKind::identifier,
                4u8 => EbnfParserRuleKind::character,
                5u8 => EbnfParserRuleKind::terminal,
                6u8 => EbnfParserRuleKind::concatenation,
                7u8 => EbnfParserRuleKind::alternation,
                8u8 => EbnfParserRuleKind::rhs,
                9u8 => EbnfParserRuleKind::term,
                10u8 => EbnfParserRuleKind::factor,
                11u8 => EbnfParserRuleKind::rule,
                12u8 => EbnfParserRuleKind::grammar,
                13u8 => EbnfParserRuleKind::term_0,
                14u8 => EbnfParserRuleKind::term_1,
                15u8 => EbnfParserRuleKind::factor_0,
                16u8 => EbnfParserRuleKind::factor_1,
                _ => EbnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = EbnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| EbnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> rhsView<'p> {
        /// The source text matched by this leaf rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct termView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> termView<'p> {
        #[inline]
        pub fn new(
            tape: &'p crate::runtime::tape::Tape,
            input: &'p str,
            offset: crate::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: crate::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: crate::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> crate::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> crate::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> EbnfParserRuleKind {
            match self.variant_idx() {
                0u8 => EbnfParserRuleKind::letter,
                1u8 => EbnfParserRuleKind::digit,
                2u8 => EbnfParserRuleKind::symbol,
                3u8 => EbnfParserRuleKind::identifier,
                4u8 => EbnfParserRuleKind::character,
                5u8 => EbnfParserRuleKind::terminal,
                6u8 => EbnfParserRuleKind::concatenation,
                7u8 => EbnfParserRuleKind::alternation,
                8u8 => EbnfParserRuleKind::rhs,
                9u8 => EbnfParserRuleKind::term,
                10u8 => EbnfParserRuleKind::factor,
                11u8 => EbnfParserRuleKind::rule,
                12u8 => EbnfParserRuleKind::grammar,
                13u8 => EbnfParserRuleKind::term_0,
                14u8 => EbnfParserRuleKind::term_1,
                15u8 => EbnfParserRuleKind::factor_0,
                16u8 => EbnfParserRuleKind::factor_1,
                _ => EbnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = EbnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| EbnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> termView<'p> {
        ///If variant `branch_0` (branch 0) was chosen, return its child view.
        #[inline]
        pub fn as_branch_0(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor
                    .child(0)
                    .map(|c| EbnfParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `branch_0` (branch 0) was chosen.
        #[inline]
        pub fn is_branch_0(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If variant `branch_1` (branch 1) was chosen, return its child view.
        #[inline]
        pub fn as_branch_1(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor
                    .child(0)
                    .map(|c| EbnfParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `branch_1` (branch 1) was chosen.
        #[inline]
        pub fn is_branch_1(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        ///If variant `branch_2` (branch 2) was chosen, return its child view.
        #[inline]
        pub fn as_branch_2(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            if self.cursor.meta_idx() == 2u8 {
                self.cursor
                    .child(0)
                    .map(|c| EbnfParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `branch_2` (branch 2) was chosen.
        #[inline]
        pub fn is_branch_2(&self) -> bool {
            self.cursor.meta_idx() == 2u8
        }
        ///If variant `terminal` (branch 3) was chosen, return its child view.
        #[inline]
        pub fn as_terminal(&self) -> ::core::option::Option<terminalView<'p>> {
            if self.cursor.meta_idx() == 3u8 {
                self.cursor.child(0).map(|c| terminalView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `terminal` (branch 3) was chosen.
        #[inline]
        pub fn is_terminal(&self) -> bool {
            self.cursor.meta_idx() == 3u8
        }
        ///If variant `identifier` (branch 4) was chosen, return its child view.
        #[inline]
        pub fn as_identifier(&self) -> ::core::option::Option<identifierView<'p>> {
            if self.cursor.meta_idx() == 4u8 {
                self.cursor.child(0).map(|c| identifierView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `identifier` (branch 4) was chosen.
        #[inline]
        pub fn is_identifier(&self) -> bool {
            self.cursor.meta_idx() == 4u8
        }
        ///If sub-variant `term_0` was chosen (branch 0), return its child view.
        #[inline]
        pub fn as_term_0(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor
                    .child(0)
                    .map(|c| EbnfParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_term_0(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If sub-variant `term_0_sv1` was chosen (branch 1), return its child view.
        #[inline]
        pub fn as_term_0_sv1(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor
                    .child(0)
                    .map(|c| EbnfParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_term_0_sv1(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        ///If sub-variant `term_0_sv2` was chosen (branch 2), return its child view.
        #[inline]
        pub fn as_term_0_sv2(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            if self.cursor.meta_idx() == 2u8 {
                self.cursor
                    .child(0)
                    .map(|c| EbnfParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_term_0_sv2(&self) -> bool {
            self.cursor.meta_idx() == 2u8
        }
        ///If sub-variant `term_1` was chosen (branch 4), return its child view.
        #[inline]
        pub fn as_term_1(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            if self.cursor.meta_idx() == 4u8 {
                self.cursor
                    .child(0)
                    .map(|c| EbnfParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_term_1(&self) -> bool {
            self.cursor.meta_idx() == 4u8
        }
        /// The chosen branch's child as a generic node view,
        /// regardless of which variant was selected.
        #[inline]
        pub fn chosen(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(0).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
    }
    /// Typed value enum — payload-eligible branches carry typed
    /// values directly; non-eligible branches wrap a cursor view.
    #[derive(Clone, Debug)]
    pub enum termValue<'p> {
        branch_0(EbnfParserNodeView<'p>),
        branch_1(EbnfParserNodeView<'p>),
        branch_2(EbnfParserNodeView<'p>),
        terminal(EbnfParserNodeView<'p>),
        identifier(&'p str),
    }
    impl<'p> termView<'p> {
        /// Decode the chosen branch's value. Payload-eligible
        /// branches return typed scalars/aggregates; other
        /// branches return cursor-wrapped sub-views.
        #[inline]
        pub fn value(&self) -> ::core::option::Option<termValue<'p>> {
            match self.cursor.meta_idx() {
                0u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        termValue::branch_0(
                            EbnfParserNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                1u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        termValue::branch_1(
                            EbnfParserNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                2u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        termValue::branch_2(
                            EbnfParserNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                3u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        termValue::terminal(
                            EbnfParserNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                4u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_Span(__rec) {
                        Some((lo, hi)) => &self.input[lo as usize..hi as usize],
                        None => {
                            let (lo, hi) = __cursor.span();
                            &self.input[lo as usize..hi as usize]
                        }
                    };
                    Some(termValue::identifier(__value))
                }
                _ => None,
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct factorView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> factorView<'p> {
        #[inline]
        pub fn new(
            tape: &'p crate::runtime::tape::Tape,
            input: &'p str,
            offset: crate::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: crate::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: crate::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> crate::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> crate::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> EbnfParserRuleKind {
            match self.variant_idx() {
                0u8 => EbnfParserRuleKind::letter,
                1u8 => EbnfParserRuleKind::digit,
                2u8 => EbnfParserRuleKind::symbol,
                3u8 => EbnfParserRuleKind::identifier,
                4u8 => EbnfParserRuleKind::character,
                5u8 => EbnfParserRuleKind::terminal,
                6u8 => EbnfParserRuleKind::concatenation,
                7u8 => EbnfParserRuleKind::alternation,
                8u8 => EbnfParserRuleKind::rhs,
                9u8 => EbnfParserRuleKind::term,
                10u8 => EbnfParserRuleKind::factor,
                11u8 => EbnfParserRuleKind::rule,
                12u8 => EbnfParserRuleKind::grammar,
                13u8 => EbnfParserRuleKind::term_0,
                14u8 => EbnfParserRuleKind::term_1,
                15u8 => EbnfParserRuleKind::factor_0,
                16u8 => EbnfParserRuleKind::factor_1,
                _ => EbnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = EbnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| EbnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> factorView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<termView<'p>> {
            self.cursor.child(0usize).map(|c| termView::from_cursor(c, self.input))
        }
        ///The `term` child as a typed view.
        #[inline]
        pub fn term(&self) -> ::core::option::Option<termView<'p>> {
            self.cursor.child(0usize).map(|c| termView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct ruleView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> ruleView<'p> {
        #[inline]
        pub fn new(
            tape: &'p crate::runtime::tape::Tape,
            input: &'p str,
            offset: crate::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: crate::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: crate::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> crate::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> crate::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> EbnfParserRuleKind {
            match self.variant_idx() {
                0u8 => EbnfParserRuleKind::letter,
                1u8 => EbnfParserRuleKind::digit,
                2u8 => EbnfParserRuleKind::symbol,
                3u8 => EbnfParserRuleKind::identifier,
                4u8 => EbnfParserRuleKind::character,
                5u8 => EbnfParserRuleKind::terminal,
                6u8 => EbnfParserRuleKind::concatenation,
                7u8 => EbnfParserRuleKind::alternation,
                8u8 => EbnfParserRuleKind::rhs,
                9u8 => EbnfParserRuleKind::term,
                10u8 => EbnfParserRuleKind::factor,
                11u8 => EbnfParserRuleKind::rule,
                12u8 => EbnfParserRuleKind::grammar,
                13u8 => EbnfParserRuleKind::term_0,
                14u8 => EbnfParserRuleKind::term_1,
                15u8 => EbnfParserRuleKind::factor_0,
                16u8 => EbnfParserRuleKind::factor_1,
                _ => EbnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = EbnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| EbnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> ruleView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<identifierView<'p>> {
            self.cursor.child(0usize).map(|c| identifierView::from_cursor(c, self.input))
        }
        ///The `identifier` child as a typed view.
        #[inline]
        pub fn identifier(&self) -> ::core::option::Option<identifierView<'p>> {
            self.cursor.child(0usize).map(|c| identifierView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 2 as a typed view.
        #[inline]
        pub fn child_2(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor
                .child(2usize)
                .map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 3 as a typed view.
        #[inline]
        pub fn child_3(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor
                .child(3usize)
                .map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 4 as a typed view.
        #[inline]
        pub fn child_4(&self) -> ::core::option::Option<rhsView<'p>> {
            self.cursor.child(4usize).map(|c| rhsView::from_cursor(c, self.input))
        }
        ///The `rhs` child as a typed view.
        #[inline]
        pub fn rhs(&self) -> ::core::option::Option<rhsView<'p>> {
            self.cursor.child(4usize).map(|c| rhsView::from_cursor(c, self.input))
        }
        ///Child at position 5 as a typed view.
        #[inline]
        pub fn child_5(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor
                .child(5usize)
                .map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 6 as a typed view.
        #[inline]
        pub fn child_6(&self) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor
                .child(6usize)
                .map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            7usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct grammarView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> grammarView<'p> {
        #[inline]
        pub fn new(
            tape: &'p crate::runtime::tape::Tape,
            input: &'p str,
            offset: crate::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: crate::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: crate::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> crate::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> crate::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> EbnfParserRuleKind {
            match self.variant_idx() {
                0u8 => EbnfParserRuleKind::letter,
                1u8 => EbnfParserRuleKind::digit,
                2u8 => EbnfParserRuleKind::symbol,
                3u8 => EbnfParserRuleKind::identifier,
                4u8 => EbnfParserRuleKind::character,
                5u8 => EbnfParserRuleKind::terminal,
                6u8 => EbnfParserRuleKind::concatenation,
                7u8 => EbnfParserRuleKind::alternation,
                8u8 => EbnfParserRuleKind::rhs,
                9u8 => EbnfParserRuleKind::term,
                10u8 => EbnfParserRuleKind::factor,
                11u8 => EbnfParserRuleKind::rule,
                12u8 => EbnfParserRuleKind::grammar,
                13u8 => EbnfParserRuleKind::term_0,
                14u8 => EbnfParserRuleKind::term_1,
                15u8 => EbnfParserRuleKind::factor_0,
                16u8 => EbnfParserRuleKind::factor_1,
                _ => EbnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = EbnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| EbnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> grammarView<'p> {
        /// Iterator over each repetition element as a typed view.
        #[inline]
        pub fn iter(
            &self,
        ) -> impl ::core::iter::Iterator<Item = EbnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| EbnfParserNodeView::from_cursor(c, input))
        }
        /// The number of elements in this repetition.
        #[inline]
        pub fn len(&self) -> usize {
            self.cursor.child_count()
        }
        /// Whether this repetition matched zero elements.
        #[inline]
        pub fn is_empty(&self) -> bool {
            self.len() == 0
        }
        /// The i-th element as a typed view, if present.
        #[inline]
        pub fn get(&self, i: usize) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
    }
    /// Generic node view over any tape record for this grammar.
    #[derive(Clone, Copy, Debug)]
    pub struct EbnfParserNodeView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    /// Rule-identity discriminator for `NodeView::rule_kind`
    /// and per-rule view `rule_kind` accessors. One variant
    /// per non-transparent rule (in declaration order),
    /// followed by one variant per distinct sub-variant name
    /// from heterogeneous alt coercion, plus a fallback
    /// `Unknown` for records the discriminator table does
    /// not cover (leaf spans, alt branch indices, etc.).
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum EbnfParserRuleKind {
        letter,
        digit,
        symbol,
        identifier,
        character,
        terminal,
        concatenation,
        alternation,
        rhs,
        term,
        factor,
        rule,
        grammar,
        term_0,
        term_1,
        factor_0,
        factor_1,
        /// Fallback for records whose variant_idx is not a
        /// known rule- or sub-variant discriminator.
        Unknown,
    }
    impl<'p> EbnfParserNodeView<'p> {
        #[inline]
        pub fn new(
            tape: &'p crate::runtime::tape::Tape,
            input: &'p str,
            offset: crate::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: crate::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: crate::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> crate::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> crate::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> EbnfParserRuleKind {
            match self.variant_idx() {
                0u8 => EbnfParserRuleKind::letter,
                1u8 => EbnfParserRuleKind::digit,
                2u8 => EbnfParserRuleKind::symbol,
                3u8 => EbnfParserRuleKind::identifier,
                4u8 => EbnfParserRuleKind::character,
                5u8 => EbnfParserRuleKind::terminal,
                6u8 => EbnfParserRuleKind::concatenation,
                7u8 => EbnfParserRuleKind::alternation,
                8u8 => EbnfParserRuleKind::rhs,
                9u8 => EbnfParserRuleKind::term,
                10u8 => EbnfParserRuleKind::factor,
                11u8 => EbnfParserRuleKind::rule,
                12u8 => EbnfParserRuleKind::grammar,
                13u8 => EbnfParserRuleKind::term_0,
                14u8 => EbnfParserRuleKind::term_1,
                15u8 => EbnfParserRuleKind::factor_0,
                16u8 => EbnfParserRuleKind::factor_1,
                _ => EbnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = EbnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| EbnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<EbnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| EbnfParserNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl crate::runtime::Root for EbnfParser {
        type View<'p> = grammarView<'p>;
        #[inline]
        fn make_view<'p>(
            tape: &'p crate::runtime::tape::Tape<()>,
            input: &'p str,
            root: crate::runtime::tape::TapeOffset,
        ) -> Self::View<'p> {
            grammarView::new(tape, input, root)
        }
    }
    impl EbnfParser {
        /// The name of the root rule for this grammar.
        #[inline]
        pub fn root_rule_name() -> &'static str {
            "grammar"
        }
    }
    /// AY-II.W0.d — grammar-derived direct-to-struct projection.
    ///
    /// Emitted storage for a rule whose child sequence projects
    /// onto a fixed-layout tuple. Packed admissions read every
    /// field from `Tape::payload_bytes` at scalar offsets; rich
    /// (resolver-backed) admissions mix scalar payload reads with
    /// per-child cursor handles — the materialiser walks
    /// `view.child(i)` at the admitted `CHILD_INDICES` to
    /// populate cursor fields.
    ///
    /// `NAMED_BINDING` is `""` when the admission came from a
    /// pure layout arm; non-empty when the grammar author spelt
    /// a `-> Name` annotation. Consumers that want a semantic-
    /// type hint (e.g. CSS `"Color"`) read this const.
    #[derive(::core::marker::Copy, ::core::clone::Clone, ::core::fmt::Debug)]
    #[doc(hidden)]
    pub struct EbnfParserLetterProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: (u32, u32),
    }
    impl EbnfParserLetterProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "letter";
        /// Grammar-declared `-> Name` binding; empty string
        /// when the admission came from a pure layout arm.
        #[doc(hidden)]
        pub const NAMED_BINDING: &'static str = "";
        /// Number of fields (scalar + cursor) the layout pass
        /// admitted for this projection.
        #[doc(hidden)]
        pub const FIELD_COUNT: usize = 1;
        /// Total bytes the projection's packed portion occupies
        /// in the aggregate payload buffer; `0` when every
        /// field is a cursor handle.
        #[doc(hidden)]
        pub const TOTAL_BYTES: u8 = 8;
    }
    /// AY-II.W0.d — grammar-derived direct-to-struct projection.
    ///
    /// Emitted storage for a rule whose child sequence projects
    /// onto a fixed-layout tuple. Packed admissions read every
    /// field from `Tape::payload_bytes` at scalar offsets; rich
    /// (resolver-backed) admissions mix scalar payload reads with
    /// per-child cursor handles — the materialiser walks
    /// `view.child(i)` at the admitted `CHILD_INDICES` to
    /// populate cursor fields.
    ///
    /// `NAMED_BINDING` is `""` when the admission came from a
    /// pure layout arm; non-empty when the grammar author spelt
    /// a `-> Name` annotation. Consumers that want a semantic-
    /// type hint (e.g. CSS `"Color"`) read this const.
    #[derive(::core::marker::Copy, ::core::clone::Clone, ::core::fmt::Debug)]
    #[doc(hidden)]
    pub struct EbnfParserDigitProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: (u32, u32),
    }
    impl EbnfParserDigitProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "digit";
        /// Grammar-declared `-> Name` binding; empty string
        /// when the admission came from a pure layout arm.
        #[doc(hidden)]
        pub const NAMED_BINDING: &'static str = "";
        /// Number of fields (scalar + cursor) the layout pass
        /// admitted for this projection.
        #[doc(hidden)]
        pub const FIELD_COUNT: usize = 1;
        /// Total bytes the projection's packed portion occupies
        /// in the aggregate payload buffer; `0` when every
        /// field is a cursor handle.
        #[doc(hidden)]
        pub const TOTAL_BYTES: u8 = 8;
    }
    /// AY-II.W0.d — grammar-derived direct-to-struct projection.
    ///
    /// Emitted storage for a rule whose child sequence projects
    /// onto a fixed-layout tuple. Packed admissions read every
    /// field from `Tape::payload_bytes` at scalar offsets; rich
    /// (resolver-backed) admissions mix scalar payload reads with
    /// per-child cursor handles — the materialiser walks
    /// `view.child(i)` at the admitted `CHILD_INDICES` to
    /// populate cursor fields.
    ///
    /// `NAMED_BINDING` is `""` when the admission came from a
    /// pure layout arm; non-empty when the grammar author spelt
    /// a `-> Name` annotation. Consumers that want a semantic-
    /// type hint (e.g. CSS `"Color"`) read this const.
    #[derive(::core::marker::Copy, ::core::clone::Clone, ::core::fmt::Debug)]
    #[doc(hidden)]
    pub struct EbnfParserSymbolProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: (u32, u32),
    }
    impl EbnfParserSymbolProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "symbol";
        /// Grammar-declared `-> Name` binding; empty string
        /// when the admission came from a pure layout arm.
        #[doc(hidden)]
        pub const NAMED_BINDING: &'static str = "";
        /// Number of fields (scalar + cursor) the layout pass
        /// admitted for this projection.
        #[doc(hidden)]
        pub const FIELD_COUNT: usize = 1;
        /// Total bytes the projection's packed portion occupies
        /// in the aggregate payload buffer; `0` when every
        /// field is a cursor handle.
        #[doc(hidden)]
        pub const TOTAL_BYTES: u8 = 8;
    }
    /// AY-II.W0.d — per-grammar direct-to-struct projection
    /// admissions, derived from `ir.payload_layouts` + the
    /// `RustNamedTypes` resolver.
    ///
    /// Each `(rule_name, struct_name)` pair identifies a
    /// non-transparent rule whose projection admits direct-to-
    /// struct storage. `struct_name` is ALWAYS the synthesised
    /// `<Grammar><RuleCamel>Projection` struct emitted alongside
    /// this const — no resolver-bound name dispatch.
    pub const PROJECTION_DIRECT_TO_STRUCT: &[(&str, &str); 3usize] = &[
        ("letter", "EbnfParserLetterProjection"),
        ("digit", "EbnfParserDigitProjection"),
        ("symbol", "EbnfParserSymbolProjection"),
    ];
    /// AY-II.W0.d — grammar-declared `-> Name` bindings, indexed in
    /// lockstep with `PROJECTION_DIRECT_TO_STRUCT`. Empty string for
    /// admissions that did not spell a named type.
    #[doc(hidden)]
    pub const PROJECTION_NAMED_BINDINGS: &[&str; 3usize] = &["", "", ""];
    /// AY-II.W0.d — canonical evidence that every admission has a
    /// matching `materialize_projection_<rule>_<Grammar>` fn.
    /// Indexed in lockstep with `PROJECTION_DIRECT_TO_STRUCT`; the
    /// wire-contract totality test asserts both slices share the
    /// same length per grammar.
    #[doc(hidden)]
    pub const PROJECTION_MATERIALIZERS: &[&str; 3usize] = &[
        "materialize_projection_letter_EbnfParser",
        "materialize_projection_digit_EbnfParser",
        "materialize_projection_symbol_EbnfParser",
    ];
    /// AY-II.W0.d — canonical evidence that every admission has a
    /// matching `<Grammar>Value::<RuleName>` enum variant
    /// (production consumer). Indexed in lockstep with
    /// `PROJECTION_DIRECT_TO_STRUCT`.
    #[doc(hidden)]
    pub const PROJECTION_CONSUMERS: &[&str; 3usize] = &[
        "EbnfParserValue::letter",
        "EbnfParserValue::digit",
        "EbnfParserValue::symbol",
    ];
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_letter() -> (&'static str, usize, &'static str) {
        ("letter", 1, "")
    }
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_digit() -> (&'static str, usize, &'static str) {
        ("digit", 1, "")
    }
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_symbol() -> (&'static str, usize, &'static str) {
        ("symbol", 1, "")
    }
    /// AY-II.W0'.b — grammar-emitted value enum. Eager
    /// materialisation target for `Parsed::to_value()`. Variants
    /// enumerate non-transparent rules; admitted rules carry the
    /// matching `<Grammar><RuleCamel>Projection` struct directly,
    /// non-admitted rules carry their shape-classified payload.
    #[derive(Clone, Debug)]
    pub enum EbnfParserValue<'p> {
        letter(EbnfParserLetterProjection),
        digit(EbnfParserDigitProjection),
        symbol(EbnfParserSymbolProjection),
        identifier(::std::vec::Vec<EbnfParserValue<'p>>),
        character(::std::vec::Vec<EbnfParserValue<'p>>),
        terminal(::std::vec::Vec<EbnfParserValue<'p>>),
        concatenation(::std::vec::Vec<EbnfParserValue<'p>>),
        alternation(::std::vec::Vec<EbnfParserValue<'p>>),
        rhs(EbnfParserNodeView<'p>),
        term(::std::vec::Vec<EbnfParserValue<'p>>),
        factor(::std::vec::Vec<EbnfParserValue<'p>>),
        rule(::std::vec::Vec<EbnfParserValue<'p>>),
        grammar(::std::vec::Vec<EbnfParserValue<'p>>),
        /// Fallback for records whose `variant_idx` is not a
        /// known rule discriminator (recovered records, stray
        /// sub-variant indices).
        Unknown(EbnfParserNodeView<'p>),
    }
    /// B5.W0.6 — joint `(kind, variant_idx)` dispatch local to the
    /// fused-pipeline projection path.
    ///
    /// `variant_idx = (rule_id & 0xFF)` collapses every rule whose
    /// id-mod-256 collides; for non-rule structural compounds the
    /// shape emitters stamp `variant_idx = 0` as a placeholder
    /// (see `emitter/shapes/{flat,array,object,inline}.rs`), which
    /// pre-B5.W0.6 collided with rule_id=0 (CSS L4 `namedColor`,
    /// JSON `null`, etc.) and routed Seq/Alt/Repeat intermediates
    /// to a leaf-rule's materialiser. The materialiser then panicked
    /// against the compound's `child_off` (a column rank, not an
    /// arena byte offset) at `payload_bytes`'s precondition assert.
    ///
    /// The dispatch now consults `kind` AS WELL AS `variant_idx`:
    /// a compound-kind frame carrying the placeholder `variant_idx
    /// = 0` is an intermediate without a rule binding and routes
    /// to `Unknown`. The `ValueFrame` doc-comment at
    /// `crates/tape/src/builder/value.rs:47` already declares this
    /// invariant — pre-B5.W0.6 the codegen ignored it.
    #[inline(always)]
    fn project_rule_kind_EbnfParser(
        kind: crate::runtime::tape::TapeKind,
        variant_idx: u8,
    ) -> EbnfParserRuleKind {
        if variant_idx == 0 && kind.is_compound() {
            return EbnfParserRuleKind::Unknown;
        }
        match variant_idx {
            0u8 => EbnfParserRuleKind::letter,
            1u8 => EbnfParserRuleKind::digit,
            2u8 => EbnfParserRuleKind::symbol,
            3u8 => EbnfParserRuleKind::identifier,
            4u8 => EbnfParserRuleKind::character,
            5u8 => EbnfParserRuleKind::terminal,
            6u8 => EbnfParserRuleKind::concatenation,
            7u8 => EbnfParserRuleKind::alternation,
            8u8 => EbnfParserRuleKind::rhs,
            9u8 => EbnfParserRuleKind::term,
            10u8 => EbnfParserRuleKind::factor,
            11u8 => EbnfParserRuleKind::rule,
            12u8 => EbnfParserRuleKind::grammar,
            _ => EbnfParserRuleKind::Unknown,
        }
    }
    /// B5.W0.6 — push the projected value(s) for the record at
    /// `offset` onto `out`. For rule-bound records this is a single
    /// `<Grammar>Value` variant constructed via [`#frame_fn`]. For
    /// intermediate compound records (the `variant_idx=0` non-rule
    /// structural compounds emitted at inner Seq / Repeat / Alt
    /// positions) it recurses through the children, flattening the
    /// intermediate transparently — the user-visible value tree
    /// only carries rule-bound variants.
    ///
    /// Mirrors the walker-tape parity contract: the substrate emits
    /// one tape record per IR production, but only rule-bound
    /// productions surface as `<Grammar>Value` variants; structural
    /// intermediates are an implementation detail of the tape
    /// shape, not of the value tree.
    ///
    /// Reads `kind` + `variant_idx` from the tape (not the value
    /// frame). The materializer pattern at
    /// `materialize_projection_<rule>_<Grammar>` already treats
    /// `offset` as a tape offset (`tape.try_get(TapeOffset(offset))`);
    /// the dispatch is therefore consistent with the materialiser
    /// surface — tape is the canonical record substrate, the value
    /// frames are a parallel cache used only for typed scalar
    /// payload reads on leaves with a payload tag.
    #[inline]
    fn project_push_children_EbnfParser<'p>(
        output: &crate::runtime::tape::Tape<EbnfParser>,
        input: &'p str,
        offset: u32,
        out: &mut ::std::vec::Vec<EbnfParserValue<'p>>,
    ) {
        let __tape = output;
        let __rec = match __tape.try_get(crate::runtime::tape::TapeOffset(offset)) {
            ::core::option::Option::Some(r) => r,
            ::core::option::Option::None => return,
        };
        if __rec.variant_idx() == 0 && __rec.kind().is_compound() {
            let __cur = crate::runtime::tape::TapeCursor::new(
                __tape,
                crate::runtime::tape::TapeOffset(offset),
            );
            for __child in __cur.children() {
                project_push_children_EbnfParser(output, input, __child.offset().0, out);
            }
        } else {
            out.push(project_frame_EbnfParser(output, input, offset));
        }
    }
    /// AY-II.W0'.b — per-frame projector. Reads one record from the
    /// fused-pipeline [`Tape<R>`](crate::runtime::tape::Tape)
    /// tape and constructs the matching `<Grammar>Value` variant.
    /// Admitted rules tail-call their grammar-derived materializer;
    /// non-admitted rules construct the variant inline. Compound
    /// variants recurse through this same fn.
    ///
    /// B5.W0.6 — kind + variant_idx + span are read from the tape
    /// record (not the value frame). The value frame substrate is
    /// only consulted for typed-scalar payload reads on leaves
    /// whose `value_payload_for(frame)` returns the column-decoded
    /// payload — that path remains in the scalar arm.
    #[inline]
    fn project_frame_EbnfParser<'p>(
        output: &crate::runtime::tape::Tape<EbnfParser>,
        input: &'p str,
        offset: u32,
    ) -> EbnfParserValue<'p> {
        let __tape = output;
        let __rec = match __tape.try_get(crate::runtime::tape::TapeOffset(offset)) {
            ::core::option::Option::Some(r) => r,
            ::core::option::Option::None => {
                ::core::panic!(
                    "AY-II.W0'.b: tape offset {} out of range (tape len: {})", offset,
                    __tape.len(),
                );
            }
        };
        match project_rule_kind_EbnfParser(__rec.kind(), __rec.variant_idx()) {
            EbnfParserRuleKind::letter => {
                let proj = materialize_projection_letter_EbnfParser(
                        output,
                        input,
                        offset,
                    )
                    .unwrap_or_else(|| {
                        ::core::panic!(
                            "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                            "letter", offset,
                        );
                    });
                EbnfParserValue::letter(proj)
            }
            EbnfParserRuleKind::digit => {
                let proj = materialize_projection_digit_EbnfParser(output, input, offset)
                    .unwrap_or_else(|| {
                        ::core::panic!(
                            "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                            "digit", offset,
                        );
                    });
                EbnfParserValue::digit(proj)
            }
            EbnfParserRuleKind::symbol => {
                let proj = materialize_projection_symbol_EbnfParser(
                        output,
                        input,
                        offset,
                    )
                    .unwrap_or_else(|| {
                        ::core::panic!(
                            "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                            "symbol", offset,
                        );
                    });
                EbnfParserValue::symbol(proj)
            }
            EbnfParserRuleKind::identifier => {
                let mut children: ::std::vec::Vec<EbnfParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_EbnfParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                EbnfParserValue::identifier(children)
            }
            EbnfParserRuleKind::character => {
                let mut children: ::std::vec::Vec<EbnfParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_EbnfParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                EbnfParserValue::character(children)
            }
            EbnfParserRuleKind::terminal => {
                let mut children: ::std::vec::Vec<EbnfParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_EbnfParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                EbnfParserValue::terminal(children)
            }
            EbnfParserRuleKind::concatenation => {
                let mut children: ::std::vec::Vec<EbnfParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_EbnfParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                EbnfParserValue::concatenation(children)
            }
            EbnfParserRuleKind::alternation => {
                let mut children: ::std::vec::Vec<EbnfParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_EbnfParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                EbnfParserValue::alternation(children)
            }
            EbnfParserRuleKind::rhs => {
                ::core::panic!(
                    "AY-II.W0'.b: Cursor-shape variant projection not yet \
                     available; tape record offset {}",
                    offset,
                );
            }
            EbnfParserRuleKind::term => {
                let mut children: ::std::vec::Vec<EbnfParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_EbnfParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                EbnfParserValue::term(children)
            }
            EbnfParserRuleKind::factor => {
                let mut children: ::std::vec::Vec<EbnfParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_EbnfParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                EbnfParserValue::factor(children)
            }
            EbnfParserRuleKind::rule => {
                let mut children: ::std::vec::Vec<EbnfParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_EbnfParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                EbnfParserValue::rule(children)
            }
            EbnfParserRuleKind::grammar => {
                let mut children: ::std::vec::Vec<EbnfParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_EbnfParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                EbnfParserValue::grammar(children)
            }
            _ => {
                ::core::panic!(
                    "AY-II.W0'.b: unclassified (kind={:?}, variant_idx={}) on tape record at offset {}",
                    __rec.kind(), __rec.variant_idx(), offset,
                );
            }
        }
    }
    /// AY-II.W0'.b — fused-pipeline root projector. Reads the root
    /// record from the tape and constructs the grammar's
    /// `<Grammar>Value<'p>` in one pass. No tape walk, no reparse,
    /// no visitor dispatch.
    ///
    /// B5.W1 — when the root tape record is a structural
    /// intermediate compound (variant_idx=0, kind compound — the
    /// shape emitters' Repeat / Seq scaffolding lands here), the
    /// projector descends into the first rule-bound child rather
    /// than panicking on `Unknown`. This mirrors
    /// `project_push_children_<Grammar>`'s transparent-recursion
    /// invariant.
    #[inline]
    fn project_value_EbnfParser<'p>(
        output: &crate::runtime::tape::Tape<EbnfParser>,
        input: &'p str,
    ) -> EbnfParserValue<'p> {
        let root_off = output.root_offset();
        let __tape = output;
        let mut __cur_off = root_off;
        loop {
            let __rec = match __tape.try_get(crate::runtime::tape::TapeOffset(__cur_off))
            {
                ::core::option::Option::Some(r) => r,
                ::core::option::Option::None => break,
            };
            if __rec.variant_idx() == 0 && __rec.kind().is_compound() {
                if __rec.has_children() {
                    if let ::core::option::Option::Some(__child) = __rec
                        .child_off
                        .as_u32()
                        .checked_sub(0)
                    {
                        if __child != ::core::u32::MAX {
                            __cur_off = __child;
                            continue;
                        }
                    }
                }
                break;
            }
            break;
        }
        project_frame_EbnfParser(output, input, __cur_off)
    }
    impl crate::runtime::ValueRoot for EbnfParser {
        type Value<'p> = EbnfParserValue<'p>;
        #[inline]
        fn project_value_output<'p>(
            output: &crate::runtime::tape::Tape<EbnfParser>,
            input: &'p str,
        ) -> Self::Value<'p>
        where
            Self: 'p,
        {
            project_value_EbnfParser(output, input)
        }
    }
    /// AY-II.W0'.c — policy-driven path walker. Descends from
    /// `view` per the given path, returning the narrowed
    /// NodeView on hit or `None` when any step misses.
    ///
    /// The per-step dispatch reads `cur.rule_kind()` and
    /// resolves to the structural-scan primitive the rule's
    /// [`STRUCTURAL_SCAN_POLICY`] entry admits: rules admitting
    /// `OBJECT_KEY_SEEK` use
    /// [`TapeCursor::bounded_lookahead`] + [`TapeCursor::object_key_seek`]
    /// for the key-match + value-hop sequence; rules admitting
    /// `SCAN_STRUCTURAL_BOUNDED` use
    /// [`TapeCursor::scan_structural_bounded`] for positional
    /// access. Rules outside the policy's admission fall
    /// through to a generic children iteration.
    ///
    /// [`STRUCTURAL_SCAN_POLICY`]: crate::STRUCTURAL_SCAN_POLICY
    /// [`TapeCursor::bounded_lookahead`]: crate::runtime::tape::TapeCursor::bounded_lookahead
    /// [`TapeCursor::object_key_seek`]: crate::runtime::tape::TapeCursor::object_key_seek
    /// [`TapeCursor::scan_structural_bounded`]: crate::runtime::tape::TapeCursor::scan_structural_bounded
    #[inline]
    fn __path_walk<'p>(
        view: EbnfParserNodeView<'p>,
        path: crate::runtime::Path<'_>,
    ) -> ::core::option::Option<EbnfParserNodeView<'p>> {
        let cur_input = view.input();
        let mut cur = view;
        for seg in path.iter() {
            match seg {
                crate::runtime::PathSegment::Field(key) => {
                    match cur.rule_kind() {
                        EbnfParserRuleKind::letter
                        | EbnfParserRuleKind::digit
                        | EbnfParserRuleKind::symbol
                        | EbnfParserRuleKind::identifier
                        | EbnfParserRuleKind::character
                        | EbnfParserRuleKind::concatenation
                        | EbnfParserRuleKind::alternation
                        | EbnfParserRuleKind::term
                        | EbnfParserRuleKind::factor
                        | EbnfParserRuleKind::rule
                        | EbnfParserRuleKind::grammar => {
                            let parent = cur.cursor();
                            let (_, parent_end) = parent.span();
                            let mut iter = parent.bounded_lookahead(parent_end);
                            let mut hit: ::core::option::Option<
                                EbnfParserNodeView<'p>,
                            > = None;
                            loop {
                                let k_cur = match iter.next() {
                                    ::core::option::Option::Some(c) => c,
                                    ::core::option::Option::None => break,
                                };
                                let _ = iter.next();
                                let (k_lo, k_hi) = k_cur.span();
                                let raw = &cur_input[k_lo as usize..k_hi as usize];
                                let key_text = if raw.as_bytes().first()
                                    == ::core::option::Option::Some(&b'"')
                                    && raw.as_bytes().last()
                                        == ::core::option::Option::Some(&b'"') && raw.len() >= 2
                                {
                                    &raw[1..raw.len() - 1]
                                } else {
                                    raw
                                };
                                if key_text == *key {
                                    let v_cursor = parent.object_key_seek((k_lo, k_hi));
                                    hit = v_cursor
                                        .map(|c| EbnfParserNodeView::from_cursor(c, cur_input));
                                    break;
                                }
                            }
                            cur = match hit {
                                ::core::option::Option::Some(v) => v,
                                ::core::option::Option::None => {
                                    return ::core::option::Option::None;
                                }
                            };
                        }
                        EbnfParserRuleKind::terminal => {
                            let parent = cur.cursor();
                            let (_, parent_end) = parent.span();
                            let mut iter = parent.bounded_lookahead(parent_end);
                            let mut hit: ::core::option::Option<
                                EbnfParserNodeView<'p>,
                            > = None;
                            loop {
                                let k_cur = match iter.next() {
                                    ::core::option::Option::Some(c) => c,
                                    ::core::option::Option::None => break,
                                };
                                let v_cur = match iter.next() {
                                    ::core::option::Option::Some(c) => c,
                                    ::core::option::Option::None => break,
                                };
                                let (k_lo, k_hi) = k_cur.span();
                                let raw = &cur_input[k_lo as usize..k_hi as usize];
                                let key_text = if raw.as_bytes().first()
                                    == ::core::option::Option::Some(&b'"')
                                    && raw.as_bytes().last()
                                        == ::core::option::Option::Some(&b'"') && raw.len() >= 2
                                {
                                    &raw[1..raw.len() - 1]
                                } else {
                                    raw
                                };
                                if key_text == *key {
                                    hit = ::core::option::Option::Some(
                                        EbnfParserNodeView::from_cursor(v_cur, cur_input),
                                    );
                                    break;
                                }
                            }
                            cur = match hit {
                                ::core::option::Option::Some(v) => v,
                                ::core::option::Option::None => {
                                    return ::core::option::Option::None;
                                }
                            };
                        }
                        _ => {
                            let mut it = cur.children();
                            let mut found = None;
                            loop {
                                let k = match it.next() {
                                    ::core::option::Option::Some(k) => k,
                                    ::core::option::Option::None => break,
                                };
                                let v = match it.next() {
                                    ::core::option::Option::Some(v) => v,
                                    ::core::option::Option::None => break,
                                };
                                let raw = k.span_text();
                                let key_text = if raw.as_bytes().first()
                                    == ::core::option::Option::Some(&b'"')
                                    && raw.as_bytes().last()
                                        == ::core::option::Option::Some(&b'"') && raw.len() >= 2
                                {
                                    &raw[1..raw.len() - 1]
                                } else {
                                    raw
                                };
                                if key_text == *key {
                                    found = ::core::option::Option::Some(v);
                                    break;
                                }
                            }
                            cur = match found {
                                ::core::option::Option::Some(v) => v,
                                ::core::option::Option::None => {
                                    return ::core::option::Option::None;
                                }
                            };
                        }
                    }
                }
                crate::runtime::PathSegment::Index(i) => {
                    match cur.rule_kind() {
                        EbnfParserRuleKind::letter
                        | EbnfParserRuleKind::digit
                        | EbnfParserRuleKind::symbol
                        | EbnfParserRuleKind::identifier
                        | EbnfParserRuleKind::character
                        | EbnfParserRuleKind::concatenation
                        | EbnfParserRuleKind::alternation
                        | EbnfParserRuleKind::term
                        | EbnfParserRuleKind::factor
                        | EbnfParserRuleKind::rule
                        | EbnfParserRuleKind::grammar => {
                            let parent = cur.cursor();
                            let (_, parent_end) = parent.span();
                            let scan = parent.scan_structural_bounded(parent_end);
                            cur = match scan.iter().nth(*i) {
                                ::core::option::Option::Some(c) => {
                                    EbnfParserNodeView::from_cursor(c, cur_input)
                                }
                                ::core::option::Option::None => {
                                    return ::core::option::Option::None;
                                }
                            };
                        }
                        _ => {
                            cur = cur.child(*i)?;
                        }
                    }
                }
            }
        }
        ::core::option::Option::Some(cur)
    }
    impl crate::runtime::PathQuery<&'static str> for EbnfParser {
        #[inline]
        fn query<'p>(
            view: Self::View<'p>,
            path: crate::runtime::Path<'_>,
        ) -> ::core::option::Option<&'static str>
        where
            Self: 'p,
        {
            let node = EbnfParserNodeView::from_cursor(view.cursor(), view.input());
            __path_walk(node, path)?;
            ::core::option::Option::None
        }
    }
    impl crate::runtime::PathQuery<f64> for EbnfParser {
        #[inline]
        fn query<'p>(
            view: Self::View<'p>,
            path: crate::runtime::Path<'_>,
        ) -> ::core::option::Option<f64>
        where
            Self: 'p,
        {
            let node = EbnfParserNodeView::from_cursor(view.cursor(), view.input());
            let hit = __path_walk(node, path)?;
            let tape = hit.cursor().tape();
            let rec = hit.cursor().record();
            if let ::core::option::Option::Some(v) = tape.payload_f64(rec) {
                return ::core::option::Option::Some(v);
            }
            hit.span_text().parse::<f64>().ok()
        }
    }
    impl crate::runtime::PathQuery<bool> for EbnfParser {
        #[inline]
        fn query<'p>(
            view: Self::View<'p>,
            path: crate::runtime::Path<'_>,
        ) -> ::core::option::Option<bool>
        where
            Self: 'p,
        {
            let node = EbnfParserNodeView::from_cursor(view.cursor(), view.input());
            let hit = __path_walk(node, path)?;
            let tape = hit.cursor().tape();
            let rec = hit.cursor().record();
            if let ::core::option::Option::Some(v) = tape.payload_bool(rec) {
                return ::core::option::Option::Some(v);
            }
            match hit.span_text() {
                "true" => ::core::option::Option::Some(true),
                "false" => ::core::option::Option::Some(false),
                _ => ::core::option::Option::None,
            }
        }
    }
    /// AY-II.W0'.b — grammar-derived direct-to-struct projection
    /// helper. Reads the admitted rule's frame from the
    /// fused-pipeline [`Tape<R>`](crate::runtime::tape::Tape)
    /// slab and constructs the matching projection struct;
    /// returns `None` when the slab's frame is absent or the
    /// tape's aggregate buffer is too short.
    ///
    /// Routed from `project_frame_<Grammar>` per admission.
    /// `#[inline]` so LLVM folds the body into the dispatcher at
    /// monomorphisation time. Emitted 1:1 per
    /// [`PROJECTION_DIRECT_TO_STRUCT`] entry — post-AY-II.W0'.b
    /// totality is admission : materialiser : consumer at
    /// 1:1:1 per grammar with runtime call-count truth.
    #[inline]
    #[doc(hidden)]
    pub fn materialize_projection_letter_EbnfParser<'p>(
        output: &crate::runtime::tape::Tape<EbnfParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<EbnfParserLetterProjection> {
        let _ = input;
        let frame = output.frame(offset)?;
        let __bytes: &[u8] = &[];
        let _ = __bytes;
        let field_0: (u32, u32) = (frame.span_lo, frame.span_hi);
        ::core::option::Option::Some(EbnfParserLetterProjection {
            field_0,
        })
    }
    /// AY-II.W0'.b — grammar-derived direct-to-struct projection
    /// helper. Reads the admitted rule's frame from the
    /// fused-pipeline [`Tape<R>`](crate::runtime::tape::Tape)
    /// slab and constructs the matching projection struct;
    /// returns `None` when the slab's frame is absent or the
    /// tape's aggregate buffer is too short.
    ///
    /// Routed from `project_frame_<Grammar>` per admission.
    /// `#[inline]` so LLVM folds the body into the dispatcher at
    /// monomorphisation time. Emitted 1:1 per
    /// [`PROJECTION_DIRECT_TO_STRUCT`] entry — post-AY-II.W0'.b
    /// totality is admission : materialiser : consumer at
    /// 1:1:1 per grammar with runtime call-count truth.
    #[inline]
    #[doc(hidden)]
    pub fn materialize_projection_digit_EbnfParser<'p>(
        output: &crate::runtime::tape::Tape<EbnfParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<EbnfParserDigitProjection> {
        let _ = input;
        let frame = output.frame(offset)?;
        let __bytes: &[u8] = &[];
        let _ = __bytes;
        let field_0: (u32, u32) = (frame.span_lo, frame.span_hi);
        ::core::option::Option::Some(EbnfParserDigitProjection {
            field_0,
        })
    }
    /// AY-II.W0'.b — grammar-derived direct-to-struct projection
    /// helper. Reads the admitted rule's frame from the
    /// fused-pipeline [`Tape<R>`](crate::runtime::tape::Tape)
    /// slab and constructs the matching projection struct;
    /// returns `None` when the slab's frame is absent or the
    /// tape's aggregate buffer is too short.
    ///
    /// Routed from `project_frame_<Grammar>` per admission.
    /// `#[inline]` so LLVM folds the body into the dispatcher at
    /// monomorphisation time. Emitted 1:1 per
    /// [`PROJECTION_DIRECT_TO_STRUCT`] entry — post-AY-II.W0'.b
    /// totality is admission : materialiser : consumer at
    /// 1:1:1 per grammar with runtime call-count truth.
    #[inline]
    #[doc(hidden)]
    pub fn materialize_projection_symbol_EbnfParser<'p>(
        output: &crate::runtime::tape::Tape<EbnfParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<EbnfParserSymbolProjection> {
        let _ = input;
        let frame = output.frame(offset)?;
        let __bytes: &[u8] = &[];
        let _ = __bytes;
        let field_0: (u32, u32) = (frame.span_lo, frame.span_hi);
        ::core::option::Option::Some(EbnfParserSymbolProjection {
            field_0,
        })
    }
    impl EbnfParser {
        fn __letter_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __byte = match state.src_bytes.get(state.offset) {
                        Some(&b) => b,
                        None => return false,
                    };
                    match __byte {
                        b'A' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'A')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'A');
                            };
                        }
                        b'B' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'B')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'B');
                            };
                        }
                        b'C' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'C')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'C');
                            };
                        }
                        b'D' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'D')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'D');
                            };
                        }
                        b'E' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'E')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'E');
                            };
                        }
                        b'F' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'F')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'F');
                            };
                        }
                        b'G' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'G')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'G');
                            };
                        }
                        b'H' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'H')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'H');
                            };
                        }
                        b'I' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'I')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'I');
                            };
                        }
                        b'J' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'J')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'J');
                            };
                        }
                        b'K' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'K')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'K');
                            };
                        }
                        b'L' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'L')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'L');
                            };
                        }
                        b'M' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'M')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'M');
                            };
                        }
                        b'N' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'N')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'N');
                            };
                        }
                        b'O' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'O')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'O');
                            };
                        }
                        b'P' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'P')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'P');
                            };
                        }
                        b'Q' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'Q')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'Q');
                            };
                        }
                        b'R' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'R')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'R');
                            };
                        }
                        b'S' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'S')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'S');
                            };
                        }
                        b'T' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'T')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'T');
                            };
                        }
                        b'U' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'U')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'U');
                            };
                        }
                        b'V' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'V')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'V');
                            };
                        }
                        b'W' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'W')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'W');
                            };
                        }
                        b'X' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'X')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'X');
                            };
                        }
                        b'Y' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'Y')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'Y');
                            };
                        }
                        b'Z' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'Z')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'Z');
                            };
                        }
                        b'a' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'a')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'a');
                            };
                        }
                        b'b' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'b')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'b');
                            };
                        }
                        b'c' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'c')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'c');
                            };
                        }
                        b'd' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'd')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'd');
                            };
                        }
                        b'e' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'e')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'e');
                            };
                        }
                        b'f' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'f')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'f');
                            };
                        }
                        b'g' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'g')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'g');
                            };
                        }
                        b'h' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'h')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'h');
                            };
                        }
                        b'i' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'i')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'i');
                            };
                        }
                        b'j' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'j')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'j');
                            };
                        }
                        b'k' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'k')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'k');
                            };
                        }
                        b'l' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'l')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'l');
                            };
                        }
                        b'm' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'm')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'm');
                            };
                        }
                        b'n' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'n')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'n');
                            };
                        }
                        b'o' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'o')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'o');
                            };
                        }
                        b'p' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'p')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'p');
                            };
                        }
                        b'q' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'q')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'q');
                            };
                        }
                        b'r' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'r')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'r');
                            };
                        }
                        b's' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b's')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b's');
                            };
                        }
                        b't' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b't')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b't');
                            };
                        }
                        b'u' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'u')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'u');
                            };
                        }
                        b'v' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'v')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'v');
                            };
                        }
                        b'w' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'w')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'w');
                            };
                        }
                        b'x' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'x')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'x');
                            };
                        }
                        b'y' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'y')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'y');
                            };
                        }
                        b'z' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'z')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'z');
                            };
                        }
                        _ => {
                            return false;
                        }
                    }
                };
                true
            }
        }
        pub fn letter_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__letter_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __digit_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __byte = match state.src_bytes.get(state.offset) {
                        Some(&b) => b,
                        None => return false,
                    };
                    match __byte {
                        b'0' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'0')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'0');
                            };
                        }
                        b'1' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'1')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'1');
                            };
                        }
                        b'2' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'2')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'2');
                            };
                        }
                        b'3' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'3')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'3');
                            };
                        }
                        b'4' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'4')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'4');
                            };
                        }
                        b'5' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'5')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'5');
                            };
                        }
                        b'6' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'6')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'6');
                            };
                        }
                        b'7' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'7')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'7');
                            };
                        }
                        b'8' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'8')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'8');
                            };
                        }
                        b'9' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'9')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'9');
                            };
                        }
                        _ => {
                            return false;
                        }
                    }
                };
                true
            }
        }
        pub fn digit_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__digit_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __symbol_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __byte = match state.src_bytes.get(state.offset) {
                        Some(&b) => b,
                        None => return false,
                    };
                    match __byte {
                        b'[' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'[')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'[');
                            };
                        }
                        b']' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b']')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b']');
                            };
                        }
                        b'{' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'{')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'{');
                            };
                        }
                        b'}' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'}')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'}');
                            };
                        }
                        b'(' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'(')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'(');
                            };
                        }
                        b')' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b')')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b')');
                            };
                        }
                        b'<' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'<')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'<');
                            };
                        }
                        b'>' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'>')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'>');
                            };
                        }
                        b'\'' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'\'');
                            };
                        }
                        b'"' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'"')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'"');
                            };
                        }
                        b'=' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'=')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'=');
                            };
                        }
                        b'|' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'|')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'|');
                            };
                        }
                        b'.' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'.');
                            };
                        }
                        b',' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b',')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b',');
                            };
                        }
                        b';' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b';')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b';');
                            };
                        }
                        b'-' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'-');
                            };
                        }
                        b'+' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'+')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'+');
                            };
                        }
                        b'*' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'*');
                            };
                        }
                        b'?' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'?')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'?');
                            };
                        }
                        b'\n' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'\n')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'\n');
                            };
                        }
                        b'\t' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'\t')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'\t');
                            };
                        }
                        b'\r' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'\r')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'\r');
                            };
                        }
                        b'\x0C' => {
                            {
                                if state.src_bytes.get(state.offset).copied()
                                    != Some(b'\x0C')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'\x0C');
                            };
                        }
                        b'\x08' => {
                            {
                                if state.src_bytes.get(state.offset).copied()
                                    != Some(b'\x08')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'\x08');
                            };
                        }
                        b'\\' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'\\');
                            };
                        }
                        _ => {
                            return false;
                        }
                    }
                };
                true
            }
        }
        pub fn symbol_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__symbol_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __identifier_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !Self::__letter_prettify(state, __builder) {
                        return false;
                    }
                    {
                        let mut __rep_count2 = 0usize;
                        while __rep_count2 < 4294967295 {
                            let __rep_cp3 = state.offset;
                            if !{
                                let __pretty_cp0 = state.offset;
                                let __pretty_bcp1 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        let __byte = match state.src_bytes.get(state.offset) {
                                            Some(&b) => b,
                                            None => return false,
                                        };
                                        match __byte {
                                            b'_' => {
                                                {
                                                    if state.src_bytes.get(state.offset).copied() != Some(b'_')
                                                    {
                                                        return false;
                                                    }
                                                    state.offset += 1;
                                                    __builder.char(b'_');
                                                };
                                            }
                                            b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' | b'8'
                                            | b'9' => {
                                                if !Self::__digit_prettify(state, __builder) {
                                                    return false;
                                                }
                                            }
                                            b'A' | b'B' | b'C' | b'D' | b'E' | b'F' | b'G' | b'H' | b'I'
                                            | b'J' | b'K' | b'L' | b'M' | b'N' | b'O' | b'P' | b'Q'
                                            | b'R' | b'S' | b'T' | b'U' | b'V' | b'W' | b'X' | b'Y'
                                            | b'Z' | b'a' | b'b' | b'c' | b'd' | b'e' | b'f' | b'g'
                                            | b'h' | b'i' | b'j' | b'k' | b'l' | b'm' | b'n' | b'o'
                                            | b'p' | b'q' | b'r' | b's' | b't' | b'u' | b'v' | b'w'
                                            | b'x' | b'y' | b'z' => {
                                                if !Self::__letter_prettify(state, __builder) {
                                                    return false;
                                                }
                                            }
                                            _ => {
                                                return false;
                                            }
                                        }
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp0;
                                    __builder.restore(__pretty_bcp1);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp3;
                                break;
                            }
                            if state.offset == __rep_cp3 {
                                break;
                            }
                            __rep_count2 += 1;
                        }
                    };
                };
                true
            }
        }
        pub fn identifier_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__identifier_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __character_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __byte = match state.src_bytes.get(state.offset) {
                        Some(&b) => b,
                        None => return false,
                    };
                    match __byte {
                        b'_' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'_')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'_');
                            };
                        }
                        b' ' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b' ')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b' ');
                            };
                        }
                        b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' | b'8'
                        | b'9' => {
                            if !Self::__digit_prettify(state, __builder) {
                                return false;
                            }
                        }
                        b'\x08' | b'\t' | b'\n' | b'\x0C' | b'\r' | b'"' | b'\'' | b'('
                        | b')' | b'*' | b'+' | b',' | b'-' | b'.' | b';' | b'<' | b'='
                        | b'>' | b'?' | b'[' | b'\\' | b']' | b'{' | b'|' | b'}' => {
                            if !Self::__symbol_prettify(state, __builder) {
                                return false;
                            }
                        }
                        b'A' | b'B' | b'C' | b'D' | b'E' | b'F' | b'G' | b'H' | b'I'
                        | b'J' | b'K' | b'L' | b'M' | b'N' | b'O' | b'P' | b'Q' | b'R'
                        | b'S' | b'T' | b'U' | b'V' | b'W' | b'X' | b'Y' | b'Z' | b'a'
                        | b'b' | b'c' | b'd' | b'e' | b'f' | b'g' | b'h' | b'i' | b'j'
                        | b'k' | b'l' | b'm' | b'n' | b'o' | b'p' | b'q' | b'r' | b's'
                        | b't' | b'u' | b'v' | b'w' | b'x' | b'y' | b'z' => {
                            if !Self::__letter_prettify(state, __builder) {
                                return false;
                            }
                        }
                        _ => {
                            return false;
                        }
                    }
                };
                true
            }
        }
        pub fn character_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__character_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __terminal_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __byte = match state.src_bytes.get(state.offset) {
                        Some(&b) => b,
                        None => return false,
                    };
                    match __byte {
                        b'\'' => {
                            {
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b'\'');
                                };
                                {
                                    {
                                        let __pretty_cp4 = state.offset;
                                        let __pretty_bcp5 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b'\'');
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp4;
                                            __builder.restore(__pretty_bcp5);
                                        }
                                        __ok
                                    };
                                    if !Self::__character_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                {
                                    let mut __rep_count10 = 0usize;
                                    while __rep_count10 < 4294967295 {
                                        let __rep_cp11 = state.offset;
                                        if !{
                                            let __pretty_cp8 = state.offset;
                                            let __pretty_bcp9 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    {
                                                        let __pretty_cp6 = state.offset;
                                                        let __pretty_bcp7 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                                                {
                                                                    return false;
                                                                }
                                                                state.offset += 1;
                                                                __builder.char(b'\'');
                                                            };
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp6;
                                                            __builder.restore(__pretty_bcp7);
                                                        }
                                                        __ok
                                                    };
                                                    if !Self::__character_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp8;
                                                __builder.restore(__pretty_bcp9);
                                            }
                                            __ok
                                        } {
                                            state.offset = __rep_cp11;
                                            break;
                                        }
                                        if state.offset == __rep_cp11 {
                                            break;
                                        }
                                        __rep_count10 += 1;
                                    }
                                };
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b'\'');
                                };
                            };
                        }
                        b'"' => {
                            {
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'"')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b'"');
                                };
                                {
                                    {
                                        let __pretty_cp12 = state.offset;
                                        let __pretty_bcp13 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'"')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b'"');
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp12;
                                            __builder.restore(__pretty_bcp13);
                                        }
                                        __ok
                                    };
                                    if !Self::__character_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                {
                                    let mut __rep_count18 = 0usize;
                                    while __rep_count18 < 4294967295 {
                                        let __rep_cp19 = state.offset;
                                        if !{
                                            let __pretty_cp16 = state.offset;
                                            let __pretty_bcp17 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    {
                                                        let __pretty_cp14 = state.offset;
                                                        let __pretty_bcp15 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'"')
                                                                {
                                                                    return false;
                                                                }
                                                                state.offset += 1;
                                                                __builder.char(b'"');
                                                            };
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp14;
                                                            __builder.restore(__pretty_bcp15);
                                                        }
                                                        __ok
                                                    };
                                                    if !Self::__character_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp16;
                                                __builder.restore(__pretty_bcp17);
                                            }
                                            __ok
                                        } {
                                            state.offset = __rep_cp19;
                                            break;
                                        }
                                        if state.offset == __rep_cp19 {
                                            break;
                                        }
                                        __rep_count18 += 1;
                                    }
                                };
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'"')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b'"');
                                };
                            };
                        }
                        _ => {
                            return false;
                        }
                    }
                };
                true
            }
        }
        pub fn terminal_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__terminal_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __concatenation_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __rep_start26 = state.offset;
                    let __rep_bcp27 = __builder.checkpoint();
                    let mut __rep_count24 = 0usize;
                    while __rep_count24 < 4294967295 {
                        let __rep_cp25 = state.offset;
                        if !{
                            let __pretty_cp22 = state.offset;
                            let __pretty_bcp23 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __start = state.offset;
                                        if {
                                            let __start = state.offset;
                                            let __end = state.src_bytes.len();
                                            let mut __pos = __start;
                                            while __pos < __end {
                                                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                if (__b >= b'\t' && __b <= b'\n')
                                                    || (__b >= b'\x0C' && __b <= b'\r') || __b == b' '
                                                {
                                                    __pos += 1;
                                                } else {
                                                    break;
                                                }
                                            }
                                            state.offset = __pos;
                                            Some(::parse_that::Span::new(__start, __pos, state.src))
                                        }
                                            .is_none()
                                        {
                                            return false;
                                        }
                                        let __matched = &state.src[__start..state.offset];
                                        if !__matched.is_empty() {
                                            __builder.text(__matched);
                                        }
                                    };
                                    if !Self::__factor_prettify(state, __builder) {
                                        return false;
                                    }
                                    {
                                        let __start = state.offset;
                                        if {
                                            let __start = state.offset;
                                            let __end = state.src_bytes.len();
                                            let mut __pos = __start;
                                            while __pos < __end {
                                                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                if (__b >= b'\t' && __b <= b'\n')
                                                    || (__b >= b'\x0C' && __b <= b'\r') || __b == b' '
                                                {
                                                    __pos += 1;
                                                } else {
                                                    break;
                                                }
                                            }
                                            state.offset = __pos;
                                            Some(::parse_that::Span::new(__start, __pos, state.src))
                                        }
                                            .is_none()
                                        {
                                            return false;
                                        }
                                        let __matched = &state.src[__start..state.offset];
                                        if !__matched.is_empty() {
                                            __builder.text(__matched);
                                        }
                                    };
                                    {
                                        let _ = {
                                            let __pretty_cp20 = state.offset;
                                            let __pretty_bcp21 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                    {
                                                        return false;
                                                    }
                                                    state.offset += 1;
                                                    __builder.char(b',');
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp20;
                                                __builder.restore(__pretty_bcp21);
                                            }
                                            __ok
                                        };
                                        true
                                    };
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp22;
                                __builder.restore(__pretty_bcp23);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp25;
                            break;
                        }
                        if state.offset == __rep_cp25 {
                            break;
                        }
                        __rep_count24 += 1;
                    }
                    if __rep_count24 < 1 {
                        state.offset = __rep_start26;
                        __builder.restore(__rep_bcp27);
                        return false;
                    }
                };
                true
            }
        }
        pub fn concatenation_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__concatenation_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __alternation_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            let __pretty_ok = {
                {
                    {
                        let __rep_start34 = state.offset;
                        let __rep_bcp35 = __builder.checkpoint();
                        let mut __rep_count32 = 0usize;
                        while __rep_count32 < 4294967295 {
                            let __rep_cp33 = state.offset;
                            if !{
                                let __pretty_cp30 = state.offset;
                                let __pretty_bcp31 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            let __start = state.offset;
                                            if {
                                                let __start = state.offset;
                                                let __end = state.src_bytes.len();
                                                let mut __pos = __start;
                                                while __pos < __end {
                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                    if (__b >= b'\t' && __b <= b'\n')
                                                        || (__b >= b'\x0C' && __b <= b'\r') || __b == b' '
                                                    {
                                                        __pos += 1;
                                                    } else {
                                                        break;
                                                    }
                                                }
                                                state.offset = __pos;
                                                Some(::parse_that::Span::new(__start, __pos, state.src))
                                            }
                                                .is_none()
                                            {
                                                return false;
                                            }
                                            let __matched = &state.src[__start..state.offset];
                                            if !__matched.is_empty() {
                                                __builder.text(__matched);
                                            }
                                        };
                                        if !Self::__concatenation_prettify(state, __builder) {
                                            return false;
                                        }
                                        {
                                            let __start = state.offset;
                                            if {
                                                let __start = state.offset;
                                                let __end = state.src_bytes.len();
                                                let mut __pos = __start;
                                                while __pos < __end {
                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                    if (__b >= b'\t' && __b <= b'\n')
                                                        || (__b >= b'\x0C' && __b <= b'\r') || __b == b' '
                                                    {
                                                        __pos += 1;
                                                    } else {
                                                        break;
                                                    }
                                                }
                                                state.offset = __pos;
                                                Some(::parse_that::Span::new(__start, __pos, state.src))
                                            }
                                                .is_none()
                                            {
                                                return false;
                                            }
                                            let __matched = &state.src[__start..state.offset];
                                            if !__matched.is_empty() {
                                                __builder.text(__matched);
                                            }
                                        };
                                        {
                                            let _ = {
                                                let __pretty_cp28 = state.offset;
                                                let __pretty_bcp29 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        if state.src_bytes.get(state.offset).copied() != Some(b'|')
                                                        {
                                                            return false;
                                                        }
                                                        state.offset += 1;
                                                        __builder.char(b'|');
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp28;
                                                    __builder.restore(__pretty_bcp29);
                                                }
                                                __ok
                                            };
                                            true
                                        };
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp30;
                                    __builder.restore(__pretty_bcp31);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp33;
                                break;
                            }
                            if state.offset == __rep_cp33 {
                                break;
                            }
                            __rep_count32 += 1;
                        }
                        if __rep_count32 < 1 {
                            state.offset = __rep_start34;
                            __builder.restore(__rep_bcp35);
                            return false;
                        }
                    };
                    true
                }
            };
            __builder.group_close();
            __pretty_ok
        }
        pub fn alternation_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__alternation_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __rhs_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                if !Self::__alternation_prettify(state, __builder) {
                    return false;
                }
                true
            }
        }
        pub fn rhs_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__rhs_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __term_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __byte = match state.src_bytes.get(state.offset) {
                        Some(&b) => b,
                        None => return false,
                    };
                    match __byte {
                        b'(' => {
                            {
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'(')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b'(');
                                };
                                {
                                    let __start = state.offset;
                                    if {
                                        let __start = state.offset;
                                        let __end = state.src_bytes.len();
                                        let mut __pos = __start;
                                        while __pos < __end {
                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                            if (__b >= b'\t' && __b <= b'\n')
                                                || (__b >= b'\x0C' && __b <= b'\r') || __b == b' '
                                            {
                                                __pos += 1;
                                            } else {
                                                break;
                                            }
                                        }
                                        state.offset = __pos;
                                        Some(::parse_that::Span::new(__start, __pos, state.src))
                                    }
                                        .is_none()
                                    {
                                        return false;
                                    }
                                    let __matched = &state.src[__start..state.offset];
                                    if !__matched.is_empty() {
                                        __builder.text(__matched);
                                    }
                                };
                                if !Self::__rhs_prettify(state, __builder) {
                                    return false;
                                }
                                {
                                    let __start = state.offset;
                                    if {
                                        let __start = state.offset;
                                        let __end = state.src_bytes.len();
                                        let mut __pos = __start;
                                        while __pos < __end {
                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                            if (__b >= b'\t' && __b <= b'\n')
                                                || (__b >= b'\x0C' && __b <= b'\r') || __b == b' '
                                            {
                                                __pos += 1;
                                            } else {
                                                break;
                                            }
                                        }
                                        state.offset = __pos;
                                        Some(::parse_that::Span::new(__start, __pos, state.src))
                                    }
                                        .is_none()
                                    {
                                        return false;
                                    }
                                    let __matched = &state.src[__start..state.offset];
                                    if !__matched.is_empty() {
                                        __builder.text(__matched);
                                    }
                                };
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b')')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b')');
                                };
                            };
                        }
                        b'[' => {
                            {
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'[')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b'[');
                                };
                                {
                                    let __start = state.offset;
                                    if {
                                        let __start = state.offset;
                                        let __end = state.src_bytes.len();
                                        let mut __pos = __start;
                                        while __pos < __end {
                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                            if (__b >= b'\t' && __b <= b'\n')
                                                || (__b >= b'\x0C' && __b <= b'\r') || __b == b' '
                                            {
                                                __pos += 1;
                                            } else {
                                                break;
                                            }
                                        }
                                        state.offset = __pos;
                                        Some(::parse_that::Span::new(__start, __pos, state.src))
                                    }
                                        .is_none()
                                    {
                                        return false;
                                    }
                                    let __matched = &state.src[__start..state.offset];
                                    if !__matched.is_empty() {
                                        __builder.text(__matched);
                                    }
                                };
                                if !Self::__rhs_prettify(state, __builder) {
                                    return false;
                                }
                                {
                                    let __start = state.offset;
                                    if {
                                        let __start = state.offset;
                                        let __end = state.src_bytes.len();
                                        let mut __pos = __start;
                                        while __pos < __end {
                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                            if (__b >= b'\t' && __b <= b'\n')
                                                || (__b >= b'\x0C' && __b <= b'\r') || __b == b' '
                                            {
                                                __pos += 1;
                                            } else {
                                                break;
                                            }
                                        }
                                        state.offset = __pos;
                                        Some(::parse_that::Span::new(__start, __pos, state.src))
                                    }
                                        .is_none()
                                    {
                                        return false;
                                    }
                                    let __matched = &state.src[__start..state.offset];
                                    if !__matched.is_empty() {
                                        __builder.text(__matched);
                                    }
                                };
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b']')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b']');
                                };
                            };
                        }
                        b'{' => {
                            {
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'{')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b'{');
                                };
                                {
                                    let __start = state.offset;
                                    if {
                                        let __start = state.offset;
                                        let __end = state.src_bytes.len();
                                        let mut __pos = __start;
                                        while __pos < __end {
                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                            if (__b >= b'\t' && __b <= b'\n')
                                                || (__b >= b'\x0C' && __b <= b'\r') || __b == b' '
                                            {
                                                __pos += 1;
                                            } else {
                                                break;
                                            }
                                        }
                                        state.offset = __pos;
                                        Some(::parse_that::Span::new(__start, __pos, state.src))
                                    }
                                        .is_none()
                                    {
                                        return false;
                                    }
                                    let __matched = &state.src[__start..state.offset];
                                    if !__matched.is_empty() {
                                        __builder.text(__matched);
                                    }
                                };
                                if !Self::__rhs_prettify(state, __builder) {
                                    return false;
                                }
                                {
                                    let __start = state.offset;
                                    if {
                                        let __start = state.offset;
                                        let __end = state.src_bytes.len();
                                        let mut __pos = __start;
                                        while __pos < __end {
                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                            if (__b >= b'\t' && __b <= b'\n')
                                                || (__b >= b'\x0C' && __b <= b'\r') || __b == b' '
                                            {
                                                __pos += 1;
                                            } else {
                                                break;
                                            }
                                        }
                                        state.offset = __pos;
                                        Some(::parse_that::Span::new(__start, __pos, state.src))
                                    }
                                        .is_none()
                                    {
                                        return false;
                                    }
                                    let __matched = &state.src[__start..state.offset];
                                    if !__matched.is_empty() {
                                        __builder.text(__matched);
                                    }
                                };
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'}')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b'}');
                                };
                            };
                        }
                        b'"' | b'\'' => {
                            if !Self::__terminal_prettify(state, __builder) {
                                return false;
                            }
                        }
                        b'A' | b'B' | b'C' | b'D' | b'E' | b'F' | b'G' | b'H' | b'I'
                        | b'J' | b'K' | b'L' | b'M' | b'N' | b'O' | b'P' | b'Q' | b'R'
                        | b'S' | b'T' | b'U' | b'V' | b'W' | b'X' | b'Y' | b'Z' | b'a'
                        | b'b' | b'c' | b'd' | b'e' | b'f' | b'g' | b'h' | b'i' | b'j'
                        | b'k' | b'l' | b'm' | b'n' | b'o' | b'p' | b'q' | b'r' | b's'
                        | b't' | b'u' | b'v' | b'w' | b'x' | b'y' | b'z' => {
                            if !Self::__identifier_prettify(state, __builder) {
                                return false;
                            }
                        }
                        _ => {
                            return false;
                        }
                    }
                };
                true
            }
        }
        pub fn term_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__term_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __factor_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !Self::__term_prettify(state, __builder) {
                        return false;
                    }
                    {
                        {
                            let __start = state.offset;
                            if {
                                let __start = state.offset;
                                let __end = state.src_bytes.len();
                                let mut __pos = __start;
                                while __pos < __end {
                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                    if (__b >= b'\t' && __b <= b'\n')
                                        || (__b >= b'\x0C' && __b <= b'\r') || __b == b' '
                                    {
                                        __pos += 1;
                                    } else {
                                        break;
                                    }
                                }
                                state.offset = __pos;
                                Some(::parse_that::Span::new(__start, __pos, state.src))
                            }
                                .is_none()
                            {
                                return false;
                            }
                            let __matched = &state.src[__start..state.offset];
                            if !__matched.is_empty() {
                                __builder.text(__matched);
                            }
                        };
                        {
                            let Some(&__byte) = state.src_bytes.get(state.offset) else {
                                return true;
                            };
                            match __byte {
                                b'?' => {
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'?')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b'?');
                                    };
                                }
                                b'*' => {
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b'*');
                                    };
                                }
                                b'+' => {
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'+')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b'+');
                                    };
                                }
                                b'-' => {
                                    {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'-');
                                        };
                                        {
                                            let __start = state.offset;
                                            if {
                                                let __start = state.offset;
                                                let __end = state.src_bytes.len();
                                                let mut __pos = __start;
                                                while __pos < __end {
                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                    if (__b >= b'\t' && __b <= b'\n')
                                                        || (__b >= b'\x0C' && __b <= b'\r') || __b == b' '
                                                    {
                                                        __pos += 1;
                                                    } else {
                                                        break;
                                                    }
                                                }
                                                state.offset = __pos;
                                                Some(::parse_that::Span::new(__start, __pos, state.src))
                                            }
                                                .is_none()
                                            {
                                                return false;
                                            }
                                            let __matched = &state.src[__start..state.offset];
                                            if !__matched.is_empty() {
                                                __builder.text(__matched);
                                            }
                                        };
                                        if !Self::__term_prettify(state, __builder) {
                                            return false;
                                        }
                                    };
                                }
                                b'\t' | b'\n' | b'\x0C' | b'\r' | b' ' => {}
                                _ => {}
                            }
                        };
                    };
                };
                true
            }
        }
        pub fn factor_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__factor_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __rule_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            let __pretty_ok = {
                {
                    {
                        if !Self::__identifier_prettify(state, __builder) {
                            return false;
                        }
                        {
                            let __start = state.offset;
                            if {
                                let __start = state.offset;
                                let __end = state.src_bytes.len();
                                let mut __pos = __start;
                                while __pos < __end {
                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                    if (__b >= b'\t' && __b <= b'\n')
                                        || (__b >= b'\x0C' && __b <= b'\r') || __b == b' '
                                    {
                                        __pos += 1;
                                    } else {
                                        break;
                                    }
                                }
                                state.offset = __pos;
                                Some(::parse_that::Span::new(__start, __pos, state.src))
                            }
                                .is_none()
                            {
                                return false;
                            }
                            let __matched = &state.src[__start..state.offset];
                            if !__matched.is_empty() {
                                __builder.text(__matched);
                            }
                        };
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'=') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'=');
                        };
                        {
                            let __start = state.offset;
                            if {
                                let __start = state.offset;
                                let __end = state.src_bytes.len();
                                let mut __pos = __start;
                                while __pos < __end {
                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                    if (__b >= b'\t' && __b <= b'\n')
                                        || (__b >= b'\x0C' && __b <= b'\r') || __b == b' '
                                    {
                                        __pos += 1;
                                    } else {
                                        break;
                                    }
                                }
                                state.offset = __pos;
                                Some(::parse_that::Span::new(__start, __pos, state.src))
                            }
                                .is_none()
                            {
                                return false;
                            }
                            let __matched = &state.src[__start..state.offset];
                            if !__matched.is_empty() {
                                __builder.text(__matched);
                            }
                        };
                        if !Self::__rhs_prettify(state, __builder) {
                            return false;
                        }
                        {
                            let __start = state.offset;
                            if {
                                let __start = state.offset;
                                let __end = state.src_bytes.len();
                                let mut __pos = __start;
                                while __pos < __end {
                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                    if (__b >= b'\t' && __b <= b'\n')
                                        || (__b >= b'\x0C' && __b <= b'\r') || __b == b' '
                                    {
                                        __pos += 1;
                                    } else {
                                        break;
                                    }
                                }
                                state.offset = __pos;
                                Some(::parse_that::Span::new(__start, __pos, state.src))
                            }
                                .is_none()
                            {
                                return false;
                            }
                            let __matched = &state.src[__start..state.offset];
                            if !__matched.is_empty() {
                                __builder.text(__matched);
                            }
                        };
                        {
                            let __byte = match state.src_bytes.get(state.offset) {
                                Some(&b) => b,
                                None => return false,
                            };
                            match __byte {
                                b';' => {
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b';')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b';');
                                    };
                                }
                                b'.' => {
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b'.');
                                    };
                                }
                                _ => {
                                    return false;
                                }
                            }
                        };
                    };
                    true
                }
            };
            __builder.group_close();
            __pretty_ok
        }
        pub fn rule_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__rule_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __grammar_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let mut __rep_count37 = 0usize;
                    while __rep_count37 < 4294967295 {
                        let __rep_cp38 = state.offset;
                        let __iter_cp = if __rep_count37 > 0 {
                            Some(__builder.checkpoint())
                        } else {
                            None
                        };
                        if __rep_count37 > 0 {
                            __builder.hardline();
                        }
                        if !{
                            let __pretty_cp36 = state.offset;
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __start = state.offset;
                                        if {
                                            let __start = state.offset;
                                            let __end = state.src_bytes.len();
                                            let mut __pos = __start;
                                            while __pos < __end {
                                                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                if (__b >= b'\t' && __b <= b'\n')
                                                    || (__b >= b'\x0C' && __b <= b'\r') || __b == b' '
                                                {
                                                    __pos += 1;
                                                } else {
                                                    break;
                                                }
                                            }
                                            state.offset = __pos;
                                            Some(::parse_that::Span::new(__start, __pos, state.src))
                                        }
                                            .is_none()
                                        {
                                            return false;
                                        }
                                        let __matched = &state.src[__start..state.offset];
                                        if !__matched.is_empty() {
                                            __builder.text(__matched);
                                        }
                                    };
                                    if !Self::__rule_prettify(state, __builder) {
                                        return false;
                                    }
                                    {
                                        let __start = state.offset;
                                        if {
                                            let __start = state.offset;
                                            let __end = state.src_bytes.len();
                                            let mut __pos = __start;
                                            while __pos < __end {
                                                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                if (__b >= b'\t' && __b <= b'\n')
                                                    || (__b >= b'\x0C' && __b <= b'\r') || __b == b' '
                                                {
                                                    __pos += 1;
                                                } else {
                                                    break;
                                                }
                                            }
                                            state.offset = __pos;
                                            Some(::parse_that::Span::new(__start, __pos, state.src))
                                        }
                                            .is_none()
                                        {
                                            return false;
                                        }
                                        let __matched = &state.src[__start..state.offset];
                                        if !__matched.is_empty() {
                                            __builder.text(__matched);
                                        }
                                    };
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp36;
                            }
                            __ok
                        } {
                            state.offset = __rep_cp38;
                            if let Some(__bcp) = __iter_cp {
                                __builder.restore(__bcp);
                            }
                            break;
                        }
                        if state.offset == __rep_cp38 {
                            if let Some(__bcp) = __iter_cp {
                                __builder.restore(__bcp);
                            }
                            break;
                        }
                        __rep_count37 += 1;
                    }
                };
                true
            }
        }
        pub fn grammar_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__grammar_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        pub fn serialize_letter<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: EbnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_digit<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: EbnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_symbol<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: EbnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_identifier<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: EbnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_character<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: EbnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_terminal<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: EbnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_concatenation<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: EbnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_alternation<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: EbnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_rhs<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: EbnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_term<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: EbnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_factor<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: EbnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_rule<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: EbnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_grammar<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: EbnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        fn __dispatch_serialize<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: EbnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            match __v.variant_idx() {
                0u8 => {
                    Self::serialize_letter(__v, __ser);
                }
                1u8 => {
                    Self::serialize_digit(__v, __ser);
                }
                2u8 => {
                    Self::serialize_symbol(__v, __ser);
                }
                3u8 => {
                    Self::serialize_identifier(__v, __ser);
                }
                4u8 => {
                    Self::serialize_character(__v, __ser);
                }
                5u8 => {
                    Self::serialize_terminal(__v, __ser);
                }
                6u8 => {
                    Self::serialize_concatenation(__v, __ser);
                }
                7u8 => {
                    Self::serialize_alternation(__v, __ser);
                }
                8u8 => {
                    Self::serialize_rhs(__v, __ser);
                }
                9u8 => {
                    Self::serialize_term(__v, __ser);
                }
                10u8 => {
                    Self::serialize_factor(__v, __ser);
                }
                11u8 => {
                    Self::serialize_rule(__v, __ser);
                }
                12u8 => {
                    Self::serialize_grammar(__v, __ser);
                }
                _ => {
                    __ser.text(__v.span_text());
                }
            }
        }
        pub fn serialize_compact<'a>(__v: EbnfParserNodeView<'a>) -> String {
            let mut __ser = ::bbnf_ser::StringSerializer::new();
            Self::serialize_grammar(__v, &mut __ser);
            __ser.finish()
        }
        pub fn serialize<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: EbnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            Self::serialize_grammar(__v, __ser);
        }
        /// AW-IV.W1.δ — associated-constant accessor for the
        /// grammar's consolidated codegen fingerprint. Alias
        /// of the module-scope `GRAMMAR_PROFILE` const; the
        /// underlying bytes live in `.rodata` once. Downstream
        /// consumers (wire-contract tests, per-grammar
        /// introspection, cross-grammar harnesses) use
        /// `<Grammar>::GRAMMAR_PROFILE` to disambiguate when
        /// multiple grammars coexist in the same test file —
        /// the module-scope `pub use ...::*` would otherwise
        /// collide on the unqualified `GRAMMAR_PROFILE` name.
        pub const GRAMMAR_PROFILE: crate::runtime::tape::GrammarProfile = GRAMMAR_PROFILE;
        /// AY.W6.2 — associated-constant accessor for the
        /// grammar's direct-to-struct projection admission
        /// list. Alias of the module-scope
        /// `PROJECTION_DIRECT_TO_STRUCT` slice; downstream
        /// consumers that coexist with multiple grammars in
        /// one test binary read via
        /// `<Grammar>::PROJECTION_DIRECT_TO_STRUCT` to
        /// disambiguate.
        pub const PROJECTION_DIRECT_TO_STRUCT: &'static [(&'static str, &'static str)] = PROJECTION_DIRECT_TO_STRUCT;
        /// AY-II.W0.d — grammar-declared `-> Name` bindings
        /// per admission. Indexed in lockstep with
        /// `PROJECTION_DIRECT_TO_STRUCT`; empty string when
        /// the admission came from a pure layout arm.
        #[doc(hidden)]
        pub const PROJECTION_NAMED_BINDINGS: &'static [&'static str] = PROJECTION_NAMED_BINDINGS;
        /// AY-II.W0.d — materialiser function names per
        /// admission. Canonical wire-contract evidence that
        /// every `PROJECTION_DIRECT_TO_STRUCT` entry has a
        /// matching `materialize_projection_<rule>_<Grammar>`
        /// fn in the emitter output.
        #[doc(hidden)]
        pub const PROJECTION_MATERIALIZERS: &'static [&'static str] = PROJECTION_MATERIALIZERS;
        /// AY-II.W0.d — production consumer names per
        /// admission. Each entry identifies the
        /// `<Grammar>Value::<RuleName>` variant that consumes
        /// the admitted rule at runtime.
        #[doc(hidden)]
        pub const PROJECTION_CONSUMERS: &'static [&'static str] = PROJECTION_CONSUMERS;
        /// Parse an input string and return a zero-copy
        /// `Parsed<'_, Self>` that borrows the input directly.
        ///
        /// AY-II.W0'.a: `parse()` routes through the shape
        /// dispatcher against a single `Tape<R>`. The
        /// hot path here:
        ///
        /// 1. Allocate a sized `Tape<R>` — owns both
        ///    tape + value-frame substrates in one handle.
        /// 2. Call the shape dispatcher, which decomposes
        ///    into per-shape bodies inlined at the call
        ///    site. Every compound / leaf push stamps both
        ///    column families atomically.
        /// 3. Finalise via `Tape<R>::finish_fused::<Self>`
        ///    — returns `Tape<Self>` holding tape +
        ///    value, handed to `Parsed::new_fused_output` directly.
        pub fn parse(
            input: &str,
        ) -> ::core::result::Result<
            crate::runtime::ebnf::EbnfDocument<'_>,
            crate::runtime::ParseErr,
        > {
            let __input_bytes = input.as_bytes();
            let mut state = __shape_support_EbnfParser::ScanState::new();
            let mut builder = crate::runtime::ebnf::EbnfStructBuilder::new();
            {
                let mut pos: usize = 0;
                parse_EbnfParser_grammar(
                        __input_bytes,
                        &mut pos,
                        &mut state,
                        &mut builder,
                    )
                    .map_err(|e| match e {
                        crate::runtime::tape::DtaError::Syntax { offset, .. } => {
                            crate::runtime::ParseErr::Syntax {
                                offset,
                                rule: None,
                            }
                        }
                        crate::runtime::tape::DtaError::UnexpectedEnd { offset } => {
                            crate::runtime::ParseErr::Syntax {
                                offset,
                                rule: None,
                            }
                        }
                        crate::runtime::tape::DtaError::InvalidState { .. } => {
                            crate::runtime::ParseErr::Syntax {
                                offset: 0,
                                rule: None,
                            }
                        }
                    })?;
                let _ = __shape_support_EbnfParser::skip_space(
                    __input_bytes,
                    &mut pos,
                    &mut state,
                );
                if pos != input.len() {
                    return Err(crate::runtime::ParseErr::Syntax {
                        offset: pos as u32,
                        rule: None,
                    });
                }
            }
            ::core::result::Result::Ok(builder.finalise(input))
        }
    }
    impl<'p> identifierView<'p> {
        /// Identifier text — slice of the owning `Parsed`'s
        /// input covered by this view's record span.
        #[inline]
        pub fn identifier_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
    }
    /// Walk `cursor`'s sub-tree depth-first and return the text
    /// of the first reachable identifier record. Returns `""`
    /// when no identifier is reachable.
    #[inline]
    pub(crate) fn cst_identifier_text<'p>(
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    ) -> &'p str {
        match cst_find_identifier_cursor(cursor, 3u8) {
            ::core::option::Option::Some(found) => {
                let (lo, hi) = found.span();
                &input[lo as usize..hi as usize]
            }
            ::core::option::Option::None => "",
        }
    }
    /// Walk `cursor`'s sub-tree depth-first and return the
    /// `(lo, hi)` span of the first reachable identifier record.
    /// Returns `(0, 0)` when no identifier is reachable.
    #[inline]
    pub(crate) fn cst_identifier_span<'p>(
        cursor: crate::runtime::tape::TapeCursor<'p>,
        _input: &'p str,
    ) -> (u32, u32) {
        cst_find_identifier_cursor(cursor, 3u8).map(|c| c.span()).unwrap_or((0, 0))
    }
    /// DFS helper shared by `cst_identifier_text` and
    /// `cst_identifier_span`. Returns the first cursor under
    /// `start` whose `variant_idx` matches `target_idx`.
    #[inline]
    fn cst_find_identifier_cursor<'p>(
        start: crate::runtime::tape::TapeCursor<'p>,
        target_idx: u8,
    ) -> ::core::option::Option<crate::runtime::tape::TapeCursor<'p>> {
        if start.variant_idx() == target_idx {
            return ::core::option::Option::Some(start);
        }
        for child in start.children() {
            if let ::core::option::Option::Some(found) = cst_find_identifier_cursor(
                child,
                target_idx,
            ) {
                return ::core::option::Option::Some(found);
            }
        }
        ::core::option::Option::None
    }
}
pub use __ebnfparser_emit_impl::*;
