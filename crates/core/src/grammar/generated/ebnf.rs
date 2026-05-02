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
    static __GRAMMAR_STRUCTURAL_ALPHABET: [u8; 89usize] = [
        8, 9, 10, 12, 13, 32, 34, 39, 40, 41, 42, 43, 44, 45, 46, 48, 49, 50, 51, 52, 53,
        54, 55, 56, 57, 59, 60, 61, 62, 63, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
        76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 95, 97,
        98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113,
        114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125,
    ];
    pub const GRAMMAR_STRUCTURAL_ALPHABET: &[u8] = &__GRAMMAR_STRUCTURAL_ALPHABET;
    pub const GRAMMAR_STRUCTURAL_DIGRAPHS: &[(u8, u8)] = &[];
    pub const GRAMMAR_STRUCTURAL_DIGRAPH_MASK: [u64; 4] = [0, 0, 0, 0];
    pub const GRAMMAR_STRUCTURAL_QUOTE_CLASSES: &[u8] = &[];
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
    /// Grammar-local Pratt operator metadata.
    ///
    /// The dense LUT carries precedence, associativity, arity, and
    /// the two-byte flag. This sparse slice only carries the data
    /// needed to resolve ambiguous first bytes and stamp the
    /// grammar's operator discriminant.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct PrattEntry {
        pub byte: u8,
        pub second_byte: ::core::option::Option<u8>,
        pub op_discriminant: u8,
    }
    /// AW-III.W6.5 — aggregate dense Pratt precedence LUT.
    ///
    /// Union of every Pratt rule's packed LUT (last-write-wins
    /// per byte). See
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
    pub const PRECEDENCE_ENTRIES: &[PrattEntry] = &[];
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
                ::simd_scan::StructuralIndex,
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
        /// against the grammar's mined structural alphabet.
        /// Idempotent; consumers may call freely.
        #[inline]
        pub(crate) fn ensure_structural_index<'a>(
            state: &'a mut ScanState,
            input: &[u8],
        ) -> &'a ::simd_scan::StructuralIndex {
            state
                .structural_index
                .get_or_init(|| {
                    let alphabet = ::simd_scan::StructuralAlphabet {
                        singletons: super::GRAMMAR_STRUCTURAL_ALPHABET,
                        digraph_mask: super::GRAMMAR_STRUCTURAL_DIGRAPH_MASK,
                        digraph_pairs: super::GRAMMAR_STRUCTURAL_DIGRAPHS,
                        quote_classes: super::GRAMMAR_STRUCTURAL_QUOTE_CLASSES,
                    };
                    ::simd_scan::scan_structural(input, &alphabet)
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
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder;
        let first = __shape_support_EbnfParser::skip_space(input, p, state)
            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
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
        let __dispatch_result: ::core::result::Result<(), crate::runtime::DtaError> = (||
        {
            'try_branches: loop {
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [65u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(0u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [66u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(1u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [67u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(2u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [68u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(3u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [69u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(4u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [70u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(5u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [71u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(6u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [72u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(7u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [73u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(8u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [74u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(9u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [75u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(10u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [76u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(11u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [77u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(12u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [78u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(13u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [79u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(14u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [80u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(15u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [81u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(16u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [82u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(17u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [83u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(18u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [84u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(19u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [85u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(20u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [86u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(21u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [87u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(22u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [88u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(23u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [89u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(24u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [90u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(25u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [97u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(26u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [98u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(27u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [99u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(28u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [100u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(29u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [101u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(30u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [102u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(31u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [103u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(32u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [104u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(33u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [105u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(34u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [106u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(35u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [107u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(36u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [108u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(37u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [109u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(38u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [110u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(39u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [111u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(40u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [112u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(41u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [113u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(42u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [114u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(43u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [115u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(44u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [116u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(45u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [117u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(46u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [118u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(47u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [119u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(48u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [120u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(49u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [121u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(50u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [122u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(51u32);
                        break 'try_branches;
                    }
                }
                return Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
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
    /// fn so the target writes directly into the same
    /// builder. Returns unit for StructDirect composition.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_keyword_EbnfParser_digit<'p>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let _ = state;
        match first_byte {
            48u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [48u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder
                        .push_leaf_with_str(unsafe {
                            ::core::str::from_utf8_unchecked(&input[at..end])
                        });
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            49u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [49u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder
                        .push_leaf_with_str(unsafe {
                            ::core::str::from_utf8_unchecked(&input[at..end])
                        });
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            50u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [50u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder
                        .push_leaf_with_str(unsafe {
                            ::core::str::from_utf8_unchecked(&input[at..end])
                        });
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            51u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [51u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder
                        .push_leaf_with_str(unsafe {
                            ::core::str::from_utf8_unchecked(&input[at..end])
                        });
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            52u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [52u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder
                        .push_leaf_with_str(unsafe {
                            ::core::str::from_utf8_unchecked(&input[at..end])
                        });
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            53u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [53u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder
                        .push_leaf_with_str(unsafe {
                            ::core::str::from_utf8_unchecked(&input[at..end])
                        });
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            54u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [54u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder
                        .push_leaf_with_str(unsafe {
                            ::core::str::from_utf8_unchecked(&input[at..end])
                        });
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            55u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [55u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder
                        .push_leaf_with_str(unsafe {
                            ::core::str::from_utf8_unchecked(&input[at..end])
                        });
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            56u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [56u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder
                        .push_leaf_with_str(unsafe {
                            ::core::str::from_utf8_unchecked(&input[at..end])
                        });
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            57u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [57u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder
                        .push_leaf_with_str(unsafe {
                            ::core::str::from_utf8_unchecked(&input[at..end])
                        });
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            _ => {
                ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
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
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder;
        let first = __shape_support_EbnfParser::skip_space(input, p, state)
            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
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
        let __dispatch_result: ::core::result::Result<(), crate::runtime::DtaError> = (||
        {
            'try_branches: loop {
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [91u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(0u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [93u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(1u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [123u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(2u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [125u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(3u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [40u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(4u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [41u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(5u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [60u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(6u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [62u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(7u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [39u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(8u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [34u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(9u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [61u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(10u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [124u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(11u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [46u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(12u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [44u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(13u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [59u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(14u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [45u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(15u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [43u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(16u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [42u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(17u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [63u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(18u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [10u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(19u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [9u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(20u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [13u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(21u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [12u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(22u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [8u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(23u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [92u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(24u32);
                        break 'try_branches;
                    }
                }
                return Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
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
    /// Compound emission lands as typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    ///
    /// AZ-III.W2.4.r — content-only bodies (no Ref /
    /// TokenDispatch in the IR) capture `*p` before and after
    /// the per-position emission and push one synthetic Span
    /// leaf carrying the consumed source slice; this restores
    /// the contract `bootstrap_parser` met for `regex` /
    /// `literal` / `comment` / `big_comment` / `import_path`
    /// (all flat-shape rules whose grammar projection is
    /// `-> Span` or whose host walker reads via `byte_span()`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_EbnfParser_identifier<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
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
        <crate::runtime::ebnf::EbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
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
                            crate::runtime::DtaError,
                        > = (|| {
                            'try_branches: loop {
                                {
                                    let __alt_save_p = *p;
                                    let __alt_builder_checkpoint = builder.checkpoint();
                                    let __alt_result: ::core::result::Result<
                                        (),
                                        crate::runtime::DtaError,
                                    > = (|| {
                                        let at = *p;
                                        let end = at + 1usize;
                                        if input.len() < end || input[at..end] != [95u8] {
                                            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                                offset: at as u32,
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
                                        crate::runtime::DtaError,
                                    > = (|| {
                                        let _ = ({
                                            let __first = __shape_support_EbnfParser::skip_space(
                                                    input,
                                                    p,
                                                    state,
                                                )
                                                .ok_or(crate::runtime::DtaError::UnexpectedEnd {
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
                                        crate::runtime::DtaError,
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
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: *p as u32,
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
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: *p as u32,
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
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::ebnf::EbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __identifier_handle,
                );
                ::core::result::Result::Ok(())
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
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder;
        let first = __shape_support_EbnfParser::skip_space(input, p, state)
            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
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
        let __dispatch_result: ::core::result::Result<(), crate::runtime::DtaError> = (||
        {
            'try_branches: loop {
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [95u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
                        builder.push_branch_tag(0u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() >= end && input[at..end] == [32u8] {
                        *p = end;
                        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(
                            builder,
                        );
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
                            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
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
                return Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
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
    /// fn so the target writes directly into the same
    /// builder. Returns unit for StructDirect composition.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_keyword_EbnfParser_terminal<'p>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let _ = state;
        match first_byte {
            34u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [34u8] {
                    let __seq_span_lo = *p;
                    let __seq_builder_checkpoint = builder.checkpoint();
                    let __seq_result: ::core::result::Result<
                        (),
                        crate::runtime::DtaError,
                    > = (|| {
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [34u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                        }
                        {
                            let __minus_save_p = *p;
                            let __minus_builder_checkpoint = builder.checkpoint();
                            let __minus_excl: ::core::result::Result<
                                (),
                                crate::runtime::DtaError,
                            > = (|| {
                                {
                                    let at = *p;
                                    let end = at + 1usize;
                                    if input.len() < end || input[at..end] != [34u8] {
                                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                            offset: at as u32,
                                        });
                                    }
                                    *p = end;
                                }
                                ::core::result::Result::Ok(())
                            })();
                            *p = __minus_save_p;
                            builder.rollback(__minus_builder_checkpoint);
                            if __minus_excl.is_ok() {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: *p as u32,
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
                                    crate::runtime::DtaError,
                                > = (|| {
                                    {
                                        let __minus_save_p = *p;
                                        let __minus_builder_checkpoint = builder.checkpoint();
                                        let __minus_excl: ::core::result::Result<
                                            (),
                                            crate::runtime::DtaError,
                                        > = (|| {
                                            {
                                                let at = *p;
                                                let end = at + 1usize;
                                                if input.len() < end || input[at..end] != [34u8] {
                                                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                                        offset: at as u32,
                                                    });
                                                }
                                                *p = end;
                                            }
                                            ::core::result::Result::Ok(())
                                        })();
                                        *p = __minus_save_p;
                                        builder.rollback(__minus_builder_checkpoint);
                                        if __minus_excl.is_ok() {
                                            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                                offset: *p as u32,
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
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: *p as u32,
                                });
                            }
                        }
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [34u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
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
                            return ::core::result::Result::Ok(());
                        }
                        ::core::result::Result::Err(__err) => {
                            *p = __seq_span_lo;
                            builder.rollback(__seq_builder_checkpoint);
                            return ::core::result::Result::Err(__err);
                        }
                    }
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            39u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [39u8] {
                    let __seq_span_lo = *p;
                    let __seq_builder_checkpoint = builder.checkpoint();
                    let __seq_result: ::core::result::Result<
                        (),
                        crate::runtime::DtaError,
                    > = (|| {
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [39u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                        }
                        {
                            let __minus_save_p = *p;
                            let __minus_builder_checkpoint = builder.checkpoint();
                            let __minus_excl: ::core::result::Result<
                                (),
                                crate::runtime::DtaError,
                            > = (|| {
                                {
                                    let at = *p;
                                    let end = at + 1usize;
                                    if input.len() < end || input[at..end] != [39u8] {
                                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                            offset: at as u32,
                                        });
                                    }
                                    *p = end;
                                }
                                ::core::result::Result::Ok(())
                            })();
                            *p = __minus_save_p;
                            builder.rollback(__minus_builder_checkpoint);
                            if __minus_excl.is_ok() {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: *p as u32,
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
                                    crate::runtime::DtaError,
                                > = (|| {
                                    {
                                        let __minus_save_p = *p;
                                        let __minus_builder_checkpoint = builder.checkpoint();
                                        let __minus_excl: ::core::result::Result<
                                            (),
                                            crate::runtime::DtaError,
                                        > = (|| {
                                            {
                                                let at = *p;
                                                let end = at + 1usize;
                                                if input.len() < end || input[at..end] != [39u8] {
                                                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                                        offset: at as u32,
                                                    });
                                                }
                                                *p = end;
                                            }
                                            ::core::result::Result::Ok(())
                                        })();
                                        *p = __minus_save_p;
                                        builder.rollback(__minus_builder_checkpoint);
                                        if __minus_excl.is_ok() {
                                            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                                offset: *p as u32,
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
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: *p as u32,
                                });
                            }
                        }
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [39u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
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
                            return ::core::result::Result::Ok(());
                        }
                        ::core::result::Result::Err(__err) => {
                            *p = __seq_span_lo;
                            builder.rollback(__seq_builder_checkpoint);
                            return ::core::result::Result::Err(__err);
                        }
                    }
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            _ => {
                ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                })
            }
        }
    }
    /// AZ-I.W2.RF — per-grammar Flat-shape parse function,
    /// **struct-direct body**. Targets the grammar's concrete
    /// `StructBuilder` (JSON / CSS L4 / Sheets per the
    /// resolver's `SubstrateBinding`).
    ///
    /// Compound emission lands as typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    ///
    /// AZ-III.W2.4.r — content-only bodies (no Ref /
    /// TokenDispatch in the IR) capture `*p` before and after
    /// the per-position emission and push one synthetic Span
    /// leaf carrying the consumed source slice; this restores
    /// the contract `bootstrap_parser` met for `regex` /
    /// `literal` / `comment` / `big_comment` / `import_path`
    /// (all flat-shape rules whose grammar projection is
    /// `-> Span` or whose host walker reads via `byte_span()`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_EbnfParser_concatenation<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
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
        <crate::runtime::ebnf::EbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
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
                            crate::runtime::DtaError,
                        > = (|| {
                            {
                                let __scan_start = *p;
                                let Some(match_len) = __regex_scan_EbnfParser(
                                    "[ \\t\\n\\r\\f]*",
                                    input,
                                    *p,
                                ) else {
                                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                        offset: __scan_start as u32,
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
                                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                        offset: __scan_start as u32,
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
                                        crate::runtime::DtaError,
                                    > = (|| {
                                        let at = *p;
                                        let end = at + 1usize;
                                        if input.len() < end || input[at..end] != [44u8] {
                                            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                                offset: at as u32,
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
                                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                        offset: *p as u32,
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
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: *p as u32,
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
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::ebnf::EbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __concatenation_handle,
                );
                ::core::result::Result::Ok(())
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
    /// Compound emission lands as typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    ///
    /// AZ-III.W2.4.r — content-only bodies (no Ref /
    /// TokenDispatch in the IR) capture `*p` before and after
    /// the per-position emission and push one synthetic Span
    /// leaf carrying the consumed source slice; this restores
    /// the contract `bootstrap_parser` met for `regex` /
    /// `literal` / `comment` / `big_comment` / `import_path`
    /// (all flat-shape rules whose grammar projection is
    /// `-> Span` or whose host walker reads via `byte_span()`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_EbnfParser_alternation<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
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
        <crate::runtime::ebnf::EbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
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
                            crate::runtime::DtaError,
                        > = (|| {
                            {
                                let __scan_start = *p;
                                let Some(match_len) = __regex_scan_EbnfParser(
                                    "[ \\t\\n\\r\\f]*",
                                    input,
                                    *p,
                                ) else {
                                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                        offset: __scan_start as u32,
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
                                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                        offset: __scan_start as u32,
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
                                        crate::runtime::DtaError,
                                    > = (|| {
                                        let at = *p;
                                        let end = at + 1usize;
                                        if input.len() < end || input[at..end] != [124u8] {
                                            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                                offset: at as u32,
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
                                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                        offset: *p as u32,
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
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: *p as u32,
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
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::ebnf::EbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __alternation_handle,
                );
                ::core::result::Result::Ok(())
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
    /// expression names `builder` against the concrete
    /// struct-builder.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_scalar_EbnfParser_rhs<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
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
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder;
        let first = __shape_support_EbnfParser::skip_space(input, p, state)
            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
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
        let __dispatch_result: ::core::result::Result<(), crate::runtime::DtaError> = (||
        {
            'try_branches: loop {
                {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    let attempt: ::core::result::Result<(), crate::runtime::DtaError> = (||
                    {
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [40u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
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
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: __scan_start as u32,
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
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: __scan_start as u32,
                                });
                            };
                            *p += match_len as usize;
                        }
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [41u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
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
                    let attempt: ::core::result::Result<(), crate::runtime::DtaError> = (||
                    {
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [91u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
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
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: __scan_start as u32,
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
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: __scan_start as u32,
                                });
                            };
                            *p += match_len as usize;
                        }
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [93u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
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
                    let attempt: ::core::result::Result<(), crate::runtime::DtaError> = (||
                    {
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [123u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
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
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: __scan_start as u32,
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
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: __scan_start as u32,
                                });
                            };
                            *p += match_len as usize;
                        }
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [125u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
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
                            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
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
                return Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
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
    /// Compound emission lands as typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    ///
    /// AZ-III.W2.4.r — content-only bodies (no Ref /
    /// TokenDispatch in the IR) capture `*p` before and after
    /// the per-position emission and push one synthetic Span
    /// leaf carrying the consumed source slice; this restores
    /// the contract `bootstrap_parser` met for `regex` /
    /// `literal` / `comment` / `big_comment` / `import_path`
    /// (all flat-shape rules whose grammar projection is
    /// `-> Span` or whose host walker reads via `byte_span()`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_EbnfParser_factor<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
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
        <crate::runtime::ebnf::EbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
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
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
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
                            crate::runtime::DtaError,
                        > = (|| {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [63u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
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
                            crate::runtime::DtaError,
                        > = (|| {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [42u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
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
                            crate::runtime::DtaError,
                        > = (|| {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [43u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
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
                            crate::runtime::DtaError,
                        > = (|| {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [45u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
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
                                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                        offset: __scan_start as u32,
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
                            crate::runtime::DtaError,
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
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: *p as u32,
                    });
                }
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::ebnf::EbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::ebnf::EbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __factor_handle,
                );
                ::core::result::Result::Ok(())
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
    /// Compound emission lands as typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-position pushes (string keys, recursive
    /// value calls, byte literals) land directly on the topmost
    /// open frame.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge (Flat → Wrap → Flat through
    /// the grammar's `__value` discriminant). LLVM's inliner
    /// collapses plain `#[inline]` candidates only when
    /// profitable and bails cleanly on detected recursion.
    ///
    /// AZ-III.W2.4.r — content-only bodies (no Ref /
    /// TokenDispatch in the IR) capture `*p` before and after
    /// the per-position emission and push one synthetic Span
    /// leaf carrying the consumed source slice; this restores
    /// the contract `bootstrap_parser` met for `regex` /
    /// `literal` / `comment` / `big_comment` / `import_path`
    /// (all flat-shape rules whose grammar projection is
    /// `-> Span` or whose host walker reads via `byte_span()`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_EbnfParser_rule<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_EbnfParser::ScanState,
        builder: &mut crate::runtime::ebnf::EbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
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
        <crate::runtime::ebnf::EbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
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
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
            }
            {
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [61u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
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
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
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
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
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
                            crate::runtime::DtaError,
                        > = (|| {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [59u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
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
                            crate::runtime::DtaError,
                        > = (|| {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [46u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
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
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: *p as u32,
                    });
                }
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::ebnf::EbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::ebnf::EbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __rule_handle,
                );
                ::core::result::Result::Ok(())
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
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
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
            let __iter_result: ::core::result::Result<(), crate::runtime::DtaError> = (||
            {
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
        Ok(())
    }
    /// AW-V.W3.2 — top-level shape dispatcher.
    ///
    /// Mirrors the walker's `value` rule ByteDispatch: skip leading
    /// whitespace, dispatch on the first byte to the chosen branch
    /// shape fn, return unit after the chosen shape succeeds. No outer Rule /
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
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
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
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        let _ = __shape_support_EbnfParser::skip_space(input, p, state);
        parse_array_EbnfParser_grammar(input, p, state, builder)
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
        /// Parse an input string and return the grammar-specific
        /// document that owns the StructDirect runtime arena.
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
                        crate::runtime::DtaError::Syntax { offset } => {
                            crate::runtime::ParseErr::Syntax {
                                offset,
                                rule: None,
                            }
                        }
                        crate::runtime::DtaError::UnexpectedEnd { offset } => {
                            crate::runtime::ParseErr::Syntax {
                                offset,
                                rule: None,
                            }
                        }
                        crate::runtime::DtaError::InvalidState { .. } => {
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
}
pub use __ebnfparser_emit_impl::*;
