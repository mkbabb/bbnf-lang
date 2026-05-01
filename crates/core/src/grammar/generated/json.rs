//! AUTO-GENERATED from `[workspace.metadata.bbnf.grammars]` — do not edit manually.
//! Regenerate: cargo xtask regen --grammar json

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

pub struct JsonParser;
mod __jsonparser_emit_impl {
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
    pub const GRAMMAR_JsonParser: [&'static str; 1usize] = [
        include_str!(
            concat!(env!("CARGO_MANIFEST_DIR"), "/../../grammar/json/json.bbnf")
        ),
    ];
    static __GRAMMAR_STRUCTURAL_ALPHABET: [u8; 6usize] = [44, 58, 91, 93, 123, 125];
    static __GRAMMAR_STRUCTURAL_QUOTE_CLASSES: [u8; 1usize] = [34];
    pub const GRAMMAR_STRUCTURAL_ALPHABET: &[u8] = &__GRAMMAR_STRUCTURAL_ALPHABET;
    pub const GRAMMAR_STRUCTURAL_DIGRAPHS: &[(u8, u8)] = &[];
    pub const GRAMMAR_STRUCTURAL_DIGRAPH_MASK: [u64; 4] = [0, 0, 0, 0];
    pub const GRAMMAR_STRUCTURAL_QUOTE_CLASSES: &[u8] = &__GRAMMAR_STRUCTURAL_QUOTE_CLASSES;
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
    static __DTA_REGEX_4: &str = "-?(0|[1-9]\\d*)(\\.\\d+)?([eE][+-]?\\d+)?";
    static __DTA_REGEX_5: &str = "\"(?:[^\"\\\\]|\\\\(?:[\"\\\\\\/bfnrt]|u[0-9a-fA-F]{4}))*\"";
    #[inline]
    #[cold]
    fn __regex_scan_JsonParser(
        pattern: &str,
        input: &[u8],
        pos: usize,
    ) -> ::core::option::Option<u32> {
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_4.as_ptr())
            || pattern == __DTA_REGEX_4
        {
            return '__dfa: {
                let mut __dfa_state: u32 = 0;
                let mut __dfa_p: usize = pos;
                let mut __dfa_last_match: ::core::option::Option<u32> = ::core::option::Option::None;
                loop {
                    let b = match input.get(__dfa_p) {
                        ::core::option::Option::Some(&b) => b,
                        ::core::option::Option::None => break,
                    };
                    match __dfa_state {
                        0 => {
                            match b {
                                48 => __dfa_state = 1,
                                49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 3;
                                }
                                45 => __dfa_state = 5,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                69 | 101 => __dfa_state = 4,
                                46 => __dfa_state = 7,
                                _ => break,
                            }
                        }
                        2 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 2;
                                }
                                _ => break,
                            }
                        }
                        3 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 3;
                                }
                                69 | 101 => __dfa_state = 4,
                                46 => __dfa_state = 7,
                                _ => break,
                            }
                        }
                        4 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 2;
                                }
                                43 | 45 => __dfa_state = 8,
                                _ => break,
                            }
                        }
                        5 => {
                            match b {
                                48 => __dfa_state = 1,
                                49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 3;
                                }
                                _ => break,
                            }
                        }
                        6 => {
                            match b {
                                69 | 101 => __dfa_state = 4,
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 6;
                                }
                                _ => break,
                            }
                        }
                        7 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 6;
                                }
                                _ => break,
                            }
                        }
                        8 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 2;
                                }
                                _ => break,
                            }
                        }
                        _ => unsafe { ::core::hint::unreachable_unchecked() }
                    }
                    __dfa_p += 1;
                    match __dfa_state {
                        1 | 2 | 3 | 6 => {
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_5.as_ptr())
            || pattern == __DTA_REGEX_5
        {
            return '__dfa: {
                let mut __dfa_state: u32 = 0;
                let mut __dfa_p: usize = pos;
                let mut __dfa_last_match: ::core::option::Option<u32> = ::core::option::Option::None;
                loop {
                    let b = match input.get(__dfa_p) {
                        ::core::option::Option::Some(&b) => b,
                        ::core::option::Option::None => break,
                    };
                    match __dfa_state {
                        0 => {
                            match b {
                                34 => __dfa_state = 2,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                _ => break,
                            }
                        }
                        2 => {
                            match b {
                                34 => __dfa_state = 1,
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 35 | 36 | 37 | 38
                                | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49 | 50
                                | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62
                                | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74
                                | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86
                                | 87 | 88 | 89 | 90 | 91 | 93 | 94 | 95 | 96 | 97 | 98 | 99
                                | 100 | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108 | 109
                                | 110 | 111 | 112 | 113 | 114 | 115 | 116 | 117 | 118 | 119
                                | 120 | 121 | 122 | 123 | 124 | 125 | 126 | 127 | 128 | 129
                                | 130 | 131 | 132 | 133 | 134 | 135 | 136 | 137 | 138 | 139
                                | 140 | 141 | 142 | 143 | 144 | 145 | 146 | 147 | 148 | 149
                                | 150 | 151 | 152 | 153 | 154 | 155 | 156 | 157 | 158 | 159
                                | 160 | 161 | 162 | 163 | 164 | 165 | 166 | 167 | 168 | 169
                                | 170 | 171 | 172 | 173 | 174 | 175 | 176 | 177 | 178 | 179
                                | 180 | 181 | 182 | 183 | 184 | 185 | 186 | 187 | 188 | 189
                                | 190 | 191 | 192 | 193 | 194 | 195 | 196 | 197 | 198 | 199
                                | 200 | 201 | 202 | 203 | 204 | 205 | 206 | 207 | 208 | 209
                                | 210 | 211 | 212 | 213 | 214 | 215 | 216 | 217 | 218 | 219
                                | 220 | 221 | 222 | 223 | 224 | 225 | 226 | 227 | 228 | 229
                                | 230 | 231 | 232 | 233 | 234 | 235 | 236 | 237 | 238 | 239
                                | 240 | 241 | 242 | 243 | 244 | 245 | 246 | 247 | 248 | 249
                                | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 2,
                                92 => __dfa_state = 4,
                                _ => break,
                            }
                        }
                        3 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 65 | 66
                                | 67 | 68 | 69 | 70 | 97 | 98 | 99 | 100 | 101 | 102 => {
                                    __dfa_state = 7;
                                }
                                _ => break,
                            }
                        }
                        4 => {
                            match b {
                                34 | 47 | 92 | 98 | 102 | 110 | 114 | 116 => __dfa_state = 2,
                                117 => __dfa_state = 3,
                                _ => break,
                            }
                        }
                        5 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 65 | 66
                                | 67 | 68 | 69 | 70 | 97 | 98 | 99 | 100 | 101 | 102 => {
                                    __dfa_state = 2;
                                }
                                _ => break,
                            }
                        }
                        6 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 65 | 66
                                | 67 | 68 | 69 | 70 | 97 | 98 | 99 | 100 | 101 | 102 => {
                                    __dfa_state = 5;
                                }
                                _ => break,
                            }
                        }
                        7 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 65 | 66
                                | 67 | 68 | 69 | 70 | 97 | 98 | 99 | 100 | 101 | 102 => {
                                    __dfa_state = 6;
                                }
                                _ => break,
                            }
                        }
                        _ => unsafe { ::core::hint::unreachable_unchecked() }
                    }
                    __dfa_p += 1;
                    match __dfa_state {
                        1 => {
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
    #[inline(never)]
    #[cold]
    #[allow(non_snake_case)]
    fn parse_number_fallback(bytes: &[u8]) -> f64 {
        let s = unsafe { ::core::str::from_utf8_unchecked(bytes) };
        s.parse::<f64>().unwrap_or(f64::NAN)
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
    pub(crate) mod __shape_support_JsonParser {
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
    /// AZ-I.W2.RD — struct-direct Keyword-shape parse fn
    /// (single-literal body).
    ///
    /// Matches the literal byte sequence and routes the
    /// rule's projected payload through the `StructBuilder`
    /// trait surface. Returns unit on success
    /// for compositional uniformity with the tape-path
    /// emission; the offset is unused by struct-direct
    /// callers (the dispatcher discards `Ok(_)` payloads).
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_keyword_JsonParser_null(
        input: &[u8],
        p: &mut usize,
        _first_byte: u8,
        _state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'_>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let at = *p;
        let end = at + 4usize;
        if input.len() < end || input[at..end] != [110u8, 117u8, 108u8, 108u8] {
            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                offset: at as u32,
            });
        }
        *p = end;
        builder.push_leaf_with_unit();
        ::core::result::Result::Ok(())
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
    pub fn parse_keyword_JsonParser_bool<'p>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let _ = state;
        match first_byte {
            102u8 => {
                if input.len() >= *p + 5usize
                    && input[*p..*p + 5usize] == [102u8, 97u8, 108u8, 115u8, 101u8]
                {
                    let at = *p;
                    let end = at + 5usize;
                    *p = end;
                    builder.push_leaf_with_bool(false);
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            116u8 => {
                if input.len() >= *p + 4usize
                    && input[*p..*p + 4usize] == [116u8, 114u8, 117u8, 101u8]
                {
                    let at = *p;
                    let end = at + 4usize;
                    *p = end;
                    builder.push_leaf_with_bool(true);
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
    /// AZ-I.W2.RC — per-grammar Number-shape parse function
    /// (struct-direct substrate).
    ///
    /// Mirrors `json_prototype::number::parse_number_body` for the
    /// scan body; the trailing leaf push routes through
    /// `builder.push_leaf_with_f64(value)` against the
    /// grammar-specific concrete `StructBuilder` impl. `first_byte`
    /// is the byte the dispatcher already matched; passing it
    /// avoids a redundant re-read for the sign check.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_number_JsonParser_number<'p>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        const POW10_U64: [u64; 17] = [
            1, 10, 100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000, 100_000_000,
            1_000_000_000, 10_000_000_000, 100_000_000_000, 1_000_000_000_000,
            10_000_000_000_000, 100_000_000_000_000, 1_000_000_000_000_000,
            10_000_000_000_000_000,
        ];
        let _ = POW10_U64;
        let start = *p;
        let len = input.len();
        let negative = first_byte == b'-';
        if negative {
            *p += 1;
        }
        let int_start = *p;
        let mut mantissa: u64 = 0;
        let mut many_digits = false;
        while *p < len {
            let b = input[*p];
            if b.is_ascii_digit() {
                mantissa = mantissa.wrapping_mul(10).wrapping_add((b - b'0') as u64);
                *p += 1;
            } else {
                break;
            }
        }
        if *p == int_start {
            return Err(crate::runtime::DtaError::Syntax {
                offset: start as u32,
            });
        }
        let int_digit_count = *p - int_start;
        if int_digit_count > 19 {
            many_digits = true;
        }
        let mut fractional_digit_count: i64 = 0;
        if input.get(*p) == Some(&b'.') {
            *p += 1;
            let frac_start = *p;
            while *p < len {
                let b = input[*p];
                if b.is_ascii_digit() {
                    mantissa = mantissa.wrapping_mul(10).wrapping_add((b - b'0') as u64);
                    *p += 1;
                } else {
                    break;
                }
            }
            fractional_digit_count = (*p - frac_start) as i64;
            if fractional_digit_count == 0 {
                return Err(crate::runtime::DtaError::Syntax {
                    offset: start as u32,
                });
            }
            if int_digit_count as i64 + fractional_digit_count > 19 {
                many_digits = true;
            }
        }
        let mut exponent: i64 = -fractional_digit_count;
        let exp_byte = input.get(*p).copied();
        if exp_byte == Some(b'e') || exp_byte == Some(b'E') {
            *p += 1;
            let exp_negative = match input.get(*p) {
                Some(b'+') => {
                    *p += 1;
                    false
                }
                Some(b'-') => {
                    *p += 1;
                    true
                }
                _ => false,
            };
            let exp_start = *p;
            let mut exp_val: i64 = 0;
            while *p < len {
                let b = input[*p];
                if b.is_ascii_digit() {
                    exp_val = exp_val
                        .saturating_mul(10)
                        .saturating_add((b - b'0') as i64);
                    *p += 1;
                } else {
                    break;
                }
            }
            if *p == exp_start {
                return Err(crate::runtime::DtaError::Syntax {
                    offset: start as u32,
                });
            }
            exponent += if exp_negative { -exp_val } else { exp_val };
        }
        let end = *p;
        let bytes = &input[start..end];
        let value = if many_digits {
            parse_number_fallback(bytes)
        } else {
            match ::parse_that::parsers::eisel_lemire::compute_f64(
                exponent,
                mantissa,
                negative,
            ) {
                Some(v) => v,
                None => parse_number_fallback(bytes),
            }
        };
        builder.push_leaf_with_f64(value);
        Ok(())
    }
    /// AZ-I.W2.RC — per-grammar String-shape parse function
    /// (struct-direct substrate).
    ///
    /// `"` must NOT be consumed by the caller — this
    /// function reads it, scans for the closing quote, and
    /// pushes a `&'p str` leaf via the builder. The borrow
    /// path slices the input directly; the cold escape
    /// path decodes into the builder's arena and emits
    /// the decoded bytes via the same `push_leaf_with_str`
    /// surface.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_string_JsonParser_string<'p>(
        input: &'p [u8],
        p: &mut usize,
        _state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let open = *p;
        if input.get(open).copied() != Some(b'"') {
            return Err(crate::runtime::DtaError::Syntax {
                offset: open as u32,
            });
        }
        let body_start = open + 1;
        let tail = match input.get(body_start..) {
            Some(t) => t,
            None => {
                return Err(crate::runtime::DtaError::UnexpectedEnd {
                    offset: open as u32,
                });
            }
        };
        match __shape_support_JsonParser::first_quote_or_backslash(tail) {
            Some((off, b'"')) => {
                let end = body_start + off;
                *p = end + 1;
                let body: &'p str = unsafe {
                    ::core::str::from_utf8_unchecked(&input[body_start..end])
                };
                builder.push_leaf_with_str(body);
                Ok(())
            }
            Some((_off, b'\\')) => {
                let mut buf: Vec<u8> = Vec::with_capacity(
                    input.len().saturating_sub(open + 1),
                );
                match ::parse_that::parsers::scan::decode_json_string_to_arena(
                    input,
                    open,
                    &mut buf,
                ) {
                    Some(
                        (
                            ::parse_that::parsers::scan::StringPayload::Owned { .. },
                            end_pos,
                        ),
                    ) => {
                        *p = end_pos;
                        let bytes: Box<[u8]> = buf.into_boxed_slice();
                        let leaked: &'static [u8] = Box::leak(bytes);
                        let leaked_str: &'static str = unsafe {
                            ::core::str::from_utf8_unchecked(leaked)
                        };
                        builder.push_leaf_with_str(leaked_str);
                        Ok(())
                    }
                    Some(
                        (
                            ::parse_that::parsers::scan::StringPayload::Borrowed {
                                start,
                                end,
                            },
                            end_pos,
                        ),
                    ) => {
                        *p = end_pos;
                        let body: &'p str = unsafe {
                            ::core::str::from_utf8_unchecked(
                                &input[start as usize..end as usize],
                            )
                        };
                        builder.push_leaf_with_str(body);
                        Ok(())
                    }
                    None => {
                        Err(crate::runtime::DtaError::Syntax {
                            offset: open as u32,
                        })
                    }
                }
            }
            Some(_) => unreachable!(),
            None => {
                Err(crate::runtime::DtaError::UnexpectedEnd {
                    offset: open as u32,
                })
            }
        }
    }
    /// AZ-I.W2.RB — per-grammar Object-shape parse function,
    /// **struct-direct body**. Targets [`JsonStructBuilder`].
    ///
    /// Walker-tape compound emission is replaced by typed
    /// `begin_compound` / `end_compound` calls against the in-flight
    /// frame stack. Per-element pushes (string keys + value
    /// dispatch) land directly on the topmost open frame.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_object_JsonParser_object<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder;
        if input.get(*p).copied() != Some(b'{') {
            return Err(crate::runtime::DtaError::Syntax {
                offset: *p as u32,
            });
        }
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 4u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("object"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __handle = builder.begin_compound(&__layout);
        *p += 1;
        let _ = __shape_support_JsonParser::skip_space(input, p, state);
        if input.get(*p).copied() == Some(b'}') {
            *p += 1;
            builder.end_compound(__handle);
            return Ok(());
        }
        loop {
            if input.get(*p).copied() != Some(b'"') {
                return Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            parse_string_JsonParser_string(input, p, state, builder)?;
            let _ = __shape_support_JsonParser::skip_space(input, p, state);
            if input.get(*p).copied() != Some(b':') {
                return Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            *p += 1;
            let _ = __shape_support_JsonParser::skip_space(input, p, state);
            ({
                let _ = __shape_support_JsonParser::skip_space(input, p, state);
                parse_wrap_JsonParser_value(input, p, state, builder)
            })?;
            let _ = __shape_support_JsonParser::skip_space(input, p, state);
            match input.get(*p).copied() {
                Some(b',') => {
                    *p += 1;
                    let _ = __shape_support_JsonParser::skip_space(input, p, state);
                }
                Some(b'}') => {
                    *p += 1;
                    builder.end_compound(__handle);
                    return Ok(());
                }
                _ => {
                    return Err(crate::runtime::DtaError::Syntax {
                        offset: *p as u32,
                    });
                }
            }
        }
    }
    /// AZ-I.W2.RB — per-grammar Array-shape parse function,
    /// **struct-direct body** (Shape 1 — wrapped homogeneous repeat).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_array_JsonParser_array<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder;
        if input.get(*p).copied() != Some(b'[') {
            return Err(crate::runtime::DtaError::Syntax {
                offset: *p as u32,
            });
        }
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 5u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("array"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __array_checkpoint = builder.checkpoint();
        let __handle = builder.begin_compound(&__layout);
        let __array_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            *p += 1;
            let _ = __shape_support_JsonParser::skip_space(input, p, state);
            if input.get(*p).copied() == Some(b']') {
                *p += 1;
                return Ok(());
            }
            loop {
                ({
                    let _ = __shape_support_JsonParser::skip_space(input, p, state);
                    parse_wrap_JsonParser_value(input, p, state, builder)
                })?;
                let _ = __shape_support_JsonParser::skip_space(input, p, state);
                match input.get(*p).copied() {
                    Some(b',') => {
                        *p += 1;
                        let _ = __shape_support_JsonParser::skip_space(input, p, state);
                    }
                    Some(b']') => {
                        *p += 1;
                        return Ok(());
                    }
                    _ => {
                        return Err(crate::runtime::DtaError::Syntax {
                            offset: *p as u32,
                        });
                    }
                }
            }
        })();
        match __array_result {
            Ok(()) => {
                builder.end_compound(__handle);
                Ok(())
            }
            Err(__err) => {
                builder.rollback(__array_checkpoint);
                Err(__err)
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
    pub fn parse_flat_JsonParser_pair<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __pair_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 6u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("pair"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __pair_handle = <crate::runtime::json::JsonStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__pair_layout);
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = ({
                    let _ = __shape_support_JsonParser::skip_space(input, p, state);
                    parse_string_JsonParser_string(input, p, state, builder)
                })?;
            }
            {
                let _ = __shape_support_JsonParser::skip_space(input, p, state);
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [58u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
                let _ = __shape_support_JsonParser::skip_space(input, p, state);
            }
            {
                let _ = ({
                    let _ = __shape_support_JsonParser::skip_space(input, p, state);
                    parse_wrap_JsonParser_value(input, p, state, builder)
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::json::JsonStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __pair_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2.RD — struct-direct Wrap-shape parse function.
    ///
    /// Opens a Wrap frame on the builder, dispatches to the matched
    /// branch's shape fn (which carries its own
    /// begin_compound/end_compound for compound branches and the
    /// matching push_leaf_with_* for scalar branches), stamps the
    /// chosen branch index via push_branch_tag, then closes the
    /// Wrap frame. Mirrors `JsonStructBuilder::OpenFrame::Wrap`'s
    /// forward-the-single-child semantics.
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_wrap_JsonParser_value<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let first = __shape_support_JsonParser::skip_space(input, p, state)
            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        'try_branches: loop {
            match first {
                34u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_string_JsonParser_string(input, p, state, builder) {
                        ::core::result::Result::Ok(_) => {
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                45u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                48u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                49u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                50u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                51u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                52u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                53u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                54u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                55u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                56u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                57u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                91u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_array_JsonParser_array(input, p, state, builder) {
                        ::core::result::Result::Ok(_) => {
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                102u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_keyword_JsonParser_bool(
                        input,
                        p,
                        first,
                        state,
                        builder,
                    ) {
                        ::core::result::Result::Ok(_) => {
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                110u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_keyword_JsonParser_null(
                        input,
                        p,
                        first,
                        state,
                        builder,
                    ) {
                        ::core::result::Result::Ok(_) => {
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                116u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_keyword_JsonParser_bool(
                        input,
                        p,
                        first,
                        state,
                        builder,
                    ) {
                        ::core::result::Result::Ok(_) => {
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                123u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_object_JsonParser_object(input, p, state, builder) {
                        ::core::result::Result::Ok(_) => {
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                _ => {}
            }
            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                offset: *p as u32,
            });
        }
        ::core::result::Result::Ok(())
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
    pub fn parse_JsonParser_value<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        parse_JsonParser_value__value(input, p, state, builder)
    }
    /// AW-V.W3.2 — value-position shape dispatcher. Called both at
    /// the grammar root and from Object / Array compound bodies.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_JsonParser_value__value<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        let first = __shape_support_JsonParser::skip_space(input, p, state)
            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        let __result = match first {
            b'{' => parse_object_JsonParser_object(input, p, state, builder),
            b'[' => parse_array_JsonParser_array(input, p, state, builder),
            b'"' => parse_string_JsonParser_string(input, p, state, builder),
            b'-' | b'0'..=b'9' => {
                parse_number_JsonParser_number(input, p, first, builder)
            }
            b't' | b'f' => parse_keyword_JsonParser_bool(input, p, first, state, builder),
            b'n' => parse_keyword_JsonParser_null(input, p, first, state, builder),
            c => {
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
        };
        __result
    }
    impl JsonParser {
        fn __null_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __s = "null";
                    let __bytes = __s.as_bytes();
                    let __slc = match state.src_bytes.get(state.offset..) {
                        Some(s) if s.len() >= 4usize => s,
                        _ => return false,
                    };
                    if &__slc[..4usize] != __bytes {
                        return false;
                    }
                    __builder.text(&state.src[state.offset..state.offset + 4usize]);
                    state.offset += 4usize;
                };
                true
            }
        }
        pub fn null_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__null_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __bool_prettify<'a>(
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
                        b'f' => {
                            {
                                let __s = "false";
                                let __bytes = __s.as_bytes();
                                let __slc = match state.src_bytes.get(state.offset..) {
                                    Some(s) if s.len() >= 5usize => s,
                                    _ => return false,
                                };
                                if &__slc[..5usize] != __bytes {
                                    return false;
                                }
                                __builder
                                    .text(&state.src[state.offset..state.offset + 5usize]);
                                state.offset += 5usize;
                            };
                        }
                        b't' => {
                            {
                                let __s = "true";
                                let __bytes = __s.as_bytes();
                                let __slc = match state.src_bytes.get(state.offset..) {
                                    Some(s) if s.len() >= 4usize => s,
                                    _ => return false,
                                };
                                if &__slc[..4usize] != __bytes {
                                    return false;
                                }
                                __builder
                                    .text(&state.src[state.offset..state.offset + 4usize]);
                                state.offset += 4usize;
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
        pub fn bool_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__bool_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __number_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __start = state.offset;
                    if ::parse_that::scan_number_strict_span(state).is_none() {
                        return false;
                    }
                    let __matched = &state.src[__start..state.offset];
                    if !__matched.is_empty() {
                        __builder.text(__matched);
                    }
                };
                true
            }
        }
        pub fn number_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__number_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __string_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __start = state.offset;
                    if ::parse_that::scan_quoted_string_strict(state).is_none() {
                        return false;
                    }
                    let __matched = &state.src[__start..state.offset];
                    if !__matched.is_empty() {
                        __builder.text(__matched);
                    }
                };
                true
            }
        }
        pub fn string_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__string_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __object_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            let __pretty_ok = {
                {
                    {
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
                                if !{
                                    let __pretty_cp11 = state.offset;
                                    let __pretty_bcp12 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            let __ows9 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder.text_inline_ws(&state.src[__ows9..state.offset]);
                                            {
                                                let mut __rep_count7 = 0usize;
                                                while __rep_count7 < 4294967295 {
                                                    let __rep_cp8 = state.offset;
                                                    if !{
                                                        let __pretty_cp5 = state.offset;
                                                        let __pretty_bcp6 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            {
                                                                if !Self::__pair_prettify(state, __builder) {
                                                                    return false;
                                                                }
                                                                {
                                                                    let _ = {
                                                                        let __pretty_cp3 = state.offset;
                                                                        let __pretty_bcp4 = __builder.checkpoint();
                                                                        let __ok = (|| -> bool {
                                                                            {
                                                                                let __ows0 = state.offset;
                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                let __ows1 = state.offset;
                                                                                {
                                                                                    if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                                    {
                                                                                        return false;
                                                                                    }
                                                                                    state.offset += 1;
                                                                                    __builder.char(b',');
                                                                                };
                                                                                __builder.text_inline_ws(&state.src[__ows0..__ows1]);
                                                                                let __ows2 = state.offset;
                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                __builder.text_inline_ws(&state.src[__ows2..state.offset]);
                                                                            };
                                                                            true
                                                                        })();
                                                                        if !__ok {
                                                                            state.offset = __pretty_cp3;
                                                                            __builder.restore(__pretty_bcp4);
                                                                        }
                                                                        __ok
                                                                    };
                                                                    true
                                                                };
                                                            };
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp5;
                                                            __builder.restore(__pretty_bcp6);
                                                        }
                                                        __ok
                                                    } {
                                                        state.offset = __rep_cp8;
                                                        break;
                                                    }
                                                    if state.offset == __rep_cp8 {
                                                        break;
                                                    }
                                                    __rep_count7 += 1;
                                                }
                                            };
                                            let __ows10 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder.text_inline_ws(&state.src[__ows10..state.offset]);
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp11;
                                        __builder.restore(__pretty_bcp12);
                                    }
                                    __ok
                                } {
                                    return false;
                                }
                            };
                        };
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'}') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'}');
                        };
                    };
                    true
                }
            };
            __builder.group_close();
            __pretty_ok
        }
        pub fn object_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__object_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __array_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            let __pretty_ok = {
                {
                    {
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
                                if !{
                                    let __pretty_cp24 = state.offset;
                                    let __pretty_bcp25 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            let __ows22 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder.text_inline_ws(&state.src[__ows22..state.offset]);
                                            {
                                                let mut __rep_count20 = 0usize;
                                                while __rep_count20 < 4294967295 {
                                                    let __rep_cp21 = state.offset;
                                                    if !{
                                                        let __pretty_cp18 = state.offset;
                                                        let __pretty_bcp19 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            {
                                                                if !Self::__value_prettify(state, __builder) {
                                                                    return false;
                                                                }
                                                                {
                                                                    let _ = {
                                                                        let __pretty_cp16 = state.offset;
                                                                        let __pretty_bcp17 = __builder.checkpoint();
                                                                        let __ok = (|| -> bool {
                                                                            {
                                                                                let __ows13 = state.offset;
                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                let __ows14 = state.offset;
                                                                                {
                                                                                    if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                                    {
                                                                                        return false;
                                                                                    }
                                                                                    state.offset += 1;
                                                                                    __builder.char(b',');
                                                                                };
                                                                                __builder.text_inline_ws(&state.src[__ows13..__ows14]);
                                                                                let __ows15 = state.offset;
                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                __builder.text_inline_ws(&state.src[__ows15..state.offset]);
                                                                            };
                                                                            true
                                                                        })();
                                                                        if !__ok {
                                                                            state.offset = __pretty_cp16;
                                                                            __builder.restore(__pretty_bcp17);
                                                                        }
                                                                        __ok
                                                                    };
                                                                    true
                                                                };
                                                            };
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp18;
                                                            __builder.restore(__pretty_bcp19);
                                                        }
                                                        __ok
                                                    } {
                                                        state.offset = __rep_cp21;
                                                        break;
                                                    }
                                                    if state.offset == __rep_cp21 {
                                                        break;
                                                    }
                                                    __rep_count20 += 1;
                                                }
                                            };
                                            let __ows23 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder.text_inline_ws(&state.src[__ows23..state.offset]);
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp24;
                                        __builder.restore(__pretty_bcp25);
                                    }
                                    __ok
                                } {
                                    return false;
                                }
                            };
                        };
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b']') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b']');
                        };
                    };
                    true
                }
            };
            __builder.group_close();
            __pretty_ok
        }
        pub fn array_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__array_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __pair_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            let __pretty_ok = {
                {
                    {
                        {
                            let __start = state.offset;
                            if ::parse_that::scan_quoted_string_strict(state).is_none() {
                                return false;
                            }
                            let __matched = &state.src[__start..state.offset];
                            if !__matched.is_empty() {
                                __builder.text(__matched);
                            }
                        };
                        {
                            {
                                let __ows26 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                let __ows27 = state.offset;
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b':')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b':');
                                };
                                __builder.text_inline_ws(&state.src[__ows26..__ows27]);
                                let __ows28 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows28..state.offset]);
                            };
                            if !Self::__value_prettify(state, __builder) {
                                return false;
                            }
                        };
                    };
                    true
                }
            };
            __builder.group_close();
            __pretty_ok
        }
        pub fn pair_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__pair_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __value_prettify<'a>(
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
                        b'{' => {
                            if !Self::__object_prettify(state, __builder) {
                                return false;
                            }
                        }
                        b'[' => {
                            if !Self::__array_prettify(state, __builder) {
                                return false;
                            }
                        }
                        b'"' => {
                            {
                                let __start = state.offset;
                                if ::parse_that::scan_quoted_string_strict(state).is_none()
                                {
                                    return false;
                                }
                                let __matched = &state.src[__start..state.offset];
                                if !__matched.is_empty() {
                                    __builder.text(__matched);
                                }
                            };
                        }
                        b'n' => {
                            {
                                let __s = "null";
                                let __bytes = __s.as_bytes();
                                let __slc = match state.src_bytes.get(state.offset..) {
                                    Some(s) if s.len() >= 4usize => s,
                                    _ => return false,
                                };
                                if &__slc[..4usize] != __bytes {
                                    return false;
                                }
                                __builder
                                    .text(&state.src[state.offset..state.offset + 4usize]);
                                state.offset += 4usize;
                            };
                        }
                        b'f' | b't' => {
                            if !Self::__bool_prettify(state, __builder) {
                                return false;
                            }
                        }
                        b'-' | b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7'
                        | b'8' | b'9' => {
                            {
                                let __start = state.offset;
                                if ::parse_that::scan_number_strict_span(state).is_none() {
                                    return false;
                                }
                                let __matched = &state.src[__start..state.offset];
                                if !__matched.is_empty() {
                                    __builder.text(__matched);
                                }
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
        pub fn value_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__value_prettify(state, &mut __builder) {
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
            crate::runtime::json::JsonDocument<'_>,
            crate::runtime::ParseErr,
        > {
            let __input_bytes = input.as_bytes();
            let mut state = __shape_support_JsonParser::ScanState::new();
            let mut builder = crate::runtime::json::JsonStructBuilder::new();
            {
                let mut pos: usize = 0;
                parse_JsonParser_value(__input_bytes, &mut pos, &mut state, &mut builder)
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
                let _ = __shape_support_JsonParser::skip_space(
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
pub use __jsonparser_emit_impl::*;
