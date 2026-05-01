//! AUTO-GENERATED from `[workspace.metadata.bbnf.grammars]` — do not edit manually.
//! Regenerate: cargo xtask regen --grammar csv

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

pub struct CsvParser;
mod __csvparser_emit_impl {
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
    pub const GRAMMAR_CsvParser: [&'static str; 1usize] = [
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../../grammar/misc/csv.bbnf")),
    ];
    static __GRAMMAR_STRUCTURAL_ALPHABET: [u8; 2usize] = [34, 44];
    pub const GRAMMAR_STRUCTURAL_ALPHABET: &[u8] = &__GRAMMAR_STRUCTURAL_ALPHABET;
    pub const GRAMMAR_STRUCTURAL_DIGRAPHS: &[(u8, u8)] = &[];
    pub const GRAMMAR_STRUCTURAL_DIGRAPH_MASK: [u64; 4] = [0, 0, 0, 0];
    pub const GRAMMAR_STRUCTURAL_QUOTE_CLASSES: &[u8] = &[];
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
    static __DTA_REGEX_2: &str = "[^\"]*";
    static __DTA_REGEX_17: &str = "\\r?\\n";
    static __DTA_HREGEX_22: &str = "[^,\"\\r\\n]+";
    #[inline]
    #[cold]
    fn __regex_scan_CsvParser(
        pattern: &str,
        input: &[u8],
        pos: usize,
    ) -> ::core::option::Option<u32> {
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_2.as_ptr())
            || pattern == __DTA_REGEX_2
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
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 35 | 36 | 37 | 38
                                | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49 | 50
                                | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62
                                | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74
                                | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86
                                | 87 | 88 | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 | 98
                                | 99 | 100 | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108
                                | 109 | 110 | 111 | 112 | 113 | 114 | 115 | 116 | 117 | 118
                                | 119 | 120 | 121 | 122 | 123 | 124 | 125 | 126 | 127 | 128
                                | 129 | 130 | 131 | 132 | 133 | 134 | 135 | 136 | 137 | 138
                                | 139 | 140 | 141 | 142 | 143 | 144 | 145 | 146 | 147 | 148
                                | 149 | 150 | 151 | 152 | 153 | 154 | 155 | 156 | 157 | 158
                                | 159 | 160 | 161 | 162 | 163 | 164 | 165 | 166 | 167 | 168
                                | 169 | 170 | 171 | 172 | 173 | 174 | 175 | 176 | 177 | 178
                                | 179 | 180 | 181 | 182 | 183 | 184 | 185 | 186 | 187 | 188
                                | 189 | 190 | 191 | 192 | 193 | 194 | 195 | 196 | 197 | 198
                                | 199 | 200 | 201 | 202 | 203 | 204 | 205 | 206 | 207 | 208
                                | 209 | 210 | 211 | 212 | 213 | 214 | 215 | 216 | 217 | 218
                                | 219 | 220 | 221 | 222 | 223 | 224 | 225 | 226 | 227 | 228
                                | 229 | 230 | 231 | 232 | 233 | 234 | 235 | 236 | 237 | 238
                                | 239 | 240 | 241 | 242 | 243 | 244 | 245 | 246 | 247 | 248
                                | 249 | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 0,
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_17.as_ptr())
            || pattern == __DTA_REGEX_17
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
                                10 => __dfa_state = 1,
                                13 => __dfa_state = 2,
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
                                10 => __dfa_state = 1,
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_HREGEX_22.as_ptr())
            || pattern == __DTA_HREGEX_22
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
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 11 | 12 | 14 | 15
                                | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27
                                | 28 | 29 | 30 | 31 | 32 | 33 | 35 | 36 | 37 | 38 | 39 | 40
                                | 41 | 42 | 43 | 45 | 46 | 47 | 48 | 49 | 50 | 51 | 52 | 53
                                | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63 | 64 | 65
                                | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77
                                | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89
                                | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 | 98 | 99 | 100
                                | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108 | 109 | 110
                                | 111 | 112 | 113 | 114 | 115 | 116 | 117 | 118 | 119 | 120
                                | 121 | 122 | 123 | 124 | 125 | 126 | 127 | 128 | 129 | 130
                                | 131 | 132 | 133 | 134 | 135 | 136 | 137 | 138 | 139 | 140
                                | 141 | 142 | 143 | 144 | 145 | 146 | 147 | 148 | 149 | 150
                                | 151 | 152 | 153 | 154 | 155 | 156 | 157 | 158 | 159 | 160
                                | 161 | 162 | 163 | 164 | 165 | 166 | 167 | 168 | 169 | 170
                                | 171 | 172 | 173 | 174 | 175 | 176 | 177 | 178 | 179 | 180
                                | 181 | 182 | 183 | 184 | 185 | 186 | 187 | 188 | 189 | 190
                                | 191 | 192 | 193 | 194 | 195 | 196 | 197 | 198 | 199 | 200
                                | 201 | 202 | 203 | 204 | 205 | 206 | 207 | 208 | 209 | 210
                                | 211 | 212 | 213 | 214 | 215 | 216 | 217 | 218 | 219 | 220
                                | 221 | 222 | 223 | 224 | 225 | 226 | 227 | 228 | 229 | 230
                                | 231 | 232 | 233 | 234 | 235 | 236 | 237 | 238 | 239 | 240
                                | 241 | 242 | 243 | 244 | 245 | 246 | 247 | 248 | 249 | 250
                                | 251 | 252 | 253 | 254 | 255 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 11 | 12 | 14 | 15
                                | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27
                                | 28 | 29 | 30 | 31 | 32 | 33 | 35 | 36 | 37 | 38 | 39 | 40
                                | 41 | 42 | 43 | 45 | 46 | 47 | 48 | 49 | 50 | 51 | 52 | 53
                                | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63 | 64 | 65
                                | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77
                                | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89
                                | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 | 98 | 99 | 100
                                | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108 | 109 | 110
                                | 111 | 112 | 113 | 114 | 115 | 116 | 117 | 118 | 119 | 120
                                | 121 | 122 | 123 | 124 | 125 | 126 | 127 | 128 | 129 | 130
                                | 131 | 132 | 133 | 134 | 135 | 136 | 137 | 138 | 139 | 140
                                | 141 | 142 | 143 | 144 | 145 | 146 | 147 | 148 | 149 | 150
                                | 151 | 152 | 153 | 154 | 155 | 156 | 157 | 158 | 159 | 160
                                | 161 | 162 | 163 | 164 | 165 | 166 | 167 | 168 | 169 | 170
                                | 171 | 172 | 173 | 174 | 175 | 176 | 177 | 178 | 179 | 180
                                | 181 | 182 | 183 | 184 | 185 | 186 | 187 | 188 | 189 | 190
                                | 191 | 192 | 193 | 194 | 195 | 196 | 197 | 198 | 199 | 200
                                | 201 | 202 | 203 | 204 | 205 | 206 | 207 | 208 | 209 | 210
                                | 211 | 212 | 213 | 214 | 215 | 216 | 217 | 218 | 219 | 220
                                | 221 | 222 | 223 | 224 | 225 | 226 | 227 | 228 | 229 | 230
                                | 231 | 232 | 233 | 234 | 235 | 236 | 237 | 238 | 239 | 240
                                | 241 | 242 | 243 | 244 | 245 | 246 | 247 | 248 | 249 | 250
                                | 251 | 252 | 253 | 254 | 255 => __dfa_state = 1,
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
    /// AW-V.W3.2 — per-grammar shape-dispatch support.
    ///
    /// Inlined by every `parse_<shape>_<grammar>_<rule>` emitted
    /// sibling; carries the SIMD whitespace bitmap cache + the
    /// quoted-string scanner primitive. The module is private to
    /// the generated code — downstream consumers route through the
    /// top-level `parse_<grammar>_<root>` which inlines every
    /// helper under workspace LTO.
    #[allow(dead_code, non_snake_case)]
    pub(crate) mod __shape_support_CsvParser {
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
    /// AZ-I.W2-act.B3 — per-grammar HRegex-shape parse function,
    /// **struct-direct body**.
    ///
    /// Runs the per-grammar regex scan, decodes the matched bytes
    /// per the rule's host-fn descriptor (HexConvert, NumberConvert,
    /// or Expr { Input, return_type }), and routes the decoded
    /// value through the StructBuilder trait. Returns
    /// unit for StructDirect composition with sibling
    /// shape fns under struct-direct mode.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_hregex_CsvParser_textdata<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CsvParser::ScanState,
        builder: &mut crate::runtime::csv::CsvStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        let span_lo = *p as u32;
        let Some(match_len) = __regex_scan_CsvParser("[^,\"\\r\\n]+", input, *p) else {
            return Err(crate::runtime::DtaError::Syntax {
                offset: span_lo,
            });
        };
        *p += match_len as usize;
        let span_hi = *p as u32;
        <crate::runtime::csv::CsvStructBuilder<
            'p,
        > as crate::runtime::StructBuilder>::push_leaf_with_str(
            builder,
            core::str::from_utf8(&input[span_lo as usize..span_hi as usize])
                .unwrap_or(""),
        );
        Ok(())
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
    pub fn parse_flat_CsvParser_escaped<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CsvParser::ScanState,
        builder: &mut crate::runtime::csv::CsvStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __span_lo: usize = *p;
        let __escaped_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 1u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("escaped"),
            kind: ::bbnf_ir::registry::LayoutKind::NewtypeWrapper,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __escaped_handle = <crate::runtime::csv::CsvStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__escaped_layout);
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
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
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_CsvParser("[^\"]*", input, *p)
                    else {
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
                if input.len() < end || input[at..end] != [34u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                let __span_hi: usize = *p;
                let __span_slice: &str = ::core::str::from_utf8(
                        &input[__span_lo..__span_hi],
                    )
                    .unwrap_or("");
                <crate::runtime::csv::CsvStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_leaf_with_str(
                    builder,
                    __span_slice,
                );
                <crate::runtime::csv::CsvStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __escaped_handle,
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
    pub fn parse_flat_CsvParser_record<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CsvParser::ScanState,
        builder: &mut crate::runtime::csv::CsvStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __record_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 2u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("record"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __record_handle = <crate::runtime::csv::CsvStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__record_layout);
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                'try_branches: loop {
                    {
                        let __alt_save_p = *p;
                        let __alt_builder_checkpoint = builder.checkpoint();
                        let __alt_result: ::core::result::Result<
                            (),
                            crate::runtime::DtaError,
                        > = (|| {
                            let _ = ({
                                let _ = __shape_support_CsvParser::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_flat_CsvParser_escaped(input, p, state, builder)
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
                                parse_hregex_CsvParser_textdata(input, p, state, builder)
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
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [44u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                            'try_branches: loop {
                                {
                                    let __alt_save_p = *p;
                                    let __alt_builder_checkpoint = builder.checkpoint();
                                    let __alt_result: ::core::result::Result<
                                        (),
                                        crate::runtime::DtaError,
                                    > = (|| {
                                        let _ = ({
                                            let _ = __shape_support_CsvParser::skip_space(
                                                input,
                                                p,
                                                state,
                                            );
                                            parse_flat_CsvParser_escaped(input, p, state, builder)
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
                                            parse_hregex_CsvParser_textdata(input, p, state, builder)
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
                <crate::runtime::csv::CsvStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __record_handle,
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
    pub fn parse_flat_CsvParser_csv<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CsvParser::ScanState,
        builder: &mut crate::runtime::csv::CsvStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __csv_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 3u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("csv"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __csv_handle = <crate::runtime::csv::CsvStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__csv_layout);
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = ({ parse_flat_CsvParser_record(input, p, state, builder) })?;
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
                            {
                                let __scan_start = *p;
                                let Some(match_len) = __regex_scan_CsvParser(
                                    "\\r?\\n",
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
                                parse_flat_CsvParser_record(input, p, state, builder)
                            })?;
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
                <crate::runtime::csv::CsvStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(builder, __csv_handle);
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
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
    pub fn parse_CsvParser_csv<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CsvParser::ScanState,
        builder: &mut crate::runtime::csv::CsvStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        parse_CsvParser_csv__value(input, p, state, builder)
    }
    /// AW-V.W3.2 — value-position shape dispatcher. Called both at
    /// the grammar root and from Object / Array compound bodies.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_CsvParser_csv__value<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_CsvParser::ScanState,
        builder: &mut crate::runtime::csv::CsvStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        let _ = __shape_support_CsvParser::skip_space(input, p, state);
        parse_flat_CsvParser_csv(input, p, state, builder)
    }
    impl CsvParser {
        /// Parse an input string and return the grammar-specific
        /// document that owns the StructDirect runtime arena.
        pub fn parse(
            input: &str,
        ) -> ::core::result::Result<
            crate::runtime::csv::CsvDocument<'_>,
            crate::runtime::ParseErr,
        > {
            let __input_bytes = input.as_bytes();
            let mut state = __shape_support_CsvParser::ScanState::new();
            let mut builder = crate::runtime::csv::CsvStructBuilder::new();
            {
                let mut pos: usize = 0;
                parse_CsvParser_csv(__input_bytes, &mut pos, &mut state, &mut builder)
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
                let _ = __shape_support_CsvParser::skip_space(
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
pub use __csvparser_emit_impl::*;
