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

use ::bbnf::runtime::tape::*;
use ::bbnf::runtime::{Parsed, ParseErr, Root};
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
    static __GRAMMAR_PROFILE_ALPHABET: [u8; 2usize] = [34, 44];
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
    static __DTA_REGEX_1: &str = "[^\"]*";
    static __DTA_REGEX_17: &str = "\\r?\\n";
    static __DTA_HREGEX_22: &str = "[^,\"\\r\\n]+";
    #[inline]
    #[cold]
    fn __regex_scan_CsvParser(
        pattern: &str,
        input: &[u8],
        pos: usize,
    ) -> ::core::option::Option<u32> {
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_1.as_ptr())
            || pattern == __DTA_REGEX_1
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
    /// AW-V.W4-fix — per-grammar Flat-shape parse function,
    /// walker-tape-identical.
    ///
    /// Emits one outer Seq compound plus per-position inner
    /// records. Ref / Regex / Alt positions recurse through the
    /// grammar's value-position dispatcher (the walker's
    /// authoritative state path).
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`): this fn
    /// sits on a cross-shape recursive edge
    /// (`parse_flat_<grammar>_<rule>` → `emit_ref_call_tape` →
    /// peer shape fn → back here through the grammar's `__value`
    /// discriminant). LLVM's inliner collapses plain `#[inline]`
    /// candidates only when profitable and bails cleanly on
    /// detected recursion; `#[inline(always)]` would recurse the
    /// inliner until stack exhaustion (observed SIGBUS in
    /// BbnfBootstrap's `grammar_item` triangle during W0a.2.e).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_CsvParser_escaped(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CsvParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.position();
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [34u8] {
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: at as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            *p = end;
            let _ = builder
                .push_leaf_with(
                    crate::runtime::tape::TapeKind::Literal,
                    at as u32,
                    end as u32,
                    0u8,
                    0,
                    crate::runtime::tape::PayloadData::None,
                );
        }
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_CsvParser("[^\"]*", input, *p) else {
                    return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                        offset: span_lo,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    });
                };
                *p += match_len as usize;
                let span_hi = *p as u32;
                let _ = builder
                    .push_leaf_with(
                        crate::runtime::tape::TapeKind::Span,
                        span_lo,
                        span_hi,
                        0u8,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [34u8] {
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: at as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            *p = end;
            let _ = builder
                .push_leaf_with(
                    crate::runtime::tape::TapeKind::Literal,
                    at as u32,
                    end as u32,
                    0u8,
                    0,
                    crate::runtime::tape::PayloadData::None,
                );
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                crate::runtime::tape::TapeKind::Seq,
                span_lo,
                0u8,
                0u8,
                0u8,
                0u16,
            );
        builder
            .end_compound_post_order(
                outer_off,
                span_hi,
                crate::runtime::tape::TapeOffset(outer_child),
            );
        Ok(crate::runtime::tape::TapeOffset(outer_off))
    }
    /// AW-V.W4-fix — per-grammar HRegex-shape parse function.
    ///
    /// Regex scan via the per-grammar adapter; emits a
    /// `TapeKind::Regex` leaf carrying the matched span. Decoder
    /// hooks (host_fn payloads) are wired at the dispatcher level
    /// post-scan; the raw Span-leaf path is the default.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_hregex_CsvParser_textdata(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CsvParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let Some(match_len) = __regex_scan_CsvParser("[^,\"\\r\\n]+", input, *p) else {
            return Err(crate::runtime::tape::DtaError::Syntax {
                offset: span_lo,
                failing_state: crate::runtime::tape::DtaStateId::NONE,
                failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
            });
        };
        *p += match_len as usize;
        let span_hi = *p as u32;
        let leaf_off = builder
            .push_leaf_with(
                crate::runtime::tape::TapeKind::Regex,
                span_lo,
                span_hi,
                1u8,
                0,
                crate::runtime::tape::PayloadData::None,
            );
        Ok(leaf_off)
    }
    /// AW-V.W4-fix — per-grammar Flat-shape parse function,
    /// walker-tape-identical.
    ///
    /// Emits one outer Seq compound plus per-position inner
    /// records. Ref / Regex / Alt positions recurse through the
    /// grammar's value-position dispatcher (the walker's
    /// authoritative state path).
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`): this fn
    /// sits on a cross-shape recursive edge
    /// (`parse_flat_<grammar>_<rule>` → `emit_ref_call_tape` →
    /// peer shape fn → back here through the grammar's `__value`
    /// discriminant). LLVM's inliner collapses plain `#[inline]`
    /// candidates only when profitable and bails cleanly on
    /// detected recursion; `#[inline(always)]` would recurse the
    /// inliner until stack exhaustion (observed SIGBUS in
    /// BbnfBootstrap's `grammar_item` triangle during W0a.2.e).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_CsvParser_record(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CsvParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.position();
        {
            {
                let first = __shape_support_CsvParser::skip_space(input, p, state)
                    .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                        offset: *p as u32,
                    })?;
                'try_branches: loop {
                    match first {
                        34u8 => {
                            let attempt_p = *p;
                            let attempt_len = builder.position();
                            match {
                                let _ = __shape_support_CsvParser::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_flat_CsvParser_escaped(input, p, state, builder)
                            } {
                                Ok(_) => break 'try_branches,
                                Err(_) => {
                                    *p = attempt_p;
                                    builder.rollback_to(attempt_len);
                                }
                            }
                        }
                        _ => {}
                    }
                    {
                        let attempt_p = *p;
                        let attempt_len = builder.position();
                        match {
                            parse_hregex_CsvParser_textdata(input, p, state, builder)
                        } {
                            Ok(_) => break 'try_branches,
                            Err(_) => {
                                *p = attempt_p;
                                builder.rollback_to(attempt_len);
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
        }
        {
            let repeat_lo = *p as u32;
            let repeat_child = builder.position();
            let mut iter_count: u32 = 0;
            loop {
                let save_p = *p;
                let save_cols = builder.position();
                let iter_lo = *p as u32;
                let iter_child = builder.position();
                let attempt = (|| -> ::core::result::Result<
                    (),
                    crate::runtime::tape::DtaError,
                > {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() < end || input[at..end] != [44u8] {
                        return Err(crate::runtime::tape::DtaError::Syntax {
                            offset: at as u32,
                            failing_state: crate::runtime::tape::DtaStateId::NONE,
                            failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                        });
                    }
                    *p = end;
                    let _ = builder
                        .push_leaf_with(
                            crate::runtime::tape::TapeKind::Literal,
                            at as u32,
                            end as u32,
                            2u8,
                            0,
                            crate::runtime::tape::PayloadData::None,
                        );
                    {
                        let first = __shape_support_CsvParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                                offset: *p as u32,
                            })?;
                        'try_branches: loop {
                            match first {
                                34u8 => {
                                    let attempt_p = *p;
                                    let attempt_len = builder.position();
                                    match {
                                        let _ = __shape_support_CsvParser::skip_space(
                                            input,
                                            p,
                                            state,
                                        );
                                        parse_flat_CsvParser_escaped(input, p, state, builder)
                                    } {
                                        Ok(_) => break 'try_branches,
                                        Err(_) => {
                                            *p = attempt_p;
                                            builder.rollback_to(attempt_len);
                                        }
                                    }
                                }
                                _ => {}
                            }
                            {
                                let attempt_p = *p;
                                let attempt_len = builder.position();
                                match {
                                    parse_hregex_CsvParser_textdata(input, p, state, builder)
                                } {
                                    Ok(_) => break 'try_branches,
                                    Err(_) => {
                                        *p = attempt_p;
                                        builder.rollback_to(attempt_len);
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
                    Ok(())
                })();
                if attempt.is_err() {
                    *p = save_p;
                    builder.rollback_to(save_cols);
                    break;
                }
                if *p == save_p {
                    builder.rollback_to(save_cols);
                    break;
                }
                let iter_hi = *p as u32;
                let __iter_off = builder
                    .begin_compound(
                        crate::runtime::tape::TapeKind::Seq,
                        iter_lo,
                        0u8,
                        0u8,
                        0u8,
                        0u16,
                    );
                builder
                    .end_compound_post_order(
                        __iter_off,
                        iter_hi,
                        crate::runtime::tape::TapeOffset(iter_child),
                    );
                iter_count = iter_count.saturating_add(1);
            }
            if iter_count < (0usize as u32) {
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            let repeat_hi = *p as u32;
            let __repeat_off = builder
                .begin_compound(
                    crate::runtime::tape::TapeKind::Repeat,
                    repeat_lo,
                    0u8,
                    0u8,
                    0u8,
                    0u16,
                );
            builder
                .end_compound_post_order(
                    __repeat_off,
                    repeat_hi,
                    crate::runtime::tape::TapeOffset(repeat_child),
                );
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                crate::runtime::tape::TapeKind::Seq,
                span_lo,
                2u8,
                0u8,
                0u8,
                0u16,
            );
        builder
            .end_compound_post_order(
                outer_off,
                span_hi,
                crate::runtime::tape::TapeOffset(outer_child),
            );
        Ok(crate::runtime::tape::TapeOffset(outer_off))
    }
    /// AW-V.W4-fix — per-grammar Flat-shape parse function,
    /// walker-tape-identical.
    ///
    /// Emits one outer Seq compound plus per-position inner
    /// records. Ref / Regex / Alt positions recurse through the
    /// grammar's value-position dispatcher (the walker's
    /// authoritative state path).
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`): this fn
    /// sits on a cross-shape recursive edge
    /// (`parse_flat_<grammar>_<rule>` → `emit_ref_call_tape` →
    /// peer shape fn → back here through the grammar's `__value`
    /// discriminant). LLVM's inliner collapses plain `#[inline]`
    /// candidates only when profitable and bails cleanly on
    /// detected recursion; `#[inline(always)]` would recurse the
    /// inliner until stack exhaustion (observed SIGBUS in
    /// BbnfBootstrap's `grammar_item` triangle during W0a.2.e).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_flat_CsvParser_csv(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CsvParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.position();
        {
            let _ = ({ parse_flat_CsvParser_record(input, p, state, builder) })?;
        }
        {
            let repeat_lo = *p as u32;
            let repeat_child = builder.position();
            let mut iter_count: u32 = 0;
            loop {
                let save_p = *p;
                let save_cols = builder.position();
                let iter_lo = *p as u32;
                let iter_child = builder.position();
                let attempt = (|| -> ::core::result::Result<
                    (),
                    crate::runtime::tape::DtaError,
                > {
                    {
                        let span_lo = *p as u32;
                        let Some(match_len) = __regex_scan_CsvParser(
                            "\\r?\\n",
                            input,
                            *p,
                        ) else {
                            return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                offset: span_lo,
                                failing_state: crate::runtime::tape::DtaStateId::NONE,
                                failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                            });
                        };
                        *p += match_len as usize;
                        let span_hi = *p as u32;
                        let _ = builder
                            .push_leaf_with(
                                crate::runtime::tape::TapeKind::Span,
                                span_lo,
                                span_hi,
                                3u8,
                                0,
                                crate::runtime::tape::PayloadData::None,
                            );
                    }
                    let _ = ({ parse_flat_CsvParser_record(input, p, state, builder) })?;
                    Ok(())
                })();
                if attempt.is_err() {
                    *p = save_p;
                    builder.rollback_to(save_cols);
                    break;
                }
                if *p == save_p {
                    builder.rollback_to(save_cols);
                    break;
                }
                let iter_hi = *p as u32;
                let __iter_off = builder
                    .begin_compound(
                        crate::runtime::tape::TapeKind::Seq,
                        iter_lo,
                        0u8,
                        0u8,
                        0u8,
                        0u16,
                    );
                builder
                    .end_compound_post_order(
                        __iter_off,
                        iter_hi,
                        crate::runtime::tape::TapeOffset(iter_child),
                    );
                iter_count = iter_count.saturating_add(1);
            }
            if iter_count < (0usize as u32) {
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            let repeat_hi = *p as u32;
            let __repeat_off = builder
                .begin_compound(
                    crate::runtime::tape::TapeKind::Repeat,
                    repeat_lo,
                    0u8,
                    0u8,
                    0u8,
                    0u16,
                );
            builder
                .end_compound_post_order(
                    __repeat_off,
                    repeat_hi,
                    crate::runtime::tape::TapeOffset(repeat_child),
                );
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                crate::runtime::tape::TapeKind::Seq,
                span_lo,
                3u8,
                0u8,
                0u8,
                0u16,
            );
        builder
            .end_compound_post_order(
                outer_off,
                span_hi,
                crate::runtime::tape::TapeOffset(outer_child),
            );
        Ok(crate::runtime::tape::TapeOffset(outer_off))
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
    pub fn parse_flat_visitor_CsvParser_escaped<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CsvParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [34u8] {
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
                let Some(match_len) = __regex_scan_CsvParser("[^\"]*", input, *p) else {
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
            if input.len() < end || input[at..end] != [34u8] {
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: at as u32,
                    rule: None,
                });
            }
            *p = end;
        }
        Ok(())
    }
    /// AW-V.W4-fix — visitor-path HRegex-shape parse function.
    ///
    /// Regex scan via the per-grammar adapter; fires the
    /// visitor's `string()` event with the matched span when
    /// visitor is a StringVisitor. Non-string decoders (host_fn
    /// payloads) dispatch at the per-grammar consumer wave.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_hregex_visitor_CsvParser_textdata<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CsvParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::StringVisitor,
    {
        let span_lo = *p;
        let Some(match_len) = __regex_scan_CsvParser("[^,\"\\r\\n]+", input, *p) else {
            return Err(crate::runtime::ParseErr::Syntax {
                offset: span_lo as u32,
                rule: None,
            });
        };
        let span_hi = *p + match_len as usize;
        *p = span_hi;
        visitor
            .string(&input[span_lo..span_hi])
            .map_err(|_| {
                crate::runtime::ParseErr::Syntax {
                    offset: span_lo as u32,
                    rule: None,
                }
            })
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
    pub fn parse_flat_visitor_CsvParser_record<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CsvParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        {
            {
                let first = __shape_support_CsvParser::skip_space(input, p, state)
                    .ok_or(crate::runtime::ParseErr::Syntax {
                        offset: *p as u32,
                        rule: None,
                    })?;
                'try_branches: loop {
                    match first {
                        34u8 => {
                            let attempt_p = *p;
                            match {
                                let _ = __shape_support_CsvParser::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_flat_visitor_CsvParser_escaped(
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
                        _ => {}
                    }
                    {
                        let attempt_p = *p;
                        match {
                            let _ = __shape_support_CsvParser::skip_space(
                                input,
                                p,
                                state,
                            );
                            parse_hregex_visitor_CsvParser_textdata(
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
        }
        {
            let mut iter_count: u32 = 0;
            loop {
                let save_p = *p;
                let res = (|| -> ::core::result::Result<(), crate::runtime::ParseErr> {
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() < end || input[at..end] != [44u8] {
                        return Err(crate::runtime::ParseErr::Syntax {
                            offset: at as u32,
                            rule: None,
                        });
                    }
                    *p = end;
                    {
                        let first = __shape_support_CsvParser::skip_space(
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
                                34u8 => {
                                    let attempt_p = *p;
                                    match {
                                        let _ = __shape_support_CsvParser::skip_space(
                                            input,
                                            p,
                                            state,
                                        );
                                        parse_flat_visitor_CsvParser_escaped(
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
                                _ => {}
                            }
                            {
                                let attempt_p = *p;
                                match {
                                    let _ = __shape_support_CsvParser::skip_space(
                                        input,
                                        p,
                                        state,
                                    );
                                    parse_hregex_visitor_CsvParser_textdata(
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
    pub fn parse_flat_visitor_CsvParser_csv<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CsvParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        {
            ({
                let _ = __shape_support_CsvParser::skip_space(input, p, state);
                parse_flat_visitor_CsvParser_record(input, p, state, visitor)
            })?;
        }
        {
            let mut iter_count: u32 = 0;
            loop {
                let save_p = *p;
                let res = (|| -> ::core::result::Result<(), crate::runtime::ParseErr> {
                    {
                        let span_lo = *p;
                        let Some(match_len) = __regex_scan_CsvParser(
                            "\\r?\\n",
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
                        let _ = __shape_support_CsvParser::skip_space(input, p, state);
                        parse_flat_visitor_CsvParser_record(input, p, state, visitor)
                    })?;
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
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Sparse,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 1u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Empty,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 2u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Sparse,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(2),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 3u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Sparse,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(2),
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
    pub fn parse_CsvParser_csv(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CsvParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        parse_CsvParser_csv__value(input, p, state, builder)
    }
    /// AW-V.W3.2 — value-position shape dispatcher. Called both at
    /// the grammar root and from Object / Array compound bodies.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_CsvParser_csv__value(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CsvParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let _ = __shape_support_CsvParser::skip_space(input, p, state);
        parse_flat_CsvParser_csv(input, p, state, builder)
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
    pub fn parse_CsvParser_csv_visitor<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CsvParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        parse_CsvParser_csv_visitor__value(input, p, state, visitor)
    }
    /// AW-V.W3-bench-fix — value-position visitor-path dispatcher.
    /// Called both at the grammar root and from the object / array
    /// shape fns' value-position recursion.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_CsvParser_csv_visitor__value<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CsvParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        let _ = __shape_support_CsvParser::skip_space(input, p, state);
        parse_flat_visitor_CsvParser_csv(input, p, state, visitor)
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct escapedView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> escapedView<'p> {
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
        pub fn rule_kind(&self) -> CsvParserRuleKind {
            match self.variant_idx() {
                0u8 => CsvParserRuleKind::escaped,
                1u8 => CsvParserRuleKind::textdata,
                2u8 => CsvParserRuleKind::record,
                3u8 => CsvParserRuleKind::csv,
                _ => CsvParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CsvParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor.children().map(move |c| CsvParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<CsvParserNodeView<'p>> {
            self.cursor.child(i).map(|c| CsvParserNodeView::from_cursor(c, self.input))
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
    impl<'p> escapedView<'p> {
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
    pub struct textdataView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> textdataView<'p> {
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
        pub fn rule_kind(&self) -> CsvParserRuleKind {
            match self.variant_idx() {
                0u8 => CsvParserRuleKind::escaped,
                1u8 => CsvParserRuleKind::textdata,
                2u8 => CsvParserRuleKind::record,
                3u8 => CsvParserRuleKind::csv,
                _ => CsvParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CsvParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor.children().map(move |c| CsvParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<CsvParserNodeView<'p>> {
            self.cursor.child(i).map(|c| CsvParserNodeView::from_cursor(c, self.input))
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
    impl<'p> textdataView<'p> {
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
    pub struct recordView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> recordView<'p> {
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
        pub fn rule_kind(&self) -> CsvParserRuleKind {
            match self.variant_idx() {
                0u8 => CsvParserRuleKind::escaped,
                1u8 => CsvParserRuleKind::textdata,
                2u8 => CsvParserRuleKind::record,
                3u8 => CsvParserRuleKind::csv,
                _ => CsvParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CsvParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor.children().map(move |c| CsvParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<CsvParserNodeView<'p>> {
            self.cursor.child(i).map(|c| CsvParserNodeView::from_cursor(c, self.input))
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
    impl<'p> recordView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<CsvParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| CsvParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<CsvParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| CsvParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct csvView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> csvView<'p> {
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
        pub fn rule_kind(&self) -> CsvParserRuleKind {
            match self.variant_idx() {
                0u8 => CsvParserRuleKind::escaped,
                1u8 => CsvParserRuleKind::textdata,
                2u8 => CsvParserRuleKind::record,
                3u8 => CsvParserRuleKind::csv,
                _ => CsvParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CsvParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor.children().map(move |c| CsvParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<CsvParserNodeView<'p>> {
            self.cursor.child(i).map(|c| CsvParserNodeView::from_cursor(c, self.input))
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
    impl<'p> csvView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<recordView<'p>> {
            self.cursor.child(0usize).map(|c| recordView::from_cursor(c, self.input))
        }
        ///The `record` child as a typed view.
        #[inline]
        pub fn record(&self) -> ::core::option::Option<recordView<'p>> {
            self.cursor.child(0usize).map(|c| recordView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<CsvParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| CsvParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generic node view over any tape record for this grammar.
    #[derive(Clone, Copy, Debug)]
    pub struct CsvParserNodeView<'p> {
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
    pub enum CsvParserRuleKind {
        escaped,
        textdata,
        record,
        csv,
        /// Fallback for records whose variant_idx is not a
        /// known rule- or sub-variant discriminator.
        Unknown,
    }
    impl<'p> CsvParserNodeView<'p> {
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
        pub fn rule_kind(&self) -> CsvParserRuleKind {
            match self.variant_idx() {
                0u8 => CsvParserRuleKind::escaped,
                1u8 => CsvParserRuleKind::textdata,
                2u8 => CsvParserRuleKind::record,
                3u8 => CsvParserRuleKind::csv,
                _ => CsvParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CsvParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor.children().map(move |c| CsvParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<CsvParserNodeView<'p>> {
            self.cursor.child(i).map(|c| CsvParserNodeView::from_cursor(c, self.input))
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
    impl crate::runtime::Root for CsvParser {
        type View<'p> = csvView<'p>;
        #[inline]
        fn make_view<'p>(
            tape: &'p crate::runtime::tape::Tape<()>,
            input: &'p str,
            root: crate::runtime::tape::TapeOffset,
        ) -> Self::View<'p> {
            csvView::new(tape, input, root)
        }
    }
    impl CsvParser {
        /// The name of the root rule for this grammar.
        #[inline]
        pub fn root_rule_name() -> &'static str {
            "csv"
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
    pub struct CsvParserEscapedProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: (u32, u32),
    }
    impl CsvParserEscapedProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "escaped";
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
    pub struct CsvParserTextdataProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: (u32, u32),
    }
    impl CsvParserTextdataProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "textdata";
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
    pub const PROJECTION_DIRECT_TO_STRUCT: &[(&str, &str); 2usize] = &[
        ("escaped", "CsvParserEscapedProjection"),
        ("textdata", "CsvParserTextdataProjection"),
    ];
    /// AY-II.W0.d — grammar-declared `-> Name` bindings, indexed in
    /// lockstep with `PROJECTION_DIRECT_TO_STRUCT`. Empty string for
    /// admissions that did not spell a named type.
    #[doc(hidden)]
    pub const PROJECTION_NAMED_BINDINGS: &[&str; 2usize] = &["", ""];
    /// AY-II.W0.d — canonical evidence that every admission has a
    /// matching `materialize_projection_<rule>_<Grammar>` fn.
    /// Indexed in lockstep with `PROJECTION_DIRECT_TO_STRUCT`; the
    /// wire-contract totality test asserts both slices share the
    /// same length per grammar.
    #[doc(hidden)]
    pub const PROJECTION_MATERIALIZERS: &[&str; 2usize] = &[
        "materialize_projection_escaped_CsvParser",
        "materialize_projection_textdata_CsvParser",
    ];
    /// AY-II.W0.d — canonical evidence that every admission has a
    /// matching `<Grammar>Value::<RuleName>` enum variant
    /// (production consumer). Indexed in lockstep with
    /// `PROJECTION_DIRECT_TO_STRUCT`.
    #[doc(hidden)]
    pub const PROJECTION_CONSUMERS: &[&str; 2usize] = &[
        "CsvParserValue::escaped",
        "CsvParserValue::textdata",
    ];
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_escaped() -> (&'static str, usize, &'static str) {
        ("escaped", 1, "")
    }
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_textdata() -> (&'static str, usize, &'static str) {
        ("textdata", 1, "")
    }
    /// AY-II.W0'.b — grammar-emitted value enum. Eager
    /// materialisation target for `Parsed::to_value()`. Variants
    /// enumerate non-transparent rules; admitted rules carry the
    /// matching `<Grammar><RuleCamel>Projection` struct directly,
    /// non-admitted rules carry their shape-classified payload.
    #[derive(Clone, Debug)]
    pub enum CsvParserValue<'p> {
        escaped(CsvParserEscapedProjection),
        textdata(CsvParserTextdataProjection),
        record(::std::vec::Vec<CsvParserValue<'p>>),
        csv(::std::vec::Vec<CsvParserValue<'p>>),
        /// Fallback for records whose `variant_idx` is not a
        /// known rule discriminator (recovered records, stray
        /// sub-variant indices).
        Unknown(CsvParserNodeView<'p>),
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
    fn project_rule_kind_CsvParser(
        kind: crate::runtime::tape::TapeKind,
        variant_idx: u8,
    ) -> CsvParserRuleKind {
        if variant_idx == 0 && kind.is_compound() {
            return CsvParserRuleKind::Unknown;
        }
        match variant_idx {
            0u8 => CsvParserRuleKind::escaped,
            1u8 => CsvParserRuleKind::textdata,
            2u8 => CsvParserRuleKind::record,
            3u8 => CsvParserRuleKind::csv,
            _ => CsvParserRuleKind::Unknown,
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
    fn project_push_children_CsvParser<'p>(
        output: &crate::runtime::tape::Tape<CsvParser>,
        input: &'p str,
        offset: u32,
        out: &mut ::std::vec::Vec<CsvParserValue<'p>>,
    ) {
        let __tape = output.tape();
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
                project_push_children_CsvParser(output, input, __child.offset().0, out);
            }
        } else {
            out.push(project_frame_CsvParser(output, input, offset));
        }
    }
    /// AY-II.W0'.b — per-frame projector. Reads one record from the
    /// fused-pipeline [`FusedOutput`](crate::runtime::tape::Tape)
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
    fn project_frame_CsvParser<'p>(
        output: &crate::runtime::tape::Tape<CsvParser>,
        input: &'p str,
        offset: u32,
    ) -> CsvParserValue<'p> {
        let __tape = output.tape();
        let __rec = match __tape.try_get(crate::runtime::tape::TapeOffset(offset)) {
            ::core::option::Option::Some(r) => r,
            ::core::option::Option::None => {
                ::core::panic!(
                    "AY-II.W0'.b: tape offset {} out of range (tape len: {})", offset,
                    __tape.len(),
                );
            }
        };
        match project_rule_kind_CsvParser(__rec.kind(), __rec.variant_idx()) {
            CsvParserRuleKind::escaped => {
                let proj = materialize_projection_escaped_CsvParser(
                        output,
                        input,
                        offset,
                    )
                    .unwrap_or_else(|| {
                        ::core::panic!(
                            "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                            "escaped", offset,
                        );
                    });
                CsvParserValue::escaped(proj)
            }
            CsvParserRuleKind::textdata => {
                let proj = materialize_projection_textdata_CsvParser(
                        output,
                        input,
                        offset,
                    )
                    .unwrap_or_else(|| {
                        ::core::panic!(
                            "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                            "textdata", offset,
                        );
                    });
                CsvParserValue::textdata(proj)
            }
            CsvParserRuleKind::record => {
                let mut children: ::std::vec::Vec<CsvParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_CsvParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                CsvParserValue::record(children)
            }
            CsvParserRuleKind::csv => {
                let mut children: ::std::vec::Vec<CsvParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_CsvParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                CsvParserValue::csv(children)
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
    #[inline]
    fn project_value_CsvParser<'p>(
        output: &crate::runtime::tape::Tape<CsvParser>,
        input: &'p str,
    ) -> CsvParserValue<'p> {
        let root_off = output.value_root_offset();
        project_frame_CsvParser(output, input, root_off)
    }
    impl crate::runtime::ValueRoot for CsvParser {
        type Value<'p> = CsvParserValue<'p>;
        #[inline]
        fn project_value_output<'p>(
            output: &crate::runtime::tape::Tape<CsvParser>,
            input: &'p str,
        ) -> Self::Value<'p>
        where
            Self: 'p,
        {
            project_value_CsvParser(output, input)
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
        view: CsvParserNodeView<'p>,
        path: crate::runtime::Path<'_>,
    ) -> ::core::option::Option<CsvParserNodeView<'p>> {
        let cur_input = view.input();
        let mut cur = view;
        for seg in path.iter() {
            match seg {
                crate::runtime::PathSegment::Field(key) => {
                    match cur.rule_kind() {
                        CsvParserRuleKind::record | CsvParserRuleKind::csv => {
                            let parent = cur.cursor();
                            let (_, parent_end) = parent.span();
                            let mut iter = parent.bounded_lookahead(parent_end);
                            let mut hit: ::core::option::Option<CsvParserNodeView<'p>> = None;
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
                                        CsvParserNodeView::from_cursor(v_cur, cur_input),
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
                        _ => {
                            cur = cur.child(*i)?;
                        }
                    }
                }
            }
        }
        ::core::option::Option::Some(cur)
    }
    impl crate::runtime::PathQuery<&'static str> for CsvParser {
        #[inline]
        fn query<'p>(
            view: Self::View<'p>,
            path: crate::runtime::Path<'_>,
        ) -> ::core::option::Option<&'static str>
        where
            Self: 'p,
        {
            let node = CsvParserNodeView::from_cursor(view.cursor(), view.input());
            __path_walk(node, path)?;
            ::core::option::Option::None
        }
    }
    impl crate::runtime::PathQuery<f64> for CsvParser {
        #[inline]
        fn query<'p>(
            view: Self::View<'p>,
            path: crate::runtime::Path<'_>,
        ) -> ::core::option::Option<f64>
        where
            Self: 'p,
        {
            let node = CsvParserNodeView::from_cursor(view.cursor(), view.input());
            let hit = __path_walk(node, path)?;
            let tape = hit.cursor().tape();
            let rec = hit.cursor().record();
            if let ::core::option::Option::Some(v) = tape.payload_f64(rec) {
                return ::core::option::Option::Some(v);
            }
            hit.span_text().parse::<f64>().ok()
        }
    }
    impl crate::runtime::PathQuery<bool> for CsvParser {
        #[inline]
        fn query<'p>(
            view: Self::View<'p>,
            path: crate::runtime::Path<'_>,
        ) -> ::core::option::Option<bool>
        where
            Self: 'p,
        {
            let node = CsvParserNodeView::from_cursor(view.cursor(), view.input());
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
    /// fused-pipeline [`FusedOutput`](crate::runtime::tape::Tape)
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
    pub fn materialize_projection_escaped_CsvParser<'p>(
        output: &crate::runtime::tape::Tape<CsvParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<CsvParserEscapedProjection> {
        let _ = input;
        let frame = output.value_frame_at(offset)?;
        let __bytes: &[u8] = &[];
        let _ = __bytes;
        let field_0: (u32, u32) = (frame.span_lo, frame.span_hi);
        ::core::option::Option::Some(CsvParserEscapedProjection {
            field_0,
        })
    }
    /// AY-II.W0'.b — grammar-derived direct-to-struct projection
    /// helper. Reads the admitted rule's frame from the
    /// fused-pipeline [`FusedOutput`](crate::runtime::tape::Tape)
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
    pub fn materialize_projection_textdata_CsvParser<'p>(
        output: &crate::runtime::tape::Tape<CsvParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<CsvParserTextdataProjection> {
        let _ = input;
        let frame = output.value_frame_at(offset)?;
        let __bytes: &[u8] = &[];
        let _ = __bytes;
        let field_0: (u32, u32) = (frame.span_lo, frame.span_hi);
        ::core::option::Option::Some(CsvParserTextdataProjection {
            field_0,
        })
    }
    impl CsvParser {
        pub fn serialize_escaped<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CsvParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_textdata<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CsvParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_record<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CsvParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_csv<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CsvParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        fn __dispatch_serialize<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CsvParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            match __v.variant_idx() {
                0u8 => {
                    Self::serialize_escaped(__v, __ser);
                }
                1u8 => {
                    Self::serialize_textdata(__v, __ser);
                }
                2u8 => {
                    Self::serialize_record(__v, __ser);
                }
                3u8 => {
                    Self::serialize_csv(__v, __ser);
                }
                _ => {
                    __ser.text(__v.span_text());
                }
            }
        }
        pub fn serialize_compact<'a>(__v: CsvParserNodeView<'a>) -> String {
            let mut __ser = ::bbnf_ser::StringSerializer::new();
            Self::serialize_csv(__v, &mut __ser);
            __ser.finish()
        }
        pub fn serialize<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CsvParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            Self::serialize_csv(__v, __ser);
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
        /// dispatcher against a single `FusedBuilder`. The
        /// hot path here:
        ///
        /// 1. Allocate a sized `FusedBuilder` — owns both
        ///    tape + value-frame substrates in one handle.
        /// 2. Call the shape dispatcher, which decomposes
        ///    into per-shape bodies inlined at the call
        ///    site. Every compound / leaf push stamps both
        ///    column families atomically.
        /// 3. Finalise via `FusedBuilder::finish_fused::<Self>`
        ///    — returns `FusedOutput<Self>` holding tape +
        ///    value, handed to `Parsed::new_fused_output` directly.
        pub fn parse(
            input: &str,
        ) -> ::core::result::Result<
            crate::runtime::Parsed<'_, Self>,
            crate::runtime::ParseErr,
        > {
            let __input_bytes = input.as_bytes();
            let mut state = __shape_support_CsvParser::ScanState::new();
            let mut tape = crate::runtime::tape::Tape::<
                (),
            >::with_capacity(GRAMMAR_PROFILE.capacity_for(input.len()));
            let root_off = {
                let mut pos: usize = 0;
                let off = parse_CsvParser_csv(
                        __input_bytes,
                        &mut pos,
                        &mut state,
                        &mut tape,
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
                off
            };
            let tape = tape
                .finish_fused::<Self>(root_off.0)
                .map_err(crate::runtime::ParseErr::Tape)?;
            ::core::result::Result::Ok(
                crate::runtime::Parsed::new(tape, input, root_off),
            )
        }
    }
    #[inline]
    pub(crate) fn cst_identifier_text<'p>(
        _cursor: crate::runtime::tape::TapeCursor<'p>,
        _input: &'p str,
    ) -> &'p str {
        ""
    }
    #[inline]
    pub(crate) fn cst_identifier_span<'p>(
        _cursor: crate::runtime::tape::TapeCursor<'p>,
        _input: &'p str,
    ) -> (u32, u32) {
        (0, 0)
    }
}
pub use __csvparser_emit_impl::*;
