//! AUTO-GENERATED from `[workspace.metadata.bbnf.grammars]` — do not edit manually.
//! Regenerate: cargo xtask regen --grammar bnf

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

pub struct BnfParser;
mod __bnfparser_emit_impl {
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
    pub const GRAMMAR_BnfParser: [&'static str; 1usize] = [
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../../grammar/bnf/bnf.bbnf")),
    ];
    static __GRAMMAR_PROFILE_ALPHABET: [u8; 4usize] = [34, 60, 62, 124];
    /// Per-grammar codegen fingerprint — consolidated static
    /// profile emitted by Tranche AV Phase 1. Every downstream
    /// consumer (tape capacity, scanner dispatch) reads the
    /// matching field.
    pub const GRAMMAR_PROFILE: ::bbnf::runtime::tape::GrammarProfile = ::bbnf::runtime::tape::GrammarProfile {
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
    pub const PRECEDENCE_ENTRIES: &[::bbnf::runtime::tape::DtaPrecedenceEntry] = &[];
    /// AW-III.W6.5 — total mined operator count for this
    /// grammar. Non-zero iff the lift admitted ≥ 1 chain OR the
    /// shape classifier admitted ≥ 1 single-rung Pratt rule.
    pub const PRECEDENCE_OPERATOR_COUNT: usize = 0usize;
    static __DTA_REGEX_1: &str = "(\\\\.|[^\"\\\\])*";
    static __DTA_REGEX_5: &str = "[a-zA-Z_][a-zA-Z0-9_-]*";
    static __DTA_REGEX_12: &str = "[ \\t]*";
    static __DTA_REGEX_33: &str = "\\n";
    /// AY.W4.3 — per-pattern (LAST-byte-set lo, hi) packed
    /// `CharSet128` tuples. `(0, 0)` means narrowing is
    /// disabled for that pattern (suffix not deterministic).
    ///
    /// The adapter consults this when invoked: if the pattern's
    /// entry is non-zero AND the input slice from `pos` does not
    /// contain any byte in the LAST set, the regex cannot
    /// complete a match — skip the DFA walk entirely.
    #[allow(dead_code)]
    pub(crate) const __REGEX_LAST_BYTE_SET_BnfParser: [(u64, u64); 4] = [
        (0, 0),
        (0, 0),
        (0, 0),
        (1024, 0),
    ];
    #[inline]
    #[cold]
    fn __regex_scan_BnfParser(
        pattern: &str,
        input: &[u8],
        pos: usize,
    ) -> ::core::option::Option<u32> {
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_1.as_ptr())
            || pattern == __DTA_REGEX_1
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_BnfParser[0];
                if (__lb_lo | __lb_hi) != 0 {
                    let __scan_end = (pos + 256).min(input.len());
                    let __slice = &input[pos..__scan_end];
                    let mut __found = false;
                    for &__b in __slice {
                        let __test = if __b < 64 {
                            (__lb_lo >> __b) & 1
                        } else if __b < 128 {
                            (__lb_hi >> (__b - 64)) & 1
                        } else {
                            0
                        };
                        if __test != 0 {
                            __found = true;
                            break;
                        }
                    }
                    if !__found && __scan_end == input.len() {
                        return ::core::option::Option::None;
                    }
                }
            }
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
                                | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 0,
                                92 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 11 | 12 | 13 | 14
                                | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26
                                | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_5.as_ptr())
            || pattern == __DTA_REGEX_5
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_BnfParser[1];
                if (__lb_lo | __lb_hi) != 0 {
                    let __scan_end = (pos + 256).min(input.len());
                    let __slice = &input[pos..__scan_end];
                    let mut __found = false;
                    for &__b in __slice {
                        let __test = if __b < 64 {
                            (__lb_lo >> __b) & 1
                        } else if __b < 128 {
                            (__lb_hi >> (__b - 64)) & 1
                        } else {
                            0
                        };
                        if __test != 0 {
                            __found = true;
                            break;
                        }
                    }
                    if !__found && __scan_end == input.len() {
                        return ::core::option::Option::None;
                    }
                }
            }
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
                                65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76
                                | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88
                                | 89 | 90 | 95 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104
                                | 105 | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114
                                | 115 | 116 | 117 | 118 | 119 | 120 | 121 | 122 => {
                                    __dfa_state = 1;
                                }
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                45 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 65
                                | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77
                                | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89
                                | 90 | 95 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
                                | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115
                                | 116 | 117 | 118 | 119 | 120 | 121 | 122 => __dfa_state = 1,
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_12.as_ptr())
            || pattern == __DTA_REGEX_12
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_BnfParser[2];
                if (__lb_lo | __lb_hi) != 0 {
                    let __scan_end = (pos + 256).min(input.len());
                    let __slice = &input[pos..__scan_end];
                    let mut __found = false;
                    for &__b in __slice {
                        let __test = if __b < 64 {
                            (__lb_lo >> __b) & 1
                        } else if __b < 128 {
                            (__lb_hi >> (__b - 64)) & 1
                        } else {
                            0
                        };
                        if __test != 0 {
                            __found = true;
                            break;
                        }
                    }
                    if !__found && __scan_end == input.len() {
                        return ::core::option::Option::None;
                    }
                }
            }
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
                                9 | 32 => __dfa_state = 0,
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_33.as_ptr())
            || pattern == __DTA_REGEX_33
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_BnfParser[3];
                if (__lb_lo | __lb_hi) != 0 {
                    let __scan_end = (pos + 256).min(input.len());
                    let __slice = &input[pos..__scan_end];
                    let mut __found = false;
                    for &__b in __slice {
                        let __test = if __b < 64 {
                            (__lb_lo >> __b) & 1
                        } else if __b < 128 {
                            (__lb_hi >> (__b - 64)) & 1
                        } else {
                            0
                        };
                        if __test != 0 {
                            __found = true;
                            break;
                        }
                    }
                    if !__found && __scan_end == input.len() {
                        return ::core::option::Option::None;
                    }
                }
            }
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
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
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
    pub(crate) mod __shape_support_BnfParser {
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
                ::bbnf::runtime::tape::StructuralIndex,
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
        ) -> &'a ::bbnf::runtime::tape::StructuralIndex {
            state
                .structural_index
                .get_or_init(|| {
                    ::bbnf::runtime::tape::scan_structural(
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
    pub fn parse_flat_BnfParser_terminal(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_BnfParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.columns_mut().len() as u32;
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [34u8] {
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: at as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            *p = end;
            let _ = builder
                .push_leaf_with(
                    ::bbnf::runtime::tape::TapeKind::Literal,
                    at as u32,
                    end as u32,
                    0u8,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
        }
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_BnfParser(
                    "(\\\\.|[^\"\\\\])*",
                    input,
                    *p,
                ) else {
                    return ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                        offset: span_lo,
                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    });
                };
                *p += match_len as usize;
                let span_hi = *p as u32;
                let _ = builder
                    .push_leaf_with(
                        ::bbnf::runtime::tape::TapeKind::Span,
                        span_lo,
                        span_hi,
                        0u8,
                        0,
                        ::bbnf::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [34u8] {
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: at as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            *p = end;
            let _ = builder
                .push_leaf_with(
                    ::bbnf::runtime::tape::TapeKind::Literal,
                    at as u32,
                    end as u32,
                    0u8,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
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
                ::bbnf::runtime::tape::TapeOffset(outer_child),
            );
        Ok(::bbnf::runtime::tape::TapeOffset(outer_off))
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
    pub fn parse_flat_BnfParser_nonterminal(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_BnfParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.columns_mut().len() as u32;
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [60u8] {
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: at as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            *p = end;
            let _ = builder
                .push_leaf_with(
                    ::bbnf::runtime::tape::TapeKind::Literal,
                    at as u32,
                    end as u32,
                    1u8,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
        }
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_BnfParser(
                    "[a-zA-Z_][a-zA-Z0-9_-]*",
                    input,
                    *p,
                ) else {
                    return ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                        offset: span_lo,
                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    });
                };
                *p += match_len as usize;
                let span_hi = *p as u32;
                let _ = builder
                    .push_leaf_with(
                        ::bbnf::runtime::tape::TapeKind::Span,
                        span_lo,
                        span_hi,
                        1u8,
                        0,
                        ::bbnf::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [62u8] {
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: at as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            *p = end;
            let _ = builder
                .push_leaf_with(
                    ::bbnf::runtime::tape::TapeKind::Literal,
                    at as u32,
                    end as u32,
                    1u8,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
                span_lo,
                1u8,
                0u8,
                0u8,
                0u16,
            );
        builder
            .end_compound_post_order(
                outer_off,
                span_hi,
                ::bbnf::runtime::tape::TapeOffset(outer_child),
            );
        Ok(::bbnf::runtime::tape::TapeOffset(outer_off))
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
    pub fn parse_flat_BnfParser_alternation(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_BnfParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.columns_mut().len() as u32;
        {
            let repeat_lo = *p as u32;
            let repeat_child = builder.columns_mut().len() as u32;
            let mut iter_count: u32 = 0;
            loop {
                let save_p = *p;
                let save_cols = builder.columns_mut().len() as u32;
                let iter_lo = *p as u32;
                let iter_child = builder.columns_mut().len() as u32;
                let attempt = (|| -> ::core::result::Result<
                    (),
                    ::bbnf::runtime::tape::DtaError,
                > {
                    {
                        let first = __shape_support_BnfParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                                offset: *p as u32,
                            })?;
                        'try_branches: loop {
                            match first {
                                34u8 => {
                                    let attempt_p = *p;
                                    let attempt_len = builder.columns_mut().len() as u32;
                                    match {
                                        let _ = __shape_support_BnfParser::skip_space(
                                            input,
                                            p,
                                            state,
                                        );
                                        parse_flat_BnfParser_terminal(input, p, state, builder)
                                    } {
                                        Ok(_) => break 'try_branches,
                                        Err(_) => {
                                            *p = attempt_p;
                                            builder.rollback_to(attempt_len);
                                        }
                                    }
                                }
                                60u8 => {
                                    let attempt_p = *p;
                                    let attempt_len = builder.columns_mut().len() as u32;
                                    match {
                                        let _ = __shape_support_BnfParser::skip_space(
                                            input,
                                            p,
                                            state,
                                        );
                                        parse_flat_BnfParser_nonterminal(input, p, state, builder)
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
                            return ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                                offset: *p as u32,
                                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                            });
                        }
                    }
                    {
                        let span_lo = *p as u32;
                        let Some(match_len) = __regex_scan_BnfParser(
                            "[ \\t]*",
                            input,
                            *p,
                        ) else {
                            return ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                                offset: span_lo,
                                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                            });
                        };
                        *p += match_len as usize;
                        let span_hi = *p as u32;
                        let _ = builder
                            .push_leaf_with(
                                ::bbnf::runtime::tape::TapeKind::Span,
                                span_lo,
                                span_hi,
                                2u8,
                                0,
                                ::bbnf::runtime::tape::PayloadData::None,
                            );
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
                        ::bbnf::runtime::tape::TapeKind::Seq,
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
                        ::bbnf::runtime::tape::TapeOffset(iter_child),
                    );
                iter_count = iter_count.saturating_add(1);
            }
            if iter_count < (1usize as u32) {
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            let repeat_hi = *p as u32;
            let __repeat_off = builder
                .begin_compound(
                    ::bbnf::runtime::tape::TapeKind::Repeat,
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
                    ::bbnf::runtime::tape::TapeOffset(repeat_child),
                );
        }
        {
            let repeat_lo = *p as u32;
            let repeat_child = builder.columns_mut().len() as u32;
            let mut iter_count: u32 = 0;
            loop {
                let save_p = *p;
                let save_cols = builder.columns_mut().len() as u32;
                let iter_lo = *p as u32;
                let iter_child = builder.columns_mut().len() as u32;
                let attempt = (|| -> ::core::result::Result<
                    (),
                    ::bbnf::runtime::tape::DtaError,
                > {
                    {
                        let span_lo = *p as u32;
                        let Some(match_len) = __regex_scan_BnfParser(
                            "[ \\t]*",
                            input,
                            *p,
                        ) else {
                            return ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                                offset: span_lo,
                                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                            });
                        };
                        *p += match_len as usize;
                        let span_hi = *p as u32;
                        let _ = builder
                            .push_leaf_with(
                                ::bbnf::runtime::tape::TapeKind::Span,
                                span_lo,
                                span_hi,
                                2u8,
                                0,
                                ::bbnf::runtime::tape::PayloadData::None,
                            );
                    }
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() < end || input[at..end] != [124u8] {
                        return Err(::bbnf::runtime::tape::DtaError::Syntax {
                            offset: at as u32,
                            failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                            failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                        });
                    }
                    *p = end;
                    let _ = builder
                        .push_leaf_with(
                            ::bbnf::runtime::tape::TapeKind::Literal,
                            at as u32,
                            end as u32,
                            2u8,
                            0,
                            ::bbnf::runtime::tape::PayloadData::None,
                        );
                    {
                        let span_lo = *p as u32;
                        let Some(match_len) = __regex_scan_BnfParser(
                            "[ \\t]*",
                            input,
                            *p,
                        ) else {
                            return ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                                offset: span_lo,
                                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                            });
                        };
                        *p += match_len as usize;
                        let span_hi = *p as u32;
                        let _ = builder
                            .push_leaf_with(
                                ::bbnf::runtime::tape::TapeKind::Span,
                                span_lo,
                                span_hi,
                                2u8,
                                0,
                                ::bbnf::runtime::tape::PayloadData::None,
                            );
                    }
                    let repeat_lo = *p as u32;
                    let repeat_child = builder.columns_mut().len() as u32;
                    let mut iter_count: u32 = 0;
                    loop {
                        let save_p = *p;
                        let save_cols = builder.columns_mut().len() as u32;
                        let iter_lo = *p as u32;
                        let iter_child = builder.columns_mut().len() as u32;
                        let attempt = (|| -> ::core::result::Result<
                            (),
                            ::bbnf::runtime::tape::DtaError,
                        > {
                            {
                                let first = __shape_support_BnfParser::skip_space(
                                        input,
                                        p,
                                        state,
                                    )
                                    .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                                        offset: *p as u32,
                                    })?;
                                'try_branches: loop {
                                    match first {
                                        34u8 => {
                                            let attempt_p = *p;
                                            let attempt_len = builder.columns_mut().len() as u32;
                                            match {
                                                let _ = __shape_support_BnfParser::skip_space(
                                                    input,
                                                    p,
                                                    state,
                                                );
                                                parse_flat_BnfParser_terminal(input, p, state, builder)
                                            } {
                                                Ok(_) => break 'try_branches,
                                                Err(_) => {
                                                    *p = attempt_p;
                                                    builder.rollback_to(attempt_len);
                                                }
                                            }
                                        }
                                        60u8 => {
                                            let attempt_p = *p;
                                            let attempt_len = builder.columns_mut().len() as u32;
                                            match {
                                                let _ = __shape_support_BnfParser::skip_space(
                                                    input,
                                                    p,
                                                    state,
                                                );
                                                parse_flat_BnfParser_nonterminal(input, p, state, builder)
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
                                    return ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                                        offset: *p as u32,
                                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                                    });
                                }
                            }
                            {
                                let span_lo = *p as u32;
                                let Some(match_len) = __regex_scan_BnfParser(
                                    "[ \\t]*",
                                    input,
                                    *p,
                                ) else {
                                    return ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                                        offset: span_lo,
                                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                                    });
                                };
                                *p += match_len as usize;
                                let span_hi = *p as u32;
                                let _ = builder
                                    .push_leaf_with(
                                        ::bbnf::runtime::tape::TapeKind::Span,
                                        span_lo,
                                        span_hi,
                                        2u8,
                                        0,
                                        ::bbnf::runtime::tape::PayloadData::None,
                                    );
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
                                ::bbnf::runtime::tape::TapeKind::Seq,
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
                                ::bbnf::runtime::tape::TapeOffset(iter_child),
                            );
                        iter_count = iter_count.saturating_add(1);
                    }
                    if iter_count < (1usize as u32) {
                        return Err(::bbnf::runtime::tape::DtaError::Syntax {
                            offset: *p as u32,
                            failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                            failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                        });
                    }
                    let repeat_hi = *p as u32;
                    let __repeat_off = builder
                        .begin_compound(
                            ::bbnf::runtime::tape::TapeKind::Repeat,
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
                            ::bbnf::runtime::tape::TapeOffset(repeat_child),
                        );
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
                        ::bbnf::runtime::tape::TapeKind::Seq,
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
                        ::bbnf::runtime::tape::TapeOffset(iter_child),
                    );
                iter_count = iter_count.saturating_add(1);
            }
            if iter_count < (0usize as u32) {
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            let repeat_hi = *p as u32;
            let __repeat_off = builder
                .begin_compound(
                    ::bbnf::runtime::tape::TapeKind::Repeat,
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
                    ::bbnf::runtime::tape::TapeOffset(repeat_child),
                );
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
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
                ::bbnf::runtime::tape::TapeOffset(outer_child),
            );
        Ok(::bbnf::runtime::tape::TapeOffset(outer_off))
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
    pub fn parse_flat_BnfParser_rule(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_BnfParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.columns_mut().len() as u32;
        {
            let _ = ({
                let _ = __shape_support_BnfParser::skip_space(input, p, state);
                parse_flat_BnfParser_nonterminal(input, p, state, builder)
            })?;
        }
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_BnfParser("[ \\t]*", input, *p) else {
                    return ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                        offset: span_lo,
                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    });
                };
                *p += match_len as usize;
                let span_hi = *p as u32;
                let _ = builder
                    .push_leaf_with(
                        ::bbnf::runtime::tape::TapeKind::Span,
                        span_lo,
                        span_hi,
                        3u8,
                        0,
                        ::bbnf::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            let at = *p;
            let end = at + 3usize;
            if input.len() < end || input[at..end] != [58u8, 58u8, 61u8] {
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: at as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            *p = end;
            let _ = builder
                .push_leaf_with(
                    ::bbnf::runtime::tape::TapeKind::Literal,
                    at as u32,
                    end as u32,
                    3u8,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
        }
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_BnfParser("[ \\t]*", input, *p) else {
                    return ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                        offset: span_lo,
                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    });
                };
                *p += match_len as usize;
                let span_hi = *p as u32;
                let _ = builder
                    .push_leaf_with(
                        ::bbnf::runtime::tape::TapeKind::Span,
                        span_lo,
                        span_hi,
                        3u8,
                        0,
                        ::bbnf::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            let _ = ({
                let _ = __shape_support_BnfParser::skip_space(input, p, state);
                parse_flat_BnfParser_alternation(input, p, state, builder)
            })?;
        }
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_BnfParser("[ \\t]*", input, *p) else {
                    return ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                        offset: span_lo,
                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    });
                };
                *p += match_len as usize;
                let span_hi = *p as u32;
                let _ = builder
                    .push_leaf_with(
                        ::bbnf::runtime::tape::TapeKind::Span,
                        span_lo,
                        span_hi,
                        3u8,
                        0,
                        ::bbnf::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            let repeat_lo = *p as u32;
            let repeat_child = builder.columns_mut().len() as u32;
            let iter_save_p = *p;
            let iter_save_cols = builder.columns_mut().len() as u32;
            let iter_lo = *p as u32;
            let iter_child = builder.columns_mut().len() as u32;
            let opt_attempt: ::core::result::Result<
                (),
                ::bbnf::runtime::tape::DtaError,
            > = (|| {
                {
                    let span_lo = *p as u32;
                    let Some(match_len) = __regex_scan_BnfParser("\\n", input, *p) else {
                        return ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                            offset: span_lo,
                            failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                            failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                        });
                    };
                    *p += match_len as usize;
                    let span_hi = *p as u32;
                    let _ = builder
                        .push_leaf_with(
                            ::bbnf::runtime::tape::TapeKind::Span,
                            span_lo,
                            span_hi,
                            3u8,
                            0,
                            ::bbnf::runtime::tape::PayloadData::None,
                        );
                }
                Ok(())
            })();
            let matched = opt_attempt.is_ok();
            if !matched {
                *p = iter_save_p;
                builder.rollback_to(iter_save_cols);
            } else {
                let iter_hi = *p as u32;
                let __iter_off = builder
                    .begin_compound(
                        ::bbnf::runtime::tape::TapeKind::Seq,
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
                        ::bbnf::runtime::tape::TapeOffset(iter_child),
                    );
            }
            let repeat_hi = *p as u32;
            let __repeat_off = builder
                .begin_compound(
                    ::bbnf::runtime::tape::TapeKind::Repeat,
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
                    ::bbnf::runtime::tape::TapeOffset(repeat_child),
                );
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
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
                ::bbnf::runtime::tape::TapeOffset(outer_child),
            );
        Ok(::bbnf::runtime::tape::TapeOffset(outer_off))
    }
    /// AX.W0a.2.a — per-grammar Array-shape parse function
    /// (Shape 2 — direct-Repeat entry-rule list,
    /// **walker-tape-identical**).
    ///
    /// Emits a single Rule compound (the Repeat frame)
    /// carrying the per-iteration children. Matches the
    /// walker's direct lowering of a `Repeat { .. }` body
    /// where the rule's variant stamp lands on the Rule
    /// compound itself (no outer Seq wrapper).
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_array_BnfParser_grammar(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_BnfParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let repeat_open = *p as u32;
        let repeat_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Rule,
                repeat_open,
                0u8,
                0u8,
                0u8,
                0u16,
            );
        loop {
            let iter_save_p = *p;
            let __iter_save_cols = builder.columns_mut().len() as u32;
            if input.get(*p).is_none() {
                break;
            }
            let iter_result: ::core::result::Result<
                (),
                ::bbnf::runtime::tape::DtaError,
            > = (|| {
                let iter_open = *p as u32;
                let iter_off = builder
                    .begin_compound(
                        ::bbnf::runtime::tape::TapeKind::Seq,
                        iter_open,
                        0,
                        0u8,
                        0u8,
                        0u16,
                    );
                let _ = __shape_support_BnfParser::skip_space(input, p, state);
                let _value_off = ({
                    let _ = __shape_support_BnfParser::skip_space(input, p, state);
                    parse_flat_BnfParser_rule(input, p, state, builder)
                })?;
                let _ = __shape_support_BnfParser::skip_space(input, p, state);
                let iter_close = *p as u32;
                builder.end_compound(iter_off, iter_close);
                Ok(())
            })();
            match iter_result {
                Ok(()) => {
                    if *p == iter_save_p {
                        break;
                    }
                }
                Err(_) => {
                    *p = iter_save_p;
                    builder.rollback_to(__iter_save_cols);
                    break;
                }
            }
        }
        let repeat_close = *p as u32;
        builder.end_compound(repeat_off, repeat_close);
        Ok(::bbnf::runtime::tape::TapeOffset(repeat_off))
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
    pub fn parse_flat_visitor_BnfParser_terminal<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_BnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
    where
        V: ::bbnf::runtime::tape::ObjectVisitor + ::bbnf::runtime::tape::ArrayVisitor
            + ::bbnf::runtime::tape::StringVisitor + ::bbnf::runtime::tape::NumberVisitor
            + ::bbnf::runtime::tape::KeywordVisitor,
    {
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [34u8] {
                return Err(::bbnf::runtime::ParseErr::Syntax {
                    offset: at as u32,
                    rule: None,
                });
            }
            *p = end;
        }
        {
            {
                let span_lo = *p;
                let Some(match_len) = __regex_scan_BnfParser(
                    "(\\\\.|[^\"\\\\])*",
                    input,
                    *p,
                ) else {
                    return ::core::result::Result::Err(::bbnf::runtime::ParseErr::Syntax {
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
                return Err(::bbnf::runtime::ParseErr::Syntax {
                    offset: at as u32,
                    rule: None,
                });
            }
            *p = end;
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
    pub fn parse_flat_visitor_BnfParser_nonterminal<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_BnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
    where
        V: ::bbnf::runtime::tape::ObjectVisitor + ::bbnf::runtime::tape::ArrayVisitor
            + ::bbnf::runtime::tape::StringVisitor + ::bbnf::runtime::tape::NumberVisitor
            + ::bbnf::runtime::tape::KeywordVisitor,
    {
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [60u8] {
                return Err(::bbnf::runtime::ParseErr::Syntax {
                    offset: at as u32,
                    rule: None,
                });
            }
            *p = end;
        }
        {
            {
                let span_lo = *p;
                let Some(match_len) = __regex_scan_BnfParser(
                    "[a-zA-Z_][a-zA-Z0-9_-]*",
                    input,
                    *p,
                ) else {
                    return ::core::result::Result::Err(::bbnf::runtime::ParseErr::Syntax {
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
            if input.len() < end || input[at..end] != [62u8] {
                return Err(::bbnf::runtime::ParseErr::Syntax {
                    offset: at as u32,
                    rule: None,
                });
            }
            *p = end;
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
    pub fn parse_flat_visitor_BnfParser_alternation<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_BnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
    where
        V: ::bbnf::runtime::tape::ObjectVisitor + ::bbnf::runtime::tape::ArrayVisitor
            + ::bbnf::runtime::tape::StringVisitor + ::bbnf::runtime::tape::NumberVisitor
            + ::bbnf::runtime::tape::KeywordVisitor,
    {
        {
            let mut iter_count: u32 = 0;
            loop {
                let save_p = *p;
                let res = (|| -> ::core::result::Result<(), ::bbnf::runtime::ParseErr> {
                    {
                        let first = __shape_support_BnfParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(::bbnf::runtime::ParseErr::Syntax {
                                offset: *p as u32,
                                rule: None,
                            })?;
                        'try_branches: loop {
                            match first {
                                34u8 => {
                                    let attempt_p = *p;
                                    match {
                                        let _ = __shape_support_BnfParser::skip_space(
                                            input,
                                            p,
                                            state,
                                        );
                                        parse_flat_visitor_BnfParser_terminal(
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
                                60u8 => {
                                    let attempt_p = *p;
                                    match {
                                        let _ = __shape_support_BnfParser::skip_space(
                                            input,
                                            p,
                                            state,
                                        );
                                        parse_flat_visitor_BnfParser_nonterminal(
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
                            return ::core::result::Result::Err(::bbnf::runtime::ParseErr::Syntax {
                                offset: *p as u32,
                                rule: None,
                            });
                        }
                    }
                    {
                        let span_lo = *p;
                        let Some(match_len) = __regex_scan_BnfParser(
                            "[ \\t]*",
                            input,
                            *p,
                        ) else {
                            return ::core::result::Result::Err(::bbnf::runtime::ParseErr::Syntax {
                                offset: span_lo as u32,
                                rule: None,
                            });
                        };
                        *p = span_lo + match_len as usize;
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
                return Err(::bbnf::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                });
            }
        }
        {
            let mut iter_count: u32 = 0;
            loop {
                let save_p = *p;
                let res = (|| -> ::core::result::Result<(), ::bbnf::runtime::ParseErr> {
                    {
                        let span_lo = *p;
                        let Some(match_len) = __regex_scan_BnfParser(
                            "[ \\t]*",
                            input,
                            *p,
                        ) else {
                            return ::core::result::Result::Err(::bbnf::runtime::ParseErr::Syntax {
                                offset: span_lo as u32,
                                rule: None,
                            });
                        };
                        *p = span_lo + match_len as usize;
                    }
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() < end || input[at..end] != [124u8] {
                        return Err(::bbnf::runtime::ParseErr::Syntax {
                            offset: at as u32,
                            rule: None,
                        });
                    }
                    *p = end;
                    {
                        let span_lo = *p;
                        let Some(match_len) = __regex_scan_BnfParser(
                            "[ \\t]*",
                            input,
                            *p,
                        ) else {
                            return ::core::result::Result::Err(::bbnf::runtime::ParseErr::Syntax {
                                offset: span_lo as u32,
                                rule: None,
                            });
                        };
                        *p = span_lo + match_len as usize;
                    }
                    let mut iter_count: u32 = 0;
                    loop {
                        let save_p = *p;
                        let res = (|| -> ::core::result::Result<
                            (),
                            ::bbnf::runtime::ParseErr,
                        > {
                            {
                                let first = __shape_support_BnfParser::skip_space(
                                        input,
                                        p,
                                        state,
                                    )
                                    .ok_or(::bbnf::runtime::ParseErr::Syntax {
                                        offset: *p as u32,
                                        rule: None,
                                    })?;
                                'try_branches: loop {
                                    match first {
                                        34u8 => {
                                            let attempt_p = *p;
                                            match {
                                                let _ = __shape_support_BnfParser::skip_space(
                                                    input,
                                                    p,
                                                    state,
                                                );
                                                parse_flat_visitor_BnfParser_terminal(
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
                                        60u8 => {
                                            let attempt_p = *p;
                                            match {
                                                let _ = __shape_support_BnfParser::skip_space(
                                                    input,
                                                    p,
                                                    state,
                                                );
                                                parse_flat_visitor_BnfParser_nonterminal(
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
                                    return ::core::result::Result::Err(::bbnf::runtime::ParseErr::Syntax {
                                        offset: *p as u32,
                                        rule: None,
                                    });
                                }
                            }
                            {
                                let span_lo = *p;
                                let Some(match_len) = __regex_scan_BnfParser(
                                    "[ \\t]*",
                                    input,
                                    *p,
                                ) else {
                                    return ::core::result::Result::Err(::bbnf::runtime::ParseErr::Syntax {
                                        offset: span_lo as u32,
                                        rule: None,
                                    });
                                };
                                *p = span_lo + match_len as usize;
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
                        return Err(::bbnf::runtime::ParseErr::Syntax {
                            offset: *p as u32,
                            rule: None,
                        });
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
                return Err(::bbnf::runtime::ParseErr::Syntax {
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
    pub fn parse_flat_visitor_BnfParser_rule<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_BnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
    where
        V: ::bbnf::runtime::tape::ObjectVisitor + ::bbnf::runtime::tape::ArrayVisitor
            + ::bbnf::runtime::tape::StringVisitor + ::bbnf::runtime::tape::NumberVisitor
            + ::bbnf::runtime::tape::KeywordVisitor,
    {
        {
            ({
                let _ = __shape_support_BnfParser::skip_space(input, p, state);
                parse_flat_visitor_BnfParser_nonterminal(input, p, state, visitor)
            })?;
        }
        {
            {
                let span_lo = *p;
                let Some(match_len) = __regex_scan_BnfParser("[ \\t]*", input, *p) else {
                    return ::core::result::Result::Err(::bbnf::runtime::ParseErr::Syntax {
                        offset: span_lo as u32,
                        rule: None,
                    });
                };
                *p = span_lo + match_len as usize;
            }
        }
        {
            let at = *p;
            let end = at + 3usize;
            if input.len() < end || input[at..end] != [58u8, 58u8, 61u8] {
                return Err(::bbnf::runtime::ParseErr::Syntax {
                    offset: at as u32,
                    rule: None,
                });
            }
            *p = end;
        }
        {
            {
                let span_lo = *p;
                let Some(match_len) = __regex_scan_BnfParser("[ \\t]*", input, *p) else {
                    return ::core::result::Result::Err(::bbnf::runtime::ParseErr::Syntax {
                        offset: span_lo as u32,
                        rule: None,
                    });
                };
                *p = span_lo + match_len as usize;
            }
        }
        {
            ({
                let _ = __shape_support_BnfParser::skip_space(input, p, state);
                parse_flat_visitor_BnfParser_alternation(input, p, state, visitor)
            })?;
        }
        {
            {
                let span_lo = *p;
                let Some(match_len) = __regex_scan_BnfParser("[ \\t]*", input, *p) else {
                    return ::core::result::Result::Err(::bbnf::runtime::ParseErr::Syntax {
                        offset: span_lo as u32,
                        rule: None,
                    });
                };
                *p = span_lo + match_len as usize;
            }
        }
        {
            let save_p = *p;
            let res = (|| -> ::core::result::Result<(), ::bbnf::runtime::ParseErr> {
                {
                    let span_lo = *p;
                    let Some(match_len) = __regex_scan_BnfParser("\\n", input, *p) else {
                        return ::core::result::Result::Err(::bbnf::runtime::ParseErr::Syntax {
                            offset: span_lo as u32,
                            rule: None,
                        });
                    };
                    *p = span_lo + match_len as usize;
                }
                Ok(())
            })();
            if res.is_err() {
                *p = save_p;
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
    pub fn parse_array_visitor_BnfParser_grammar<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_BnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
    where
        V: ::bbnf::runtime::tape::ObjectVisitor + ::bbnf::runtime::tape::ArrayVisitor
            + ::bbnf::runtime::tape::StringVisitor + ::bbnf::runtime::tape::NumberVisitor
            + ::bbnf::runtime::tape::KeywordVisitor,
    {
        let begin_at = *p;
        if input.get(*p).copied() != Some(b'[') {
            return Err(::bbnf::runtime::ParseErr::Syntax {
                offset: begin_at as u32,
                rule: None,
            });
        }
        *p += 1;
        visitor
            .begin_array()
            .map_err(|_| ::bbnf::runtime::ParseErr::Syntax {
                offset: begin_at as u32,
                rule: None,
            })?;
        if let Some(b) = __shape_support_BnfParser::skip_space(input, p, state) {
            if b == b']' {
                *p += 1;
                return visitor
                    .end_array()
                    .map_err(|_| ::bbnf::runtime::ParseErr::Syntax {
                        offset: *p as u32,
                        rule: None,
                    });
            }
        } else {
            return Err(::bbnf::runtime::ParseErr::Syntax {
                offset: *p as u32,
                rule: None,
            });
        }
        loop {
            ({
                let _ = __shape_support_BnfParser::skip_space(input, p, state);
                parse_flat_visitor_BnfParser_rule(input, p, state, visitor)
            })?;
            match __shape_support_BnfParser::skip_space(input, p, state) {
                Some(b']') => {
                    *p += 1;
                    return visitor
                        .end_array()
                        .map_err(|_| ::bbnf::runtime::ParseErr::Syntax {
                            offset: *p as u32,
                            rule: None,
                        });
                }
                Some(b',') => {
                    *p += 1;
                    let _ = __shape_support_BnfParser::skip_space(input, p, state);
                }
                _ => {
                    return Err(::bbnf::runtime::ParseErr::Syntax {
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
    /// [`::bbnf::runtime::tape::TapeCursor::object_key_seek`] /
    /// [`::bbnf::runtime::tape::TapeCursor::bounded_lookahead`] /
    /// [`::bbnf::runtime::tape::TapeCursor::scan_structural_bounded`]
    /// per the entry's `activation` bitmap.
    ///
    /// No runtime flag; no hand-routed grammar specialisation.
    /// AY-II.W0'.c retires the `#[allow(dead_code)]` that
    /// previously guarded this surface — the emitted grammar now
    /// carries a same-translation-unit consumer through
    /// `__path_walk`'s dispatch.
    pub const STRUCTURAL_SCAN_POLICY: &[::bbnf::runtime::tape::ScanPolicyEntry] = &[
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 0u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Sparse,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(2),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 1u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Sparse,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 2u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Sparse,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(2),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 3u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Sparse,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(2),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 4u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Sparse,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(2),
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
    pub fn parse_BnfParser_grammar(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_BnfParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        parse_BnfParser_grammar__value(input, p, state, builder)
    }
    /// AW-V.W3.2 — value-position shape dispatcher. Called both at
    /// the grammar root and from Object / Array compound bodies.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_BnfParser_grammar__value(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_BnfParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let _ = __shape_support_BnfParser::skip_space(input, p, state);
        parse_array_BnfParser_grammar(input, p, state, builder)
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
    pub fn parse_BnfParser_grammar_visitor<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_BnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
    where
        V: ::bbnf::runtime::tape::ObjectVisitor + ::bbnf::runtime::tape::ArrayVisitor
            + ::bbnf::runtime::tape::StringVisitor + ::bbnf::runtime::tape::NumberVisitor
            + ::bbnf::runtime::tape::KeywordVisitor,
    {
        parse_BnfParser_grammar_visitor__value(input, p, state, visitor)
    }
    /// AW-V.W3-bench-fix — value-position visitor-path dispatcher.
    /// Called both at the grammar root and from the object / array
    /// shape fns' value-position recursion.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_BnfParser_grammar_visitor__value<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_BnfParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
    where
        V: ::bbnf::runtime::tape::ObjectVisitor + ::bbnf::runtime::tape::ArrayVisitor
            + ::bbnf::runtime::tape::StringVisitor + ::bbnf::runtime::tape::NumberVisitor
            + ::bbnf::runtime::tape::KeywordVisitor,
    {
        let _ = __shape_support_BnfParser::skip_space(input, p, state);
        parse_array_visitor_BnfParser_grammar(input, p, state, visitor)
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct terminalView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> terminalView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
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
        pub fn rule_kind(&self) -> BnfParserRuleKind {
            match self.variant_idx() {
                0u8 => BnfParserRuleKind::terminal,
                1u8 => BnfParserRuleKind::nonterminal,
                2u8 => BnfParserRuleKind::alternation,
                3u8 => BnfParserRuleKind::rule,
                4u8 => BnfParserRuleKind::grammar,
                _ => BnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor.children().map(move |c| BnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<BnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| BnfParserNodeView::from_cursor(c, self.input))
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
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<BnfParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| BnfParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<BnfParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| BnfParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 2 as a typed view.
        #[inline]
        pub fn child_2(&self) -> ::core::option::Option<BnfParserNodeView<'p>> {
            self.cursor
                .child(2usize)
                .map(|c| BnfParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            3usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct nonterminalView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> nonterminalView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
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
        pub fn rule_kind(&self) -> BnfParserRuleKind {
            match self.variant_idx() {
                0u8 => BnfParserRuleKind::terminal,
                1u8 => BnfParserRuleKind::nonterminal,
                2u8 => BnfParserRuleKind::alternation,
                3u8 => BnfParserRuleKind::rule,
                4u8 => BnfParserRuleKind::grammar,
                _ => BnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor.children().map(move |c| BnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<BnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| BnfParserNodeView::from_cursor(c, self.input))
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
    impl<'p> nonterminalView<'p> {
        /// The source text matched by this leaf rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// Get the sub-span value as a string slice.
        ///
        /// Payload-first: reads the packed (lo, hi) u32 pair from
        /// the tape payload buffer in O(1). Falls back to the
        /// record's own span text if no payload is present.
        #[inline]
        pub fn value(&self) -> &'p str {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            if let Some((lo, hi)) = tape.payload_Span(rec) {
                return &self.input[lo as usize..hi as usize];
            }
            self.span_text()
        }
        /// Alias for backward compatibility. Prefer `.value()`.
        #[inline]
        pub fn as_span(&self) -> &'p str {
            self.value()
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
    pub struct alternationView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> alternationView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
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
        pub fn rule_kind(&self) -> BnfParserRuleKind {
            match self.variant_idx() {
                0u8 => BnfParserRuleKind::terminal,
                1u8 => BnfParserRuleKind::nonterminal,
                2u8 => BnfParserRuleKind::alternation,
                3u8 => BnfParserRuleKind::rule,
                4u8 => BnfParserRuleKind::grammar,
                _ => BnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor.children().map(move |c| BnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<BnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| BnfParserNodeView::from_cursor(c, self.input))
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
        /// The key as source text (the Span matched by the first
        /// child of the original Seq).
        #[inline]
        pub fn key(&self) -> &'p str {
            self.span_text()
        }
        /// Alias for `.key()` — the source text of the key Span.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// The key Span as `(lo, hi)` byte offsets.
        #[inline]
        pub fn key_span(&self) -> (u32, u32) {
            self.span()
        }
        /// The value scalar decoded from the aggregate payload.
        ///
        /// Returns the zero-initialized value if no payload was
        /// written for this record.
        #[inline]
        pub fn value(&self) -> (u32, u32) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 8usize) {
                Some(__bytes) => {
                    let __raw = u64::from_le_bytes(
                        <[u8; 8]>::try_from(&__bytes[0usize..8usize])
                            .expect("kv_pair slice is 8 bytes"),
                    );
                    (__raw as u32, (__raw >> 32) as u32)
                }
                None => (0_u32, 0_u32),
            }
        }
        /// The matched byte range as a `Range<usize>`.
        #[inline]
        pub fn byte_range(&self) -> ::core::ops::Range<usize> {
            let (lo, hi) = self.span();
            lo as usize..hi as usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct ruleView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> ruleView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
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
        pub fn rule_kind(&self) -> BnfParserRuleKind {
            match self.variant_idx() {
                0u8 => BnfParserRuleKind::terminal,
                1u8 => BnfParserRuleKind::nonterminal,
                2u8 => BnfParserRuleKind::alternation,
                3u8 => BnfParserRuleKind::rule,
                4u8 => BnfParserRuleKind::grammar,
                _ => BnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor.children().map(move |c| BnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<BnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| BnfParserNodeView::from_cursor(c, self.input))
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
        pub fn child_0(&self) -> ::core::option::Option<nonterminalView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| nonterminalView::from_cursor(c, self.input))
        }
        ///The `nonterminal` child as a typed view.
        #[inline]
        pub fn nonterminal(&self) -> ::core::option::Option<nonterminalView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| nonterminalView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<BnfParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| BnfParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 2 as a typed view.
        #[inline]
        pub fn child_2(&self) -> ::core::option::Option<BnfParserNodeView<'p>> {
            self.cursor
                .child(2usize)
                .map(|c| BnfParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 3 as a typed view.
        #[inline]
        pub fn child_3(&self) -> ::core::option::Option<BnfParserNodeView<'p>> {
            self.cursor
                .child(3usize)
                .map(|c| BnfParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 4 as a typed view.
        #[inline]
        pub fn child_4(&self) -> ::core::option::Option<alternationView<'p>> {
            self.cursor
                .child(4usize)
                .map(|c| alternationView::from_cursor(c, self.input))
        }
        ///The `alternation` child as a typed view.
        #[inline]
        pub fn alternation(&self) -> ::core::option::Option<alternationView<'p>> {
            self.cursor
                .child(4usize)
                .map(|c| alternationView::from_cursor(c, self.input))
        }
        ///Child at position 5 as a typed view.
        #[inline]
        pub fn child_5(&self) -> ::core::option::Option<BnfParserNodeView<'p>> {
            self.cursor
                .child(5usize)
                .map(|c| BnfParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 6 as a typed view.
        #[inline]
        pub fn child_6(&self) -> ::core::option::Option<BnfParserNodeView<'p>> {
            self.cursor
                .child(6usize)
                .map(|c| BnfParserNodeView::from_cursor(c, self.input))
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
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> grammarView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
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
        pub fn rule_kind(&self) -> BnfParserRuleKind {
            match self.variant_idx() {
                0u8 => BnfParserRuleKind::terminal,
                1u8 => BnfParserRuleKind::nonterminal,
                2u8 => BnfParserRuleKind::alternation,
                3u8 => BnfParserRuleKind::rule,
                4u8 => BnfParserRuleKind::grammar,
                _ => BnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor.children().map(move |c| BnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<BnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| BnfParserNodeView::from_cursor(c, self.input))
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
        pub fn iter(&self) -> impl ::core::iter::Iterator<Item = ruleView<'p>> + 'p {
            let input = self.input;
            self.cursor.children().map(move |c| ruleView::from_cursor(c, input))
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
        pub fn get(&self, i: usize) -> ::core::option::Option<ruleView<'p>> {
            self.cursor.child(i).map(|c| ruleView::from_cursor(c, self.input))
        }
    }
    /// Generic node view over any tape record for this grammar.
    #[derive(Clone, Copy, Debug)]
    pub struct BnfParserNodeView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
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
    pub enum BnfParserRuleKind {
        terminal,
        nonterminal,
        alternation,
        rule,
        grammar,
        /// Fallback for records whose variant_idx is not a
        /// known rule- or sub-variant discriminator.
        Unknown,
    }
    impl<'p> BnfParserNodeView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
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
        pub fn rule_kind(&self) -> BnfParserRuleKind {
            match self.variant_idx() {
                0u8 => BnfParserRuleKind::terminal,
                1u8 => BnfParserRuleKind::nonterminal,
                2u8 => BnfParserRuleKind::alternation,
                3u8 => BnfParserRuleKind::rule,
                4u8 => BnfParserRuleKind::grammar,
                _ => BnfParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BnfParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor.children().map(move |c| BnfParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<BnfParserNodeView<'p>> {
            self.cursor.child(i).map(|c| BnfParserNodeView::from_cursor(c, self.input))
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
    impl ::bbnf::runtime::Root for BnfParser {
        type View<'p> = grammarView<'p>;
        #[inline]
        fn make_view<'p>(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            root: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self::View<'p> {
            grammarView::new(tape, input, root)
        }
    }
    impl BnfParser {
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
    pub struct BnfParserAlternationProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: (u32, u32),
    }
    impl BnfParserAlternationProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "alternation";
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
    pub const PROJECTION_DIRECT_TO_STRUCT: &[(&str, &str); 1usize] = &[
        ("alternation", "BnfParserAlternationProjection"),
    ];
    /// AY-II.W0.d — grammar-declared `-> Name` bindings, indexed in
    /// lockstep with `PROJECTION_DIRECT_TO_STRUCT`. Empty string for
    /// admissions that did not spell a named type.
    #[doc(hidden)]
    pub const PROJECTION_NAMED_BINDINGS: &[&str; 1usize] = &[""];
    /// AY-II.W0.d — canonical evidence that every admission has a
    /// matching `materialize_projection_<rule>_<Grammar>` fn.
    /// Indexed in lockstep with `PROJECTION_DIRECT_TO_STRUCT`; the
    /// wire-contract totality test asserts both slices share the
    /// same length per grammar.
    #[doc(hidden)]
    pub const PROJECTION_MATERIALIZERS: &[&str; 1usize] = &[
        "materialize_projection_alternation_BnfParser",
    ];
    /// AY-II.W0.d — canonical evidence that every admission has a
    /// matching `<Grammar>Value::<RuleName>` enum variant
    /// (production consumer). Indexed in lockstep with
    /// `PROJECTION_DIRECT_TO_STRUCT`.
    #[doc(hidden)]
    pub const PROJECTION_CONSUMERS: &[&str; 1usize] = &["BnfParserValue::alternation"];
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_alternation() -> (&'static str, usize, &'static str) {
        ("alternation", 1, "")
    }
    /// AY-II.W0'.b — grammar-emitted value enum. Eager
    /// materialisation target for `Parsed::to_value()`. Variants
    /// enumerate non-transparent rules; admitted rules carry the
    /// matching `<Grammar><RuleCamel>Projection` struct directly,
    /// non-admitted rules carry their shape-classified payload.
    #[derive(Clone, Debug)]
    pub enum BnfParserValue<'p> {
        terminal(&'p str),
        nonterminal(&'p str),
        alternation(BnfParserAlternationProjection),
        rule(::std::vec::Vec<BnfParserValue<'p>>),
        grammar(::std::vec::Vec<BnfParserValue<'p>>),
        /// Fallback for records whose `variant_idx` is not a
        /// known rule discriminator (recovered records, stray
        /// sub-variant indices).
        Unknown(BnfParserNodeView<'p>),
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
    fn project_rule_kind_BnfParser(
        kind: ::bbnf::runtime::tape::TapeKind,
        variant_idx: u8,
    ) -> BnfParserRuleKind {
        if variant_idx == 0 && kind.is_compound() {
            return BnfParserRuleKind::Unknown;
        }
        match variant_idx {
            0u8 => BnfParserRuleKind::terminal,
            1u8 => BnfParserRuleKind::nonterminal,
            2u8 => BnfParserRuleKind::alternation,
            3u8 => BnfParserRuleKind::rule,
            4u8 => BnfParserRuleKind::grammar,
            _ => BnfParserRuleKind::Unknown,
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
    fn project_push_children_BnfParser<'p>(
        output: &::bbnf::runtime::FusedOutput<BnfParser>,
        input: &'p str,
        offset: u32,
        out: &mut ::std::vec::Vec<BnfParserValue<'p>>,
    ) {
        let __tape = output.tape();
        let __rec = match __tape.try_get(::bbnf::runtime::tape::TapeOffset(offset)) {
            ::core::option::Option::Some(r) => r,
            ::core::option::Option::None => return,
        };
        if __rec.variant_idx() == 0 && __rec.kind().is_compound() {
            let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                __tape,
                ::bbnf::runtime::tape::TapeOffset(offset),
            );
            for __child in __cur.children() {
                project_push_children_BnfParser(output, input, __child.offset().0, out);
            }
        } else {
            out.push(project_frame_BnfParser(output, input, offset));
        }
    }
    /// AY-II.W0'.b — per-frame projector. Reads one record from the
    /// fused-pipeline [`FusedOutput`](::bbnf::runtime::FusedOutput)
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
    fn project_frame_BnfParser<'p>(
        output: &::bbnf::runtime::FusedOutput<BnfParser>,
        input: &'p str,
        offset: u32,
    ) -> BnfParserValue<'p> {
        let __tape = output.tape();
        let __rec = match __tape.try_get(::bbnf::runtime::tape::TapeOffset(offset)) {
            ::core::option::Option::Some(r) => r,
            ::core::option::Option::None => {
                ::core::panic!(
                    "AY-II.W0'.b: tape offset {} out of range (tape len: {})", offset,
                    __tape.len(),
                );
            }
        };
        match project_rule_kind_BnfParser(__rec.kind(), __rec.variant_idx()) {
            BnfParserRuleKind::terminal => {
                let span = &input[__rec.span_lo as usize..__rec.span_hi as usize];
                BnfParserValue::terminal(span)
            }
            BnfParserRuleKind::nonterminal => {
                let span = &input[__rec.span_lo as usize..__rec.span_hi as usize];
                BnfParserValue::nonterminal(span)
            }
            BnfParserRuleKind::alternation => {
                let proj = materialize_projection_alternation_BnfParser(
                        output,
                        input,
                        offset,
                    )
                    .unwrap_or_else(|| {
                        ::core::panic!(
                            "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                            "alternation", offset,
                        );
                    });
                BnfParserValue::alternation(proj)
            }
            BnfParserRuleKind::rule => {
                let mut children: ::std::vec::Vec<BnfParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_BnfParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                BnfParserValue::rule(children)
            }
            BnfParserRuleKind::grammar => {
                let mut children: ::std::vec::Vec<BnfParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_BnfParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                BnfParserValue::grammar(children)
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
    fn project_value_BnfParser<'p>(
        output: &::bbnf::runtime::FusedOutput<BnfParser>,
        input: &'p str,
    ) -> BnfParserValue<'p> {
        let root_off = output.value_root_offset();
        project_frame_BnfParser(output, input, root_off)
    }
    impl ::bbnf::runtime::ValueRoot for BnfParser {
        type Value<'p> = BnfParserValue<'p>;
        #[inline]
        fn project_value_output<'p>(
            output: &::bbnf::runtime::FusedOutput<BnfParser>,
            input: &'p str,
        ) -> Self::Value<'p>
        where
            Self: 'p,
        {
            project_value_BnfParser(output, input)
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
    /// [`TapeCursor::bounded_lookahead`]: ::bbnf::runtime::tape::TapeCursor::bounded_lookahead
    /// [`TapeCursor::object_key_seek`]: ::bbnf::runtime::tape::TapeCursor::object_key_seek
    /// [`TapeCursor::scan_structural_bounded`]: ::bbnf::runtime::tape::TapeCursor::scan_structural_bounded
    #[inline]
    fn __path_walk<'p>(
        view: BnfParserNodeView<'p>,
        path: ::bbnf::runtime::Path<'_>,
    ) -> ::core::option::Option<BnfParserNodeView<'p>> {
        let cur_input = view.input();
        let mut cur = view;
        for seg in path.iter() {
            match seg {
                ::bbnf::runtime::PathSegment::Field(key) => {
                    match cur.rule_kind() {
                        BnfParserRuleKind::terminal
                        | BnfParserRuleKind::alternation
                        | BnfParserRuleKind::rule
                        | BnfParserRuleKind::grammar => {
                            let parent = cur.cursor();
                            let (_, parent_end) = parent.span();
                            let mut iter = parent.bounded_lookahead(parent_end);
                            let mut hit: ::core::option::Option<BnfParserNodeView<'p>> = None;
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
                                        BnfParserNodeView::from_cursor(v_cur, cur_input),
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
                ::bbnf::runtime::PathSegment::Index(i) => {
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
    impl ::bbnf::runtime::PathQuery<&'static str> for BnfParser {
        #[inline]
        fn query<'p>(
            view: Self::View<'p>,
            path: ::bbnf::runtime::Path<'_>,
        ) -> ::core::option::Option<&'static str>
        where
            Self: 'p,
        {
            let node = BnfParserNodeView::from_cursor(view.cursor(), view.input());
            __path_walk(node, path)?;
            ::core::option::Option::None
        }
    }
    impl ::bbnf::runtime::PathQuery<f64> for BnfParser {
        #[inline]
        fn query<'p>(
            view: Self::View<'p>,
            path: ::bbnf::runtime::Path<'_>,
        ) -> ::core::option::Option<f64>
        where
            Self: 'p,
        {
            let node = BnfParserNodeView::from_cursor(view.cursor(), view.input());
            let hit = __path_walk(node, path)?;
            let tape = hit.cursor().tape();
            let rec = hit.cursor().record();
            if let ::core::option::Option::Some(v) = tape.payload_f64(rec) {
                return ::core::option::Option::Some(v);
            }
            hit.span_text().parse::<f64>().ok()
        }
    }
    impl ::bbnf::runtime::PathQuery<bool> for BnfParser {
        #[inline]
        fn query<'p>(
            view: Self::View<'p>,
            path: ::bbnf::runtime::Path<'_>,
        ) -> ::core::option::Option<bool>
        where
            Self: 'p,
        {
            let node = BnfParserNodeView::from_cursor(view.cursor(), view.input());
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
    /// fused-pipeline [`FusedOutput`](::bbnf::runtime::FusedOutput)
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
    pub fn materialize_projection_alternation_BnfParser<'p>(
        output: &::bbnf::runtime::FusedOutput<BnfParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<BnfParserAlternationProjection> {
        let _ = input;
        let frame = output.value_frame_at(offset)?;
        let __bytes: &[u8] = &[];
        let _ = __bytes;
        let field_0: (u32, u32) = (frame.span_lo, frame.span_hi);
        ::core::option::Option::Some(BnfParserAlternationProjection {
            field_0,
        })
    }
    impl BnfParser {
        fn __terminal_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'"') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'"');
                    };
                    {
                        let __start = state.offset;
                        if {
                            let __start = state.offset;
                            let __result: Option<()> = (|| {
                                {
                                    let mut __rep_count: u32 = 0;
                                    loop {
                                        let __save = state.offset;
                                        let __ok = (|| -> Option<()> {
                                            {
                                                let __save_alt = state.offset;
                                                let __alt_ok = (|| -> Option<()> {
                                                    if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                                    {
                                                        return None;
                                                    }
                                                    state.offset += 1;
                                                    {
                                                        let __b = *state.src_bytes.get(state.offset)?;
                                                        if !(!(__b == b'\n')) {
                                                            return None;
                                                        }
                                                        state.offset += 1;
                                                    }
                                                    Some(())
                                                })();
                                                let __alt_ok = if __alt_ok.is_none() {
                                                    state.offset = __save_alt;
                                                    (|| -> Option<()> {
                                                        {
                                                            let __b = *state.src_bytes.get(state.offset)?;
                                                            if !(!((__b == b'"' || __b == b'\\'))) {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                        }
                                                        Some(())
                                                    })()
                                                } else {
                                                    __alt_ok
                                                };
                                                if __alt_ok.is_none() {
                                                    return None;
                                                }
                                            }
                                            Some(())
                                        })();
                                        if __ok.is_none() {
                                            state.offset = __save;
                                            break;
                                        }
                                        if state.offset == __save {
                                            break;
                                        }
                                        __rep_count += 1;
                                    }
                                }
                                Some(())
                            })();
                            if __result.is_some() {
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                state.offset = __start;
                                None
                            }
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
                        if state.src_bytes.get(state.offset).copied() != Some(b'"') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'"');
                    };
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
        fn __nonterminal_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'<') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'<');
                        };
                        {
                            let __start = state.offset;
                            if ::parse_that::scan_ident(
                                    state,
                                    &::parse_that::DEFAULT_IDENT_CONFIG,
                                )
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
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'>') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'>');
                    };
                };
                true
            }
        }
        pub fn nonterminal_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__nonterminal_prettify(state, &mut __builder) {
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
                        {
                            let __rep_start4 = state.offset;
                            let __rep_bcp5 = __builder.checkpoint();
                            let mut __rep_count2 = 0usize;
                            while __rep_count2 < 4294967295 {
                                let __rep_cp3 = state.offset;
                                if !{
                                    let __pretty_cp0 = state.offset;
                                    let __pretty_bcp1 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            {
                                                let __byte = match state.src_bytes.get(state.offset) {
                                                    Some(&b) => b,
                                                    None => return false,
                                                };
                                                match __byte {
                                                    b'"' => {
                                                        if !Self::__terminal_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                    }
                                                    b'<' => {
                                                        {
                                                            {
                                                                {
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'<')
                                                                    {
                                                                        return false;
                                                                    }
                                                                    state.offset += 1;
                                                                    __builder.char(b'<');
                                                                };
                                                                {
                                                                    let __start = state.offset;
                                                                    if ::parse_that::scan_ident(
                                                                            state,
                                                                            &::parse_that::DEFAULT_IDENT_CONFIG,
                                                                        )
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
                                                            {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'>')
                                                                {
                                                                    return false;
                                                                }
                                                                state.offset += 1;
                                                                __builder.char(b'>');
                                                            };
                                                        };
                                                    }
                                                    _ => {
                                                        return false;
                                                    }
                                                }
                                            };
                                            {
                                                let __start = state.offset;
                                                if {
                                                    let __start = state.offset;
                                                    let __end = state.src_bytes.len();
                                                    let mut __pos = __start;
                                                    while __pos < __end {
                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                        if __b == b'\t' || __b == b' ' {
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
                            if __rep_count2 < 1 {
                                state.offset = __rep_start4;
                                __builder.restore(__rep_bcp5);
                                return false;
                            }
                        };
                        {
                            let mut __rep_count14 = 0usize;
                            while __rep_count14 < 4294967295 {
                                let __rep_cp15 = state.offset;
                                if !{
                                    let __pretty_cp12 = state.offset;
                                    let __pretty_bcp13 = __builder.checkpoint();
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
                                                        if __b == b'\t' || __b == b' ' {
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
                                                if state.src_bytes.get(state.offset).copied() != Some(b'|')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b'|');
                                            };
                                            {
                                                let __start = state.offset;
                                                if {
                                                    let __start = state.offset;
                                                    let __end = state.src_bytes.len();
                                                    let mut __pos = __start;
                                                    while __pos < __end {
                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                        if __b == b'\t' || __b == b' ' {
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
                                                let __rep_start10 = state.offset;
                                                let __rep_bcp11 = __builder.checkpoint();
                                                let mut __rep_count8 = 0usize;
                                                while __rep_count8 < 4294967295 {
                                                    let __rep_cp9 = state.offset;
                                                    if !{
                                                        let __pretty_cp6 = state.offset;
                                                        let __pretty_bcp7 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            {
                                                                {
                                                                    let __byte = match state.src_bytes.get(state.offset) {
                                                                        Some(&b) => b,
                                                                        None => return false,
                                                                    };
                                                                    match __byte {
                                                                        b'"' => {
                                                                            if !Self::__terminal_prettify(state, __builder) {
                                                                                return false;
                                                                            }
                                                                        }
                                                                        b'<' => {
                                                                            {
                                                                                {
                                                                                    {
                                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'<')
                                                                                        {
                                                                                            return false;
                                                                                        }
                                                                                        state.offset += 1;
                                                                                        __builder.char(b'<');
                                                                                    };
                                                                                    {
                                                                                        let __start = state.offset;
                                                                                        if ::parse_that::scan_ident(
                                                                                                state,
                                                                                                &::parse_that::DEFAULT_IDENT_CONFIG,
                                                                                            )
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
                                                                                {
                                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'>')
                                                                                    {
                                                                                        return false;
                                                                                    }
                                                                                    state.offset += 1;
                                                                                    __builder.char(b'>');
                                                                                };
                                                                            };
                                                                        }
                                                                        _ => {
                                                                            return false;
                                                                        }
                                                                    }
                                                                };
                                                                {
                                                                    let __start = state.offset;
                                                                    if {
                                                                        let __start = state.offset;
                                                                        let __end = state.src_bytes.len();
                                                                        let mut __pos = __start;
                                                                        while __pos < __end {
                                                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                            if __b == b'\t' || __b == b' ' {
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
                                                            state.offset = __pretty_cp6;
                                                            __builder.restore(__pretty_bcp7);
                                                        }
                                                        __ok
                                                    } {
                                                        state.offset = __rep_cp9;
                                                        break;
                                                    }
                                                    if state.offset == __rep_cp9 {
                                                        break;
                                                    }
                                                    __rep_count8 += 1;
                                                }
                                                if __rep_count8 < 1 {
                                                    state.offset = __rep_start10;
                                                    __builder.restore(__rep_bcp11);
                                                    return false;
                                                }
                                            };
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp12;
                                        __builder.restore(__pretty_bcp13);
                                    }
                                    __ok
                                } {
                                    state.offset = __rep_cp15;
                                    break;
                                }
                                if state.offset == __rep_cp15 {
                                    break;
                                }
                                __rep_count14 += 1;
                            }
                        };
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
        fn __rule_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            let __pretty_ok = {
                {
                    {
                        {
                            {
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'<')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b'<');
                                };
                                {
                                    let __start = state.offset;
                                    if ::parse_that::scan_ident(
                                            state,
                                            &::parse_that::DEFAULT_IDENT_CONFIG,
                                        )
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
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'>')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'>');
                            };
                        };
                        {
                            let __start = state.offset;
                            if {
                                let __start = state.offset;
                                let __end = state.src_bytes.len();
                                let mut __pos = __start;
                                while __pos < __end {
                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                    if __b == b'\t' || __b == b' ' {
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
                            let __s = "::=";
                            let __bytes = __s.as_bytes();
                            let __slc = match state.src_bytes.get(state.offset..) {
                                Some(s) if s.len() >= 3usize => s,
                                _ => return false,
                            };
                            if &__slc[..3usize] != __bytes {
                                return false;
                            }
                            __builder
                                .text(&state.src[state.offset..state.offset + 3usize]);
                            state.offset += 3usize;
                        };
                        {
                            let __start = state.offset;
                            if {
                                let __start = state.offset;
                                let __end = state.src_bytes.len();
                                let mut __pos = __start;
                                while __pos < __end {
                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                    if __b == b'\t' || __b == b' ' {
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
                        if !Self::__alternation_prettify(state, __builder) {
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
                                    if __b == b'\t' || __b == b' ' {
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
                                let __pretty_cp16 = state.offset;
                                let __pretty_bcp17 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        let __start = state.offset;
                                        if {
                                            let __start = state.offset;
                                            let __result: Option<()> = (|| {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'\n')
                                                {
                                                    return None;
                                                }
                                                state.offset += 1;
                                                Some(())
                                            })();
                                            if __result.is_some() && state.offset > __start {
                                                Some(
                                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                                )
                                            } else {
                                                state.offset = __start;
                                                None
                                            }
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
                    let mut __rep_count23 = 0usize;
                    while __rep_count23 < 4294967295 {
                        let __rep_cp24 = state.offset;
                        let __iter_cp = if __rep_count23 > 0 {
                            Some(__builder.checkpoint())
                        } else {
                            None
                        };
                        if __rep_count23 > 0 {
                            __builder.hardline();
                        }
                        if !{
                            let __pretty_cp22 = state.offset;
                            let __ok = (|| -> bool {
                                {
                                    if !{
                                        let __pretty_cp20 = state.offset;
                                        let __pretty_bcp21 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                let __ows18 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder.text_inline_ws(&state.src[__ows18..state.offset]);
                                                if !Self::__rule_prettify(state, __builder) {
                                                    return false;
                                                }
                                                let __ows19 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder.text_inline_ws(&state.src[__ows19..state.offset]);
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp20;
                                            __builder.restore(__pretty_bcp21);
                                        }
                                        __ok
                                    } {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp22;
                            }
                            __ok
                        } {
                            state.offset = __rep_cp24;
                            if let Some(__bcp) = __iter_cp {
                                __builder.restore(__bcp);
                            }
                            break;
                        }
                        if state.offset == __rep_cp24 {
                            if let Some(__bcp) = __iter_cp {
                                __builder.restore(__bcp);
                            }
                            break;
                        }
                        __rep_count23 += 1;
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
        pub fn serialize_terminal<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: BnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_nonterminal<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: BnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_alternation<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: BnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_rule<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: BnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_grammar<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: BnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        fn __dispatch_serialize<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: BnfParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            match __v.variant_idx() {
                0u8 => {
                    Self::serialize_terminal(__v, __ser);
                }
                1u8 => {
                    Self::serialize_nonterminal(__v, __ser);
                }
                2u8 => {
                    Self::serialize_alternation(__v, __ser);
                }
                3u8 => {
                    Self::serialize_rule(__v, __ser);
                }
                4u8 => {
                    Self::serialize_grammar(__v, __ser);
                }
                _ => {
                    __ser.text(__v.span_text());
                }
            }
        }
        pub fn serialize_compact<'a>(__v: BnfParserNodeView<'a>) -> String {
            let mut __ser = ::bbnf_ser::StringSerializer::new();
            Self::serialize_grammar(__v, &mut __ser);
            __ser.finish()
        }
        pub fn serialize<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: BnfParserNodeView<'a>,
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
        pub const GRAMMAR_PROFILE: ::bbnf::runtime::tape::GrammarProfile = GRAMMAR_PROFILE;
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
            ::bbnf::runtime::Parsed<'_, Self>,
            ::bbnf::runtime::ParseErr,
        > {
            let __input_bytes = input.as_bytes();
            let mut state = __shape_support_BnfParser::ScanState::new();
            let mut builder = ::bbnf::runtime::tape::FusedBuilder::with_capacity(
                GRAMMAR_PROFILE.capacity_for(input.len()),
            );
            let root_off = {
                let mut pos: usize = 0;
                let off = parse_BnfParser_grammar(
                        __input_bytes,
                        &mut pos,
                        &mut state,
                        &mut builder,
                    )
                    .map_err(|e| match e {
                        ::bbnf::runtime::tape::DtaError::Syntax { offset, .. } => {
                            ::bbnf::runtime::ParseErr::Syntax {
                                offset,
                                rule: None,
                            }
                        }
                        ::bbnf::runtime::tape::DtaError::UnexpectedEnd { offset } => {
                            ::bbnf::runtime::ParseErr::Syntax {
                                offset,
                                rule: None,
                            }
                        }
                        ::bbnf::runtime::tape::DtaError::InvalidState { .. } => {
                            ::bbnf::runtime::ParseErr::Syntax {
                                offset: 0,
                                rule: None,
                            }
                        }
                    })?;
                let _ = __shape_support_BnfParser::skip_space(
                    __input_bytes,
                    &mut pos,
                    &mut state,
                );
                if pos != input.len() {
                    return Err(::bbnf::runtime::ParseErr::Syntax {
                        offset: pos as u32,
                        rule: None,
                    });
                }
                off
            };
            let output = builder
                .finish_fused::<Self>(root_off.0)
                .map_err(::bbnf::runtime::ParseErr::Tape)?;
            ::core::result::Result::Ok(
                ::bbnf::runtime::Parsed::new_fused_output(output, input, root_off),
            )
        }
    }
    #[inline]
    pub(crate) fn cst_identifier_text<'p>(
        _cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        _input: &'p str,
    ) -> &'p str {
        ""
    }
    #[inline]
    pub(crate) fn cst_identifier_span<'p>(
        _cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        _input: &'p str,
    ) -> (u32, u32) {
        (0, 0)
    }
}
pub use __bnfparser_emit_impl::*;
