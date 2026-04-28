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

use crate::runtime::tape::*;
use crate::runtime::{Parsed, ParseErr, Root};
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
    static __GRAMMAR_PROFILE_ALPHABET: [u8; 6usize] = [44, 58, 91, 93, 123, 125];
    static __GRAMMAR_PROFILE_QUOTE_CLASSES: [u8; 1usize] = [34];
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
        structural_quote_classes: &__GRAMMAR_PROFILE_QUOTE_CLASSES,
    };
    /// AW-III.W6.2 — PHF keyword table.
    ///
    /// Mined literal-led Alt branches, sorted lexicographically.
    /// Binary search dispatches in O(log N) compares; LLVM lowers
    /// the fixed-size table to a balanced compare tree.
    static __PHF_JsonParser_7_KW: [&[u8]; 3usize] = [b"[", b"null", b"{"];
    /// Per-entry branch discriminant — parallel to [`#kw_ident`].
    /// Entry `i`'s keyword bytes at `#kw_ident[i]` route to the
    /// branch with discriminant `#idx_ident[i]`.
    static __PHF_JsonParser_7_IDX: [u8; 3usize] = [1, 3, 0];
    /// AW-III.W6.2 — dispatch the mined keyword table for rule
    /// `#rule_id`.
    ///
    /// Returns `Some(branch_idx)` when `bytes` matches a mined
    /// keyword, `None` otherwise. Called from the walker's
    /// AltLinear / ClassifyByte arm to short-circuit the branch
    /// scan to a single binary search.
    #[allow(dead_code)]
    #[inline]
    fn __phf_JsonParser_dispatch_7(bytes: &[u8]) -> ::core::option::Option<u8> {
        match __PHF_JsonParser_7_KW.binary_search(&bytes) {
            ::core::result::Result::Ok(idx) => {
                ::core::option::Option::Some(__PHF_JsonParser_7_IDX[idx])
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
    /// AY.W4.1 — string escape-path decoder. Cold path; routes
    /// through `parse_that::parsers::scan::decode_json_string_to_arena`,
    /// a fully-SIMD scan + escape-decode kernel. The kernel
    /// internally does its own SIMD `u8x16` re-scan from `open`
    /// (a few dozen bytes of redundant work vs. the fast-path
    /// SIMD scan that already located the first `\\`), then
    /// performs SIMD-stride copies of escape-free runs between
    /// each escape sequence.
    ///
    /// The arena layout is `(len: u32 LE, bytes: [u8; len])` per
    /// `Tape::payload_string_bytes`'s contract. We reserve 4
    /// bytes for the length prefix before invoking the kernel,
    /// then back-stamp the decoded length once the kernel
    /// returns.
    ///
    /// In the rare case where the SIMD fast-path scan flagged a
    /// backslash that was actually escaped (impossible given
    /// odd-parity tracking but defensive), the kernel may return
    /// `StringPayload::Borrowed` — we rewind the reserved prefix
    /// bytes and push a borrow-safe leaf instead.
    #[cold]
    #[inline(never)]
    #[allow(non_snake_case)]
    fn parse_string_escaped(
        input: &[u8],
        p: &mut usize,
        open: usize,
        builder: &mut crate::runtime::tape::Tape<()>,
        variant_idx: u8,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let arena = builder.arena_mut();
        let frame_offset = arena.len() as u32;
        arena.extend_from_slice(&[0u8; 4]);
        let body_start_in_arena = arena.len();
        match ::parse_that::parsers::scan::decode_json_string_to_arena(
            input,
            open,
            arena,
        ) {
            Some(
                (::parse_that::parsers::scan::StringPayload::Owned { .. }, end_pos),
            ) => {
                *p = end_pos;
                let arena_final = builder.arena_mut();
                let decoded_len = (arena_final.len() - body_start_in_arena) as u32;
                let prefix = frame_offset as usize;
                arena_final[prefix..prefix + 4]
                    .copy_from_slice(&decoded_len.to_le_bytes());
                let lo = open as u32;
                let hi = *p as u32;
                let leaf = builder
                    .push_leaf_with_arena_frame(
                        crate::runtime::tape::TapeKind::Span,
                        lo,
                        hi,
                        variant_idx,
                        0,
                        frame_offset,
                    );
                Ok(leaf)
            }
            Some(
                (::parse_that::parsers::scan::StringPayload::Borrowed { .. }, end_pos),
            ) => {
                let arena_final = builder.arena_mut();
                arena_final.truncate(frame_offset as usize);
                *p = end_pos;
                let leaf = builder
                    .push_leaf_borrowed_string(
                        crate::runtime::tape::TapeKind::Span,
                        open as u32,
                        *p as u32,
                        variant_idx,
                        0,
                    );
                Ok(leaf)
            }
            None => {
                let arena_final = builder.arena_mut();
                arena_final.truncate(frame_offset as usize);
                Err(crate::runtime::tape::DtaError::Syntax {
                    offset: open as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                })
            }
        }
    }
    /// AY.W4.1 — visitor-path escape decoder. Cold path.
    ///
    /// Reuses the SIMD-fused `decode_json_string_to_arena` kernel
    /// against a stack-local `Vec<u8>` buffer; the borrow / owned
    /// dispatch is unified at the visitor call site.
    #[cold]
    #[inline(never)]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    fn parse_string_visitor_escaped_JsonParser<V>(
        input: &[u8],
        p: &mut usize,
        body_start: usize,
        _esc_start: usize,
        visitor: &mut V,
        is_key: bool,
        open: usize,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::StringVisitor + crate::runtime::tape::ObjectVisitor,
    {
        let mut buf: Vec<u8> = Vec::with_capacity(
            input.len().saturating_sub(body_start),
        );
        match ::parse_that::parsers::scan::decode_json_string_to_arena(
            input,
            open,
            &mut buf,
        ) {
            Some(
                (::parse_that::parsers::scan::StringPayload::Owned { .. }, end_pos),
            ) => {
                *p = end_pos;
                if is_key {
                    visitor
                        .key(&buf)
                        .map_err(|_| {
                            crate::runtime::ParseErr::Syntax {
                                offset: open as u32,
                                rule: None,
                            }
                        })
                } else {
                    visitor
                        .string(&buf)
                        .map_err(|_| {
                            crate::runtime::ParseErr::Syntax {
                                offset: open as u32,
                                rule: None,
                            }
                        })
                }
            }
            Some(
                (
                    ::parse_that::parsers::scan::StringPayload::Borrowed { start, end },
                    end_pos,
                ),
            ) => {
                *p = end_pos;
                let body = &input[start as usize..end as usize];
                if is_key {
                    visitor
                        .key(body)
                        .map_err(|_| {
                            crate::runtime::ParseErr::Syntax {
                                offset: open as u32,
                                rule: None,
                            }
                        })
                } else {
                    visitor
                        .string(body)
                        .map_err(|_| {
                            crate::runtime::ParseErr::Syntax {
                                offset: open as u32,
                                rule: None,
                            }
                        })
                }
            }
            None => {
                Err(crate::runtime::ParseErr::Syntax {
                    offset: open as u32,
                    rule: None,
                })
            }
        }
    }
    #[inline(never)]
    #[cold]
    #[allow(non_snake_case)]
    fn parse_number_fallback(bytes: &[u8]) -> f64 {
        let s = unsafe { ::core::str::from_utf8_unchecked(bytes) };
        s.parse::<f64>().unwrap_or(f64::NAN)
    }
    /// AW-V.W3-bench-fix — NEON 16-digit parallel accumulator.
    ///
    /// Ported from `sonic-number-0.1.2/src/arch/aarch64.rs:83–137`
    /// (MIT) — mirrors the prototype's `simd_str2int`. Returns
    /// `(sum, count)` where `count` is the number of leading
    /// decimal digits consumed in the 16-byte window.
    ///
    /// SAFETY: caller must guarantee `c.len() >= 16`.
    #[cfg(target_arch = "aarch64")]
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    unsafe fn __number_simd_str2int(c: &[u8]) -> (u64, usize) {
        use core::arch::aarch64::*;
        debug_assert!(c.len() >= 16);
        unsafe {
            let data = vld1q_u8(c.as_ptr());
            let zero_char = vdupq_n_u8(b'0');
            let digits = vsubq_u8(data, zero_char);
            let gt_nine = vcgtq_u8(digits, vdupq_n_u8(9));
            let mask16 = vreinterpretq_u16_u8(gt_nine);
            let mask8 = vshrn_n_u16::<4>(mask16);
            let mask64 = vget_lane_u64::<0>(vreinterpret_u64_u8(mask8));
            let need = 16usize;
            let mut count = need;
            if mask64 != 0 {
                let parsed = (mask64.trailing_zeros() >> 2) as usize;
                if parsed < need {
                    count = parsed;
                }
            }
            #[inline(always)]
            unsafe fn packadd_1_local(v: uint8x16_t) -> uint16x8_t {
                use core::arch::aarch64::*;
                unsafe {
                    let even = vuzp1q_u8(v, v);
                    let odd = vuzp2q_u8(v, v);
                    vaddw_u8(
                        vmull_u8(vget_low_u8(even), vdup_n_u8(10)),
                        vget_low_u8(odd),
                    )
                }
            }
            #[inline(always)]
            unsafe fn packadd_2_local(v: uint16x8_t) -> uint32x4_t {
                use core::arch::aarch64::*;
                unsafe {
                    let even = vuzp1q_u16(v, v);
                    let odd = vuzp2q_u16(v, v);
                    vaddw_u16(vmull_n_u16(vget_low_u16(even), 100), vget_low_u16(odd))
                }
            }
            #[inline(always)]
            unsafe fn packadd_4_local(v: uint32x4_t) -> uint64x2_t {
                use core::arch::aarch64::*;
                unsafe {
                    let even = vuzp1q_u32(v, v);
                    let odd = vuzp2q_u32(v, v);
                    vaddw_u32(vmull_n_u32(vget_low_u32(even), 10000), vget_low_u32(odd))
                }
            }
            let sum = match count {
                0 => 0,
                1 => vgetq_lane_u8::<0>(digits) as u64,
                2 => {
                    (vgetq_lane_u8::<0>(digits) as u64) * 10
                        + (vgetq_lane_u8::<1>(digits) as u64)
                }
                3 => {
                    let shifted = vextq_u8::<13>(vdupq_n_u8(0), digits);
                    let p1 = packadd_1_local(shifted);
                    (vgetq_lane_u16::<6>(p1) as u64) * 100
                        + (vgetq_lane_u16::<7>(p1) as u64)
                }
                4 => {
                    let shifted = vextq_u8::<12>(vdupq_n_u8(0), digits);
                    let p1 = packadd_1_local(shifted);
                    (vgetq_lane_u16::<6>(p1) as u64) * 100
                        + (vgetq_lane_u16::<7>(p1) as u64)
                }
                5 => {
                    let shifted = vextq_u8::<11>(vdupq_n_u8(0), digits);
                    let p1 = packadd_1_local(shifted);
                    let p2 = packadd_2_local(p1);
                    (vgetq_lane_u32::<2>(p2) as u64) * 10000
                        + (vgetq_lane_u32::<3>(p2) as u64)
                }
                6 => {
                    let shifted = vextq_u8::<10>(vdupq_n_u8(0), digits);
                    let p1 = packadd_1_local(shifted);
                    let p2 = packadd_2_local(p1);
                    (vgetq_lane_u32::<2>(p2) as u64) * 10000
                        + (vgetq_lane_u32::<3>(p2) as u64)
                }
                7 => {
                    let shifted = vextq_u8::<9>(vdupq_n_u8(0), digits);
                    let p1 = packadd_1_local(shifted);
                    let p2 = packadd_2_local(p1);
                    (vgetq_lane_u32::<2>(p2) as u64) * 10000
                        + (vgetq_lane_u32::<3>(p2) as u64)
                }
                8 => {
                    let shifted = vextq_u8::<8>(vdupq_n_u8(0), digits);
                    let p1 = packadd_1_local(shifted);
                    let p2 = packadd_2_local(p1);
                    (vgetq_lane_u32::<2>(p2) as u64) * 10000
                        + (vgetq_lane_u32::<3>(p2) as u64)
                }
                9 => {
                    let shifted = vextq_u8::<7>(vdupq_n_u8(0), digits);
                    let p1 = packadd_1_local(shifted);
                    let p2 = packadd_2_local(p1);
                    let p4 = packadd_4_local(p2);
                    vgetq_lane_u64::<0>(p4) * 100000000 + vgetq_lane_u64::<1>(p4)
                }
                10 => {
                    let shifted = vextq_u8::<6>(vdupq_n_u8(0), digits);
                    let p1 = packadd_1_local(shifted);
                    let p2 = packadd_2_local(p1);
                    let p4 = packadd_4_local(p2);
                    vgetq_lane_u64::<0>(p4) * 100000000 + vgetq_lane_u64::<1>(p4)
                }
                11 => {
                    let shifted = vextq_u8::<5>(vdupq_n_u8(0), digits);
                    let p1 = packadd_1_local(shifted);
                    let p2 = packadd_2_local(p1);
                    let p4 = packadd_4_local(p2);
                    vgetq_lane_u64::<0>(p4) * 100000000 + vgetq_lane_u64::<1>(p4)
                }
                12 => {
                    let shifted = vextq_u8::<4>(vdupq_n_u8(0), digits);
                    let p1 = packadd_1_local(shifted);
                    let p2 = packadd_2_local(p1);
                    let p4 = packadd_4_local(p2);
                    vgetq_lane_u64::<0>(p4) * 100000000 + vgetq_lane_u64::<1>(p4)
                }
                13 => {
                    let shifted = vextq_u8::<3>(vdupq_n_u8(0), digits);
                    let p1 = packadd_1_local(shifted);
                    let p2 = packadd_2_local(p1);
                    let p4 = packadd_4_local(p2);
                    vgetq_lane_u64::<0>(p4) * 100000000 + vgetq_lane_u64::<1>(p4)
                }
                14 => {
                    let shifted = vextq_u8::<2>(vdupq_n_u8(0), digits);
                    let p1 = packadd_1_local(shifted);
                    let p2 = packadd_2_local(p1);
                    let p4 = packadd_4_local(p2);
                    vgetq_lane_u64::<0>(p4) * 100000000 + vgetq_lane_u64::<1>(p4)
                }
                15 => {
                    let shifted = vextq_u8::<1>(vdupq_n_u8(0), digits);
                    let p1 = packadd_1_local(shifted);
                    let p2 = packadd_2_local(p1);
                    let p4 = packadd_4_local(p2);
                    vgetq_lane_u64::<0>(p4) * 100000000 + vgetq_lane_u64::<1>(p4)
                }
                16 => {
                    let p1 = packadd_1_local(digits);
                    let p2 = packadd_2_local(p1);
                    let p = packadd_4_local(p2);
                    vgetq_lane_u64::<0>(p) * 100000000 + vgetq_lane_u64::<1>(p)
                }
                _ => core::hint::unreachable_unchecked(),
            };
            (sum, count)
        }
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
    /// AZ-I.W2.RD — struct-direct Keyword-shape parse fn
    /// (single-literal body).
    ///
    /// Matches the literal byte sequence and routes the
    /// rule's projected payload through the `StructBuilder`
    /// trait surface. Returns `TapeOffset::NONE` on success
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
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        use crate::runtime::builder::StructBuilder as _;
        let at = *p;
        let end = at + 4usize;
        if input.len() < end || input[at..end] != [110u8, 117u8, 108u8, 108u8] {
            return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                offset: at as u32,
                failing_state: crate::runtime::tape::DtaStateId::NONE,
                failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
            });
        }
        *p = end;
        builder.push_leaf_with_unit();
        ::core::result::Result::Ok(crate::runtime::tape::TapeOffset::NONE)
    }
    /// AZ-I.W2.RD — struct-direct Keyword-shape parse fn
    /// (Alt of literal-led branches).
    ///
    /// Each branch's typed payload routes through
    /// `builder.push_leaf_with_bool` (TypeDesc::Bool) or
    /// `builder.push_leaf_with_unit` (TypeDesc::U8 /
    /// untyped). Returns `TapeOffset::NONE` for
    /// compositional uniformity.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_keyword_JsonParser_bool(
        input: &[u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'_>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
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
                    builder.push_leaf_with_bool(((0u32) as u32) != 0u32);
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
            116u8 => {
                if input.len() >= *p + 4usize
                    && input[*p..*p + 4usize] == [116u8, 114u8, 117u8, 101u8]
                {
                    let at = *p;
                    let end = at + 4usize;
                    *p = end;
                    builder.push_leaf_with_bool(((1u32) as u32) != 0u32);
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
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
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
            return Err(crate::runtime::tape::DtaError::Syntax {
                offset: start as u32,
                failing_state: crate::runtime::tape::DtaStateId::NONE,
                failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
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
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: start as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
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
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: start as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
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
        Ok(crate::runtime::tape::TapeOffset::NONE)
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
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        use crate::runtime::builder::StructBuilder as _;
        let open = *p;
        if input.get(open).copied() != Some(b'"') {
            return Err(crate::runtime::tape::DtaError::Syntax {
                offset: open as u32,
                failing_state: crate::runtime::tape::DtaStateId::NONE,
                failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
            });
        }
        let body_start = open + 1;
        let tail = match input.get(body_start..) {
            Some(t) => t,
            None => {
                return Err(crate::runtime::tape::DtaError::UnexpectedEnd {
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
                Ok(crate::runtime::tape::TapeOffset::NONE)
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
                        Ok(crate::runtime::tape::TapeOffset::NONE)
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
                        Ok(crate::runtime::tape::TapeOffset::NONE)
                    }
                    None => {
                        Err(crate::runtime::tape::DtaError::Syntax {
                            offset: open as u32,
                            failing_state: crate::runtime::tape::DtaStateId::NONE,
                            failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                        })
                    }
                }
            }
            Some(_) => unreachable!(),
            None => {
                Err(crate::runtime::tape::DtaError::UnexpectedEnd {
                    offset: open as u32,
                })
            }
        }
    }
    /// AZ-I.W2.RB — per-grammar Array-shape parse function,
    /// **struct-direct body**. Targets [`JsonStructBuilder`].
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_array_JsonParser_array<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        use crate::runtime::builder::StructBuilder;
        if input.get(*p).copied() != Some(b'[') {
            return Err(crate::runtime::tape::DtaError::Syntax {
                offset: *p as u32,
                failing_state: crate::runtime::tape::DtaStateId::NONE,
                failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
            });
        }
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 4u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("array"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __handle = builder.begin_compound(&__layout);
        *p += 1;
        let _ = __shape_support_JsonParser::skip_space(input, p, state);
        if input.get(*p).copied() == Some(b']') {
            *p += 1;
            builder.end_compound(__handle);
            return Ok(crate::runtime::tape::TapeOffset::NONE);
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
                    builder.end_compound(__handle);
                    return Ok(crate::runtime::tape::TapeOffset::NONE);
                }
                _ => {
                    return Err(crate::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    });
                }
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
    pub fn parse_flat_JsonParser_pair<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let __pair_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 5u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("pair"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __pair_handle = <crate::runtime::json::JsonStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__pair_layout);
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
                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                    offset: at as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
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
        <crate::runtime::json::JsonStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::end_compound(builder, __pair_handle);
        ::core::result::Result::Ok(crate::runtime::tape::TapeOffset::NONE)
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
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        use crate::runtime::builder::StructBuilder;
        if input.get(*p).copied() != Some(b'{') {
            return Err(crate::runtime::tape::DtaError::Syntax {
                offset: *p as u32,
                failing_state: crate::runtime::tape::DtaStateId::NONE,
                failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
            });
        }
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 6u32 as ::bbnf_ir::RuleId,
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
            return Ok(crate::runtime::tape::TapeOffset::NONE);
        }
        loop {
            if input.get(*p).copied() != Some(b'"') {
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            parse_string_JsonParser_string(input, p, state, builder)?;
            let _ = __shape_support_JsonParser::skip_space(input, p, state);
            if input.get(*p).copied() != Some(b':') {
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
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
                    return Ok(crate::runtime::tape::TapeOffset::NONE);
                }
                _ => {
                    return Err(crate::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    });
                }
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
    /// Returns `TapeOffset::NONE` for compositional uniformity
    /// with sibling shape fns under struct-direct mode; the
    /// offset is unused by struct-direct callers.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_wrap_JsonParser_value<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let __wrap_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 7u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("value"),
            kind: ::bbnf_ir::registry::LayoutKind::TaggedEnum,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __wrap_handle = <crate::runtime::json::JsonStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__wrap_layout);
        let mut __wrap_branch_idx: u32 = 0;
        let first = __shape_support_JsonParser::skip_space(input, p, state)
            .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        'try_branches: loop {
            match first {
                34u8 => {
                    let attempt_p = *p;
                    match parse_string_JsonParser_string(input, p, state, builder) {
                        ::core::result::Result::Ok(_) => {
                            __wrap_branch_idx = 2u32;
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                45u8 => {
                    let attempt_p = *p;
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            __wrap_branch_idx = 5u32;
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                48u8 => {
                    let attempt_p = *p;
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            __wrap_branch_idx = 5u32;
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                49u8 => {
                    let attempt_p = *p;
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            __wrap_branch_idx = 5u32;
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                50u8 => {
                    let attempt_p = *p;
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            __wrap_branch_idx = 5u32;
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                51u8 => {
                    let attempt_p = *p;
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            __wrap_branch_idx = 5u32;
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                52u8 => {
                    let attempt_p = *p;
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            __wrap_branch_idx = 5u32;
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                53u8 => {
                    let attempt_p = *p;
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            __wrap_branch_idx = 5u32;
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                54u8 => {
                    let attempt_p = *p;
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            __wrap_branch_idx = 5u32;
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                55u8 => {
                    let attempt_p = *p;
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            __wrap_branch_idx = 5u32;
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                56u8 => {
                    let attempt_p = *p;
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            __wrap_branch_idx = 5u32;
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                57u8 => {
                    let attempt_p = *p;
                    match parse_number_JsonParser_number(input, p, first, builder) {
                        ::core::result::Result::Ok(_) => {
                            __wrap_branch_idx = 5u32;
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                91u8 => {
                    let attempt_p = *p;
                    match parse_array_JsonParser_array(input, p, state, builder) {
                        ::core::result::Result::Ok(_) => {
                            __wrap_branch_idx = 1u32;
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                102u8 => {
                    let attempt_p = *p;
                    match parse_keyword_JsonParser_bool(
                        input,
                        p,
                        first,
                        state,
                        builder,
                    ) {
                        ::core::result::Result::Ok(_) => {
                            __wrap_branch_idx = 4u32;
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                110u8 => {
                    let attempt_p = *p;
                    match parse_keyword_JsonParser_null(
                        input,
                        p,
                        first,
                        state,
                        builder,
                    ) {
                        ::core::result::Result::Ok(_) => {
                            __wrap_branch_idx = 3u32;
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                116u8 => {
                    let attempt_p = *p;
                    match parse_keyword_JsonParser_bool(
                        input,
                        p,
                        first,
                        state,
                        builder,
                    ) {
                        ::core::result::Result::Ok(_) => {
                            __wrap_branch_idx = 4u32;
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                123u8 => {
                    let attempt_p = *p;
                    match parse_object_JsonParser_object(input, p, state, builder) {
                        ::core::result::Result::Ok(_) => {
                            __wrap_branch_idx = 0u32;
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                        }
                    }
                }
                _ => {}
            }
            <crate::runtime::json::JsonStructBuilder<
                '_,
            > as crate::runtime::StructBuilder>::end_compound(builder, __wrap_handle);
            return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                offset: *p as u32,
                failing_state: crate::runtime::tape::DtaStateId::NONE,
                failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
            });
        }
        <crate::runtime::json::JsonStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::push_branch_tag(builder, __wrap_branch_idx);
        <crate::runtime::json::JsonStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::end_compound(builder, __wrap_handle);
        ::core::result::Result::Ok(crate::runtime::tape::TapeOffset::NONE)
    }
    /// AW-V.W3-bench-fix — visitor-path Keyword-shape parse
    /// function (single-literal body).
    ///
    /// AX.W0a.2.g — `state` parameter unused for single-
    /// literal form.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_keyword_visitor_JsonParser_null<V>(
        input: &[u8],
        p: &mut usize,
        _first_byte: u8,
        _state: &mut __shape_support_JsonParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::KeywordVisitor + crate::runtime::tape::ObjectVisitor
            + crate::runtime::tape::ArrayVisitor + crate::runtime::tape::StringVisitor
            + crate::runtime::tape::NumberVisitor,
    {
        let at = *p;
        let end = at + 4usize;
        if input.len() < end || input[at..end] != [110u8, 117u8, 108u8, 108u8] {
            return Err(crate::runtime::ParseErr::Syntax {
                offset: at as u32,
                rule: None,
            });
        }
        *p = end;
        visitor
            .null()
            .map_err(|_| crate::runtime::ParseErr::Syntax {
                offset: at as u32,
                rule: None,
            })
    }
    /// AW-V.W3-bench-fix — visitor-path Keyword-shape parse
    /// function (Alt of literal-led or Ref-led branches).
    ///
    /// AX.W0a.2.g — admits Ref-led branches; threads `state`
    /// for downstream visitor-path Ref calls.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_keyword_visitor_JsonParser_bool<V>(
        input: &[u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_JsonParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::KeywordVisitor + crate::runtime::tape::ObjectVisitor
            + crate::runtime::tape::ArrayVisitor + crate::runtime::tape::StringVisitor
            + crate::runtime::tape::NumberVisitor,
    {
        let _ = state;
        match first_byte {
            102u8 => {
                if input.len() >= *p + 5usize
                    && input[*p..*p + 5usize] == [102u8, 97u8, 108u8, 115u8, 101u8]
                {
                    let at = *p;
                    let end = at + 5usize;
                    *p = end;
                    return visitor
                        .bool(false)
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
            116u8 => {
                if input.len() >= *p + 4usize
                    && input[*p..*p + 4usize] == [116u8, 114u8, 117u8, 101u8]
                {
                    let at = *p;
                    let end = at + 4usize;
                    *p = end;
                    return visitor
                        .bool(true)
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
    /// AW-V.W3-bench-fix — visitor-path Number-shape parse function.
    ///
    /// Mirrors `json_prototype::number::parse_number_body::<V>`.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_number_visitor_JsonParser_number<V>(
        input: &[u8],
        p: &mut usize,
        first_byte: u8,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::NumberVisitor,
    {
        const POW10_U64: [u64; 17] = [
            1, 10, 100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000, 100_000_000,
            1_000_000_000, 10_000_000_000, 100_000_000_000, 1_000_000_000_000,
            10_000_000_000_000, 100_000_000_000_000, 1_000_000_000_000_000,
            10_000_000_000_000_000,
        ];
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
            return Err(crate::runtime::ParseErr::Syntax {
                offset: start as u32,
                rule: None,
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
            #[cfg(target_arch = "aarch64")]
            {
                while *p + 16 <= len {
                    let (sum, count) = unsafe {
                        __number_simd_str2int(&input[*p..*p + 16])
                    };
                    if count == 0 {
                        break;
                    }
                    mantissa = mantissa.wrapping_mul(POW10_U64[count]).wrapping_add(sum);
                    *p += count;
                    if count < 16 {
                        break;
                    }
                }
            }
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
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: start as u32,
                    rule: None,
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
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: start as u32,
                    rule: None,
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
        visitor
            .number_f64(value)
            .map_err(|_| crate::runtime::ParseErr::Syntax {
                offset: start as u32,
                rule: None,
            })
    }
    /// AW-V.W3-bench-fix — visitor-path String-shape parse function.
    ///
    /// Mirrors `json_prototype::string::parse_string_body::<V>`.
    /// `"` must NOT be consumed by the caller. Borrow-path reads
    /// the full span from input; escape-path decodes into a local
    /// buffer.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_string_visitor_JsonParser_string<V>(
        input: &[u8],
        p: &mut usize,
        _state: &mut __shape_support_JsonParser::ScanState,
        visitor: &mut V,
        is_key: bool,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::StringVisitor + crate::runtime::tape::ObjectVisitor,
    {
        let open = *p;
        if input.get(open).copied() != Some(b'"') {
            return Err(crate::runtime::ParseErr::Syntax {
                offset: open as u32,
                rule: None,
            });
        }
        let body_start = open + 1;
        let tail = match input.get(body_start..) {
            Some(t) => t,
            None => {
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: open as u32,
                    rule: None,
                });
            }
        };
        match __shape_support_JsonParser::first_quote_or_backslash(tail) {
            Some((off, b'"')) => {
                let end = body_start + off;
                let body = &input[body_start..end];
                *p = end + 1;
                if is_key {
                    visitor
                        .key(body)
                        .map_err(|_| crate::runtime::ParseErr::Syntax {
                            offset: open as u32,
                            rule: None,
                        })
                } else {
                    visitor
                        .string(body)
                        .map_err(|_| crate::runtime::ParseErr::Syntax {
                            offset: open as u32,
                            rule: None,
                        })
                }
            }
            Some((off, b'\\')) => {
                let esc_start = body_start + off;
                parse_string_visitor_escaped_JsonParser(
                    input,
                    p,
                    body_start,
                    esc_start,
                    visitor,
                    is_key,
                    open,
                )
            }
            Some(_) => unreachable!(),
            None => {
                Err(crate::runtime::ParseErr::Syntax {
                    offset: open as u32,
                    rule: None,
                })
            }
        }
    }
    /// AW-V.W3-bench-fix — visitor-path Array-shape parse function.
    ///
    /// Mirrors `json_prototype::parse_array::<V>`. Bypasses
    /// the tape entirely.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_array_visitor_JsonParser_array<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
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
        if let Some(b) = __shape_support_JsonParser::skip_space(input, p, state) {
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
                let _ = __shape_support_JsonParser::skip_space(input, p, state);
                parse_wrap_visitor_JsonParser_value(input, p, state, visitor)
            })?;
            match __shape_support_JsonParser::skip_space(input, p, state) {
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
                    let _ = __shape_support_JsonParser::skip_space(input, p, state);
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
    pub fn parse_flat_visitor_JsonParser_pair<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        {
            ({
                let _ = __shape_support_JsonParser::skip_space(input, p, state);
                parse_string_visitor_JsonParser_string(input, p, state, visitor, false)
            })?;
        }
        {
            let _ = __shape_support_JsonParser::skip_space(input, p, state);
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [58u8] {
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: at as u32,
                    rule: None,
                });
            }
            *p = end;
            let _ = __shape_support_JsonParser::skip_space(input, p, state);
        }
        {
            ({
                let _ = __shape_support_JsonParser::skip_space(input, p, state);
                parse_wrap_visitor_JsonParser_value(input, p, state, visitor)
            })?;
        }
        Ok(())
    }
    /// AW-V.W3-bench-fix — visitor-path Object-shape parse function.
    ///
    /// Mirrors `json_prototype::parse_object::<V>`. Bypasses
    /// the tape entirely; visitor method calls drive materialisation.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_object_visitor_JsonParser_object<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        let begin_at = *p;
        if input.get(*p).copied() != Some(b'{') {
            return Err(crate::runtime::ParseErr::Syntax {
                offset: begin_at as u32,
                rule: None,
            });
        }
        *p += 1;
        visitor
            .begin_object()
            .map_err(|_| crate::runtime::ParseErr::Syntax {
                offset: begin_at as u32,
                rule: None,
            })?;
        let mut cur = __shape_support_JsonParser::skip_space(input, p, state);
        if cur == Some(b'}') {
            *p += 1;
            return visitor
                .end_object()
                .map_err(|_| crate::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                });
        }
        loop {
            if cur != Some(b'"') {
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                });
            }
            parse_string_visitor_JsonParser_string(input, p, state, visitor, true)?;
            if __shape_support_JsonParser::skip_space(input, p, state) != Some(b':') {
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                });
            }
            *p += 1;
            let _ = __shape_support_JsonParser::skip_space(input, p, state);
            ({
                let _ = __shape_support_JsonParser::skip_space(input, p, state);
                parse_wrap_visitor_JsonParser_value(input, p, state, visitor)
            })?;
            match __shape_support_JsonParser::skip_space(input, p, state) {
                Some(b'}') => {
                    *p += 1;
                    return visitor
                        .end_object()
                        .map_err(|_| crate::runtime::ParseErr::Syntax {
                            offset: *p as u32,
                            rule: None,
                        });
                }
                Some(b',') => {
                    *p += 1;
                    cur = __shape_support_JsonParser::skip_space(input, p, state);
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
    /// AW-V.W4-fix — visitor-path Wrap-shape parse function.
    ///
    /// Transparent dispatcher — skip leading ws, byte-dispatch to
    /// the chosen branch's visitor-path shape fn. No visitor event
    /// fires here; the chosen branch's visitor fn owns the event
    /// emission.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
    /// recursion rationale.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_wrap_visitor_JsonParser_value<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        let first = __shape_support_JsonParser::skip_space(input, p, state)
            .ok_or(crate::runtime::ParseErr::Syntax {
                offset: *p as u32,
                rule: None,
            })?;
        match first {
            123u8 => parse_object_visitor_JsonParser_object(input, p, state, visitor),
            91u8 => parse_array_visitor_JsonParser_array(input, p, state, visitor),
            34u8 => {
                parse_string_visitor_JsonParser_string(input, p, state, visitor, false)
            }
            110u8 => {
                parse_keyword_visitor_JsonParser_null(input, p, first, state, visitor)
            }
            102u8 | 116u8 => {
                parse_keyword_visitor_JsonParser_bool(input, p, first, state, visitor)
            }
            45u8 | 48u8 | 49u8 | 50u8 | 51u8 | 52u8 | 53u8 | 54u8 | 55u8 | 56u8
            | 57u8 => parse_number_visitor_JsonParser_number(input, p, first, visitor),
            _ => parse_JsonParser_value_visitor__value(input, p, state, visitor),
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
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Empty,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 1u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Empty,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 2u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Empty,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 3u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Empty,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 4u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Sparse,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 5u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Empty,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 6u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Sparse,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 7u32,
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
    pub fn parse_JsonParser_value<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        builder: &mut crate::runtime::json::JsonStructBuilder<'p>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
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
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let first = __shape_support_JsonParser::skip_space(input, p, state)
            .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
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
                return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
        };
        __result
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
    pub fn parse_JsonParser_value_visitor<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        parse_JsonParser_value_visitor__value(input, p, state, visitor)
    }
    /// AW-V.W3-bench-fix — value-position visitor-path dispatcher.
    /// Called both at the grammar root and from the object / array
    /// shape fns' value-position recursion.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_JsonParser_value_visitor__value<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_JsonParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        let first = __shape_support_JsonParser::skip_space(input, p, state)
            .ok_or(crate::runtime::ParseErr::Syntax {
                offset: *p as u32,
                rule: None,
            })?;
        match first {
            b'{' => parse_object_visitor_JsonParser_object(input, p, state, visitor),
            b'[' => parse_array_visitor_JsonParser_array(input, p, state, visitor),
            b'"' => {
                parse_string_visitor_JsonParser_string(input, p, state, visitor, false)
            }
            b'-' | b'0'..=b'9' => {
                parse_number_visitor_JsonParser_number(input, p, first, visitor)
            }
            b't' | b'f' => {
                parse_keyword_visitor_JsonParser_bool(input, p, first, state, visitor)
            }
            b'n' => {
                parse_keyword_visitor_JsonParser_null(input, p, first, state, visitor)
            }
            _ => {
                Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32,
                    rule: None,
                })
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct nullView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> nullView<'p> {
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
        pub fn rule_kind(&self) -> JsonParserRuleKind {
            match self.variant_idx() {
                0u8 => JsonParserRuleKind::null,
                1u8 => JsonParserRuleKind::bool,
                2u8 => JsonParserRuleKind::number,
                3u8 => JsonParserRuleKind::string,
                4u8 => JsonParserRuleKind::array,
                5u8 => JsonParserRuleKind::pair,
                6u8 => JsonParserRuleKind::object,
                7u8 => JsonParserRuleKind::value,
                8u8 => JsonParserRuleKind::value_0,
                9u8 => JsonParserRuleKind::value_1,
                10u8 => JsonParserRuleKind::value_2,
                _ => JsonParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = JsonParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| JsonParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<JsonParserNodeView<'p>> {
            self.cursor.child(i).map(|c| JsonParserNodeView::from_cursor(c, self.input))
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
    impl<'p> nullView<'p> {
        /// The source text matched by this leaf rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// Get the parsed scalar value.
        ///
        /// Payload-first: reads the pre-computed value from the
        /// tape payload buffer in O(1). Falls back to span text
        /// parsing if no payload is present.
        #[inline]
        pub fn value(&self) -> u8 {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            if let Some(v) = tape.payload_u8(rec) {
                return v;
            }
            self.span_text().parse::<u8>().unwrap_or(0)
        }
        /// Convert the matched span to the scalar type.
        ///
        /// Alias for backward compatibility. Prefer `.value()`.
        #[inline]
        pub fn as_u8(&self) -> u8 {
            self.value()
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct boolView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> boolView<'p> {
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
        pub fn rule_kind(&self) -> JsonParserRuleKind {
            match self.variant_idx() {
                0u8 => JsonParserRuleKind::null,
                1u8 => JsonParserRuleKind::bool,
                2u8 => JsonParserRuleKind::number,
                3u8 => JsonParserRuleKind::string,
                4u8 => JsonParserRuleKind::array,
                5u8 => JsonParserRuleKind::pair,
                6u8 => JsonParserRuleKind::object,
                7u8 => JsonParserRuleKind::value,
                8u8 => JsonParserRuleKind::value_0,
                9u8 => JsonParserRuleKind::value_1,
                10u8 => JsonParserRuleKind::value_2,
                _ => JsonParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = JsonParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| JsonParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<JsonParserNodeView<'p>> {
            self.cursor.child(i).map(|c| JsonParserNodeView::from_cursor(c, self.input))
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
    impl<'p> boolView<'p> {
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
        pub fn value(&self) -> (bool) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 1usize) {
                Some(__bytes) => (__bytes[0usize] != 0),
                None => (false),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct numberView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> numberView<'p> {
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
        pub fn rule_kind(&self) -> JsonParserRuleKind {
            match self.variant_idx() {
                0u8 => JsonParserRuleKind::null,
                1u8 => JsonParserRuleKind::bool,
                2u8 => JsonParserRuleKind::number,
                3u8 => JsonParserRuleKind::string,
                4u8 => JsonParserRuleKind::array,
                5u8 => JsonParserRuleKind::pair,
                6u8 => JsonParserRuleKind::object,
                7u8 => JsonParserRuleKind::value,
                8u8 => JsonParserRuleKind::value_0,
                9u8 => JsonParserRuleKind::value_1,
                10u8 => JsonParserRuleKind::value_2,
                _ => JsonParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = JsonParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| JsonParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<JsonParserNodeView<'p>> {
            self.cursor.child(i).map(|c| JsonParserNodeView::from_cursor(c, self.input))
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
    impl<'p> numberView<'p> {
        /// The source text matched by this leaf rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// Get the parsed scalar value.
        ///
        /// Payload-first: reads the pre-computed value from the
        /// tape payload buffer in O(1). Falls back to span text
        /// parsing if no payload is present.
        #[inline]
        pub fn value(&self) -> f64 {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            if let Some(v) = tape.payload_f64(rec) {
                return v;
            }
            self.span_text().parse::<f64>().unwrap_or(0.0)
        }
        /// Convert the matched span to the scalar type.
        ///
        /// Alias for backward compatibility. Prefer `.value()`.
        #[inline]
        pub fn as_f64(&self) -> f64 {
            self.value()
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct stringView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> stringView<'p> {
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
        pub fn rule_kind(&self) -> JsonParserRuleKind {
            match self.variant_idx() {
                0u8 => JsonParserRuleKind::null,
                1u8 => JsonParserRuleKind::bool,
                2u8 => JsonParserRuleKind::number,
                3u8 => JsonParserRuleKind::string,
                4u8 => JsonParserRuleKind::array,
                5u8 => JsonParserRuleKind::pair,
                6u8 => JsonParserRuleKind::object,
                7u8 => JsonParserRuleKind::value,
                8u8 => JsonParserRuleKind::value_0,
                9u8 => JsonParserRuleKind::value_1,
                10u8 => JsonParserRuleKind::value_2,
                _ => JsonParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = JsonParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| JsonParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<JsonParserNodeView<'p>> {
            self.cursor.child(i).map(|c| JsonParserNodeView::from_cursor(c, self.input))
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
    impl<'p> stringView<'p> {
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
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct arrayView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> arrayView<'p> {
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
        pub fn rule_kind(&self) -> JsonParserRuleKind {
            match self.variant_idx() {
                0u8 => JsonParserRuleKind::null,
                1u8 => JsonParserRuleKind::bool,
                2u8 => JsonParserRuleKind::number,
                3u8 => JsonParserRuleKind::string,
                4u8 => JsonParserRuleKind::array,
                5u8 => JsonParserRuleKind::pair,
                6u8 => JsonParserRuleKind::object,
                7u8 => JsonParserRuleKind::value,
                8u8 => JsonParserRuleKind::value_0,
                9u8 => JsonParserRuleKind::value_1,
                10u8 => JsonParserRuleKind::value_2,
                _ => JsonParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = JsonParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| JsonParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<JsonParserNodeView<'p>> {
            self.cursor.child(i).map(|c| JsonParserNodeView::from_cursor(c, self.input))
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
    impl<'p> arrayView<'p> {
        /// The source text matched by this leaf rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct pairView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> pairView<'p> {
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
        pub fn rule_kind(&self) -> JsonParserRuleKind {
            match self.variant_idx() {
                0u8 => JsonParserRuleKind::null,
                1u8 => JsonParserRuleKind::bool,
                2u8 => JsonParserRuleKind::number,
                3u8 => JsonParserRuleKind::string,
                4u8 => JsonParserRuleKind::array,
                5u8 => JsonParserRuleKind::pair,
                6u8 => JsonParserRuleKind::object,
                7u8 => JsonParserRuleKind::value,
                8u8 => JsonParserRuleKind::value_0,
                9u8 => JsonParserRuleKind::value_1,
                10u8 => JsonParserRuleKind::value_2,
                _ => JsonParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = JsonParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| JsonParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<JsonParserNodeView<'p>> {
            self.cursor.child(i).map(|c| JsonParserNodeView::from_cursor(c, self.input))
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
    impl<'p> pairView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<stringView<'p>> {
            self.cursor.child(0usize).map(|c| stringView::from_cursor(c, self.input))
        }
        ///The `string` child as a typed view.
        #[inline]
        pub fn string(&self) -> ::core::option::Option<stringView<'p>> {
            self.cursor.child(0usize).map(|c| stringView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<valueView<'p>> {
            self.cursor.child(1usize).map(|c| valueView::from_cursor(c, self.input))
        }
        ///The `value` child as a typed view.
        #[inline]
        pub fn value(&self) -> ::core::option::Option<valueView<'p>> {
            self.cursor.child(1usize).map(|c| valueView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct objectView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> objectView<'p> {
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
        pub fn rule_kind(&self) -> JsonParserRuleKind {
            match self.variant_idx() {
                0u8 => JsonParserRuleKind::null,
                1u8 => JsonParserRuleKind::bool,
                2u8 => JsonParserRuleKind::number,
                3u8 => JsonParserRuleKind::string,
                4u8 => JsonParserRuleKind::array,
                5u8 => JsonParserRuleKind::pair,
                6u8 => JsonParserRuleKind::object,
                7u8 => JsonParserRuleKind::value,
                8u8 => JsonParserRuleKind::value_0,
                9u8 => JsonParserRuleKind::value_1,
                10u8 => JsonParserRuleKind::value_2,
                _ => JsonParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = JsonParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| JsonParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<JsonParserNodeView<'p>> {
            self.cursor.child(i).map(|c| JsonParserNodeView::from_cursor(c, self.input))
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
    impl<'p> objectView<'p> {
        /// The source text matched by this leaf rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct valueView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> valueView<'p> {
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
        pub fn rule_kind(&self) -> JsonParserRuleKind {
            match self.variant_idx() {
                0u8 => JsonParserRuleKind::null,
                1u8 => JsonParserRuleKind::bool,
                2u8 => JsonParserRuleKind::number,
                3u8 => JsonParserRuleKind::string,
                4u8 => JsonParserRuleKind::array,
                5u8 => JsonParserRuleKind::pair,
                6u8 => JsonParserRuleKind::object,
                7u8 => JsonParserRuleKind::value,
                8u8 => JsonParserRuleKind::value_0,
                9u8 => JsonParserRuleKind::value_1,
                10u8 => JsonParserRuleKind::value_2,
                _ => JsonParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = JsonParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| JsonParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<JsonParserNodeView<'p>> {
            self.cursor.child(i).map(|c| JsonParserNodeView::from_cursor(c, self.input))
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
    impl<'p> valueView<'p> {
        ///If variant `object` (branch 0) was chosen, return its child view.
        #[inline]
        pub fn as_object(&self) -> ::core::option::Option<objectView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor.child(0).map(|c| objectView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `object` (branch 0) was chosen.
        #[inline]
        pub fn is_object(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If variant `array` (branch 1) was chosen, return its child view.
        #[inline]
        pub fn as_array(&self) -> ::core::option::Option<arrayView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor.child(0).map(|c| arrayView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `array` (branch 1) was chosen.
        #[inline]
        pub fn is_array(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        ///If variant `string` (branch 2) was chosen, return its child view.
        #[inline]
        pub fn as_string(&self) -> ::core::option::Option<stringView<'p>> {
            if self.cursor.meta_idx() == 2u8 {
                self.cursor.child(0).map(|c| stringView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `string` (branch 2) was chosen.
        #[inline]
        pub fn is_string(&self) -> bool {
            self.cursor.meta_idx() == 2u8
        }
        ///If variant `null` (branch 3) was chosen, return its child view.
        #[inline]
        pub fn as_null(&self) -> ::core::option::Option<nullView<'p>> {
            if self.cursor.meta_idx() == 3u8 {
                self.cursor.child(0).map(|c| nullView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `null` (branch 3) was chosen.
        #[inline]
        pub fn is_null(&self) -> bool {
            self.cursor.meta_idx() == 3u8
        }
        ///If variant `bool` (branch 4) was chosen, return its child view.
        #[inline]
        pub fn as_bool(&self) -> ::core::option::Option<boolView<'p>> {
            if self.cursor.meta_idx() == 4u8 {
                self.cursor.child(0).map(|c| boolView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `bool` (branch 4) was chosen.
        #[inline]
        pub fn is_bool(&self) -> bool {
            self.cursor.meta_idx() == 4u8
        }
        ///If variant `number` (branch 5) was chosen, return its child view.
        #[inline]
        pub fn as_number(&self) -> ::core::option::Option<numberView<'p>> {
            if self.cursor.meta_idx() == 5u8 {
                self.cursor.child(0).map(|c| numberView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `number` (branch 5) was chosen.
        #[inline]
        pub fn is_number(&self) -> bool {
            self.cursor.meta_idx() == 5u8
        }
        ///If sub-variant `value_0` was chosen (branch 3), return its child view.
        #[inline]
        pub fn as_value_0(&self) -> ::core::option::Option<JsonParserNodeView<'p>> {
            if self.cursor.meta_idx() == 3u8 {
                self.cursor
                    .child(0)
                    .map(|c| JsonParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_value_0(&self) -> bool {
            self.cursor.meta_idx() == 3u8
        }
        ///If sub-variant `value_1` was chosen (branch 4), return its child view.
        #[inline]
        pub fn as_value_1(&self) -> ::core::option::Option<JsonParserNodeView<'p>> {
            if self.cursor.meta_idx() == 4u8 {
                self.cursor
                    .child(0)
                    .map(|c| JsonParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_value_1(&self) -> bool {
            self.cursor.meta_idx() == 4u8
        }
        ///If sub-variant `value_2` was chosen (branch 5), return its child view.
        #[inline]
        pub fn as_value_2(&self) -> ::core::option::Option<JsonParserNodeView<'p>> {
            if self.cursor.meta_idx() == 5u8 {
                self.cursor
                    .child(0)
                    .map(|c| JsonParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_value_2(&self) -> bool {
            self.cursor.meta_idx() == 5u8
        }
        /// The chosen branch's child as a generic node view,
        /// regardless of which variant was selected.
        #[inline]
        pub fn chosen(&self) -> ::core::option::Option<JsonParserNodeView<'p>> {
            self.cursor.child(0).map(|c| JsonParserNodeView::from_cursor(c, self.input))
        }
    }
    /// Typed value enum — payload-eligible branches carry typed
    /// values directly; non-eligible branches wrap a cursor view.
    #[derive(Clone, Debug)]
    pub enum valueValue<'p> {
        object(JsonParserNodeView<'p>),
        array(JsonParserNodeView<'p>),
        string(((u32, u32))),
        null(u8),
        bool((bool)),
        number(f64),
    }
    impl<'p> valueView<'p> {
        /// Decode the chosen branch's value. Payload-eligible
        /// branches return typed scalars/aggregates; other
        /// branches return cursor-wrapped sub-views.
        #[inline]
        pub fn value(&self) -> ::core::option::Option<valueValue<'p>> {
            match self.cursor.meta_idx() {
                0u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        valueValue::object(
                            JsonParserNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                1u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        valueValue::array(
                            JsonParserNodeView::from_cursor(__child, self.input),
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
                    Some(valueValue::string(__value))
                }
                3u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = __tape
                        .payload_u8(__rec)
                        .unwrap_or(<u8 as ::core::default::Default>::default());
                    Some(valueValue::null(__value))
                }
                4u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_bytes(__rec, 1usize) {
                        Some(__bytes) => (__bytes[0usize] != 0),
                        None => (false),
                    };
                    Some(valueValue::bool(__value))
                }
                5u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = __tape
                        .payload_f64(__rec)
                        .unwrap_or(<f64 as ::core::default::Default>::default());
                    Some(valueValue::number(__value))
                }
                _ => None,
            }
        }
    }
    /// Generic node view over any tape record for this grammar.
    #[derive(Clone, Copy, Debug)]
    pub struct JsonParserNodeView<'p> {
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
    pub enum JsonParserRuleKind {
        null,
        bool,
        number,
        string,
        array,
        pair,
        object,
        value,
        value_0,
        value_1,
        value_2,
        /// Fallback for records whose variant_idx is not a
        /// known rule- or sub-variant discriminator.
        Unknown,
    }
    impl<'p> JsonParserNodeView<'p> {
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
        pub fn rule_kind(&self) -> JsonParserRuleKind {
            match self.variant_idx() {
                0u8 => JsonParserRuleKind::null,
                1u8 => JsonParserRuleKind::bool,
                2u8 => JsonParserRuleKind::number,
                3u8 => JsonParserRuleKind::string,
                4u8 => JsonParserRuleKind::array,
                5u8 => JsonParserRuleKind::pair,
                6u8 => JsonParserRuleKind::object,
                7u8 => JsonParserRuleKind::value,
                8u8 => JsonParserRuleKind::value_0,
                9u8 => JsonParserRuleKind::value_1,
                10u8 => JsonParserRuleKind::value_2,
                _ => JsonParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = JsonParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| JsonParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(&self, i: usize) -> ::core::option::Option<JsonParserNodeView<'p>> {
            self.cursor.child(i).map(|c| JsonParserNodeView::from_cursor(c, self.input))
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
    impl crate::runtime::Root for JsonParser {
        type View<'p> = valueView<'p>;
        #[inline]
        fn make_view<'p>(
            tape: &'p crate::runtime::tape::Tape<()>,
            input: &'p str,
            root: crate::runtime::tape::TapeOffset,
        ) -> Self::View<'p> {
            valueView::new(tape, input, root)
        }
    }
    impl JsonParser {
        /// The name of the root rule for this grammar.
        #[inline]
        pub fn root_rule_name() -> &'static str {
            "value"
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
    pub struct JsonParserBoolProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: bool,
    }
    impl JsonParserBoolProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "bool";
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
        pub const TOTAL_BYTES: u8 = 1;
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
    pub struct JsonParserStringProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: (u32, u32),
    }
    impl JsonParserStringProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "string";
        /// Grammar-declared `-> Name` binding; empty string
        /// when the admission came from a pure layout arm.
        #[doc(hidden)]
        pub const NAMED_BINDING: &'static str = "String";
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
        ("bool", "JsonParserBoolProjection"),
        ("string", "JsonParserStringProjection"),
    ];
    /// AY-II.W0.d — grammar-declared `-> Name` bindings, indexed in
    /// lockstep with `PROJECTION_DIRECT_TO_STRUCT`. Empty string for
    /// admissions that did not spell a named type.
    #[doc(hidden)]
    pub const PROJECTION_NAMED_BINDINGS: &[&str; 2usize] = &["", "String"];
    /// AY-II.W0.d — canonical evidence that every admission has a
    /// matching `materialize_projection_<rule>_<Grammar>` fn.
    /// Indexed in lockstep with `PROJECTION_DIRECT_TO_STRUCT`; the
    /// wire-contract totality test asserts both slices share the
    /// same length per grammar.
    #[doc(hidden)]
    pub const PROJECTION_MATERIALIZERS: &[&str; 2usize] = &[
        "materialize_projection_bool_JsonParser",
        "materialize_projection_string_JsonParser",
    ];
    /// AY-II.W0.d — canonical evidence that every admission has a
    /// matching `<Grammar>Value::<RuleName>` enum variant
    /// (production consumer). Indexed in lockstep with
    /// `PROJECTION_DIRECT_TO_STRUCT`.
    #[doc(hidden)]
    pub const PROJECTION_CONSUMERS: &[&str; 2usize] = &[
        "JsonParserValue::bool",
        "JsonParserValue::string",
    ];
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_bool() -> (&'static str, usize, &'static str) {
        ("bool", 1, "")
    }
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_string() -> (&'static str, usize, &'static str) {
        ("string", 1, "String")
    }
    /// AY-II.W0'.b — grammar-emitted value enum. Eager
    /// materialisation target for `Parsed::to_value()`. Variants
    /// enumerate non-transparent rules; admitted rules carry the
    /// matching `<Grammar><RuleCamel>Projection` struct directly,
    /// non-admitted rules carry their shape-classified payload.
    #[derive(Clone, Debug)]
    pub enum JsonParserValue<'p> {
        null(u8),
        bool(JsonParserBoolProjection),
        number(f64),
        string(JsonParserStringProjection),
        array(&'p str),
        pair(::std::vec::Vec<JsonParserValue<'p>>),
        object(&'p str),
        value(::std::vec::Vec<JsonParserValue<'p>>),
        /// Fallback for records whose `variant_idx` is not a
        /// known rule discriminator (recovered records, stray
        /// sub-variant indices).
        Unknown(JsonParserNodeView<'p>),
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
    fn project_rule_kind_JsonParser(
        kind: crate::runtime::tape::TapeKind,
        variant_idx: u8,
    ) -> JsonParserRuleKind {
        if variant_idx == 0 && kind.is_compound() {
            return JsonParserRuleKind::Unknown;
        }
        match variant_idx {
            0u8 => JsonParserRuleKind::null,
            1u8 => JsonParserRuleKind::bool,
            2u8 => JsonParserRuleKind::number,
            3u8 => JsonParserRuleKind::string,
            4u8 => JsonParserRuleKind::array,
            5u8 => JsonParserRuleKind::pair,
            6u8 => JsonParserRuleKind::object,
            7u8 => JsonParserRuleKind::value,
            _ => JsonParserRuleKind::Unknown,
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
    fn project_push_children_JsonParser<'p>(
        output: &crate::runtime::tape::Tape<JsonParser>,
        input: &'p str,
        offset: u32,
        out: &mut ::std::vec::Vec<JsonParserValue<'p>>,
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
                project_push_children_JsonParser(output, input, __child.offset().0, out);
            }
        } else {
            out.push(project_frame_JsonParser(output, input, offset));
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
    fn project_frame_JsonParser<'p>(
        output: &crate::runtime::tape::Tape<JsonParser>,
        input: &'p str,
        offset: u32,
    ) -> JsonParserValue<'p> {
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
        match project_rule_kind_JsonParser(__rec.kind(), __rec.variant_idx()) {
            JsonParserRuleKind::null => {
                let v: u8 = output
                    .frame(offset)
                    .and_then(|f| output.payload_for(f))
                    .and_then(|p| p.as_u32())
                    .map(|v| v as u8)
                    .unwrap_or_else(|| {
                        (&input[__rec.span_lo as usize..__rec.span_hi as usize])
                            .parse::<u8>()
                            .unwrap_or(0)
                    });
                JsonParserValue::null(v)
            }
            JsonParserRuleKind::bool => {
                let proj = materialize_projection_bool_JsonParser(output, input, offset)
                    .unwrap_or_else(|| {
                        ::core::panic!(
                            "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                            "bool", offset,
                        );
                    });
                JsonParserValue::bool(proj)
            }
            JsonParserRuleKind::number => {
                let v: f64 = output
                    .frame(offset)
                    .and_then(|f| output.payload_for(f))
                    .and_then(|p| p.as_f64())
                    .unwrap_or_else(|| {
                        (&input[__rec.span_lo as usize..__rec.span_hi as usize])
                            .parse::<f64>()
                            .unwrap_or(0.0)
                    });
                JsonParserValue::number(v)
            }
            JsonParserRuleKind::string => {
                let proj = materialize_projection_string_JsonParser(
                        output,
                        input,
                        offset,
                    )
                    .unwrap_or_else(|| {
                        ::core::panic!(
                            "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                            "string", offset,
                        );
                    });
                JsonParserValue::string(proj)
            }
            JsonParserRuleKind::array => {
                let span = &input[__rec.span_lo as usize..__rec.span_hi as usize];
                JsonParserValue::array(span)
            }
            JsonParserRuleKind::pair => {
                let mut children: ::std::vec::Vec<JsonParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_JsonParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                JsonParserValue::pair(children)
            }
            JsonParserRuleKind::object => {
                let span = &input[__rec.span_lo as usize..__rec.span_hi as usize];
                JsonParserValue::object(span)
            }
            JsonParserRuleKind::value => {
                let mut children: ::std::vec::Vec<JsonParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_JsonParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                JsonParserValue::value(children)
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
    fn project_value_JsonParser<'p>(
        output: &crate::runtime::tape::Tape<JsonParser>,
        input: &'p str,
    ) -> JsonParserValue<'p> {
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
        project_frame_JsonParser(output, input, __cur_off)
    }
    impl crate::runtime::ValueRoot for JsonParser {
        type Value<'p> = JsonParserValue<'p>;
        #[inline]
        fn project_value_output<'p>(
            output: &crate::runtime::tape::Tape<JsonParser>,
            input: &'p str,
        ) -> Self::Value<'p>
        where
            Self: 'p,
        {
            project_value_JsonParser(output, input)
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
        view: JsonParserNodeView<'p>,
        path: crate::runtime::Path<'_>,
    ) -> ::core::option::Option<JsonParserNodeView<'p>> {
        let cur_input = view.input();
        let mut cur = view;
        for seg in path.iter() {
            match seg {
                crate::runtime::PathSegment::Field(key) => {
                    match cur.rule_kind() {
                        JsonParserRuleKind::value => {
                            let parent = cur.cursor();
                            let (_, parent_end) = parent.span();
                            let mut iter = parent.bounded_lookahead(parent_end);
                            let mut hit: ::core::option::Option<
                                JsonParserNodeView<'p>,
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
                                        JsonParserNodeView::from_cursor(v_cur, cur_input),
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
    impl crate::runtime::PathQuery<&'static str> for JsonParser {
        #[inline]
        fn query<'p>(
            view: Self::View<'p>,
            path: crate::runtime::Path<'_>,
        ) -> ::core::option::Option<&'static str>
        where
            Self: 'p,
        {
            let node = JsonParserNodeView::from_cursor(view.cursor(), view.input());
            __path_walk(node, path)?;
            ::core::option::Option::None
        }
    }
    impl crate::runtime::PathQuery<f64> for JsonParser {
        #[inline]
        fn query<'p>(
            view: Self::View<'p>,
            path: crate::runtime::Path<'_>,
        ) -> ::core::option::Option<f64>
        where
            Self: 'p,
        {
            let node = JsonParserNodeView::from_cursor(view.cursor(), view.input());
            let hit = __path_walk(node, path)?;
            let tape = hit.cursor().tape();
            let rec = hit.cursor().record();
            if let ::core::option::Option::Some(v) = tape.payload_f64(rec) {
                return ::core::option::Option::Some(v);
            }
            hit.span_text().parse::<f64>().ok()
        }
    }
    impl crate::runtime::PathQuery<bool> for JsonParser {
        #[inline]
        fn query<'p>(
            view: Self::View<'p>,
            path: crate::runtime::Path<'_>,
        ) -> ::core::option::Option<bool>
        where
            Self: 'p,
        {
            let node = JsonParserNodeView::from_cursor(view.cursor(), view.input());
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
    pub fn materialize_projection_bool_JsonParser<'p>(
        output: &crate::runtime::tape::Tape<JsonParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<JsonParserBoolProjection> {
        let _ = input;
        let frame = output.frame(offset)?;
        let __tape = output;
        let __tape_rec = __tape.try_get(crate::runtime::tape::TapeOffset(offset))?;
        let __bytes = __tape.payload_bytes(__tape_rec, 1)?;
        let field_0: bool = {
            let __b = *__bytes.get(0)?;
            let _ = 1;
            __b != 0
        };
        ::core::option::Option::Some(JsonParserBoolProjection {
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
    pub fn materialize_projection_string_JsonParser<'p>(
        output: &crate::runtime::tape::Tape<JsonParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<JsonParserStringProjection> {
        let _ = input;
        let frame = output.frame(offset)?;
        let __bytes: &[u8] = &[];
        let _ = __bytes;
        let field_0: (u32, u32) = (frame.span_lo, frame.span_hi);
        ::core::option::Option::Some(JsonParserStringProjection {
            field_0,
        })
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
                                                                if !Self::__value_prettify(state, __builder) {
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
                                let __ows13 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                let __ows14 = state.offset;
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b':')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b':');
                                };
                                __builder.text_inline_ws(&state.src[__ows13..__ows14]);
                                let __ows15 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows15..state.offset]);
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
                                    let __pretty_cp27 = state.offset;
                                    let __pretty_bcp28 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            let __ows25 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder.text_inline_ws(&state.src[__ows25..state.offset]);
                                            {
                                                let mut __rep_count23 = 0usize;
                                                while __rep_count23 < 4294967295 {
                                                    let __rep_cp24 = state.offset;
                                                    if !{
                                                        let __pretty_cp21 = state.offset;
                                                        let __pretty_bcp22 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            {
                                                                if !Self::__pair_prettify(state, __builder) {
                                                                    return false;
                                                                }
                                                                {
                                                                    let _ = {
                                                                        let __pretty_cp19 = state.offset;
                                                                        let __pretty_bcp20 = __builder.checkpoint();
                                                                        let __ok = (|| -> bool {
                                                                            {
                                                                                let __ows16 = state.offset;
                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                let __ows17 = state.offset;
                                                                                {
                                                                                    if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                                    {
                                                                                        return false;
                                                                                    }
                                                                                    state.offset += 1;
                                                                                    __builder.char(b',');
                                                                                };
                                                                                __builder.text_inline_ws(&state.src[__ows16..__ows17]);
                                                                                let __ows18 = state.offset;
                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                __builder.text_inline_ws(&state.src[__ows18..state.offset]);
                                                                            };
                                                                            true
                                                                        })();
                                                                        if !__ok {
                                                                            state.offset = __pretty_cp19;
                                                                            __builder.restore(__pretty_bcp20);
                                                                        }
                                                                        __ok
                                                                    };
                                                                    true
                                                                };
                                                            };
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp21;
                                                            __builder.restore(__pretty_bcp22);
                                                        }
                                                        __ok
                                                    } {
                                                        state.offset = __rep_cp24;
                                                        break;
                                                    }
                                                    if state.offset == __rep_cp24 {
                                                        break;
                                                    }
                                                    __rep_count23 += 1;
                                                }
                                            };
                                            let __ows26 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder.text_inline_ws(&state.src[__ows26..state.offset]);
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp27;
                                        __builder.restore(__pretty_bcp28);
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
        pub fn serialize_null<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: JsonParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_bool<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: JsonParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_number<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: JsonParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_string<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: JsonParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_array<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: JsonParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_pair<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: JsonParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_object<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: JsonParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_value<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: JsonParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        fn __dispatch_serialize<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: JsonParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            match __v.variant_idx() {
                0u8 => {
                    Self::serialize_null(__v, __ser);
                }
                1u8 => {
                    Self::serialize_bool(__v, __ser);
                }
                2u8 => {
                    Self::serialize_number(__v, __ser);
                }
                3u8 => {
                    Self::serialize_string(__v, __ser);
                }
                4u8 => {
                    Self::serialize_array(__v, __ser);
                }
                5u8 => {
                    Self::serialize_pair(__v, __ser);
                }
                6u8 => {
                    Self::serialize_object(__v, __ser);
                }
                7u8 => {
                    Self::serialize_value(__v, __ser);
                }
                _ => {
                    __ser.text(__v.span_text());
                }
            }
        }
        pub fn serialize_compact<'a>(__v: JsonParserNodeView<'a>) -> String {
            let mut __ser = ::bbnf_ser::StringSerializer::new();
            Self::serialize_value(__v, &mut __ser);
            __ser.finish()
        }
        pub fn serialize<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: JsonParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            Self::serialize_value(__v, __ser);
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
            ::core::result::Result::Ok(builder.finalise())
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
pub use __jsonparser_emit_impl::*;
