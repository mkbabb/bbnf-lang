//! AUTO-GENERATED from `[workspace.metadata.bbnf.grammars]` — do not edit manually.
//! Regenerate: cargo xtask regen --grammar css_pretty

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

pub struct CssPrettyParser;
mod __cssprettyparser_emit_impl {
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
    pub const GRAMMAR_CssPrettyParser: [&'static str; 1usize] = [
        include_str!(
            concat!(env!("CARGO_MANIFEST_DIR"), "/../../grammar/css/pretty.bbnf")
        ),
    ];
    static __GRAMMAR_PROFILE_ALPHABET: [u8; 6usize] = [33, 44, 58, 59, 123, 125];
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
    static __PHF_CssPrettyParser_11_KW: [&[u8]; 4usize] = [
        b"@font-face",
        b"@import",
        b"@media",
        b"@supports",
    ];
    /// Per-entry branch discriminant — parallel to [`#kw_ident`].
    /// Entry `i`'s keyword bytes at `#kw_ident[i]` route to the
    /// branch with discriminant `#idx_ident[i]`.
    static __PHF_CssPrettyParser_11_IDX: [u8; 4usize] = [2, 3, 0, 1];
    /// AW-III.W6.2 — dispatch the mined keyword table for rule
    /// `#rule_id`.
    ///
    /// Returns `Some(branch_idx)` when `bytes` matches a mined
    /// keyword, `None` otherwise. Called from the walker's
    /// AltLinear / ClassifyByte arm to short-circuit the branch
    /// scan to a single binary search.
    #[allow(dead_code)]
    #[inline]
    fn __phf_CssPrettyParser_dispatch_11(bytes: &[u8]) -> ::core::option::Option<u8> {
        match __PHF_CssPrettyParser_11_KW.binary_search(&bytes) {
            ::core::result::Result::Ok(idx) => {
                ::core::option::Option::Some(__PHF_CssPrettyParser_11_IDX[idx])
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
    static __DTA_REGEX_1: &str = "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*";
    static __DTA_REGEX_4: &str = "[a-zA-Z_][\\w-]*|--[\\w-]+|-[a-zA-Z][\\w-]*";
    static __DTA_REGEX_8: &str = "[^;{}!,]+";
    static __DTA_REGEX_38: &str = "[^{};]+";
    static __DTA_REGEX_43: &str = "[^{]+";
    static __DTA_REGEX_66: &str = "@[a-zA-Z][\\w-]*";
    static __DTA_REGEX_67: &str = "[^;{}]*";
    /// AY.W4.3 — per-pattern (LAST-byte-set lo, hi) packed
    /// `CharSet128` tuples. `(0, 0)` means narrowing is
    /// disabled for that pattern (suffix not deterministic).
    ///
    /// The adapter consults this when invoked: if the pattern's
    /// entry is non-zero AND the input slice from `pos` does not
    /// contain any byte in the LAST set, the regex cannot
    /// complete a match — skip the DFA walk entirely.
    #[allow(dead_code)]
    pub(crate) const __REGEX_LAST_BYTE_SET_CssPrettyParser: [(u64, u64); 7] = [
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
    ];
    #[inline]
    #[cold]
    fn __regex_scan_CssPrettyParser(
        pattern: &str,
        input: &[u8],
        pos: usize,
    ) -> ::core::option::Option<u32> {
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_1.as_ptr())
            || pattern == __DTA_REGEX_1
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_CssPrettyParser[0];
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
                                9 | 10 | 11 | 12 | 13 | 32 => __dfa_state = 0,
                                47 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                42 => __dfa_state = 3,
                                _ => break,
                            }
                        }
                        2 => {
                            match b {
                                9 | 10 | 11 | 12 | 13 | 32 => __dfa_state = 2,
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 14 | 15 | 16 | 17 | 18
                                | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30
                                | 31 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 | 41 | 43 | 44
                                | 45 | 46 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57
                                | 58 | 59 | 60 | 61 | 62 | 63 | 64 | 65 | 66 | 67 | 68 | 69
                                | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77 | 78 | 79 | 80 | 81
                                | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89 | 90 | 91 | 92 | 93
                                | 94 | 95 | 96 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104
                                | 105 | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114
                                | 115 | 116 | 117 | 118 | 119 | 120 | 121 | 122 | 123 | 124
                                | 125 | 126 | 127 | 128 | 129 | 130 | 131 | 132 | 133 | 134
                                | 135 | 136 | 137 | 138 | 139 | 140 | 141 | 142 | 143 | 144
                                | 145 | 146 | 147 | 148 | 149 | 150 | 151 | 152 | 153 | 154
                                | 155 | 156 | 157 | 158 | 159 | 160 | 161 | 162 | 163 | 164
                                | 165 | 166 | 167 | 168 | 169 | 170 | 171 | 172 | 173 | 174
                                | 175 | 176 | 177 | 178 | 179 | 180 | 181 | 182 | 183 | 184
                                | 185 | 186 | 187 | 188 | 189 | 190 | 191 | 192 | 193 | 194
                                | 195 | 196 | 197 | 198 | 199 | 200 | 201 | 202 | 203 | 204
                                | 205 | 206 | 207 | 208 | 209 | 210 | 211 | 212 | 213 | 214
                                | 215 | 216 | 217 | 218 | 219 | 220 | 221 | 222 | 223 | 224
                                | 225 | 226 | 227 | 228 | 229 | 230 | 231 | 232 | 233 | 234
                                | 235 | 236 | 237 | 238 | 239 | 240 | 241 | 242 | 243 | 244
                                | 245 | 246 | 247 | 248 | 249 | 250 | 251 | 252 | 253 | 254
                                | 255 => __dfa_state = 3,
                                42 => __dfa_state = 4,
                                47 => __dfa_state = 6,
                                _ => break,
                            }
                        }
                        3 => {
                            match b {
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 43 | 44 | 45 | 46 | 47 | 48 | 49 | 50
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
                                | 249 | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 3,
                                42 => __dfa_state = 4,
                                _ => break,
                            }
                        }
                        4 => {
                            match b {
                                47 => __dfa_state = 0,
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 43 | 44 | 45 | 46 | 48 | 49 | 50 | 51
                                | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63
                                | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75
                                | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87
                                | 88 | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 | 98 | 99
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
                                | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 3,
                                42 => __dfa_state = 5,
                                _ => break,
                            }
                        }
                        5 => {
                            match b {
                                47 => __dfa_state = 2,
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 43 | 44 | 45 | 46 | 48 | 49 | 50 | 51
                                | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63
                                | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75
                                | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87
                                | 88 | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 | 98 | 99
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
                                | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 3,
                                42 => __dfa_state = 5,
                                _ => break,
                            }
                        }
                        6 => {
                            match b {
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 43 | 44 | 45 | 46 | 47 | 48 | 49 | 50
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
                                | 249 | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 3,
                                42 => __dfa_state = 5,
                                _ => break,
                            }
                        }
                        _ => unsafe { ::core::hint::unreachable_unchecked() }
                    }
                    __dfa_p += 1;
                    match __dfa_state {
                        0 | 2 => {
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_4.as_ptr())
            || pattern == __DTA_REGEX_4
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_CssPrettyParser[1];
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
                                45 => __dfa_state = 3,
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
                        2 => {
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
                        3 => {
                            match b {
                                65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76
                                | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88
                                | 89 | 90 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
                                | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115
                                | 116 | 117 | 118 | 119 | 120 | 121 | 122 => __dfa_state = 1,
                                45 => __dfa_state = 2,
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_8.as_ptr())
            || pattern == __DTA_REGEX_8
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_CssPrettyParser[2];
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
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 34 | 35 | 36 | 37 | 38
                                | 39 | 40 | 41 | 42 | 43 | 45 | 46 | 47 | 48 | 49 | 50 | 51
                                | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 60 | 61 | 62 | 63 | 64
                                | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76
                                | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88
                                | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 | 98 | 99 | 100
                                | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108 | 109 | 110
                                | 111 | 112 | 113 | 114 | 115 | 116 | 117 | 118 | 119 | 120
                                | 121 | 122 | 124 | 126 | 127 | 128 | 129 | 130 | 131 | 132
                                | 133 | 134 | 135 | 136 | 137 | 138 | 139 | 140 | 141 | 142
                                | 143 | 144 | 145 | 146 | 147 | 148 | 149 | 150 | 151 | 152
                                | 153 | 154 | 155 | 156 | 157 | 158 | 159 | 160 | 161 | 162
                                | 163 | 164 | 165 | 166 | 167 | 168 | 169 | 170 | 171 | 172
                                | 173 | 174 | 175 | 176 | 177 | 178 | 179 | 180 | 181 | 182
                                | 183 | 184 | 185 | 186 | 187 | 188 | 189 | 190 | 191 | 192
                                | 193 | 194 | 195 | 196 | 197 | 198 | 199 | 200 | 201 | 202
                                | 203 | 204 | 205 | 206 | 207 | 208 | 209 | 210 | 211 | 212
                                | 213 | 214 | 215 | 216 | 217 | 218 | 219 | 220 | 221 | 222
                                | 223 | 224 | 225 | 226 | 227 | 228 | 229 | 230 | 231 | 232
                                | 233 | 234 | 235 | 236 | 237 | 238 | 239 | 240 | 241 | 242
                                | 243 | 244 | 245 | 246 | 247 | 248 | 249 | 250 | 251 | 252
                                | 253 | 254 | 255 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 34 | 35 | 36 | 37 | 38
                                | 39 | 40 | 41 | 42 | 43 | 45 | 46 | 47 | 48 | 49 | 50 | 51
                                | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 60 | 61 | 62 | 63 | 64
                                | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76
                                | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88
                                | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 | 98 | 99 | 100
                                | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108 | 109 | 110
                                | 111 | 112 | 113 | 114 | 115 | 116 | 117 | 118 | 119 | 120
                                | 121 | 122 | 124 | 126 | 127 | 128 | 129 | 130 | 131 | 132
                                | 133 | 134 | 135 | 136 | 137 | 138 | 139 | 140 | 141 | 142
                                | 143 | 144 | 145 | 146 | 147 | 148 | 149 | 150 | 151 | 152
                                | 153 | 154 | 155 | 156 | 157 | 158 | 159 | 160 | 161 | 162
                                | 163 | 164 | 165 | 166 | 167 | 168 | 169 | 170 | 171 | 172
                                | 173 | 174 | 175 | 176 | 177 | 178 | 179 | 180 | 181 | 182
                                | 183 | 184 | 185 | 186 | 187 | 188 | 189 | 190 | 191 | 192
                                | 193 | 194 | 195 | 196 | 197 | 198 | 199 | 200 | 201 | 202
                                | 203 | 204 | 205 | 206 | 207 | 208 | 209 | 210 | 211 | 212
                                | 213 | 214 | 215 | 216 | 217 | 218 | 219 | 220 | 221 | 222
                                | 223 | 224 | 225 | 226 | 227 | 228 | 229 | 230 | 231 | 232
                                | 233 | 234 | 235 | 236 | 237 | 238 | 239 | 240 | 241 | 242
                                | 243 | 244 | 245 | 246 | 247 | 248 | 249 | 250 | 251 | 252
                                | 253 | 254 | 255 => __dfa_state = 1,
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_38.as_ptr())
            || pattern == __DTA_REGEX_38
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_CssPrettyParser[3];
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
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49
                                | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 60 | 61 | 62
                                | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74
                                | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86
                                | 87 | 88 | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 | 98
                                | 99 | 100 | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108
                                | 109 | 110 | 111 | 112 | 113 | 114 | 115 | 116 | 117 | 118
                                | 119 | 120 | 121 | 122 | 124 | 126 | 127 | 128 | 129 | 130
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
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49
                                | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 60 | 61 | 62
                                | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74
                                | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86
                                | 87 | 88 | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 | 98
                                | 99 | 100 | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108
                                | 109 | 110 | 111 | 112 | 113 | 114 | 115 | 116 | 117 | 118
                                | 119 | 120 | 121 | 122 | 124 | 126 | 127 | 128 | 129 | 130
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_43.as_ptr())
            || pattern == __DTA_REGEX_43
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_CssPrettyParser[4];
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
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49
                                | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61
                                | 62 | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73
                                | 74 | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85
                                | 86 | 87 | 88 | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97
                                | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105 | 106 | 107
                                | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115 | 116 | 117
                                | 118 | 119 | 120 | 121 | 122 | 124 | 125 | 126 | 127 | 128
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
                                | 249 | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49
                                | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61
                                | 62 | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73
                                | 74 | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85
                                | 86 | 87 | 88 | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97
                                | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105 | 106 | 107
                                | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115 | 116 | 117
                                | 118 | 119 | 120 | 121 | 122 | 124 | 125 | 126 | 127 | 128
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
                                | 249 | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 1,
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_66.as_ptr())
            || pattern == __DTA_REGEX_66
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_CssPrettyParser[5];
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
                                64 => __dfa_state = 2,
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
                        2 => {
                            match b {
                                65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76
                                | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88
                                | 89 | 90 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_67.as_ptr())
            || pattern == __DTA_REGEX_67
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_CssPrettyParser[6];
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
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49
                                | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 60 | 61 | 62
                                | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74
                                | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86
                                | 87 | 88 | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 | 98
                                | 99 | 100 | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108
                                | 109 | 110 | 111 | 112 | 113 | 114 | 115 | 116 | 117 | 118
                                | 119 | 120 | 121 | 122 | 124 | 126 | 127 | 128 | 129 | 130
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
                                | 251 | 252 | 253 | 254 | 255 => __dfa_state = 0,
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
    pub(crate) mod __shape_support_CssPrettyParser {
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
        /// Skip whitespace AND `/* ... */` block comments at `*p`,
        /// returning the first non-whitespace, non-comment byte
        /// (or `None` on EOF). Hot-path fast-exit when the next
        /// byte is neither whitespace nor `/`.
        ///
        /// When the next byte is `/`: if followed by `*`, enter
        /// the slow path to consume the comment body; otherwise
        /// return `Some(b'/')` — a bare `/` is a semantic byte.
        #[inline(always)]
        pub fn skip_space(
            input: &[u8],
            p: &mut usize,
            state: &mut ScanState,
        ) -> Option<u8> {
            match input.get(*p) {
                Some(
                    &b,
                ) if b != b' ' && b != b'\t' && b != b'\n' && b != b'\r' && b != b'/' => {
                    Some(b)
                }
                Some(&b'/') if input.get(*p + 1) != Some(&b'*') => Some(b'/'),
                None => None,
                _ => {
                    skip_space_slow(input, p, state);
                    input.get(*p).copied()
                }
            }
        }
        /// Advance `*p` past ASCII whitespace AND `/* ... */` block
        /// comments. The bitmap cache accelerates pure-whitespace
        /// runs; comment detection runs on every iteration where
        /// `*p` points at `/`.
        ///
        /// AY.W4.3 — opens with a CTNS-style structural-index probe
        /// when the grammar mines a non-empty alphabet. On long
        /// whitespace runs that don't intersect comment openers,
        /// the probe jumps directly to the next structural byte
        /// instead of iterating SIMD stripes.
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
                        } else {
                            *p = cache_base as usize + 64;
                            continue;
                        }
                    }
                }
                if *p >= input.len() {
                    return;
                }
                if *p + 64 > input.len() {
                    while let Some(&b) = input.get(*p) {
                        if b != b' ' && b != b'\t' && b != b'\n' && b != b'\r' {
                            break;
                        }
                        *p += 1;
                    }
                } else {
                    let stripe = unsafe {
                        ::core::slice::from_raw_parts(input.as_ptr().add(*p), 64)
                    };
                    let mask = nospace_bitmap_64(stripe);
                    state.nospace_bits = mask;
                    state.nospace_start = *p as isize;
                    if mask != 0 {
                        let bit = mask.trailing_zeros() as usize;
                        *p += bit;
                    } else {
                        *p += 64;
                        continue;
                    }
                }
                if input.get(*p) == Some(&b'/') && input.get(*p + 1) == Some(&b'*') {
                    *p += 2;
                    let len = input.len();
                    loop {
                        if *p + 1 >= len {
                            *p = len;
                            state.nospace_start = -1;
                            return;
                        }
                        let slice = unsafe { input.get_unchecked(*p..len) };
                        match slice.iter().position(|&b| b == b'*') {
                            None => {
                                *p = len;
                                state.nospace_start = -1;
                                return;
                            }
                            Some(rel) => {
                                *p += rel + 1;
                                if input.get(*p) == Some(&b'/') {
                                    *p += 1;
                                    break;
                                }
                            }
                        }
                    }
                    state.nospace_start = -1;
                    continue;
                }
                return;
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
    pub fn parse_flat_CssPrettyParser_important(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.enter_post_order_children();
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [33u8] {
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
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                        0u8,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            let at = *p;
            let end = at + 9usize;
            if input.len() < end
                || input[at..end]
                    != [105u8, 109u8, 112u8, 111u8, 114u8, 116u8, 97u8, 110u8, 116u8]
            {
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
            .begin_compound_post(
                crate::runtime::tape::TapeKind::Seq,
                span_lo,
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
    pub fn parse_flat_CssPrettyParser_declaration(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.enter_post_order_children();
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "[a-zA-Z_][\\w-]*|--[\\w-]+|-[a-zA-Z][\\w-]*",
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
                        1u8,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                        1u8,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [58u8] {
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
                    1u8,
                    0,
                    crate::runtime::tape::PayloadData::None,
                );
        }
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                        1u8,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            let repeat_lo = *p as u32;
            let repeat_child = builder.enter_post_order_children();
            let iter_save_p = *p;
            let iter_save_cols = builder.position();
            let iter_lo = *p as u32;
            let iter_child = builder.enter_post_order_children();
            let opt_attempt: ::core::result::Result<
                (),
                crate::runtime::tape::DtaError,
            > = (|| {
                {
                    let span_lo = *p as u32;
                    let Some(match_len) = __regex_scan_CssPrettyParser(
                        "[^;{}!,]+",
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
                            1u8,
                            0,
                            crate::runtime::tape::PayloadData::None,
                        );
                }
                let repeat_lo = *p as u32;
                let repeat_child = builder.enter_post_order_children();
                let mut iter_count: u32 = 0;
                loop {
                    let save_p = *p;
                    let save_cols = builder.position();
                    let iter_lo = *p as u32;
                    let iter_child = builder.enter_post_order_children();
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
                                1u8,
                                0,
                                crate::runtime::tape::PayloadData::None,
                            );
                        {
                            let span_lo = *p as u32;
                            let Some(match_len) = __regex_scan_CssPrettyParser(
                                "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                                    1u8,
                                    0,
                                    crate::runtime::tape::PayloadData::None,
                                );
                        }
                        {
                            let span_lo = *p as u32;
                            let Some(match_len) = __regex_scan_CssPrettyParser(
                                "[^;{}!,]+",
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
                                    1u8,
                                    0,
                                    crate::runtime::tape::PayloadData::None,
                                );
                        }
                        Ok(())
                    })();
                    if attempt.is_err() {
                        *p = save_p;
                        builder.rollback_to(save_cols);
                        builder.exit_post_order_children();
                        break;
                    }
                    if *p == save_p {
                        builder.rollback_to(save_cols);
                        builder.exit_post_order_children();
                        break;
                    }
                    let iter_hi = *p as u32;
                    let __iter_off = builder
                        .begin_compound_post(
                            crate::runtime::tape::TapeKind::Seq,
                            iter_lo,
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
                    builder.exit_post_order_children();
                    return Err(crate::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    });
                }
                let repeat_hi = *p as u32;
                let __repeat_off = builder
                    .begin_compound_post(
                        crate::runtime::tape::TapeKind::Repeat,
                        repeat_lo,
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
                Ok(())
            })();
            let matched = opt_attempt.is_ok();
            if !matched {
                *p = iter_save_p;
                builder.rollback_to(iter_save_cols);
                builder.exit_post_order_children();
            } else {
                let iter_hi = *p as u32;
                let __iter_off = builder
                    .begin_compound_post(
                        crate::runtime::tape::TapeKind::Seq,
                        iter_lo,
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
            }
            let repeat_hi = *p as u32;
            let __repeat_off = builder
                .begin_compound_post(
                    crate::runtime::tape::TapeKind::Repeat,
                    repeat_lo,
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
        {
            let repeat_lo = *p as u32;
            let repeat_child = builder.enter_post_order_children();
            let iter_save_p = *p;
            let iter_save_cols = builder.position();
            let iter_lo = *p as u32;
            let iter_child = builder.enter_post_order_children();
            let opt_attempt: ::core::result::Result<
                (),
                crate::runtime::tape::DtaError,
            > = (|| {
                let _ = ({
                    let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                    parse_flat_CssPrettyParser_important(input, p, state, builder)
                })?;
                Ok(())
            })();
            let matched = opt_attempt.is_ok();
            if !matched {
                *p = iter_save_p;
                builder.rollback_to(iter_save_cols);
                builder.exit_post_order_children();
            } else {
                let iter_hi = *p as u32;
                let __iter_off = builder
                    .begin_compound_post(
                        crate::runtime::tape::TapeKind::Seq,
                        iter_lo,
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
            }
            let repeat_hi = *p as u32;
            let __repeat_off = builder
                .begin_compound_post(
                    crate::runtime::tape::TapeKind::Repeat,
                    repeat_lo,
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
        {
            let repeat_lo = *p as u32;
            let repeat_child = builder.enter_post_order_children();
            let iter_save_p = *p;
            let iter_save_cols = builder.position();
            let iter_lo = *p as u32;
            let iter_child = builder.enter_post_order_children();
            let opt_attempt: ::core::result::Result<
                (),
                crate::runtime::tape::DtaError,
            > = (|| {
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [59u8] {
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
                        1u8,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
                Ok(())
            })();
            let matched = opt_attempt.is_ok();
            if !matched {
                *p = iter_save_p;
                builder.rollback_to(iter_save_cols);
                builder.exit_post_order_children();
            } else {
                let iter_hi = *p as u32;
                let __iter_off = builder
                    .begin_compound_post(
                        crate::runtime::tape::TapeKind::Seq,
                        iter_lo,
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
            }
            let repeat_hi = *p as u32;
            let __repeat_off = builder
                .begin_compound_post(
                    crate::runtime::tape::TapeKind::Repeat,
                    repeat_lo,
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
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                        1u8,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
            }
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound_post(
                crate::runtime::tape::TapeKind::Seq,
                span_lo,
                1u8,
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
    pub fn parse_flat_CssPrettyParser_blockContent(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.enter_post_order_children();
        {
            let repeat_lo = *p as u32;
            let repeat_child = builder.enter_post_order_children();
            let mut iter_count: u32 = 0;
            loop {
                let save_p = *p;
                let save_cols = builder.position();
                let iter_lo = *p as u32;
                let iter_child = builder.enter_post_order_children();
                let attempt = (|| -> ::core::result::Result<
                    (),
                    crate::runtime::tape::DtaError,
                > {
                    {
                        let span_lo = *p as u32;
                        let Some(match_len) = __regex_scan_CssPrettyParser(
                            "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                                2u8,
                                0,
                                crate::runtime::tape::PayloadData::None,
                            );
                    }
                    {
                        let first = __shape_support_CssPrettyParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                                offset: *p as u32,
                            })?;
                        let alt_lo = *p as u32;
                        let alt_child = builder.enter_post_order_children();
                        'try_branches: loop {
                            match first {
                                _ => {}
                            }
                            {
                                let attempt_p = *p;
                                let attempt_len = builder.position();
                                match {
                                    let _ = __shape_support_CssPrettyParser::skip_space(
                                        input,
                                        p,
                                        state,
                                    );
                                    parse_flat_CssPrettyParser_declaration(
                                        input,
                                        p,
                                        state,
                                        builder,
                                    )
                                } {
                                    Ok(_) => break 'try_branches,
                                    Err(_) => {
                                        *p = attempt_p;
                                        builder.rollback_to(attempt_len);
                                    }
                                }
                            }
                            {
                                let attempt_p = *p;
                                let attempt_len = builder.position();
                                match {
                                    parse_wrap_CssPrettyParser_ruleItem(
                                        input,
                                        p,
                                        state,
                                        builder,
                                    )
                                } {
                                    Ok(_) => break 'try_branches,
                                    Err(_) => {
                                        *p = attempt_p;
                                        builder.rollback_to(attempt_len);
                                    }
                                }
                            }
                            builder.exit_post_order_children();
                            return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                                offset: *p as u32,
                                failing_state: crate::runtime::tape::DtaStateId::NONE,
                                failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                            });
                        }
                        let alt_hi = *p as u32;
                        let __alt_off = builder
                            .begin_compound_post(
                                crate::runtime::tape::TapeKind::Alt,
                                alt_lo,
                                2u8,
                                0u8,
                                0u16,
                            );
                        builder
                            .end_compound_post_order(
                                __alt_off,
                                alt_hi,
                                crate::runtime::tape::TapeOffset(alt_child),
                            );
                    }
                    Ok(())
                })();
                if attempt.is_err() {
                    *p = save_p;
                    builder.rollback_to(save_cols);
                    builder.exit_post_order_children();
                    break;
                }
                if *p == save_p {
                    builder.rollback_to(save_cols);
                    builder.exit_post_order_children();
                    break;
                }
                let iter_hi = *p as u32;
                let __iter_off = builder
                    .begin_compound_post(
                        crate::runtime::tape::TapeKind::Seq,
                        iter_lo,
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
                builder.exit_post_order_children();
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            let repeat_hi = *p as u32;
            let __repeat_off = builder
                .begin_compound_post(
                    crate::runtime::tape::TapeKind::Repeat,
                    repeat_lo,
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
            .begin_compound_post(
                crate::runtime::tape::TapeKind::Seq,
                span_lo,
                2u8,
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
    pub fn parse_flat_CssPrettyParser_ruleBlock(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.enter_post_order_children();
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [123u8] {
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
                    3u8,
                    0,
                    crate::runtime::tape::PayloadData::None,
                );
        }
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
        }
        {
            let _ = ({
                let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                parse_flat_CssPrettyParser_blockContent(input, p, state, builder)
            })?;
        }
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
        }
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [125u8] {
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
                    3u8,
                    0,
                    crate::runtime::tape::PayloadData::None,
                );
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound_post(
                crate::runtime::tape::TapeKind::Seq,
                span_lo,
                3u8,
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
    pub fn parse_flat_CssPrettyParser_qualifiedRule(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.enter_post_order_children();
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_CssPrettyParser("[^{};]+", input, *p)
                else {
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
                        4u8,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                        4u8,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            let _ = ({
                let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                parse_flat_CssPrettyParser_ruleBlock(input, p, state, builder)
            })?;
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound_post(
                crate::runtime::tape::TapeKind::Seq,
                span_lo,
                4u8,
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
    pub fn parse_flat_CssPrettyParser_mediaRule(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.enter_post_order_children();
        {
            let at = *p;
            let end = at + 6usize;
            if input.len() < end
                || input[at..end] != [64u8, 109u8, 101u8, 100u8, 105u8, 97u8]
            {
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
                    5u8,
                    0,
                    crate::runtime::tape::PayloadData::None,
                );
        }
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_CssPrettyParser("[^{]+", input, *p)
                else {
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
                        5u8,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            let _ = ({
                let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                parse_flat_CssPrettyParser_ruleBlock(input, p, state, builder)
            })?;
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound_post(
                crate::runtime::tape::TapeKind::Seq,
                span_lo,
                5u8,
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
    pub fn parse_flat_CssPrettyParser_supportsRule(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.enter_post_order_children();
        {
            let at = *p;
            let end = at + 9usize;
            if input.len() < end
                || input[at..end]
                    != [64u8, 115u8, 117u8, 112u8, 112u8, 111u8, 114u8, 116u8, 115u8]
            {
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
                    6u8,
                    0,
                    crate::runtime::tape::PayloadData::None,
                );
        }
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_CssPrettyParser("[^{]+", input, *p)
                else {
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
                        6u8,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            let _ = ({
                let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                parse_flat_CssPrettyParser_ruleBlock(input, p, state, builder)
            })?;
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound_post(
                crate::runtime::tape::TapeKind::Seq,
                span_lo,
                6u8,
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
    pub fn parse_flat_CssPrettyParser_fontFaceRule(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.enter_post_order_children();
        {
            let at = *p;
            let end = at + 10usize;
            if input.len() < end
                || input[at..end]
                    != [64u8, 102u8, 111u8, 110u8, 116u8, 45u8, 102u8, 97u8, 99u8, 101u8]
            {
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
                    7u8,
                    0,
                    crate::runtime::tape::PayloadData::None,
                );
        }
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                        7u8,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            let _ = ({
                let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                parse_flat_CssPrettyParser_ruleBlock(input, p, state, builder)
            })?;
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound_post(
                crate::runtime::tape::TapeKind::Seq,
                span_lo,
                7u8,
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
    pub fn parse_flat_CssPrettyParser_importRule(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.enter_post_order_children();
        {
            let at = *p;
            let end = at + 7usize;
            if input.len() < end
                || input[at..end] != [64u8, 105u8, 109u8, 112u8, 111u8, 114u8, 116u8]
            {
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
                    8u8,
                    0,
                    crate::runtime::tape::PayloadData::None,
                );
        }
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "[^;{}!,]+",
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
                        8u8,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            let repeat_lo = *p as u32;
            let repeat_child = builder.enter_post_order_children();
            let mut iter_count: u32 = 0;
            loop {
                let save_p = *p;
                let save_cols = builder.position();
                let iter_lo = *p as u32;
                let iter_child = builder.enter_post_order_children();
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
                            8u8,
                            0,
                            crate::runtime::tape::PayloadData::None,
                        );
                    {
                        let span_lo = *p as u32;
                        let Some(match_len) = __regex_scan_CssPrettyParser(
                            "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                                8u8,
                                0,
                                crate::runtime::tape::PayloadData::None,
                            );
                    }
                    {
                        let span_lo = *p as u32;
                        let Some(match_len) = __regex_scan_CssPrettyParser(
                            "[^;{}!,]+",
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
                                8u8,
                                0,
                                crate::runtime::tape::PayloadData::None,
                            );
                    }
                    Ok(())
                })();
                if attempt.is_err() {
                    *p = save_p;
                    builder.rollback_to(save_cols);
                    builder.exit_post_order_children();
                    break;
                }
                if *p == save_p {
                    builder.rollback_to(save_cols);
                    builder.exit_post_order_children();
                    break;
                }
                let iter_hi = *p as u32;
                let __iter_off = builder
                    .begin_compound_post(
                        crate::runtime::tape::TapeKind::Seq,
                        iter_lo,
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
                builder.exit_post_order_children();
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            let repeat_hi = *p as u32;
            let __repeat_off = builder
                .begin_compound_post(
                    crate::runtime::tape::TapeKind::Repeat,
                    repeat_lo,
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
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [59u8] {
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
                    8u8,
                    0,
                    crate::runtime::tape::PayloadData::None,
                );
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound_post(
                crate::runtime::tape::TapeKind::Seq,
                span_lo,
                8u8,
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
    /// AW-V.W3.2 — per-grammar Keyword-shape parse function
    /// (Alt of literal-led or Ref-led branches).
    ///
    /// AX.W0a.2.g — admits Ref-led branches whose target
    /// resolves to a literal-prefix body (per `leading_
    /// literal_bytes`). For each first-byte group, each
    /// candidate's full prefix is checked before committing:
    /// Literal branches emit the legacy leaf push;
    /// Ref branches delegate to the target's shape fn via
    /// [`emit_ref_call_tape`], threading `state` through.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_keyword_CssPrettyParser_atRuleBody(
        input: &[u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let _ = state;
        match first_byte {
            59u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [59u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    let off = builder
                        .push_leaf_with(
                            crate::runtime::tape::TapeKind::Span,
                            at as u32,
                            end as u32,
                            9u8,
                            0u8,
                            crate::runtime::tape::PayloadData::None,
                        );
                    return Ok(off);
                }
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            123u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [123u8] {
                    let __ref_save_p = *p;
                    let __ref_save_cols = builder.position();
                    match ({
                        let _ = __shape_support_CssPrettyParser::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_flat_CssPrettyParser_ruleBlock(input, p, state, builder)
                    }) {
                        ::core::result::Result::Ok(__off) => {
                            return ::core::result::Result::Ok(__off);
                        }
                        ::core::result::Result::Err(_) => {
                            *p = __ref_save_p;
                            builder.rollback_to(__ref_save_cols);
                        }
                    }
                }
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            _ => {
                Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                })
            }
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
    pub fn parse_flat_CssPrettyParser_genericAtRule(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.enter_post_order_children();
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "@[a-zA-Z][\\w-]*",
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
                        10u8,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_CssPrettyParser("[^;{}]*", input, *p)
                else {
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
                        10u8,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            let _ = ({
                let __first = __shape_support_CssPrettyParser::skip_space(
                        input,
                        p,
                        state,
                    )
                    .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                        offset: *p as u32,
                    })?;
                parse_keyword_CssPrettyParser_atRuleBody(
                    input,
                    p,
                    __first,
                    state,
                    builder,
                )
            })?;
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound_post(
                crate::runtime::tape::TapeKind::Seq,
                span_lo,
                10u8,
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
    /// AW-V.W4-fix — per-grammar Wrap-shape parse function.
    ///
    /// Transparent dispatcher — skip leading ws, byte-dispatch
    /// to the chosen branch's shape fn, return that shape fn's
    /// offset unchanged. No outer compound emission; the
    /// branch's own shape fn owns the tape record.
    ///
    /// AX.W0a.2.f — compound; see `flat.rs` emission for the
    /// `#[inline]` downgrade rationale (LLVM inline-cycle
    /// collapse vs hard-requirement inliner abort).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_wrap_CssPrettyParser_atRule(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let mut __wrap_chosen_meta: u8 = 0;
        let first = __shape_support_CssPrettyParser::skip_space(input, p, state)
            .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        'try_branches: loop {
            match first {
                64u8 => {
                    {
                        let attempt_p = *p;
                        let attempt_len = builder.position();
                        match parse_flat_CssPrettyParser_mediaRule(
                            input,
                            p,
                            state,
                            builder,
                        ) {
                            Ok(_) => {
                                __wrap_chosen_meta = 0u8;
                                break 'try_branches;
                            }
                            Err(_) => {
                                *p = attempt_p;
                                builder.rollback_to(attempt_len);
                            }
                        }
                    }
                    {
                        let attempt_p = *p;
                        let attempt_len = builder.position();
                        match parse_flat_CssPrettyParser_supportsRule(
                            input,
                            p,
                            state,
                            builder,
                        ) {
                            Ok(_) => {
                                __wrap_chosen_meta = 1u8;
                                break 'try_branches;
                            }
                            Err(_) => {
                                *p = attempt_p;
                                builder.rollback_to(attempt_len);
                            }
                        }
                    }
                    {
                        let attempt_p = *p;
                        let attempt_len = builder.position();
                        match parse_flat_CssPrettyParser_fontFaceRule(
                            input,
                            p,
                            state,
                            builder,
                        ) {
                            Ok(_) => {
                                __wrap_chosen_meta = 2u8;
                                break 'try_branches;
                            }
                            Err(_) => {
                                *p = attempt_p;
                                builder.rollback_to(attempt_len);
                            }
                        }
                    }
                    {
                        let attempt_p = *p;
                        let attempt_len = builder.position();
                        match parse_flat_CssPrettyParser_importRule(
                            input,
                            p,
                            state,
                            builder,
                        ) {
                            Ok(_) => {
                                __wrap_chosen_meta = 3u8;
                                break 'try_branches;
                            }
                            Err(_) => {
                                *p = attempt_p;
                                builder.rollback_to(attempt_len);
                            }
                        }
                    }
                    {
                        let attempt_p = *p;
                        let attempt_len = builder.position();
                        match parse_flat_CssPrettyParser_genericAtRule(
                            input,
                            p,
                            state,
                            builder,
                        ) {
                            Ok(_) => {
                                __wrap_chosen_meta = 4u8;
                                break 'try_branches;
                            }
                            Err(_) => {
                                *p = attempt_p;
                                builder.rollback_to(attempt_len);
                            }
                        }
                    }
                }
                _ => {}
            }
            return ::core::result::Result::Err(crate::runtime::tape::DtaError::Syntax {
                offset: *p as u32,
                failing_state: crate::runtime::tape::DtaStateId::NONE,
                failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
            });
        }
        let _ = __wrap_chosen_meta;
        Ok(crate::runtime::tape::TapeOffset::NONE)
    }
    /// AW-V.W4-fix — per-grammar Wrap-shape parse function.
    ///
    /// Transparent dispatcher — skip leading ws, byte-dispatch
    /// to the chosen branch's shape fn, return that shape fn's
    /// offset unchanged. No outer compound emission; the
    /// branch's own shape fn owns the tape record.
    ///
    /// AX.W0a.2.f — compound; see `flat.rs` emission for the
    /// `#[inline]` downgrade rationale (LLVM inline-cycle
    /// collapse vs hard-requirement inliner abort).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_wrap_CssPrettyParser_ruleItem(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let mut __wrap_chosen_meta: u8 = 0;
        let first = *input
            .get(*p)
            .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        'try_branches: loop {
            match first {
                64u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.position();
                    match parse_wrap_CssPrettyParser_atRule(input, p, state, builder) {
                        Ok(_) => {
                            __wrap_chosen_meta = 1u8;
                            break 'try_branches;
                        }
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
                match parse_flat_CssPrettyParser_qualifiedRule(
                    input,
                    p,
                    state,
                    builder,
                ) {
                    Ok(_) => {
                        __wrap_chosen_meta = 0u8;
                        break 'try_branches;
                    }
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
        let _ = __wrap_chosen_meta;
        Ok(crate::runtime::tape::TapeOffset::NONE)
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
    pub fn parse_flat_CssPrettyParser_ruleList(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.enter_post_order_children();
        {
            let repeat_lo = *p as u32;
            let repeat_child = builder.enter_post_order_children();
            let mut iter_count: u32 = 0;
            loop {
                let save_p = *p;
                let save_cols = builder.position();
                let iter_lo = *p as u32;
                let iter_child = builder.enter_post_order_children();
                let attempt = (|| -> ::core::result::Result<
                    (),
                    crate::runtime::tape::DtaError,
                > {
                    {
                        let span_lo = *p as u32;
                        let Some(match_len) = __regex_scan_CssPrettyParser(
                            "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                                13u8,
                                0,
                                crate::runtime::tape::PayloadData::None,
                            );
                    }
                    let _ = ({
                        parse_wrap_CssPrettyParser_ruleItem(input, p, state, builder)
                    })?;
                    Ok(())
                })();
                if attempt.is_err() {
                    *p = save_p;
                    builder.rollback_to(save_cols);
                    builder.exit_post_order_children();
                    break;
                }
                if *p == save_p {
                    builder.rollback_to(save_cols);
                    builder.exit_post_order_children();
                    break;
                }
                let iter_hi = *p as u32;
                let __iter_off = builder
                    .begin_compound_post(
                        crate::runtime::tape::TapeKind::Seq,
                        iter_lo,
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
                builder.exit_post_order_children();
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            let repeat_hi = *p as u32;
            let __repeat_off = builder
                .begin_compound_post(
                    crate::runtime::tape::TapeKind::Repeat,
                    repeat_lo,
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
            .begin_compound_post(
                crate::runtime::tape::TapeKind::Seq,
                span_lo,
                13u8,
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
    pub fn parse_flat_CssPrettyParser_stylesheet(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.enter_post_order_children();
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                        14u8,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            let _ = ({
                let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                parse_flat_CssPrettyParser_ruleList(input, p, state, builder)
            })?;
        }
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                        14u8,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
            }
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound_post(
                crate::runtime::tape::TapeKind::Seq,
                span_lo,
                14u8,
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
    pub fn parse_flat_visitor_CssPrettyParser_important<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
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
            if input.len() < end || input[at..end] != [33u8] {
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
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
            let end = at + 9usize;
            if input.len() < end
                || input[at..end]
                    != [105u8, 109u8, 112u8, 111u8, 114u8, 116u8, 97u8, 110u8, 116u8]
            {
                return Err(crate::runtime::ParseErr::Syntax {
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
    pub fn parse_flat_visitor_CssPrettyParser_declaration<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        {
            {
                let span_lo = *p;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "[a-zA-Z_][\\w-]*|--[\\w-]+|-[a-zA-Z][\\w-]*",
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
                let span_lo = *p;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
            if input.len() < end || input[at..end] != [58u8] {
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
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
            let save_p = *p;
            let res = (|| -> ::core::result::Result<(), crate::runtime::ParseErr> {
                {
                    let span_lo = *p;
                    let Some(match_len) = __regex_scan_CssPrettyParser(
                        "[^;{}!,]+",
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
                let mut iter_count: u32 = 0;
                loop {
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
                        {
                            let span_lo = *p;
                            let Some(match_len) = __regex_scan_CssPrettyParser(
                                "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                        {
                            let span_lo = *p;
                            let Some(match_len) = __regex_scan_CssPrettyParser(
                                "[^;{}!,]+",
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
                Ok(())
            })();
            if res.is_err() {
                *p = save_p;
            }
        }
        {
            let save_p = *p;
            let res = (|| -> ::core::result::Result<(), crate::runtime::ParseErr> {
                ({
                    let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                    parse_flat_visitor_CssPrettyParser_important(
                        input,
                        p,
                        state,
                        visitor,
                    )
                })?;
                Ok(())
            })();
            if res.is_err() {
                *p = save_p;
            }
        }
        {
            let save_p = *p;
            let res = (|| -> ::core::result::Result<(), crate::runtime::ParseErr> {
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [59u8] {
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
        }
        {
            {
                let span_lo = *p;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
    pub fn parse_flat_visitor_CssPrettyParser_blockContent<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
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
                        let Some(match_len) = __regex_scan_CssPrettyParser(
                            "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                    {
                        let first = __shape_support_CssPrettyParser::skip_space(
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
                                _ => {}
                            }
                            {
                                let attempt_p = *p;
                                match {
                                    let _ = __shape_support_CssPrettyParser::skip_space(
                                        input,
                                        p,
                                        state,
                                    );
                                    parse_flat_visitor_CssPrettyParser_declaration(
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
                            {
                                let attempt_p = *p;
                                match {
                                    let _ = __shape_support_CssPrettyParser::skip_space(
                                        input,
                                        p,
                                        state,
                                    );
                                    parse_wrap_visitor_CssPrettyParser_ruleItem(
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
    pub fn parse_flat_visitor_CssPrettyParser_ruleBlock<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
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
            if input.len() < end || input[at..end] != [123u8] {
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
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                parse_flat_visitor_CssPrettyParser_blockContent(input, p, state, visitor)
            })?;
        }
        {
            {
                let span_lo = *p;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
            if input.len() < end || input[at..end] != [125u8] {
                return Err(crate::runtime::ParseErr::Syntax {
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
    pub fn parse_flat_visitor_CssPrettyParser_qualifiedRule<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        {
            {
                let span_lo = *p;
                let Some(match_len) = __regex_scan_CssPrettyParser("[^{};]+", input, *p)
                else {
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
                let span_lo = *p;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                parse_flat_visitor_CssPrettyParser_ruleBlock(input, p, state, visitor)
            })?;
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
    pub fn parse_flat_visitor_CssPrettyParser_mediaRule<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        {
            let at = *p;
            let end = at + 6usize;
            if input.len() < end
                || input[at..end] != [64u8, 109u8, 101u8, 100u8, 105u8, 97u8]
            {
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
                let Some(match_len) = __regex_scan_CssPrettyParser("[^{]+", input, *p)
                else {
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
                let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                parse_flat_visitor_CssPrettyParser_ruleBlock(input, p, state, visitor)
            })?;
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
    pub fn parse_flat_visitor_CssPrettyParser_supportsRule<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        {
            let at = *p;
            let end = at + 9usize;
            if input.len() < end
                || input[at..end]
                    != [64u8, 115u8, 117u8, 112u8, 112u8, 111u8, 114u8, 116u8, 115u8]
            {
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
                let Some(match_len) = __regex_scan_CssPrettyParser("[^{]+", input, *p)
                else {
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
                let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                parse_flat_visitor_CssPrettyParser_ruleBlock(input, p, state, visitor)
            })?;
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
    pub fn parse_flat_visitor_CssPrettyParser_fontFaceRule<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        {
            let at = *p;
            let end = at + 10usize;
            if input.len() < end
                || input[at..end]
                    != [64u8, 102u8, 111u8, 110u8, 116u8, 45u8, 102u8, 97u8, 99u8, 101u8]
            {
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
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                parse_flat_visitor_CssPrettyParser_ruleBlock(input, p, state, visitor)
            })?;
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
    pub fn parse_flat_visitor_CssPrettyParser_importRule<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        {
            let at = *p;
            let end = at + 7usize;
            if input.len() < end
                || input[at..end] != [64u8, 105u8, 109u8, 112u8, 111u8, 114u8, 116u8]
            {
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
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "[^;{}!,]+",
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
                        let span_lo = *p;
                        let Some(match_len) = __regex_scan_CssPrettyParser(
                            "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                    {
                        let span_lo = *p;
                        let Some(match_len) = __regex_scan_CssPrettyParser(
                            "[^;{}!,]+",
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
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [59u8] {
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: at as u32,
                    rule: None,
                });
            }
            *p = end;
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
    pub fn parse_keyword_visitor_CssPrettyParser_atRuleBody<V>(
        input: &[u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::KeywordVisitor + crate::runtime::tape::ObjectVisitor
            + crate::runtime::tape::ArrayVisitor + crate::runtime::tape::StringVisitor
            + crate::runtime::tape::NumberVisitor,
    {
        let _ = state;
        match first_byte {
            59u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [59u8] {
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
            123u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [123u8] {
                    return ({
                        let _ = __shape_support_CssPrettyParser::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_flat_visitor_CssPrettyParser_ruleBlock(
                            input,
                            p,
                            state,
                            visitor,
                        )
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
    pub fn parse_flat_visitor_CssPrettyParser_genericAtRule<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        {
            {
                let span_lo = *p;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "@[a-zA-Z][\\w-]*",
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
                let span_lo = *p;
                let Some(match_len) = __regex_scan_CssPrettyParser("[^;{}]*", input, *p)
                else {
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
                let __first = __shape_support_CssPrettyParser::skip_space(
                        input,
                        p,
                        state,
                    )
                    .ok_or(crate::runtime::ParseErr::Syntax {
                        offset: *p as u32,
                        rule: None,
                    })?;
                parse_keyword_visitor_CssPrettyParser_atRuleBody(
                    input,
                    p,
                    __first,
                    state,
                    visitor,
                )
            })?;
        }
        Ok(())
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
    pub fn parse_wrap_visitor_CssPrettyParser_atRule<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        let first = __shape_support_CssPrettyParser::skip_space(input, p, state)
            .ok_or(crate::runtime::ParseErr::Syntax {
                offset: *p as u32,
                rule: None,
            })?;
        match first {
            64u8 => {
                parse_flat_visitor_CssPrettyParser_mediaRule(input, p, state, visitor)
            }
            64u8 => {
                parse_flat_visitor_CssPrettyParser_supportsRule(input, p, state, visitor)
            }
            64u8 => {
                parse_flat_visitor_CssPrettyParser_fontFaceRule(input, p, state, visitor)
            }
            64u8 => {
                parse_flat_visitor_CssPrettyParser_importRule(input, p, state, visitor)
            }
            64u8 => {
                parse_flat_visitor_CssPrettyParser_genericAtRule(
                    input,
                    p,
                    state,
                    visitor,
                )
            }
            _ => {
                parse_CssPrettyParser_stylesheet_visitor__value(input, p, state, visitor)
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
    pub fn parse_wrap_visitor_CssPrettyParser_ruleItem<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        let first = __shape_support_CssPrettyParser::skip_space(input, p, state)
            .ok_or(crate::runtime::ParseErr::Syntax {
                offset: *p as u32,
                rule: None,
            })?;
        match first {
            64u8 => parse_wrap_visitor_CssPrettyParser_atRule(input, p, state, visitor),
            _ => {
                parse_CssPrettyParser_stylesheet_visitor__value(input, p, state, visitor)
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
    pub fn parse_flat_visitor_CssPrettyParser_ruleList<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
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
                        let Some(match_len) = __regex_scan_CssPrettyParser(
                            "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                        let _ = __shape_support_CssPrettyParser::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_wrap_visitor_CssPrettyParser_ruleItem(
                            input,
                            p,
                            state,
                            visitor,
                        )
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
    pub fn parse_flat_visitor_CssPrettyParser_stylesheet<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        {
            {
                let span_lo = *p;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
                let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
                parse_flat_visitor_CssPrettyParser_ruleList(input, p, state, visitor)
            })?;
        }
        {
            {
                let span_lo = *p;
                let Some(match_len) = __regex_scan_CssPrettyParser(
                    "(?s)(?:\\s|\\/\\*[^*]*(?:\\*+[^\\/][^*]*)*\\*+\\/)*",
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
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(2),
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
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Sparse,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 4u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Sparse,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(2),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 5u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Empty,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 6u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Empty,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 7u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Empty,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 8u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Empty,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 9u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Sparse,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(2),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 10u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Empty,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 11u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Empty,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 12u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Sparse,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(2),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 13u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Empty,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        crate::runtime::tape::ScanPolicyEntry {
            rule_id: 14u32,
            alphabet_class: crate::runtime::tape::ScanAlphabetClass::Empty,
            activation: crate::runtime::tape::ScanActivationFlags::from_bits(0),
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
    pub fn parse_CssPrettyParser_stylesheet(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        parse_CssPrettyParser_stylesheet__value(input, p, state, builder)
    }
    /// AW-V.W3.2 — value-position shape dispatcher. Called both at
    /// the grammar root and from Object / Array compound bodies.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_CssPrettyParser_stylesheet__value(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        builder: &mut crate::runtime::tape::Tape<()>,
    ) -> ::core::result::Result<
        crate::runtime::tape::TapeOffset,
        crate::runtime::tape::DtaError,
    > {
        let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
        parse_flat_CssPrettyParser_stylesheet(input, p, state, builder)
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
    pub fn parse_CssPrettyParser_stylesheet_visitor<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        parse_CssPrettyParser_stylesheet_visitor__value(input, p, state, visitor)
    }
    /// AW-V.W3-bench-fix — value-position visitor-path dispatcher.
    /// Called both at the grammar root and from the object / array
    /// shape fns' value-position recursion.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_CssPrettyParser_stylesheet_visitor__value<V>(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_CssPrettyParser::ScanState,
        visitor: &mut V,
    ) -> ::core::result::Result<(), crate::runtime::ParseErr>
    where
        V: crate::runtime::tape::ObjectVisitor + crate::runtime::tape::ArrayVisitor
            + crate::runtime::tape::StringVisitor + crate::runtime::tape::NumberVisitor
            + crate::runtime::tape::KeywordVisitor,
    {
        let _ = __shape_support_CssPrettyParser::skip_space(input, p, state);
        parse_flat_visitor_CssPrettyParser_stylesheet(input, p, state, visitor)
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct importantView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> importantView<'p> {
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
        pub fn rule_kind(&self) -> CssPrettyParserRuleKind {
            match self.variant_idx() {
                0u8 => CssPrettyParserRuleKind::important,
                1u8 => CssPrettyParserRuleKind::declaration,
                2u8 => CssPrettyParserRuleKind::blockContent,
                3u8 => CssPrettyParserRuleKind::ruleBlock,
                4u8 => CssPrettyParserRuleKind::qualifiedRule,
                5u8 => CssPrettyParserRuleKind::mediaRule,
                6u8 => CssPrettyParserRuleKind::supportsRule,
                7u8 => CssPrettyParserRuleKind::fontFaceRule,
                8u8 => CssPrettyParserRuleKind::importRule,
                9u8 => CssPrettyParserRuleKind::atRuleBody,
                10u8 => CssPrettyParserRuleKind::genericAtRule,
                11u8 => CssPrettyParserRuleKind::atRule,
                12u8 => CssPrettyParserRuleKind::ruleItem,
                13u8 => CssPrettyParserRuleKind::ruleList,
                14u8 => CssPrettyParserRuleKind::stylesheet,
                15u8 => CssPrettyParserRuleKind::atRuleBody_0,
                16u8 => CssPrettyParserRuleKind::atRule_0,
                _ => CssPrettyParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CssPrettyParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| CssPrettyParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
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
    impl<'p> importantView<'p> {
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
    pub struct declarationView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> declarationView<'p> {
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
        pub fn rule_kind(&self) -> CssPrettyParserRuleKind {
            match self.variant_idx() {
                0u8 => CssPrettyParserRuleKind::important,
                1u8 => CssPrettyParserRuleKind::declaration,
                2u8 => CssPrettyParserRuleKind::blockContent,
                3u8 => CssPrettyParserRuleKind::ruleBlock,
                4u8 => CssPrettyParserRuleKind::qualifiedRule,
                5u8 => CssPrettyParserRuleKind::mediaRule,
                6u8 => CssPrettyParserRuleKind::supportsRule,
                7u8 => CssPrettyParserRuleKind::fontFaceRule,
                8u8 => CssPrettyParserRuleKind::importRule,
                9u8 => CssPrettyParserRuleKind::atRuleBody,
                10u8 => CssPrettyParserRuleKind::genericAtRule,
                11u8 => CssPrettyParserRuleKind::atRule,
                12u8 => CssPrettyParserRuleKind::ruleItem,
                13u8 => CssPrettyParserRuleKind::ruleList,
                14u8 => CssPrettyParserRuleKind::stylesheet,
                15u8 => CssPrettyParserRuleKind::atRuleBody_0,
                16u8 => CssPrettyParserRuleKind::atRule_0,
                _ => CssPrettyParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CssPrettyParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| CssPrettyParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
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
    impl<'p> declarationView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 2 as a typed view.
        #[inline]
        pub fn child_2(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(2usize)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 3 as a typed view.
        #[inline]
        pub fn child_3(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(3usize)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 4 as a typed view.
        #[inline]
        pub fn child_4(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(4usize)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 5 as a typed view.
        #[inline]
        pub fn child_5(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(5usize)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 6 as a typed view.
        #[inline]
        pub fn child_6(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(6usize)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            7usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct blockContentView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> blockContentView<'p> {
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
        pub fn rule_kind(&self) -> CssPrettyParserRuleKind {
            match self.variant_idx() {
                0u8 => CssPrettyParserRuleKind::important,
                1u8 => CssPrettyParserRuleKind::declaration,
                2u8 => CssPrettyParserRuleKind::blockContent,
                3u8 => CssPrettyParserRuleKind::ruleBlock,
                4u8 => CssPrettyParserRuleKind::qualifiedRule,
                5u8 => CssPrettyParserRuleKind::mediaRule,
                6u8 => CssPrettyParserRuleKind::supportsRule,
                7u8 => CssPrettyParserRuleKind::fontFaceRule,
                8u8 => CssPrettyParserRuleKind::importRule,
                9u8 => CssPrettyParserRuleKind::atRuleBody,
                10u8 => CssPrettyParserRuleKind::genericAtRule,
                11u8 => CssPrettyParserRuleKind::atRule,
                12u8 => CssPrettyParserRuleKind::ruleItem,
                13u8 => CssPrettyParserRuleKind::ruleList,
                14u8 => CssPrettyParserRuleKind::stylesheet,
                15u8 => CssPrettyParserRuleKind::atRuleBody_0,
                16u8 => CssPrettyParserRuleKind::atRule_0,
                _ => CssPrettyParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CssPrettyParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| CssPrettyParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
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
    impl<'p> blockContentView<'p> {
        /// Iterator over each repetition element as a typed view.
        #[inline]
        pub fn iter(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CssPrettyParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| CssPrettyParserNodeView::from_cursor(c, input))
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
        pub fn get(
            &self,
            i: usize,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct ruleBlockView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> ruleBlockView<'p> {
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
        pub fn rule_kind(&self) -> CssPrettyParserRuleKind {
            match self.variant_idx() {
                0u8 => CssPrettyParserRuleKind::important,
                1u8 => CssPrettyParserRuleKind::declaration,
                2u8 => CssPrettyParserRuleKind::blockContent,
                3u8 => CssPrettyParserRuleKind::ruleBlock,
                4u8 => CssPrettyParserRuleKind::qualifiedRule,
                5u8 => CssPrettyParserRuleKind::mediaRule,
                6u8 => CssPrettyParserRuleKind::supportsRule,
                7u8 => CssPrettyParserRuleKind::fontFaceRule,
                8u8 => CssPrettyParserRuleKind::importRule,
                9u8 => CssPrettyParserRuleKind::atRuleBody,
                10u8 => CssPrettyParserRuleKind::genericAtRule,
                11u8 => CssPrettyParserRuleKind::atRule,
                12u8 => CssPrettyParserRuleKind::ruleItem,
                13u8 => CssPrettyParserRuleKind::ruleList,
                14u8 => CssPrettyParserRuleKind::stylesheet,
                15u8 => CssPrettyParserRuleKind::atRuleBody_0,
                16u8 => CssPrettyParserRuleKind::atRule_0,
                _ => CssPrettyParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CssPrettyParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| CssPrettyParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
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
    impl<'p> ruleBlockView<'p> {
        /// The source text matched by this leaf rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct qualifiedRuleView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> qualifiedRuleView<'p> {
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
        pub fn rule_kind(&self) -> CssPrettyParserRuleKind {
            match self.variant_idx() {
                0u8 => CssPrettyParserRuleKind::important,
                1u8 => CssPrettyParserRuleKind::declaration,
                2u8 => CssPrettyParserRuleKind::blockContent,
                3u8 => CssPrettyParserRuleKind::ruleBlock,
                4u8 => CssPrettyParserRuleKind::qualifiedRule,
                5u8 => CssPrettyParserRuleKind::mediaRule,
                6u8 => CssPrettyParserRuleKind::supportsRule,
                7u8 => CssPrettyParserRuleKind::fontFaceRule,
                8u8 => CssPrettyParserRuleKind::importRule,
                9u8 => CssPrettyParserRuleKind::atRuleBody,
                10u8 => CssPrettyParserRuleKind::genericAtRule,
                11u8 => CssPrettyParserRuleKind::atRule,
                12u8 => CssPrettyParserRuleKind::ruleItem,
                13u8 => CssPrettyParserRuleKind::ruleList,
                14u8 => CssPrettyParserRuleKind::stylesheet,
                15u8 => CssPrettyParserRuleKind::atRuleBody_0,
                16u8 => CssPrettyParserRuleKind::atRule_0,
                _ => CssPrettyParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CssPrettyParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| CssPrettyParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
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
    impl<'p> qualifiedRuleView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 2 as a typed view.
        #[inline]
        pub fn child_2(&self) -> ::core::option::Option<ruleBlockView<'p>> {
            self.cursor.child(2usize).map(|c| ruleBlockView::from_cursor(c, self.input))
        }
        ///The `ruleBlock` child as a typed view.
        #[inline]
        pub fn ruleBlock(&self) -> ::core::option::Option<ruleBlockView<'p>> {
            self.cursor.child(2usize).map(|c| ruleBlockView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            3usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct mediaRuleView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> mediaRuleView<'p> {
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
        pub fn rule_kind(&self) -> CssPrettyParserRuleKind {
            match self.variant_idx() {
                0u8 => CssPrettyParserRuleKind::important,
                1u8 => CssPrettyParserRuleKind::declaration,
                2u8 => CssPrettyParserRuleKind::blockContent,
                3u8 => CssPrettyParserRuleKind::ruleBlock,
                4u8 => CssPrettyParserRuleKind::qualifiedRule,
                5u8 => CssPrettyParserRuleKind::mediaRule,
                6u8 => CssPrettyParserRuleKind::supportsRule,
                7u8 => CssPrettyParserRuleKind::fontFaceRule,
                8u8 => CssPrettyParserRuleKind::importRule,
                9u8 => CssPrettyParserRuleKind::atRuleBody,
                10u8 => CssPrettyParserRuleKind::genericAtRule,
                11u8 => CssPrettyParserRuleKind::atRule,
                12u8 => CssPrettyParserRuleKind::ruleItem,
                13u8 => CssPrettyParserRuleKind::ruleList,
                14u8 => CssPrettyParserRuleKind::stylesheet,
                15u8 => CssPrettyParserRuleKind::atRuleBody_0,
                16u8 => CssPrettyParserRuleKind::atRule_0,
                _ => CssPrettyParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CssPrettyParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| CssPrettyParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
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
    impl<'p> mediaRuleView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 2 as a typed view.
        #[inline]
        pub fn child_2(&self) -> ::core::option::Option<ruleBlockView<'p>> {
            self.cursor.child(2usize).map(|c| ruleBlockView::from_cursor(c, self.input))
        }
        ///The `ruleBlock` child as a typed view.
        #[inline]
        pub fn ruleBlock(&self) -> ::core::option::Option<ruleBlockView<'p>> {
            self.cursor.child(2usize).map(|c| ruleBlockView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            3usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct supportsRuleView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> supportsRuleView<'p> {
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
        pub fn rule_kind(&self) -> CssPrettyParserRuleKind {
            match self.variant_idx() {
                0u8 => CssPrettyParserRuleKind::important,
                1u8 => CssPrettyParserRuleKind::declaration,
                2u8 => CssPrettyParserRuleKind::blockContent,
                3u8 => CssPrettyParserRuleKind::ruleBlock,
                4u8 => CssPrettyParserRuleKind::qualifiedRule,
                5u8 => CssPrettyParserRuleKind::mediaRule,
                6u8 => CssPrettyParserRuleKind::supportsRule,
                7u8 => CssPrettyParserRuleKind::fontFaceRule,
                8u8 => CssPrettyParserRuleKind::importRule,
                9u8 => CssPrettyParserRuleKind::atRuleBody,
                10u8 => CssPrettyParserRuleKind::genericAtRule,
                11u8 => CssPrettyParserRuleKind::atRule,
                12u8 => CssPrettyParserRuleKind::ruleItem,
                13u8 => CssPrettyParserRuleKind::ruleList,
                14u8 => CssPrettyParserRuleKind::stylesheet,
                15u8 => CssPrettyParserRuleKind::atRuleBody_0,
                16u8 => CssPrettyParserRuleKind::atRule_0,
                _ => CssPrettyParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CssPrettyParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| CssPrettyParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
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
    impl<'p> supportsRuleView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 2 as a typed view.
        #[inline]
        pub fn child_2(&self) -> ::core::option::Option<ruleBlockView<'p>> {
            self.cursor.child(2usize).map(|c| ruleBlockView::from_cursor(c, self.input))
        }
        ///The `ruleBlock` child as a typed view.
        #[inline]
        pub fn ruleBlock(&self) -> ::core::option::Option<ruleBlockView<'p>> {
            self.cursor.child(2usize).map(|c| ruleBlockView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            3usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct fontFaceRuleView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> fontFaceRuleView<'p> {
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
        pub fn rule_kind(&self) -> CssPrettyParserRuleKind {
            match self.variant_idx() {
                0u8 => CssPrettyParserRuleKind::important,
                1u8 => CssPrettyParserRuleKind::declaration,
                2u8 => CssPrettyParserRuleKind::blockContent,
                3u8 => CssPrettyParserRuleKind::ruleBlock,
                4u8 => CssPrettyParserRuleKind::qualifiedRule,
                5u8 => CssPrettyParserRuleKind::mediaRule,
                6u8 => CssPrettyParserRuleKind::supportsRule,
                7u8 => CssPrettyParserRuleKind::fontFaceRule,
                8u8 => CssPrettyParserRuleKind::importRule,
                9u8 => CssPrettyParserRuleKind::atRuleBody,
                10u8 => CssPrettyParserRuleKind::genericAtRule,
                11u8 => CssPrettyParserRuleKind::atRule,
                12u8 => CssPrettyParserRuleKind::ruleItem,
                13u8 => CssPrettyParserRuleKind::ruleList,
                14u8 => CssPrettyParserRuleKind::stylesheet,
                15u8 => CssPrettyParserRuleKind::atRuleBody_0,
                16u8 => CssPrettyParserRuleKind::atRule_0,
                _ => CssPrettyParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CssPrettyParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| CssPrettyParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
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
    impl<'p> fontFaceRuleView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 2 as a typed view.
        #[inline]
        pub fn child_2(&self) -> ::core::option::Option<ruleBlockView<'p>> {
            self.cursor.child(2usize).map(|c| ruleBlockView::from_cursor(c, self.input))
        }
        ///The `ruleBlock` child as a typed view.
        #[inline]
        pub fn ruleBlock(&self) -> ::core::option::Option<ruleBlockView<'p>> {
            self.cursor.child(2usize).map(|c| ruleBlockView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            3usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct importRuleView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> importRuleView<'p> {
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
        pub fn rule_kind(&self) -> CssPrettyParserRuleKind {
            match self.variant_idx() {
                0u8 => CssPrettyParserRuleKind::important,
                1u8 => CssPrettyParserRuleKind::declaration,
                2u8 => CssPrettyParserRuleKind::blockContent,
                3u8 => CssPrettyParserRuleKind::ruleBlock,
                4u8 => CssPrettyParserRuleKind::qualifiedRule,
                5u8 => CssPrettyParserRuleKind::mediaRule,
                6u8 => CssPrettyParserRuleKind::supportsRule,
                7u8 => CssPrettyParserRuleKind::fontFaceRule,
                8u8 => CssPrettyParserRuleKind::importRule,
                9u8 => CssPrettyParserRuleKind::atRuleBody,
                10u8 => CssPrettyParserRuleKind::genericAtRule,
                11u8 => CssPrettyParserRuleKind::atRule,
                12u8 => CssPrettyParserRuleKind::ruleItem,
                13u8 => CssPrettyParserRuleKind::ruleList,
                14u8 => CssPrettyParserRuleKind::stylesheet,
                15u8 => CssPrettyParserRuleKind::atRuleBody_0,
                16u8 => CssPrettyParserRuleKind::atRule_0,
                _ => CssPrettyParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CssPrettyParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| CssPrettyParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
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
    impl<'p> importRuleView<'p> {
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
    pub struct atRuleBodyView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> atRuleBodyView<'p> {
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
        pub fn rule_kind(&self) -> CssPrettyParserRuleKind {
            match self.variant_idx() {
                0u8 => CssPrettyParserRuleKind::important,
                1u8 => CssPrettyParserRuleKind::declaration,
                2u8 => CssPrettyParserRuleKind::blockContent,
                3u8 => CssPrettyParserRuleKind::ruleBlock,
                4u8 => CssPrettyParserRuleKind::qualifiedRule,
                5u8 => CssPrettyParserRuleKind::mediaRule,
                6u8 => CssPrettyParserRuleKind::supportsRule,
                7u8 => CssPrettyParserRuleKind::fontFaceRule,
                8u8 => CssPrettyParserRuleKind::importRule,
                9u8 => CssPrettyParserRuleKind::atRuleBody,
                10u8 => CssPrettyParserRuleKind::genericAtRule,
                11u8 => CssPrettyParserRuleKind::atRule,
                12u8 => CssPrettyParserRuleKind::ruleItem,
                13u8 => CssPrettyParserRuleKind::ruleList,
                14u8 => CssPrettyParserRuleKind::stylesheet,
                15u8 => CssPrettyParserRuleKind::atRuleBody_0,
                16u8 => CssPrettyParserRuleKind::atRule_0,
                _ => CssPrettyParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CssPrettyParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| CssPrettyParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
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
    impl<'p> atRuleBodyView<'p> {
        ///If variant `branch_0` (branch 0) was chosen, return its child view.
        #[inline]
        pub fn as_branch_0(
            &self,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor
                    .child(0)
                    .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `branch_0` (branch 0) was chosen.
        #[inline]
        pub fn is_branch_0(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If variant `ruleBlock` (branch 1) was chosen, return its child view.
        #[inline]
        pub fn as_ruleBlock(&self) -> ::core::option::Option<ruleBlockView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor.child(0).map(|c| ruleBlockView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `ruleBlock` (branch 1) was chosen.
        #[inline]
        pub fn is_ruleBlock(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        ///If sub-variant `atRuleBody_0` was chosen (branch 0), return its child view.
        #[inline]
        pub fn as_atRuleBody_0(
            &self,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor
                    .child(0)
                    .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_atRuleBody_0(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        /// The chosen branch's child as a generic node view,
        /// regardless of which variant was selected.
        #[inline]
        pub fn chosen(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(0)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct genericAtRuleView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> genericAtRuleView<'p> {
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
        pub fn rule_kind(&self) -> CssPrettyParserRuleKind {
            match self.variant_idx() {
                0u8 => CssPrettyParserRuleKind::important,
                1u8 => CssPrettyParserRuleKind::declaration,
                2u8 => CssPrettyParserRuleKind::blockContent,
                3u8 => CssPrettyParserRuleKind::ruleBlock,
                4u8 => CssPrettyParserRuleKind::qualifiedRule,
                5u8 => CssPrettyParserRuleKind::mediaRule,
                6u8 => CssPrettyParserRuleKind::supportsRule,
                7u8 => CssPrettyParserRuleKind::fontFaceRule,
                8u8 => CssPrettyParserRuleKind::importRule,
                9u8 => CssPrettyParserRuleKind::atRuleBody,
                10u8 => CssPrettyParserRuleKind::genericAtRule,
                11u8 => CssPrettyParserRuleKind::atRule,
                12u8 => CssPrettyParserRuleKind::ruleItem,
                13u8 => CssPrettyParserRuleKind::ruleList,
                14u8 => CssPrettyParserRuleKind::stylesheet,
                15u8 => CssPrettyParserRuleKind::atRuleBody_0,
                16u8 => CssPrettyParserRuleKind::atRule_0,
                _ => CssPrettyParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CssPrettyParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| CssPrettyParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
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
    impl<'p> genericAtRuleView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 2 as a typed view.
        #[inline]
        pub fn child_2(&self) -> ::core::option::Option<atRuleBodyView<'p>> {
            self.cursor.child(2usize).map(|c| atRuleBodyView::from_cursor(c, self.input))
        }
        ///The `atRuleBody` child as a typed view.
        #[inline]
        pub fn atRuleBody(&self) -> ::core::option::Option<atRuleBodyView<'p>> {
            self.cursor.child(2usize).map(|c| atRuleBodyView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            3usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct atRuleView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> atRuleView<'p> {
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
        pub fn rule_kind(&self) -> CssPrettyParserRuleKind {
            match self.variant_idx() {
                0u8 => CssPrettyParserRuleKind::important,
                1u8 => CssPrettyParserRuleKind::declaration,
                2u8 => CssPrettyParserRuleKind::blockContent,
                3u8 => CssPrettyParserRuleKind::ruleBlock,
                4u8 => CssPrettyParserRuleKind::qualifiedRule,
                5u8 => CssPrettyParserRuleKind::mediaRule,
                6u8 => CssPrettyParserRuleKind::supportsRule,
                7u8 => CssPrettyParserRuleKind::fontFaceRule,
                8u8 => CssPrettyParserRuleKind::importRule,
                9u8 => CssPrettyParserRuleKind::atRuleBody,
                10u8 => CssPrettyParserRuleKind::genericAtRule,
                11u8 => CssPrettyParserRuleKind::atRule,
                12u8 => CssPrettyParserRuleKind::ruleItem,
                13u8 => CssPrettyParserRuleKind::ruleList,
                14u8 => CssPrettyParserRuleKind::stylesheet,
                15u8 => CssPrettyParserRuleKind::atRuleBody_0,
                16u8 => CssPrettyParserRuleKind::atRule_0,
                _ => CssPrettyParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CssPrettyParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| CssPrettyParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
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
    impl<'p> atRuleView<'p> {
        ///If variant `mediaRule` (branch 0) was chosen, return its child view.
        #[inline]
        pub fn as_mediaRule(&self) -> ::core::option::Option<mediaRuleView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor.child(0).map(|c| mediaRuleView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `mediaRule` (branch 0) was chosen.
        #[inline]
        pub fn is_mediaRule(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If variant `supportsRule` (branch 1) was chosen, return its child view.
        #[inline]
        pub fn as_supportsRule(&self) -> ::core::option::Option<supportsRuleView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor
                    .child(0)
                    .map(|c| supportsRuleView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `supportsRule` (branch 1) was chosen.
        #[inline]
        pub fn is_supportsRule(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        ///If variant `fontFaceRule` (branch 2) was chosen, return its child view.
        #[inline]
        pub fn as_fontFaceRule(&self) -> ::core::option::Option<fontFaceRuleView<'p>> {
            if self.cursor.meta_idx() == 2u8 {
                self.cursor
                    .child(0)
                    .map(|c| fontFaceRuleView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `fontFaceRule` (branch 2) was chosen.
        #[inline]
        pub fn is_fontFaceRule(&self) -> bool {
            self.cursor.meta_idx() == 2u8
        }
        ///If variant `importRule` (branch 3) was chosen, return its child view.
        #[inline]
        pub fn as_importRule(&self) -> ::core::option::Option<importRuleView<'p>> {
            if self.cursor.meta_idx() == 3u8 {
                self.cursor.child(0).map(|c| importRuleView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `importRule` (branch 3) was chosen.
        #[inline]
        pub fn is_importRule(&self) -> bool {
            self.cursor.meta_idx() == 3u8
        }
        ///If variant `genericAtRule` (branch 4) was chosen, return its child view.
        #[inline]
        pub fn as_genericAtRule(&self) -> ::core::option::Option<genericAtRuleView<'p>> {
            if self.cursor.meta_idx() == 4u8 {
                self.cursor
                    .child(0)
                    .map(|c| genericAtRuleView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `genericAtRule` (branch 4) was chosen.
        #[inline]
        pub fn is_genericAtRule(&self) -> bool {
            self.cursor.meta_idx() == 4u8
        }
        ///If sub-variant `atRule_0` was chosen (branch 3), return its child view.
        #[inline]
        pub fn as_atRule_0(
            &self,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            if self.cursor.meta_idx() == 3u8 {
                self.cursor
                    .child(0)
                    .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_atRule_0(&self) -> bool {
            self.cursor.meta_idx() == 3u8
        }
        /// The chosen branch's child as a generic node view,
        /// regardless of which variant was selected.
        #[inline]
        pub fn chosen(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(0)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
    }
    /// Typed value enum — payload-eligible branches carry typed
    /// values directly; non-eligible branches wrap a cursor view.
    #[derive(Clone, Debug)]
    pub enum atRuleValue<'p> {
        mediaRule(CssPrettyParserNodeView<'p>),
        supportsRule(CssPrettyParserNodeView<'p>),
        fontFaceRule(CssPrettyParserNodeView<'p>),
        importRule(((u32, u32))),
        genericAtRule(CssPrettyParserNodeView<'p>),
    }
    impl<'p> atRuleView<'p> {
        /// Decode the chosen branch's value. Payload-eligible
        /// branches return typed scalars/aggregates; other
        /// branches return cursor-wrapped sub-views.
        #[inline]
        pub fn value(&self) -> ::core::option::Option<atRuleValue<'p>> {
            match self.cursor.meta_idx() {
                0u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        atRuleValue::mediaRule(
                            CssPrettyParserNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                1u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        atRuleValue::supportsRule(
                            CssPrettyParserNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                2u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        atRuleValue::fontFaceRule(
                            CssPrettyParserNodeView::from_cursor(__child, self.input),
                        ),
                    )
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
                    Some(atRuleValue::importRule(__value))
                }
                4u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        atRuleValue::genericAtRule(
                            CssPrettyParserNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                _ => None,
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct ruleItemView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> ruleItemView<'p> {
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
        pub fn rule_kind(&self) -> CssPrettyParserRuleKind {
            match self.variant_idx() {
                0u8 => CssPrettyParserRuleKind::important,
                1u8 => CssPrettyParserRuleKind::declaration,
                2u8 => CssPrettyParserRuleKind::blockContent,
                3u8 => CssPrettyParserRuleKind::ruleBlock,
                4u8 => CssPrettyParserRuleKind::qualifiedRule,
                5u8 => CssPrettyParserRuleKind::mediaRule,
                6u8 => CssPrettyParserRuleKind::supportsRule,
                7u8 => CssPrettyParserRuleKind::fontFaceRule,
                8u8 => CssPrettyParserRuleKind::importRule,
                9u8 => CssPrettyParserRuleKind::atRuleBody,
                10u8 => CssPrettyParserRuleKind::genericAtRule,
                11u8 => CssPrettyParserRuleKind::atRule,
                12u8 => CssPrettyParserRuleKind::ruleItem,
                13u8 => CssPrettyParserRuleKind::ruleList,
                14u8 => CssPrettyParserRuleKind::stylesheet,
                15u8 => CssPrettyParserRuleKind::atRuleBody_0,
                16u8 => CssPrettyParserRuleKind::atRule_0,
                _ => CssPrettyParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CssPrettyParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| CssPrettyParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
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
    impl<'p> ruleItemView<'p> {
        ///If variant `qualifiedRule` (branch 0) was chosen, return its child view.
        #[inline]
        pub fn as_qualifiedRule(&self) -> ::core::option::Option<qualifiedRuleView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor
                    .child(0)
                    .map(|c| qualifiedRuleView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `qualifiedRule` (branch 0) was chosen.
        #[inline]
        pub fn is_qualifiedRule(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If variant `atRule` (branch 1) was chosen, return its child view.
        #[inline]
        pub fn as_atRule(&self) -> ::core::option::Option<atRuleView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor.child(0).map(|c| atRuleView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `atRule` (branch 1) was chosen.
        #[inline]
        pub fn is_atRule(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        /// The chosen branch's child as a generic node view,
        /// regardless of which variant was selected.
        #[inline]
        pub fn chosen(&self) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(0)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct ruleListView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> ruleListView<'p> {
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
        pub fn rule_kind(&self) -> CssPrettyParserRuleKind {
            match self.variant_idx() {
                0u8 => CssPrettyParserRuleKind::important,
                1u8 => CssPrettyParserRuleKind::declaration,
                2u8 => CssPrettyParserRuleKind::blockContent,
                3u8 => CssPrettyParserRuleKind::ruleBlock,
                4u8 => CssPrettyParserRuleKind::qualifiedRule,
                5u8 => CssPrettyParserRuleKind::mediaRule,
                6u8 => CssPrettyParserRuleKind::supportsRule,
                7u8 => CssPrettyParserRuleKind::fontFaceRule,
                8u8 => CssPrettyParserRuleKind::importRule,
                9u8 => CssPrettyParserRuleKind::atRuleBody,
                10u8 => CssPrettyParserRuleKind::genericAtRule,
                11u8 => CssPrettyParserRuleKind::atRule,
                12u8 => CssPrettyParserRuleKind::ruleItem,
                13u8 => CssPrettyParserRuleKind::ruleList,
                14u8 => CssPrettyParserRuleKind::stylesheet,
                15u8 => CssPrettyParserRuleKind::atRuleBody_0,
                16u8 => CssPrettyParserRuleKind::atRule_0,
                _ => CssPrettyParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CssPrettyParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| CssPrettyParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
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
    impl<'p> ruleListView<'p> {
        /// Iterator over each repetition element as a typed view.
        #[inline]
        pub fn iter(&self) -> impl ::core::iter::Iterator<Item = ruleItemView<'p>> + 'p {
            let input = self.input;
            self.cursor.children().map(move |c| ruleItemView::from_cursor(c, input))
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
        pub fn get(&self, i: usize) -> ::core::option::Option<ruleItemView<'p>> {
            self.cursor.child(i).map(|c| ruleItemView::from_cursor(c, self.input))
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct stylesheetView<'p> {
        cursor: crate::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> stylesheetView<'p> {
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
        pub fn rule_kind(&self) -> CssPrettyParserRuleKind {
            match self.variant_idx() {
                0u8 => CssPrettyParserRuleKind::important,
                1u8 => CssPrettyParserRuleKind::declaration,
                2u8 => CssPrettyParserRuleKind::blockContent,
                3u8 => CssPrettyParserRuleKind::ruleBlock,
                4u8 => CssPrettyParserRuleKind::qualifiedRule,
                5u8 => CssPrettyParserRuleKind::mediaRule,
                6u8 => CssPrettyParserRuleKind::supportsRule,
                7u8 => CssPrettyParserRuleKind::fontFaceRule,
                8u8 => CssPrettyParserRuleKind::importRule,
                9u8 => CssPrettyParserRuleKind::atRuleBody,
                10u8 => CssPrettyParserRuleKind::genericAtRule,
                11u8 => CssPrettyParserRuleKind::atRule,
                12u8 => CssPrettyParserRuleKind::ruleItem,
                13u8 => CssPrettyParserRuleKind::ruleList,
                14u8 => CssPrettyParserRuleKind::stylesheet,
                15u8 => CssPrettyParserRuleKind::atRuleBody_0,
                16u8 => CssPrettyParserRuleKind::atRule_0,
                _ => CssPrettyParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CssPrettyParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| CssPrettyParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
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
    impl<'p> stylesheetView<'p> {
        /// The source text matched by this leaf rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
    }
    /// Generic node view over any tape record for this grammar.
    #[derive(Clone, Copy, Debug)]
    pub struct CssPrettyParserNodeView<'p> {
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
    pub enum CssPrettyParserRuleKind {
        important,
        declaration,
        blockContent,
        ruleBlock,
        qualifiedRule,
        mediaRule,
        supportsRule,
        fontFaceRule,
        importRule,
        atRuleBody,
        genericAtRule,
        atRule,
        ruleItem,
        ruleList,
        stylesheet,
        atRuleBody_0,
        atRule_0,
        /// Fallback for records whose variant_idx is not a
        /// known rule- or sub-variant discriminator.
        Unknown,
    }
    impl<'p> CssPrettyParserNodeView<'p> {
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
        pub fn rule_kind(&self) -> CssPrettyParserRuleKind {
            match self.variant_idx() {
                0u8 => CssPrettyParserRuleKind::important,
                1u8 => CssPrettyParserRuleKind::declaration,
                2u8 => CssPrettyParserRuleKind::blockContent,
                3u8 => CssPrettyParserRuleKind::ruleBlock,
                4u8 => CssPrettyParserRuleKind::qualifiedRule,
                5u8 => CssPrettyParserRuleKind::mediaRule,
                6u8 => CssPrettyParserRuleKind::supportsRule,
                7u8 => CssPrettyParserRuleKind::fontFaceRule,
                8u8 => CssPrettyParserRuleKind::importRule,
                9u8 => CssPrettyParserRuleKind::atRuleBody,
                10u8 => CssPrettyParserRuleKind::genericAtRule,
                11u8 => CssPrettyParserRuleKind::atRule,
                12u8 => CssPrettyParserRuleKind::ruleItem,
                13u8 => CssPrettyParserRuleKind::ruleList,
                14u8 => CssPrettyParserRuleKind::stylesheet,
                15u8 => CssPrettyParserRuleKind::atRuleBody_0,
                16u8 => CssPrettyParserRuleKind::atRule_0,
                _ => CssPrettyParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = CssPrettyParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| CssPrettyParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| CssPrettyParserNodeView::from_cursor(c, self.input))
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
    impl crate::runtime::Root for CssPrettyParser {
        type View<'p> = stylesheetView<'p>;
        #[inline]
        fn make_view<'p>(
            tape: &'p crate::runtime::tape::Tape<()>,
            input: &'p str,
            root: crate::runtime::tape::TapeOffset,
        ) -> Self::View<'p> {
            stylesheetView::new(tape, input, root)
        }
    }
    impl CssPrettyParser {
        /// The name of the root rule for this grammar.
        #[inline]
        pub fn root_rule_name() -> &'static str {
            "stylesheet"
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
    pub struct CssPrettyParserImportantProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: (u32, u32),
    }
    impl CssPrettyParserImportantProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "important";
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
    pub struct CssPrettyParserImportRuleProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: (u32, u32),
    }
    impl CssPrettyParserImportRuleProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "importRule";
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
        ("important", "CssPrettyParserImportantProjection"),
        ("importRule", "CssPrettyParserImportRuleProjection"),
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
        "materialize_projection_important_CssPrettyParser",
        "materialize_projection_importrule_CssPrettyParser",
    ];
    /// AY-II.W0.d — canonical evidence that every admission has a
    /// matching `<Grammar>Value::<RuleName>` enum variant
    /// (production consumer). Indexed in lockstep with
    /// `PROJECTION_DIRECT_TO_STRUCT`.
    #[doc(hidden)]
    pub const PROJECTION_CONSUMERS: &[&str; 2usize] = &[
        "CssPrettyParserValue::important",
        "CssPrettyParserValue::importRule",
    ];
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_important() -> (&'static str, usize, &'static str) {
        ("important", 1, "")
    }
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_importrule() -> (&'static str, usize, &'static str) {
        ("importRule", 1, "")
    }
    /// AY-II.W0'.b — grammar-emitted value enum. Eager
    /// materialisation target for `Parsed::to_value()`. Variants
    /// enumerate non-transparent rules; admitted rules carry the
    /// matching `<Grammar><RuleCamel>Projection` struct directly,
    /// non-admitted rules carry their shape-classified payload.
    #[derive(Clone, Debug)]
    pub enum CssPrettyParserValue<'p> {
        important(CssPrettyParserImportantProjection),
        declaration(::std::vec::Vec<CssPrettyParserValue<'p>>),
        blockContent(::std::vec::Vec<CssPrettyParserValue<'p>>),
        ruleBlock(&'p str),
        qualifiedRule(::std::vec::Vec<CssPrettyParserValue<'p>>),
        mediaRule(::std::vec::Vec<CssPrettyParserValue<'p>>),
        supportsRule(::std::vec::Vec<CssPrettyParserValue<'p>>),
        fontFaceRule(::std::vec::Vec<CssPrettyParserValue<'p>>),
        importRule(CssPrettyParserImportRuleProjection),
        atRuleBody(::std::vec::Vec<CssPrettyParserValue<'p>>),
        genericAtRule(::std::vec::Vec<CssPrettyParserValue<'p>>),
        atRule(::std::vec::Vec<CssPrettyParserValue<'p>>),
        ruleItem(::std::vec::Vec<CssPrettyParserValue<'p>>),
        ruleList(::std::vec::Vec<CssPrettyParserValue<'p>>),
        stylesheet(&'p str),
        /// Fallback for records whose `variant_idx` is not a
        /// known rule discriminator (recovered records, stray
        /// sub-variant indices).
        Unknown(CssPrettyParserNodeView<'p>),
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
    fn project_rule_kind_CssPrettyParser(
        kind: crate::runtime::tape::TapeKind,
        variant_idx: u8,
    ) -> CssPrettyParserRuleKind {
        if variant_idx == 0 && kind.is_compound() {
            return CssPrettyParserRuleKind::Unknown;
        }
        match variant_idx {
            0u8 => CssPrettyParserRuleKind::important,
            1u8 => CssPrettyParserRuleKind::declaration,
            2u8 => CssPrettyParserRuleKind::blockContent,
            3u8 => CssPrettyParserRuleKind::ruleBlock,
            4u8 => CssPrettyParserRuleKind::qualifiedRule,
            5u8 => CssPrettyParserRuleKind::mediaRule,
            6u8 => CssPrettyParserRuleKind::supportsRule,
            7u8 => CssPrettyParserRuleKind::fontFaceRule,
            8u8 => CssPrettyParserRuleKind::importRule,
            9u8 => CssPrettyParserRuleKind::atRuleBody,
            10u8 => CssPrettyParserRuleKind::genericAtRule,
            11u8 => CssPrettyParserRuleKind::atRule,
            12u8 => CssPrettyParserRuleKind::ruleItem,
            13u8 => CssPrettyParserRuleKind::ruleList,
            14u8 => CssPrettyParserRuleKind::stylesheet,
            _ => CssPrettyParserRuleKind::Unknown,
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
    fn project_push_children_CssPrettyParser<'p>(
        output: &crate::runtime::tape::Tape<CssPrettyParser>,
        input: &'p str,
        offset: u32,
        out: &mut ::std::vec::Vec<CssPrettyParserValue<'p>>,
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
                project_push_children_CssPrettyParser(
                    output,
                    input,
                    __child.offset().0,
                    out,
                );
            }
        } else {
            out.push(project_frame_CssPrettyParser(output, input, offset));
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
    fn project_frame_CssPrettyParser<'p>(
        output: &crate::runtime::tape::Tape<CssPrettyParser>,
        input: &'p str,
        offset: u32,
    ) -> CssPrettyParserValue<'p> {
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
        match project_rule_kind_CssPrettyParser(__rec.kind(), __rec.variant_idx()) {
            CssPrettyParserRuleKind::important => {
                let proj = materialize_projection_important_CssPrettyParser(
                        output,
                        input,
                        offset,
                    )
                    .unwrap_or_else(|| {
                        ::core::panic!(
                            "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                            "important", offset,
                        );
                    });
                CssPrettyParserValue::important(proj)
            }
            CssPrettyParserRuleKind::declaration => {
                let mut children: ::std::vec::Vec<CssPrettyParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_CssPrettyParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                CssPrettyParserValue::declaration(children)
            }
            CssPrettyParserRuleKind::blockContent => {
                let mut children: ::std::vec::Vec<CssPrettyParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_CssPrettyParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                CssPrettyParserValue::blockContent(children)
            }
            CssPrettyParserRuleKind::ruleBlock => {
                let span = &input[__rec.span_lo as usize..__rec.span_hi as usize];
                CssPrettyParserValue::ruleBlock(span)
            }
            CssPrettyParserRuleKind::qualifiedRule => {
                let mut children: ::std::vec::Vec<CssPrettyParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_CssPrettyParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                CssPrettyParserValue::qualifiedRule(children)
            }
            CssPrettyParserRuleKind::mediaRule => {
                let mut children: ::std::vec::Vec<CssPrettyParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_CssPrettyParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                CssPrettyParserValue::mediaRule(children)
            }
            CssPrettyParserRuleKind::supportsRule => {
                let mut children: ::std::vec::Vec<CssPrettyParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_CssPrettyParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                CssPrettyParserValue::supportsRule(children)
            }
            CssPrettyParserRuleKind::fontFaceRule => {
                let mut children: ::std::vec::Vec<CssPrettyParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_CssPrettyParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                CssPrettyParserValue::fontFaceRule(children)
            }
            CssPrettyParserRuleKind::importRule => {
                let proj = materialize_projection_importrule_CssPrettyParser(
                        output,
                        input,
                        offset,
                    )
                    .unwrap_or_else(|| {
                        ::core::panic!(
                            "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                            "importRule", offset,
                        );
                    });
                CssPrettyParserValue::importRule(proj)
            }
            CssPrettyParserRuleKind::atRuleBody => {
                let mut children: ::std::vec::Vec<CssPrettyParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_CssPrettyParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                CssPrettyParserValue::atRuleBody(children)
            }
            CssPrettyParserRuleKind::genericAtRule => {
                let mut children: ::std::vec::Vec<CssPrettyParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_CssPrettyParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                CssPrettyParserValue::genericAtRule(children)
            }
            CssPrettyParserRuleKind::atRule => {
                let mut children: ::std::vec::Vec<CssPrettyParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_CssPrettyParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                CssPrettyParserValue::atRule(children)
            }
            CssPrettyParserRuleKind::ruleItem => {
                let mut children: ::std::vec::Vec<CssPrettyParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_CssPrettyParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                CssPrettyParserValue::ruleItem(children)
            }
            CssPrettyParserRuleKind::ruleList => {
                let mut children: ::std::vec::Vec<CssPrettyParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = crate::runtime::tape::TapeCursor::new(
                    __tape,
                    crate::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_CssPrettyParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                CssPrettyParserValue::ruleList(children)
            }
            CssPrettyParserRuleKind::stylesheet => {
                let span = &input[__rec.span_lo as usize..__rec.span_hi as usize];
                CssPrettyParserValue::stylesheet(span)
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
    fn project_value_CssPrettyParser<'p>(
        output: &crate::runtime::tape::Tape<CssPrettyParser>,
        input: &'p str,
    ) -> CssPrettyParserValue<'p> {
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
        project_frame_CssPrettyParser(output, input, __cur_off)
    }
    impl crate::runtime::ValueRoot for CssPrettyParser {
        type Value<'p> = CssPrettyParserValue<'p>;
        #[inline]
        fn project_value_output<'p>(
            output: &crate::runtime::tape::Tape<CssPrettyParser>,
            input: &'p str,
        ) -> Self::Value<'p>
        where
            Self: 'p,
        {
            project_value_CssPrettyParser(output, input)
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
        view: CssPrettyParserNodeView<'p>,
        path: crate::runtime::Path<'_>,
    ) -> ::core::option::Option<CssPrettyParserNodeView<'p>> {
        let cur_input = view.input();
        let mut cur = view;
        for seg in path.iter() {
            match seg {
                crate::runtime::PathSegment::Field(key) => {
                    match cur.rule_kind() {
                        CssPrettyParserRuleKind::important
                        | CssPrettyParserRuleKind::qualifiedRule
                        | CssPrettyParserRuleKind::atRuleBody
                        | CssPrettyParserRuleKind::ruleItem => {
                            let parent = cur.cursor();
                            let (_, parent_end) = parent.span();
                            let mut iter = parent.bounded_lookahead(parent_end);
                            let mut hit: ::core::option::Option<
                                CssPrettyParserNodeView<'p>,
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
                                        CssPrettyParserNodeView::from_cursor(v_cur, cur_input),
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
    impl crate::runtime::PathQuery<&'static str> for CssPrettyParser {
        #[inline]
        fn query<'p>(
            view: Self::View<'p>,
            path: crate::runtime::Path<'_>,
        ) -> ::core::option::Option<&'static str>
        where
            Self: 'p,
        {
            let node = CssPrettyParserNodeView::from_cursor(view.cursor(), view.input());
            __path_walk(node, path)?;
            ::core::option::Option::None
        }
    }
    impl crate::runtime::PathQuery<f64> for CssPrettyParser {
        #[inline]
        fn query<'p>(
            view: Self::View<'p>,
            path: crate::runtime::Path<'_>,
        ) -> ::core::option::Option<f64>
        where
            Self: 'p,
        {
            let node = CssPrettyParserNodeView::from_cursor(view.cursor(), view.input());
            let hit = __path_walk(node, path)?;
            let tape = hit.cursor().tape();
            let rec = hit.cursor().record();
            if let ::core::option::Option::Some(v) = tape.payload_f64(rec) {
                return ::core::option::Option::Some(v);
            }
            hit.span_text().parse::<f64>().ok()
        }
    }
    impl crate::runtime::PathQuery<bool> for CssPrettyParser {
        #[inline]
        fn query<'p>(
            view: Self::View<'p>,
            path: crate::runtime::Path<'_>,
        ) -> ::core::option::Option<bool>
        where
            Self: 'p,
        {
            let node = CssPrettyParserNodeView::from_cursor(view.cursor(), view.input());
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
    pub fn materialize_projection_important_CssPrettyParser<'p>(
        output: &crate::runtime::tape::Tape<CssPrettyParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<CssPrettyParserImportantProjection> {
        let _ = input;
        let frame = output.frame(offset)?;
        let __bytes: &[u8] = &[];
        let _ = __bytes;
        let field_0: (u32, u32) = (frame.span_lo, frame.span_hi);
        ::core::option::Option::Some(CssPrettyParserImportantProjection {
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
    pub fn materialize_projection_importrule_CssPrettyParser<'p>(
        output: &crate::runtime::tape::Tape<CssPrettyParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<CssPrettyParserImportRuleProjection> {
        let _ = input;
        let frame = output.frame(offset)?;
        let __bytes: &[u8] = &[];
        let _ = __bytes;
        let field_0: (u32, u32) = (frame.span_lo, frame.span_hi);
        ::core::option::Option::Some(CssPrettyParserImportRuleProjection {
            field_0,
        })
    }
    impl CssPrettyParser {
        fn __important_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'!') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'!');
                    };
                    {
                        let __start = state.offset;
                        if ::parse_that::scan_ws_block_comments(state).is_none() {
                            return false;
                        }
                        let __matched = &state.src[__start..state.offset];
                        if !__matched.is_empty() {
                            __builder.text(__matched);
                        }
                    };
                    {
                        let __s = "important";
                        let __bytes = __s.as_bytes();
                        let __slc = match state.src_bytes.get(state.offset..) {
                            Some(s) if s.len() >= 9usize => s,
                            _ => return false,
                        };
                        if &__slc[..9usize] != __bytes {
                            return false;
                        }
                        __builder.text(&state.src[state.offset..state.offset + 9usize]);
                        state.offset += 9usize;
                    };
                };
                true
            }
        }
        pub fn important_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__important_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __declaration_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            let __pretty_ok = {
                {
                    {
                        {
                            let __start = state.offset;
                            if {
                                let __start = state.offset;
                                let __result: Option<()> = (|| {
                                    {
                                        let __save_alt = state.offset;
                                        let __alt_ok = (|| -> Option<()> {
                                            {
                                                let __b = *state.src_bytes.get(state.offset)?;
                                                if !(((__b >= b'A' && __b <= b'Z') || __b == b'_'
                                                    || (__b >= b'a' && __b <= b'z')))
                                                {
                                                    return None;
                                                }
                                                state.offset += 1;
                                            }
                                            {
                                                let __end = state.src_bytes.len();
                                                let mut __pos = state.offset;
                                                while __pos < __end {
                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                    if (__b == b'-' || (__b >= b'0' && __b <= b'9')
                                                        || (__b >= b'A' && __b <= b'Z') || __b == b'_'
                                                        || (__b >= b'a' && __b <= b'z'))
                                                    {
                                                        __pos += 1;
                                                    } else {
                                                        break;
                                                    }
                                                }
                                                state.offset = __pos;
                                            }
                                            Some(())
                                        })();
                                        let __alt_ok = if __alt_ok.is_none() {
                                            state.offset = __save_alt;
                                            (|| -> Option<()> {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                                {
                                                    return None;
                                                }
                                                state.offset += 1;
                                                if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                                {
                                                    return None;
                                                }
                                                state.offset += 1;
                                                {
                                                    let __loop_start = state.offset;
                                                    let __end = state.src_bytes.len();
                                                    let mut __pos = state.offset;
                                                    while __pos < __end {
                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                        if (__b == b'-' || (__b >= b'0' && __b <= b'9')
                                                            || (__b >= b'A' && __b <= b'Z') || __b == b'_'
                                                            || (__b >= b'a' && __b <= b'z'))
                                                        {
                                                            __pos += 1;
                                                        } else {
                                                            break;
                                                        }
                                                    }
                                                    if __pos < __loop_start + 1 as usize {
                                                        return None;
                                                    }
                                                    state.offset = __pos;
                                                }
                                                Some(())
                                            })()
                                        } else {
                                            __alt_ok
                                        };
                                        let __alt_ok = if __alt_ok.is_none() {
                                            state.offset = __save_alt;
                                            (|| -> Option<()> {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                                {
                                                    return None;
                                                }
                                                state.offset += 1;
                                                {
                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                    if !(__b.is_ascii_alphabetic()) {
                                                        return None;
                                                    }
                                                    state.offset += 1;
                                                }
                                                {
                                                    let __end = state.src_bytes.len();
                                                    let mut __pos = state.offset;
                                                    while __pos < __end {
                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                        if (__b == b'-' || (__b >= b'0' && __b <= b'9')
                                                            || (__b >= b'A' && __b <= b'Z') || __b == b'_'
                                                            || (__b >= b'a' && __b <= b'z'))
                                                        {
                                                            __pos += 1;
                                                        } else {
                                                            break;
                                                        }
                                                    }
                                                    state.offset = __pos;
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
                        {
                            let __start = state.offset;
                            if ::parse_that::scan_ws_block_comments(state).is_none() {
                                return false;
                            }
                            let __matched = &state.src[__start..state.offset];
                            if !__matched.is_empty() {
                                __builder.text(__matched);
                            }
                        };
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b':') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b':');
                        };
                        {
                            let __start = state.offset;
                            if ::parse_that::scan_ws_block_comments(state).is_none() {
                                return false;
                            }
                            let __matched = &state.src[__start..state.offset];
                            if !__matched.is_empty() {
                                __builder.text(__matched);
                            }
                        };
                        {
                            let _ = {
                                let __pretty_cp4 = state.offset;
                                let __pretty_bcp5 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            let __start = state.offset;
                                            if {
                                                let __start = state.offset;
                                                loop {
                                                    ::parse_that::scan_ws_block_comments(state);
                                                    if state.offset >= state.src_bytes.len() {
                                                        break;
                                                    }
                                                    let __b = state.src_bytes[state.offset];
                                                    if __b == b'!' || __b == b',' || __b == b';' || __b == b'{'
                                                        || __b == b'}'
                                                    {
                                                        break;
                                                    }
                                                    state.offset += 1;
                                                }
                                                if state.offset > __start {
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
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
                                            let mut __rep_count2 = 0usize;
                                            while __rep_count2 < 4294967295 {
                                                let __rep_cp3 = state.offset;
                                                if !{
                                                    let __pretty_cp0 = state.offset;
                                                    let __pretty_bcp1 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        {
                                                            {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                {
                                                                    return false;
                                                                }
                                                                state.offset += 1;
                                                                __builder.char(b',');
                                                            };
                                                            {
                                                                let __start = state.offset;
                                                                if ::parse_that::scan_ws_block_comments(state).is_none() {
                                                                    return false;
                                                                }
                                                                let __matched = &state.src[__start..state.offset];
                                                                if !__matched.is_empty() {
                                                                    __builder.text(__matched);
                                                                }
                                                            };
                                                            {
                                                                let __start = state.offset;
                                                                if {
                                                                    let __start = state.offset;
                                                                    loop {
                                                                        ::parse_that::scan_ws_block_comments(state);
                                                                        if state.offset >= state.src_bytes.len() {
                                                                            break;
                                                                        }
                                                                        let __b = state.src_bytes[state.offset];
                                                                        if __b == b'!' || __b == b',' || __b == b';' || __b == b'{'
                                                                            || __b == b'}'
                                                                        {
                                                                            break;
                                                                        }
                                                                        state.offset += 1;
                                                                    }
                                                                    if state.offset > __start {
                                                                        Some(
                                                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                                                        )
                                                                    } else {
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
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp4;
                                    __builder.restore(__pretty_bcp5);
                                }
                                __ok
                            };
                            true
                        };
                        {
                            let _ = {
                                let __pretty_cp6 = state.offset;
                                let __pretty_bcp7 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'!')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'!');
                                        };
                                        {
                                            let __start = state.offset;
                                            if ::parse_that::scan_ws_block_comments(state).is_none() {
                                                return false;
                                            }
                                            let __matched = &state.src[__start..state.offset];
                                            if !__matched.is_empty() {
                                                __builder.text(__matched);
                                            }
                                        };
                                        {
                                            let __s = "important";
                                            let __bytes = __s.as_bytes();
                                            let __slc = match state.src_bytes.get(state.offset..) {
                                                Some(s) if s.len() >= 9usize => s,
                                                _ => return false,
                                            };
                                            if &__slc[..9usize] != __bytes {
                                                return false;
                                            }
                                            __builder
                                                .text(&state.src[state.offset..state.offset + 9usize]);
                                            state.offset += 9usize;
                                        };
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp6;
                                    __builder.restore(__pretty_bcp7);
                                }
                                __ok
                            };
                            true
                        };
                        {
                            {
                                let _ = {
                                    let __pretty_cp8 = state.offset;
                                    let __pretty_bcp9 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b';')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b';');
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp8;
                                        __builder.restore(__pretty_bcp9);
                                    }
                                    __ok
                                };
                                true
                            };
                            {
                                let __start = state.offset;
                                if ::parse_that::scan_ws_block_comments(state).is_none() {
                                    return false;
                                }
                                let __matched = &state.src[__start..state.offset];
                                if !__matched.is_empty() {
                                    __builder.text(__matched);
                                }
                            };
                        };
                    };
                    true
                }
            };
            __builder.group_close();
            __pretty_ok
        }
        pub fn declaration_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__declaration_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __blockContent_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.indent_open();
            __builder.hardline();
            let __pretty_ok = {
                {
                    {
                        let mut __rep_count13 = 0usize;
                        while __rep_count13 < 4294967295 {
                            let __rep_cp14 = state.offset;
                            let __iter_cp = if __rep_count13 > 0 {
                                Some(__builder.checkpoint())
                            } else {
                                None
                            };
                            if __rep_count13 > 0 {
                                __builder.hardline();
                            }
                            if !{
                                let __pretty_cp12 = state.offset;
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            let __start = state.offset;
                                            if ::parse_that::scan_ws_block_comments(state).is_none() {
                                                return false;
                                            }
                                            let __matched = &state.src[__start..state.offset];
                                            if !__matched.is_empty() {
                                                __builder.text(__matched);
                                            }
                                        };
                                        {
                                            if !{
                                                let __pretty_cp10 = state.offset;
                                                let __pretty_bcp11 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    if !Self::__declaration_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp10;
                                                    __builder.restore(__pretty_bcp11);
                                                }
                                                __ok
                                            } {
                                                if !Self::__ruleItem_prettify(state, __builder) {
                                                    return false;
                                                }
                                            }
                                        };
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp12;
                                }
                                __ok
                            } {
                                state.offset = __rep_cp14;
                                if let Some(__bcp) = __iter_cp {
                                    __builder.restore(__bcp);
                                }
                                break;
                            }
                            if state.offset == __rep_cp14 {
                                if let Some(__bcp) = __iter_cp {
                                    __builder.restore(__bcp);
                                }
                                break;
                            }
                            __rep_count13 += 1;
                        }
                    };
                    true
                }
            };
            __builder.indent_close();
            __builder.hardline();
            __pretty_ok
        }
        pub fn blockContent_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__blockContent_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __ruleBlock_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'{') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'{');
                        };
                        {
                            {
                                {
                                    let __start = state.offset;
                                    if ::parse_that::scan_ws_block_comments(state).is_none() {
                                        return false;
                                    }
                                    let __matched = &state.src[__start..state.offset];
                                    if !__matched.is_empty() {
                                        __builder.text(__matched);
                                    }
                                };
                                if !Self::__blockContent_prettify(state, __builder) {
                                    return false;
                                }
                            };
                            {
                                let __start = state.offset;
                                if ::parse_that::scan_ws_block_comments(state).is_none() {
                                    return false;
                                }
                                let __matched = &state.src[__start..state.offset];
                                if !__matched.is_empty() {
                                    __builder.text(__matched);
                                }
                            };
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
        }
        pub fn ruleBlock_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__ruleBlock_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __qualifiedRule_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            __builder.indent_open();
            let __pretty_ok = {
                {
                    {
                        {
                            let __start = state.offset;
                            if {
                                let __start = state.offset;
                                loop {
                                    ::parse_that::scan_ws_block_comments(state);
                                    if state.offset >= state.src_bytes.len() {
                                        break;
                                    }
                                    let __b = state.src_bytes[state.offset];
                                    if __b == b';' || __b == b'{' || __b == b'}' {
                                        break;
                                    }
                                    state.offset += 1;
                                }
                                if state.offset > __start {
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
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
                            let __start = state.offset;
                            if ::parse_that::scan_ws_block_comments(state).is_none() {
                                return false;
                            }
                            let __matched = &state.src[__start..state.offset];
                            if !__matched.is_empty() {
                                __builder.text(__matched);
                            }
                        };
                        if !Self::__ruleBlock_prettify(state, __builder) {
                            return false;
                        }
                    };
                    true
                }
            };
            __builder.indent_close();
            __builder.group_close();
            __pretty_ok
        }
        pub fn qualifiedRule_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__qualifiedRule_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __mediaRule_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __s = "@media";
                        let __bytes = __s.as_bytes();
                        let __slc = match state.src_bytes.get(state.offset..) {
                            Some(s) if s.len() >= 6usize => s,
                            _ => return false,
                        };
                        if &__slc[..6usize] != __bytes {
                            return false;
                        }
                        __builder.text(&state.src[state.offset..state.offset + 6usize]);
                        state.offset += 6usize;
                    };
                    {
                        let __start = state.offset;
                        if {
                            let __start = state.offset;
                            loop {
                                ::parse_that::scan_ws_block_comments(state);
                                if state.offset >= state.src_bytes.len() {
                                    break;
                                }
                                let __b = state.src_bytes[state.offset];
                                if __b == b'{' {
                                    break;
                                }
                                state.offset += 1;
                            }
                            if state.offset > __start {
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
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
                    if !Self::__ruleBlock_prettify(state, __builder) {
                        return false;
                    }
                };
                true
            }
        }
        pub fn mediaRule_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__mediaRule_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __supportsRule_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __s = "@supports";
                        let __bytes = __s.as_bytes();
                        let __slc = match state.src_bytes.get(state.offset..) {
                            Some(s) if s.len() >= 9usize => s,
                            _ => return false,
                        };
                        if &__slc[..9usize] != __bytes {
                            return false;
                        }
                        __builder.text(&state.src[state.offset..state.offset + 9usize]);
                        state.offset += 9usize;
                    };
                    {
                        let __start = state.offset;
                        if {
                            let __start = state.offset;
                            loop {
                                ::parse_that::scan_ws_block_comments(state);
                                if state.offset >= state.src_bytes.len() {
                                    break;
                                }
                                let __b = state.src_bytes[state.offset];
                                if __b == b'{' {
                                    break;
                                }
                                state.offset += 1;
                            }
                            if state.offset > __start {
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
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
                    if !Self::__ruleBlock_prettify(state, __builder) {
                        return false;
                    }
                };
                true
            }
        }
        pub fn supportsRule_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__supportsRule_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __fontFaceRule_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __s = "@font-face";
                        let __bytes = __s.as_bytes();
                        let __slc = match state.src_bytes.get(state.offset..) {
                            Some(s) if s.len() >= 10usize => s,
                            _ => return false,
                        };
                        if &__slc[..10usize] != __bytes {
                            return false;
                        }
                        __builder.text(&state.src[state.offset..state.offset + 10usize]);
                        state.offset += 10usize;
                    };
                    {
                        let __start = state.offset;
                        if ::parse_that::scan_ws_block_comments(state).is_none() {
                            return false;
                        }
                        let __matched = &state.src[__start..state.offset];
                        if !__matched.is_empty() {
                            __builder.text(__matched);
                        }
                    };
                    if !Self::__ruleBlock_prettify(state, __builder) {
                        return false;
                    }
                };
                true
            }
        }
        pub fn fontFaceRule_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__fontFaceRule_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __importRule_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __s = "@import";
                        let __bytes = __s.as_bytes();
                        let __slc = match state.src_bytes.get(state.offset..) {
                            Some(s) if s.len() >= 7usize => s,
                            _ => return false,
                        };
                        if &__slc[..7usize] != __bytes {
                            return false;
                        }
                        __builder.text(&state.src[state.offset..state.offset + 7usize]);
                        state.offset += 7usize;
                    };
                    {
                        let __start = state.offset;
                        if {
                            let __start = state.offset;
                            loop {
                                ::parse_that::scan_ws_block_comments(state);
                                if state.offset >= state.src_bytes.len() {
                                    break;
                                }
                                let __b = state.src_bytes[state.offset];
                                if __b == b'!' || __b == b',' || __b == b';' || __b == b'{'
                                    || __b == b'}'
                                {
                                    break;
                                }
                                state.offset += 1;
                            }
                            if state.offset > __start {
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
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
                        let mut __rep_count17 = 0usize;
                        while __rep_count17 < 4294967295 {
                            let __rep_cp18 = state.offset;
                            if !{
                                let __pretty_cp15 = state.offset;
                                let __pretty_bcp16 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b',');
                                        };
                                        {
                                            let __start = state.offset;
                                            if ::parse_that::scan_ws_block_comments(state).is_none() {
                                                return false;
                                            }
                                            let __matched = &state.src[__start..state.offset];
                                            if !__matched.is_empty() {
                                                __builder.text(__matched);
                                            }
                                        };
                                        {
                                            let __start = state.offset;
                                            if {
                                                let __start = state.offset;
                                                loop {
                                                    ::parse_that::scan_ws_block_comments(state);
                                                    if state.offset >= state.src_bytes.len() {
                                                        break;
                                                    }
                                                    let __b = state.src_bytes[state.offset];
                                                    if __b == b'!' || __b == b',' || __b == b';' || __b == b'{'
                                                        || __b == b'}'
                                                    {
                                                        break;
                                                    }
                                                    state.offset += 1;
                                                }
                                                if state.offset > __start {
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
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
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp15;
                                    __builder.restore(__pretty_bcp16);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp18;
                                break;
                            }
                            if state.offset == __rep_cp18 {
                                break;
                            }
                            __rep_count17 += 1;
                        }
                    };
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b';') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b';');
                    };
                };
                true
            }
        }
        pub fn importRule_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__importRule_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __atRuleBody_prettify<'a>(
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
                        b'{' => {
                            if !Self::__ruleBlock_prettify(state, __builder) {
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
        pub fn atRuleBody_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__atRuleBody_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __genericAtRule_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __start = state.offset;
                        if {
                            let __start = state.offset;
                            let __result: Option<()> = (|| {
                                if state.src_bytes.get(state.offset).copied() != Some(b'@')
                                {
                                    return None;
                                }
                                state.offset += 1;
                                {
                                    let __b = *state.src_bytes.get(state.offset)?;
                                    if !(__b.is_ascii_alphabetic()) {
                                        return None;
                                    }
                                    state.offset += 1;
                                }
                                {
                                    let __end = state.src_bytes.len();
                                    let mut __pos = state.offset;
                                    while __pos < __end {
                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                        if (__b == b'-' || (__b >= b'0' && __b <= b'9')
                                            || (__b >= b'A' && __b <= b'Z') || __b == b'_'
                                            || (__b >= b'a' && __b <= b'z'))
                                        {
                                            __pos += 1;
                                        } else {
                                            break;
                                        }
                                    }
                                    state.offset = __pos;
                                }
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
                    {
                        let __start = state.offset;
                        if {
                            let __start = state.offset;
                            loop {
                                ::parse_that::scan_ws_block_comments(state);
                                if state.offset >= state.src_bytes.len() {
                                    break;
                                }
                                let __b = state.src_bytes[state.offset];
                                if __b == b';' || __b == b'{' || __b == b'}' {
                                    break;
                                }
                                state.offset += 1;
                            }
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
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
                    if !Self::__atRuleBody_prettify(state, __builder) {
                        return false;
                    }
                };
                true
            }
        }
        pub fn genericAtRule_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__genericAtRule_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __atRule_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            __builder.indent_open();
            let __pretty_ok = {
                {
                    {
                        if !{
                            let __pretty_cp27 = state.offset;
                            let __pretty_bcp28 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                if !Self::__mediaRule_prettify(state, __builder) {
                                    return false;
                                }
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp27;
                                __builder.restore(__pretty_bcp28);
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp25 = state.offset;
                                    let __pretty_bcp26 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        if !Self::__supportsRule_prettify(state, __builder) {
                                            return false;
                                        }
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp25;
                                        __builder.restore(__pretty_bcp26);
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp23 = state.offset;
                                            let __pretty_bcp24 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                if !Self::__fontFaceRule_prettify(state, __builder) {
                                                    return false;
                                                }
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp23;
                                                __builder.restore(__pretty_bcp24);
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp21 = state.offset;
                                                    let __pretty_bcp22 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        if !Self::__importRule_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp21;
                                                        __builder.restore(__pretty_bcp22);
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp19 = state.offset;
                                                            let __pretty_bcp20 = __builder.checkpoint();
                                                            let __ok = (|| -> bool {
                                                                if !Self::__genericAtRule_prettify(state, __builder) {
                                                                    return false;
                                                                }
                                                                true
                                                            })();
                                                            if !__ok {
                                                                state.offset = __pretty_cp19;
                                                                __builder.restore(__pretty_bcp20);
                                                            }
                                                            __ok
                                                        } {
                                                            return false;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    };
                    true
                }
            };
            __builder.indent_close();
            __builder.group_close();
            __pretty_ok
        }
        pub fn atRule_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__atRule_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __ruleItem_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp29 = state.offset;
                        let __pretty_bcp30 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__qualifiedRule_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp29;
                            __builder.restore(__pretty_bcp30);
                        }
                        __ok
                    } {
                        if !Self::__atRule_prettify(state, __builder) {
                            return false;
                        }
                    }
                };
                true
            }
        }
        pub fn ruleItem_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__ruleItem_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __ruleList_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let mut __rep_count32 = 0usize;
                    while __rep_count32 < 4294967295 {
                        let __rep_cp33 = state.offset;
                        let __iter_cp = if __rep_count32 > 0 {
                            Some(__builder.checkpoint())
                        } else {
                            None
                        };
                        if __rep_count32 > 0 {
                            __builder.hardline();
                        }
                        if !{
                            let __pretty_cp31 = state.offset;
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __start = state.offset;
                                        if ::parse_that::scan_ws_block_comments(state).is_none() {
                                            return false;
                                        }
                                        let __matched = &state.src[__start..state.offset];
                                        if !__matched.is_empty() {
                                            __builder.text(__matched);
                                        }
                                    };
                                    if !Self::__ruleItem_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp31;
                            }
                            __ok
                        } {
                            state.offset = __rep_cp33;
                            if let Some(__bcp) = __iter_cp {
                                __builder.restore(__bcp);
                            }
                            break;
                        }
                        if state.offset == __rep_cp33 {
                            if let Some(__bcp) = __iter_cp {
                                __builder.restore(__bcp);
                            }
                            break;
                        }
                        __rep_count32 += 1;
                    }
                };
                true
            }
        }
        pub fn ruleList_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__ruleList_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __stylesheet_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        {
                            let __start = state.offset;
                            if ::parse_that::scan_ws_block_comments(state).is_none() {
                                return false;
                            }
                            let __matched = &state.src[__start..state.offset];
                            if !__matched.is_empty() {
                                __builder.text(__matched);
                            }
                        };
                        if !Self::__ruleList_prettify(state, __builder) {
                            return false;
                        }
                    };
                    {
                        let __start = state.offset;
                        if ::parse_that::scan_ws_block_comments(state).is_none() {
                            return false;
                        }
                        let __matched = &state.src[__start..state.offset];
                        if !__matched.is_empty() {
                            __builder.text(__matched);
                        }
                    };
                };
                true
            }
        }
        pub fn stylesheet_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__stylesheet_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        pub fn serialize_important<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CssPrettyParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_declaration<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CssPrettyParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_blockContent<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CssPrettyParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_ruleBlock<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CssPrettyParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_qualifiedRule<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CssPrettyParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_mediaRule<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CssPrettyParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_supportsRule<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CssPrettyParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_fontFaceRule<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CssPrettyParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_importRule<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CssPrettyParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_atRuleBody<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CssPrettyParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_genericAtRule<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CssPrettyParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_atRule<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CssPrettyParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_ruleItem<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CssPrettyParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_ruleList<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CssPrettyParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_stylesheet<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CssPrettyParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        fn __dispatch_serialize<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CssPrettyParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            match __v.variant_idx() {
                0u8 => {
                    Self::serialize_important(__v, __ser);
                }
                1u8 => {
                    Self::serialize_declaration(__v, __ser);
                }
                2u8 => {
                    Self::serialize_blockContent(__v, __ser);
                }
                3u8 => {
                    Self::serialize_ruleBlock(__v, __ser);
                }
                4u8 => {
                    Self::serialize_qualifiedRule(__v, __ser);
                }
                5u8 => {
                    Self::serialize_mediaRule(__v, __ser);
                }
                6u8 => {
                    Self::serialize_supportsRule(__v, __ser);
                }
                7u8 => {
                    Self::serialize_fontFaceRule(__v, __ser);
                }
                8u8 => {
                    Self::serialize_importRule(__v, __ser);
                }
                9u8 => {
                    Self::serialize_atRuleBody(__v, __ser);
                }
                10u8 => {
                    Self::serialize_genericAtRule(__v, __ser);
                }
                11u8 => {
                    Self::serialize_atRule(__v, __ser);
                }
                12u8 => {
                    Self::serialize_ruleItem(__v, __ser);
                }
                13u8 => {
                    Self::serialize_ruleList(__v, __ser);
                }
                14u8 => {
                    Self::serialize_stylesheet(__v, __ser);
                }
                _ => {
                    __ser.text(__v.span_text());
                }
            }
        }
        pub fn serialize_compact<'a>(__v: CssPrettyParserNodeView<'a>) -> String {
            let mut __ser = ::bbnf_ser::StringSerializer::new();
            Self::serialize_stylesheet(__v, &mut __ser);
            __ser.finish()
        }
        pub fn serialize<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: CssPrettyParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            Self::serialize_stylesheet(__v, __ser);
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
            crate::runtime::Parsed<'_, Self>,
            crate::runtime::ParseErr,
        > {
            let __input_bytes = input.as_bytes();
            let mut state = __shape_support_CssPrettyParser::ScanState::new();
            let mut tape = crate::runtime::tape::Tape::<
                (),
            >::with_capacity(GRAMMAR_PROFILE.capacity_for(input.len()));
            let root_off = {
                let mut pos: usize = 0;
                let off = parse_CssPrettyParser_stylesheet(
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
                let _ = __shape_support_CssPrettyParser::skip_space(
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
            let tape: crate::runtime::tape::Tape<()> = tape
                .finish(root_off.0)
                .map_err(crate::runtime::ParseErr::Tape)?;
            let tape: crate::runtime::tape::Tape<Self> = unsafe {
                ::core::mem::transmute(tape)
            };
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
pub use __cssprettyparser_emit_impl::*;
