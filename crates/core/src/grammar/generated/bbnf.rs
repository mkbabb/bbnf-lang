//! AUTO-GENERATED from `[workspace.metadata.bbnf.grammars]` — do not edit manually.
//! Regenerate: cargo xtask regen --grammar bbnf

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

pub struct BbnfBootstrap;
mod __bbnfbootstrap_emit_impl {
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
    pub const GRAMMAR_BbnfBootstrap: [&'static str; 1usize] = [
        include_str!(
            concat!(env!("CARGO_MANIFEST_DIR"), "/../../grammar/bbnf/bbnf.bbnf")
        ),
    ];
    static __GRAMMAR_STRUCTURAL_ALPHABET: [u8; 28usize] = [
        33, 34, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 58, 59, 60, 61, 62, 63, 64,
        91, 93, 96, 117, 123, 124, 125, 206,
    ];
    static __GRAMMAR_STRUCTURAL_DIGRAPHS: [(u8, u8); 17usize] = [
        (33, 61),
        (38, 38),
        (42, 47),
        (45, 62),
        (47, 42),
        (47, 47),
        (58, 58),
        (60, 60),
        (60, 61),
        (61, 61),
        (62, 61),
        (62, 62),
        (63, 119),
        (64, 123),
        (117, 56),
        (124, 124),
        (206, 181),
    ];
    pub const GRAMMAR_STRUCTURAL_ALPHABET: &[u8] = &__GRAMMAR_STRUCTURAL_ALPHABET;
    pub const GRAMMAR_STRUCTURAL_DIGRAPHS: &[(u8, u8)] = &__GRAMMAR_STRUCTURAL_DIGRAPHS;
    pub const GRAMMAR_STRUCTURAL_DIGRAPH_MASK: [u64; 4] = [
        17582233548629213184, 1161928703861587969, 0, 16384,
    ];
    pub const GRAMMAR_STRUCTURAL_QUOTE_CLASSES: &[u8] = &[];
    /// AW-III.W6.2 — PHF keyword table.
    ///
    /// Mined literal-led Alt branches, sorted lexicographically.
    /// Binary search dispatches in O(log N) compares; LLVM lowers
    /// the fixed-size table to a balanced compare tree.
    static __PHF_BbnfBootstrap_5_KW: [&[u8]; 3usize] = [b"%", b"*", b"/"];
    /// Per-entry branch discriminant — parallel to [`#kw_ident`].
    /// Entry `i`'s keyword bytes at `#kw_ident[i]` route to the
    /// branch with discriminant `#idx_ident[i]`.
    static __PHF_BbnfBootstrap_5_IDX: [u8; 3usize] = [2, 0, 1];
    /// AW-III.W6.2 — dispatch the mined keyword table for rule
    /// `#rule_id`.
    ///
    /// Returns `Some(branch_idx)` when `bytes` matches a mined
    /// keyword, `None` otherwise. Called from the walker's
    /// AltLinear / ClassifyByte arm to short-circuit the branch
    /// scan to a single binary search.
    #[allow(dead_code)]
    #[inline]
    fn __phf_BbnfBootstrap_dispatch_5(bytes: &[u8]) -> ::core::option::Option<u8> {
        match __PHF_BbnfBootstrap_5_KW.binary_search(&bytes) {
            ::core::result::Result::Ok(idx) => {
                ::core::option::Option::Some(__PHF_BbnfBootstrap_5_IDX[idx])
            }
            ::core::result::Result::Err(_) => ::core::option::Option::None,
        }
    }
    /// AW-III.W6.2 — PHF keyword table.
    ///
    /// Mined literal-led Alt branches, sorted lexicographically.
    /// Binary search dispatches in O(log N) compares; LLVM lowers
    /// the fixed-size table to a balanced compare tree.
    static __PHF_BbnfBootstrap_7_KW: [&[u8]; 6usize] = [
        b"!=",
        b"<",
        b"<=",
        b"==",
        b">",
        b">=",
    ];
    /// Per-entry branch discriminant — parallel to [`#kw_ident`].
    /// Entry `i`'s keyword bytes at `#kw_ident[i]` route to the
    /// branch with discriminant `#idx_ident[i]`.
    static __PHF_BbnfBootstrap_7_IDX: [u8; 6usize] = [1, 4, 2, 0, 5, 3];
    /// AW-III.W6.2 — dispatch the mined keyword table for rule
    /// `#rule_id`.
    ///
    /// Returns `Some(branch_idx)` when `bytes` matches a mined
    /// keyword, `None` otherwise. Called from the walker's
    /// AltLinear / ClassifyByte arm to short-circuit the branch
    /// scan to a single binary search.
    #[allow(dead_code)]
    #[inline]
    fn __phf_BbnfBootstrap_dispatch_7(bytes: &[u8]) -> ::core::option::Option<u8> {
        match __PHF_BbnfBootstrap_7_KW.binary_search(&bytes) {
            ::core::result::Result::Ok(idx) => {
                ::core::option::Option::Some(__PHF_BbnfBootstrap_7_IDX[idx])
            }
            ::core::result::Result::Err(_) => ::core::option::Option::None,
        }
    }
    /// AW-III.W6.2 — PHF keyword table.
    ///
    /// Mined literal-led Alt branches, sorted lexicographically.
    /// Binary search dispatches in O(log N) compares; LLVM lowers
    /// the fixed-size table to a balanced compare tree.
    static __PHF_BbnfBootstrap_8_KW: [&[u8]; 10usize] = [
        b"bool",
        b"f32",
        b"f64",
        b"i32",
        b"i64",
        b"u16",
        b"u32",
        b"u64",
        b"u8",
        b"usize",
    ];
    /// Per-entry branch discriminant — parallel to [`#kw_ident`].
    /// Entry `i`'s keyword bytes at `#kw_ident[i]` route to the
    /// branch with discriminant `#idx_ident[i]`.
    static __PHF_BbnfBootstrap_8_IDX: [u8; 10usize] = [8, 6, 7, 4, 5, 1, 2, 3, 0, 9];
    /// AW-III.W6.2 — dispatch the mined keyword table for rule
    /// `#rule_id`.
    ///
    /// Returns `Some(branch_idx)` when `bytes` matches a mined
    /// keyword, `None` otherwise. Called from the walker's
    /// AltLinear / ClassifyByte arm to short-circuit the branch
    /// scan to a single binary search.
    #[allow(dead_code)]
    #[inline]
    fn __phf_BbnfBootstrap_dispatch_8(bytes: &[u8]) -> ::core::option::Option<u8> {
        match __PHF_BbnfBootstrap_8_KW.binary_search(&bytes) {
            ::core::result::Result::Ok(idx) => {
                ::core::option::Option::Some(__PHF_BbnfBootstrap_8_IDX[idx])
            }
            ::core::result::Result::Err(_) => ::core::option::Option::None,
        }
    }
    /// AW-III.W6.2 — PHF keyword table.
    ///
    /// Mined literal-led Alt branches, sorted lexicographically.
    /// Binary search dispatches in O(log N) compares; LLVM lowers
    /// the fixed-size table to a balanced compare tree.
    static __PHF_BbnfBootstrap_14_KW: [&[u8]; 4usize] = [b"*", b"+", b"?", b"?w"];
    /// Per-entry branch discriminant — parallel to [`#kw_ident`].
    /// Entry `i`'s keyword bytes at `#kw_ident[i]` route to the
    /// branch with discriminant `#idx_ident[i]`.
    static __PHF_BbnfBootstrap_14_IDX: [u8; 4usize] = [2, 3, 1, 0];
    /// AW-III.W6.2 — dispatch the mined keyword table for rule
    /// `#rule_id`.
    ///
    /// Returns `Some(branch_idx)` when `bytes` matches a mined
    /// keyword, `None` otherwise. Called from the walker's
    /// AltLinear / ClassifyByte arm to short-circuit the branch
    /// scan to a single binary search.
    #[allow(dead_code)]
    #[inline]
    fn __phf_BbnfBootstrap_dispatch_14(bytes: &[u8]) -> ::core::option::Option<u8> {
        match __PHF_BbnfBootstrap_14_KW.binary_search(&bytes) {
            ::core::result::Result::Ok(idx) => {
                ::core::option::Option::Some(__PHF_BbnfBootstrap_14_IDX[idx])
            }
            ::core::result::Result::Err(_) => ::core::option::Option::None,
        }
    }
    /// AW-III.W6.2 — PHF keyword table.
    ///
    /// Mined literal-led Alt branches, sorted lexicographically.
    /// Binary search dispatches in O(log N) compares; LLVM lowers
    /// the fixed-size table to a balanced compare tree.
    static __PHF_BbnfBootstrap_15_KW: [&[u8]; 3usize] = [b"-", b"<<", b">>"];
    /// Per-entry branch discriminant — parallel to [`#kw_ident`].
    /// Entry `i`'s keyword bytes at `#kw_ident[i]` route to the
    /// branch with discriminant `#idx_ident[i]`.
    static __PHF_BbnfBootstrap_15_IDX: [u8; 3usize] = [2, 0, 1];
    /// AW-III.W6.2 — dispatch the mined keyword table for rule
    /// `#rule_id`.
    ///
    /// Returns `Some(branch_idx)` when `bytes` matches a mined
    /// keyword, `None` otherwise. Called from the walker's
    /// AltLinear / ClassifyByte arm to short-circuit the branch
    /// scan to a single binary search.
    #[allow(dead_code)]
    #[inline]
    fn __phf_BbnfBootstrap_dispatch_15(bytes: &[u8]) -> ::core::option::Option<u8> {
        match __PHF_BbnfBootstrap_15_KW.binary_search(&bytes) {
            ::core::result::Result::Ok(idx) => {
                ::core::option::Option::Some(__PHF_BbnfBootstrap_15_IDX[idx])
            }
            ::core::result::Result::Err(_) => ::core::option::Option::None,
        }
    }
    /// AW-III.W6.2 — PHF keyword table.
    ///
    /// Mined literal-led Alt branches, sorted lexicographically.
    /// Binary search dispatches in O(log N) compares; LLVM lowers
    /// the fixed-size table to a balanced compare tree.
    static __PHF_BbnfBootstrap_35_KW: [&[u8]; 3usize] = [b"\"", b"(", b"input"];
    /// Per-entry branch discriminant — parallel to [`#kw_ident`].
    /// Entry `i`'s keyword bytes at `#kw_ident[i]` route to the
    /// branch with discriminant `#idx_ident[i]`.
    static __PHF_BbnfBootstrap_35_IDX: [u8; 3usize] = [3, 7, 5];
    /// AW-III.W6.2 — dispatch the mined keyword table for rule
    /// `#rule_id`.
    ///
    /// Returns `Some(branch_idx)` when `bytes` matches a mined
    /// keyword, `None` otherwise. Called from the walker's
    /// AltLinear / ClassifyByte arm to short-circuit the branch
    /// scan to a single binary search.
    #[allow(dead_code)]
    #[inline]
    fn __phf_BbnfBootstrap_dispatch_35(bytes: &[u8]) -> ::core::option::Option<u8> {
        match __PHF_BbnfBootstrap_35_KW.binary_search(&bytes) {
            ::core::result::Result::Ok(idx) => {
                ::core::option::Option::Some(__PHF_BbnfBootstrap_35_IDX[idx])
            }
            ::core::result::Result::Err(_) => ::core::option::Option::None,
        }
    }
    /// AW-III.W6.2 — PHF keyword table.
    ///
    /// Mined literal-led Alt branches, sorted lexicographically.
    /// Binary search dispatches in O(log N) compares; LLVM lowers
    /// the fixed-size table to a balanced compare tree.
    static __PHF_BbnfBootstrap_43_KW: [&[u8]; 7usize] = [
        b"(",
        b"/",
        b"@{",
        b"[",
        b"epsilon",
        b"{",
        b"\xCE\xB5",
    ];
    /// Per-entry branch discriminant — parallel to [`#kw_ident`].
    /// Entry `i`'s keyword bytes at `#kw_ident[i]` route to the
    /// branch with discriminant `#idx_ident[i]`.
    static __PHF_BbnfBootstrap_43_IDX: [u8; 7usize] = [6, 4, 5, 7, 1, 8, 0];
    /// AW-III.W6.2 — dispatch the mined keyword table for rule
    /// `#rule_id`.
    ///
    /// Returns `Some(branch_idx)` when `bytes` matches a mined
    /// keyword, `None` otherwise. Called from the walker's
    /// AltLinear / ClassifyByte arm to short-circuit the branch
    /// scan to a single binary search.
    #[allow(dead_code)]
    #[inline]
    fn __phf_BbnfBootstrap_dispatch_43(bytes: &[u8]) -> ::core::option::Option<u8> {
        match __PHF_BbnfBootstrap_43_KW.binary_search(&bytes) {
            ::core::result::Result::Ok(idx) => {
                ::core::option::Option::Some(__PHF_BbnfBootstrap_43_IDX[idx])
            }
            ::core::result::Result::Err(_) => ::core::option::Option::None,
        }
    }
    /// AW-III.W6.2 — PHF keyword table.
    ///
    /// Mined literal-led Alt branches, sorted lexicographically.
    /// Binary search dispatches in O(log N) compares; LLVM lowers
    /// the fixed-size table to a balanced compare tree.
    static __PHF_BbnfBootstrap_50_KW: [&[u8]; 7usize] = [
        b"@debug",
        b"@host",
        b"@import",
        b"@pretty",
        b"@recover",
        b"@token",
        b"@ws",
    ];
    /// Per-entry branch discriminant — parallel to [`#kw_ident`].
    /// Entry `i`'s keyword bytes at `#kw_ident[i]` route to the
    /// branch with discriminant `#idx_ident[i]`.
    static __PHF_BbnfBootstrap_50_IDX: [u8; 7usize] = [5, 6, 0, 2, 1, 4, 3];
    /// AW-III.W6.2 — dispatch the mined keyword table for rule
    /// `#rule_id`.
    ///
    /// Returns `Some(branch_idx)` when `bytes` matches a mined
    /// keyword, `None` otherwise. Called from the walker's
    /// AltLinear / ClassifyByte arm to short-circuit the branch
    /// scan to a single binary search.
    #[allow(dead_code)]
    #[inline]
    fn __phf_BbnfBootstrap_dispatch_50(bytes: &[u8]) -> ::core::option::Option<u8> {
        match __PHF_BbnfBootstrap_50_KW.binary_search(&bytes) {
            ::core::result::Result::Ok(idx) => {
                ::core::option::Option::Some(__PHF_BbnfBootstrap_50_IDX[idx])
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
    /// AX.W0a.2.l — per-rule dense Pratt precedence LUT.
    ///
    /// One byte per dispatch byte for this Pratt rule's
    /// operator alphabet. Consulted inline by the rule's
    /// emitted `parse_pratt_*` body. See `bbnf::backend::
    /// rust::emitter::precedence` for the bit layout.
    pub const PRECEDENCE_LUT_value_mul: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 0u8, 1u8, 2u8, 0u8, 2u8, 0u8, 1u8,
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
    /// AX.W0a.2.l — per-rule sparse Pratt metadata slice.
    ///
    /// One entry per mined operator for this rule.
    /// Consulted by the rule's emitted `parse_pratt_*`
    /// body when the LUT byte's bit-7 two-byte flag is
    /// set, to resolve the second byte + discriminant.
    pub const PRECEDENCE_ENTRIES_value_mul: &[PrattEntry] = &[
        PrattEntry {
            byte: 43u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 45u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 1u8,
        },
        PrattEntry {
            byte: 42u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 47u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 1u8,
        },
        PrattEntry {
            byte: 37u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 2u8,
        },
    ];
    /// AX.W0a.2.l — per-rule dense Pratt precedence LUT.
    ///
    /// One byte per dispatch byte for this Pratt rule's
    /// operator alphabet. Consulted inline by the rule's
    /// emitted `parse_pratt_*` body. See `bbnf::backend::
    /// rust::emitter::precedence` for the bit layout.
    pub const PRECEDENCE_LUT_value_add: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 0u8, 1u8, 2u8, 0u8, 2u8, 0u8, 1u8,
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
    /// AX.W0a.2.l — per-rule sparse Pratt metadata slice.
    ///
    /// One entry per mined operator for this rule.
    /// Consulted by the rule's emitted `parse_pratt_*`
    /// body when the LUT byte's bit-7 two-byte flag is
    /// set, to resolve the second byte + discriminant.
    pub const PRECEDENCE_ENTRIES_value_add: &[PrattEntry] = &[
        PrattEntry {
            byte: 43u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 45u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 1u8,
        },
        PrattEntry {
            byte: 42u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 47u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 1u8,
        },
        PrattEntry {
            byte: 37u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 2u8,
        },
    ];
    /// AX.W0a.2.l — per-rule dense Pratt precedence LUT.
    ///
    /// One byte per dispatch byte for this Pratt rule's
    /// operator alphabet. Consulted inline by the rule's
    /// emitted `parse_pratt_*` body. See `bbnf::backend::
    /// rust::emitter::precedence` for the bit layout.
    pub const PRECEDENCE_LUT_value_path: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 129u8, 0u8, 0u8, 0u8, 0u8, 0u8,
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
    /// AX.W0a.2.l — per-rule sparse Pratt metadata slice.
    ///
    /// One entry per mined operator for this rule.
    /// Consulted by the rule's emitted `parse_pratt_*`
    /// body when the LUT byte's bit-7 two-byte flag is
    /// set, to resolve the second byte + discriminant.
    pub const PRECEDENCE_ENTRIES_value_path: &[PrattEntry] = &[
        PrattEntry {
            byte: 58u8,
            second_byte: ::core::option::Option::Some(58u8),
            op_discriminant: 0u8,
        },
    ];
    /// AX.W0a.2.l — per-rule dense Pratt precedence LUT.
    ///
    /// One byte per dispatch byte for this Pratt rule's
    /// operator alphabet. Consulted inline by the rule's
    /// emitted `parse_pratt_*` body. See `bbnf::backend::
    /// rust::emitter::precedence` for the bit layout.
    pub const PRECEDENCE_LUT_value_or: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 129u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
    ];
    /// AX.W0a.2.l — per-rule sparse Pratt metadata slice.
    ///
    /// One entry per mined operator for this rule.
    /// Consulted by the rule's emitted `parse_pratt_*`
    /// body when the LUT byte's bit-7 two-byte flag is
    /// set, to resolve the second byte + discriminant.
    pub const PRECEDENCE_ENTRIES_value_or: &[PrattEntry] = &[
        PrattEntry {
            byte: 124u8,
            second_byte: ::core::option::Option::Some(124u8),
            op_discriminant: 0u8,
        },
    ];
    /// AX.W0a.2.l — per-rule dense Pratt precedence LUT.
    ///
    /// One byte per dispatch byte for this Pratt rule's
    /// operator alphabet. Consulted inline by the rule's
    /// emitted `parse_pratt_*` body. See `bbnf::backend::
    /// rust::emitter::precedence` for the bit layout.
    pub const PRECEDENCE_LUT_value_cmp: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 129u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 129u8, 129u8, 129u8,
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
        0u8,
    ];
    /// AX.W0a.2.l — per-rule sparse Pratt metadata slice.
    ///
    /// One entry per mined operator for this rule.
    /// Consulted by the rule's emitted `parse_pratt_*`
    /// body when the LUT byte's bit-7 two-byte flag is
    /// set, to resolve the second byte + discriminant.
    pub const PRECEDENCE_ENTRIES_value_cmp: &[PrattEntry] = &[
        PrattEntry {
            byte: 61u8,
            second_byte: ::core::option::Option::Some(61u8),
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 33u8,
            second_byte: ::core::option::Option::Some(61u8),
            op_discriminant: 1u8,
        },
        PrattEntry {
            byte: 60u8,
            second_byte: ::core::option::Option::Some(61u8),
            op_discriminant: 2u8,
        },
        PrattEntry {
            byte: 62u8,
            second_byte: ::core::option::Option::Some(61u8),
            op_discriminant: 3u8,
        },
        PrattEntry {
            byte: 60u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 4u8,
        },
        PrattEntry {
            byte: 62u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 5u8,
        },
    ];
    /// AX.W0a.2.l — per-rule dense Pratt precedence LUT.
    ///
    /// One byte per dispatch byte for this Pratt rule's
    /// operator alphabet. Consulted inline by the rule's
    /// emitted `parse_pratt_*` body. See `bbnf::backend::
    /// rust::emitter::precedence` for the bit layout.
    pub const PRECEDENCE_LUT_value_and: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 129u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
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
    /// AX.W0a.2.l — per-rule sparse Pratt metadata slice.
    ///
    /// One entry per mined operator for this rule.
    /// Consulted by the rule's emitted `parse_pratt_*`
    /// body when the LUT byte's bit-7 two-byte flag is
    /// set, to resolve the second byte + discriminant.
    pub const PRECEDENCE_ENTRIES_value_and: &[PrattEntry] = &[
        PrattEntry {
            byte: 38u8,
            second_byte: ::core::option::Option::Some(38u8),
            op_discriminant: 0u8,
        },
    ];
    /// AX.W0a.2.l — per-rule dense Pratt precedence LUT.
    ///
    /// One byte per dispatch byte for this Pratt rule's
    /// operator alphabet. Consulted inline by the rule's
    /// emitted `parse_pratt_*` body. See `bbnf::backend::
    /// rust::emitter::precedence` for the bit layout.
    pub const PRECEDENCE_LUT_binary_factor: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 129u8, 0u8, 129u8,
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
        0u8,
    ];
    /// AX.W0a.2.l — per-rule sparse Pratt metadata slice.
    ///
    /// One entry per mined operator for this rule.
    /// Consulted by the rule's emitted `parse_pratt_*`
    /// body when the LUT byte's bit-7 two-byte flag is
    /// set, to resolve the second byte + discriminant.
    pub const PRECEDENCE_ENTRIES_binary_factor: &[PrattEntry] = &[
        PrattEntry {
            byte: 60u8,
            second_byte: ::core::option::Option::Some(60u8),
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 62u8,
            second_byte: ::core::option::Option::Some(62u8),
            op_discriminant: 1u8,
        },
        PrattEntry {
            byte: 45u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 2u8,
        },
    ];
    /// AW-III.W6.5 — aggregate dense Pratt precedence LUT.
    ///
    /// Union of every Pratt rule's packed LUT (last-write-wins
    /// per byte). See
    /// `bbnf::backend::rust::emitter::precedence` for the bit
    /// layout.
    pub const PRECEDENCE_LUT: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 129u8, 0u8, 0u8, 0u8, 1u8, 129u8, 0u8, 0u8, 0u8, 1u8, 2u8, 0u8, 1u8, 0u8,
        1u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 129u8, 0u8, 129u8, 129u8,
        129u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 129u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8,
    ];
    /// AW-III.W6.5 — aggregate sparse Pratt metadata slice.
    ///
    /// Flat union of every rule's mined operator entries.
    pub const PRECEDENCE_ENTRIES: &[PrattEntry] = &[
        PrattEntry {
            byte: 43u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 45u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 1u8,
        },
        PrattEntry {
            byte: 42u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 47u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 1u8,
        },
        PrattEntry {
            byte: 37u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 2u8,
        },
        PrattEntry {
            byte: 43u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 45u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 1u8,
        },
        PrattEntry {
            byte: 42u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 47u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 1u8,
        },
        PrattEntry {
            byte: 37u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 2u8,
        },
        PrattEntry {
            byte: 58u8,
            second_byte: ::core::option::Option::Some(58u8),
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 124u8,
            second_byte: ::core::option::Option::Some(124u8),
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 61u8,
            second_byte: ::core::option::Option::Some(61u8),
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 33u8,
            second_byte: ::core::option::Option::Some(61u8),
            op_discriminant: 1u8,
        },
        PrattEntry {
            byte: 60u8,
            second_byte: ::core::option::Option::Some(61u8),
            op_discriminant: 2u8,
        },
        PrattEntry {
            byte: 62u8,
            second_byte: ::core::option::Option::Some(61u8),
            op_discriminant: 3u8,
        },
        PrattEntry {
            byte: 60u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 4u8,
        },
        PrattEntry {
            byte: 62u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 5u8,
        },
        PrattEntry {
            byte: 38u8,
            second_byte: ::core::option::Option::Some(38u8),
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 60u8,
            second_byte: ::core::option::Option::Some(60u8),
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 62u8,
            second_byte: ::core::option::Option::Some(62u8),
            op_discriminant: 1u8,
        },
        PrattEntry {
            byte: 45u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 2u8,
        },
    ];
    /// AW-III.W6.5 — total mined operator count for this
    /// grammar. Non-zero iff the lift admitted ≥ 1 chain OR the
    /// shape classifier admitted ≥ 1 single-rung Pratt rule.
    pub const PRECEDENCE_OPERATOR_COUNT: usize = 22usize;
    static __DTA_REGEX_0: &str = "0[xX][0-9a-fA-F]+\\w*|[0-9]+\\w*";
    static __DTA_REGEX_1: &str = "[0-9]*\\.[0-9]+([eE][+-]?[0-9]+)?\\w*";
    static __DTA_REGEX_6: &str = "(\\\\.|[^\"\\\\])*";
    static __DTA_REGEX_9: &str = "[_a-zA-Z][_a-zA-Z0-9]*";
    static __DTA_REGEX_36: &str = "[_a-zA-Z][_a-zA-Z0-9-]*";
    static __DTA_REGEX_42: &str = "(\\\\.|[^'\\\\])*";
    static __DTA_REGEX_46: &str = "(\\\\.|[^`\\\\])*";
    static __DTA_REGEX_51: &str = "(\\\\.|[^\\/])+";
    static __DTA_REGEX_55: &str = "[^\\*]*";
    static __DTA_REGEX_62: &str = ".*";
    static __DTA_REGEX_119: &str = "[^)]*";
    /// AY.W4.3 — per-pattern (LAST-byte-set lo, hi) packed
    /// `CharSet128` tuples. `(0, 0)` means narrowing is
    /// disabled for that pattern (suffix not deterministic).
    ///
    /// The adapter consults this when invoked: if the pattern's
    /// entry is non-zero AND the input slice from `pos` does not
    /// contain any byte in the LAST set, the regex cannot
    /// complete a match — skip the DFA walk entirely.
    #[allow(dead_code)]
    pub(crate) const __REGEX_LAST_BYTE_SET_BbnfBootstrap: [(u64, u64); 11] = [
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
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
    fn __regex_scan_BbnfBootstrap(
        pattern: &str,
        input: &[u8],
        pos: usize,
    ) -> ::core::option::Option<u32> {
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_0.as_ptr())
            || pattern == __DTA_REGEX_0
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_BbnfBootstrap[0];
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
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 1;
                                }
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 65 | 66
                                | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77 | 78
                                | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89 | 90
                                | 95 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_1.as_ptr())
            || pattern == __DTA_REGEX_1
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_BbnfBootstrap[1];
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
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 0;
                                }
                                46 => __dfa_state = 4,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 1;
                                }
                                69 | 101 => __dfa_state = 2,
                                65 | 66 | 67 | 68 | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77
                                | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89
                                | 90 | 95 | 97 | 98 | 99 | 100 | 102 | 103 | 104 | 105 | 106
                                | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115 | 116
                                | 117 | 118 | 119 | 120 | 121 | 122 => __dfa_state = 3,
                                _ => break,
                            }
                        }
                        2 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 65 | 66
                                | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77 | 78
                                | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89 | 90
                                | 95 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
                                | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115
                                | 116 | 117 | 118 | 119 | 120 | 121 | 122 => __dfa_state = 3,
                                43 | 45 => __dfa_state = 5,
                                _ => break,
                            }
                        }
                        3 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 65 | 66
                                | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77 | 78
                                | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89 | 90
                                | 95 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
                                | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115
                                | 116 | 117 | 118 | 119 | 120 | 121 | 122 => __dfa_state = 3,
                                _ => break,
                            }
                        }
                        4 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 1;
                                }
                                _ => break,
                            }
                        }
                        5 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 3;
                                }
                                _ => break,
                            }
                        }
                        _ => unsafe { ::core::hint::unreachable_unchecked() }
                    }
                    __dfa_p += 1;
                    match __dfa_state {
                        1 | 2 | 3 => {
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_6.as_ptr())
            || pattern == __DTA_REGEX_6
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_BbnfBootstrap[2];
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_9.as_ptr())
            || pattern == __DTA_REGEX_9
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_BbnfBootstrap[3];
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
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 65 | 66
                                | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77 | 78
                                | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89 | 90
                                | 95 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_36.as_ptr())
            || pattern == __DTA_REGEX_36
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_BbnfBootstrap[4];
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_42.as_ptr())
            || pattern == __DTA_REGEX_42
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_BbnfBootstrap[5];
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
                                | 38 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49 | 50
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_46.as_ptr())
            || pattern == __DTA_REGEX_46
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_BbnfBootstrap[6];
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
                                | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61
                                | 62 | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73
                                | 74 | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85
                                | 86 | 87 | 88 | 89 | 90 | 91 | 93 | 94 | 95 | 97 | 98 | 99
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_51.as_ptr())
            || pattern == __DTA_REGEX_51
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_BbnfBootstrap[7];
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
                                | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 48 | 49 | 50
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
                                | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 1,
                                92 => __dfa_state = 2,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 48 | 49 | 50
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
                                | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 1,
                                92 => __dfa_state = 2,
                                _ => break,
                            }
                        }
                        2 => {
                            match b {
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49
                                | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61
                                | 62 | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73
                                | 74 | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85
                                | 86 | 87 | 88 | 89 | 90 | 91 | 93 | 94 | 95 | 96 | 97 | 98
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
                                | 249 | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 1,
                                92 => __dfa_state = 2,
                                _ => break,
                            }
                        }
                        _ => unsafe { ::core::hint::unreachable_unchecked() }
                    }
                    __dfa_p += 1;
                    match __dfa_state {
                        1 | 2 => {
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_55.as_ptr())
            || pattern == __DTA_REGEX_55
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_BbnfBootstrap[8];
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_62.as_ptr())
            || pattern == __DTA_REGEX_62
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_BbnfBootstrap[9];
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_119.as_ptr())
            || pattern == __DTA_REGEX_119
        {
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_BbnfBootstrap[10];
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
                                | 38 | 39 | 40 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49 | 50
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
    pub(crate) mod __shape_support_BbnfBootstrap {
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
    pub fn parse_hregex_BbnfBootstrap_int_lit<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        let span_lo = *p as u32;
        let Some(match_len) = __regex_scan_BbnfBootstrap(
            "0[xX][0-9a-fA-F]+\\w*|[0-9]+\\w*",
            input,
            *p,
        ) else {
            return Err(crate::runtime::DtaError::Syntax {
                offset: span_lo,
            });
        };
        *p += match_len as usize;
        let span_hi = *p as u32;
        let __i64: i64 = core::str::from_utf8(&input[span_lo as usize..span_hi as usize])
            .ok()
            .and_then(|s| s.parse::<i64>().ok())
            .unwrap_or(0);
        <crate::runtime::bbnf::BbnfStructBuilder<
            'p,
        > as crate::runtime::StructBuilder>::push_leaf_with_i64(builder, __i64);
        Ok(())
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
    pub fn parse_hregex_BbnfBootstrap_float_lit<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        let span_lo = *p as u32;
        let Some(match_len) = __regex_scan_BbnfBootstrap(
            "[0-9]*\\.[0-9]+([eE][+-]?[0-9]+)?\\w*",
            input,
            *p,
        ) else {
            return Err(crate::runtime::DtaError::Syntax {
                offset: span_lo,
            });
        };
        *p += match_len as usize;
        let span_hi = *p as u32;
        let __f64: f64 = core::str::from_utf8(&input[span_lo as usize..span_hi as usize])
            .ok()
            .and_then(|s| s.parse::<f64>().ok())
            .unwrap_or(0.0);
        <crate::runtime::bbnf::BbnfStructBuilder<
            'p,
        > as crate::runtime::StructBuilder>::push_leaf_with_f64(builder, __f64);
        Ok(())
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
    pub fn parse_keyword_BbnfBootstrap_bool_lit<'p>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
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
                    builder.push_leaf_with_unit();
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
                    builder.push_leaf_with_unit();
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
    pub fn parse_flat_BbnfBootstrap_string_lit<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __span_lo: usize = *p;
        let __string_lit_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 3u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("string_lit"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __string_lit_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__string_lit_layout,
        );
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
                    let Some(match_len) = __regex_scan_BbnfBootstrap(
                        "(\\\\.|[^\"\\\\])*",
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
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_leaf_with_str(
                    builder,
                    __span_slice,
                );
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __string_lit_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
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
    pub fn parse_hregex_BbnfBootstrap_value_ident<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        let span_lo = *p as u32;
        let Some(match_len) = __regex_scan_BbnfBootstrap(
            "[_a-zA-Z][_a-zA-Z0-9]*",
            input,
            *p,
        ) else {
            return Err(crate::runtime::DtaError::Syntax {
                offset: span_lo,
            });
        };
        *p += match_len as usize;
        let span_hi = *p as u32;
        <crate::runtime::bbnf::BbnfStructBuilder<
            'p,
        > as crate::runtime::StructBuilder>::push_leaf_with_str(
            builder,
            core::str::from_utf8(&input[span_lo as usize..span_hi as usize])
                .unwrap_or(""),
        );
        Ok(())
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
    pub fn parse_keyword_BbnfBootstrap_mul_op<'p>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let _ = state;
        match first_byte {
            37u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [37u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            42u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [42u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            47u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [47u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
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
    pub fn parse_keyword_BbnfBootstrap_add_op<'p>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let _ = state;
        match first_byte {
            43u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [43u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            45u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [45u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
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
    pub fn parse_keyword_BbnfBootstrap_cmp_op<'p>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let _ = state;
        match first_byte {
            33u8 => {
                if input.len() >= *p + 2usize && input[*p..*p + 2usize] == [33u8, 61u8] {
                    let at = *p;
                    let end = at + 2usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            60u8 => {
                if input.len() >= *p + 2usize && input[*p..*p + 2usize] == [60u8, 61u8] {
                    let at = *p;
                    let end = at + 2usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(());
                }
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [60u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            61u8 => {
                if input.len() >= *p + 2usize && input[*p..*p + 2usize] == [61u8, 61u8] {
                    let at = *p;
                    let end = at + 2usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            62u8 => {
                if input.len() >= *p + 2usize && input[*p..*p + 2usize] == [62u8, 61u8] {
                    let at = *p;
                    let end = at + 2usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(());
                }
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [62u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
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
    pub fn parse_altdispatch_BbnfBootstrap_type_name<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder;
        let first = __shape_support_BbnfBootstrap::skip_space(input, p, state)
            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 8u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("type_name"),
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
                    let end = at + 2usize;
                    if input.len() >= end && input[at..end] == [117u8, 56u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(0u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 3usize;
                    if input.len() >= end && input[at..end] == [117u8, 49u8, 54u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(1u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 3usize;
                    if input.len() >= end && input[at..end] == [117u8, 51u8, 50u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(2u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 3usize;
                    if input.len() >= end && input[at..end] == [117u8, 54u8, 52u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(3u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 3usize;
                    if input.len() >= end && input[at..end] == [105u8, 51u8, 50u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(4u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 3usize;
                    if input.len() >= end && input[at..end] == [105u8, 54u8, 52u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(5u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 3usize;
                    if input.len() >= end && input[at..end] == [102u8, 51u8, 50u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(6u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 3usize;
                    if input.len() >= end && input[at..end] == [102u8, 54u8, 52u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(7u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 4usize;
                    if input.len() >= end
                        && input[at..end] == [98u8, 111u8, 111u8, 108u8]
                    {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(8u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 5usize;
                    if input.len() >= end
                        && input[at..end] == [117u8, 115u8, 105u8, 122u8, 101u8]
                    {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(9u32);
                        break 'try_branches;
                    }
                }
                {
                    if let ::core::option::Option::Some(match_len) = __regex_scan_BbnfBootstrap(
                        "[_a-zA-Z][_a-zA-Z0-9]*",
                        input,
                        *p,
                    ) {
                        *p += match_len as usize;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(10u32);
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
    pub fn parse_hregex_BbnfBootstrap_identifier<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        let span_lo = *p as u32;
        let Some(match_len) = __regex_scan_BbnfBootstrap(
            "[_a-zA-Z][_a-zA-Z0-9-]*",
            input,
            *p,
        ) else {
            return Err(crate::runtime::DtaError::Syntax {
                offset: span_lo,
            });
        };
        *p += match_len as usize;
        let span_hi = *p as u32;
        <crate::runtime::bbnf::BbnfStructBuilder<
            'p,
        > as crate::runtime::StructBuilder>::push_leaf_with_str(
            builder,
            core::str::from_utf8(&input[span_lo as usize..span_hi as usize])
                .unwrap_or(""),
        );
        Ok(())
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
    pub fn parse_keyword_BbnfBootstrap_literal<'p>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
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
                            let __scan_start = *p;
                            let Some(match_len) = __regex_scan_BbnfBootstrap(
                                "(\\\\.|[^\"\\\\])*",
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
                            let __scan_start = *p;
                            let Some(match_len) = __regex_scan_BbnfBootstrap(
                                "(\\\\.|[^'\\\\])*",
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
            96u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [96u8] {
                    let __seq_span_lo = *p;
                    let __seq_builder_checkpoint = builder.checkpoint();
                    let __seq_result: ::core::result::Result<
                        (),
                        crate::runtime::DtaError,
                    > = (|| {
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [96u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                        }
                        {
                            let __scan_start = *p;
                            let Some(match_len) = __regex_scan_BbnfBootstrap(
                                "(\\\\.|[^`\\\\])*",
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
                            if input.len() < end || input[at..end] != [96u8] {
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
    pub fn parse_flat_BbnfBootstrap_regex<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __span_lo: usize = *p;
        let __regex_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 11u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("regex"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __regex_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__regex_layout);
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [47u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
            }
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_BbnfBootstrap(
                        "(\\\\.|[^\\/])+",
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
                if input.len() < end || input[at..end] != [47u8] {
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
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_leaf_with_str(
                    builder,
                    __span_slice,
                );
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __regex_handle,
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
    pub fn parse_flat_BbnfBootstrap_big_comment<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __span_lo: usize = *p;
        let __big_comment_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 12u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("big_comment"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __big_comment_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__big_comment_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let at = *p;
                let end = at + 2usize;
                if input.len() < end || input[at..end] != [47u8, 42u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_BbnfBootstrap(
                        "[^\\*]*",
                        input,
                        *p,
                    ) else {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let at = *p;
                let end = at + 2usize;
                if input.len() < end || input[at..end] != [42u8, 47u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
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
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_leaf_with_str(
                    builder,
                    __span_slice,
                );
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __big_comment_handle,
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
    pub fn parse_flat_BbnfBootstrap_comment<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __span_lo: usize = *p;
        let __comment_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 13u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("comment"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __comment_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__comment_layout);
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let at = *p;
                let end = at + 2usize;
                if input.len() < end || input[at..end] != [47u8, 47u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_BbnfBootstrap(".*", input, *p)
                    else {
                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                            offset: __scan_start as u32,
                        });
                    };
                    *p += match_len as usize;
                }
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
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
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_leaf_with_str(
                    builder,
                    __span_slice,
                );
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __comment_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
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
    pub fn parse_keyword_BbnfBootstrap_modifier<'p>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let _ = state;
        match first_byte {
            42u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [42u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            43u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [43u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            63u8 => {
                if input.len() >= *p + 2usize && input[*p..*p + 2usize] == [63u8, 119u8]
                {
                    let at = *p;
                    let end = at + 2usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(());
                }
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [63u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
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
    pub fn parse_keyword_BbnfBootstrap_binary_operators<'p>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let _ = state;
        match first_byte {
            45u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [45u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            60u8 => {
                if input.len() >= *p + 2usize && input[*p..*p + 2usize] == [60u8, 60u8] {
                    let at = *p;
                    let end = at + 2usize;
                    *p = end;
                    builder.push_leaf_with_unit();
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            62u8 => {
                if input.len() >= *p + 2usize && input[*p..*p + 2usize] == [62u8, 62u8] {
                    let at = *p;
                    let end = at + 2usize;
                    *p = end;
                    builder.push_leaf_with_unit();
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
    pub fn parse_flat_BbnfBootstrap_import_path<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __span_lo: usize = *p;
        let __import_path_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 16u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("import_path"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __import_path_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__import_path_layout,
        );
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
                    let Some(match_len) = __regex_scan_BbnfBootstrap(
                        "(\\\\.|[^\"\\\\])*",
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
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_leaf_with_str(
                    builder,
                    __span_slice,
                );
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __import_path_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2-act.recovery — per-grammar Pratt-shape parse
    /// function, **struct-direct body**. Targets the grammar's
    /// concrete `StructBuilder`.
    ///
    /// Opens a compound for the rule (e.g. `add_expr` →
    /// `SheetsCompoundKind::AddExpr`), dispatches operands +
    /// stamps operator branch tags inline, closes the compound.
    /// Children land in the order
    /// `[lhs_subtree, op_tag, rhs_subtree, op_tag, …]` — the
    /// rule's structural alphabet is preserved verbatim;
    /// associativity-honouring binary-tree reduction is a
    /// consumer-side projection (the generated module exposes
    /// `PRECEDENCE_LUT_<rule>` + `PRECEDENCE_ENTRIES_<rule>` for
    /// that purpose).
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge through the value dispatcher.
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_variables,
        unused_mut,
        unused_assignments
    )]
    pub fn parse_pratt_BbnfBootstrap_value_path<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
        let __value_path_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 17u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("value_path"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __value_path_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__value_path_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let _ = ({
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                parse_hregex_BbnfBootstrap_value_ident(input, p, state, builder)
            })?;
            loop {
                let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let mut lut_byte: u8 = PRECEDENCE_LUT_value_path[op_byte as usize];
                if lut_byte == 0 {
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    op_byte = input.get(*p).copied().unwrap_or(0);
                    lut_byte = PRECEDENCE_LUT_value_path[op_byte as usize];
                }
                if lut_byte == 0 {
                    break;
                }
                let two_byte: u8 = (lut_byte >> 7) & 0x01u8;
                let second_byte: ::core::option::Option<u8> = input.get(*p + 1).copied();
                let (op_width, op_discriminant, op_matched) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    let mut matched: bool = false;
                    for e in PRECEDENCE_ENTRIES_value_path.iter() {
                        if e.byte == op_byte && e.second_byte.is_none() {
                            found_disc = e.op_discriminant;
                            matched = true;
                            break;
                        }
                    }
                    (1u32, found_disc, matched)
                } else {
                    let mut found_disc: u8 = 0u8;
                    let mut matched_two_byte: bool = false;
                    let mut matched_single: bool = false;
                    for e in PRECEDENCE_ENTRIES_value_path.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    if !matched_two_byte {
                        for e in PRECEDENCE_ENTRIES_value_path.iter() {
                            if e.byte == op_byte && e.second_byte.is_none() {
                                found_disc = e.op_discriminant;
                                matched_single = true;
                                break;
                            }
                        }
                    }
                    let width = if matched_two_byte { 2u32 } else { 1u32 };
                    (width, found_disc, matched_two_byte || matched_single)
                };
                if !op_matched {
                    break;
                }
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_branch_tag(
                    builder,
                    op_discriminant as u32,
                );
                *p = (*p).saturating_add(op_width as usize);
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_hregex_BbnfBootstrap_value_ident(input, p, state, builder)
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::end_compound(builder, __value_path_handle);
        __body_result?;
        ::core::result::Result::Ok(())
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
    pub fn parse_flat_BbnfBootstrap_value_input<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __value_input_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 18u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("value_input"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __value_input_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__value_input_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let at = *p;
                let end = at + 5usize;
                if input.len() < end
                    || input[at..end] != [105u8, 110u8, 112u8, 117u8, 116u8]
                {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
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
                            if input.len() < end || input[at..end] != [46u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                            let _ = ({
                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_hregex_BbnfBootstrap_value_ident(
                                    input,
                                    p,
                                    state,
                                    builder,
                                )
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
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __value_input_handle,
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
    pub fn parse_flat_BbnfBootstrap_type_annotation<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __type_annotation_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 19u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("type_annotation"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __type_annotation_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__type_annotation_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [58u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_altdispatch_BbnfBootstrap_type_name(input, p, state, builder)
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __type_annotation_handle,
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
    pub fn parse_scalar_BbnfBootstrap_lhs<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        {
            let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            parse_hregex_BbnfBootstrap_identifier(input, p, state, builder)
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
    pub fn parse_flat_BbnfBootstrap_import_items<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __import_items_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 21u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("import_items"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __import_items_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__import_items_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [123u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_hregex_BbnfBootstrap_identifier(input, p, state, builder)
                })?;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
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
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [44u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let _ = ({
                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_hregex_BbnfBootstrap_identifier(
                                    input,
                                    p,
                                    state,
                                    builder,
                                )
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
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
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
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __import_items_handle,
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
    pub fn parse_flat_BbnfBootstrap_pretty_hint<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __pretty_hint_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 22u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("pretty_hint"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __pretty_hint_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__pretty_hint_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_hregex_BbnfBootstrap_identifier(input, p, state, builder)
                })?;
            }
            {
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
                            if input.len() < end || input[at..end] != [40u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                            {
                                let __scan_start = *p;
                                let Some(match_len) = __regex_scan_BbnfBootstrap(
                                    "[^)]*",
                                    input,
                                    *p,
                                ) else {
                                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                        offset: __scan_start as u32,
                                    });
                                };
                                *p += match_len as usize;
                            }
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [41u8] {
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
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __pretty_hint_handle,
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
    pub fn parse_flat_BbnfBootstrap_token_directive<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __token_directive_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 23u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("token_directive"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __token_directive_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__token_directive_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let at = *p;
                let end = at + 6usize;
                if input.len() < end
                    || input[at..end] != [64u8, 116u8, 111u8, 107u8, 101u8, 110u8]
                {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_hregex_BbnfBootstrap_identifier(input, p, state, builder)
                })?;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
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
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __token_directive_handle,
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
    pub fn parse_flat_BbnfBootstrap_debug_directive<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __debug_directive_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 24u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("debug_directive"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __debug_directive_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__debug_directive_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let at = *p;
                let end = at + 6usize;
                if input.len() < end
                    || input[at..end] != [64u8, 100u8, 101u8, 98u8, 117u8, 103u8]
                {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
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
                            let _ = ({
                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_hregex_BbnfBootstrap_identifier(
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
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
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
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __debug_directive_handle,
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
    pub fn parse_flat_BbnfBootstrap_host_directive<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __host_directive_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 25u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("host_directive"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __host_directive_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__host_directive_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let at = *p;
                let end = at + 5usize;
                if input.len() < end
                    || input[at..end] != [64u8, 104u8, 111u8, 115u8, 116u8]
                {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_hregex_BbnfBootstrap_identifier(input, p, state, builder)
                })?;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
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
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [58u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let _ = ({
                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_altdispatch_BbnfBootstrap_type_name(
                                    input,
                                    p,
                                    state,
                                    builder,
                                )
                            })?;
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
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
            {
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
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __host_directive_handle,
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
    pub fn parse_flat_BbnfBootstrap_ws_directive<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __ws_directive_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 26u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("ws_directive"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __ws_directive_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__ws_directive_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let at = *p;
                let end = at + 3usize;
                if input.len() < end || input[at..end] != [64u8, 119u8, 115u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_flat_BbnfBootstrap_regex(input, p, state, builder)
                })?;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
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
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __ws_directive_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2-act.recovery — per-grammar Pratt-shape parse
    /// function, **struct-direct body**. Targets the grammar's
    /// concrete `StructBuilder`.
    ///
    /// Opens a compound for the rule (e.g. `add_expr` →
    /// `SheetsCompoundKind::AddExpr`), dispatches operands +
    /// stamps operator branch tags inline, closes the compound.
    /// Children land in the order
    /// `[lhs_subtree, op_tag, rhs_subtree, op_tag, …]` — the
    /// rule's structural alphabet is preserved verbatim;
    /// associativity-honouring binary-tree reduction is a
    /// consumer-side projection (the generated module exposes
    /// `PRECEDENCE_LUT_<rule>` + `PRECEDENCE_ENTRIES_<rule>` for
    /// that purpose).
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge through the value dispatcher.
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_variables,
        unused_mut,
        unused_assignments
    )]
    pub fn parse_pratt_BbnfBootstrap_value_mul<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
        let __value_mul_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 27u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("value_mul"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __value_mul_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__value_mul_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let _ = ({
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                parse_altdispatch_BbnfBootstrap_value_unary(input, p, state, builder)
            })?;
            loop {
                let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let mut lut_byte: u8 = PRECEDENCE_LUT_value_mul[op_byte as usize];
                if lut_byte == 0 {
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    op_byte = input.get(*p).copied().unwrap_or(0);
                    lut_byte = PRECEDENCE_LUT_value_mul[op_byte as usize];
                }
                if lut_byte == 0 {
                    break;
                }
                let two_byte: u8 = (lut_byte >> 7) & 0x01u8;
                let second_byte: ::core::option::Option<u8> = input.get(*p + 1).copied();
                let (op_width, op_discriminant, op_matched) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    let mut matched: bool = false;
                    for e in PRECEDENCE_ENTRIES_value_mul.iter() {
                        if e.byte == op_byte && e.second_byte.is_none() {
                            found_disc = e.op_discriminant;
                            matched = true;
                            break;
                        }
                    }
                    (1u32, found_disc, matched)
                } else {
                    let mut found_disc: u8 = 0u8;
                    let mut matched_two_byte: bool = false;
                    let mut matched_single: bool = false;
                    for e in PRECEDENCE_ENTRIES_value_mul.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    if !matched_two_byte {
                        for e in PRECEDENCE_ENTRIES_value_mul.iter() {
                            if e.byte == op_byte && e.second_byte.is_none() {
                                found_disc = e.op_discriminant;
                                matched_single = true;
                                break;
                            }
                        }
                    }
                    let width = if matched_two_byte { 2u32 } else { 1u32 };
                    (width, found_disc, matched_two_byte || matched_single)
                };
                if !op_matched {
                    break;
                }
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_branch_tag(
                    builder,
                    op_discriminant as u32,
                );
                *p = (*p).saturating_add(op_width as usize);
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_altdispatch_BbnfBootstrap_value_unary(input, p, state, builder)
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::end_compound(builder, __value_mul_handle);
        __body_result?;
        ::core::result::Result::Ok(())
    }
    /// AZ-I.W2-act.recovery — per-grammar Pratt-shape parse
    /// function, **struct-direct body**. Targets the grammar's
    /// concrete `StructBuilder`.
    ///
    /// Opens a compound for the rule (e.g. `add_expr` →
    /// `SheetsCompoundKind::AddExpr`), dispatches operands +
    /// stamps operator branch tags inline, closes the compound.
    /// Children land in the order
    /// `[lhs_subtree, op_tag, rhs_subtree, op_tag, …]` — the
    /// rule's structural alphabet is preserved verbatim;
    /// associativity-honouring binary-tree reduction is a
    /// consumer-side projection (the generated module exposes
    /// `PRECEDENCE_LUT_<rule>` + `PRECEDENCE_ENTRIES_<rule>` for
    /// that purpose).
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge through the value dispatcher.
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_variables,
        unused_mut,
        unused_assignments
    )]
    pub fn parse_pratt_BbnfBootstrap_value_or<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
        let __value_or_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 28u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("value_or"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __value_or_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__value_or_layout);
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let _ = ({
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                parse_pratt_BbnfBootstrap_value_and(input, p, state, builder)
            })?;
            loop {
                let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let mut lut_byte: u8 = PRECEDENCE_LUT_value_or[op_byte as usize];
                if lut_byte == 0 {
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    op_byte = input.get(*p).copied().unwrap_or(0);
                    lut_byte = PRECEDENCE_LUT_value_or[op_byte as usize];
                }
                if lut_byte == 0 {
                    break;
                }
                let two_byte: u8 = (lut_byte >> 7) & 0x01u8;
                let second_byte: ::core::option::Option<u8> = input.get(*p + 1).copied();
                let (op_width, op_discriminant, op_matched) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    let mut matched: bool = false;
                    for e in PRECEDENCE_ENTRIES_value_or.iter() {
                        if e.byte == op_byte && e.second_byte.is_none() {
                            found_disc = e.op_discriminant;
                            matched = true;
                            break;
                        }
                    }
                    (1u32, found_disc, matched)
                } else {
                    let mut found_disc: u8 = 0u8;
                    let mut matched_two_byte: bool = false;
                    let mut matched_single: bool = false;
                    for e in PRECEDENCE_ENTRIES_value_or.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    if !matched_two_byte {
                        for e in PRECEDENCE_ENTRIES_value_or.iter() {
                            if e.byte == op_byte && e.second_byte.is_none() {
                                found_disc = e.op_discriminant;
                                matched_single = true;
                                break;
                            }
                        }
                    }
                    let width = if matched_two_byte { 2u32 } else { 1u32 };
                    (width, found_disc, matched_two_byte || matched_single)
                };
                if !op_matched {
                    break;
                }
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_branch_tag(
                    builder,
                    op_discriminant as u32,
                );
                *p = (*p).saturating_add(op_width as usize);
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_pratt_BbnfBootstrap_value_and(input, p, state, builder)
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::end_compound(builder, __value_or_handle);
        __body_result?;
        ::core::result::Result::Ok(())
    }
    /// AZ-I.W2-act.recovery — per-grammar Pratt-shape parse
    /// function, **struct-direct body**. Targets the grammar's
    /// concrete `StructBuilder`.
    ///
    /// Opens a compound for the rule (e.g. `add_expr` →
    /// `SheetsCompoundKind::AddExpr`), dispatches operands +
    /// stamps operator branch tags inline, closes the compound.
    /// Children land in the order
    /// `[lhs_subtree, op_tag, rhs_subtree, op_tag, …]` — the
    /// rule's structural alphabet is preserved verbatim;
    /// associativity-honouring binary-tree reduction is a
    /// consumer-side projection (the generated module exposes
    /// `PRECEDENCE_LUT_<rule>` + `PRECEDENCE_ENTRIES_<rule>` for
    /// that purpose).
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge through the value dispatcher.
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_variables,
        unused_mut,
        unused_assignments
    )]
    pub fn parse_pratt_BbnfBootstrap_value_add<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
        let __value_add_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 29u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("value_add"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __value_add_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__value_add_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let _ = ({
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                parse_pratt_BbnfBootstrap_value_mul(input, p, state, builder)
            })?;
            loop {
                let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let mut lut_byte: u8 = PRECEDENCE_LUT_value_add[op_byte as usize];
                if lut_byte == 0 {
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    op_byte = input.get(*p).copied().unwrap_or(0);
                    lut_byte = PRECEDENCE_LUT_value_add[op_byte as usize];
                }
                if lut_byte == 0 {
                    break;
                }
                let two_byte: u8 = (lut_byte >> 7) & 0x01u8;
                let second_byte: ::core::option::Option<u8> = input.get(*p + 1).copied();
                let (op_width, op_discriminant, op_matched) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    let mut matched: bool = false;
                    for e in PRECEDENCE_ENTRIES_value_add.iter() {
                        if e.byte == op_byte && e.second_byte.is_none() {
                            found_disc = e.op_discriminant;
                            matched = true;
                            break;
                        }
                    }
                    (1u32, found_disc, matched)
                } else {
                    let mut found_disc: u8 = 0u8;
                    let mut matched_two_byte: bool = false;
                    let mut matched_single: bool = false;
                    for e in PRECEDENCE_ENTRIES_value_add.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    if !matched_two_byte {
                        for e in PRECEDENCE_ENTRIES_value_add.iter() {
                            if e.byte == op_byte && e.second_byte.is_none() {
                                found_disc = e.op_discriminant;
                                matched_single = true;
                                break;
                            }
                        }
                    }
                    let width = if matched_two_byte { 2u32 } else { 1u32 };
                    (width, found_disc, matched_two_byte || matched_single)
                };
                if !op_matched {
                    break;
                }
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_branch_tag(
                    builder,
                    op_discriminant as u32,
                );
                *p = (*p).saturating_add(op_width as usize);
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_pratt_BbnfBootstrap_value_mul(input, p, state, builder)
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::end_compound(builder, __value_add_handle);
        __body_result?;
        ::core::result::Result::Ok(())
    }
    /// AZ-I.W2-act.recovery — per-grammar Pratt-shape parse
    /// function, **struct-direct body**. Targets the grammar's
    /// concrete `StructBuilder`.
    ///
    /// Opens a compound for the rule (e.g. `add_expr` →
    /// `SheetsCompoundKind::AddExpr`), dispatches operands +
    /// stamps operator branch tags inline, closes the compound.
    /// Children land in the order
    /// `[lhs_subtree, op_tag, rhs_subtree, op_tag, …]` — the
    /// rule's structural alphabet is preserved verbatim;
    /// associativity-honouring binary-tree reduction is a
    /// consumer-side projection (the generated module exposes
    /// `PRECEDENCE_LUT_<rule>` + `PRECEDENCE_ENTRIES_<rule>` for
    /// that purpose).
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge through the value dispatcher.
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_variables,
        unused_mut,
        unused_assignments
    )]
    pub fn parse_pratt_BbnfBootstrap_value_cmp<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
        let __value_cmp_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 30u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("value_cmp"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __value_cmp_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__value_cmp_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let _ = ({
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                parse_pratt_BbnfBootstrap_value_add(input, p, state, builder)
            })?;
            loop {
                let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let mut lut_byte: u8 = PRECEDENCE_LUT_value_cmp[op_byte as usize];
                if lut_byte == 0 {
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    op_byte = input.get(*p).copied().unwrap_or(0);
                    lut_byte = PRECEDENCE_LUT_value_cmp[op_byte as usize];
                }
                if lut_byte == 0 {
                    break;
                }
                let two_byte: u8 = (lut_byte >> 7) & 0x01u8;
                let second_byte: ::core::option::Option<u8> = input.get(*p + 1).copied();
                let (op_width, op_discriminant, op_matched) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    let mut matched: bool = false;
                    for e in PRECEDENCE_ENTRIES_value_cmp.iter() {
                        if e.byte == op_byte && e.second_byte.is_none() {
                            found_disc = e.op_discriminant;
                            matched = true;
                            break;
                        }
                    }
                    (1u32, found_disc, matched)
                } else {
                    let mut found_disc: u8 = 0u8;
                    let mut matched_two_byte: bool = false;
                    let mut matched_single: bool = false;
                    for e in PRECEDENCE_ENTRIES_value_cmp.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    if !matched_two_byte {
                        for e in PRECEDENCE_ENTRIES_value_cmp.iter() {
                            if e.byte == op_byte && e.second_byte.is_none() {
                                found_disc = e.op_discriminant;
                                matched_single = true;
                                break;
                            }
                        }
                    }
                    let width = if matched_two_byte { 2u32 } else { 1u32 };
                    (width, found_disc, matched_two_byte || matched_single)
                };
                if !op_matched {
                    break;
                }
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_branch_tag(
                    builder,
                    op_discriminant as u32,
                );
                *p = (*p).saturating_add(op_width as usize);
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_pratt_BbnfBootstrap_value_add(input, p, state, builder)
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::end_compound(builder, __value_cmp_handle);
        __body_result?;
        ::core::result::Result::Ok(())
    }
    /// AZ-I.W2-act.recovery — per-grammar Pratt-shape parse
    /// function, **struct-direct body**. Targets the grammar's
    /// concrete `StructBuilder`.
    ///
    /// Opens a compound for the rule (e.g. `add_expr` →
    /// `SheetsCompoundKind::AddExpr`), dispatches operands +
    /// stamps operator branch tags inline, closes the compound.
    /// Children land in the order
    /// `[lhs_subtree, op_tag, rhs_subtree, op_tag, …]` — the
    /// rule's structural alphabet is preserved verbatim;
    /// associativity-honouring binary-tree reduction is a
    /// consumer-side projection (the generated module exposes
    /// `PRECEDENCE_LUT_<rule>` + `PRECEDENCE_ENTRIES_<rule>` for
    /// that purpose).
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge through the value dispatcher.
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_variables,
        unused_mut,
        unused_assignments
    )]
    pub fn parse_pratt_BbnfBootstrap_value_and<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
        let __value_and_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 31u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("value_and"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __value_and_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__value_and_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let _ = ({
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                parse_pratt_BbnfBootstrap_value_cmp(input, p, state, builder)
            })?;
            loop {
                let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let mut lut_byte: u8 = PRECEDENCE_LUT_value_and[op_byte as usize];
                if lut_byte == 0 {
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    op_byte = input.get(*p).copied().unwrap_or(0);
                    lut_byte = PRECEDENCE_LUT_value_and[op_byte as usize];
                }
                if lut_byte == 0 {
                    break;
                }
                let two_byte: u8 = (lut_byte >> 7) & 0x01u8;
                let second_byte: ::core::option::Option<u8> = input.get(*p + 1).copied();
                let (op_width, op_discriminant, op_matched) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    let mut matched: bool = false;
                    for e in PRECEDENCE_ENTRIES_value_and.iter() {
                        if e.byte == op_byte && e.second_byte.is_none() {
                            found_disc = e.op_discriminant;
                            matched = true;
                            break;
                        }
                    }
                    (1u32, found_disc, matched)
                } else {
                    let mut found_disc: u8 = 0u8;
                    let mut matched_two_byte: bool = false;
                    let mut matched_single: bool = false;
                    for e in PRECEDENCE_ENTRIES_value_and.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    if !matched_two_byte {
                        for e in PRECEDENCE_ENTRIES_value_and.iter() {
                            if e.byte == op_byte && e.second_byte.is_none() {
                                found_disc = e.op_discriminant;
                                matched_single = true;
                                break;
                            }
                        }
                    }
                    let width = if matched_two_byte { 2u32 } else { 1u32 };
                    (width, found_disc, matched_two_byte || matched_single)
                };
                if !op_matched {
                    break;
                }
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_branch_tag(
                    builder,
                    op_discriminant as u32,
                );
                *p = (*p).saturating_add(op_width as usize);
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_pratt_BbnfBootstrap_value_cmp(input, p, state, builder)
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::end_compound(builder, __value_and_handle);
        __body_result?;
        ::core::result::Result::Ok(())
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
    pub fn parse_flat_BbnfBootstrap_value_closure<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __value_closure_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 32u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("value_closure"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __value_closure_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__value_closure_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [124u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
            }
            {
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_hregex_BbnfBootstrap_value_ident(input, p, state, builder)
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
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [44u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let _ = ({
                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_hregex_BbnfBootstrap_value_ident(
                                    input,
                                    p,
                                    state,
                                    builder,
                                )
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
            {
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [124u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
            }
            {
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_wrap_BbnfBootstrap_value_expr(input, p, state, builder)
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __value_closure_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
                ::core::result::Result::Err(__err)
            }
        }
    }
    /// AZ-I.W2-act.B3 — per-grammar ArgList-shape parse function,
    /// **struct-direct body**.
    ///
    /// Opens a compound on the grammar's StructBuilder
    /// (`begin_compound(&__layout)`), walks the head + parens +
    /// arg positions, and closes via `end_compound(handle)`. The
    /// builder routes the (LayoutKind, rule_name) to its concrete
    /// Function frame variant (CSS L4 — calc / min / max / clamp
    /// / var / env / url / gradient / transform / etc.).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_arglist_BbnfBootstrap_value_fn_call<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 33u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("value_fn_call"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __arglist_checkpoint = <crate::runtime::bbnf::BbnfStructBuilder<
            'p,
        > as crate::runtime::StructBuilder>::checkpoint(builder);
        let __handle = <crate::runtime::bbnf::BbnfStructBuilder<
            'p,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__layout);
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let _ = ({
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                parse_pratt_BbnfBootstrap_value_path(input, p, state, builder)
            })?;
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [40u8] {
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: at as u32,
                });
            }
            *p = end;
            loop {
                let __save = *p;
                let __res: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
                    let _ = ({
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_wrap_BbnfBootstrap_value_expr(input, p, state, builder)
                    })?;
                    loop {
                        let __save = *p;
                        let __res: ::core::result::Result<
                            (),
                            crate::runtime::DtaError,
                        > = (|| {
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [44u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let _ = ({
                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_wrap_BbnfBootstrap_value_expr(
                                    input,
                                    p,
                                    state,
                                    builder,
                                )
                            })?;
                            Ok(())
                        })();
                        if __res.is_err() {
                            *p = __save;
                            break;
                        }
                    }
                    Ok(())
                })();
                if __res.is_err() {
                    *p = __save;
                    break;
                }
            }
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [41u8] {
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: at as u32,
                });
            }
            *p = end;
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::bbnf::BbnfStructBuilder<
                    'p,
                > as crate::runtime::StructBuilder>::end_compound(builder, __handle);
                Ok(())
            }
            ::core::result::Result::Err(__err) => {
                <crate::runtime::bbnf::BbnfStructBuilder<
                    'p,
                > as crate::runtime::StructBuilder>::rollback(
                    builder,
                    __arglist_checkpoint,
                );
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
    pub fn parse_wrap_BbnfBootstrap_value_expr<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let first = __shape_support_BbnfBootstrap::skip_space(input, p, state)
            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        'try_branches: loop {
            match first {
                124u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_flat_BbnfBootstrap_value_closure(
                        input,
                        p,
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
                _ => {}
            }
            {
                let attempt_p = *p;
                let attempt_builder = builder.checkpoint();
                match parse_pratt_BbnfBootstrap_value_or(input, p, state, builder) {
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
            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                offset: *p as u32,
            });
        }
        ::core::result::Result::Ok(())
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
    pub fn parse_altdispatch_BbnfBootstrap_value_atom<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder;
        let first = __shape_support_BbnfBootstrap::skip_space(input, p, state)
            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 35u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("value_atom"),
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
                    match {
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_hregex_BbnfBootstrap_int_lit(input, p, state, builder)
                    } {
                        Ok(_) => {
                            builder.push_branch_tag(0u32);
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
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_hregex_BbnfBootstrap_float_lit(input, p, state, builder)
                    } {
                        Ok(_) => {
                            builder.push_branch_tag(1u32);
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
                        let __first = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                                offset: *p as u32,
                            })?;
                        parse_keyword_BbnfBootstrap_bool_lit(
                            input,
                            p,
                            __first,
                            state,
                            builder,
                        )
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
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_flat_BbnfBootstrap_string_lit(input, p, state, builder)
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
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_arglist_BbnfBootstrap_value_fn_call(
                            input,
                            p,
                            state,
                            builder,
                        )
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
                {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match {
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_flat_BbnfBootstrap_value_input(input, p, state, builder)
                    } {
                        Ok(_) => {
                            builder.push_branch_tag(5u32);
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
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_pratt_BbnfBootstrap_value_path(input, p, state, builder)
                    } {
                        Ok(_) => {
                            builder.push_branch_tag(6u32);
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
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        let _ = ({
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            parse_wrap_BbnfBootstrap_value_expr(input, p, state, builder)
                        })?;
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
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
                            builder.push_branch_tag(7u32);
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
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
    pub fn parse_altdispatch_BbnfBootstrap_value_unary<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder;
        let first = __shape_support_BbnfBootstrap::skip_space(input, p, state)
            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 36u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("value_unary"),
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
                        'try_branches: loop {
                            {
                                let __alt_save_p = *p;
                                let __alt_builder_checkpoint = builder.checkpoint();
                                let __alt_result: ::core::result::Result<
                                    (),
                                    crate::runtime::DtaError,
                                > = (|| {
                                    {
                                        let at = *p;
                                        let end = at + 1usize;
                                        if input.len() < end || input[at..end] != [33u8] {
                                            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                                offset: at as u32,
                                            });
                                        }
                                        *p = end;
                                    }
                                    ::core::result::Result::Ok(())
                                })();
                                match __alt_result {
                                    ::core::result::Result::Ok(()) => {
                                        builder.commit(__alt_builder_checkpoint);
                                        break 'try_branches;
                                    }
                                    ::core::result::Result::Err(_) => {
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
                                    {
                                        let at = *p;
                                        let end = at + 1usize;
                                        if input.len() < end || input[at..end] != [45u8] {
                                            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                                offset: at as u32,
                                            });
                                        }
                                        *p = end;
                                    }
                                    ::core::result::Result::Ok(())
                                })();
                                match __alt_result {
                                    ::core::result::Result::Ok(()) => {
                                        builder.commit(__alt_builder_checkpoint);
                                        break 'try_branches;
                                    }
                                    ::core::result::Result::Err(_) => {
                                        *p = __alt_save_p;
                                        builder.rollback(__alt_builder_checkpoint);
                                    }
                                }
                            }
                            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                offset: *p as u32,
                            });
                        }
                        let _ = ({
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            parse_altdispatch_BbnfBootstrap_value_atom(
                                input,
                                p,
                                state,
                                builder,
                            )
                        })?;
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
                    match {
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_altdispatch_BbnfBootstrap_value_atom(
                            input,
                            p,
                            state,
                            builder,
                        )
                    } {
                        Ok(_) => {
                            builder.push_branch_tag(1u32);
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
    pub fn parse_flat_BbnfBootstrap_import_directive<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __import_directive_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 37u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("import_directive"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __import_directive_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__import_directive_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let at = *p;
                let end = at + 7usize;
                if input.len() < end
                    || input[at..end] != [64u8, 105u8, 109u8, 112u8, 111u8, 114u8, 116u8]
                {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                'try_branches: loop {
                    {
                        let __alt_save_p = *p;
                        let __alt_builder_checkpoint = builder.checkpoint();
                        let __alt_result: ::core::result::Result<
                            (),
                            crate::runtime::DtaError,
                        > = (|| {
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let _ = ({
                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_flat_BbnfBootstrap_import_items(
                                    input,
                                    p,
                                    state,
                                    builder,
                                )
                            })?;
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let at = *p;
                            let end = at + 4usize;
                            if input.len() < end
                                || input[at..end] != [102u8, 114u8, 111u8, 109u8]
                            {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let _ = ({
                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_flat_BbnfBootstrap_import_path(
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
                    {
                        let __alt_save_p = *p;
                        let __alt_builder_checkpoint = builder.checkpoint();
                        let __alt_result: ::core::result::Result<
                            (),
                            crate::runtime::DtaError,
                        > = (|| {
                            let _ = ({
                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_flat_BbnfBootstrap_import_path(
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
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
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
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __import_directive_handle,
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
    pub fn parse_flat_BbnfBootstrap_pretty_directive<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __pretty_directive_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 38u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("pretty_directive"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __pretty_directive_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__pretty_directive_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let at = *p;
                let end = at + 7usize;
                if input.len() < end
                    || input[at..end] != [64u8, 112u8, 114u8, 101u8, 116u8, 116u8, 121u8]
                {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
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
                            let _ = ({
                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_hregex_BbnfBootstrap_identifier(
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
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
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
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let _ = ({
                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_flat_BbnfBootstrap_pretty_hint(
                                    input,
                                    p,
                                    state,
                                    builder,
                                )
                            })?;
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
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
            {
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
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __pretty_directive_handle,
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
    pub fn parse_flat_BbnfBootstrap_alternation<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __alternation_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 39u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("alternation"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __alternation_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__alternation_layout,
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
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let _ = ({
                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_flat_BbnfBootstrap_concatenation(
                                    input,
                                    p,
                                    state,
                                    builder,
                                )
                            })?;
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
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
                <crate::runtime::bbnf::BbnfStructBuilder<
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
    pub fn parse_flat_BbnfBootstrap_call_arg<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __call_arg_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 40u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("call_arg"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __call_arg_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__call_arg_layout);
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
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let _ = ({
                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_pratt_BbnfBootstrap_binary_factor(
                                    input,
                                    p,
                                    state,
                                    builder,
                                )
                            })?;
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
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
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __call_arg_handle,
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
    pub fn parse_flat_BbnfBootstrap_concatenation<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __concatenation_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 41u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("concatenation"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __concatenation_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__concatenation_layout,
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
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let _ = ({
                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_pratt_BbnfBootstrap_binary_factor(
                                    input,
                                    p,
                                    state,
                                    builder,
                                )
                            })?;
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
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
                <crate::runtime::bbnf::BbnfStructBuilder<
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
    pub fn parse_flat_BbnfBootstrap_closure<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __closure_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 42u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("closure"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __closure_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__closure_layout);
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [124u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
            }
            {
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_hregex_BbnfBootstrap_identifier(input, p, state, builder)
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
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [44u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let _ = ({
                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_hregex_BbnfBootstrap_identifier(
                                    input,
                                    p,
                                    state,
                                    builder,
                                )
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
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [124u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_wrap_BbnfBootstrap_rhs(input, p, state, builder)
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __closure_handle,
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
    pub fn parse_altdispatch_BbnfBootstrap_term<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder;
        let first = __shape_support_BbnfBootstrap::skip_space(input, p, state)
            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 43u32 as ::bbnf_ir::RuleId,
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
                    let at = *p;
                    let end = at + 2usize;
                    if input.len() >= end && input[at..end] == [206u8, 181u8] {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(0u32);
                        break 'try_branches;
                    }
                }
                {
                    let at = *p;
                    let end = at + 7usize;
                    if input.len() >= end
                        && input[at..end]
                            == [101u8, 112u8, 115u8, 105u8, 108u8, 111u8, 110u8]
                    {
                        *p = end;
                        builder.push_leaf_with_unit();
                        builder.push_branch_tag(1u32);
                        break 'try_branches;
                    }
                }
                {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    let attempt: ::core::result::Result<(), crate::runtime::DtaError> = (||
                    {
                        let _ = ({
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            parse_hregex_BbnfBootstrap_identifier(
                                input,
                                p,
                                state,
                                builder,
                            )
                        })?;
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
                                    let _ = __shape_support_BbnfBootstrap::skip_space(
                                        input,
                                        p,
                                        state,
                                    );
                                    let _ = ({
                                        let _ = __shape_support_BbnfBootstrap::skip_space(
                                            input,
                                            p,
                                            state,
                                        );
                                        parse_flat_BbnfBootstrap_call_arg(input, p, state, builder)
                                    })?;
                                    let _ = __shape_support_BbnfBootstrap::skip_space(
                                        input,
                                        p,
                                        state,
                                    );
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
                                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                                    input,
                                                    p,
                                                    state,
                                                );
                                                {
                                                    let at = *p;
                                                    let end = at + 1usize;
                                                    if input.len() < end || input[at..end] != [44u8] {
                                                        return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                                            offset: at as u32,
                                                        });
                                                    }
                                                    *p = end;
                                                }
                                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                                    input,
                                                    p,
                                                    state,
                                                );
                                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                                    input,
                                                    p,
                                                    state,
                                                );
                                                let _ = ({
                                                    let _ = __shape_support_BbnfBootstrap::skip_space(
                                                        input,
                                                        p,
                                                        state,
                                                    );
                                                    parse_flat_BbnfBootstrap_call_arg(input, p, state, builder)
                                                })?;
                                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                                    input,
                                                    p,
                                                    state,
                                                );
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
                                        if input.len() < end || input[at..end] != [41u8] {
                                            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                                offset: at as u32,
                                            });
                                        }
                                        *p = end;
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
                        let __first = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                                offset: *p as u32,
                            })?;
                        parse_keyword_BbnfBootstrap_literal(
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
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_flat_BbnfBootstrap_regex(input, p, state, builder)
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
                {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    let attempt: ::core::result::Result<(), crate::runtime::DtaError> = (||
                    {
                        {
                            let at = *p;
                            let end = at + 2usize;
                            if input.len() < end || input[at..end] != [64u8, 123u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                        }
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        let _ = ({
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            parse_wrap_BbnfBootstrap_rhs(input, p, state, builder)
                        })?;
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
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
                            builder.push_branch_tag(5u32);
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
                            if input.len() < end || input[at..end] != [40u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                        }
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        let _ = ({
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            parse_wrap_BbnfBootstrap_rhs(input, p, state, builder)
                        })?;
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
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
                            builder.push_branch_tag(6u32);
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
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        let _ = ({
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            parse_wrap_BbnfBootstrap_rhs(input, p, state, builder)
                        })?;
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
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
                            builder.push_branch_tag(7u32);
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
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        let _ = ({
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            parse_wrap_BbnfBootstrap_rhs(input, p, state, builder)
                        })?;
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
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
                            builder.push_branch_tag(8u32);
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
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
    /// AZ-I.W2-act.recovery — per-grammar Pratt-shape parse
    /// function, **struct-direct body**. Targets the grammar's
    /// concrete `StructBuilder`.
    ///
    /// Opens a compound for the rule (e.g. `add_expr` →
    /// `SheetsCompoundKind::AddExpr`), dispatches operands +
    /// stamps operator branch tags inline, closes the compound.
    /// Children land in the order
    /// `[lhs_subtree, op_tag, rhs_subtree, op_tag, …]` — the
    /// rule's structural alphabet is preserved verbatim;
    /// associativity-honouring binary-tree reduction is a
    /// consumer-side projection (the generated module exposes
    /// `PRECEDENCE_LUT_<rule>` + `PRECEDENCE_ENTRIES_<rule>` for
    /// that purpose).
    ///
    /// Returns unit for StructDirect composition
    /// with sibling shape fns under struct-direct mode.
    ///
    /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
    /// cross-shape recursive edge through the value dispatcher.
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_variables,
        unused_mut,
        unused_assignments
    )]
    pub fn parse_pratt_BbnfBootstrap_binary_factor<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
        let __binary_factor_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 44u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("binary_factor"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __binary_factor_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__binary_factor_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let _ = ({
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                parse_flat_BbnfBootstrap_mapped_factor(input, p, state, builder)
            })?;
            loop {
                let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let mut lut_byte: u8 = PRECEDENCE_LUT_binary_factor[op_byte as usize];
                if lut_byte == 0 {
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    op_byte = input.get(*p).copied().unwrap_or(0);
                    lut_byte = PRECEDENCE_LUT_binary_factor[op_byte as usize];
                }
                if lut_byte == 0 {
                    break;
                }
                let two_byte: u8 = (lut_byte >> 7) & 0x01u8;
                let second_byte: ::core::option::Option<u8> = input.get(*p + 1).copied();
                let (op_width, op_discriminant, op_matched) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    let mut matched: bool = false;
                    for e in PRECEDENCE_ENTRIES_binary_factor.iter() {
                        if e.byte == op_byte && e.second_byte.is_none() {
                            found_disc = e.op_discriminant;
                            matched = true;
                            break;
                        }
                    }
                    (1u32, found_disc, matched)
                } else {
                    let mut found_disc: u8 = 0u8;
                    let mut matched_two_byte: bool = false;
                    let mut matched_single: bool = false;
                    for e in PRECEDENCE_ENTRIES_binary_factor.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    if !matched_two_byte {
                        for e in PRECEDENCE_ENTRIES_binary_factor.iter() {
                            if e.byte == op_byte && e.second_byte.is_none() {
                                found_disc = e.op_discriminant;
                                matched_single = true;
                                break;
                            }
                        }
                    }
                    let width = if matched_two_byte { 2u32 } else { 1u32 };
                    (width, found_disc, matched_two_byte || matched_single)
                };
                if !op_matched {
                    break;
                }
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_branch_tag(
                    builder,
                    op_discriminant as u32,
                );
                *p = (*p).saturating_add(op_width as usize);
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_flat_BbnfBootstrap_mapped_factor(input, p, state, builder)
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::end_compound(
            builder,
            __binary_factor_handle,
        );
        __body_result?;
        ::core::result::Result::Ok(())
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
    pub fn parse_wrap_BbnfBootstrap_rhs<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let first = __shape_support_BbnfBootstrap::skip_space(input, p, state)
            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        'try_branches: loop {
            match first {
                124u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_flat_BbnfBootstrap_closure(input, p, state, builder) {
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
            {
                let attempt_p = *p;
                let attempt_builder = builder.checkpoint();
                match parse_flat_BbnfBootstrap_alternation(input, p, state, builder) {
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
            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                offset: *p as u32,
            });
        }
        ::core::result::Result::Ok(())
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
    pub fn parse_flat_BbnfBootstrap_factor<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __factor_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 46u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("factor"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __factor_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__factor_layout);
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
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
                            let _ = ({
                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_flat_BbnfBootstrap_big_comment(
                                    input,
                                    p,
                                    state,
                                    builder,
                                )
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
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_altdispatch_BbnfBootstrap_term(input, p, state, builder)
                })?;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
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
                            let _ = ({
                                let __first = __shape_support_BbnfBootstrap::skip_space(
                                        input,
                                        p,
                                        state,
                                    )
                                    .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                                        offset: *p as u32,
                                    })?;
                                parse_keyword_BbnfBootstrap_modifier(
                                    input,
                                    p,
                                    __first,
                                    state,
                                    builder,
                                )
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
            {
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
                            let _ = ({
                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_flat_BbnfBootstrap_big_comment(
                                    input,
                                    p,
                                    state,
                                    builder,
                                )
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
                <crate::runtime::bbnf::BbnfStructBuilder<
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
    pub fn parse_flat_BbnfBootstrap_mapped_factor<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __mapped_factor_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 47u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("mapped_factor"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __mapped_factor_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__mapped_factor_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_flat_BbnfBootstrap_factor(input, p, state, builder)
                })?;
            }
            {
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
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let at = *p;
                            let end = at + 2usize;
                            if input.len() < end || input[at..end] != [45u8, 62u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                input,
                                p,
                                state,
                            );
                            let _ = ({
                                let _ = __shape_support_BbnfBootstrap::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_wrap_BbnfBootstrap_value_expr(
                                    input,
                                    p,
                                    state,
                                    builder,
                                )
                            })?;
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
                                        let _ = ({
                                            let _ = __shape_support_BbnfBootstrap::skip_space(
                                                input,
                                                p,
                                                state,
                                            );
                                            parse_flat_BbnfBootstrap_type_annotation(
                                                input,
                                                p,
                                                state,
                                                builder,
                                            )
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
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __mapped_factor_handle,
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
    pub fn parse_flat_BbnfBootstrap_rule<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __rule_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 48u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("rule"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __rule_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__rule_layout);
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_scalar_BbnfBootstrap_lhs(input, p, state, builder)
                })?;
            }
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [61u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_wrap_BbnfBootstrap_rhs(input, p, state, builder)
                })?;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
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
                <crate::runtime::bbnf::BbnfStructBuilder<
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
    pub fn parse_flat_BbnfBootstrap_recover_directive<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __flat_checkpoint = builder.checkpoint();
        let __recover_directive_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 49u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("recover_directive"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __recover_directive_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__recover_directive_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let at = *p;
                let end = at + 8usize;
                if input.len() < end
                    || input[at..end]
                        != [64u8, 114u8, 101u8, 99u8, 111u8, 118u8, 101u8, 114u8]
                {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_hregex_BbnfBootstrap_identifier(input, p, state, builder)
                })?;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_wrap_BbnfBootstrap_rhs(input, p, state, builder)
                })?;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
            }
            {
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
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __recover_directive_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(__err) => {
                builder.rollback(__flat_checkpoint);
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
    pub fn parse_keyword_BbnfBootstrap_directive<'p>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let _ = state;
        match first_byte {
            64u8 => {
                if input.len() >= *p + 8usize
                    && input[*p..*p + 8usize]
                        == [64u8, 114u8, 101u8, 99u8, 111u8, 118u8, 101u8, 114u8]
                {
                    let __ref_save_p = *p;
                    let __ref_builder_checkpoint = builder.checkpoint();
                    match ({
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_flat_BbnfBootstrap_recover_directive(
                            input,
                            p,
                            state,
                            builder,
                        )
                    }) {
                        ::core::result::Result::Ok(__off) => {
                            builder.commit(__ref_builder_checkpoint);
                            return ::core::result::Result::Ok(__off);
                        }
                        ::core::result::Result::Err(_) => {
                            *p = __ref_save_p;
                            builder.rollback(__ref_builder_checkpoint);
                        }
                    }
                }
                if input.len() >= *p + 7usize
                    && input[*p..*p + 7usize]
                        == [64u8, 105u8, 109u8, 112u8, 111u8, 114u8, 116u8]
                {
                    let __ref_save_p = *p;
                    let __ref_builder_checkpoint = builder.checkpoint();
                    match ({
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_flat_BbnfBootstrap_import_directive(
                            input,
                            p,
                            state,
                            builder,
                        )
                    }) {
                        ::core::result::Result::Ok(__off) => {
                            builder.commit(__ref_builder_checkpoint);
                            return ::core::result::Result::Ok(__off);
                        }
                        ::core::result::Result::Err(_) => {
                            *p = __ref_save_p;
                            builder.rollback(__ref_builder_checkpoint);
                        }
                    }
                }
                if input.len() >= *p + 7usize
                    && input[*p..*p + 7usize]
                        == [64u8, 112u8, 114u8, 101u8, 116u8, 116u8, 121u8]
                {
                    let __ref_save_p = *p;
                    let __ref_builder_checkpoint = builder.checkpoint();
                    match ({
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_flat_BbnfBootstrap_pretty_directive(
                            input,
                            p,
                            state,
                            builder,
                        )
                    }) {
                        ::core::result::Result::Ok(__off) => {
                            builder.commit(__ref_builder_checkpoint);
                            return ::core::result::Result::Ok(__off);
                        }
                        ::core::result::Result::Err(_) => {
                            *p = __ref_save_p;
                            builder.rollback(__ref_builder_checkpoint);
                        }
                    }
                }
                if input.len() >= *p + 6usize
                    && input[*p..*p + 6usize]
                        == [64u8, 116u8, 111u8, 107u8, 101u8, 110u8]
                {
                    let __ref_save_p = *p;
                    let __ref_builder_checkpoint = builder.checkpoint();
                    match ({
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_flat_BbnfBootstrap_token_directive(
                            input,
                            p,
                            state,
                            builder,
                        )
                    }) {
                        ::core::result::Result::Ok(__off) => {
                            builder.commit(__ref_builder_checkpoint);
                            return ::core::result::Result::Ok(__off);
                        }
                        ::core::result::Result::Err(_) => {
                            *p = __ref_save_p;
                            builder.rollback(__ref_builder_checkpoint);
                        }
                    }
                }
                if input.len() >= *p + 6usize
                    && input[*p..*p + 6usize] == [64u8, 100u8, 101u8, 98u8, 117u8, 103u8]
                {
                    let __ref_save_p = *p;
                    let __ref_builder_checkpoint = builder.checkpoint();
                    match ({
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_flat_BbnfBootstrap_debug_directive(
                            input,
                            p,
                            state,
                            builder,
                        )
                    }) {
                        ::core::result::Result::Ok(__off) => {
                            builder.commit(__ref_builder_checkpoint);
                            return ::core::result::Result::Ok(__off);
                        }
                        ::core::result::Result::Err(_) => {
                            *p = __ref_save_p;
                            builder.rollback(__ref_builder_checkpoint);
                        }
                    }
                }
                if input.len() >= *p + 5usize
                    && input[*p..*p + 5usize] == [64u8, 104u8, 111u8, 115u8, 116u8]
                {
                    let __ref_save_p = *p;
                    let __ref_builder_checkpoint = builder.checkpoint();
                    match ({
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_flat_BbnfBootstrap_host_directive(input, p, state, builder)
                    }) {
                        ::core::result::Result::Ok(__off) => {
                            builder.commit(__ref_builder_checkpoint);
                            return ::core::result::Result::Ok(__off);
                        }
                        ::core::result::Result::Err(_) => {
                            *p = __ref_save_p;
                            builder.rollback(__ref_builder_checkpoint);
                        }
                    }
                }
                if input.len() >= *p + 3usize
                    && input[*p..*p + 3usize] == [64u8, 119u8, 115u8]
                {
                    let __ref_save_p = *p;
                    let __ref_builder_checkpoint = builder.checkpoint();
                    match ({
                        let _ = __shape_support_BbnfBootstrap::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_flat_BbnfBootstrap_ws_directive(input, p, state, builder)
                    }) {
                        ::core::result::Result::Ok(__off) => {
                            builder.commit(__ref_builder_checkpoint);
                            return ::core::result::Result::Ok(__off);
                        }
                        ::core::result::Result::Err(_) => {
                            *p = __ref_save_p;
                            builder.rollback(__ref_builder_checkpoint);
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
    pub fn parse_wrap_BbnfBootstrap_grammar_item<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder as _;
        let __wrap_checkpoint = builder.checkpoint();
        let __wrap_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 51u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("grammar_item"),
            kind: ::bbnf_ir::registry::LayoutKind::TaggedEnum,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __wrap_handle = <crate::runtime::bbnf::BbnfStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__wrap_layout);
        let mut __wrap_branch_idx: u32 = 0;
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let first = __shape_support_BbnfBootstrap::skip_space(input, p, state)
                .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;
            'try_branches: loop {
                match first {
                    47u8 => {
                        {
                            let attempt_p = *p;
                            let attempt_builder = builder.checkpoint();
                            match parse_flat_BbnfBootstrap_comment(
                                input,
                                p,
                                state,
                                builder,
                            ) {
                                ::core::result::Result::Ok(_) => {
                                    __wrap_branch_idx = 0u32;
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
                            match parse_flat_BbnfBootstrap_big_comment(
                                input,
                                p,
                                state,
                                builder,
                            ) {
                                ::core::result::Result::Ok(_) => {
                                    __wrap_branch_idx = 1u32;
                                    builder.commit(attempt_builder);
                                    break 'try_branches;
                                }
                                ::core::result::Result::Err(_) => {
                                    *p = attempt_p;
                                    builder.rollback(attempt_builder);
                                }
                            }
                        }
                    }
                    _ => {}
                }
                {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_keyword_BbnfBootstrap_directive(
                        input,
                        p,
                        first,
                        state,
                        builder,
                    ) {
                        ::core::result::Result::Ok(_) => {
                            __wrap_branch_idx = 2u32;
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
                    match parse_flat_BbnfBootstrap_rule(input, p, state, builder) {
                        ::core::result::Result::Ok(_) => {
                            __wrap_branch_idx = 3u32;
                            builder.commit(attempt_builder);
                            break 'try_branches;
                        }
                        ::core::result::Result::Err(_) => {
                            *p = attempt_p;
                            builder.rollback(attempt_builder);
                        }
                    }
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_branch_tag(
                    builder,
                    __wrap_branch_idx,
                );
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __wrap_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(e) => {
                <crate::runtime::bbnf::BbnfStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::rollback(
                    builder,
                    __wrap_checkpoint,
                );
                ::core::result::Result::Err(e)
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
    pub fn parse_array_BbnfBootstrap_grammar<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        use crate::runtime::builder::StructBuilder;
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 52u32 as ::bbnf_ir::RuleId,
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
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                ({
                    let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
                    parse_wrap_BbnfBootstrap_grammar_item(input, p, state, builder)
                })?;
                let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
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
    pub fn parse_BbnfBootstrap_grammar<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        parse_BbnfBootstrap_grammar__value(input, p, state, builder)
    }
    /// AW-V.W3.2 — value-position shape dispatcher. Called both at
    /// the grammar root and from Object / Array compound bodies.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_BbnfBootstrap_grammar__value<'p>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_BbnfBootstrap::ScanState,
        builder: &mut crate::runtime::bbnf::BbnfStructBuilder<'p>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError> {
        let _ = __shape_support_BbnfBootstrap::skip_space(input, p, state);
        parse_array_BbnfBootstrap_grammar(input, p, state, builder)
    }
    impl BbnfBootstrap {
        fn __int_lit_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __start = state.offset;
                    if {
                        let __start = state.offset;
                        let __result: Option<()> = (|| {
                            {
                                let __save_alt = state.offset;
                                let __alt_ok = (|| -> Option<()> {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'0')
                                    {
                                        return None;
                                    }
                                    state.offset += 1;
                                    {
                                        let __b = *state.src_bytes.get(state.offset)?;
                                        if !((__b == b'X' || __b == b'x')) {
                                            return None;
                                        }
                                        state.offset += 1;
                                    }
                                    {
                                        if ::parse_that::scan_hex_mut(state).is_none() {
                                            return None;
                                        }
                                    }
                                    {
                                        let __end = state.src_bytes.len();
                                        let mut __pos = state.offset;
                                        while __pos < __end {
                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                            if (__b.is_ascii_alphanumeric() || __b == b'_') {
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
                                        {
                                            if ::parse_that::scan_digits_mut(state).is_none() {
                                                return None;
                                            }
                                        }
                                        {
                                            let __end = state.src_bytes.len();
                                            let mut __pos = state.offset;
                                            while __pos < __end {
                                                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                if (__b.is_ascii_alphanumeric() || __b == b'_') {
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
                true
            }
        }
        pub fn int_lit_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__int_lit_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __float_lit_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __start = state.offset;
                    if {
                        let __start = state.offset;
                        let __result: Option<()> = (|| {
                            {
                                let _ = ::parse_that::scan_digits_star_mut(state);
                            }
                            if state.src_bytes.get(state.offset).copied() != Some(b'.') {
                                return None;
                            }
                            state.offset += 1;
                            {
                                if ::parse_that::scan_digits_mut(state).is_none() {
                                    return None;
                                }
                            }
                            {
                                let __save = state.offset;
                                let __ok = (|| -> Option<()> {
                                    {
                                        let __b = *state.src_bytes.get(state.offset)?;
                                        if !((__b == b'E' || __b == b'e')) {
                                            return None;
                                        }
                                        state.offset += 1;
                                    }
                                    {
                                        let __save = state.offset;
                                        let __ok = (|| -> Option<()> {
                                            {
                                                let __b = *state.src_bytes.get(state.offset)?;
                                                if !((__b == b'+' || __b == b'-')) {
                                                    return None;
                                                }
                                                state.offset += 1;
                                            }
                                            Some(())
                                        })();
                                        if __ok.is_none() {
                                            state.offset = __save;
                                        }
                                    }
                                    {
                                        if ::parse_that::scan_digits_mut(state).is_none() {
                                            return None;
                                        }
                                    }
                                    Some(())
                                })();
                                if __ok.is_none() {
                                    state.offset = __save;
                                }
                            }
                            {
                                let __end = state.src_bytes.len();
                                let mut __pos = state.offset;
                                while __pos < __end {
                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                    if (__b.is_ascii_alphanumeric() || __b == b'_') {
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
                true
            }
        }
        pub fn float_lit_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__float_lit_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __bool_lit_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp0 = state.offset;
                        let __ok = (|| -> bool {
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
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp0;
                        }
                        __ok
                    } {
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
                };
                true
            }
        }
        pub fn bool_lit_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__bool_lit_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __string_lit_prettify<'a>(
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
        pub fn string_lit_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__string_lit_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __value_ident_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
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
                true
            }
        }
        pub fn value_ident_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__value_ident_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __mul_op_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp3 = state.offset;
                        let __ok = (|| -> bool {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'*');
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp3;
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp2 = state.offset;
                                let __ok = (|| -> bool {
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b'/');
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp2;
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp1 = state.offset;
                                        let __ok = (|| -> bool {
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'%')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b'%');
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp1;
                                        }
                                        __ok
                                    } {
                                        return false;
                                    }
                                }
                            }
                        }
                    }
                };
                true
            }
        }
        pub fn mul_op_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__mul_op_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __add_op_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp4 = state.offset;
                        let __ok = (|| -> bool {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'+')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'+');
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp4;
                        }
                        __ok
                    } {
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'-') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'-');
                        };
                    }
                };
                true
            }
        }
        pub fn add_op_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__add_op_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __cmp_op_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp10 = state.offset;
                        let __ok = (|| -> bool {
                            {
                                let __s = "==";
                                let __bytes = __s.as_bytes();
                                let __slc = match state.src_bytes.get(state.offset..) {
                                    Some(s) if s.len() >= 2usize => s,
                                    _ => return false,
                                };
                                if &__slc[..2usize] != __bytes {
                                    return false;
                                }
                                __builder
                                    .text(&state.src[state.offset..state.offset + 2usize]);
                                state.offset += 2usize;
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp10;
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp9 = state.offset;
                                let __ok = (|| -> bool {
                                    {
                                        let __s = "!=";
                                        let __bytes = __s.as_bytes();
                                        let __slc = match state.src_bytes.get(state.offset..) {
                                            Some(s) if s.len() >= 2usize => s,
                                            _ => return false,
                                        };
                                        if &__slc[..2usize] != __bytes {
                                            return false;
                                        }
                                        __builder
                                            .text(&state.src[state.offset..state.offset + 2usize]);
                                        state.offset += 2usize;
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp9;
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp8 = state.offset;
                                        let __ok = (|| -> bool {
                                            {
                                                let __s = "<=";
                                                let __bytes = __s.as_bytes();
                                                let __slc = match state.src_bytes.get(state.offset..) {
                                                    Some(s) if s.len() >= 2usize => s,
                                                    _ => return false,
                                                };
                                                if &__slc[..2usize] != __bytes {
                                                    return false;
                                                }
                                                __builder
                                                    .text(&state.src[state.offset..state.offset + 2usize]);
                                                state.offset += 2usize;
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp8;
                                        }
                                        __ok
                                    } {
                                        {
                                            if !{
                                                let __pretty_cp7 = state.offset;
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __s = ">=";
                                                        let __bytes = __s.as_bytes();
                                                        let __slc = match state.src_bytes.get(state.offset..) {
                                                            Some(s) if s.len() >= 2usize => s,
                                                            _ => return false,
                                                        };
                                                        if &__slc[..2usize] != __bytes {
                                                            return false;
                                                        }
                                                        __builder
                                                            .text(&state.src[state.offset..state.offset + 2usize]);
                                                        state.offset += 2usize;
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp7;
                                                }
                                                __ok
                                            } {
                                                {
                                                    if !{
                                                        let __pretty_cp6 = state.offset;
                                                        let __ok = (|| -> bool {
                                                            {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'<')
                                                                {
                                                                    return false;
                                                                }
                                                                state.offset += 1;
                                                                __builder.char(b'<');
                                                            };
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp6;
                                                        }
                                                        __ok
                                                    } {
                                                        {
                                                            if !{
                                                                let __pretty_cp5 = state.offset;
                                                                let __ok = (|| -> bool {
                                                                    {
                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'>')
                                                                        {
                                                                            return false;
                                                                        }
                                                                        state.offset += 1;
                                                                        __builder.char(b'>');
                                                                    };
                                                                    true
                                                                })();
                                                                if !__ok {
                                                                    state.offset = __pretty_cp5;
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
                        }
                    }
                };
                true
            }
        }
        pub fn cmp_op_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__cmp_op_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __type_name_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp21 = state.offset;
                        let __ok = (|| -> bool {
                            {
                                let __s = "u8";
                                let __bytes = __s.as_bytes();
                                let __slc = match state.src_bytes.get(state.offset..) {
                                    Some(s) if s.len() >= 2usize => s,
                                    _ => return false,
                                };
                                if &__slc[..2usize] != __bytes {
                                    return false;
                                }
                                __builder
                                    .text(&state.src[state.offset..state.offset + 2usize]);
                                state.offset += 2usize;
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp21;
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp20 = state.offset;
                                let __ok = (|| -> bool {
                                    {
                                        let __s = "u16";
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
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp20;
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp19 = state.offset;
                                        let __ok = (|| -> bool {
                                            {
                                                let __s = "u32";
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
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp19;
                                        }
                                        __ok
                                    } {
                                        {
                                            if !{
                                                let __pretty_cp18 = state.offset;
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __s = "u64";
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
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp18;
                                                }
                                                __ok
                                            } {
                                                {
                                                    if !{
                                                        let __pretty_cp17 = state.offset;
                                                        let __ok = (|| -> bool {
                                                            {
                                                                let __s = "i32";
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
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp17;
                                                        }
                                                        __ok
                                                    } {
                                                        {
                                                            if !{
                                                                let __pretty_cp16 = state.offset;
                                                                let __ok = (|| -> bool {
                                                                    {
                                                                        let __s = "i64";
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
                                                                    true
                                                                })();
                                                                if !__ok {
                                                                    state.offset = __pretty_cp16;
                                                                }
                                                                __ok
                                                            } {
                                                                {
                                                                    if !{
                                                                        let __pretty_cp15 = state.offset;
                                                                        let __ok = (|| -> bool {
                                                                            {
                                                                                let __s = "f32";
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
                                                                            true
                                                                        })();
                                                                        if !__ok {
                                                                            state.offset = __pretty_cp15;
                                                                        }
                                                                        __ok
                                                                    } {
                                                                        {
                                                                            if !{
                                                                                let __pretty_cp14 = state.offset;
                                                                                let __ok = (|| -> bool {
                                                                                    {
                                                                                        let __s = "f64";
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
                                                                                    true
                                                                                })();
                                                                                if !__ok {
                                                                                    state.offset = __pretty_cp14;
                                                                                }
                                                                                __ok
                                                                            } {
                                                                                {
                                                                                    if !{
                                                                                        let __pretty_cp13 = state.offset;
                                                                                        let __ok = (|| -> bool {
                                                                                            {
                                                                                                let __s = "bool";
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
                                                                                            true
                                                                                        })();
                                                                                        if !__ok {
                                                                                            state.offset = __pretty_cp13;
                                                                                        }
                                                                                        __ok
                                                                                    } {
                                                                                        {
                                                                                            if !{
                                                                                                let __pretty_cp12 = state.offset;
                                                                                                let __ok = (|| -> bool {
                                                                                                    {
                                                                                                        let __s = "usize";
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
                                                                                                    true
                                                                                                })();
                                                                                                if !__ok {
                                                                                                    state.offset = __pretty_cp12;
                                                                                                }
                                                                                                __ok
                                                                                            } {
                                                                                                {
                                                                                                    if !{
                                                                                                        let __pretty_cp11 = state.offset;
                                                                                                        let __ok = (|| -> bool {
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
                                                                                                            true
                                                                                                        })();
                                                                                                        if !__ok {
                                                                                                            state.offset = __pretty_cp11;
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
                                                                }
                                                            }
                                                        }
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
        }
        pub fn type_name_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__type_name_prettify(state, &mut __builder) {
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
        fn __literal_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp26 = state.offset;
                        let __pretty_bcp27 = __builder.checkpoint();
                        let __ok = (|| -> bool {
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
                                    if state.src_bytes.get(state.offset).copied() != Some(b'"')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b'"');
                                };
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp26;
                            __builder.restore(__pretty_bcp27);
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp24 = state.offset;
                                let __pretty_bcp25 = __builder.checkpoint();
                                let __ok = (|| -> bool {
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
                                                                                if !(!((__b == b'\'' || __b == b'\\'))) {
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
                                            if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'\'');
                                        };
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp24;
                                    __builder.restore(__pretty_bcp25);
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp22 = state.offset;
                                        let __pretty_bcp23 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                {
                                                    if state.src_bytes.get(state.offset).copied() != Some(b'`')
                                                    {
                                                        return false;
                                                    }
                                                    state.offset += 1;
                                                    __builder.char(b'`');
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
                                                                                        if !(!((__b == b'\\' || __b == b'`'))) {
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
                                                    if state.src_bytes.get(state.offset).copied() != Some(b'`')
                                                    {
                                                        return false;
                                                    }
                                                    state.offset += 1;
                                                    __builder.char(b'`');
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
                                        return false;
                                    }
                                }
                            }
                        }
                    }
                };
                true
            }
        }
        pub fn literal_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__literal_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __regex_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'/') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'/');
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
                                                            if !(!(__b == b'/')) {
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
                                    if __rep_count < 1 {
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
                        if state.src_bytes.get(state.offset).copied() != Some(b'/') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'/');
                    };
                };
                true
            }
        }
        pub fn regex_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__regex_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __big_comment_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp30 = state.offset;
                        let __pretty_bcp31 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows28 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows28..state.offset]);
                                {
                                    {
                                        let __s = "/*";
                                        let __bytes = __s.as_bytes();
                                        let __slc = match state.src_bytes.get(state.offset..) {
                                            Some(s) if s.len() >= 2usize => s,
                                            _ => return false,
                                        };
                                        if &__slc[..2usize] != __bytes {
                                            return false;
                                        }
                                        __builder
                                            .text(&state.src[state.offset..state.offset + 2usize]);
                                        state.offset += 2usize;
                                    };
                                    {
                                        let __start = state.offset;
                                        if {
                                            let __start = state.offset;
                                            let __scan = if __start >= state.src_bytes.len() {
                                                0
                                            } else {
                                                ({
                                                    static __LO_LUT: [u8; 16] = [
                                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                                                    ];
                                                    static __HI_LUT: [u8; 16] = [
                                                        0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                    ];
                                                    #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
                                                    let __result: ::core::option::Option<usize> = 'avx2_scan: {
                                                        use ::core::arch::x86_64::*;
                                                        unsafe {
                                                            let __bytes = state.src_bytes.as_slice();
                                                            let __len = __bytes.len();
                                                            let __ptr = __bytes.as_ptr();
                                                            let __lo_v = _mm256_broadcastsi128_si256(
                                                                _mm_loadu_si128(__LO_LUT.as_ptr() as *const __m128i),
                                                            );
                                                            let __hi_v = _mm256_broadcastsi128_si256(
                                                                _mm_loadu_si128(__HI_LUT.as_ptr() as *const __m128i),
                                                            );
                                                            let __lo_mask = _mm256_set1_epi8(0x0F);
                                                            let __zero = _mm256_setzero_si256();
                                                            let mut __i = __start;
                                                            while __i + 32 <= __len {
                                                                let __chunk = _mm256_loadu_si256(
                                                                    __ptr.add(__i) as *const __m256i,
                                                                );
                                                                let __lo_n = _mm256_and_si256(__chunk, __lo_mask);
                                                                let __hi_n = _mm256_and_si256(
                                                                    _mm256_srli_epi16(__chunk, 4),
                                                                    __lo_mask,
                                                                );
                                                                let __lo_r = _mm256_shuffle_epi8(__lo_v, __lo_n);
                                                                let __hi_r = _mm256_shuffle_epi8(__hi_v, __hi_n);
                                                                let __matched = _mm256_and_si256(__lo_r, __hi_r);
                                                                let __nz = _mm256_cmpgt_epi8(__matched, __zero);
                                                                let __mask = _mm256_movemask_epi8(__nz) as u32;
                                                                if __mask != 0 {
                                                                    let __rel = __mask.trailing_zeros() as usize;
                                                                    break 'avx2_scan {
                                                                        ::core::option::Option::Some((__i + __rel) - __start)
                                                                    };
                                                                }
                                                                __i += 32;
                                                            }
                                                            let mut __byte_lut = [false; 256];
                                                            {
                                                                let mut __b: u16 = 0;
                                                                while __b < 256 {
                                                                    let __blo = __LO_LUT[(__b & 0x0F) as usize];
                                                                    let __bhi = __HI_LUT[(__b >> 4) as usize];
                                                                    __byte_lut[__b as usize] = (__blo & __bhi) != 0;
                                                                    __b += 1;
                                                                }
                                                            }
                                                            while __i < __len {
                                                                let __b = *__ptr.add(__i);
                                                                if __byte_lut[__b as usize] {
                                                                    break 'avx2_scan {
                                                                        ::core::option::Option::Some(__i - __start)
                                                                    };
                                                                }
                                                                __i += 1;
                                                            }
                                                            ::core::option::Option::None
                                                        }
                                                    };
                                                    #[cfg(
                                                        not(all(target_arch = "x86_64", target_feature = "avx2"))
                                                    )]
                                                    let __result: ::core::option::Option<usize> = ::parse_that::find_next_structural_from(
                                                            state.padded(),
                                                            __start,
                                                            &__LO_LUT,
                                                            &__HI_LUT,
                                                        )
                                                        .map(|(pos, _)| pos - __start);
                                                    __result
                                                })
                                                    .unwrap_or(state.src_bytes.len() - __start)
                                            };
                                            state.offset = __start + __scan;
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
                                    {
                                        let __s = "*/";
                                        let __bytes = __s.as_bytes();
                                        let __slc = match state.src_bytes.get(state.offset..) {
                                            Some(s) if s.len() >= 2usize => s,
                                            _ => return false,
                                        };
                                        if &__slc[..2usize] != __bytes {
                                            return false;
                                        }
                                        __builder
                                            .text(&state.src[state.offset..state.offset + 2usize]);
                                        state.offset += 2usize;
                                    };
                                };
                                let __ows29 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows29..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp30;
                            __builder.restore(__pretty_bcp31);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                true
            }
        }
        pub fn big_comment_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__big_comment_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __comment_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp34 = state.offset;
                        let __pretty_bcp35 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows32 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows32..state.offset]);
                                {
                                    {
                                        let __s = "//";
                                        let __bytes = __s.as_bytes();
                                        let __slc = match state.src_bytes.get(state.offset..) {
                                            Some(s) if s.len() >= 2usize => s,
                                            _ => return false,
                                        };
                                        if &__slc[..2usize] != __bytes {
                                            return false;
                                        }
                                        __builder
                                            .text(&state.src[state.offset..state.offset + 2usize]);
                                        state.offset += 2usize;
                                    };
                                    {
                                        let __start = state.offset;
                                        if {
                                            let __start = state.offset;
                                            let __scan = if __start >= state.src_bytes.len() {
                                                0
                                            } else {
                                                ({
                                                    static __LO_LUT: [u8; 16] = [
                                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                                                    ];
                                                    static __HI_LUT: [u8; 16] = [
                                                        1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                    ];
                                                    #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
                                                    let __result: ::core::option::Option<usize> = 'avx2_scan: {
                                                        use ::core::arch::x86_64::*;
                                                        unsafe {
                                                            let __bytes = state.src_bytes.as_slice();
                                                            let __len = __bytes.len();
                                                            let __ptr = __bytes.as_ptr();
                                                            let __lo_v = _mm256_broadcastsi128_si256(
                                                                _mm_loadu_si128(__LO_LUT.as_ptr() as *const __m128i),
                                                            );
                                                            let __hi_v = _mm256_broadcastsi128_si256(
                                                                _mm_loadu_si128(__HI_LUT.as_ptr() as *const __m128i),
                                                            );
                                                            let __lo_mask = _mm256_set1_epi8(0x0F);
                                                            let __zero = _mm256_setzero_si256();
                                                            let mut __i = __start;
                                                            while __i + 32 <= __len {
                                                                let __chunk = _mm256_loadu_si256(
                                                                    __ptr.add(__i) as *const __m256i,
                                                                );
                                                                let __lo_n = _mm256_and_si256(__chunk, __lo_mask);
                                                                let __hi_n = _mm256_and_si256(
                                                                    _mm256_srli_epi16(__chunk, 4),
                                                                    __lo_mask,
                                                                );
                                                                let __lo_r = _mm256_shuffle_epi8(__lo_v, __lo_n);
                                                                let __hi_r = _mm256_shuffle_epi8(__hi_v, __hi_n);
                                                                let __matched = _mm256_and_si256(__lo_r, __hi_r);
                                                                let __nz = _mm256_cmpgt_epi8(__matched, __zero);
                                                                let __mask = _mm256_movemask_epi8(__nz) as u32;
                                                                if __mask != 0 {
                                                                    let __rel = __mask.trailing_zeros() as usize;
                                                                    break 'avx2_scan {
                                                                        ::core::option::Option::Some((__i + __rel) - __start)
                                                                    };
                                                                }
                                                                __i += 32;
                                                            }
                                                            let mut __byte_lut = [false; 256];
                                                            {
                                                                let mut __b: u16 = 0;
                                                                while __b < 256 {
                                                                    let __blo = __LO_LUT[(__b & 0x0F) as usize];
                                                                    let __bhi = __HI_LUT[(__b >> 4) as usize];
                                                                    __byte_lut[__b as usize] = (__blo & __bhi) != 0;
                                                                    __b += 1;
                                                                }
                                                            }
                                                            while __i < __len {
                                                                let __b = *__ptr.add(__i);
                                                                if __byte_lut[__b as usize] {
                                                                    break 'avx2_scan {
                                                                        ::core::option::Option::Some(__i - __start)
                                                                    };
                                                                }
                                                                __i += 1;
                                                            }
                                                            ::core::option::Option::None
                                                        }
                                                    };
                                                    #[cfg(
                                                        not(all(target_arch = "x86_64", target_feature = "avx2"))
                                                    )]
                                                    let __result: ::core::option::Option<usize> = ::parse_that::find_next_structural_from(
                                                            state.padded(),
                                                            __start,
                                                            &__LO_LUT,
                                                            &__HI_LUT,
                                                        )
                                                        .map(|(pos, _)| pos - __start);
                                                    __result
                                                })
                                                    .unwrap_or(state.src_bytes.len() - __start)
                                            };
                                            state.offset = __start + __scan;
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
                                };
                                let __ows33 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows33..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp34;
                            __builder.restore(__pretty_bcp35);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                true
            }
        }
        pub fn comment_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__comment_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __modifier_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp39 = state.offset;
                        let __ok = (|| -> bool {
                            {
                                let __s = "?w";
                                let __bytes = __s.as_bytes();
                                let __slc = match state.src_bytes.get(state.offset..) {
                                    Some(s) if s.len() >= 2usize => s,
                                    _ => return false,
                                };
                                if &__slc[..2usize] != __bytes {
                                    return false;
                                }
                                __builder
                                    .text(&state.src[state.offset..state.offset + 2usize]);
                                state.offset += 2usize;
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp39;
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp38 = state.offset;
                                let __ok = (|| -> bool {
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'?')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b'?');
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp38;
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp37 = state.offset;
                                        let __ok = (|| -> bool {
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b'*');
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp37;
                                        }
                                        __ok
                                    } {
                                        {
                                            if !{
                                                let __pretty_cp36 = state.offset;
                                                let __ok = (|| -> bool {
                                                    {
                                                        if state.src_bytes.get(state.offset).copied() != Some(b'+')
                                                        {
                                                            return false;
                                                        }
                                                        state.offset += 1;
                                                        __builder.char(b'+');
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp36;
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
                };
                true
            }
        }
        pub fn modifier_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__modifier_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __binary_operators_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp42 = state.offset;
                        let __ok = (|| -> bool {
                            {
                                let __s = "<<";
                                let __bytes = __s.as_bytes();
                                let __slc = match state.src_bytes.get(state.offset..) {
                                    Some(s) if s.len() >= 2usize => s,
                                    _ => return false,
                                };
                                if &__slc[..2usize] != __bytes {
                                    return false;
                                }
                                __builder
                                    .text(&state.src[state.offset..state.offset + 2usize]);
                                state.offset += 2usize;
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp42;
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp41 = state.offset;
                                let __ok = (|| -> bool {
                                    {
                                        let __s = ">>";
                                        let __bytes = __s.as_bytes();
                                        let __slc = match state.src_bytes.get(state.offset..) {
                                            Some(s) if s.len() >= 2usize => s,
                                            _ => return false,
                                        };
                                        if &__slc[..2usize] != __bytes {
                                            return false;
                                        }
                                        __builder
                                            .text(&state.src[state.offset..state.offset + 2usize]);
                                        state.offset += 2usize;
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp41;
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp40 = state.offset;
                                        let __ok = (|| -> bool {
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b'-');
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp40;
                                        }
                                        __ok
                                    } {
                                        return false;
                                    }
                                }
                            }
                        }
                    }
                };
                true
            }
        }
        pub fn binary_operators_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__binary_operators_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __import_path_prettify<'a>(
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
        pub fn import_path_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__import_path_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __value_path_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
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
                    {
                        let mut __rep_count45 = 0usize;
                        while __rep_count45 < 4294967295 {
                            let __rep_cp46 = state.offset;
                            if !{
                                let __pretty_cp43 = state.offset;
                                let __pretty_bcp44 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            let __s = "::";
                                            let __bytes = __s.as_bytes();
                                            let __slc = match state.src_bytes.get(state.offset..) {
                                                Some(s) if s.len() >= 2usize => s,
                                                _ => return false,
                                            };
                                            if &__slc[..2usize] != __bytes {
                                                return false;
                                            }
                                            __builder
                                                .text(&state.src[state.offset..state.offset + 2usize]);
                                            state.offset += 2usize;
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
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp43;
                                    __builder.restore(__pretty_bcp44);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp46;
                                break;
                            }
                            if state.offset == __rep_cp46 {
                                break;
                            }
                            __rep_count45 += 1;
                        }
                    };
                };
                true
            }
        }
        pub fn value_path_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__value_path_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __value_input_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __s = "input";
                        let __bytes = __s.as_bytes();
                        let __slc = match state.src_bytes.get(state.offset..) {
                            Some(s) if s.len() >= 5usize => s,
                            _ => return false,
                        };
                        if &__slc[..5usize] != __bytes {
                            return false;
                        }
                        __builder.text(&state.src[state.offset..state.offset + 5usize]);
                        state.offset += 5usize;
                    };
                    {
                        let mut __rep_count49 = 0usize;
                        while __rep_count49 < 4294967295 {
                            let __rep_cp50 = state.offset;
                            if !{
                                let __pretty_cp47 = state.offset;
                                let __pretty_bcp48 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'.');
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
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp47;
                                    __builder.restore(__pretty_bcp48);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp50;
                                break;
                            }
                            if state.offset == __rep_cp50 {
                                break;
                            }
                            __rep_count49 += 1;
                        }
                    };
                };
                true
            }
        }
        pub fn value_input_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__value_input_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __type_annotation_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __ows51 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows52 = state.offset;
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b':') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b':');
                        };
                        __builder.text_inline_ws(&state.src[__ows51..__ows52]);
                        let __ows53 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows53..state.offset]);
                    };
                    if !Self::__type_name_prettify(state, __builder) {
                        return false;
                    }
                };
                true
            }
        }
        pub fn type_annotation_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__type_annotation_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __lhs_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
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
                true
            }
        }
        pub fn lhs_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__lhs_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __import_items_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __ows54 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows55 = state.offset;
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'{') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'{');
                        };
                        __builder.text_inline_ws(&state.src[__ows54..__ows55]);
                        let __ows56 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows56..state.offset]);
                    };
                    {
                        if !{
                            let __pretty_cp66 = state.offset;
                            let __pretty_bcp67 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows64 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder.text_inline_ws(&state.src[__ows64..state.offset]);
                                    {
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
                                        {
                                            let mut __rep_count62 = 0usize;
                                            while __rep_count62 < 4294967295 {
                                                let __rep_cp63 = state.offset;
                                                if !{
                                                    let __pretty_cp60 = state.offset;
                                                    let __pretty_bcp61 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        {
                                                            {
                                                                let __ows57 = state.offset;
                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                let __ows58 = state.offset;
                                                                {
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                    {
                                                                        return false;
                                                                    }
                                                                    state.offset += 1;
                                                                    __builder.char(b',');
                                                                };
                                                                __builder.text_inline_ws(&state.src[__ows57..__ows58]);
                                                                let __ows59 = state.offset;
                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                __builder.text_inline_ws(&state.src[__ows59..state.offset]);
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
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp60;
                                                        __builder.restore(__pretty_bcp61);
                                                    }
                                                    __ok
                                                } {
                                                    state.offset = __rep_cp63;
                                                    break;
                                                }
                                                if state.offset == __rep_cp63 {
                                                    break;
                                                }
                                                __rep_count62 += 1;
                                            }
                                        };
                                    };
                                    let __ows65 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder.text_inline_ws(&state.src[__ows65..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp66;
                                __builder.restore(__pretty_bcp67);
                            }
                            __ok
                        } {
                            return false;
                        }
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
        pub fn import_items_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__import_items_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __pretty_hint_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
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
                    {
                        let _ = {
                            let __pretty_cp68 = state.offset;
                            let __pretty_bcp69 = __builder.checkpoint();
                            let __ok = (|| -> bool {
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
                                            let __scan = if __start >= state.src_bytes.len() {
                                                0
                                            } else {
                                                ({
                                                    static __LO_LUT: [u8; 16] = [
                                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                                                    ];
                                                    static __HI_LUT: [u8; 16] = [
                                                        0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                    ];
                                                    #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
                                                    let __result: ::core::option::Option<usize> = 'avx2_scan: {
                                                        use ::core::arch::x86_64::*;
                                                        unsafe {
                                                            let __bytes = state.src_bytes.as_slice();
                                                            let __len = __bytes.len();
                                                            let __ptr = __bytes.as_ptr();
                                                            let __lo_v = _mm256_broadcastsi128_si256(
                                                                _mm_loadu_si128(__LO_LUT.as_ptr() as *const __m128i),
                                                            );
                                                            let __hi_v = _mm256_broadcastsi128_si256(
                                                                _mm_loadu_si128(__HI_LUT.as_ptr() as *const __m128i),
                                                            );
                                                            let __lo_mask = _mm256_set1_epi8(0x0F);
                                                            let __zero = _mm256_setzero_si256();
                                                            let mut __i = __start;
                                                            while __i + 32 <= __len {
                                                                let __chunk = _mm256_loadu_si256(
                                                                    __ptr.add(__i) as *const __m256i,
                                                                );
                                                                let __lo_n = _mm256_and_si256(__chunk, __lo_mask);
                                                                let __hi_n = _mm256_and_si256(
                                                                    _mm256_srli_epi16(__chunk, 4),
                                                                    __lo_mask,
                                                                );
                                                                let __lo_r = _mm256_shuffle_epi8(__lo_v, __lo_n);
                                                                let __hi_r = _mm256_shuffle_epi8(__hi_v, __hi_n);
                                                                let __matched = _mm256_and_si256(__lo_r, __hi_r);
                                                                let __nz = _mm256_cmpgt_epi8(__matched, __zero);
                                                                let __mask = _mm256_movemask_epi8(__nz) as u32;
                                                                if __mask != 0 {
                                                                    let __rel = __mask.trailing_zeros() as usize;
                                                                    break 'avx2_scan {
                                                                        ::core::option::Option::Some((__i + __rel) - __start)
                                                                    };
                                                                }
                                                                __i += 32;
                                                            }
                                                            let mut __byte_lut = [false; 256];
                                                            {
                                                                let mut __b: u16 = 0;
                                                                while __b < 256 {
                                                                    let __blo = __LO_LUT[(__b & 0x0F) as usize];
                                                                    let __bhi = __HI_LUT[(__b >> 4) as usize];
                                                                    __byte_lut[__b as usize] = (__blo & __bhi) != 0;
                                                                    __b += 1;
                                                                }
                                                            }
                                                            while __i < __len {
                                                                let __b = *__ptr.add(__i);
                                                                if __byte_lut[__b as usize] {
                                                                    break 'avx2_scan {
                                                                        ::core::option::Option::Some(__i - __start)
                                                                    };
                                                                }
                                                                __i += 1;
                                                            }
                                                            ::core::option::Option::None
                                                        }
                                                    };
                                                    #[cfg(
                                                        not(all(target_arch = "x86_64", target_feature = "avx2"))
                                                    )]
                                                    let __result: ::core::option::Option<usize> = ::parse_that::find_next_structural_from(
                                                            state.padded(),
                                                            __start,
                                                            &__LO_LUT,
                                                            &__HI_LUT,
                                                        )
                                                        .map(|(pos, _)| pos - __start);
                                                    __result
                                                })
                                                    .unwrap_or(state.src_bytes.len() - __start)
                                            };
                                            state.offset = __start + __scan;
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
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b')')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b')');
                                    };
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp68;
                                __builder.restore(__pretty_bcp69);
                            }
                            __ok
                        };
                        true
                    };
                };
                true
            }
        }
        pub fn pretty_hint_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__pretty_hint_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __token_directive_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __ows70 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows71 = state.offset;
                        {
                            let __s = "@token";
                            let __bytes = __s.as_bytes();
                            let __slc = match state.src_bytes.get(state.offset..) {
                                Some(s) if s.len() >= 6usize => s,
                                _ => return false,
                            };
                            if &__slc[..6usize] != __bytes {
                                return false;
                            }
                            __builder
                                .text(&state.src[state.offset..state.offset + 6usize]);
                            state.offset += 6usize;
                        };
                        __builder.text_inline_ws(&state.src[__ows70..__ows71]);
                        let __ows72 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows72..state.offset]);
                    };
                    {
                        let __ows73 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows74 = state.offset;
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
                        __builder.text_inline_ws(&state.src[__ows73..__ows74]);
                        let __ows75 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows75..state.offset]);
                    };
                    {
                        let _ = {
                            let __pretty_cp77 = state.offset;
                            let __pretty_bcp78 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    if !{
                                        let __pretty_cp76 = state.offset;
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
                                            state.offset = __pretty_cp76;
                                        }
                                        __ok
                                    } {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'.');
                                        };
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp77;
                                __builder.restore(__pretty_bcp78);
                            }
                            __ok
                        };
                        true
                    };
                };
                true
            }
        }
        pub fn token_directive_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__token_directive_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __debug_directive_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __ows79 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows80 = state.offset;
                        {
                            let __s = "@debug";
                            let __bytes = __s.as_bytes();
                            let __slc = match state.src_bytes.get(state.offset..) {
                                Some(s) if s.len() >= 6usize => s,
                                _ => return false,
                            };
                            if &__slc[..6usize] != __bytes {
                                return false;
                            }
                            __builder
                                .text(&state.src[state.offset..state.offset + 6usize]);
                            state.offset += 6usize;
                        };
                        __builder.text_inline_ws(&state.src[__ows79..__ows80]);
                        let __ows81 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows81..state.offset]);
                    };
                    {
                        if !{
                            let __pretty_cp85 = state.offset;
                            let __pretty_bcp86 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows83 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder.text_inline_ws(&state.src[__ows83..state.offset]);
                                    {
                                        if !{
                                            let __pretty_cp82 = state.offset;
                                            let __ok = (|| -> bool {
                                                {
                                                    if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                    {
                                                        return false;
                                                    }
                                                    state.offset += 1;
                                                    __builder.char(b'*');
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp82;
                                            }
                                            __ok
                                        } {
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
                                        }
                                    };
                                    let __ows84 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder.text_inline_ws(&state.src[__ows84..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp85;
                                __builder.restore(__pretty_bcp86);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        let _ = {
                            let __pretty_cp88 = state.offset;
                            let __pretty_bcp89 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    if !{
                                        let __pretty_cp87 = state.offset;
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
                                            state.offset = __pretty_cp87;
                                        }
                                        __ok
                                    } {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'.');
                                        };
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp88;
                                __builder.restore(__pretty_bcp89);
                            }
                            __ok
                        };
                        true
                    };
                };
                true
            }
        }
        pub fn debug_directive_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__debug_directive_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __host_directive_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __ows90 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows91 = state.offset;
                        {
                            let __s = "@host";
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
                        __builder.text_inline_ws(&state.src[__ows90..__ows91]);
                        let __ows92 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows92..state.offset]);
                    };
                    {
                        let __ows93 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows94 = state.offset;
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
                        __builder.text_inline_ws(&state.src[__ows93..__ows94]);
                        let __ows95 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows95..state.offset]);
                    };
                    {
                        let _ = {
                            let __pretty_cp103 = state.offset;
                            let __pretty_bcp104 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows96 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows97 = state.offset;
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b':')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b':');
                                        };
                                        __builder.text_inline_ws(&state.src[__ows96..__ows97]);
                                        let __ows98 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder.text_inline_ws(&state.src[__ows98..state.offset]);
                                    };
                                    {
                                        if !{
                                            let __pretty_cp101 = state.offset;
                                            let __pretty_bcp102 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows99 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder.text_inline_ws(&state.src[__ows99..state.offset]);
                                                    if !Self::__type_name_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows100 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows100..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp101;
                                                __builder.restore(__pretty_bcp102);
                                            }
                                            __ok
                                        } {
                                            return false;
                                        }
                                    };
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp103;
                                __builder.restore(__pretty_bcp104);
                            }
                            __ok
                        };
                        true
                    };
                    {
                        let _ = {
                            let __pretty_cp106 = state.offset;
                            let __pretty_bcp107 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    if !{
                                        let __pretty_cp105 = state.offset;
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
                                            state.offset = __pretty_cp105;
                                        }
                                        __ok
                                    } {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'.');
                                        };
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp106;
                                __builder.restore(__pretty_bcp107);
                            }
                            __ok
                        };
                        true
                    };
                };
                true
            }
        }
        pub fn host_directive_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__host_directive_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __ws_directive_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __ows108 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows109 = state.offset;
                        {
                            let __s = "@ws";
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
                        __builder.text_inline_ws(&state.src[__ows108..__ows109]);
                        let __ows110 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows110..state.offset]);
                    };
                    {
                        if !{
                            let __pretty_cp113 = state.offset;
                            let __pretty_bcp114 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows111 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows111..state.offset]);
                                    if !Self::__regex_prettify(state, __builder) {
                                        return false;
                                    }
                                    let __ows112 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows112..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp113;
                                __builder.restore(__pretty_bcp114);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        let _ = {
                            let __pretty_cp116 = state.offset;
                            let __pretty_bcp117 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    if !{
                                        let __pretty_cp115 = state.offset;
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
                                            state.offset = __pretty_cp115;
                                        }
                                        __ok
                                    } {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'.');
                                        };
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp116;
                                __builder.restore(__pretty_bcp117);
                            }
                            __ok
                        };
                        true
                    };
                };
                true
            }
        }
        pub fn ws_directive_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__ws_directive_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __value_mul_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !Self::__value_unary_prettify(state, __builder) {
                        return false;
                    }
                    {
                        let mut __rep_count124 = 0usize;
                        while __rep_count124 < 4294967295 {
                            let __rep_cp125 = state.offset;
                            if !{
                                let __pretty_cp122 = state.offset;
                                let __pretty_bcp123 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            if !{
                                                let __pretty_cp120 = state.offset;
                                                let __pretty_bcp121 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows118 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows118..state.offset]);
                                                        if !Self::__mul_op_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        let __ows119 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows119..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp120;
                                                    __builder.restore(__pretty_bcp121);
                                                }
                                                __ok
                                            } {
                                                return false;
                                            }
                                        };
                                        if !Self::__value_unary_prettify(state, __builder) {
                                            return false;
                                        }
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp122;
                                    __builder.restore(__pretty_bcp123);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp125;
                                break;
                            }
                            if state.offset == __rep_cp125 {
                                break;
                            }
                            __rep_count124 += 1;
                        }
                    };
                };
                true
            }
        }
        pub fn value_mul_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__value_mul_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __value_or_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !Self::__value_and_prettify(state, __builder) {
                        return false;
                    }
                    {
                        let mut __rep_count131 = 0usize;
                        while __rep_count131 < 4294967295 {
                            let __rep_cp132 = state.offset;
                            if !{
                                let __pretty_cp129 = state.offset;
                                let __pretty_bcp130 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            let __ows126 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            let __ows127 = state.offset;
                                            {
                                                let __s = "||";
                                                let __bytes = __s.as_bytes();
                                                let __slc = match state.src_bytes.get(state.offset..) {
                                                    Some(s) if s.len() >= 2usize => s,
                                                    _ => return false,
                                                };
                                                if &__slc[..2usize] != __bytes {
                                                    return false;
                                                }
                                                __builder
                                                    .text(&state.src[state.offset..state.offset + 2usize]);
                                                state.offset += 2usize;
                                            };
                                            __builder.text_inline_ws(&state.src[__ows126..__ows127]);
                                            let __ows128 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder
                                                .text_inline_ws(&state.src[__ows128..state.offset]);
                                        };
                                        if !Self::__value_and_prettify(state, __builder) {
                                            return false;
                                        }
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp129;
                                    __builder.restore(__pretty_bcp130);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp132;
                                break;
                            }
                            if state.offset == __rep_cp132 {
                                break;
                            }
                            __rep_count131 += 1;
                        }
                    };
                };
                true
            }
        }
        pub fn value_or_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__value_or_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __value_add_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !Self::__value_mul_prettify(state, __builder) {
                        return false;
                    }
                    {
                        let mut __rep_count140 = 0usize;
                        while __rep_count140 < 4294967295 {
                            let __rep_cp141 = state.offset;
                            if !{
                                let __pretty_cp138 = state.offset;
                                let __pretty_bcp139 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            if !{
                                                let __pretty_cp136 = state.offset;
                                                let __pretty_bcp137 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows134 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows134..state.offset]);
                                                        {
                                                            if !{
                                                                let __pretty_cp133 = state.offset;
                                                                let __ok = (|| -> bool {
                                                                    {
                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'+')
                                                                        {
                                                                            return false;
                                                                        }
                                                                        state.offset += 1;
                                                                        __builder.char(b'+');
                                                                    };
                                                                    true
                                                                })();
                                                                if !__ok {
                                                                    state.offset = __pretty_cp133;
                                                                }
                                                                __ok
                                                            } {
                                                                {
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                                                    {
                                                                        return false;
                                                                    }
                                                                    state.offset += 1;
                                                                    __builder.char(b'-');
                                                                };
                                                            }
                                                        };
                                                        let __ows135 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows135..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp136;
                                                    __builder.restore(__pretty_bcp137);
                                                }
                                                __ok
                                            } {
                                                return false;
                                            }
                                        };
                                        if !Self::__value_mul_prettify(state, __builder) {
                                            return false;
                                        }
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp138;
                                    __builder.restore(__pretty_bcp139);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp141;
                                break;
                            }
                            if state.offset == __rep_cp141 {
                                break;
                            }
                            __rep_count140 += 1;
                        }
                    };
                };
                true
            }
        }
        pub fn value_add_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__value_add_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __value_cmp_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !Self::__value_add_prettify(state, __builder) {
                        return false;
                    }
                    {
                        let mut __rep_count148 = 0usize;
                        while __rep_count148 < 4294967295 {
                            let __rep_cp149 = state.offset;
                            if !{
                                let __pretty_cp146 = state.offset;
                                let __pretty_bcp147 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            if !{
                                                let __pretty_cp144 = state.offset;
                                                let __pretty_bcp145 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows142 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows142..state.offset]);
                                                        if !Self::__cmp_op_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        let __ows143 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows143..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp144;
                                                    __builder.restore(__pretty_bcp145);
                                                }
                                                __ok
                                            } {
                                                return false;
                                            }
                                        };
                                        if !Self::__value_add_prettify(state, __builder) {
                                            return false;
                                        }
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp146;
                                    __builder.restore(__pretty_bcp147);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp149;
                                break;
                            }
                            if state.offset == __rep_cp149 {
                                break;
                            }
                            __rep_count148 += 1;
                        }
                    };
                };
                true
            }
        }
        pub fn value_cmp_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__value_cmp_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __value_and_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !Self::__value_cmp_prettify(state, __builder) {
                        return false;
                    }
                    {
                        let mut __rep_count155 = 0usize;
                        while __rep_count155 < 4294967295 {
                            let __rep_cp156 = state.offset;
                            if !{
                                let __pretty_cp153 = state.offset;
                                let __pretty_bcp154 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            let __ows150 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            let __ows151 = state.offset;
                                            {
                                                let __s = "&&";
                                                let __bytes = __s.as_bytes();
                                                let __slc = match state.src_bytes.get(state.offset..) {
                                                    Some(s) if s.len() >= 2usize => s,
                                                    _ => return false,
                                                };
                                                if &__slc[..2usize] != __bytes {
                                                    return false;
                                                }
                                                __builder
                                                    .text(&state.src[state.offset..state.offset + 2usize]);
                                                state.offset += 2usize;
                                            };
                                            __builder.text_inline_ws(&state.src[__ows150..__ows151]);
                                            let __ows152 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder
                                                .text_inline_ws(&state.src[__ows152..state.offset]);
                                        };
                                        if !Self::__value_cmp_prettify(state, __builder) {
                                            return false;
                                        }
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp153;
                                    __builder.restore(__pretty_bcp154);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp156;
                                break;
                            }
                            if state.offset == __rep_cp156 {
                                break;
                            }
                            __rep_count155 += 1;
                        }
                    };
                };
                true
            }
        }
        pub fn value_and_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__value_and_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __value_closure_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'|') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'|');
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
                    {
                        let mut __rep_count162 = 0usize;
                        while __rep_count162 < 4294967295 {
                            let __rep_cp163 = state.offset;
                            if !{
                                let __pretty_cp160 = state.offset;
                                let __pretty_bcp161 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            let __ows157 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            let __ows158 = state.offset;
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b',');
                                            };
                                            __builder.text_inline_ws(&state.src[__ows157..__ows158]);
                                            let __ows159 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder
                                                .text_inline_ws(&state.src[__ows159..state.offset]);
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
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp160;
                                    __builder.restore(__pretty_bcp161);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp163;
                                break;
                            }
                            if state.offset == __rep_cp163 {
                                break;
                            }
                            __rep_count162 += 1;
                        }
                    };
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'|') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'|');
                    };
                    if !Self::__value_expr_prettify(state, __builder) {
                        return false;
                    }
                };
                true
            }
        }
        pub fn value_closure_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__value_closure_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __value_fn_call_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !Self::__value_path_prettify(state, __builder) {
                        return false;
                    }
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'(') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'(');
                    };
                    {
                        let _ = {
                            let __pretty_cp171 = state.offset;
                            let __pretty_bcp172 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    if !Self::__value_expr_prettify(state, __builder) {
                                        return false;
                                    }
                                    {
                                        let mut __rep_count169 = 0usize;
                                        while __rep_count169 < 4294967295 {
                                            let __rep_cp170 = state.offset;
                                            if !{
                                                let __pretty_cp167 = state.offset;
                                                let __pretty_bcp168 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        {
                                                            let __ows164 = state.offset;
                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                            let __ows165 = state.offset;
                                                            {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                {
                                                                    return false;
                                                                }
                                                                state.offset += 1;
                                                                __builder.char(b',');
                                                            };
                                                            __builder.text_inline_ws(&state.src[__ows164..__ows165]);
                                                            let __ows166 = state.offset;
                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                            __builder
                                                                .text_inline_ws(&state.src[__ows166..state.offset]);
                                                        };
                                                        if !Self::__value_expr_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp167;
                                                    __builder.restore(__pretty_bcp168);
                                                }
                                                __ok
                                            } {
                                                state.offset = __rep_cp170;
                                                break;
                                            }
                                            if state.offset == __rep_cp170 {
                                                break;
                                            }
                                            __rep_count169 += 1;
                                        }
                                    };
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp171;
                                __builder.restore(__pretty_bcp172);
                            }
                            __ok
                        };
                        true
                    };
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b')') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b')');
                    };
                };
                true
            }
        }
        pub fn value_fn_call_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__value_fn_call_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __value_expr_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp173 = state.offset;
                        let __pretty_bcp174 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__value_closure_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp173;
                            __builder.restore(__pretty_bcp174);
                        }
                        __ok
                    } {
                        if !Self::__value_or_prettify(state, __builder) {
                            return false;
                        }
                    }
                };
                true
            }
        }
        pub fn value_expr_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__value_expr_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __value_atom_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp193 = state.offset;
                        let __ok = (|| -> bool {
                            {
                                let __start = state.offset;
                                if {
                                    let __start = state.offset;
                                    let __result: Option<()> = (|| {
                                        {
                                            let __save_alt = state.offset;
                                            let __alt_ok = (|| -> Option<()> {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'0')
                                                {
                                                    return None;
                                                }
                                                state.offset += 1;
                                                {
                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                    if !((__b == b'X' || __b == b'x')) {
                                                        return None;
                                                    }
                                                    state.offset += 1;
                                                }
                                                {
                                                    if ::parse_that::scan_hex_mut(state).is_none() {
                                                        return None;
                                                    }
                                                }
                                                {
                                                    let __end = state.src_bytes.len();
                                                    let mut __pos = state.offset;
                                                    while __pos < __end {
                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                        if (__b.is_ascii_alphanumeric() || __b == b'_') {
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
                                                    {
                                                        if ::parse_that::scan_digits_mut(state).is_none() {
                                                            return None;
                                                        }
                                                    }
                                                    {
                                                        let __end = state.src_bytes.len();
                                                        let mut __pos = state.offset;
                                                        while __pos < __end {
                                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                            if (__b.is_ascii_alphanumeric() || __b == b'_') {
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
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp193;
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp192 = state.offset;
                                let __ok = (|| -> bool {
                                    {
                                        let __start = state.offset;
                                        if {
                                            let __start = state.offset;
                                            let __result: Option<()> = (|| {
                                                {
                                                    let _ = ::parse_that::scan_digits_star_mut(state);
                                                }
                                                if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                                {
                                                    return None;
                                                }
                                                state.offset += 1;
                                                {
                                                    if ::parse_that::scan_digits_mut(state).is_none() {
                                                        return None;
                                                    }
                                                }
                                                {
                                                    let __save = state.offset;
                                                    let __ok = (|| -> Option<()> {
                                                        {
                                                            let __b = *state.src_bytes.get(state.offset)?;
                                                            if !((__b == b'E' || __b == b'e')) {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                        }
                                                        {
                                                            let __save = state.offset;
                                                            let __ok = (|| -> Option<()> {
                                                                {
                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                    if !((__b == b'+' || __b == b'-')) {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                }
                                                                Some(())
                                                            })();
                                                            if __ok.is_none() {
                                                                state.offset = __save;
                                                            }
                                                        }
                                                        {
                                                            if ::parse_that::scan_digits_mut(state).is_none() {
                                                                return None;
                                                            }
                                                        }
                                                        Some(())
                                                    })();
                                                    if __ok.is_none() {
                                                        state.offset = __save;
                                                    }
                                                }
                                                {
                                                    let __end = state.src_bytes.len();
                                                    let mut __pos = state.offset;
                                                    while __pos < __end {
                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                        if (__b.is_ascii_alphanumeric() || __b == b'_') {
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
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp192;
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp190 = state.offset;
                                        let __pretty_bcp191 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                if !{
                                                    let __pretty_cp175 = state.offset;
                                                    let __ok = (|| -> bool {
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
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp175;
                                                    }
                                                    __ok
                                                } {
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
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp190;
                                            __builder.restore(__pretty_bcp191);
                                        }
                                        __ok
                                    } {
                                        {
                                            if !{
                                                let __pretty_cp188 = state.offset;
                                                let __pretty_bcp189 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
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
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'"')
                                                            {
                                                                return false;
                                                            }
                                                            state.offset += 1;
                                                            __builder.char(b'"');
                                                        };
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp188;
                                                    __builder.restore(__pretty_bcp189);
                                                }
                                                __ok
                                            } {
                                                {
                                                    if !{
                                                        let __pretty_cp186 = state.offset;
                                                        let __pretty_bcp187 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            if !Self::__value_fn_call_prettify(state, __builder) {
                                                                return false;
                                                            }
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp186;
                                                            __builder.restore(__pretty_bcp187);
                                                        }
                                                        __ok
                                                    } {
                                                        {
                                                            if !{
                                                                let __pretty_cp184 = state.offset;
                                                                let __pretty_bcp185 = __builder.checkpoint();
                                                                let __ok = (|| -> bool {
                                                                    if !Self::__value_input_prettify(state, __builder) {
                                                                        return false;
                                                                    }
                                                                    true
                                                                })();
                                                                if !__ok {
                                                                    state.offset = __pretty_cp184;
                                                                    __builder.restore(__pretty_bcp185);
                                                                }
                                                                __ok
                                                            } {
                                                                {
                                                                    if !{
                                                                        let __pretty_cp182 = state.offset;
                                                                        let __pretty_bcp183 = __builder.checkpoint();
                                                                        let __ok = (|| -> bool {
                                                                            if !Self::__value_path_prettify(state, __builder) {
                                                                                return false;
                                                                            }
                                                                            true
                                                                        })();
                                                                        if !__ok {
                                                                            state.offset = __pretty_cp182;
                                                                            __builder.restore(__pretty_bcp183);
                                                                        }
                                                                        __ok
                                                                    } {
                                                                        {
                                                                            if !{
                                                                                let __pretty_cp180 = state.offset;
                                                                                let __pretty_bcp181 = __builder.checkpoint();
                                                                                let __ok = (|| -> bool {
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
                                                                                            if !{
                                                                                                let __pretty_cp178 = state.offset;
                                                                                                let __pretty_bcp179 = __builder.checkpoint();
                                                                                                let __ok = (|| -> bool {
                                                                                                    {
                                                                                                        let __ows176 = state.offset;
                                                                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                        __builder
                                                                                                            .text_inline_ws(&state.src[__ows176..state.offset]);
                                                                                                        if !Self::__value_expr_prettify(state, __builder) {
                                                                                                            return false;
                                                                                                        }
                                                                                                        let __ows177 = state.offset;
                                                                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                        __builder
                                                                                                            .text_inline_ws(&state.src[__ows177..state.offset]);
                                                                                                    };
                                                                                                    true
                                                                                                })();
                                                                                                if !__ok {
                                                                                                    state.offset = __pretty_cp178;
                                                                                                    __builder.restore(__pretty_bcp179);
                                                                                                }
                                                                                                __ok
                                                                                            } {
                                                                                                return false;
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
                                                                                    true
                                                                                })();
                                                                                if !__ok {
                                                                                    state.offset = __pretty_cp180;
                                                                                    __builder.restore(__pretty_bcp181);
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
                                        }
                                    }
                                }
                            }
                        }
                    }
                };
                true
            }
        }
        pub fn value_atom_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__value_atom_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __value_unary_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp195 = state.offset;
                        let __pretty_bcp196 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                {
                                    if !{
                                        let __pretty_cp194 = state.offset;
                                        let __ok = (|| -> bool {
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'!')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b'!');
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp194;
                                        }
                                        __ok
                                    } {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'-');
                                        };
                                    }
                                };
                                if !Self::__value_atom_prettify(state, __builder) {
                                    return false;
                                }
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp195;
                            __builder.restore(__pretty_bcp196);
                        }
                        __ok
                    } {
                        if !Self::__value_atom_prettify(state, __builder) {
                            return false;
                        }
                    }
                };
                true
            }
        }
        pub fn value_unary_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__value_unary_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __import_directive_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __ows197 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows198 = state.offset;
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
                            __builder
                                .text(&state.src[state.offset..state.offset + 7usize]);
                            state.offset += 7usize;
                        };
                        __builder.text_inline_ws(&state.src[__ows197..__ows198]);
                        let __ows199 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows199..state.offset]);
                    };
                    {
                        if !{
                            let __pretty_cp211 = state.offset;
                            let __pretty_bcp212 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows209 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows209..state.offset]);
                                    {
                                        if !{
                                            let __pretty_cp207 = state.offset;
                                            let __pretty_bcp208 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    {
                                                        if !{
                                                            let __pretty_cp202 = state.offset;
                                                            let __pretty_bcp203 = __builder.checkpoint();
                                                            let __ok = (|| -> bool {
                                                                {
                                                                    let __ows200 = state.offset;
                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                    __builder
                                                                        .text_inline_ws(&state.src[__ows200..state.offset]);
                                                                    if !Self::__import_items_prettify(state, __builder) {
                                                                        return false;
                                                                    }
                                                                    let __ows201 = state.offset;
                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                    __builder
                                                                        .text_inline_ws(&state.src[__ows201..state.offset]);
                                                                };
                                                                true
                                                            })();
                                                            if !__ok {
                                                                state.offset = __pretty_cp202;
                                                                __builder.restore(__pretty_bcp203);
                                                            }
                                                            __ok
                                                        } {
                                                            return false;
                                                        }
                                                    };
                                                    {
                                                        let __ows204 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        let __ows205 = state.offset;
                                                        {
                                                            let __s = "from";
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
                                                        __builder.text_inline_ws(&state.src[__ows204..__ows205]);
                                                        let __ows206 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows206..state.offset]);
                                                    };
                                                    if !Self::__import_path_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp207;
                                                __builder.restore(__pretty_bcp208);
                                            }
                                            __ok
                                        } {
                                            if !Self::__import_path_prettify(state, __builder) {
                                                return false;
                                            }
                                        }
                                    };
                                    let __ows210 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows210..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp211;
                                __builder.restore(__pretty_bcp212);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        let _ = {
                            let __pretty_cp214 = state.offset;
                            let __pretty_bcp215 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    if !{
                                        let __pretty_cp213 = state.offset;
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
                                            state.offset = __pretty_cp213;
                                        }
                                        __ok
                                    } {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'.');
                                        };
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp214;
                                __builder.restore(__pretty_bcp215);
                            }
                            __ok
                        };
                        true
                    };
                };
                true
            }
        }
        pub fn import_directive_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__import_directive_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __pretty_directive_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __ows216 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows217 = state.offset;
                        {
                            let __s = "@pretty";
                            let __bytes = __s.as_bytes();
                            let __slc = match state.src_bytes.get(state.offset..) {
                                Some(s) if s.len() >= 7usize => s,
                                _ => return false,
                            };
                            if &__slc[..7usize] != __bytes {
                                return false;
                            }
                            __builder
                                .text(&state.src[state.offset..state.offset + 7usize]);
                            state.offset += 7usize;
                        };
                        __builder.text_inline_ws(&state.src[__ows216..__ows217]);
                        let __ows218 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows218..state.offset]);
                    };
                    {
                        if !{
                            let __pretty_cp222 = state.offset;
                            let __pretty_bcp223 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows220 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows220..state.offset]);
                                    {
                                        if !{
                                            let __pretty_cp219 = state.offset;
                                            let __ok = (|| -> bool {
                                                {
                                                    if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                    {
                                                        return false;
                                                    }
                                                    state.offset += 1;
                                                    __builder.char(b'*');
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp219;
                                            }
                                            __ok
                                        } {
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
                                        }
                                    };
                                    let __ows221 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows221..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp222;
                                __builder.restore(__pretty_bcp223);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        let __rep_start232 = state.offset;
                        let __rep_bcp233 = __builder.checkpoint();
                        let mut __rep_count230 = 0usize;
                        while __rep_count230 < 4294967295 {
                            let __rep_cp231 = state.offset;
                            if !{
                                let __pretty_cp228 = state.offset;
                                let __pretty_bcp229 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        if !{
                                            let __pretty_cp226 = state.offset;
                                            let __pretty_bcp227 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows224 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows224..state.offset]);
                                                    if !Self::__pretty_hint_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows225 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows225..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp226;
                                                __builder.restore(__pretty_bcp227);
                                            }
                                            __ok
                                        } {
                                            return false;
                                        }
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp228;
                                    __builder.restore(__pretty_bcp229);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp231;
                                break;
                            }
                            if state.offset == __rep_cp231 {
                                break;
                            }
                            __rep_count230 += 1;
                        }
                        if __rep_count230 < 1 {
                            state.offset = __rep_start232;
                            __builder.restore(__rep_bcp233);
                            return false;
                        }
                    };
                    {
                        let _ = {
                            let __pretty_cp235 = state.offset;
                            let __pretty_bcp236 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    if !{
                                        let __pretty_cp234 = state.offset;
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
                                            state.offset = __pretty_cp234;
                                        }
                                        __ok
                                    } {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'.');
                                        };
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp235;
                                __builder.restore(__pretty_bcp236);
                            }
                            __ok
                        };
                        true
                    };
                };
                true
            }
        }
        pub fn pretty_directive_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__pretty_directive_prettify(state, &mut __builder) {
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
                        let __rep_start247 = state.offset;
                        let __rep_bcp248 = __builder.checkpoint();
                        let mut __rep_count245 = 0usize;
                        while __rep_count245 < 4294967295 {
                            let __rep_cp246 = state.offset;
                            if !{
                                let __pretty_cp243 = state.offset;
                                let __pretty_bcp244 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            if !{
                                                let __pretty_cp239 = state.offset;
                                                let __pretty_bcp240 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows237 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows237..state.offset]);
                                                        if !Self::__concatenation_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        let __ows238 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows238..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp239;
                                                    __builder.restore(__pretty_bcp240);
                                                }
                                                __ok
                                            } {
                                                return false;
                                            }
                                        };
                                        {
                                            let _ = {
                                                let __pretty_cp241 = state.offset;
                                                let __pretty_bcp242 = __builder.checkpoint();
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
                                                    state.offset = __pretty_cp241;
                                                    __builder.restore(__pretty_bcp242);
                                                }
                                                __ok
                                            };
                                            true
                                        };
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp243;
                                    __builder.restore(__pretty_bcp244);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp246;
                                break;
                            }
                            if state.offset == __rep_cp246 {
                                break;
                            }
                            __rep_count245 += 1;
                        }
                        if __rep_count245 < 1 {
                            state.offset = __rep_start247;
                            __builder.restore(__rep_bcp248);
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
        fn __call_arg_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __rep_start259 = state.offset;
                    let __rep_bcp260 = __builder.checkpoint();
                    let mut __rep_count257 = 0usize;
                    while __rep_count257 < 4294967295 {
                        let __rep_cp258 = state.offset;
                        if !{
                            let __pretty_cp255 = state.offset;
                            let __pretty_bcp256 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp251 = state.offset;
                                            let __pretty_bcp252 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows249 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows249..state.offset]);
                                                    if !Self::__binary_factor_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows250 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows250..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp251;
                                                __builder.restore(__pretty_bcp252);
                                            }
                                            __ok
                                        } {
                                            return false;
                                        }
                                    };
                                    {
                                        let _ = {
                                            let __pretty_cp253 = state.offset;
                                            let __pretty_bcp254 = __builder.checkpoint();
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
                                                state.offset = __pretty_cp253;
                                                __builder.restore(__pretty_bcp254);
                                            }
                                            __ok
                                        };
                                        true
                                    };
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp255;
                                __builder.restore(__pretty_bcp256);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp258;
                            break;
                        }
                        if state.offset == __rep_cp258 {
                            break;
                        }
                        __rep_count257 += 1;
                    }
                    if __rep_count257 < 1 {
                        state.offset = __rep_start259;
                        __builder.restore(__rep_bcp260);
                        return false;
                    }
                };
                true
            }
        }
        pub fn call_arg_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__call_arg_prettify(state, &mut __builder) {
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
                    let __rep_start271 = state.offset;
                    let __rep_bcp272 = __builder.checkpoint();
                    let mut __rep_count269 = 0usize;
                    while __rep_count269 < 4294967295 {
                        let __rep_cp270 = state.offset;
                        if !{
                            let __pretty_cp267 = state.offset;
                            let __pretty_bcp268 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp263 = state.offset;
                                            let __pretty_bcp264 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows261 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows261..state.offset]);
                                                    if !Self::__binary_factor_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows262 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows262..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp263;
                                                __builder.restore(__pretty_bcp264);
                                            }
                                            __ok
                                        } {
                                            return false;
                                        }
                                    };
                                    {
                                        let _ = {
                                            let __pretty_cp265 = state.offset;
                                            let __pretty_bcp266 = __builder.checkpoint();
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
                                                state.offset = __pretty_cp265;
                                                __builder.restore(__pretty_bcp266);
                                            }
                                            __ok
                                        };
                                        true
                                    };
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp267;
                                __builder.restore(__pretty_bcp268);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp270;
                            break;
                        }
                        if state.offset == __rep_cp270 {
                            break;
                        }
                        __rep_count269 += 1;
                    }
                    if __rep_count269 < 1 {
                        state.offset = __rep_start271;
                        __builder.restore(__rep_bcp272);
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
        fn __closure_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'|') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'|');
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
                    {
                        let mut __rep_count278 = 0usize;
                        while __rep_count278 < 4294967295 {
                            let __rep_cp279 = state.offset;
                            if !{
                                let __pretty_cp276 = state.offset;
                                let __pretty_bcp277 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            let __ows273 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            let __ows274 = state.offset;
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b',');
                                            };
                                            __builder.text_inline_ws(&state.src[__ows273..__ows274]);
                                            let __ows275 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder
                                                .text_inline_ws(&state.src[__ows275..state.offset]);
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
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp276;
                                    __builder.restore(__pretty_bcp277);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp279;
                                break;
                            }
                            if state.offset == __rep_cp279 {
                                break;
                            }
                            __rep_count278 += 1;
                        }
                    };
                    {
                        let __ows280 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows281 = state.offset;
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'|') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'|');
                        };
                        __builder.text_inline_ws(&state.src[__ows280..__ows281]);
                        let __ows282 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows282..state.offset]);
                    };
                    if !Self::__rhs_prettify(state, __builder) {
                        return false;
                    }
                };
                true
            }
        }
        pub fn closure_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__closure_prettify(state, &mut __builder) {
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
                    if !{
                        let __pretty_cp331 = state.offset;
                        let __ok = (|| -> bool {
                            {
                                let __s = "ε";
                                let __bytes = __s.as_bytes();
                                let __slc = match state.src_bytes.get(state.offset..) {
                                    Some(s) if s.len() >= 2usize => s,
                                    _ => return false,
                                };
                                if &__slc[..2usize] != __bytes {
                                    return false;
                                }
                                __builder
                                    .text(&state.src[state.offset..state.offset + 2usize]);
                                state.offset += 2usize;
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp331;
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp330 = state.offset;
                                let __ok = (|| -> bool {
                                    {
                                        let __s = "epsilon";
                                        let __bytes = __s.as_bytes();
                                        let __slc = match state.src_bytes.get(state.offset..) {
                                            Some(s) if s.len() >= 7usize => s,
                                            _ => return false,
                                        };
                                        if &__slc[..7usize] != __bytes {
                                            return false;
                                        }
                                        __builder
                                            .text(&state.src[state.offset..state.offset + 7usize]);
                                        state.offset += 7usize;
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp330;
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp328 = state.offset;
                                        let __pretty_bcp329 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
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
                                                {
                                                    let _ = {
                                                        let __pretty_cp298 = state.offset;
                                                        let __pretty_bcp299 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
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
                                                                    if !{
                                                                        let __pretty_cp285 = state.offset;
                                                                        let __pretty_bcp286 = __builder.checkpoint();
                                                                        let __ok = (|| -> bool {
                                                                            {
                                                                                let __ows283 = state.offset;
                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                __builder
                                                                                    .text_inline_ws(&state.src[__ows283..state.offset]);
                                                                                if !Self::__call_arg_prettify(state, __builder) {
                                                                                    return false;
                                                                                }
                                                                                let __ows284 = state.offset;
                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                __builder
                                                                                    .text_inline_ws(&state.src[__ows284..state.offset]);
                                                                            };
                                                                            true
                                                                        })();
                                                                        if !__ok {
                                                                            state.offset = __pretty_cp285;
                                                                            __builder.restore(__pretty_bcp286);
                                                                        }
                                                                        __ok
                                                                    } {
                                                                        return false;
                                                                    }
                                                                };
                                                                {
                                                                    let mut __rep_count296 = 0usize;
                                                                    while __rep_count296 < 4294967295 {
                                                                        let __rep_cp297 = state.offset;
                                                                        if !{
                                                                            let __pretty_cp294 = state.offset;
                                                                            let __pretty_bcp295 = __builder.checkpoint();
                                                                            let __ok = (|| -> bool {
                                                                                {
                                                                                    {
                                                                                        let __ows287 = state.offset;
                                                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                                                        let __ows288 = state.offset;
                                                                                        {
                                                                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                                            {
                                                                                                return false;
                                                                                            }
                                                                                            state.offset += 1;
                                                                                            __builder.char(b',');
                                                                                        };
                                                                                        __builder.text_inline_ws(&state.src[__ows287..__ows288]);
                                                                                        let __ows289 = state.offset;
                                                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                                                        __builder
                                                                                            .text_inline_ws(&state.src[__ows289..state.offset]);
                                                                                    };
                                                                                    {
                                                                                        if !{
                                                                                            let __pretty_cp292 = state.offset;
                                                                                            let __pretty_bcp293 = __builder.checkpoint();
                                                                                            let __ok = (|| -> bool {
                                                                                                {
                                                                                                    let __ows290 = state.offset;
                                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                    __builder
                                                                                                        .text_inline_ws(&state.src[__ows290..state.offset]);
                                                                                                    if !Self::__call_arg_prettify(state, __builder) {
                                                                                                        return false;
                                                                                                    }
                                                                                                    let __ows291 = state.offset;
                                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                    __builder
                                                                                                        .text_inline_ws(&state.src[__ows291..state.offset]);
                                                                                                };
                                                                                                true
                                                                                            })();
                                                                                            if !__ok {
                                                                                                state.offset = __pretty_cp292;
                                                                                                __builder.restore(__pretty_bcp293);
                                                                                            }
                                                                                            __ok
                                                                                        } {
                                                                                            return false;
                                                                                        }
                                                                                    };
                                                                                };
                                                                                true
                                                                            })();
                                                                            if !__ok {
                                                                                state.offset = __pretty_cp294;
                                                                                __builder.restore(__pretty_bcp295);
                                                                            }
                                                                            __ok
                                                                        } {
                                                                            state.offset = __rep_cp297;
                                                                            break;
                                                                        }
                                                                        if state.offset == __rep_cp297 {
                                                                            break;
                                                                        }
                                                                        __rep_count296 += 1;
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
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp298;
                                                            __builder.restore(__pretty_bcp299);
                                                        }
                                                        __ok
                                                    };
                                                    true
                                                };
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp328;
                                            __builder.restore(__pretty_bcp329);
                                        }
                                        __ok
                                    } {
                                        {
                                            if !{
                                                let __pretty_cp326 = state.offset;
                                                let __pretty_bcp327 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    if !Self::__literal_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp326;
                                                    __builder.restore(__pretty_bcp327);
                                                }
                                                __ok
                                            } {
                                                {
                                                    if !{
                                                        let __pretty_cp324 = state.offset;
                                                        let __pretty_bcp325 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            if !Self::__regex_prettify(state, __builder) {
                                                                return false;
                                                            }
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp324;
                                                            __builder.restore(__pretty_bcp325);
                                                        }
                                                        __ok
                                                    } {
                                                        {
                                                            if !{
                                                                let __pretty_cp322 = state.offset;
                                                                let __pretty_bcp323 = __builder.checkpoint();
                                                                let __ok = (|| -> bool {
                                                                    {
                                                                        {
                                                                            let __s = "@{";
                                                                            let __bytes = __s.as_bytes();
                                                                            let __slc = match state.src_bytes.get(state.offset..) {
                                                                                Some(s) if s.len() >= 2usize => s,
                                                                                _ => return false,
                                                                            };
                                                                            if &__slc[..2usize] != __bytes {
                                                                                return false;
                                                                            }
                                                                            __builder
                                                                                .text(&state.src[state.offset..state.offset + 2usize]);
                                                                            state.offset += 2usize;
                                                                        };
                                                                        {
                                                                            if !{
                                                                                let __pretty_cp302 = state.offset;
                                                                                let __pretty_bcp303 = __builder.checkpoint();
                                                                                let __ok = (|| -> bool {
                                                                                    {
                                                                                        let __ows300 = state.offset;
                                                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                                                        __builder
                                                                                            .text_inline_ws(&state.src[__ows300..state.offset]);
                                                                                        if !Self::__rhs_prettify(state, __builder) {
                                                                                            return false;
                                                                                        }
                                                                                        let __ows301 = state.offset;
                                                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                                                        __builder
                                                                                            .text_inline_ws(&state.src[__ows301..state.offset]);
                                                                                    };
                                                                                    true
                                                                                })();
                                                                                if !__ok {
                                                                                    state.offset = __pretty_cp302;
                                                                                    __builder.restore(__pretty_bcp303);
                                                                                }
                                                                                __ok
                                                                            } {
                                                                                return false;
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
                                                                    true
                                                                })();
                                                                if !__ok {
                                                                    state.offset = __pretty_cp322;
                                                                    __builder.restore(__pretty_bcp323);
                                                                }
                                                                __ok
                                                            } {
                                                                {
                                                                    if !{
                                                                        let __pretty_cp320 = state.offset;
                                                                        let __pretty_bcp321 = __builder.checkpoint();
                                                                        let __ok = (|| -> bool {
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
                                                                                    if !{
                                                                                        let __pretty_cp306 = state.offset;
                                                                                        let __pretty_bcp307 = __builder.checkpoint();
                                                                                        let __ok = (|| -> bool {
                                                                                            {
                                                                                                let __ows304 = state.offset;
                                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                __builder
                                                                                                    .text_inline_ws(&state.src[__ows304..state.offset]);
                                                                                                if !Self::__rhs_prettify(state, __builder) {
                                                                                                    return false;
                                                                                                }
                                                                                                let __ows305 = state.offset;
                                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                __builder
                                                                                                    .text_inline_ws(&state.src[__ows305..state.offset]);
                                                                                            };
                                                                                            true
                                                                                        })();
                                                                                        if !__ok {
                                                                                            state.offset = __pretty_cp306;
                                                                                            __builder.restore(__pretty_bcp307);
                                                                                        }
                                                                                        __ok
                                                                                    } {
                                                                                        return false;
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
                                                                            true
                                                                        })();
                                                                        if !__ok {
                                                                            state.offset = __pretty_cp320;
                                                                            __builder.restore(__pretty_bcp321);
                                                                        }
                                                                        __ok
                                                                    } {
                                                                        {
                                                                            if !{
                                                                                let __pretty_cp318 = state.offset;
                                                                                let __pretty_bcp319 = __builder.checkpoint();
                                                                                let __ok = (|| -> bool {
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
                                                                                                let __pretty_cp310 = state.offset;
                                                                                                let __pretty_bcp311 = __builder.checkpoint();
                                                                                                let __ok = (|| -> bool {
                                                                                                    {
                                                                                                        let __ows308 = state.offset;
                                                                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                        __builder
                                                                                                            .text_inline_ws(&state.src[__ows308..state.offset]);
                                                                                                        if !Self::__rhs_prettify(state, __builder) {
                                                                                                            return false;
                                                                                                        }
                                                                                                        let __ows309 = state.offset;
                                                                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                        __builder
                                                                                                            .text_inline_ws(&state.src[__ows309..state.offset]);
                                                                                                    };
                                                                                                    true
                                                                                                })();
                                                                                                if !__ok {
                                                                                                    state.offset = __pretty_cp310;
                                                                                                    __builder.restore(__pretty_bcp311);
                                                                                                }
                                                                                                __ok
                                                                                            } {
                                                                                                return false;
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
                                                                                    true
                                                                                })();
                                                                                if !__ok {
                                                                                    state.offset = __pretty_cp318;
                                                                                    __builder.restore(__pretty_bcp319);
                                                                                }
                                                                                __ok
                                                                            } {
                                                                                {
                                                                                    if !{
                                                                                        let __pretty_cp316 = state.offset;
                                                                                        let __pretty_bcp317 = __builder.checkpoint();
                                                                                        let __ok = (|| -> bool {
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
                                                                                                        let __pretty_cp314 = state.offset;
                                                                                                        let __pretty_bcp315 = __builder.checkpoint();
                                                                                                        let __ok = (|| -> bool {
                                                                                                            {
                                                                                                                let __ows312 = state.offset;
                                                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                                __builder
                                                                                                                    .text_inline_ws(&state.src[__ows312..state.offset]);
                                                                                                                if !Self::__rhs_prettify(state, __builder) {
                                                                                                                    return false;
                                                                                                                }
                                                                                                                let __ows313 = state.offset;
                                                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                                __builder
                                                                                                                    .text_inline_ws(&state.src[__ows313..state.offset]);
                                                                                                            };
                                                                                                            true
                                                                                                        })();
                                                                                                        if !__ok {
                                                                                                            state.offset = __pretty_cp314;
                                                                                                            __builder.restore(__pretty_bcp315);
                                                                                                        }
                                                                                                        __ok
                                                                                                    } {
                                                                                                        return false;
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
                                                                                            true
                                                                                        })();
                                                                                        if !__ok {
                                                                                            state.offset = __pretty_cp316;
                                                                                            __builder.restore(__pretty_bcp317);
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
        fn __binary_factor_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !Self::__mapped_factor_prettify(state, __builder) {
                        return false;
                    }
                    {
                        let mut __rep_count338 = 0usize;
                        while __rep_count338 < 4294967295 {
                            let __rep_cp339 = state.offset;
                            if !{
                                let __pretty_cp336 = state.offset;
                                let __pretty_bcp337 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            if !{
                                                let __pretty_cp334 = state.offset;
                                                let __pretty_bcp335 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows332 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows332..state.offset]);
                                                        if !Self::__binary_operators_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        let __ows333 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows333..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp334;
                                                    __builder.restore(__pretty_bcp335);
                                                }
                                                __ok
                                            } {
                                                return false;
                                            }
                                        };
                                        if !Self::__mapped_factor_prettify(state, __builder) {
                                            return false;
                                        }
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp336;
                                    __builder.restore(__pretty_bcp337);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp339;
                                break;
                            }
                            if state.offset == __rep_cp339 {
                                break;
                            }
                            __rep_count338 += 1;
                        }
                    };
                };
                true
            }
        }
        pub fn binary_factor_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__binary_factor_prettify(state, &mut __builder) {
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
                {
                    if !{
                        let __pretty_cp340 = state.offset;
                        let __pretty_bcp341 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__closure_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp340;
                            __builder.restore(__pretty_bcp341);
                        }
                        __ok
                    } {
                        if !Self::__alternation_prettify(state, __builder) {
                            return false;
                        }
                    }
                };
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
        fn __factor_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let _ = {
                            let __pretty_cp342 = state.offset;
                            let __pretty_bcp343 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                if !Self::__big_comment_prettify(state, __builder) {
                                    return false;
                                }
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp342;
                                __builder.restore(__pretty_bcp343);
                            }
                            __ok
                        };
                        true
                    };
                    {
                        if !{
                            let __pretty_cp346 = state.offset;
                            let __pretty_bcp347 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows344 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows344..state.offset]);
                                    if !Self::__term_prettify(state, __builder) {
                                        return false;
                                    }
                                    let __ows345 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows345..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp346;
                                __builder.restore(__pretty_bcp347);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        let _ = {
                            let __pretty_cp348 = state.offset;
                            let __pretty_bcp349 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                if !Self::__modifier_prettify(state, __builder) {
                                    return false;
                                }
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp348;
                                __builder.restore(__pretty_bcp349);
                            }
                            __ok
                        };
                        true
                    };
                    {
                        let _ = {
                            let __pretty_cp350 = state.offset;
                            let __pretty_bcp351 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                if !Self::__big_comment_prettify(state, __builder) {
                                    return false;
                                }
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp350;
                                __builder.restore(__pretty_bcp351);
                            }
                            __ok
                        };
                        true
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
        fn __mapped_factor_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !Self::__factor_prettify(state, __builder) {
                        return false;
                    }
                    {
                        let _ = {
                            let __pretty_cp357 = state.offset;
                            let __pretty_bcp358 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows352 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows353 = state.offset;
                                        {
                                            let __s = "->";
                                            let __bytes = __s.as_bytes();
                                            let __slc = match state.src_bytes.get(state.offset..) {
                                                Some(s) if s.len() >= 2usize => s,
                                                _ => return false,
                                            };
                                            if &__slc[..2usize] != __bytes {
                                                return false;
                                            }
                                            __builder
                                                .text(&state.src[state.offset..state.offset + 2usize]);
                                            state.offset += 2usize;
                                        };
                                        __builder.text_inline_ws(&state.src[__ows352..__ows353]);
                                        let __ows354 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows354..state.offset]);
                                    };
                                    {
                                        if !Self::__value_expr_prettify(state, __builder) {
                                            return false;
                                        }
                                        {
                                            let _ = {
                                                let __pretty_cp355 = state.offset;
                                                let __pretty_bcp356 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    if !Self::__type_annotation_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp355;
                                                    __builder.restore(__pretty_bcp356);
                                                }
                                                __ok
                                            };
                                            true
                                        };
                                    };
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp357;
                                __builder.restore(__pretty_bcp358);
                            }
                            __ok
                        };
                        true
                    };
                };
                true
            }
        }
        pub fn mapped_factor_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__mapped_factor_prettify(state, &mut __builder) {
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
                        {
                            let __ows359 = state.offset;
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ows360 = state.offset;
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'=')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'=');
                            };
                            __builder.text_inline_ws(&state.src[__ows359..__ows360]);
                            let __ows361 = state.offset;
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __builder.text_inline_ws(&state.src[__ows361..state.offset]);
                        };
                        {
                            if !{
                                let __pretty_cp364 = state.offset;
                                let __pretty_bcp365 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        let __ows362 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows362..state.offset]);
                                        if !Self::__rhs_prettify(state, __builder) {
                                            return false;
                                        }
                                        let __ows363 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows363..state.offset]);
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp364;
                                    __builder.restore(__pretty_bcp365);
                                }
                                __ok
                            } {
                                return false;
                            }
                        };
                        {
                            if !{
                                let __pretty_cp366 = state.offset;
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
                                    state.offset = __pretty_cp366;
                                }
                                __ok
                            } {
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b'.');
                                };
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
        fn __recover_directive_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __ows367 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows368 = state.offset;
                        {
                            let __s = "@recover";
                            let __bytes = __s.as_bytes();
                            let __slc = match state.src_bytes.get(state.offset..) {
                                Some(s) if s.len() >= 8usize => s,
                                _ => return false,
                            };
                            if &__slc[..8usize] != __bytes {
                                return false;
                            }
                            __builder
                                .text(&state.src[state.offset..state.offset + 8usize]);
                            state.offset += 8usize;
                        };
                        __builder.text_inline_ws(&state.src[__ows367..__ows368]);
                        let __ows369 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows369..state.offset]);
                    };
                    {
                        let __ows370 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows371 = state.offset;
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
                        __builder.text_inline_ws(&state.src[__ows370..__ows371]);
                        let __ows372 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows372..state.offset]);
                    };
                    {
                        if !{
                            let __pretty_cp375 = state.offset;
                            let __pretty_bcp376 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows373 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows373..state.offset]);
                                    if !Self::__rhs_prettify(state, __builder) {
                                        return false;
                                    }
                                    let __ows374 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows374..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp375;
                                __builder.restore(__pretty_bcp376);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        let _ = {
                            let __pretty_cp378 = state.offset;
                            let __pretty_bcp379 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    if !{
                                        let __pretty_cp377 = state.offset;
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
                                            state.offset = __pretty_cp377;
                                        }
                                        __ok
                                    } {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'.');
                                        };
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp378;
                                __builder.restore(__pretty_bcp379);
                            }
                            __ok
                        };
                        true
                    };
                };
                true
            }
        }
        pub fn recover_directive_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__recover_directive_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __directive_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp392 = state.offset;
                        let __pretty_bcp393 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__import_directive_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp392;
                            __builder.restore(__pretty_bcp393);
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp390 = state.offset;
                                let __pretty_bcp391 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    if !Self::__recover_directive_prettify(state, __builder) {
                                        return false;
                                    }
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp390;
                                    __builder.restore(__pretty_bcp391);
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp388 = state.offset;
                                        let __pretty_bcp389 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            if !Self::__pretty_directive_prettify(state, __builder) {
                                                return false;
                                            }
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp388;
                                            __builder.restore(__pretty_bcp389);
                                        }
                                        __ok
                                    } {
                                        {
                                            if !{
                                                let __pretty_cp386 = state.offset;
                                                let __pretty_bcp387 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    if !Self::__ws_directive_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp386;
                                                    __builder.restore(__pretty_bcp387);
                                                }
                                                __ok
                                            } {
                                                {
                                                    if !{
                                                        let __pretty_cp384 = state.offset;
                                                        let __pretty_bcp385 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            if !Self::__token_directive_prettify(state, __builder) {
                                                                return false;
                                                            }
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp384;
                                                            __builder.restore(__pretty_bcp385);
                                                        }
                                                        __ok
                                                    } {
                                                        {
                                                            if !{
                                                                let __pretty_cp382 = state.offset;
                                                                let __pretty_bcp383 = __builder.checkpoint();
                                                                let __ok = (|| -> bool {
                                                                    if !Self::__debug_directive_prettify(state, __builder) {
                                                                        return false;
                                                                    }
                                                                    true
                                                                })();
                                                                if !__ok {
                                                                    state.offset = __pretty_cp382;
                                                                    __builder.restore(__pretty_bcp383);
                                                                }
                                                                __ok
                                                            } {
                                                                {
                                                                    if !{
                                                                        let __pretty_cp380 = state.offset;
                                                                        let __pretty_bcp381 = __builder.checkpoint();
                                                                        let __ok = (|| -> bool {
                                                                            if !Self::__host_directive_prettify(state, __builder) {
                                                                                return false;
                                                                            }
                                                                            true
                                                                        })();
                                                                        if !__ok {
                                                                            state.offset = __pretty_cp380;
                                                                            __builder.restore(__pretty_bcp381);
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
                                }
                            }
                        }
                    }
                };
                true
            }
        }
        pub fn directive_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__directive_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __grammar_item_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp404 = state.offset;
                        let __pretty_bcp405 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp396 = state.offset;
                                    let __pretty_bcp397 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            let __ows394 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder
                                                .text_inline_ws(&state.src[__ows394..state.offset]);
                                            {
                                                {
                                                    let __s = "//";
                                                    let __bytes = __s.as_bytes();
                                                    let __slc = match state.src_bytes.get(state.offset..) {
                                                        Some(s) if s.len() >= 2usize => s,
                                                        _ => return false,
                                                    };
                                                    if &__slc[..2usize] != __bytes {
                                                        return false;
                                                    }
                                                    __builder
                                                        .text(&state.src[state.offset..state.offset + 2usize]);
                                                    state.offset += 2usize;
                                                };
                                                {
                                                    let __start = state.offset;
                                                    if {
                                                        let __start = state.offset;
                                                        let __scan = if __start >= state.src_bytes.len() {
                                                            0
                                                        } else {
                                                            ({
                                                                static __LO_LUT: [u8; 16] = [
                                                                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                                                                ];
                                                                static __HI_LUT: [u8; 16] = [
                                                                    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                                ];
                                                                #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
                                                                let __result: ::core::option::Option<usize> = 'avx2_scan: {
                                                                    use ::core::arch::x86_64::*;
                                                                    unsafe {
                                                                        let __bytes = state.src_bytes.as_slice();
                                                                        let __len = __bytes.len();
                                                                        let __ptr = __bytes.as_ptr();
                                                                        let __lo_v = _mm256_broadcastsi128_si256(
                                                                            _mm_loadu_si128(__LO_LUT.as_ptr() as *const __m128i),
                                                                        );
                                                                        let __hi_v = _mm256_broadcastsi128_si256(
                                                                            _mm_loadu_si128(__HI_LUT.as_ptr() as *const __m128i),
                                                                        );
                                                                        let __lo_mask = _mm256_set1_epi8(0x0F);
                                                                        let __zero = _mm256_setzero_si256();
                                                                        let mut __i = __start;
                                                                        while __i + 32 <= __len {
                                                                            let __chunk = _mm256_loadu_si256(
                                                                                __ptr.add(__i) as *const __m256i,
                                                                            );
                                                                            let __lo_n = _mm256_and_si256(__chunk, __lo_mask);
                                                                            let __hi_n = _mm256_and_si256(
                                                                                _mm256_srli_epi16(__chunk, 4),
                                                                                __lo_mask,
                                                                            );
                                                                            let __lo_r = _mm256_shuffle_epi8(__lo_v, __lo_n);
                                                                            let __hi_r = _mm256_shuffle_epi8(__hi_v, __hi_n);
                                                                            let __matched = _mm256_and_si256(__lo_r, __hi_r);
                                                                            let __nz = _mm256_cmpgt_epi8(__matched, __zero);
                                                                            let __mask = _mm256_movemask_epi8(__nz) as u32;
                                                                            if __mask != 0 {
                                                                                let __rel = __mask.trailing_zeros() as usize;
                                                                                break 'avx2_scan {
                                                                                    ::core::option::Option::Some((__i + __rel) - __start)
                                                                                };
                                                                            }
                                                                            __i += 32;
                                                                        }
                                                                        let mut __byte_lut = [false; 256];
                                                                        {
                                                                            let mut __b: u16 = 0;
                                                                            while __b < 256 {
                                                                                let __blo = __LO_LUT[(__b & 0x0F) as usize];
                                                                                let __bhi = __HI_LUT[(__b >> 4) as usize];
                                                                                __byte_lut[__b as usize] = (__blo & __bhi) != 0;
                                                                                __b += 1;
                                                                            }
                                                                        }
                                                                        while __i < __len {
                                                                            let __b = *__ptr.add(__i);
                                                                            if __byte_lut[__b as usize] {
                                                                                break 'avx2_scan {
                                                                                    ::core::option::Option::Some(__i - __start)
                                                                                };
                                                                            }
                                                                            __i += 1;
                                                                        }
                                                                        ::core::option::Option::None
                                                                    }
                                                                };
                                                                #[cfg(
                                                                    not(all(target_arch = "x86_64", target_feature = "avx2"))
                                                                )]
                                                                let __result: ::core::option::Option<usize> = ::parse_that::find_next_structural_from(
                                                                        state.padded(),
                                                                        __start,
                                                                        &__LO_LUT,
                                                                        &__HI_LUT,
                                                                    )
                                                                    .map(|(pos, _)| pos - __start);
                                                                __result
                                                            })
                                                                .unwrap_or(state.src_bytes.len() - __start)
                                                        };
                                                        state.offset = __start + __scan;
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
                                            };
                                            let __ows395 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder
                                                .text_inline_ws(&state.src[__ows395..state.offset]);
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp396;
                                        __builder.restore(__pretty_bcp397);
                                    }
                                    __ok
                                } {
                                    return false;
                                }
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp404;
                            __builder.restore(__pretty_bcp405);
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp402 = state.offset;
                                let __pretty_bcp403 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    if !Self::__big_comment_prettify(state, __builder) {
                                        return false;
                                    }
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp402;
                                    __builder.restore(__pretty_bcp403);
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp400 = state.offset;
                                        let __pretty_bcp401 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            if !Self::__directive_prettify(state, __builder) {
                                                return false;
                                            }
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp400;
                                            __builder.restore(__pretty_bcp401);
                                        }
                                        __ok
                                    } {
                                        {
                                            if !{
                                                let __pretty_cp398 = state.offset;
                                                let __pretty_bcp399 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    if !Self::__rule_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp398;
                                                    __builder.restore(__pretty_bcp399);
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
                };
                true
            }
        }
        pub fn grammar_item_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__grammar_item_prettify(state, &mut __builder) {
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
                    let mut __rep_count411 = 0usize;
                    while __rep_count411 < 4294967295 {
                        let __rep_cp412 = state.offset;
                        let __iter_cp = if __rep_count411 > 0 {
                            Some(__builder.checkpoint())
                        } else {
                            None
                        };
                        if __rep_count411 > 0 {
                            __builder.hardline();
                        }
                        if !{
                            let __pretty_cp410 = state.offset;
                            let __ok = (|| -> bool {
                                {
                                    if !{
                                        let __pretty_cp408 = state.offset;
                                        let __pretty_bcp409 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                let __ows406 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows406..state.offset]);
                                                if !Self::__grammar_item_prettify(state, __builder) {
                                                    return false;
                                                }
                                                let __ows407 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows407..state.offset]);
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp408;
                                            __builder.restore(__pretty_bcp409);
                                        }
                                        __ok
                                    } {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp410;
                            }
                            __ok
                        } {
                            state.offset = __rep_cp412;
                            if let Some(__bcp) = __iter_cp {
                                __builder.restore(__bcp);
                            }
                            break;
                        }
                        if state.offset == __rep_cp412 {
                            if let Some(__bcp) = __iter_cp {
                                __builder.restore(__bcp);
                            }
                            break;
                        }
                        __rep_count411 += 1;
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
            crate::runtime::bbnf::BbnfDocument<'_>,
            crate::runtime::ParseErr,
        > {
            let __input_bytes = input.as_bytes();
            let mut state = __shape_support_BbnfBootstrap::ScanState::new();
            let mut builder = crate::runtime::bbnf::BbnfStructBuilder::new();
            {
                let mut pos: usize = 0;
                parse_BbnfBootstrap_grammar(
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
                let _ = __shape_support_BbnfBootstrap::skip_space(
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
pub use __bbnfbootstrap_emit_impl::*;
