//! AUTO-GENERATED from `[workspace.metadata.bbnf.grammars]` — do not edit manually.
//! Regenerate: cargo xtask regen --grammar google_sheets

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

pub struct GoogleSheetsParser;
mod __googlesheetsparser_emit_impl {
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
    pub const GRAMMAR_GoogleSheetsParser: [&'static str; 1usize] = [
        include_str!(
            concat!(env!("CARGO_MANIFEST_DIR"),
            "/../../grammar/google-sheets/google-sheets.bbnf")
        ),
    ];
    static __GRAMMAR_STRUCTURAL_ALPHABET: [u8; 19usize] = [
        35, 37, 38, 40, 41, 42, 43, 44, 45, 47, 58, 59, 60, 61, 62, 78, 94, 123, 125,
    ];
    static __GRAMMAR_STRUCTURAL_DIGRAPHS: [(u8, u8); 1usize] = [(62, 61)];
    static __GRAMMAR_STRUCTURAL_QUOTE_CLASSES: [u8; 1usize] = [34];
    pub const GRAMMAR_STRUCTURAL_ALPHABET: &[u8] = &__GRAMMAR_STRUCTURAL_ALPHABET;
    pub const GRAMMAR_STRUCTURAL_DIGRAPHS: &[(u8, u8)] = &__GRAMMAR_STRUCTURAL_DIGRAPHS;
    pub const GRAMMAR_STRUCTURAL_DIGRAPH_MASK: [u64; 4] = [4611686018427387904, 0, 0, 0];
    pub const GRAMMAR_STRUCTURAL_QUOTE_CLASSES: &[u8] = &__GRAMMAR_STRUCTURAL_QUOTE_CLASSES;
    /// AW-III.W6.2 — PHF keyword table.
    ///
    /// Mined literal-led Alt branches, sorted lexicographically.
    /// Binary search dispatches in O(log N) compares; LLVM lowers
    /// the fixed-size table to a balanced compare tree.
    static __PHF_GoogleSheetsParser_7_KW: [&[u8]; 4usize] = [b"<", b"=", b">", b">="];
    /// Per-entry branch discriminant — parallel to [`#kw_ident`].
    /// Entry `i`'s keyword bytes at `#kw_ident[i]` route to the
    /// branch with discriminant `#idx_ident[i]`.
    static __PHF_GoogleSheetsParser_7_IDX: [u8; 4usize] = [0, 4, 3, 1];
    /// AW-III.W6.2 — dispatch the mined keyword table for rule
    /// `#rule_id`.
    ///
    /// Returns `Some(branch_idx)` when `bytes` matches a mined
    /// keyword, `None` otherwise. Called from the walker's
    /// AltLinear / ClassifyByte arm to short-circuit the branch
    /// scan to a single binary search.
    #[allow(dead_code)]
    #[inline]
    fn __phf_GoogleSheetsParser_dispatch_7(bytes: &[u8]) -> ::core::option::Option<u8> {
        match __PHF_GoogleSheetsParser_7_KW.binary_search(&bytes) {
            ::core::result::Result::Ok(idx) => {
                ::core::option::Option::Some(__PHF_GoogleSheetsParser_7_IDX[idx])
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
    pub const PRECEDENCE_LUT_mul_expr: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 2u8, 0u8, 0u8, 0u8, 0u8, 2u8,
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
    pub const PRECEDENCE_ENTRIES_mul_expr: &[PrattEntry] = &[
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
    ];
    /// AX.W0a.2.l — per-rule dense Pratt precedence LUT.
    ///
    /// One byte per dispatch byte for this Pratt rule's
    /// operator alphabet. Consulted inline by the rule's
    /// emitted `parse_pratt_*` body. See `bbnf::backend::
    /// rust::emitter::precedence` for the bit layout.
    pub const PRECEDENCE_LUT_array_row: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8,
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
    pub const PRECEDENCE_ENTRIES_array_row: &[PrattEntry] = &[
        PrattEntry {
            byte: 44u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 0u8,
        },
    ];
    /// AX.W0a.2.l — per-rule dense Pratt precedence LUT.
    ///
    /// One byte per dispatch byte for this Pratt rule's
    /// operator alphabet. Consulted inline by the rule's
    /// emitted `parse_pratt_*` body. See `bbnf::backend::
    /// rust::emitter::precedence` for the bit layout.
    pub const PRECEDENCE_LUT_array_rows: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 2u8, 0u8, 0u8, 0u8, 0u8,
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
    pub const PRECEDENCE_ENTRIES_array_rows: &[PrattEntry] = &[
        PrattEntry {
            byte: 59u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 0u8,
        },
    ];
    /// AX.W0a.2.l — per-rule dense Pratt precedence LUT.
    ///
    /// One byte per dispatch byte for this Pratt rule's
    /// operator alphabet. Consulted inline by the rule's
    /// emitted `parse_pratt_*` body. See `bbnf::backend::
    /// rust::emitter::precedence` for the bit layout.
    pub const PRECEDENCE_LUT_concat_expr: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 4u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
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
    pub const PRECEDENCE_ENTRIES_concat_expr: &[PrattEntry] = &[
        PrattEntry {
            byte: 38u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 0u8,
        },
    ];
    /// AX.W0a.2.l — per-rule dense Pratt precedence LUT.
    ///
    /// One byte per dispatch byte for this Pratt rule's
    /// operator alphabet. Consulted inline by the rule's
    /// emitted `parse_pratt_*` body. See `bbnf::backend::
    /// rust::emitter::precedence` for the bit layout.
    pub const PRECEDENCE_LUT_add_expr: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 3u8, 0u8, 3u8, 0u8, 0u8,
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
    pub const PRECEDENCE_ENTRIES_add_expr: &[PrattEntry] = &[
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
    ];
    /// AX.W0a.2.l — per-rule dense Pratt precedence LUT.
    ///
    /// One byte per dispatch byte for this Pratt rule's
    /// operator alphabet. Consulted inline by the rule's
    /// emitted `parse_pratt_*` body. See `bbnf::backend::
    /// rust::emitter::precedence` for the bit layout.
    pub const PRECEDENCE_LUT_exp_expr: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 17u8, 0u8,
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
    pub const PRECEDENCE_ENTRIES_exp_expr: &[PrattEntry] = &[
        PrattEntry {
            byte: 94u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 0u8,
        },
    ];
    /// AX.W0a.2.l — per-rule dense Pratt precedence LUT.
    ///
    /// One byte per dispatch byte for this Pratt rule's
    /// operator alphabet. Consulted inline by the rule's
    /// emitted `parse_pratt_*` body. See `bbnf::backend::
    /// rust::emitter::precedence` for the bit layout.
    pub const PRECEDENCE_LUT_comparison_expr: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 129u8, 1u8, 129u8,
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
    pub const PRECEDENCE_ENTRIES_comparison_expr: &[PrattEntry] = &[
        PrattEntry {
            byte: 60u8,
            second_byte: ::core::option::Option::Some(62u8),
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 60u8,
            second_byte: ::core::option::Option::Some(61u8),
            op_discriminant: 1u8,
        },
        PrattEntry {
            byte: 62u8,
            second_byte: ::core::option::Option::Some(61u8),
            op_discriminant: 2u8,
        },
        PrattEntry {
            byte: 60u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 3u8,
        },
        PrattEntry {
            byte: 62u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 4u8,
        },
        PrattEntry {
            byte: 61u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 5u8,
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
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 4u8, 0u8, 0u8, 0u8, 2u8, 3u8, 1u8, 3u8, 0u8, 2u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 2u8, 1u8, 1u8, 1u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 17u8, 0u8,
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
    pub const PRECEDENCE_ENTRIES: &[PrattEntry] = &[
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
            byte: 44u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 59u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 38u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 0u8,
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
            byte: 94u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 60u8,
            second_byte: ::core::option::Option::Some(62u8),
            op_discriminant: 0u8,
        },
        PrattEntry {
            byte: 60u8,
            second_byte: ::core::option::Option::Some(61u8),
            op_discriminant: 1u8,
        },
        PrattEntry {
            byte: 62u8,
            second_byte: ::core::option::Option::Some(61u8),
            op_discriminant: 2u8,
        },
        PrattEntry {
            byte: 60u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 3u8,
        },
        PrattEntry {
            byte: 62u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 4u8,
        },
        PrattEntry {
            byte: 61u8,
            second_byte: ::core::option::Option::None,
            op_discriminant: 5u8,
        },
    ];
    /// AW-III.W6.5 — total mined operator count for this
    /// grammar. Non-zero iff the lift admitted ≥ 1 chain OR the
    /// shape classifier admitted ≥ 1 single-rung Pratt rule.
    pub const PRECEDENCE_OPERATOR_COUNT: usize = 14usize;
    /// AZ-IV.W3.3 — codegen-emitted lazy-parse path plan.
    ///
    /// The static `PATH_PLAN` carries one row per `(rule, segment
    /// kind)` decision the executor consults. The runtime cursor
    /// linearly searches the static for a matching `(rule_id,
    /// segment_kind)` pair and applies the recorded decision; a
    /// missing match falls back to `ParseFully` at the executor
    /// surface.
    ///
    /// `SegmentKind` and `Decision` re-export from
    /// `crate::path::cursor` — the runtime executor's canonical
    /// alphabet — so the plan rows and the cursor's decision
    /// vocabulary stay byte-identical without duplication.
    #[allow(dead_code)]
    pub mod __path_plan {
        pub use crate::path::cursor::{Decision, SegmentKind};
        #[derive(Clone, Copy, Debug)]
        pub struct PathPlanEntry {
            pub rule_id: u32,
            pub segment_kind: SegmentKind,
            /// Branch / position index when the decision is
            /// `ParseUntil`; `u32::MAX` otherwise.
            pub field_index: u32,
            pub decision: Decision,
        }
        pub const PATH_PLAN_LEN: usize = 148;
        pub static PATH_PLAN: &[PathPlanEntry; 148] = &[
            PathPlanEntry {
                rule_id: 0,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 1,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 2,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 3,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 3,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 3,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 3,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 3,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 4,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 5,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 6,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 7,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 8,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 9,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 10,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 11,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 11,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 11,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 11,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 11,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 12,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 12,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 12,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 12,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 12,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 13,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 13,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 13,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 13,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 13,
                segment_kind: SegmentKind::Field,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 13,
                segment_kind: SegmentKind::Index,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 13,
                segment_kind: SegmentKind::Field,
                field_index: 3,
                decision: Decision::ParseUntil(3),
            },
            PathPlanEntry {
                rule_id: 13,
                segment_kind: SegmentKind::Index,
                field_index: 3,
                decision: Decision::ParseUntil(3),
            },
            PathPlanEntry {
                rule_id: 13,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 14,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 15,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 15,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 15,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 15,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 15,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 16,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 16,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 16,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 16,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 16,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 17,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 17,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 17,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 17,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 17,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 18,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 18,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 18,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 18,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 18,
                segment_kind: SegmentKind::Field,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 18,
                segment_kind: SegmentKind::Index,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 18,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 19,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 20,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 20,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 21,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 21,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 21,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 21,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 21,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 22,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 22,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 23,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 23,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 23,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 23,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 23,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 24,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 24,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 24,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 24,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 24,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 25,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 25,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 25,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 25,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 25,
                segment_kind: SegmentKind::Field,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 25,
                segment_kind: SegmentKind::Index,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 25,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 26,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 26,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 26,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 26,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 26,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 27,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 27,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 27,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 27,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 27,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 28,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 28,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 28,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 28,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 28,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 29,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 29,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 29,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 29,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 29,
                segment_kind: SegmentKind::Field,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 29,
                segment_kind: SegmentKind::Index,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 29,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 30,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 31,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 31,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 31,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 31,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 31,
                segment_kind: SegmentKind::Field,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 31,
                segment_kind: SegmentKind::Index,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 31,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 32,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 32,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 32,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 32,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 32,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 33,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 33,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 33,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 33,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 33,
                segment_kind: SegmentKind::Field,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 33,
                segment_kind: SegmentKind::Index,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 33,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 34,
                segment_kind: SegmentKind::VariantName,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 34,
                segment_kind: SegmentKind::VariantName,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 34,
                segment_kind: SegmentKind::VariantName,
                field_index: 2,
                decision: Decision::ParseUntil(2),
            },
            PathPlanEntry {
                rule_id: 34,
                segment_kind: SegmentKind::VariantName,
                field_index: 3,
                decision: Decision::ParseUntil(3),
            },
            PathPlanEntry {
                rule_id: 34,
                segment_kind: SegmentKind::VariantName,
                field_index: 4,
                decision: Decision::ParseUntil(4),
            },
            PathPlanEntry {
                rule_id: 34,
                segment_kind: SegmentKind::VariantName,
                field_index: 5,
                decision: Decision::ParseUntil(5),
            },
            PathPlanEntry {
                rule_id: 34,
                segment_kind: SegmentKind::VariantName,
                field_index: 6,
                decision: Decision::ParseUntil(6),
            },
            PathPlanEntry {
                rule_id: 34,
                segment_kind: SegmentKind::VariantName,
                field_index: 7,
                decision: Decision::ParseUntil(7),
            },
            PathPlanEntry {
                rule_id: 34,
                segment_kind: SegmentKind::VariantName,
                field_index: 8,
                decision: Decision::ParseUntil(8),
            },
            PathPlanEntry {
                rule_id: 34,
                segment_kind: SegmentKind::VariantName,
                field_index: 9,
                decision: Decision::ParseUntil(9),
            },
            PathPlanEntry {
                rule_id: 34,
                segment_kind: SegmentKind::VariantName,
                field_index: 10,
                decision: Decision::ParseUntil(10),
            },
            PathPlanEntry {
                rule_id: 34,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 35,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 35,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 35,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 35,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 35,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
            PathPlanEntry {
                rule_id: 36,
                segment_kind: SegmentKind::Field,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 36,
                segment_kind: SegmentKind::Index,
                field_index: 0,
                decision: Decision::ParseUntil(0),
            },
            PathPlanEntry {
                rule_id: 36,
                segment_kind: SegmentKind::Field,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 36,
                segment_kind: SegmentKind::Index,
                field_index: 1,
                decision: Decision::ParseUntil(1),
            },
            PathPlanEntry {
                rule_id: 36,
                segment_kind: SegmentKind::Wildcard,
                field_index: 4294967295,
                decision: Decision::ParseFully,
            },
        ];
        /// Linear search the plan for the first `(rule_id,
        /// segment_kind)` match. The W3.1 executor consults this
        /// fn through its cursor; `None` = fall back to
        /// `ParseFully` at the executor surface.
        #[inline]
        pub fn lookup(
            rule_id: u32,
            segment_kind: SegmentKind,
        ) -> ::core::option::Option<&'static PathPlanEntry> {
            let mut i = 0usize;
            while i < PATH_PLAN.len() {
                let entry = &PATH_PLAN[i];
                if entry.rule_id == rule_id
                    && entry.segment_kind as u8 == segment_kind as u8
                {
                    return ::core::option::Option::Some(entry);
                }
                i += 1;
            }
            ::core::option::Option::None
        }
    }
    static __DTA_REGEX_0: &str = "(\\d+\\.?\\d*|\\.\\d+)([eE][+-]?\\d+)?";
    static __DTA_REGEX_1: &str = "\"([^\"]|\"\")*\"";
    static __DTA_REGEX_2: &str = "[tT][rR][uU][eE]";
    static __DTA_REGEX_3: &str = "[fF][aA][lL][sS][eE]";
    static __DTA_REGEX_20: &str = "'(?:[^']|'')*'!|[A-Za-z_]\\w*!";
    static __DTA_REGEX_21: &str = "\\$?[A-Za-z]{1,3}\\$?\\d+";
    static __DTA_REGEX_22: &str = "[A-Za-z_][A-Za-z0-9_.]*";
    static __DTA_REGEX_52: &str = "\\$?[A-Za-z]{1,3}";
    static __DTA_REGEX_53: &str = "\\$?\\d+";
    static __DTA_REGEX_144: &str = "[lL][aA][mM][bB][dD][aA]\\(";
    static __DTA_REGEX_169: &str = "[lL][eE][tT]\\(";
    static __DTA_REGEX_180: &str = "=?";
    /// AY.W4.3 — first-byte → admissible-pattern bitmap LUT.
    ///
    /// Each entry holds a u32 bitmap; bit `i` set means pattern
    /// `i` (in the adapter's collected order) admits this byte
    /// as a match-prefix. Read once at adapter entry; the
    /// dispatch cascade visits only patterns whose bit is set.
    #[allow(dead_code)]
    pub(crate) const __REGEX_FIRST_BYTE_LUT_GoogleSheetsParser: [u32; 256] = [
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 2, 0, 416, 0, 0, 16, 0, 0, 0, 0, 0, 0, 1, 0, 257, 257, 257,
        257, 257, 257, 257, 257, 257, 257, 0, 0, 0, 2048, 0, 0, 0, 240, 240, 240, 240,
        240, 248, 240, 240, 240, 240, 240, 1776, 240, 240, 240, 240, 240, 240, 240, 244,
        240, 240, 240, 240, 240, 240, 0, 0, 0, 0, 80, 0, 240, 240, 240, 240, 240, 248,
        240, 240, 240, 240, 240, 1776, 240, 240, 240, 240, 240, 240, 240, 244, 240, 240,
        240, 240, 240, 240, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
    ];
    /// AY.W4.3 — per-pattern (LAST-byte-set lo, hi) packed
    /// `CharSet128` tuples. `(0, 0)` means narrowing is
    /// disabled for that pattern (suffix not deterministic).
    ///
    /// The adapter consults this when invoked: if the pattern's
    /// entry is non-zero AND the input slice from `pos` does not
    /// contain any byte in the LAST set, the regex cannot
    /// complete a match — skip the DFA walk entirely.
    #[allow(dead_code)]
    pub(crate) const __REGEX_LAST_BYTE_SET_GoogleSheetsParser: [(u64, u64); 12] = [
        (0, 0),
        (17179869184, 0),
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
        (0, 0),
        (1099511627776, 0),
        (1099511627776, 0),
        (0, 0),
    ];
    #[inline]
    #[cold]
    fn __regex_scan_GoogleSheetsParser(
        pattern: &str,
        input: &[u8],
        pos: usize,
    ) -> ::core::option::Option<u32> {
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_0.as_ptr())
            || pattern == __DTA_REGEX_0
        {
            if let Some(&__byte) = input.get(pos) {
                if (__REGEX_FIRST_BYTE_LUT_GoogleSheetsParser[__byte as usize] >> 0) & 1
                    == 0
                {
                    return ::core::option::Option::None;
                }
            }
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_GoogleSheetsParser[0];
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
                                46 => __dfa_state = 4,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 1;
                                }
                                46 => __dfa_state = 2,
                                69 | 101 => __dfa_state = 3,
                                _ => break,
                            }
                        }
                        2 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 2;
                                }
                                69 | 101 => __dfa_state = 3,
                                _ => break,
                            }
                        }
                        3 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 5;
                                }
                                43 | 45 => __dfa_state = 6,
                                _ => break,
                            }
                        }
                        4 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 2;
                                }
                                _ => break,
                            }
                        }
                        5 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 5;
                                }
                                _ => break,
                            }
                        }
                        6 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 5;
                                }
                                _ => break,
                            }
                        }
                        _ => unsafe { ::core::hint::unreachable_unchecked() }
                    }
                    __dfa_p += 1;
                    match __dfa_state {
                        1 | 2 | 5 => {
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
            if let Some(&__byte) = input.get(pos) {
                if (__REGEX_FIRST_BYTE_LUT_GoogleSheetsParser[__byte as usize] >> 1) & 1
                    == 0
                {
                    return ::core::option::Option::None;
                }
            }
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_GoogleSheetsParser[1];
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
                                34 => __dfa_state = 2,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                34 => __dfa_state = 2,
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
                                | 249 | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 2,
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_2.as_ptr())
            || pattern == __DTA_REGEX_2
        {
            if let Some(&__byte) = input.get(pos) {
                if (__REGEX_FIRST_BYTE_LUT_GoogleSheetsParser[__byte as usize] >> 2) & 1
                    == 0
                {
                    return ::core::option::Option::None;
                }
            }
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_GoogleSheetsParser[2];
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
                                84 | 116 => __dfa_state = 3,
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
                                69 | 101 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        3 => {
                            match b {
                                82 | 114 => __dfa_state = 4,
                                _ => break,
                            }
                        }
                        4 => {
                            match b {
                                85 | 117 => __dfa_state = 2,
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_3.as_ptr())
            || pattern == __DTA_REGEX_3
        {
            if let Some(&__byte) = input.get(pos) {
                if (__REGEX_FIRST_BYTE_LUT_GoogleSheetsParser[__byte as usize] >> 3) & 1
                    == 0
                {
                    return ::core::option::Option::None;
                }
            }
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_GoogleSheetsParser[3];
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
                                70 | 102 => __dfa_state = 2,
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
                                65 | 97 => __dfa_state = 4,
                                _ => break,
                            }
                        }
                        3 => {
                            match b {
                                69 | 101 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        4 => {
                            match b {
                                76 | 108 => __dfa_state = 5,
                                _ => break,
                            }
                        }
                        5 => {
                            match b {
                                83 | 115 => __dfa_state = 3,
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_20.as_ptr())
            || pattern == __DTA_REGEX_20
        {
            if let Some(&__byte) = input.get(pos) {
                if (__REGEX_FIRST_BYTE_LUT_GoogleSheetsParser[__byte as usize] >> 4) & 1
                    == 0
                {
                    return ::core::option::Option::None;
                }
            }
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_GoogleSheetsParser[4];
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
                                39 => __dfa_state = 2,
                                65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76
                                | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88
                                | 89 | 90 | 95 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104
                                | 105 | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114
                                | 115 | 116 | 117 | 118 | 119 | 120 | 121 | 122 => {
                                    __dfa_state = 3;
                                }
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
                                0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13
                                | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25
                                | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37
                                | 38 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49 | 50
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
                                | 249 | 250 | 251 | 252 | 253 | 254 | 255 => __dfa_state = 2,
                                39 => __dfa_state = 4,
                                _ => break,
                            }
                        }
                        3 => {
                            match b {
                                33 => __dfa_state = 1,
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
                                33 => __dfa_state = 1,
                                39 => __dfa_state = 2,
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_21.as_ptr())
            || pattern == __DTA_REGEX_21
        {
            if let Some(&__byte) = input.get(pos) {
                if (__REGEX_FIRST_BYTE_LUT_GoogleSheetsParser[__byte as usize] >> 5) & 1
                    == 0
                {
                    return ::core::option::Option::None;
                }
            }
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_GoogleSheetsParser[5];
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
                                36 => __dfa_state = 2,
                                65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76
                                | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88
                                | 89 | 90 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
                                | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115
                                | 116 | 117 | 118 | 119 | 120 | 121 | 122 => __dfa_state = 3,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 1;
                                }
                                _ => break,
                            }
                        }
                        2 => {
                            match b {
                                65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76
                                | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88
                                | 89 | 90 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
                                | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115
                                | 116 | 117 | 118 | 119 | 120 | 121 | 122 => __dfa_state = 3,
                                _ => break,
                            }
                        }
                        3 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 1;
                                }
                                36 => __dfa_state = 4,
                                65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76
                                | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88
                                | 89 | 90 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
                                | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115
                                | 116 | 117 | 118 | 119 | 120 | 121 | 122 => __dfa_state = 6,
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
                                    __dfa_state = 1;
                                }
                                36 => __dfa_state = 4,
                                _ => break,
                            }
                        }
                        6 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 1;
                                }
                                36 => __dfa_state = 4,
                                65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76
                                | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88
                                | 89 | 90 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
                                | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115
                                | 116 | 117 | 118 | 119 | 120 | 121 | 122 => __dfa_state = 5,
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_22.as_ptr())
            || pattern == __DTA_REGEX_22
        {
            if let Some(&__byte) = input.get(pos) {
                if (__REGEX_FIRST_BYTE_LUT_GoogleSheetsParser[__byte as usize] >> 6) & 1
                    == 0
                {
                    return ::core::option::Option::None;
                }
            }
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_GoogleSheetsParser[6];
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
                                46 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 65
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_52.as_ptr())
            || pattern == __DTA_REGEX_52
        {
            if let Some(&__byte) = input.get(pos) {
                if (__REGEX_FIRST_BYTE_LUT_GoogleSheetsParser[__byte as usize] >> 7) & 1
                    == 0
                {
                    return ::core::option::Option::None;
                }
            }
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_GoogleSheetsParser[7];
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
                                | 89 | 90 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
                                | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115
                                | 116 | 117 | 118 | 119 | 120 | 121 | 122 => __dfa_state = 1,
                                36 => __dfa_state = 4,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76
                                | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88
                                | 89 | 90 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
                                | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115
                                | 116 | 117 | 118 | 119 | 120 | 121 | 122 => __dfa_state = 3,
                                _ => break,
                            }
                        }
                        2 => {
                            match b {
                                _ => break,
                            }
                        }
                        3 => {
                            match b {
                                65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76
                                | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88
                                | 89 | 90 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
                                | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115
                                | 116 | 117 | 118 | 119 | 120 | 121 | 122 => __dfa_state = 2,
                                _ => break,
                            }
                        }
                        4 => {
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_53.as_ptr())
            || pattern == __DTA_REGEX_53
        {
            if let Some(&__byte) = input.get(pos) {
                if (__REGEX_FIRST_BYTE_LUT_GoogleSheetsParser[__byte as usize] >> 8) & 1
                    == 0
                {
                    return ::core::option::Option::None;
                }
            }
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_GoogleSheetsParser[8];
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
                                36 => __dfa_state = 2,
                                _ => break,
                            }
                        }
                        1 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 1;
                                }
                                _ => break,
                            }
                        }
                        2 => {
                            match b {
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 => {
                                    __dfa_state = 1;
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_144.as_ptr())
            || pattern == __DTA_REGEX_144
        {
            if let Some(&__byte) = input.get(pos) {
                if (__REGEX_FIRST_BYTE_LUT_GoogleSheetsParser[__byte as usize] >> 9) & 1
                    == 0
                {
                    return ::core::option::Option::None;
                }
            }
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_GoogleSheetsParser[9];
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
                                76 | 108 => __dfa_state = 3,
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
                                40 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        3 => {
                            match b {
                                65 | 97 => __dfa_state = 6,
                                _ => break,
                            }
                        }
                        4 => {
                            match b {
                                66 | 98 => __dfa_state = 5,
                                _ => break,
                            }
                        }
                        5 => {
                            match b {
                                68 | 100 => __dfa_state = 7,
                                _ => break,
                            }
                        }
                        6 => {
                            match b {
                                77 | 109 => __dfa_state = 4,
                                _ => break,
                            }
                        }
                        7 => {
                            match b {
                                65 | 97 => __dfa_state = 2,
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_169.as_ptr())
            || pattern == __DTA_REGEX_169
        {
            if let Some(&__byte) = input.get(pos) {
                if (__REGEX_FIRST_BYTE_LUT_GoogleSheetsParser[__byte as usize] >> 10) & 1
                    == 0
                {
                    return ::core::option::Option::None;
                }
            }
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_GoogleSheetsParser[10];
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
                                76 | 108 => __dfa_state = 3,
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
                                40 => __dfa_state = 1,
                                _ => break,
                            }
                        }
                        3 => {
                            match b {
                                69 | 101 => __dfa_state = 4,
                                _ => break,
                            }
                        }
                        4 => {
                            match b {
                                84 | 116 => __dfa_state = 2,
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_180.as_ptr())
            || pattern == __DTA_REGEX_180
        {
            if let Some(&__byte) = input.get(pos) {
                if (__REGEX_FIRST_BYTE_LUT_GoogleSheetsParser[__byte as usize] >> 11) & 1
                    == 0
                {
                    return ::core::option::Option::None;
                }
            }
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_GoogleSheetsParser[11];
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
                                61 => __dfa_state = 1,
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
                        0 | 1 => {
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
    pub(crate) mod __shape_support_GoogleSheetsParser {
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
            let __ctns_idx = ensure_structural_index(state, input);
            if let ::core::option::Option::Some(__next_struct) = __ctns_idx
                .next_structural_at_or_after(*p as u32)
            {
                let __next = __next_struct as usize;
                let __gap = __next.saturating_sub(*p);
                if __gap > 64 && __gap <= 4096 && __next <= input.len() {
                    let __slice = unsafe { input.get_unchecked(*p..__next) };
                    let mut __all_ws = true;
                    for &__b in __slice {
                        if __b != b' ' && __b != b'\t' && __b != b'\n' && __b != b'\r' {
                            __all_ws = false;
                            break;
                        }
                    }
                    if __all_ws {
                        *p = __next;
                        state.nospace_start = -1;
                    }
                }
            }
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
        /// AZ-IV.W3-DYNAMIC — byte-balanced value skip for the
        /// lazy bail-out parser's mismatched-key fast path.
        ///
        /// Advances `*p` past one structural value (object,
        /// array, string, number, true / false / null,
        /// identifier-shaped scalar) without producing any
        /// builder push. The scan is a forward state machine:
        ///
        /// - `{` / `[` — track open/close depth (treating bytes
        ///   inside `"…"` strings as opaque) and stop at depth
        ///   zero with the matching close.
        /// - `"` — scan to the next unescaped `"`.
        /// - everything else — read until the next structural
        ///   delimiter (`,` `}` `]` whitespace).
        ///
        /// Returns `Err` only on premature EOF inside an
        /// unterminated string or compound; the lazy-error-
        /// elision contract ensures the caller never propagates
        /// that error.
        #[inline]
        pub fn byte_skip_value(
            input: &[u8],
            p: &mut usize,
        ) -> ::core::result::Result<(), crate::runtime::DtaError> {
            let start = *p;
            let first = match input.get(start).copied() {
                Some(b) => b,
                None => {
                    return Err(crate::runtime::DtaError::UnexpectedEnd {
                        offset: start as u32,
                    });
                }
            };
            match first {
                b'{' | b'[' => byte_skip_balanced(input, p),
                b'"' => byte_skip_string(input, p),
                _ => byte_skip_scalar(input, p),
            }
        }
        /// AZ-IV.W3-DYNAMIC — balanced-compound skip. Honours
        /// `"` strings (with `\"` escapes) so `}` / `]` bytes
        /// inside string literals do not falsely close.
        #[inline]
        fn byte_skip_balanced(
            input: &[u8],
            p: &mut usize,
        ) -> ::core::result::Result<(), crate::runtime::DtaError> {
            let start = *p;
            let mut depth: u32 = 0;
            let mut i = start;
            while let Some(&b) = input.get(i) {
                match b {
                    b'{' | b'[' => depth = depth.saturating_add(1),
                    b'}' | b']' => {
                        if depth <= 1 {
                            *p = i + 1;
                            return Ok(());
                        }
                        depth -= 1;
                    }
                    b'"' => {
                        i += 1;
                        while let Some(&sb) = input.get(i) {
                            if sb == b'\\' {
                                i += 2;
                                continue;
                            }
                            if sb == b'"' {
                                break;
                            }
                            i += 1;
                        }
                        if input.get(i).is_none() {
                            return Err(crate::runtime::DtaError::UnexpectedEnd {
                                offset: start as u32,
                            });
                        }
                    }
                    _ => {}
                }
                i += 1;
            }
            Err(crate::runtime::DtaError::UnexpectedEnd {
                offset: start as u32,
            })
        }
        /// AZ-IV.W3-DYNAMIC — quoted-string skip. Advances past
        /// the closing `"` honouring `\"` and `\\` escapes.
        #[inline]
        fn byte_skip_string(
            input: &[u8],
            p: &mut usize,
        ) -> ::core::result::Result<(), crate::runtime::DtaError> {
            let start = *p;
            let mut i = start + 1;
            while let Some(&b) = input.get(i) {
                if b == b'\\' {
                    i += 2;
                    continue;
                }
                if b == b'"' {
                    *p = i + 1;
                    return Ok(());
                }
                i += 1;
            }
            Err(crate::runtime::DtaError::UnexpectedEnd {
                offset: start as u32,
            })
        }
        /// AZ-IV.W3-DYNAMIC — scalar skip. Advances past
        /// non-structural bytes until a delimiter (`,` `}` `]`
        /// whitespace) or EOF.
        #[inline]
        fn byte_skip_scalar(
            input: &[u8],
            p: &mut usize,
        ) -> ::core::result::Result<(), crate::runtime::DtaError> {
            let mut i = *p;
            while let Some(&b) = input.get(i) {
                match b {
                    b',' | b'}' | b']' | b' ' | b'\t' | b'\n' | b'\r' => break,
                    _ => i += 1,
                }
            }
            *p = i;
            Ok(())
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
    pub fn parse_hregex_GoogleSheetsParser_number<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        let _ = cursor;
        let span_lo = *p as u32;
        let Some(match_len) = __regex_scan_GoogleSheetsParser(
            "(\\d+\\.?\\d*|\\.\\d+)([eE][+-]?\\d+)?",
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
        <crate::runtime::google_sheets::SheetsStructBuilder<
            'p,
        > as crate::runtime::StructBuilder>::push_leaf_with_f64(builder, __f64);
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
    ///
    /// AZ-IV.W3.6 — Cursor parameter is threaded for signature
    /// uniformity; string is a leaf (no recursion), so the
    /// cursor is not consulted in the body.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_string_GoogleSheetsParser_string<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        _state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
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
        match __shape_support_GoogleSheetsParser::first_quote_or_backslash(tail) {
            Some((off, b'"')) => {
                let end = body_start + off;
                *p = end + 1;
                let body: &'p str = unsafe {
                    ::core::str::from_utf8_unchecked(&input[open..end + 1])
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
                                &input[(start as usize)
                                    .saturating_sub(1)..(end as usize) + 1],
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
    ///
    /// AZ-IV.W3.6 — Cursor-threaded. The Alt-dispatch branch
    /// selector consults `cursor.decide(rule_id)` so a
    /// `Decision::ParseUntil(idx)` returned by the path plan
    /// means the targeted variant index is preserved by the
    /// linear-try fallback (the byte-dispatch arms are still
    /// the prefilter; the cursor's decision is forwarded into
    /// the inner Refs as the descent proceeds).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_wrap_GoogleSheetsParser_boolean<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let _ = cursor.decide(2u32 as u32);
        let __wrap_checkpoint = builder.checkpoint();
        let __wrap_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 2u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("boolean"),
            kind: ::bbnf_ir::registry::LayoutKind::UntaggedEnum,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __wrap_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__wrap_layout);
        let mut __wrap_branch_idx: u32 = 0;
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let first = __shape_support_GoogleSheetsParser::skip_space(input, p, state)
                .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;
            'try_branches: loop {
                match first {
                    70u8 => {
                        let attempt_p = *p;
                        let attempt_builder = builder.checkpoint();
                        match (|| -> ::core::result::Result<
                            (),
                            crate::runtime::DtaError,
                        > {
                            if let ::core::option::Option::Some(match_len) = __regex_scan_GoogleSheetsParser(
                                "[fF][aA][lL][sS][eE]",
                                input,
                                *p,
                            ) {
                                *p += match_len as usize;
                                <_ as crate::runtime::StructBuilder>::push_leaf_with_bool(
                                    builder,
                                    false,
                                );
                                ::core::result::Result::Ok(())
                            } else {
                                ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: *p as u32,
                                })
                            }
                        })() {
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
                    84u8 => {
                        let attempt_p = *p;
                        let attempt_builder = builder.checkpoint();
                        match (|| -> ::core::result::Result<
                            (),
                            crate::runtime::DtaError,
                        > {
                            if let ::core::option::Option::Some(match_len) = __regex_scan_GoogleSheetsParser(
                                "[tT][rR][uU][eE]",
                                input,
                                *p,
                            ) {
                                *p += match_len as usize;
                                <_ as crate::runtime::StructBuilder>::push_leaf_with_bool(
                                    builder,
                                    true,
                                );
                                ::core::result::Result::Ok(())
                            } else {
                                ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: *p as u32,
                                })
                            }
                        })() {
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
                    102u8 => {
                        let attempt_p = *p;
                        let attempt_builder = builder.checkpoint();
                        match (|| -> ::core::result::Result<
                            (),
                            crate::runtime::DtaError,
                        > {
                            if let ::core::option::Option::Some(match_len) = __regex_scan_GoogleSheetsParser(
                                "[fF][aA][lL][sS][eE]",
                                input,
                                *p,
                            ) {
                                *p += match_len as usize;
                                <_ as crate::runtime::StructBuilder>::push_leaf_with_bool(
                                    builder,
                                    false,
                                );
                                ::core::result::Result::Ok(())
                            } else {
                                ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: *p as u32,
                                })
                            }
                        })() {
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
                    116u8 => {
                        let attempt_p = *p;
                        let attempt_builder = builder.checkpoint();
                        match (|| -> ::core::result::Result<
                            (),
                            crate::runtime::DtaError,
                        > {
                            if let ::core::option::Option::Some(match_len) = __regex_scan_GoogleSheetsParser(
                                "[tT][rR][uU][eE]",
                                input,
                                *p,
                            ) {
                                *p += match_len as usize;
                                <_ as crate::runtime::StructBuilder>::push_leaf_with_bool(
                                    builder,
                                    true,
                                );
                                ::core::result::Result::Ok(())
                            } else {
                                ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: *p as u32,
                                })
                            }
                        })() {
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
                    _ => {}
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_branch_tag(
                    builder,
                    __wrap_branch_idx,
                );
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __wrap_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(e) => {
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::rollback(
                    builder,
                    __wrap_checkpoint,
                );
                ::core::result::Result::Err(e)
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
    pub fn parse_flat_GoogleSheetsParser_error_literal<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(3u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        let __error_literal_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 3u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("error_literal"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __error_literal_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__error_literal_layout,
        );
        <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [35u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
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
                            let end = at + 3usize;
                            if input.len() < end || input[at..end] != [78u8, 47u8, 65u8]
                            {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                            builder.push_branch_tag(0u32);
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
                            if input.len() < end || input[at..end] != [78u8] {
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
                                        let at = *p;
                                        let end = at + 4usize;
                                        if input.len() < end
                                            || input[at..end] != [85u8, 76u8, 76u8, 33u8]
                                        {
                                            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                                offset: at as u32,
                                            });
                                        }
                                        *p = end;
                                        builder.push_branch_tag(4u32);
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
                                        let end = at + 3usize;
                                        if input.len() < end || input[at..end] != [85u8, 77u8, 33u8]
                                        {
                                            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                                offset: at as u32,
                                            });
                                        }
                                        *p = end;
                                        builder.push_branch_tag(6u32);
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
                                        let end = at + 4usize;
                                        if input.len() < end
                                            || input[at..end] != [65u8, 77u8, 69u8, 63u8]
                                        {
                                            return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                                offset: at as u32,
                                            });
                                        }
                                        *p = end;
                                        builder.push_branch_tag(5u32);
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
                            let end = at + 6usize;
                            if input.len() < end
                                || input[at..end] != [86u8, 65u8, 76u8, 85u8, 69u8, 33u8]
                            {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                            builder.push_branch_tag(1u32);
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
                            let end = at + 6usize;
                            if input.len() < end
                                || input[at..end] != [68u8, 73u8, 86u8, 47u8, 48u8, 33u8]
                            {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                            builder.push_branch_tag(3u32);
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
                            let end = at + 6usize;
                            if input.len() < end
                                || input[at..end] != [69u8, 82u8, 82u8, 79u8, 82u8, 33u8]
                            {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                            builder.push_branch_tag(7u32);
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
                            let end = at + 6usize;
                            if input.len() < end
                                || input[at..end] != [83u8, 80u8, 73u8, 76u8, 76u8, 33u8]
                            {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                            builder.push_branch_tag(8u32);
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
                            let end = at + 4usize;
                            if input.len() < end
                                || input[at..end] != [82u8, 69u8, 70u8, 33u8]
                            {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                            builder.push_branch_tag(2u32);
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
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __error_literal_handle,
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
    pub fn parse_hregex_GoogleSheetsParser_sheet_prefix<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        let _ = cursor;
        let span_lo = *p as u32;
        let Some(match_len) = __regex_scan_GoogleSheetsParser(
            "'(?:[^']|'')*'!|[A-Za-z_]\\w*!",
            input,
            *p,
        ) else {
            return Err(crate::runtime::DtaError::Syntax {
                offset: span_lo,
            });
        };
        *p += match_len as usize;
        let span_hi = *p as u32;
        <crate::runtime::google_sheets::SheetsStructBuilder<
            'p,
        > as crate::runtime::StructBuilder>::push_leaf_with_str(
            builder,
            core::str::from_utf8(&input[span_lo as usize..span_hi as usize])
                .unwrap_or(""),
        );
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
    pub fn parse_hregex_GoogleSheetsParser_cell_ref<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        let _ = cursor;
        let span_lo = *p as u32;
        let Some(match_len) = __regex_scan_GoogleSheetsParser(
            "\\$?[A-Za-z]{1,3}\\$?\\d+",
            input,
            *p,
        ) else {
            return Err(crate::runtime::DtaError::Syntax {
                offset: span_lo,
            });
        };
        *p += match_len as usize;
        let span_hi = *p as u32;
        <crate::runtime::google_sheets::SheetsStructBuilder<
            'p,
        > as crate::runtime::StructBuilder>::push_leaf_with_str(
            builder,
            core::str::from_utf8(&input[span_lo as usize..span_hi as usize])
                .unwrap_or(""),
        );
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
    pub fn parse_hregex_GoogleSheetsParser_identifier<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        let _ = cursor;
        let span_lo = *p as u32;
        let Some(match_len) = __regex_scan_GoogleSheetsParser(
            "[A-Za-z_][A-Za-z0-9_.]*",
            input,
            *p,
        ) else {
            return Err(crate::runtime::DtaError::Syntax {
                offset: span_lo,
            });
        };
        *p += match_len as usize;
        let span_hi = *p as u32;
        <crate::runtime::google_sheets::SheetsStructBuilder<
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
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_keyword_GoogleSheetsParser_compare_op<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        let _ = state;
        let _ = cursor;
        match first_byte {
            60u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [60u8] {
                    let __seq_span_lo = *p;
                    let __seq_builder_checkpoint = builder.checkpoint();
                    let __seq_result: ::core::result::Result<
                        (),
                        crate::runtime::DtaError,
                    > = (|| {
                        {
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [60u8] {
                                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                                    offset: at as u32,
                                });
                            }
                            *p = end;
                        }
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
                                        if input.len() < end || input[at..end] != [62u8] {
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
                                        <_ as crate::runtime::StructBuilder>::push_leaf_with_u64(
                                            builder,
                                            0u64,
                                        );
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
                                        if input.len() < end || input[at..end] != [61u8] {
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
                                        <_ as crate::runtime::StructBuilder>::push_leaf_with_u64(
                                            builder,
                                            1u64,
                                        );
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
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [60u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_branch_tag(4u32);
                    return ::core::result::Result::Ok(());
                }
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            61u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [61u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_branch_tag(3u32);
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
                    builder.push_branch_tag(2u32);
                    return ::core::result::Result::Ok(());
                }
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [62u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_branch_tag(5u32);
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
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_keyword_GoogleSheetsParser_unary_prefix<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        let _ = state;
        let _ = cursor;
        match first_byte {
            43u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [43u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_branch_tag(0u32);
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
                    builder.push_branch_tag(1u32);
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
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_keyword_GoogleSheetsParser_mul_op<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        let _ = state;
        let _ = cursor;
        match first_byte {
            42u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [42u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_branch_tag(0u32);
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
                    builder.push_branch_tag(1u32);
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
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_keyword_GoogleSheetsParser_add_op<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        let _ = state;
        let _ = cursor;
        match first_byte {
            43u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [43u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    builder.push_branch_tag(0u32);
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
                    builder.push_branch_tag(1u32);
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
    pub fn parse_flat_GoogleSheetsParser_cell<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(11u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        let __cell_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 11u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("cell"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __cell_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__cell_layout);
        <crate::runtime::google_sheets::SheetsStructBuilder<
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
                                let _ = __shape_support_GoogleSheetsParser::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_hregex_GoogleSheetsParser_sheet_prefix(
                                    input,
                                    p,
                                    state,
                                    builder,
                                    cursor,
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
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_hregex_GoogleSheetsParser_cell_ref(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __cell_handle,
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
    pub fn parse_flat_GoogleSheetsParser_func_open<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(12u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        let __func_open_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 12u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("func_open"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __func_open_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__func_open_layout,
        );
        <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_hregex_GoogleSheetsParser_identifier(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
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
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __func_open_handle,
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
    pub fn parse_flat_GoogleSheetsParser_range_ref<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(13u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        let __range_ref_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 13u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("range_ref"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __range_ref_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__range_ref_layout,
        );
        <crate::runtime::google_sheets::SheetsStructBuilder<
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
                                let _ = __shape_support_GoogleSheetsParser::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_hregex_GoogleSheetsParser_sheet_prefix(
                                    input,
                                    p,
                                    state,
                                    builder,
                                    cursor,
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
                'try_branches: loop {
                    {
                        let __alt_save_p = *p;
                        let __alt_builder_checkpoint = builder.checkpoint();
                        let __alt_result: ::core::result::Result<
                            (),
                            crate::runtime::DtaError,
                        > = (|| {
                            let _ = ({
                                let _ = __shape_support_GoogleSheetsParser::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_hregex_GoogleSheetsParser_cell_ref(
                                    input,
                                    p,
                                    state,
                                    builder,
                                    cursor,
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
                            let __alt_span_lo: usize = *p;
                            {
                                let __scan_start = *p;
                                let Some(match_len) = __regex_scan_GoogleSheetsParser(
                                    "\\$?[A-Za-z]{1,3}",
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
                                let __alt_span_hi: usize = *p;
                                let __alt_span_slice: &str = ::core::str::from_utf8(
                                        &input[__alt_span_lo..__alt_span_hi],
                                    )
                                    .unwrap_or("");
                                <_ as crate::runtime::StructBuilder>::push_leaf_with_str(
                                    builder,
                                    __alt_span_slice,
                                );
                            }
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
                            let __alt_span_lo: usize = *p;
                            {
                                let __scan_start = *p;
                                let Some(match_len) = __regex_scan_GoogleSheetsParser(
                                    "\\$?\\d+",
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
                                let __alt_span_hi: usize = *p;
                                let __alt_span_slice: &str = ::core::str::from_utf8(
                                        &input[__alt_span_lo..__alt_span_hi],
                                    )
                                    .unwrap_or("");
                                <_ as crate::runtime::StructBuilder>::push_leaf_with_str(
                                    builder,
                                    __alt_span_slice,
                                );
                            }
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
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [58u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
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
                            let _ = ({
                                let _ = __shape_support_GoogleSheetsParser::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_hregex_GoogleSheetsParser_cell_ref(
                                    input,
                                    p,
                                    state,
                                    builder,
                                    cursor,
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
                            let __alt_span_lo: usize = *p;
                            {
                                let __scan_start = *p;
                                let Some(match_len) = __regex_scan_GoogleSheetsParser(
                                    "\\$?[A-Za-z]{1,3}",
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
                                let __alt_span_hi: usize = *p;
                                let __alt_span_slice: &str = ::core::str::from_utf8(
                                        &input[__alt_span_lo..__alt_span_hi],
                                    )
                                    .unwrap_or("");
                                <_ as crate::runtime::StructBuilder>::push_leaf_with_str(
                                    builder,
                                    __alt_span_slice,
                                );
                            }
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
                            let __alt_span_lo: usize = *p;
                            {
                                let __scan_start = *p;
                                let Some(match_len) = __regex_scan_GoogleSheetsParser(
                                    "\\$?\\d+",
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
                                let __alt_span_hi: usize = *p;
                                let __alt_span_slice: &str = ::core::str::from_utf8(
                                        &input[__alt_span_lo..__alt_span_hi],
                                    )
                                    .unwrap_or("");
                                <_ as crate::runtime::StructBuilder>::push_leaf_with_str(
                                    builder,
                                    __alt_span_slice,
                                );
                            }
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
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __range_ref_handle,
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
    ///
    /// AZ-IV.W3.6 — Cursor-threaded. The Alt-dispatch branch
    /// selector consults `cursor.decide(rule_id)` so a
    /// `Decision::ParseUntil(idx)` returned by the path plan
    /// means the targeted variant index is preserved by the
    /// linear-try fallback (the byte-dispatch arms are still
    /// the prefilter; the cursor's decision is forwarded into
    /// the inner Refs as the descent proceeds).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_wrap_GoogleSheetsParser_cell_or_range<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let _ = cursor.decide(14u32 as u32);
        let __wrap_checkpoint = builder.checkpoint();
        let __wrap_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 14u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("cell_or_range"),
            kind: ::bbnf_ir::registry::LayoutKind::UntaggedEnum,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __wrap_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__wrap_layout);
        let mut __wrap_branch_idx: u32 = 0;
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let first = __shape_support_GoogleSheetsParser::skip_space(input, p, state)
                .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;
            'try_branches: loop {
                match first {
                    _ => {}
                }
                {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_flat_GoogleSheetsParser_range_ref(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                    match parse_flat_GoogleSheetsParser_cell(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_branch_tag(
                    builder,
                    __wrap_branch_idx,
                );
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __wrap_handle,
                );
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(e) => {
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::rollback(
                    builder,
                    __wrap_checkpoint,
                );
                ::core::result::Result::Err(e)
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
    pub fn parse_pratt_GoogleSheetsParser_comparison_expr<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        let _ = cursor;
        let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        let __comparison_expr_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 15u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("comparison_expr"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __comparison_expr_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__comparison_expr_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_pratt_GoogleSheetsParser_concat_expr(
                    input,
                    p,
                    state,
                    builder,
                    cursor,
                )
            })?;
            loop {
                let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let mut lut_byte: u8 = PRECEDENCE_LUT_comparison_expr[op_byte as usize];
                if lut_byte == 0 {
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    op_byte = input.get(*p).copied().unwrap_or(0);
                    lut_byte = PRECEDENCE_LUT_comparison_expr[op_byte as usize];
                }
                if lut_byte == 0 {
                    break;
                }
                let two_byte: u8 = (lut_byte >> 7) & 0x01u8;
                let second_byte: ::core::option::Option<u8> = input.get(*p + 1).copied();
                let (op_width, op_discriminant, op_matched) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    let mut matched: bool = false;
                    for e in PRECEDENCE_ENTRIES_comparison_expr.iter() {
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
                    for e in PRECEDENCE_ENTRIES_comparison_expr.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    if !matched_two_byte {
                        for e in PRECEDENCE_ENTRIES_comparison_expr.iter() {
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
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_branch_tag(
                    builder,
                    op_discriminant as u32,
                );
                *p = (*p).saturating_add(op_width as usize);
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_pratt_GoogleSheetsParser_concat_expr(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::end_compound(
            builder,
            __comparison_expr_handle,
        );
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
    pub fn parse_pratt_GoogleSheetsParser_mul_expr<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        let _ = cursor;
        let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        let __mul_expr_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 16u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("mul_expr"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __mul_expr_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__mul_expr_layout);
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_pratt_GoogleSheetsParser_exp_expr(input, p, state, builder, cursor)
            })?;
            loop {
                let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let mut lut_byte: u8 = PRECEDENCE_LUT_mul_expr[op_byte as usize];
                if lut_byte == 0 {
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    op_byte = input.get(*p).copied().unwrap_or(0);
                    lut_byte = PRECEDENCE_LUT_mul_expr[op_byte as usize];
                }
                if lut_byte == 0 {
                    break;
                }
                let two_byte: u8 = (lut_byte >> 7) & 0x01u8;
                let second_byte: ::core::option::Option<u8> = input.get(*p + 1).copied();
                let (op_width, op_discriminant, op_matched) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    let mut matched: bool = false;
                    for e in PRECEDENCE_ENTRIES_mul_expr.iter() {
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
                    for e in PRECEDENCE_ENTRIES_mul_expr.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    if !matched_two_byte {
                        for e in PRECEDENCE_ENTRIES_mul_expr.iter() {
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
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_branch_tag(
                    builder,
                    op_discriminant as u32,
                );
                *p = (*p).saturating_add(op_width as usize);
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_pratt_GoogleSheetsParser_exp_expr(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::end_compound(builder, __mul_expr_handle);
        __body_result?;
        ::core::result::Result::Ok(())
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
    pub fn parse_flat_GoogleSheetsParser_unary_expr<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(17u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        let __unary_expr_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 17u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("unary_expr"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __unary_expr_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__unary_expr_layout,
        );
        <crate::runtime::google_sheets::SheetsStructBuilder<
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
                            let _ = ({
                                let __first = __shape_support_GoogleSheetsParser::skip_space(
                                        input,
                                        p,
                                        state,
                                    )
                                    .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                                        offset: *p as u32,
                                    })?;
                                parse_keyword_GoogleSheetsParser_unary_prefix(
                                    input,
                                    p,
                                    __first,
                                    state,
                                    builder,
                                    cursor,
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
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_flat_GoogleSheetsParser_postfix_expr(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __unary_expr_handle,
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
    pub fn parse_flat_GoogleSheetsParser_paren_expr<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(18u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        let __paren_expr_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 18u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("paren_expr"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __paren_expr_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__paren_expr_layout,
        );
        <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
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
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_scalar_GoogleSheetsParser_expression(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
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
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __paren_expr_handle,
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
    pub fn parse_flat_GoogleSheetsParser_arg<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(19u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        let __arg_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 19u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("arg"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __arg_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__arg_layout);
        <crate::runtime::google_sheets::SheetsStructBuilder<
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
                                let _ = __shape_support_GoogleSheetsParser::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_scalar_GoogleSheetsParser_expression(
                                    input,
                                    p,
                                    state,
                                    builder,
                                    cursor,
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
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(builder, __arg_handle);
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
    pub fn parse_flat_GoogleSheetsParser_func_args<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(20u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        let __func_args_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 20u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("func_args"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __func_args_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__func_args_layout,
        );
        <crate::runtime::google_sheets::SheetsStructBuilder<
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
                            let _ = ({
                                let _ = __shape_support_GoogleSheetsParser::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_flat_GoogleSheetsParser_arg(
                                    input,
                                    p,
                                    state,
                                    builder,
                                    cursor,
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
                                        let _ = __shape_support_GoogleSheetsParser::skip_space(
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
                                        let _ = __shape_support_GoogleSheetsParser::skip_space(
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
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __func_args_handle,
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
    pub fn parse_flat_GoogleSheetsParser_let_binding<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(21u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        let __let_binding_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 21u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("let_binding"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __let_binding_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__let_binding_layout,
        );
        <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_scalar_GoogleSheetsParser_expression(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
            {
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                let at = *p;
                let end = at + 1usize;
                if input.len() < end || input[at..end] != [44u8] {
                    return ::core::result::Result::Err(crate::runtime::DtaError::Syntax {
                        offset: at as u32,
                    });
                }
                *p = end;
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            }
            {
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_scalar_GoogleSheetsParser_expression(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __let_binding_handle,
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
    pub fn parse_flat_GoogleSheetsParser_lambda_params<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(22u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        let __lambda_params_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 22u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("lambda_params"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __lambda_params_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__lambda_params_layout,
        );
        <crate::runtime::google_sheets::SheetsStructBuilder<
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
                            let _ = ({
                                let _ = __shape_support_GoogleSheetsParser::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_scalar_GoogleSheetsParser_expression(
                                    input,
                                    p,
                                    state,
                                    builder,
                                    cursor,
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
                                        let _ = __shape_support_GoogleSheetsParser::skip_space(
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
                                        let _ = __shape_support_GoogleSheetsParser::skip_space(
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
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __lambda_params_handle,
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
    pub fn parse_pratt_GoogleSheetsParser_array_row<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        let _ = cursor;
        let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        let __array_row_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 23u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("array_row"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __array_row_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__array_row_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_scalar_GoogleSheetsParser_expression(
                    input,
                    p,
                    state,
                    builder,
                    cursor,
                )
            })?;
            loop {
                let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let mut lut_byte: u8 = PRECEDENCE_LUT_array_row[op_byte as usize];
                if lut_byte == 0 {
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    op_byte = input.get(*p).copied().unwrap_or(0);
                    lut_byte = PRECEDENCE_LUT_array_row[op_byte as usize];
                }
                if lut_byte == 0 {
                    break;
                }
                let two_byte: u8 = (lut_byte >> 7) & 0x01u8;
                let second_byte: ::core::option::Option<u8> = input.get(*p + 1).copied();
                let (op_width, op_discriminant, op_matched) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    let mut matched: bool = false;
                    for e in PRECEDENCE_ENTRIES_array_row.iter() {
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
                    for e in PRECEDENCE_ENTRIES_array_row.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    if !matched_two_byte {
                        for e in PRECEDENCE_ENTRIES_array_row.iter() {
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
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_branch_tag(
                    builder,
                    op_discriminant as u32,
                );
                *p = (*p).saturating_add(op_width as usize);
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_scalar_GoogleSheetsParser_expression(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::end_compound(builder, __array_row_handle);
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
    pub fn parse_pratt_GoogleSheetsParser_array_rows<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        let _ = cursor;
        let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        let __array_rows_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 24u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("array_rows"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __array_rows_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__array_rows_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_pratt_GoogleSheetsParser_array_row(
                    input,
                    p,
                    state,
                    builder,
                    cursor,
                )
            })?;
            loop {
                let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let mut lut_byte: u8 = PRECEDENCE_LUT_array_rows[op_byte as usize];
                if lut_byte == 0 {
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    op_byte = input.get(*p).copied().unwrap_or(0);
                    lut_byte = PRECEDENCE_LUT_array_rows[op_byte as usize];
                }
                if lut_byte == 0 {
                    break;
                }
                let two_byte: u8 = (lut_byte >> 7) & 0x01u8;
                let second_byte: ::core::option::Option<u8> = input.get(*p + 1).copied();
                let (op_width, op_discriminant, op_matched) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    let mut matched: bool = false;
                    for e in PRECEDENCE_ENTRIES_array_rows.iter() {
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
                    for e in PRECEDENCE_ENTRIES_array_rows.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    if !matched_two_byte {
                        for e in PRECEDENCE_ENTRIES_array_rows.iter() {
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
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_branch_tag(
                    builder,
                    op_discriminant as u32,
                );
                *p = (*p).saturating_add(op_width as usize);
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_pratt_GoogleSheetsParser_array_row(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::end_compound(builder, __array_rows_handle);
        __body_result?;
        ::core::result::Result::Ok(())
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
    pub fn parse_flat_GoogleSheetsParser_array_literal<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(25u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        let __array_literal_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 25u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("array_literal"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __array_literal_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__array_literal_layout,
        );
        <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
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
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_pratt_GoogleSheetsParser_array_rows(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
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
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __array_literal_handle,
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
    pub fn parse_pratt_GoogleSheetsParser_concat_expr<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        let _ = cursor;
        let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        let __concat_expr_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 26u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("concat_expr"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __concat_expr_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__concat_expr_layout,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_pratt_GoogleSheetsParser_add_expr(input, p, state, builder, cursor)
            })?;
            loop {
                let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let mut lut_byte: u8 = PRECEDENCE_LUT_concat_expr[op_byte as usize];
                if lut_byte == 0 {
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    op_byte = input.get(*p).copied().unwrap_or(0);
                    lut_byte = PRECEDENCE_LUT_concat_expr[op_byte as usize];
                }
                if lut_byte == 0 {
                    break;
                }
                let two_byte: u8 = (lut_byte >> 7) & 0x01u8;
                let second_byte: ::core::option::Option<u8> = input.get(*p + 1).copied();
                let (op_width, op_discriminant, op_matched) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    let mut matched: bool = false;
                    for e in PRECEDENCE_ENTRIES_concat_expr.iter() {
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
                    for e in PRECEDENCE_ENTRIES_concat_expr.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    if !matched_two_byte {
                        for e in PRECEDENCE_ENTRIES_concat_expr.iter() {
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
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_branch_tag(
                    builder,
                    op_discriminant as u32,
                );
                *p = (*p).saturating_add(op_width as usize);
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_pratt_GoogleSheetsParser_add_expr(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::end_compound(builder, __concat_expr_handle);
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
    pub fn parse_pratt_GoogleSheetsParser_add_expr<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        let _ = cursor;
        let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        let __add_expr_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 27u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("add_expr"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __add_expr_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__add_expr_layout);
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_pratt_GoogleSheetsParser_mul_expr(input, p, state, builder, cursor)
            })?;
            loop {
                let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let mut lut_byte: u8 = PRECEDENCE_LUT_add_expr[op_byte as usize];
                if lut_byte == 0 {
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    op_byte = input.get(*p).copied().unwrap_or(0);
                    lut_byte = PRECEDENCE_LUT_add_expr[op_byte as usize];
                }
                if lut_byte == 0 {
                    break;
                }
                let two_byte: u8 = (lut_byte >> 7) & 0x01u8;
                let second_byte: ::core::option::Option<u8> = input.get(*p + 1).copied();
                let (op_width, op_discriminant, op_matched) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    let mut matched: bool = false;
                    for e in PRECEDENCE_ENTRIES_add_expr.iter() {
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
                    for e in PRECEDENCE_ENTRIES_add_expr.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    if !matched_two_byte {
                        for e in PRECEDENCE_ENTRIES_add_expr.iter() {
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
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_branch_tag(
                    builder,
                    op_discriminant as u32,
                );
                *p = (*p).saturating_add(op_width as usize);
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_pratt_GoogleSheetsParser_mul_expr(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::end_compound(builder, __add_expr_handle);
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
    pub fn parse_pratt_GoogleSheetsParser_exp_expr<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        let _ = cursor;
        let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        let __exp_expr_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 28u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("exp_expr"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __exp_expr_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__exp_expr_layout);
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_flat_GoogleSheetsParser_unary_expr(
                    input,
                    p,
                    state,
                    builder,
                    cursor,
                )
            })?;
            loop {
                let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let mut lut_byte: u8 = PRECEDENCE_LUT_exp_expr[op_byte as usize];
                if lut_byte == 0 {
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    op_byte = input.get(*p).copied().unwrap_or(0);
                    lut_byte = PRECEDENCE_LUT_exp_expr[op_byte as usize];
                }
                if lut_byte == 0 {
                    break;
                }
                let two_byte: u8 = (lut_byte >> 7) & 0x01u8;
                let second_byte: ::core::option::Option<u8> = input.get(*p + 1).copied();
                let (op_width, op_discriminant, op_matched) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    let mut matched: bool = false;
                    for e in PRECEDENCE_ENTRIES_exp_expr.iter() {
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
                    for e in PRECEDENCE_ENTRIES_exp_expr.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    if !matched_two_byte {
                        for e in PRECEDENCE_ENTRIES_exp_expr.iter() {
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
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::push_branch_tag(
                    builder,
                    op_discriminant as u32,
                );
                *p = (*p).saturating_add(op_width as usize);
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_flat_GoogleSheetsParser_unary_expr(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::end_compound(builder, __exp_expr_handle);
        __body_result?;
        ::core::result::Result::Ok(())
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
    pub fn parse_arglist_GoogleSheetsParser_lambda_call<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        let _ = cursor;
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 29u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("lambda_call"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __arglist_checkpoint = <crate::runtime::google_sheets::SheetsStructBuilder<
            'p,
        > as crate::runtime::StructBuilder>::checkpoint(builder);
        let __handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            'p,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__layout);
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let _ = parse_GoogleSheetsParser_formula__value(
                input,
                p,
                state,
                builder,
                cursor,
            )?;
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_flat_GoogleSheetsParser_lambda_params(
                    input,
                    p,
                    state,
                    builder,
                    cursor,
                )
            })?;
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
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
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    'p,
                > as crate::runtime::StructBuilder>::end_compound(builder, __handle);
                Ok(())
            }
            ::core::result::Result::Err(__err) => {
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    'p,
                > as crate::runtime::StructBuilder>::rollback(
                    builder,
                    __arglist_checkpoint,
                );
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
    pub fn parse_scalar_GoogleSheetsParser_expression<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        {
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            parse_pratt_GoogleSheetsParser_comparison_expr(
                input,
                p,
                state,
                builder,
                cursor,
            )
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
    pub fn parse_arglist_GoogleSheetsParser_func_call<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        let _ = cursor;
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 31u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("func_call"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __arglist_checkpoint = <crate::runtime::google_sheets::SheetsStructBuilder<
            'p,
        > as crate::runtime::StructBuilder>::checkpoint(builder);
        let __handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            'p,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__layout);
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_flat_GoogleSheetsParser_func_open(input, p, state, builder, cursor)
            })?;
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            loop {
                let __save = *p;
                let __res: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
                    let _ = ({
                        let _ = __shape_support_GoogleSheetsParser::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_flat_GoogleSheetsParser_func_args(
                            input,
                            p,
                            state,
                            builder,
                            cursor,
                        )
                    })?;
                    Ok(())
                })();
                if __res.is_err() {
                    *p = __save;
                    break;
                }
            }
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
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
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    'p,
                > as crate::runtime::StructBuilder>::end_compound(builder, __handle);
                Ok(())
            }
            ::core::result::Result::Err(__err) => {
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    'p,
                > as crate::runtime::StructBuilder>::rollback(
                    builder,
                    __arglist_checkpoint,
                );
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
    pub fn parse_flat_GoogleSheetsParser_let_args<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(32u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        let __let_args_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 32u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("let_args"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __let_args_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__let_args_layout);
        <crate::runtime::google_sheets::SheetsStructBuilder<
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
                            let _ = ({
                                let _ = __shape_support_GoogleSheetsParser::skip_space(
                                    input,
                                    p,
                                    state,
                                );
                                parse_flat_GoogleSheetsParser_let_binding(
                                    input,
                                    p,
                                    state,
                                    builder,
                                    cursor,
                                )
                            })?;
                            let _ = __shape_support_GoogleSheetsParser::skip_space(
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
                            let _ = __shape_support_GoogleSheetsParser::skip_space(
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
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_scalar_GoogleSheetsParser_expression(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __let_args_handle,
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
    pub fn parse_arglist_GoogleSheetsParser_let_call<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        let _ = cursor;
        let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 33u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("let_call"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __arglist_checkpoint = <crate::runtime::google_sheets::SheetsStructBuilder<
            'p,
        > as crate::runtime::StructBuilder>::checkpoint(builder);
        let __handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            'p,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__layout);
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            let _ = parse_GoogleSheetsParser_formula__value(
                input,
                p,
                state,
                builder,
                cursor,
            )?;
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_flat_GoogleSheetsParser_let_args(input, p, state, builder, cursor)
            })?;
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
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
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    'p,
                > as crate::runtime::StructBuilder>::end_compound(builder, __handle);
                Ok(())
            }
            ::core::result::Result::Err(__err) => {
                <crate::runtime::google_sheets::SheetsStructBuilder<
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
    ///
    /// AZ-IV.W3.6 — Cursor-threaded. The Alt-dispatch branch
    /// selector consults `cursor.decide(rule_id)` so a
    /// `Decision::ParseUntil(idx)` returned by the path plan
    /// means the targeted variant index is preserved by the
    /// linear-try fallback (the byte-dispatch arms are still
    /// the prefilter; the cursor's decision is forwarded into
    /// the inner Refs as the descent proceeds).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_wrap_GoogleSheetsParser_primary<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let _ = cursor.decide(34u32 as u32);
        let first = __shape_support_GoogleSheetsParser::skip_space(input, p, state)
            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        'try_branches: loop {
            match first {
                34u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_string_GoogleSheetsParser_string(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                35u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_flat_GoogleSheetsParser_error_literal(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                40u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_flat_GoogleSheetsParser_paren_expr(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                46u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_hregex_GoogleSheetsParser_number(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                48u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_hregex_GoogleSheetsParser_number(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                49u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_hregex_GoogleSheetsParser_number(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                50u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_hregex_GoogleSheetsParser_number(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                51u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_hregex_GoogleSheetsParser_number(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                52u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_hregex_GoogleSheetsParser_number(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                53u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_hregex_GoogleSheetsParser_number(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                54u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_hregex_GoogleSheetsParser_number(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                55u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_hregex_GoogleSheetsParser_number(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                56u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_hregex_GoogleSheetsParser_number(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                57u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_hregex_GoogleSheetsParser_number(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                70u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_wrap_GoogleSheetsParser_boolean(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                76u8 => {
                    {
                        let attempt_p = *p;
                        let attempt_builder = builder.checkpoint();
                        match parse_arglist_GoogleSheetsParser_let_call(
                            input,
                            p,
                            state,
                            builder,
                            cursor,
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
                    {
                        let attempt_p = *p;
                        let attempt_builder = builder.checkpoint();
                        match parse_arglist_GoogleSheetsParser_lambda_call(
                            input,
                            p,
                            state,
                            builder,
                            cursor,
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
                }
                84u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_wrap_GoogleSheetsParser_boolean(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                102u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_wrap_GoogleSheetsParser_boolean(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                108u8 => {
                    {
                        let attempt_p = *p;
                        let attempt_builder = builder.checkpoint();
                        match parse_arglist_GoogleSheetsParser_let_call(
                            input,
                            p,
                            state,
                            builder,
                            cursor,
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
                    {
                        let attempt_p = *p;
                        let attempt_builder = builder.checkpoint();
                        match parse_arglist_GoogleSheetsParser_lambda_call(
                            input,
                            p,
                            state,
                            builder,
                            cursor,
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
                }
                116u8 => {
                    let attempt_p = *p;
                    let attempt_builder = builder.checkpoint();
                    match parse_wrap_GoogleSheetsParser_boolean(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                    match parse_flat_GoogleSheetsParser_array_literal(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
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
                match parse_arglist_GoogleSheetsParser_func_call(
                    input,
                    p,
                    state,
                    builder,
                    cursor,
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
            {
                let attempt_p = *p;
                let attempt_builder = builder.checkpoint();
                match parse_wrap_GoogleSheetsParser_cell_or_range(
                    input,
                    p,
                    state,
                    builder,
                    cursor,
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
            {
                let attempt_p = *p;
                let attempt_builder = builder.checkpoint();
                match parse_hregex_GoogleSheetsParser_identifier(
                    input,
                    p,
                    state,
                    builder,
                    cursor,
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
    pub fn parse_flat_GoogleSheetsParser_postfix_expr<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(35u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        let __postfix_expr_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 35u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("postfix_expr"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __postfix_expr_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(
            builder,
            &__postfix_expr_layout,
        );
        <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_wrap_GoogleSheetsParser_primary(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
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
                            let at = *p;
                            let end = at + 1usize;
                            if input.len() < end || input[at..end] != [37u8] {
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
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __postfix_expr_handle,
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
    pub fn parse_flat_GoogleSheetsParser_formula<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        use crate::runtime::builder::StructBuilder as _;
        use crate::path::cursor::Decision as __Decision;
        let __decision: __Decision = cursor.decide(36u32 as u32);
        let __flat_checkpoint = builder.checkpoint();
        let __compound_start: u32 = *p as u32;
        let __formula_layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
            rule_id: 36u32 as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from("formula"),
            kind: ::bbnf_ir::registry::LayoutKind::Struct,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        };
        let __formula_handle = <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::begin_compound(builder, &__formula_layout);
        <crate::runtime::google_sheets::SheetsStructBuilder<
            '_,
        > as crate::runtime::StructBuilder>::record_compound_bounds_start(
            builder,
            __compound_start,
        );
        let __body_result: ::core::result::Result<(), crate::runtime::DtaError> = (|| {
            {
                {
                    let __scan_start = *p;
                    let Some(match_len) = __regex_scan_GoogleSheetsParser(
                        "=?",
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
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_scalar_GoogleSheetsParser_expression(
                        input,
                        p,
                        state,
                        builder,
                        cursor,
                    )
                })?;
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::record_compound_bounds_end(
                    builder,
                    *p as u32,
                );
                <crate::runtime::google_sheets::SheetsStructBuilder<
                    '_,
                > as crate::runtime::StructBuilder>::end_compound(
                    builder,
                    __formula_handle,
                );
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
    pub fn parse_GoogleSheetsParser_formula<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        parse_GoogleSheetsParser_formula__value(input, p, state, builder, cursor)
    }
    /// AW-V.W3.2 — value-position shape dispatcher. Called both at
    /// the grammar root and from Object / Array compound bodies.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_GoogleSheetsParser_formula__value<'p, __P>(
        input: &'p [u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut crate::runtime::google_sheets::SheetsStructBuilder<'p>,
        cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
    ) -> ::core::result::Result<(), crate::runtime::DtaError>
    where
        __P: for<'__c> crate::path::schema::PathSchema<'__c>,
    {
        let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        let _ = cursor.decide(36u32);
        parse_flat_GoogleSheetsParser_formula(input, p, state, builder, cursor)
    }
    impl GoogleSheetsParser {
        fn __number_prettify<'a>(
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
                                let __save_dispatch = state.offset;
                                let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                match __dispatch_b {
                                    b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' | b'8'
                                    | b'9' => {
                                        let __ok = (|| -> Option<()> {
                                            {
                                                if ::parse_that::scan_digits_mut(state).is_none() {
                                                    return None;
                                                }
                                            }
                                            {
                                                let __save = state.offset;
                                                let __ok = (|| -> Option<()> {
                                                    if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                                    {
                                                        return None;
                                                    }
                                                    state.offset += 1;
                                                    Some(())
                                                })();
                                                if __ok.is_none() {
                                                    state.offset = __save;
                                                }
                                            }
                                            {
                                                let _ = ::parse_that::scan_digits_star_mut(state);
                                            }
                                            Some(())
                                        })();
                                        if __ok.is_none() {
                                            state.offset = __save_dispatch;
                                            return None;
                                        }
                                    }
                                    b'.' => {
                                        let __ok = (|| -> Option<()> {
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
                                            Some(())
                                        })();
                                        if __ok.is_none() {
                                            state.offset = __save_dispatch;
                                            return None;
                                        }
                                    }
                                    _ => {
                                        return None;
                                    }
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
                    if ::parse_that::scan_string_quoted(state).is_none() {
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
        fn __boolean_prettify<'a>(
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
                        b'T' | b't' => {
                            {
                                let __start = state.offset;
                                if {
                                    let __start = state.offset;
                                    let __result: Option<()> = (|| {
                                        {
                                            let __b = *state.src_bytes.get(state.offset)?;
                                            if !((__b == b'T' || __b == b't')) {
                                                return None;
                                            }
                                            state.offset += 1;
                                        }
                                        {
                                            let __b = *state.src_bytes.get(state.offset)?;
                                            if !((__b == b'R' || __b == b'r')) {
                                                return None;
                                            }
                                            state.offset += 1;
                                        }
                                        {
                                            let __b = *state.src_bytes.get(state.offset)?;
                                            if !((__b == b'U' || __b == b'u')) {
                                                return None;
                                            }
                                            state.offset += 1;
                                        }
                                        {
                                            let __b = *state.src_bytes.get(state.offset)?;
                                            if !((__b == b'E' || __b == b'e')) {
                                                return None;
                                            }
                                            state.offset += 1;
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
                        }
                        b'F' | b'f' => {
                            {
                                let __start = state.offset;
                                if {
                                    let __start = state.offset;
                                    let __result: Option<()> = (|| {
                                        {
                                            let __b = *state.src_bytes.get(state.offset)?;
                                            if !((__b == b'F' || __b == b'f')) {
                                                return None;
                                            }
                                            state.offset += 1;
                                        }
                                        {
                                            let __b = *state.src_bytes.get(state.offset)?;
                                            if !((__b == b'A' || __b == b'a')) {
                                                return None;
                                            }
                                            state.offset += 1;
                                        }
                                        {
                                            let __b = *state.src_bytes.get(state.offset)?;
                                            if !((__b == b'L' || __b == b'l')) {
                                                return None;
                                            }
                                            state.offset += 1;
                                        }
                                        {
                                            let __b = *state.src_bytes.get(state.offset)?;
                                            if !((__b == b'S' || __b == b's')) {
                                                return None;
                                            }
                                            state.offset += 1;
                                        }
                                        {
                                            let __b = *state.src_bytes.get(state.offset)?;
                                            if !((__b == b'E' || __b == b'e')) {
                                                return None;
                                            }
                                            state.offset += 1;
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
                        }
                        _ => {
                            return false;
                        }
                    }
                };
                true
            }
        }
        pub fn boolean_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__boolean_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __error_literal_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'#') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'#');
                    };
                    {
                        if !{
                            let __pretty_cp10 = state.offset;
                            let __ok = (|| -> bool {
                                {
                                    let __s = "N/A";
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
                                state.offset = __pretty_cp10;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp8 = state.offset;
                                    let __pretty_bcp9 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'N')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b'N');
                                            };
                                            {
                                                if !{
                                                    let __pretty_cp2 = state.offset;
                                                    let __ok = (|| -> bool {
                                                        {
                                                            let __s = "ULL!";
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
                                                        state.offset = __pretty_cp2;
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp1 = state.offset;
                                                            let __ok = (|| -> bool {
                                                                {
                                                                    let __s = "UM!";
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
                                                                state.offset = __pretty_cp1;
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if !{
                                                                    let __pretty_cp0 = state.offset;
                                                                    let __ok = (|| -> bool {
                                                                        {
                                                                            let __s = "AME?";
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
                                                                    return false;
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            };
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp8;
                                        __builder.restore(__pretty_bcp9);
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp7 = state.offset;
                                            let __ok = (|| -> bool {
                                                {
                                                    let __s = "VALUE!";
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
                                                            let __s = "DIV/0!";
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
                                                                    let __s = "ERROR!";
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
                                                                true
                                                            })();
                                                            if !__ok {
                                                                state.offset = __pretty_cp5;
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if !{
                                                                    let __pretty_cp4 = state.offset;
                                                                    let __ok = (|| -> bool {
                                                                        {
                                                                            let __s = "SPILL!";
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
                                                                        true
                                                                    })();
                                                                    if !__ok {
                                                                        state.offset = __pretty_cp4;
                                                                    }
                                                                    __ok
                                                                } {
                                                                    {
                                                                        if !{
                                                                            let __pretty_cp3 = state.offset;
                                                                            let __ok = (|| -> bool {
                                                                                {
                                                                                    let __s = "REF!";
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
                                                                                state.offset = __pretty_cp3;
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
                };
                true
            }
        }
        pub fn error_literal_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__error_literal_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __sheet_prefix_prettify<'a>(
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
                                let __save_dispatch = state.offset;
                                let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                match __dispatch_b {
                                    b'\'' => {
                                        let __ok = (|| -> Option<()> {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                            {
                                                return None;
                                            }
                                            state.offset += 1;
                                            {
                                                let mut __rep_count: u32 = 0;
                                                loop {
                                                    let __save = state.offset;
                                                    let __ok = (|| -> Option<()> {
                                                        {
                                                            let __save_alt = state.offset;
                                                            let __alt_ok = (|| -> Option<()> {
                                                                {
                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                    if !(!(__b == b'\'')) {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                }
                                                                Some(())
                                                            })();
                                                            let __alt_ok = if __alt_ok.is_none() {
                                                                state.offset = __save_alt;
                                                                (|| -> Option<()> {
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                                                    {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                                                    {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
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
                                            if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                            {
                                                return None;
                                            }
                                            state.offset += 1;
                                            if state.src_bytes.get(state.offset).copied() != Some(b'!')
                                            {
                                                return None;
                                            }
                                            state.offset += 1;
                                            Some(())
                                        })();
                                        if __ok.is_none() {
                                            state.offset = __save_dispatch;
                                            return None;
                                        }
                                    }
                                    b'A' | b'B' | b'C' | b'D' | b'E' | b'F' | b'G' | b'H' | b'I'
                                    | b'J' | b'K' | b'L' | b'M' | b'N' | b'O' | b'P' | b'Q'
                                    | b'R' | b'S' | b'T' | b'U' | b'V' | b'W' | b'X' | b'Y'
                                    | b'Z' | b'_' | b'a' | b'b' | b'c' | b'd' | b'e' | b'f'
                                    | b'g' | b'h' | b'i' | b'j' | b'k' | b'l' | b'm' | b'n'
                                    | b'o' | b'p' | b'q' | b'r' | b's' | b't' | b'u' | b'v'
                                    | b'w' | b'x' | b'y' | b'z' => {
                                        let __ok = (|| -> Option<()> {
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
                                                    if (__b.is_ascii_alphanumeric() || __b == b'_') {
                                                        __pos += 1;
                                                    } else {
                                                        break;
                                                    }
                                                }
                                                state.offset = __pos;
                                            }
                                            if state.src_bytes.get(state.offset).copied() != Some(b'!')
                                            {
                                                return None;
                                            }
                                            state.offset += 1;
                                            Some(())
                                        })();
                                        if __ok.is_none() {
                                            state.offset = __save_dispatch;
                                            return None;
                                        }
                                    }
                                    _ => {
                                        return None;
                                    }
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
        pub fn sheet_prefix_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__sheet_prefix_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __cell_ref_prettify<'a>(
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
                                let __save = state.offset;
                                let __ok = (|| -> Option<()> {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'$')
                                    {
                                        return None;
                                    }
                                    state.offset += 1;
                                    Some(())
                                })();
                                if __ok.is_none() {
                                    state.offset = __save;
                                }
                            }
                            {
                                let __end = state.src_bytes.len();
                                let mut __pos = state.offset;
                                let mut __count: u32 = 0;
                                while __pos < __end && __count < 3 {
                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                    if __b.is_ascii_alphabetic() {
                                        __pos += 1;
                                        __count += 1;
                                    } else {
                                        break;
                                    }
                                }
                                if __count < 1 {
                                    return None;
                                }
                                state.offset = __pos;
                            }
                            {
                                let __save = state.offset;
                                let __ok = (|| -> Option<()> {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'$')
                                    {
                                        return None;
                                    }
                                    state.offset += 1;
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
        pub fn cell_ref_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__cell_ref_prettify(state, &mut __builder) {
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
        fn __compare_op_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp15 = state.offset;
                        let __pretty_bcp16 = __builder.checkpoint();
                        let __ok = (|| -> bool {
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
                                    let __byte = match state.src_bytes.get(state.offset) {
                                        Some(&b) => b,
                                        None => return false,
                                    };
                                    match __byte {
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
                                        _ => {
                                            return false;
                                        }
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
                        {
                            if !{
                                let __pretty_cp14 = state.offset;
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
                                    state.offset = __pretty_cp14;
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp13 = state.offset;
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
                                            state.offset = __pretty_cp13;
                                        }
                                        __ok
                                    } {
                                        {
                                            if !{
                                                let __pretty_cp12 = state.offset;
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
                                                    state.offset = __pretty_cp12;
                                                }
                                                __ok
                                            } {
                                                {
                                                    if !{
                                                        let __pretty_cp11 = state.offset;
                                                        let __ok = (|| -> bool {
                                                            {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'=')
                                                                {
                                                                    return false;
                                                                }
                                                                state.offset += 1;
                                                                __builder.char(b'=');
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
                };
                true
            }
        }
        pub fn compare_op_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__compare_op_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __unary_prefix_prettify<'a>(
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
                                if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'-');
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
        pub fn unary_prefix_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__unary_prefix_prettify(state, &mut __builder) {
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
                    let __byte = match state.src_bytes.get(state.offset) {
                        Some(&b) => b,
                        None => return false,
                    };
                    match __byte {
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
                        b'/' => {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'/');
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
                    let __byte = match state.src_bytes.get(state.offset) {
                        Some(&b) => b,
                        None => return false,
                    };
                    match __byte {
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
                                if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'-');
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
        fn __cell_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let _ = {
                            let __pretty_cp17 = state.offset;
                            let __pretty_bcp18 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __start = state.offset;
                                    if {
                                        let __start = state.offset;
                                        let __result: Option<()> = (|| {
                                            {
                                                let __save_dispatch = state.offset;
                                                let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                match __dispatch_b {
                                                    b'\'' => {
                                                        let __ok = (|| -> Option<()> {
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            {
                                                                let mut __rep_count: u32 = 0;
                                                                loop {
                                                                    let __save = state.offset;
                                                                    let __ok = (|| -> Option<()> {
                                                                        {
                                                                            let __save_alt = state.offset;
                                                                            let __alt_ok = (|| -> Option<()> {
                                                                                {
                                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                                    if !(!(__b == b'\'')) {
                                                                                        return None;
                                                                                    }
                                                                                    state.offset += 1;
                                                                                }
                                                                                Some(())
                                                                            })();
                                                                            let __alt_ok = if __alt_ok.is_none() {
                                                                                state.offset = __save_alt;
                                                                                (|| -> Option<()> {
                                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                                                                    {
                                                                                        return None;
                                                                                    }
                                                                                    state.offset += 1;
                                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                                                                    {
                                                                                        return None;
                                                                                    }
                                                                                    state.offset += 1;
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
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'!')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            Some(())
                                                        })();
                                                        if __ok.is_none() {
                                                            state.offset = __save_dispatch;
                                                            return None;
                                                        }
                                                    }
                                                    b'A' | b'B' | b'C' | b'D' | b'E' | b'F' | b'G' | b'H' | b'I'
                                                    | b'J' | b'K' | b'L' | b'M' | b'N' | b'O' | b'P' | b'Q'
                                                    | b'R' | b'S' | b'T' | b'U' | b'V' | b'W' | b'X' | b'Y'
                                                    | b'Z' | b'_' | b'a' | b'b' | b'c' | b'd' | b'e' | b'f'
                                                    | b'g' | b'h' | b'i' | b'j' | b'k' | b'l' | b'm' | b'n'
                                                    | b'o' | b'p' | b'q' | b'r' | b's' | b't' | b'u' | b'v'
                                                    | b'w' | b'x' | b'y' | b'z' => {
                                                        let __ok = (|| -> Option<()> {
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
                                                                    if (__b.is_ascii_alphanumeric() || __b == b'_') {
                                                                        __pos += 1;
                                                                    } else {
                                                                        break;
                                                                    }
                                                                }
                                                                state.offset = __pos;
                                                            }
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'!')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            Some(())
                                                        })();
                                                        if __ok.is_none() {
                                                            state.offset = __save_dispatch;
                                                            return None;
                                                        }
                                                    }
                                                    _ => {
                                                        return None;
                                                    }
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
                                state.offset = __pretty_cp17;
                                __builder.restore(__pretty_bcp18);
                            }
                            __ok
                        };
                        true
                    };
                    {
                        let __start = state.offset;
                        if {
                            let __start = state.offset;
                            let __result: Option<()> = (|| {
                                {
                                    let __save = state.offset;
                                    let __ok = (|| -> Option<()> {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'$')
                                        {
                                            return None;
                                        }
                                        state.offset += 1;
                                        Some(())
                                    })();
                                    if __ok.is_none() {
                                        state.offset = __save;
                                    }
                                }
                                {
                                    let __end = state.src_bytes.len();
                                    let mut __pos = state.offset;
                                    let mut __count: u32 = 0;
                                    while __pos < __end && __count < 3 {
                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                        if __b.is_ascii_alphabetic() {
                                            __pos += 1;
                                            __count += 1;
                                        } else {
                                            break;
                                        }
                                    }
                                    if __count < 1 {
                                        return None;
                                    }
                                    state.offset = __pos;
                                }
                                {
                                    let __save = state.offset;
                                    let __ok = (|| -> Option<()> {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'$')
                                        {
                                            return None;
                                        }
                                        state.offset += 1;
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
                };
                true
            }
        }
        pub fn cell_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__cell_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __func_open_prettify<'a>(
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
                        if state.src_bytes.get(state.offset).copied() != Some(b'(') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'(');
                    };
                };
                true
            }
        }
        pub fn func_open_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__func_open_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __range_ref_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let _ = {
                            let __pretty_cp19 = state.offset;
                            let __pretty_bcp20 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __start = state.offset;
                                    if {
                                        let __start = state.offset;
                                        let __result: Option<()> = (|| {
                                            {
                                                let __save_dispatch = state.offset;
                                                let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                match __dispatch_b {
                                                    b'\'' => {
                                                        let __ok = (|| -> Option<()> {
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            {
                                                                let mut __rep_count: u32 = 0;
                                                                loop {
                                                                    let __save = state.offset;
                                                                    let __ok = (|| -> Option<()> {
                                                                        {
                                                                            let __save_alt = state.offset;
                                                                            let __alt_ok = (|| -> Option<()> {
                                                                                {
                                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                                    if !(!(__b == b'\'')) {
                                                                                        return None;
                                                                                    }
                                                                                    state.offset += 1;
                                                                                }
                                                                                Some(())
                                                                            })();
                                                                            let __alt_ok = if __alt_ok.is_none() {
                                                                                state.offset = __save_alt;
                                                                                (|| -> Option<()> {
                                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                                                                    {
                                                                                        return None;
                                                                                    }
                                                                                    state.offset += 1;
                                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                                                                    {
                                                                                        return None;
                                                                                    }
                                                                                    state.offset += 1;
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
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'!')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            Some(())
                                                        })();
                                                        if __ok.is_none() {
                                                            state.offset = __save_dispatch;
                                                            return None;
                                                        }
                                                    }
                                                    b'A' | b'B' | b'C' | b'D' | b'E' | b'F' | b'G' | b'H' | b'I'
                                                    | b'J' | b'K' | b'L' | b'M' | b'N' | b'O' | b'P' | b'Q'
                                                    | b'R' | b'S' | b'T' | b'U' | b'V' | b'W' | b'X' | b'Y'
                                                    | b'Z' | b'_' | b'a' | b'b' | b'c' | b'd' | b'e' | b'f'
                                                    | b'g' | b'h' | b'i' | b'j' | b'k' | b'l' | b'm' | b'n'
                                                    | b'o' | b'p' | b'q' | b'r' | b's' | b't' | b'u' | b'v'
                                                    | b'w' | b'x' | b'y' | b'z' => {
                                                        let __ok = (|| -> Option<()> {
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
                                                                    if (__b.is_ascii_alphanumeric() || __b == b'_') {
                                                                        __pos += 1;
                                                                    } else {
                                                                        break;
                                                                    }
                                                                }
                                                                state.offset = __pos;
                                                            }
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'!')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            Some(())
                                                        })();
                                                        if __ok.is_none() {
                                                            state.offset = __save_dispatch;
                                                            return None;
                                                        }
                                                    }
                                                    _ => {
                                                        return None;
                                                    }
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
                                state.offset = __pretty_cp19;
                                __builder.restore(__pretty_bcp20);
                            }
                            __ok
                        };
                        true
                    };
                    {
                        if !{
                            let __pretty_cp23 = state.offset;
                            let __ok = (|| -> bool {
                                {
                                    let __start = state.offset;
                                    if {
                                        let __start = state.offset;
                                        let __result: Option<()> = (|| {
                                            {
                                                let __save = state.offset;
                                                let __ok = (|| -> Option<()> {
                                                    if state.src_bytes.get(state.offset).copied() != Some(b'$')
                                                    {
                                                        return None;
                                                    }
                                                    state.offset += 1;
                                                    Some(())
                                                })();
                                                if __ok.is_none() {
                                                    state.offset = __save;
                                                }
                                            }
                                            {
                                                let __end = state.src_bytes.len();
                                                let mut __pos = state.offset;
                                                let mut __count: u32 = 0;
                                                while __pos < __end && __count < 3 {
                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                    if __b.is_ascii_alphabetic() {
                                                        __pos += 1;
                                                        __count += 1;
                                                    } else {
                                                        break;
                                                    }
                                                }
                                                if __count < 1 {
                                                    return None;
                                                }
                                                state.offset = __pos;
                                            }
                                            {
                                                let __save = state.offset;
                                                let __ok = (|| -> Option<()> {
                                                    if state.src_bytes.get(state.offset).copied() != Some(b'$')
                                                    {
                                                        return None;
                                                    }
                                                    state.offset += 1;
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
                                state.offset = __pretty_cp23;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp22 = state.offset;
                                    let __ok = (|| -> bool {
                                        {
                                            let __start = state.offset;
                                            if {
                                                let __start = state.offset;
                                                let __result: Option<()> = (|| {
                                                    {
                                                        let __save = state.offset;
                                                        let __ok = (|| -> Option<()> {
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'$')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            Some(())
                                                        })();
                                                        if __ok.is_none() {
                                                            state.offset = __save;
                                                        }
                                                    }
                                                    {
                                                        let __end = state.src_bytes.len();
                                                        let mut __pos = state.offset;
                                                        let mut __count: u32 = 0;
                                                        while __pos < __end && __count < 3 {
                                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                            if __b.is_ascii_alphabetic() {
                                                                __pos += 1;
                                                                __count += 1;
                                                            } else {
                                                                break;
                                                            }
                                                        }
                                                        if __count < 1 {
                                                            return None;
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
                                        state.offset = __pretty_cp22;
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp21 = state.offset;
                                            let __ok = (|| -> bool {
                                                {
                                                    let __start = state.offset;
                                                    if {
                                                        let __start = state.offset;
                                                        let __result: Option<()> = (|| {
                                                            {
                                                                let __save = state.offset;
                                                                let __ok = (|| -> Option<()> {
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'$')
                                                                    {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
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
                                                state.offset = __pretty_cp21;
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
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b':') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b':');
                    };
                    {
                        if !{
                            let __pretty_cp26 = state.offset;
                            let __ok = (|| -> bool {
                                {
                                    let __start = state.offset;
                                    if {
                                        let __start = state.offset;
                                        let __result: Option<()> = (|| {
                                            {
                                                let __save = state.offset;
                                                let __ok = (|| -> Option<()> {
                                                    if state.src_bytes.get(state.offset).copied() != Some(b'$')
                                                    {
                                                        return None;
                                                    }
                                                    state.offset += 1;
                                                    Some(())
                                                })();
                                                if __ok.is_none() {
                                                    state.offset = __save;
                                                }
                                            }
                                            {
                                                let __end = state.src_bytes.len();
                                                let mut __pos = state.offset;
                                                let mut __count: u32 = 0;
                                                while __pos < __end && __count < 3 {
                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                    if __b.is_ascii_alphabetic() {
                                                        __pos += 1;
                                                        __count += 1;
                                                    } else {
                                                        break;
                                                    }
                                                }
                                                if __count < 1 {
                                                    return None;
                                                }
                                                state.offset = __pos;
                                            }
                                            {
                                                let __save = state.offset;
                                                let __ok = (|| -> Option<()> {
                                                    if state.src_bytes.get(state.offset).copied() != Some(b'$')
                                                    {
                                                        return None;
                                                    }
                                                    state.offset += 1;
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
                                state.offset = __pretty_cp26;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp25 = state.offset;
                                    let __ok = (|| -> bool {
                                        {
                                            let __start = state.offset;
                                            if {
                                                let __start = state.offset;
                                                let __result: Option<()> = (|| {
                                                    {
                                                        let __save = state.offset;
                                                        let __ok = (|| -> Option<()> {
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'$')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            Some(())
                                                        })();
                                                        if __ok.is_none() {
                                                            state.offset = __save;
                                                        }
                                                    }
                                                    {
                                                        let __end = state.src_bytes.len();
                                                        let mut __pos = state.offset;
                                                        let mut __count: u32 = 0;
                                                        while __pos < __end && __count < 3 {
                                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                            if __b.is_ascii_alphabetic() {
                                                                __pos += 1;
                                                                __count += 1;
                                                            } else {
                                                                break;
                                                            }
                                                        }
                                                        if __count < 1 {
                                                            return None;
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
                                        state.offset = __pretty_cp25;
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp24 = state.offset;
                                            let __ok = (|| -> bool {
                                                {
                                                    let __start = state.offset;
                                                    if {
                                                        let __start = state.offset;
                                                        let __result: Option<()> = (|| {
                                                            {
                                                                let __save = state.offset;
                                                                let __ok = (|| -> Option<()> {
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'$')
                                                                    {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
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
                                                state.offset = __pretty_cp24;
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
                };
                true
            }
        }
        pub fn range_ref_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__range_ref_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __cell_or_range_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp27 = state.offset;
                        let __pretty_bcp28 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__range_ref_prettify(state, __builder) {
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
                        if !Self::__cell_prettify(state, __builder) {
                            return false;
                        }
                    }
                };
                true
            }
        }
        pub fn cell_or_range_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__cell_or_range_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __comparison_expr_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        if !{
                            let __pretty_cp31 = state.offset;
                            let __pretty_bcp32 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows29 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder.text_inline_ws(&state.src[__ows29..state.offset]);
                                    if !Self::__concat_expr_prettify(state, __builder) {
                                        return false;
                                    }
                                    let __ows30 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder.text_inline_ws(&state.src[__ows30..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp31;
                                __builder.restore(__pretty_bcp32);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        let mut __rep_count43 = 0usize;
                        while __rep_count43 < 4294967295 {
                            let __rep_cp44 = state.offset;
                            if !{
                                let __pretty_cp41 = state.offset;
                                let __pretty_bcp42 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            if !{
                                                let __pretty_cp35 = state.offset;
                                                let __pretty_bcp36 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows33 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows33..state.offset]);
                                                        if !Self::__compare_op_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        let __ows34 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows34..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp35;
                                                    __builder.restore(__pretty_bcp36);
                                                }
                                                __ok
                                            } {
                                                return false;
                                            }
                                        };
                                        {
                                            if !{
                                                let __pretty_cp39 = state.offset;
                                                let __pretty_bcp40 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows37 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows37..state.offset]);
                                                        if !Self::__concat_expr_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        let __ows38 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows38..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp39;
                                                    __builder.restore(__pretty_bcp40);
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
                                    state.offset = __pretty_cp41;
                                    __builder.restore(__pretty_bcp42);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp44;
                                break;
                            }
                            if state.offset == __rep_cp44 {
                                break;
                            }
                            __rep_count43 += 1;
                        }
                    };
                };
                true
            }
        }
        pub fn comparison_expr_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__comparison_expr_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __mul_expr_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        if !{
                            let __pretty_cp47 = state.offset;
                            let __pretty_bcp48 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows45 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder.text_inline_ws(&state.src[__ows45..state.offset]);
                                    if !Self::__exp_expr_prettify(state, __builder) {
                                        return false;
                                    }
                                    let __ows46 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder.text_inline_ws(&state.src[__ows46..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp47;
                                __builder.restore(__pretty_bcp48);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        let mut __rep_count59 = 0usize;
                        while __rep_count59 < 4294967295 {
                            let __rep_cp60 = state.offset;
                            if !{
                                let __pretty_cp57 = state.offset;
                                let __pretty_bcp58 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            if !{
                                                let __pretty_cp51 = state.offset;
                                                let __pretty_bcp52 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows49 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows49..state.offset]);
                                                        if !Self::__mul_op_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        let __ows50 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows50..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp51;
                                                    __builder.restore(__pretty_bcp52);
                                                }
                                                __ok
                                            } {
                                                return false;
                                            }
                                        };
                                        {
                                            if !{
                                                let __pretty_cp55 = state.offset;
                                                let __pretty_bcp56 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows53 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows53..state.offset]);
                                                        if !Self::__exp_expr_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        let __ows54 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows54..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp55;
                                                    __builder.restore(__pretty_bcp56);
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
                                    state.offset = __pretty_cp57;
                                    __builder.restore(__pretty_bcp58);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp60;
                                break;
                            }
                            if state.offset == __rep_cp60 {
                                break;
                            }
                            __rep_count59 += 1;
                        }
                    };
                };
                true
            }
        }
        pub fn mul_expr_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__mul_expr_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __unary_expr_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let mut __rep_count63 = 0usize;
                        while __rep_count63 < 4294967295 {
                            let __rep_cp64 = state.offset;
                            if !{
                                let __pretty_cp61 = state.offset;
                                let __pretty_bcp62 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    if !Self::__unary_prefix_prettify(state, __builder) {
                                        return false;
                                    }
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp61;
                                    __builder.restore(__pretty_bcp62);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp64;
                                break;
                            }
                            if state.offset == __rep_cp64 {
                                break;
                            }
                            __rep_count63 += 1;
                        }
                    };
                    if !Self::__postfix_expr_prettify(state, __builder) {
                        return false;
                    }
                };
                true
            }
        }
        pub fn unary_expr_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__unary_expr_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __paren_expr_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'(') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'(');
                    };
                    {
                        if !{
                            let __pretty_cp67 = state.offset;
                            let __pretty_bcp68 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows65 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder.text_inline_ws(&state.src[__ows65..state.offset]);
                                    if !Self::__expression_prettify(state, __builder) {
                                        return false;
                                    }
                                    let __ows66 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder.text_inline_ws(&state.src[__ows66..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp67;
                                __builder.restore(__pretty_bcp68);
                            }
                            __ok
                        } {
                            return false;
                        }
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
        pub fn paren_expr_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__paren_expr_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __arg_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let _ = {
                        let __pretty_cp69 = state.offset;
                        let __pretty_bcp70 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__expression_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp69;
                            __builder.restore(__pretty_bcp70);
                        }
                        __ok
                    };
                    true
                };
                true
            }
        }
        pub fn arg_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__arg_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __func_args_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            __builder.indent_open();
            let __pretty_ok = {
                {
                    {
                        let __rep_start81 = state.offset;
                        let __rep_bcp82 = __builder.checkpoint();
                        let mut __rep_count79 = 0usize;
                        while __rep_count79 < 4294967295 {
                            let __rep_cp80 = state.offset;
                            let __iter_cp = if __rep_count79 > 0 {
                                Some(__builder.checkpoint())
                            } else {
                                None
                            };
                            if __rep_count79 > 0 {
                                __builder.sep(", ", "");
                            }
                            if !{
                                let __pretty_cp78 = state.offset;
                                let __ok = (|| -> bool {
                                    {
                                        if !Self::__arg_prettify(state, __builder) {
                                            return false;
                                        }
                                        {
                                            let __silent_cp76 = state.offset;
                                            let __silent_bcp77 = __builder.light_checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let _ = {
                                                        let __pretty_cp74 = state.offset;
                                                        let __pretty_bcp75 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            {
                                                                let __ows71 = state.offset;
                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                let __ows72 = state.offset;
                                                                {
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                    {
                                                                        return false;
                                                                    }
                                                                    state.offset += 1;
                                                                    __builder.char(b',');
                                                                };
                                                                __builder.text_inline_ws(&state.src[__ows71..__ows72]);
                                                                let __ows73 = state.offset;
                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                __builder.text_inline_ws(&state.src[__ows73..state.offset]);
                                                            };
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp74;
                                                            __builder.restore(__pretty_bcp75);
                                                        }
                                                        __ok
                                                    };
                                                    true
                                                };
                                                true
                                            })();
                                            __builder.light_restore(__silent_bcp77);
                                            if !__ok {
                                                state.offset = __silent_cp76;
                                                return false;
                                            }
                                        };
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp78;
                                }
                                __ok
                            } {
                                state.offset = __rep_cp80;
                                if let Some(__bcp) = __iter_cp {
                                    __builder.restore(__bcp);
                                }
                                break;
                            }
                            if state.offset == __rep_cp80 {
                                if let Some(__bcp) = __iter_cp {
                                    __builder.restore(__bcp);
                                }
                                break;
                            }
                            __rep_count79 += 1;
                        }
                        if __rep_count79 < 1 {
                            state.offset = __rep_start81;
                            __builder.restore(__rep_bcp82);
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
        pub fn func_args_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__func_args_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __let_binding_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            __builder.indent_open();
            let __pretty_ok = {
                {
                    {
                        {
                            if !Self::__expression_prettify(state, __builder) {
                                return false;
                            }
                            {
                                let __ows83 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                let __ows84 = state.offset;
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b',')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b',');
                                };
                                __builder.text_inline_ws(&state.src[__ows83..__ows84]);
                                let __ows85 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows85..state.offset]);
                            };
                        };
                        if !Self::__expression_prettify(state, __builder) {
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
        pub fn let_binding_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__let_binding_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __lambda_params_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            __builder.indent_open();
            let __pretty_ok = {
                {
                    {
                        let __rep_start96 = state.offset;
                        let __rep_bcp97 = __builder.checkpoint();
                        let mut __rep_count94 = 0usize;
                        while __rep_count94 < 4294967295 {
                            let __rep_cp95 = state.offset;
                            let __iter_cp = if __rep_count94 > 0 {
                                Some(__builder.checkpoint())
                            } else {
                                None
                            };
                            if __rep_count94 > 0 {
                                __builder.sep(", ", "");
                            }
                            if !{
                                let __pretty_cp93 = state.offset;
                                let __ok = (|| -> bool {
                                    {
                                        if !Self::__expression_prettify(state, __builder) {
                                            return false;
                                        }
                                        {
                                            let __silent_cp91 = state.offset;
                                            let __silent_bcp92 = __builder.light_checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let _ = {
                                                        let __pretty_cp89 = state.offset;
                                                        let __pretty_bcp90 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            {
                                                                let __ows86 = state.offset;
                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                let __ows87 = state.offset;
                                                                {
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                    {
                                                                        return false;
                                                                    }
                                                                    state.offset += 1;
                                                                    __builder.char(b',');
                                                                };
                                                                __builder.text_inline_ws(&state.src[__ows86..__ows87]);
                                                                let __ows88 = state.offset;
                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                __builder.text_inline_ws(&state.src[__ows88..state.offset]);
                                                            };
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp89;
                                                            __builder.restore(__pretty_bcp90);
                                                        }
                                                        __ok
                                                    };
                                                    true
                                                };
                                                true
                                            })();
                                            __builder.light_restore(__silent_bcp92);
                                            if !__ok {
                                                state.offset = __silent_cp91;
                                                return false;
                                            }
                                        };
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp93;
                                }
                                __ok
                            } {
                                state.offset = __rep_cp95;
                                if let Some(__bcp) = __iter_cp {
                                    __builder.restore(__bcp);
                                }
                                break;
                            }
                            if state.offset == __rep_cp95 {
                                if let Some(__bcp) = __iter_cp {
                                    __builder.restore(__bcp);
                                }
                                break;
                            }
                            __rep_count94 += 1;
                        }
                        if __rep_count94 < 1 {
                            state.offset = __rep_start96;
                            __builder.restore(__rep_bcp97);
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
        pub fn lambda_params_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__lambda_params_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __array_row_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !Self::__expression_prettify(state, __builder) {
                        return false;
                    }
                    {
                        let mut __rep_count105 = 0usize;
                        while __rep_count105 < 4294967295 {
                            let __rep_cp106 = state.offset;
                            if !{
                                let __pretty_cp103 = state.offset;
                                let __pretty_bcp104 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        __builder.sep(", ", "");
                                        {
                                            let __silent_cp101 = state.offset;
                                            let __silent_bcp102 = __builder.light_checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows98 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    let __ows99 = state.offset;
                                                    {
                                                        if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                        {
                                                            return false;
                                                        }
                                                        state.offset += 1;
                                                        __builder.char(b',');
                                                    };
                                                    __builder.text_inline_ws(&state.src[__ows98..__ows99]);
                                                    let __ows100 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows100..state.offset]);
                                                };
                                                true
                                            })();
                                            __builder.light_restore(__silent_bcp102);
                                            if !__ok {
                                                state.offset = __silent_cp101;
                                                return false;
                                            }
                                        };
                                        if !Self::__expression_prettify(state, __builder) {
                                            return false;
                                        }
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp103;
                                    __builder.restore(__pretty_bcp104);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp106;
                                break;
                            }
                            if state.offset == __rep_cp106 {
                                break;
                            }
                            __rep_count105 += 1;
                        }
                    };
                };
                true
            }
        }
        pub fn array_row_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__array_row_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __array_rows_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            __builder.indent_open();
            let __pretty_ok = {
                {
                    {
                        if !Self::__array_row_prettify(state, __builder) {
                            return false;
                        }
                        {
                            let mut __rep_count114 = 0usize;
                            while __rep_count114 < 4294967295 {
                                let __rep_cp115 = state.offset;
                                if !{
                                    let __pretty_cp112 = state.offset;
                                    let __pretty_bcp113 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            __builder.sep("; ", "");
                                            {
                                                let __silent_cp110 = state.offset;
                                                let __silent_bcp111 = __builder.light_checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows107 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        let __ows108 = state.offset;
                                                        {
                                                            if state.src_bytes.get(state.offset).copied() != Some(b';')
                                                            {
                                                                return false;
                                                            }
                                                            state.offset += 1;
                                                            __builder.char(b';');
                                                        };
                                                        __builder.text_inline_ws(&state.src[__ows107..__ows108]);
                                                        let __ows109 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows109..state.offset]);
                                                    };
                                                    true
                                                })();
                                                __builder.light_restore(__silent_bcp111);
                                                if !__ok {
                                                    state.offset = __silent_cp110;
                                                    return false;
                                                }
                                            };
                                            if !Self::__array_row_prettify(state, __builder) {
                                                return false;
                                            }
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp112;
                                        __builder.restore(__pretty_bcp113);
                                    }
                                    __ok
                                } {
                                    state.offset = __rep_cp115;
                                    break;
                                }
                                if state.offset == __rep_cp115 {
                                    break;
                                }
                                __rep_count114 += 1;
                            }
                        };
                    };
                    true
                }
            };
            __builder.indent_close();
            __builder.group_close();
            __pretty_ok
        }
        pub fn array_rows_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__array_rows_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __array_literal_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            __builder.indent_open();
            let __pretty_ok = {
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
                            if !{
                                let __pretty_cp118 = state.offset;
                                let __pretty_bcp119 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        let __ows116 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows116..state.offset]);
                                        if !Self::__array_rows_prettify(state, __builder) {
                                            return false;
                                        }
                                        let __ows117 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows117..state.offset]);
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp118;
                                    __builder.restore(__pretty_bcp119);
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
            };
            __builder.indent_close();
            __builder.group_close();
            __pretty_ok
        }
        pub fn array_literal_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__array_literal_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __concat_expr_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        if !{
                            let __pretty_cp122 = state.offset;
                            let __pretty_bcp123 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows120 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows120..state.offset]);
                                    if !Self::__add_expr_prettify(state, __builder) {
                                        return false;
                                    }
                                    let __ows121 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows121..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp122;
                                __builder.restore(__pretty_bcp123);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        let mut __rep_count133 = 0usize;
                        while __rep_count133 < 4294967295 {
                            let __rep_cp134 = state.offset;
                            if !{
                                let __pretty_cp131 = state.offset;
                                let __pretty_bcp132 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            let __ows124 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            let __ows125 = state.offset;
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'&')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b'&');
                                            };
                                            __builder.text_inline_ws(&state.src[__ows124..__ows125]);
                                            let __ows126 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder
                                                .text_inline_ws(&state.src[__ows126..state.offset]);
                                        };
                                        {
                                            if !{
                                                let __pretty_cp129 = state.offset;
                                                let __pretty_bcp130 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows127 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows127..state.offset]);
                                                        if !Self::__add_expr_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        let __ows128 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows128..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp129;
                                                    __builder.restore(__pretty_bcp130);
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
                                    state.offset = __pretty_cp131;
                                    __builder.restore(__pretty_bcp132);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp134;
                                break;
                            }
                            if state.offset == __rep_cp134 {
                                break;
                            }
                            __rep_count133 += 1;
                        }
                    };
                };
                true
            }
        }
        pub fn concat_expr_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__concat_expr_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __add_expr_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        if !{
                            let __pretty_cp137 = state.offset;
                            let __pretty_bcp138 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows135 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows135..state.offset]);
                                    if !Self::__mul_expr_prettify(state, __builder) {
                                        return false;
                                    }
                                    let __ows136 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows136..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp137;
                                __builder.restore(__pretty_bcp138);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        let mut __rep_count149 = 0usize;
                        while __rep_count149 < 4294967295 {
                            let __rep_cp150 = state.offset;
                            if !{
                                let __pretty_cp147 = state.offset;
                                let __pretty_bcp148 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            if !{
                                                let __pretty_cp141 = state.offset;
                                                let __pretty_bcp142 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows139 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows139..state.offset]);
                                                        if !Self::__add_op_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        let __ows140 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows140..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp141;
                                                    __builder.restore(__pretty_bcp142);
                                                }
                                                __ok
                                            } {
                                                return false;
                                            }
                                        };
                                        {
                                            if !{
                                                let __pretty_cp145 = state.offset;
                                                let __pretty_bcp146 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows143 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows143..state.offset]);
                                                        if !Self::__mul_expr_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        let __ows144 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows144..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp145;
                                                    __builder.restore(__pretty_bcp146);
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
                                    state.offset = __pretty_cp147;
                                    __builder.restore(__pretty_bcp148);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp150;
                                break;
                            }
                            if state.offset == __rep_cp150 {
                                break;
                            }
                            __rep_count149 += 1;
                        }
                    };
                };
                true
            }
        }
        pub fn add_expr_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__add_expr_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __exp_expr_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        if !{
                            let __pretty_cp153 = state.offset;
                            let __pretty_bcp154 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows151 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows151..state.offset]);
                                    if !Self::__unary_expr_prettify(state, __builder) {
                                        return false;
                                    }
                                    let __ows152 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows152..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp153;
                                __builder.restore(__pretty_bcp154);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        let mut __rep_count164 = 0usize;
                        while __rep_count164 < 4294967295 {
                            let __rep_cp165 = state.offset;
                            if !{
                                let __pretty_cp162 = state.offset;
                                let __pretty_bcp163 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            let __ows155 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            let __ows156 = state.offset;
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'^')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b'^');
                                            };
                                            __builder.text_inline_ws(&state.src[__ows155..__ows156]);
                                            let __ows157 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder
                                                .text_inline_ws(&state.src[__ows157..state.offset]);
                                        };
                                        {
                                            if !{
                                                let __pretty_cp160 = state.offset;
                                                let __pretty_bcp161 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows158 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows158..state.offset]);
                                                        if !Self::__unary_expr_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        let __ows159 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows159..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp160;
                                                    __builder.restore(__pretty_bcp161);
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
                                    state.offset = __pretty_cp162;
                                    __builder.restore(__pretty_bcp163);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp165;
                                break;
                            }
                            if state.offset == __rep_cp165 {
                                break;
                            }
                            __rep_count164 += 1;
                        }
                    };
                };
                true
            }
        }
        pub fn exp_expr_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__exp_expr_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __lambda_call_prettify<'a>(
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
                                let __result: Option<()> = (|| {
                                    {
                                        let __b = *state.src_bytes.get(state.offset)?;
                                        if !((__b == b'L' || __b == b'l')) {
                                            return None;
                                        }
                                        state.offset += 1;
                                    }
                                    {
                                        let __b = *state.src_bytes.get(state.offset)?;
                                        if !((__b == b'A' || __b == b'a')) {
                                            return None;
                                        }
                                        state.offset += 1;
                                    }
                                    {
                                        let __b = *state.src_bytes.get(state.offset)?;
                                        if !((__b == b'M' || __b == b'm')) {
                                            return None;
                                        }
                                        state.offset += 1;
                                    }
                                    {
                                        let __b = *state.src_bytes.get(state.offset)?;
                                        if !((__b == b'B' || __b == b'b')) {
                                            return None;
                                        }
                                        state.offset += 1;
                                    }
                                    {
                                        let __b = *state.src_bytes.get(state.offset)?;
                                        if !((__b == b'D' || __b == b'd')) {
                                            return None;
                                        }
                                        state.offset += 1;
                                    }
                                    {
                                        let __b = *state.src_bytes.get(state.offset)?;
                                        if !((__b == b'A' || __b == b'a')) {
                                            return None;
                                        }
                                        state.offset += 1;
                                    }
                                    if state.src_bytes.get(state.offset).copied() != Some(b'(')
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
                        {
                            if !{
                                let __pretty_cp168 = state.offset;
                                let __pretty_bcp169 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        let __ows166 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows166..state.offset]);
                                        if !Self::__lambda_params_prettify(state, __builder) {
                                            return false;
                                        }
                                        let __ows167 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows167..state.offset]);
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp168;
                                    __builder.restore(__pretty_bcp169);
                                }
                                __ok
                            } {
                                return false;
                            }
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
            };
            __builder.indent_close();
            __builder.group_close();
            __pretty_ok
        }
        pub fn lambda_call_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__lambda_call_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __expression_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                if !Self::__comparison_expr_prettify(state, __builder) {
                    return false;
                }
                true
            }
        }
        pub fn expression_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__expression_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __func_call_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            __builder.indent_open();
            let __pretty_ok = {
                {
                    {
                        if !Self::__func_open_prettify(state, __builder) {
                            return false;
                        }
                        {
                            if !{
                                let __pretty_cp174 = state.offset;
                                let __pretty_bcp175 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        let __ows172 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows172..state.offset]);
                                        {
                                            let _ = {
                                                let __pretty_cp170 = state.offset;
                                                let __pretty_bcp171 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    if !Self::__func_args_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp170;
                                                    __builder.restore(__pretty_bcp171);
                                                }
                                                __ok
                                            };
                                            true
                                        };
                                        let __ows173 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows173..state.offset]);
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp174;
                                    __builder.restore(__pretty_bcp175);
                                }
                                __ok
                            } {
                                return false;
                            }
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
            };
            __builder.indent_close();
            __builder.group_close();
            __pretty_ok
        }
        pub fn func_call_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__func_call_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __let_args_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let mut __rep_count180 = 0usize;
                        while __rep_count180 < 4294967295 {
                            let __rep_cp181 = state.offset;
                            let __iter_cp = if __rep_count180 > 0 {
                                Some(__builder.checkpoint())
                            } else {
                                None
                            };
                            if __rep_count180 > 0 {
                                __builder.hardline();
                            }
                            if !{
                                let __pretty_cp179 = state.offset;
                                let __ok = (|| -> bool {
                                    {
                                        if !Self::__let_binding_prettify(state, __builder) {
                                            return false;
                                        }
                                        {
                                            let __ows176 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            let __ows177 = state.offset;
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b',');
                                            };
                                            __builder.text_inline_ws(&state.src[__ows176..__ows177]);
                                            let __ows178 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder
                                                .text_inline_ws(&state.src[__ows178..state.offset]);
                                        };
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp179;
                                }
                                __ok
                            } {
                                state.offset = __rep_cp181;
                                if let Some(__bcp) = __iter_cp {
                                    __builder.restore(__bcp);
                                }
                                break;
                            }
                            if state.offset == __rep_cp181 {
                                if let Some(__bcp) = __iter_cp {
                                    __builder.restore(__bcp);
                                }
                                break;
                            }
                            __rep_count180 += 1;
                        }
                    };
                    if !Self::__expression_prettify(state, __builder) {
                        return false;
                    }
                };
                true
            }
        }
        pub fn let_args_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__let_args_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __let_call_prettify<'a>(
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
                                let __result: Option<()> = (|| {
                                    {
                                        let __b = *state.src_bytes.get(state.offset)?;
                                        if !((__b == b'L' || __b == b'l')) {
                                            return None;
                                        }
                                        state.offset += 1;
                                    }
                                    {
                                        let __b = *state.src_bytes.get(state.offset)?;
                                        if !((__b == b'E' || __b == b'e')) {
                                            return None;
                                        }
                                        state.offset += 1;
                                    }
                                    {
                                        let __b = *state.src_bytes.get(state.offset)?;
                                        if !((__b == b'T' || __b == b't')) {
                                            return None;
                                        }
                                        state.offset += 1;
                                    }
                                    if state.src_bytes.get(state.offset).copied() != Some(b'(')
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
                        {
                            if !{
                                let __pretty_cp184 = state.offset;
                                let __pretty_bcp185 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        let __ows182 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows182..state.offset]);
                                        if !Self::__let_args_prettify(state, __builder) {
                                            return false;
                                        }
                                        let __ows183 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows183..state.offset]);
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp184;
                                    __builder.restore(__pretty_bcp185);
                                }
                                __ok
                            } {
                                return false;
                            }
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
            };
            __builder.indent_close();
            __builder.group_close();
            __pretty_ok
        }
        pub fn let_call_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__let_call_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __primary_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp203 = state.offset;
                        let __pretty_bcp204 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__let_call_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp203;
                            __builder.restore(__pretty_bcp204);
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp201 = state.offset;
                                let __pretty_bcp202 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    if !Self::__lambda_call_prettify(state, __builder) {
                                        return false;
                                    }
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp201;
                                    __builder.restore(__pretty_bcp202);
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp199 = state.offset;
                                        let __pretty_bcp200 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            if !Self::__func_call_prettify(state, __builder) {
                                                return false;
                                            }
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp199;
                                            __builder.restore(__pretty_bcp200);
                                        }
                                        __ok
                                    } {
                                        {
                                            if !{
                                                let __pretty_cp198 = state.offset;
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __start = state.offset;
                                                        if {
                                                            let __start = state.offset;
                                                            let __result: Option<()> = (|| {
                                                                {
                                                                    let __save_dispatch = state.offset;
                                                                    let __dispatch_b = *state.src_bytes.get(state.offset)?;
                                                                    match __dispatch_b {
                                                                        b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' | b'8'
                                                                        | b'9' => {
                                                                            let __ok = (|| -> Option<()> {
                                                                                {
                                                                                    if ::parse_that::scan_digits_mut(state).is_none() {
                                                                                        return None;
                                                                                    }
                                                                                }
                                                                                {
                                                                                    let __save = state.offset;
                                                                                    let __ok = (|| -> Option<()> {
                                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                                                                        {
                                                                                            return None;
                                                                                        }
                                                                                        state.offset += 1;
                                                                                        Some(())
                                                                                    })();
                                                                                    if __ok.is_none() {
                                                                                        state.offset = __save;
                                                                                    }
                                                                                }
                                                                                {
                                                                                    let _ = ::parse_that::scan_digits_star_mut(state);
                                                                                }
                                                                                Some(())
                                                                            })();
                                                                            if __ok.is_none() {
                                                                                state.offset = __save_dispatch;
                                                                                return None;
                                                                            }
                                                                        }
                                                                        b'.' => {
                                                                            let __ok = (|| -> Option<()> {
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
                                                                                Some(())
                                                                            })();
                                                                            if __ok.is_none() {
                                                                                state.offset = __save_dispatch;
                                                                                return None;
                                                                            }
                                                                        }
                                                                        _ => {
                                                                            return None;
                                                                        }
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
                                                    state.offset = __pretty_cp198;
                                                }
                                                __ok
                                            } {
                                                {
                                                    if !{
                                                        let __pretty_cp196 = state.offset;
                                                        let __pretty_bcp197 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            if !Self::__boolean_prettify(state, __builder) {
                                                                return false;
                                                            }
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp196;
                                                            __builder.restore(__pretty_bcp197);
                                                        }
                                                        __ok
                                                    } {
                                                        {
                                                            if !{
                                                                let __pretty_cp194 = state.offset;
                                                                let __pretty_bcp195 = __builder.checkpoint();
                                                                let __ok = (|| -> bool {
                                                                    if !Self::__cell_or_range_prettify(state, __builder) {
                                                                        return false;
                                                                    }
                                                                    true
                                                                })();
                                                                if !__ok {
                                                                    state.offset = __pretty_cp194;
                                                                    __builder.restore(__pretty_bcp195);
                                                                }
                                                                __ok
                                                            } {
                                                                {
                                                                    if !{
                                                                        let __pretty_cp193 = state.offset;
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
                                                                                        if ::parse_that::scan_string_quoted(state).is_none() {
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
                                                                                            if !Self::__error_literal_prettify(state, __builder) {
                                                                                                return false;
                                                                                            }
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
                                                                                                    if !Self::__array_literal_prettify(state, __builder) {
                                                                                                        return false;
                                                                                                    }
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
                                                                                                            if !Self::__paren_expr_prettify(state, __builder) {
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
        pub fn primary_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__primary_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __postfix_expr_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !Self::__primary_prettify(state, __builder) {
                        return false;
                    }
                    {
                        let mut __rep_count207 = 0usize;
                        while __rep_count207 < 4294967295 {
                            let __rep_cp208 = state.offset;
                            if !{
                                let __pretty_cp205 = state.offset;
                                let __pretty_bcp206 = __builder.checkpoint();
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
                                    state.offset = __pretty_cp205;
                                    __builder.restore(__pretty_bcp206);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp208;
                                break;
                            }
                            if state.offset == __rep_cp208 {
                                break;
                            }
                            __rep_count207 += 1;
                        }
                    };
                };
                true
            }
        }
        pub fn postfix_expr_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__postfix_expr_prettify(state, &mut __builder) {
                    return None;
                }
                Some(__builder.finish())
            })
        }
        fn __formula_prettify<'a>(
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
                                {
                                    let __save = state.offset;
                                    let __ok = (|| -> Option<()> {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'=')
                                        {
                                            return None;
                                        }
                                        state.offset += 1;
                                        Some(())
                                    })();
                                    if __ok.is_none() {
                                        state.offset = __save;
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
                    if !Self::__expression_prettify(state, __builder) {
                        return false;
                    }
                };
                true
            }
        }
        pub fn formula_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
            Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                let mut __builder = ::pprint::FmtBuilder::with_capacity(
                    state.src.len().saturating_mul(2),
                );
                if !Self::__formula_prettify(state, &mut __builder) {
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
            crate::runtime::google_sheets::SheetsDocument<'_>,
            crate::runtime::ParseErr,
        > {
            let __input_bytes = input.as_bytes();
            let mut state = __shape_support_GoogleSheetsParser::ScanState::new();
            let mut builder = crate::runtime::google_sheets::SheetsStructBuilder::new();
            static __EAGER_EMPTY_PATH: ::std::sync::LazyLock<
                crate::path::ir::TypedPath<crate::path::markers::Json, &'static str>,
            > = ::std::sync::LazyLock::new(|| {
                crate::path::ir::TypedPath::from_owned(::std::vec::Vec::new())
            });
            let mut __eager_cursor: crate::path::cursor::PathCursor<
                'static,
                crate::path::ir::TypedPath<crate::path::markers::Json, &'static str>,
            > = crate::path::cursor::PathCursor::new(
                &*__EAGER_EMPTY_PATH,
                |_rid, _kind, _idx| crate::path::cursor::Decision::ParseFully,
            );
            {
                let mut pos: usize = 0;
                parse_GoogleSheetsParser_formula(
                        __input_bytes,
                        &mut pos,
                        &mut state,
                        &mut builder,
                        &mut __eager_cursor,
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
                let _ = __shape_support_GoogleSheetsParser::skip_space(
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
pub use __googlesheetsparser_emit_impl::*;
