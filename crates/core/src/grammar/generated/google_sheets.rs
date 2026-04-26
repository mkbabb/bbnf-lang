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

use ::bbnf::runtime::tape::*;
use ::bbnf::runtime::{Parsed, ParseErr, Root};
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
    static __GRAMMAR_PROFILE_ALPHABET: [u8; 19usize] = [
        35, 37, 38, 40, 41, 42, 43, 44, 45, 47, 58, 59, 60, 61, 62, 78, 94, 123, 125,
    ];
    static __GRAMMAR_PROFILE_DIGRAPHS: [(u8, u8); 1usize] = [(62, 61)];
    static __GRAMMAR_PROFILE_QUOTE_CLASSES: [u8; 1usize] = [34];
    /// Per-grammar codegen fingerprint — consolidated static
    /// profile emitted by Tranche AV Phase 1. Every downstream
    /// consumer (tape capacity, scanner dispatch) reads the
    /// matching field.
    pub const GRAMMAR_PROFILE: ::bbnf::runtime::tape::GrammarProfile = ::bbnf::runtime::tape::GrammarProfile {
        compounds_per_input_byte: 1f32,
        leaves_per_input_byte: 0f32,
        parallel_break_even_bytes: 1048576u32,
        structural_alphabet: &__GRAMMAR_PROFILE_ALPHABET,
        structural_digraphs: &__GRAMMAR_PROFILE_DIGRAPHS,
        structural_digraph_mask: [4611686018427387904, 0, 0, 0],
        structural_quote_classes: &__GRAMMAR_PROFILE_QUOTE_CLASSES,
    };
    /// AW-III.W6.2 — PHF keyword table.
    ///
    /// Mined literal-led Alt branches, sorted lexicographically.
    /// Binary search dispatches in O(log N) compares; LLVM lowers
    /// the fixed-size table to a balanced compare tree.
    static __PHF_GoogleSheetsParser_10_KW: [&[u8]; 4usize] = [b"<", b"=", b">", b">="];
    /// Per-entry branch discriminant — parallel to [`#kw_ident`].
    /// Entry `i`'s keyword bytes at `#kw_ident[i]` route to the
    /// branch with discriminant `#idx_ident[i]`.
    static __PHF_GoogleSheetsParser_10_IDX: [u8; 4usize] = [0, 4, 3, 1];
    /// AW-III.W6.2 — dispatch the mined keyword table for rule
    /// `#rule_id`.
    ///
    /// Returns `Some(branch_idx)` when `bytes` matches a mined
    /// keyword, `None` otherwise. Called from the walker's
    /// AltLinear / ClassifyByte arm to short-circuit the branch
    /// scan to a single binary search.
    #[allow(dead_code)]
    #[inline]
    fn __phf_GoogleSheetsParser_dispatch_10(bytes: &[u8]) -> ::core::option::Option<u8> {
        match __PHF_GoogleSheetsParser_10_KW.binary_search(&bytes) {
            ::core::result::Result::Ok(idx) => {
                ::core::option::Option::Some(__PHF_GoogleSheetsParser_10_IDX[idx])
            }
            ::core::result::Result::Err(_) => ::core::option::Option::None,
        }
    }
    /// AW-III.W6.2 — PHF keyword table.
    ///
    /// Mined literal-led Alt branches, sorted lexicographically.
    /// Binary search dispatches in O(log N) compares; LLVM lowers
    /// the fixed-size table to a balanced compare tree.
    static __PHF_GoogleSheetsParser_21_KW: [&[u8]; 3usize] = [b"#", b"(", b"{"];
    /// Per-entry branch discriminant — parallel to [`#kw_ident`].
    /// Entry `i`'s keyword bytes at `#kw_ident[i]` route to the
    /// branch with discriminant `#idx_ident[i]`.
    static __PHF_GoogleSheetsParser_21_IDX: [u8; 3usize] = [8, 10, 9];
    /// AW-III.W6.2 — dispatch the mined keyword table for rule
    /// `#rule_id`.
    ///
    /// Returns `Some(branch_idx)` when `bytes` matches a mined
    /// keyword, `None` otherwise. Called from the walker's
    /// AltLinear / ClassifyByte arm to short-circuit the branch
    /// scan to a single binary search.
    #[allow(dead_code)]
    #[inline]
    fn __phf_GoogleSheetsParser_dispatch_21(bytes: &[u8]) -> ::core::option::Option<u8> {
        match __PHF_GoogleSheetsParser_21_KW.binary_search(&bytes) {
            ::core::result::Result::Ok(idx) => {
                ::core::option::Option::Some(__PHF_GoogleSheetsParser_21_IDX[idx])
            }
            ::core::result::Result::Err(_) => ::core::option::Option::None,
        }
    }
    /// AX.W0a.2.l — per-rule dense Pratt precedence LUT.
    ///
    /// One byte per dispatch byte for this Pratt rule's
    /// operator alphabet. Consulted inline by the rule's
    /// emitted `parse_pratt_*` body. See `bbnf::backend::
    /// rust::emitter::precedence` for the bit layout.
    pub const PRECEDENCE_LUT_concat_expr: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 4u8, 0u8, 0u8, 0u8, 2u8, 3u8, 0u8, 3u8, 0u8, 2u8,
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
    pub const PRECEDENCE_ENTRIES_concat_expr: &[::bbnf::runtime::tape::DtaPrecedenceEntry] = &[
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 38u8,
            second_byte: ::core::option::Option::None,
            precedence: 4u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(12u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 43u8,
            second_byte: ::core::option::Option::None,
            precedence: 3u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(13u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 45u8,
            second_byte: ::core::option::Option::None,
            precedence: 3u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(13u32),
            op_discriminant: 1u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 42u8,
            second_byte: ::core::option::Option::None,
            precedence: 2u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(15u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 47u8,
            second_byte: ::core::option::Option::None,
            precedence: 2u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(15u32),
            op_discriminant: 1u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 94u8,
            second_byte: ::core::option::Option::None,
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Right,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(17u32),
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
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 4u8, 0u8, 0u8, 0u8, 2u8, 3u8, 0u8, 3u8, 0u8, 2u8,
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
    pub const PRECEDENCE_ENTRIES_add_expr: &[::bbnf::runtime::tape::DtaPrecedenceEntry] = &[
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 38u8,
            second_byte: ::core::option::Option::None,
            precedence: 4u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(12u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 43u8,
            second_byte: ::core::option::Option::None,
            precedence: 3u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(13u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 45u8,
            second_byte: ::core::option::Option::None,
            precedence: 3u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(13u32),
            op_discriminant: 1u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 42u8,
            second_byte: ::core::option::Option::None,
            precedence: 2u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(15u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 47u8,
            second_byte: ::core::option::Option::None,
            precedence: 2u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(15u32),
            op_discriminant: 1u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 94u8,
            second_byte: ::core::option::Option::None,
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Right,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(17u32),
            op_discriminant: 0u8,
        },
    ];
    /// AX.W0a.2.l — per-rule dense Pratt precedence LUT.
    ///
    /// One byte per dispatch byte for this Pratt rule's
    /// operator alphabet. Consulted inline by the rule's
    /// emitted `parse_pratt_*` body. See `bbnf::backend::
    /// rust::emitter::precedence` for the bit layout.
    pub const PRECEDENCE_LUT_mul_expr: [u8; 256] = [
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 4u8, 0u8, 0u8, 0u8, 2u8, 3u8, 0u8, 3u8, 0u8, 2u8,
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
    pub const PRECEDENCE_ENTRIES_mul_expr: &[::bbnf::runtime::tape::DtaPrecedenceEntry] = &[
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 38u8,
            second_byte: ::core::option::Option::None,
            precedence: 4u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(12u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 43u8,
            second_byte: ::core::option::Option::None,
            precedence: 3u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(13u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 45u8,
            second_byte: ::core::option::Option::None,
            precedence: 3u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(13u32),
            op_discriminant: 1u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 42u8,
            second_byte: ::core::option::Option::None,
            precedence: 2u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(15u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 47u8,
            second_byte: ::core::option::Option::None,
            precedence: 2u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(15u32),
            op_discriminant: 1u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 94u8,
            second_byte: ::core::option::Option::None,
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Right,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(17u32),
            op_discriminant: 0u8,
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
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 4u8, 0u8, 0u8, 0u8, 2u8, 3u8, 0u8, 3u8, 0u8, 2u8,
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
    pub const PRECEDENCE_ENTRIES_exp_expr: &[::bbnf::runtime::tape::DtaPrecedenceEntry] = &[
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 38u8,
            second_byte: ::core::option::Option::None,
            precedence: 4u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(12u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 43u8,
            second_byte: ::core::option::Option::None,
            precedence: 3u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(13u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 45u8,
            second_byte: ::core::option::Option::None,
            precedence: 3u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(13u32),
            op_discriminant: 1u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 42u8,
            second_byte: ::core::option::Option::None,
            precedence: 2u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(15u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 47u8,
            second_byte: ::core::option::Option::None,
            precedence: 2u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(15u32),
            op_discriminant: 1u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 94u8,
            second_byte: ::core::option::Option::None,
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Right,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(17u32),
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
    pub const PRECEDENCE_ENTRIES_comparison_expr: &[::bbnf::runtime::tape::DtaPrecedenceEntry] = &[
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 60u8,
            second_byte: ::core::option::Option::Some(62u8),
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(10u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 60u8,
            second_byte: ::core::option::Option::Some(61u8),
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(10u32),
            op_discriminant: 1u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 62u8,
            second_byte: ::core::option::Option::Some(61u8),
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(10u32),
            op_discriminant: 2u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 60u8,
            second_byte: ::core::option::Option::None,
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(10u32),
            op_discriminant: 3u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 62u8,
            second_byte: ::core::option::Option::None,
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(10u32),
            op_discriminant: 4u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 61u8,
            second_byte: ::core::option::Option::None,
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(10u32),
            op_discriminant: 5u8,
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
    pub const PRECEDENCE_ENTRIES_array_row: &[::bbnf::runtime::tape::DtaPrecedenceEntry] = &[
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 44u8,
            second_byte: ::core::option::Option::None,
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(32u32),
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
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 0u8,
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
    pub const PRECEDENCE_ENTRIES_array_rows: &[::bbnf::runtime::tape::DtaPrecedenceEntry] = &[
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 59u8,
            second_byte: ::core::option::Option::None,
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(33u32),
            op_discriminant: 0u8,
        },
    ];
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
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 4u8, 0u8, 0u8, 0u8, 2u8, 3u8, 1u8, 3u8, 0u8, 2u8,
        0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 1u8, 1u8, 1u8, 1u8, 0u8,
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
    /// Consulted by the walker cold-path until W0b retires it.
    pub const PRECEDENCE_ENTRIES: &[::bbnf::runtime::tape::DtaPrecedenceEntry] = &[
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 38u8,
            second_byte: ::core::option::Option::None,
            precedence: 4u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(12u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 43u8,
            second_byte: ::core::option::Option::None,
            precedence: 3u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(13u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 45u8,
            second_byte: ::core::option::Option::None,
            precedence: 3u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(13u32),
            op_discriminant: 1u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 42u8,
            second_byte: ::core::option::Option::None,
            precedence: 2u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(15u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 47u8,
            second_byte: ::core::option::Option::None,
            precedence: 2u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(15u32),
            op_discriminant: 1u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 94u8,
            second_byte: ::core::option::Option::None,
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Right,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(17u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 38u8,
            second_byte: ::core::option::Option::None,
            precedence: 4u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(12u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 43u8,
            second_byte: ::core::option::Option::None,
            precedence: 3u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(13u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 45u8,
            second_byte: ::core::option::Option::None,
            precedence: 3u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(13u32),
            op_discriminant: 1u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 42u8,
            second_byte: ::core::option::Option::None,
            precedence: 2u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(15u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 47u8,
            second_byte: ::core::option::Option::None,
            precedence: 2u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(15u32),
            op_discriminant: 1u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 94u8,
            second_byte: ::core::option::Option::None,
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Right,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(17u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 38u8,
            second_byte: ::core::option::Option::None,
            precedence: 4u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(12u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 43u8,
            second_byte: ::core::option::Option::None,
            precedence: 3u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(13u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 45u8,
            second_byte: ::core::option::Option::None,
            precedence: 3u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(13u32),
            op_discriminant: 1u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 42u8,
            second_byte: ::core::option::Option::None,
            precedence: 2u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(15u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 47u8,
            second_byte: ::core::option::Option::None,
            precedence: 2u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(15u32),
            op_discriminant: 1u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 94u8,
            second_byte: ::core::option::Option::None,
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Right,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(17u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 38u8,
            second_byte: ::core::option::Option::None,
            precedence: 4u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(12u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 43u8,
            second_byte: ::core::option::Option::None,
            precedence: 3u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(13u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 45u8,
            second_byte: ::core::option::Option::None,
            precedence: 3u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(13u32),
            op_discriminant: 1u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 42u8,
            second_byte: ::core::option::Option::None,
            precedence: 2u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(15u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 47u8,
            second_byte: ::core::option::Option::None,
            precedence: 2u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(15u32),
            op_discriminant: 1u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 94u8,
            second_byte: ::core::option::Option::None,
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Right,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(17u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 60u8,
            second_byte: ::core::option::Option::Some(62u8),
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(10u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 60u8,
            second_byte: ::core::option::Option::Some(61u8),
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(10u32),
            op_discriminant: 1u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 62u8,
            second_byte: ::core::option::Option::Some(61u8),
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(10u32),
            op_discriminant: 2u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 60u8,
            second_byte: ::core::option::Option::None,
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(10u32),
            op_discriminant: 3u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 62u8,
            second_byte: ::core::option::Option::None,
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(10u32),
            op_discriminant: 4u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 61u8,
            second_byte: ::core::option::Option::None,
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(10u32),
            op_discriminant: 5u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 44u8,
            second_byte: ::core::option::Option::None,
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(32u32),
            op_discriminant: 0u8,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 59u8,
            second_byte: ::core::option::Option::None,
            precedence: 1u8,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(33u32),
            op_discriminant: 0u8,
        },
    ];
    /// AW-III.W6.5 — total mined operator count for this
    /// grammar. Non-zero iff the lift admitted ≥ 1 chain OR the
    /// shape classifier admitted ≥ 1 single-rung Pratt rule.
    pub const PRECEDENCE_OPERATOR_COUNT: usize = 32usize;
    static __DTA_REGEX_0: &str = "(\\d+\\.?\\d*|\\.\\d+)([eE][+-]?\\d+)?";
    static __DTA_REGEX_1: &str = "\"([^\"]|\"\")*\"";
    static __DTA_REGEX_2: &str = "[tT][rR][uU][eE]";
    static __DTA_REGEX_3: &str = "[fF][aA][lL][sS][eE]";
    static __DTA_REGEX_20: &str = "'(?:[^']|'')*'!";
    static __DTA_REGEX_21: &str = "[A-Za-z_]\\w*!";
    static __DTA_REGEX_23: &str = "\\$?[A-Za-z]{1,3}\\$?\\d+";
    static __DTA_REGEX_31: &str = "\\$?[A-Za-z]{1,3}";
    static __DTA_REGEX_32: &str = "\\$?\\d+";
    static __DTA_REGEX_43: &str = "[A-Za-z_][A-Za-z0-9_.]*";
    static __DTA_REGEX_148: &str = "[lL][eE][tT]\\(";
    static __DTA_REGEX_163: &str = "[lL][aA][mM][bB][dD][aA]\\(";
    static __DTA_REGEX_195: &str = "=?";
    /// AY.W4.3 — first-byte → admissible-pattern bitmap LUT.
    ///
    /// Each entry holds a u32 bitmap; bit `i` set means pattern
    /// `i` (in the adapter's collected order) admits this byte
    /// as a match-prefix. Read once at adapter entry; the
    /// dispatch cascade visits only patterns whose bit is set.
    #[allow(dead_code)]
    pub(crate) const __REGEX_FIRST_BYTE_LUT_GoogleSheetsParser: [u32; 256] = [
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 2, 0, 448, 0, 0, 16, 0, 0, 0, 0, 0, 0, 1, 0, 257, 257, 257,
        257, 257, 257, 257, 257, 257, 257, 0, 0, 0, 4096, 0, 0, 0, 736, 736, 736, 736,
        736, 744, 736, 736, 736, 736, 736, 3808, 736, 736, 736, 736, 736, 736, 736, 740,
        736, 736, 736, 736, 736, 736, 0, 0, 0, 0, 544, 0, 736, 736, 736, 736, 736, 744,
        736, 736, 736, 736, 736, 3808, 736, 736, 736, 736, 736, 736, 736, 740, 736, 736,
        736, 736, 736, 736, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
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
    pub(crate) const __REGEX_LAST_BYTE_SET_GoogleSheetsParser: [(u64, u64); 13] = [
        (0, 0),
        (17179869184, 0),
        (0, 0),
        (0, 0),
        (8589934592, 0),
        (8589934592, 0),
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
                                39 => __dfa_state = 3,
                                _ => break,
                            }
                        }
                        3 => {
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
                                65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76
                                | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88
                                | 89 | 90 | 95 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104
                                | 105 | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114
                                | 115 | 116 | 117 | 118 | 119 | 120 | 121 | 122 => {
                                    __dfa_state = 2;
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
                                33 => __dfa_state = 1,
                                48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 65 | 66
                                | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77 | 78
                                | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89 | 90
                                | 95 | 97 | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105
                                | 106 | 107 | 108 | 109 | 110 | 111 | 112 | 113 | 114 | 115
                                | 116 | 117 | 118 | 119 | 120 | 121 | 122 => __dfa_state = 2,
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_23.as_ptr())
            || pattern == __DTA_REGEX_23
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_31.as_ptr())
            || pattern == __DTA_REGEX_31
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_32.as_ptr())
            || pattern == __DTA_REGEX_32
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_43.as_ptr())
            || pattern == __DTA_REGEX_43
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_148.as_ptr())
            || pattern == __DTA_REGEX_148
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_163.as_ptr())
            || pattern == __DTA_REGEX_163
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
        if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_195.as_ptr())
            || pattern == __DTA_REGEX_195
        {
            if let Some(&__byte) = input.get(pos) {
                if (__REGEX_FIRST_BYTE_LUT_GoogleSheetsParser[__byte as usize] >> 12) & 1
                    == 0
                {
                    return ::core::option::Option::None;
                }
            }
            if input.len() >= 64 * 1024 {
                let (__lb_lo, __lb_hi) = __REGEX_LAST_BYTE_SET_GoogleSheetsParser[12];
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
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
        variant_idx: u8,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
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
                        ::bbnf::runtime::tape::TapeKind::Span,
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
                        ::bbnf::runtime::tape::TapeKind::Span,
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
                Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: open as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
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
    fn parse_string_visitor_escaped_GoogleSheetsParser<V>(
        input: &[u8],
        p: &mut usize,
        body_start: usize,
        _esc_start: usize,
        visitor: &mut V,
        is_key: bool,
        open: usize,
    ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
    where
        V: ::bbnf::runtime::tape::StringVisitor + ::bbnf::runtime::tape::ObjectVisitor,
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
                            ::bbnf::runtime::ParseErr::Syntax {
                                offset: open as u32,
                                rule: None,
                            }
                        })
                } else {
                    visitor
                        .string(&buf)
                        .map_err(|_| {
                            ::bbnf::runtime::ParseErr::Syntax {
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
                            ::bbnf::runtime::ParseErr::Syntax {
                                offset: open as u32,
                                rule: None,
                            }
                        })
                } else {
                    visitor
                        .string(body)
                        .map_err(|_| {
                            ::bbnf::runtime::ParseErr::Syntax {
                                offset: open as u32,
                                rule: None,
                            }
                        })
                }
            }
            None => {
                Err(::bbnf::runtime::ParseErr::Syntax {
                    offset: open as u32,
                    rule: None,
                })
            }
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
            let __ctns_idx = ensure_structural_index(state, input);
            if let ::core::option::Option::Some(__next_struct) = ::bbnf::runtime::tape::next_structural_at_or_after(
                __ctns_idx,
                *p as u32,
            ) {
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
    }
    /// AX.W0a.2.q — HRegex-shape parse function with typed
    /// host-fn decode (`NumberConvert` → f64, `HexConvert` → u32).
    ///
    /// Runs the per-grammar regex scan, invokes the decoder,
    /// writes the decoded bytes into the tape arena, pushes a
    /// payload-carrying leaf (KvPair when the rule projects as
    /// `Tuple([Span, scalar])`; Span otherwise) so the walker-
    /// parity reader (`payload_bytes(rec, N)`) finds the value.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_hregex_GoogleSheetsParser_number(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        {
            let span_lo = *p as u32;
            let Some(match_len) = __regex_scan_GoogleSheetsParser(
                "(\\d+\\.?\\d*|\\.\\d+)([eE][+-]?\\d+)?",
                input,
                *p,
            ) else {
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: span_lo,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            };
            *p += match_len as usize;
            let span_hi = *p as u32;
            let __f64: f64 = core::str::from_utf8(
                    &input[span_lo as usize..span_hi as usize],
                )
                .ok()
                .and_then(|s| s.parse::<f64>().ok())
                .unwrap_or(0.0);
            let __arena_off: u32 = builder.arena_mut().len() as u32;
            builder.arena_mut().extend_from_slice(&__f64.to_le_bytes());
            let leaf_off = builder
                .push_leaf_with_arena_payload(
                    ::bbnf::runtime::tape::TapeKind::Span,
                    span_lo,
                    span_hi,
                    0u8,
                    0u8,
                    __arena_off,
                    8u32,
                );
            Ok(leaf_off)
        }
    }
    /// AW-V.W3.2 — per-grammar String-shape parse function.
    ///
    /// Mirrors `json_prototype::string::parse_string_body`.
    /// `"` must NOT be consumed by the caller — this function
    /// reads it, scans for the closing quote, and pushes a Span
    /// leaf with appropriate borrow / arena-decode metadata.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_string_GoogleSheetsParser_string(
        input: &[u8],
        p: &mut usize,
        _state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let open = *p;
        if input.get(open).copied() != Some(b'"') {
            return Err(::bbnf::runtime::tape::DtaError::Syntax {
                offset: open as u32,
                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
            });
        }
        let body_start = open + 1;
        let tail = match input.get(body_start..) {
            Some(t) => t,
            None => {
                return Err(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                    offset: open as u32,
                });
            }
        };
        match __shape_support_GoogleSheetsParser::first_quote_or_backslash(tail) {
            Some((off, b'"')) => {
                let end = body_start + off;
                *p = end + 1;
                let lo = open as u32;
                let hi = *p as u32;
                let leaf = builder
                    .push_leaf_borrowed_string(
                        ::bbnf::runtime::tape::TapeKind::Span,
                        lo,
                        hi,
                        1u8,
                        0,
                    );
                Ok(leaf)
            }
            Some((_off, b'\\')) => parse_string_escaped(input, p, open, builder, 1u8),
            Some(_) => unreachable!(),
            None => {
                Err(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                    offset: open as u32,
                })
            }
        }
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
    pub fn parse_wrap_GoogleSheetsParser_boolean(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let mut __wrap_chosen_meta: u8 = 0;
        let first = __shape_support_GoogleSheetsParser::skip_space(input, p, state)
            .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        'try_branches: loop {
            match first {
                _ => {}
            }
            {
                let attempt_p = *p;
                let attempt_len = builder.columns_mut().len() as u32;
                match {
                    let span_lo = *p as u32;
                    match __regex_scan_GoogleSheetsParser(
                        "[tT][rR][uU][eE]",
                        input,
                        *p,
                    ) {
                        ::core::option::Option::Some(len) => {
                            *p += len as usize;
                            let __arena_off: u32 = builder.arena_mut().len() as u32;
                            builder.arena_mut().push((1u32) as u8);
                            let _ = builder
                                .push_leaf_with_arena_payload(
                                    ::bbnf::runtime::tape::TapeKind::Span,
                                    span_lo,
                                    *p as u32,
                                    2u8,
                                    0u8,
                                    __arena_off,
                                    1u32,
                                );
                            ::core::result::Result::<
                                ::bbnf::runtime::tape::TapeOffset,
                                ::bbnf::runtime::tape::DtaError,
                            >::Ok(::bbnf::runtime::tape::TapeOffset::NONE)
                        }
                        ::core::option::Option::None => {
                            ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                                offset: span_lo,
                                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                            })
                        }
                    }
                } {
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
                let attempt_len = builder.columns_mut().len() as u32;
                match {
                    let span_lo = *p as u32;
                    match __regex_scan_GoogleSheetsParser(
                        "[fF][aA][lL][sS][eE]",
                        input,
                        *p,
                    ) {
                        ::core::option::Option::Some(len) => {
                            *p += len as usize;
                            let __arena_off: u32 = builder.arena_mut().len() as u32;
                            builder.arena_mut().push((0u32) as u8);
                            let _ = builder
                                .push_leaf_with_arena_payload(
                                    ::bbnf::runtime::tape::TapeKind::Span,
                                    span_lo,
                                    *p as u32,
                                    2u8,
                                    0u8,
                                    __arena_off,
                                    1u32,
                                );
                            ::core::result::Result::<
                                ::bbnf::runtime::tape::TapeOffset,
                                ::bbnf::runtime::tape::DtaError,
                            >::Ok(::bbnf::runtime::tape::TapeOffset::NONE)
                        }
                        ::core::option::Option::None => {
                            ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                                offset: span_lo,
                                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                            })
                        }
                    }
                } {
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
            return ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                offset: *p as u32,
                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
            });
        }
        let _ = __wrap_chosen_meta;
        Ok(::bbnf::runtime::tape::TapeOffset::NONE)
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
    pub fn parse_flat_GoogleSheetsParser_error_literal(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
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
            if input.len() < end || input[at..end] != [35u8] {
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
                let first = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    )
                    .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                        offset: *p as u32,
                    })?;
                let alt_lo = *p as u32;
                let alt_child = builder.columns_mut().len() as u32;
                'try_branches: loop {
                    match first {
                        68u8 => {
                            if input.len() >= *p + 6usize
                                && input[*p..*p + 6usize]
                                    == [68u8, 73u8, 86u8, 47u8, 48u8, 33u8]
                            {
                                let at = *p;
                                let end = at + 6usize;
                                *p = end;
                                let __arena_off: u32 = builder.arena_mut().len() as u32;
                                builder.arena_mut().push((3u32) as u8);
                                let _ = builder
                                    .push_leaf_with_arena_payload(
                                        ::bbnf::runtime::tape::TapeKind::Span,
                                        at as u32,
                                        end as u32,
                                        0u8,
                                        0u8,
                                        __arena_off,
                                        1u32,
                                    );
                                break 'try_branches;
                            }
                        }
                        69u8 => {
                            if input.len() >= *p + 6usize
                                && input[*p..*p + 6usize]
                                    == [69u8, 82u8, 82u8, 79u8, 82u8, 33u8]
                            {
                                let at = *p;
                                let end = at + 6usize;
                                *p = end;
                                let __arena_off: u32 = builder.arena_mut().len() as u32;
                                builder.arena_mut().push((7u32) as u8);
                                let _ = builder
                                    .push_leaf_with_arena_payload(
                                        ::bbnf::runtime::tape::TapeKind::Span,
                                        at as u32,
                                        end as u32,
                                        0u8,
                                        0u8,
                                        __arena_off,
                                        1u32,
                                    );
                                break 'try_branches;
                            }
                        }
                        78u8 => {
                            if input.len() >= *p + 5usize
                                && input[*p..*p + 5usize] == [78u8, 85u8, 76u8, 76u8, 33u8]
                            {
                                let at = *p;
                                let end = at + 5usize;
                                *p = end;
                                let __arena_off: u32 = builder.arena_mut().len() as u32;
                                builder.arena_mut().push((4u32) as u8);
                                let _ = builder
                                    .push_leaf_with_arena_payload(
                                        ::bbnf::runtime::tape::TapeKind::Span,
                                        at as u32,
                                        end as u32,
                                        0u8,
                                        0u8,
                                        __arena_off,
                                        1u32,
                                    );
                                break 'try_branches;
                            }
                            if input.len() >= *p + 5usize
                                && input[*p..*p + 5usize] == [78u8, 65u8, 77u8, 69u8, 63u8]
                            {
                                let at = *p;
                                let end = at + 5usize;
                                *p = end;
                                let __arena_off: u32 = builder.arena_mut().len() as u32;
                                builder.arena_mut().push((5u32) as u8);
                                let _ = builder
                                    .push_leaf_with_arena_payload(
                                        ::bbnf::runtime::tape::TapeKind::Span,
                                        at as u32,
                                        end as u32,
                                        0u8,
                                        0u8,
                                        __arena_off,
                                        1u32,
                                    );
                                break 'try_branches;
                            }
                            if input.len() >= *p + 4usize
                                && input[*p..*p + 4usize] == [78u8, 85u8, 77u8, 33u8]
                            {
                                let at = *p;
                                let end = at + 4usize;
                                *p = end;
                                let __arena_off: u32 = builder.arena_mut().len() as u32;
                                builder.arena_mut().push((6u32) as u8);
                                let _ = builder
                                    .push_leaf_with_arena_payload(
                                        ::bbnf::runtime::tape::TapeKind::Span,
                                        at as u32,
                                        end as u32,
                                        0u8,
                                        0u8,
                                        __arena_off,
                                        1u32,
                                    );
                                break 'try_branches;
                            }
                            if input.len() >= *p + 3usize
                                && input[*p..*p + 3usize] == [78u8, 47u8, 65u8]
                            {
                                let at = *p;
                                let end = at + 3usize;
                                *p = end;
                                let __arena_off: u32 = builder.arena_mut().len() as u32;
                                builder.arena_mut().push((0u32) as u8);
                                let _ = builder
                                    .push_leaf_with_arena_payload(
                                        ::bbnf::runtime::tape::TapeKind::Span,
                                        at as u32,
                                        end as u32,
                                        0u8,
                                        0u8,
                                        __arena_off,
                                        1u32,
                                    );
                                break 'try_branches;
                            }
                        }
                        82u8 => {
                            if input.len() >= *p + 4usize
                                && input[*p..*p + 4usize] == [82u8, 69u8, 70u8, 33u8]
                            {
                                let at = *p;
                                let end = at + 4usize;
                                *p = end;
                                let __arena_off: u32 = builder.arena_mut().len() as u32;
                                builder.arena_mut().push((2u32) as u8);
                                let _ = builder
                                    .push_leaf_with_arena_payload(
                                        ::bbnf::runtime::tape::TapeKind::Span,
                                        at as u32,
                                        end as u32,
                                        0u8,
                                        0u8,
                                        __arena_off,
                                        1u32,
                                    );
                                break 'try_branches;
                            }
                        }
                        83u8 => {
                            if input.len() >= *p + 6usize
                                && input[*p..*p + 6usize]
                                    == [83u8, 80u8, 73u8, 76u8, 76u8, 33u8]
                            {
                                let at = *p;
                                let end = at + 6usize;
                                *p = end;
                                let __arena_off: u32 = builder.arena_mut().len() as u32;
                                builder.arena_mut().push((8u32) as u8);
                                let _ = builder
                                    .push_leaf_with_arena_payload(
                                        ::bbnf::runtime::tape::TapeKind::Span,
                                        at as u32,
                                        end as u32,
                                        0u8,
                                        0u8,
                                        __arena_off,
                                        1u32,
                                    );
                                break 'try_branches;
                            }
                        }
                        86u8 => {
                            if input.len() >= *p + 6usize
                                && input[*p..*p + 6usize]
                                    == [86u8, 65u8, 76u8, 85u8, 69u8, 33u8]
                            {
                                let at = *p;
                                let end = at + 6usize;
                                *p = end;
                                let __arena_off: u32 = builder.arena_mut().len() as u32;
                                builder.arena_mut().push((1u32) as u8);
                                let _ = builder
                                    .push_leaf_with_arena_payload(
                                        ::bbnf::runtime::tape::TapeKind::Span,
                                        at as u32,
                                        end as u32,
                                        0u8,
                                        0u8,
                                        __arena_off,
                                        1u32,
                                    );
                                break 'try_branches;
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
                let alt_hi = *p as u32;
                let __alt_off = builder
                    .begin_compound(
                        ::bbnf::runtime::tape::TapeKind::Alt,
                        alt_lo,
                        0u8,
                        0u8,
                        0u8,
                        0u16,
                    );
                builder
                    .end_compound_post_order(
                        __alt_off,
                        alt_hi,
                        ::bbnf::runtime::tape::TapeOffset(alt_child),
                    );
            }
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
    pub fn parse_wrap_GoogleSheetsParser_sheet_prefix(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let mut __wrap_chosen_meta: u8 = 0;
        let first = __shape_support_GoogleSheetsParser::skip_space(input, p, state)
            .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        'try_branches: loop {
            match first {
                _ => {}
            }
            {
                let attempt_p = *p;
                let attempt_len = builder.columns_mut().len() as u32;
                match {
                    let span_lo = *p as u32;
                    match __regex_scan_GoogleSheetsParser("'(?:[^']|'')*'!", input, *p) {
                        ::core::option::Option::Some(len) => {
                            *p += len as usize;
                            let __arena_off: u32 = builder.arena_mut().len() as u32;
                            builder.arena_mut().push((0u32) as u8);
                            let _ = builder
                                .push_leaf_with_arena_payload(
                                    ::bbnf::runtime::tape::TapeKind::KvPair,
                                    span_lo,
                                    *p as u32,
                                    4u8,
                                    0u8,
                                    __arena_off,
                                    1u32,
                                );
                            ::core::result::Result::<
                                ::bbnf::runtime::tape::TapeOffset,
                                ::bbnf::runtime::tape::DtaError,
                            >::Ok(::bbnf::runtime::tape::TapeOffset::NONE)
                        }
                        ::core::option::Option::None => {
                            ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                                offset: span_lo,
                                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                            })
                        }
                    }
                } {
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
                let attempt_len = builder.columns_mut().len() as u32;
                match {
                    let span_lo = *p as u32;
                    match __regex_scan_GoogleSheetsParser("[A-Za-z_]\\w*!", input, *p) {
                        ::core::option::Option::Some(len) => {
                            *p += len as usize;
                            let __arena_off: u32 = builder.arena_mut().len() as u32;
                            builder.arena_mut().push((1u32) as u8);
                            let _ = builder
                                .push_leaf_with_arena_payload(
                                    ::bbnf::runtime::tape::TapeKind::KvPair,
                                    span_lo,
                                    *p as u32,
                                    4u8,
                                    0u8,
                                    __arena_off,
                                    1u32,
                                );
                            ::core::result::Result::<
                                ::bbnf::runtime::tape::TapeOffset,
                                ::bbnf::runtime::tape::DtaError,
                            >::Ok(::bbnf::runtime::tape::TapeOffset::NONE)
                        }
                        ::core::option::Option::None => {
                            ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                                offset: span_lo,
                                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                            })
                        }
                    }
                } {
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
            return ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                offset: *p as u32,
                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
            });
        }
        let _ = __wrap_chosen_meta;
        Ok(::bbnf::runtime::tape::TapeOffset::NONE)
    }
    /// AW-V.W4-fix — per-grammar HRegex-shape parse function.
    ///
    /// Regex scan via the per-grammar adapter; emits a
    /// `TapeKind::Regex` leaf carrying the matched span. Decoder
    /// hooks (host_fn payloads) are wired at the dispatcher level
    /// post-scan; the raw Span-leaf path is the default.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_hregex_GoogleSheetsParser_cell_ref(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let Some(match_len) = __regex_scan_GoogleSheetsParser(
            "\\$?[A-Za-z]{1,3}\\$?\\d+",
            input,
            *p,
        ) else {
            return Err(::bbnf::runtime::tape::DtaError::Syntax {
                offset: span_lo,
                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
            });
        };
        *p += match_len as usize;
        let span_hi = *p as u32;
        let leaf_off = builder
            .push_leaf_with(
                ::bbnf::runtime::tape::TapeKind::Regex,
                span_lo,
                span_hi,
                5u8,
                0,
                ::bbnf::runtime::tape::PayloadData::None,
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
    pub fn parse_flat_GoogleSheetsParser_cell(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
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
            let iter_save_p = *p;
            let iter_save_cols = builder.columns_mut().len() as u32;
            let iter_lo = *p as u32;
            let iter_child = builder.columns_mut().len() as u32;
            let opt_attempt: ::core::result::Result<
                (),
                ::bbnf::runtime::tape::DtaError,
            > = (|| {
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_wrap_GoogleSheetsParser_sheet_prefix(input, p, state, builder)
                })?;
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
        {
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_hregex_GoogleSheetsParser_cell_ref(input, p, state, builder)
            })?;
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
                span_lo,
                6u8,
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
    pub fn parse_flat_GoogleSheetsParser_range_ref(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
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
            let iter_save_p = *p;
            let iter_save_cols = builder.columns_mut().len() as u32;
            let iter_lo = *p as u32;
            let iter_child = builder.columns_mut().len() as u32;
            let opt_attempt: ::core::result::Result<
                (),
                ::bbnf::runtime::tape::DtaError,
            > = (|| {
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_wrap_GoogleSheetsParser_sheet_prefix(input, p, state, builder)
                })?;
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
        {
            {
                let first = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    )
                    .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                        offset: *p as u32,
                    })?;
                let alt_lo = *p as u32;
                let alt_child = builder.columns_mut().len() as u32;
                'try_branches: loop {
                    match first {
                        _ => {}
                    }
                    {
                        let attempt_p = *p;
                        let attempt_len = builder.columns_mut().len() as u32;
                        match {
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
                        let span_lo = *p as u32;
                        if let ::core::option::Option::Some(match_len) = __regex_scan_GoogleSheetsParser(
                            "\\$?[A-Za-z]{1,3}",
                            input,
                            *p,
                        ) {
                            *p += match_len as usize;
                            let _ = builder
                                .push_leaf(
                                    ::bbnf::runtime::tape::TapeKind::Span,
                                    span_lo,
                                    *p as u32,
                                    0,
                                    0,
                                );
                            break 'try_branches;
                        }
                    }
                    {
                        let span_lo = *p as u32;
                        if let ::core::option::Option::Some(match_len) = __regex_scan_GoogleSheetsParser(
                            "\\$?\\d+",
                            input,
                            *p,
                        ) {
                            *p += match_len as usize;
                            let _ = builder
                                .push_leaf(
                                    ::bbnf::runtime::tape::TapeKind::Span,
                                    span_lo,
                                    *p as u32,
                                    0,
                                    0,
                                );
                            break 'try_branches;
                        }
                    }
                    return ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    });
                }
                let alt_hi = *p as u32;
                let __alt_off = builder
                    .begin_compound(
                        ::bbnf::runtime::tape::TapeKind::Alt,
                        alt_lo,
                        7u8,
                        0u8,
                        0u8,
                        0u16,
                    );
                builder
                    .end_compound_post_order(
                        __alt_off,
                        alt_hi,
                        ::bbnf::runtime::tape::TapeOffset(alt_child),
                    );
            }
        }
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [58u8] {
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
                    7u8,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
        }
        {
            {
                let first = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    )
                    .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                        offset: *p as u32,
                    })?;
                let alt_lo = *p as u32;
                let alt_child = builder.columns_mut().len() as u32;
                'try_branches: loop {
                    match first {
                        _ => {}
                    }
                    {
                        let attempt_p = *p;
                        let attempt_len = builder.columns_mut().len() as u32;
                        match {
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
                        let span_lo = *p as u32;
                        if let ::core::option::Option::Some(match_len) = __regex_scan_GoogleSheetsParser(
                            "\\$?[A-Za-z]{1,3}",
                            input,
                            *p,
                        ) {
                            *p += match_len as usize;
                            let _ = builder
                                .push_leaf(
                                    ::bbnf::runtime::tape::TapeKind::Span,
                                    span_lo,
                                    *p as u32,
                                    0,
                                    0,
                                );
                            break 'try_branches;
                        }
                    }
                    {
                        let span_lo = *p as u32;
                        if let ::core::option::Option::Some(match_len) = __regex_scan_GoogleSheetsParser(
                            "\\$?\\d+",
                            input,
                            *p,
                        ) {
                            *p += match_len as usize;
                            let _ = builder
                                .push_leaf(
                                    ::bbnf::runtime::tape::TapeKind::Span,
                                    span_lo,
                                    *p as u32,
                                    0,
                                    0,
                                );
                            break 'try_branches;
                        }
                    }
                    return ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    });
                }
                let alt_hi = *p as u32;
                let __alt_off = builder
                    .begin_compound(
                        ::bbnf::runtime::tape::TapeKind::Alt,
                        alt_lo,
                        7u8,
                        0u8,
                        0u8,
                        0u16,
                    );
                builder
                    .end_compound_post_order(
                        __alt_off,
                        alt_hi,
                        ::bbnf::runtime::tape::TapeOffset(alt_child),
                    );
            }
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
                span_lo,
                7u8,
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
    pub fn parse_wrap_GoogleSheetsParser_cell_or_range(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let mut __wrap_chosen_meta: u8 = 0;
        let first = __shape_support_GoogleSheetsParser::skip_space(input, p, state)
            .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        'try_branches: loop {
            match first {
                _ => {}
            }
            {
                let attempt_p = *p;
                let attempt_len = builder.columns_mut().len() as u32;
                match parse_flat_GoogleSheetsParser_range_ref(input, p, state, builder) {
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
                let attempt_len = builder.columns_mut().len() as u32;
                match parse_flat_GoogleSheetsParser_cell(input, p, state, builder) {
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
            return ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                offset: *p as u32,
                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
            });
        }
        let _ = __wrap_chosen_meta;
        Ok(::bbnf::runtime::tape::TapeOffset::NONE)
    }
    /// AW-V.W4-fix — per-grammar HRegex-shape parse function.
    ///
    /// Regex scan via the per-grammar adapter; emits a
    /// `TapeKind::Regex` leaf carrying the matched span. Decoder
    /// hooks (host_fn payloads) are wired at the dispatcher level
    /// post-scan; the raw Span-leaf path is the default.
    #[inline(always)]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
    pub fn parse_hregex_GoogleSheetsParser_identifier(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let Some(match_len) = __regex_scan_GoogleSheetsParser(
            "[A-Za-z_][A-Za-z0-9_.]*",
            input,
            *p,
        ) else {
            return Err(::bbnf::runtime::tape::DtaError::Syntax {
                offset: span_lo,
                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
            });
        };
        *p += match_len as usize;
        let span_hi = *p as u32;
        let leaf_off = builder
            .push_leaf_with(
                ::bbnf::runtime::tape::TapeKind::Regex,
                span_lo,
                span_hi,
                9u8,
                0,
                ::bbnf::runtime::tape::PayloadData::None,
            );
        Ok(leaf_off)
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
    pub fn parse_keyword_GoogleSheetsParser_compare_op(
        input: &[u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let _ = state;
        match first_byte {
            60u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [60u8] {
                    let span_lo = *p as u32;
                    let seq_save_cols = builder.columns_mut().len() as u32;
                    let seq_attempt: ::core::result::Result<(), ()> = (|| {
                        let at = *p;
                        let end = at + 1usize;
                        if input.len() < end || input[at..end] != [60u8] {
                            return Err(());
                        }
                        *p = end;
                        let _ = builder
                            .push_leaf_with(
                                ::bbnf::runtime::tape::TapeKind::Literal,
                                at as u32,
                                end as u32,
                                0,
                                0,
                                ::bbnf::runtime::tape::PayloadData::None,
                            );
                        {
                            let __pos_attempt: ::core::result::Result<
                                (),
                                ::bbnf::runtime::tape::DtaError,
                            > = (|| {
                                {
                                    let first = __shape_support_GoogleSheetsParser::skip_space(
                                            input,
                                            p,
                                            state,
                                        )
                                        .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                                            offset: *p as u32,
                                        })?;
                                    'try_branches: loop {
                                        match first {
                                            61u8 => {
                                                let at = *p;
                                                let end = at + 1usize;
                                                if input.len() >= end && input[at..end] == [61u8] {
                                                    *p = end;
                                                    let _ = builder
                                                        .push_leaf(
                                                            ::bbnf::runtime::tape::TapeKind::Literal,
                                                            at as u32,
                                                            end as u32,
                                                            0,
                                                            0,
                                                        );
                                                    break 'try_branches;
                                                }
                                            }
                                            62u8 => {
                                                let at = *p;
                                                let end = at + 1usize;
                                                if input.len() >= end && input[at..end] == [62u8] {
                                                    *p = end;
                                                    let _ = builder
                                                        .push_leaf(
                                                            ::bbnf::runtime::tape::TapeKind::Literal,
                                                            at as u32,
                                                            end as u32,
                                                            0,
                                                            0,
                                                        );
                                                    break 'try_branches;
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
                                ::core::result::Result::Ok(())
                            })();
                            if __pos_attempt.is_err() {
                                return ::core::result::Result::Err(());
                            }
                        }
                        Ok(())
                    })();
                    if seq_attempt.is_err() {
                        *p = span_lo as usize;
                        builder.rollback_to(seq_save_cols);
                        return Err(::bbnf::runtime::tape::DtaError::Syntax {
                            offset: span_lo,
                            failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                            failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                        });
                    }
                    let span_hi = *p as u32;
                    builder.rollback_to(seq_save_cols);
                    let off = builder
                        .push_leaf_with(
                            ::bbnf::runtime::tape::TapeKind::Span,
                            span_lo,
                            span_hi,
                            10u8,
                            0u8,
                            ::bbnf::runtime::tape::PayloadData::None,
                        );
                    return Ok(off);
                }
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [60u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    let __arena_off: u32 = builder.arena_mut().len() as u32;
                    builder.arena_mut().push((4u32) as u8);
                    let off = builder
                        .push_leaf_with_arena_payload(
                            ::bbnf::runtime::tape::TapeKind::KvPair,
                            at as u32,
                            end as u32,
                            10u8,
                            0u8,
                            __arena_off,
                            1u32,
                        );
                    return Ok(off);
                }
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            61u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [61u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    let __arena_off: u32 = builder.arena_mut().len() as u32;
                    builder.arena_mut().push((3u32) as u8);
                    let off = builder
                        .push_leaf_with_arena_payload(
                            ::bbnf::runtime::tape::TapeKind::KvPair,
                            at as u32,
                            end as u32,
                            10u8,
                            0u8,
                            __arena_off,
                            1u32,
                        );
                    return Ok(off);
                }
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            62u8 => {
                if input.len() >= *p + 2usize && input[*p..*p + 2usize] == [62u8, 61u8] {
                    let at = *p;
                    let end = at + 2usize;
                    *p = end;
                    let __arena_off: u32 = builder.arena_mut().len() as u32;
                    builder.arena_mut().push((2u32) as u8);
                    let off = builder
                        .push_leaf_with_arena_payload(
                            ::bbnf::runtime::tape::TapeKind::KvPair,
                            at as u32,
                            end as u32,
                            10u8,
                            0u8,
                            __arena_off,
                            1u32,
                        );
                    return Ok(off);
                }
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [62u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    let __arena_off: u32 = builder.arena_mut().len() as u32;
                    builder.arena_mut().push((5u32) as u8);
                    let off = builder
                        .push_leaf_with_arena_payload(
                            ::bbnf::runtime::tape::TapeKind::KvPair,
                            at as u32,
                            end as u32,
                            10u8,
                            0u8,
                            __arena_off,
                            1u32,
                        );
                    return Ok(off);
                }
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            _ => {
                Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                })
            }
        }
    }
    /// AW-V.W4.1 — per-grammar Pratt-shape parse function.
    ///
    /// Runs the operand-led shunting-yard reducer bounded by the
    /// emitted per-grammar `PRECEDENCE_LUT`. The reducer mirrors
    /// the walker's `DtaState::ShuntingYard` arm — `TapeKind::Rule`
    /// outer compound + per-op reduced binary compounds via
    /// `emit_reducer_compound`.
    ///
    /// # Emitted algorithm
    ///
    /// 1. Reserve an outer Rule compound via
    ///    [`::bbnf::runtime::tape::FusedBuilder::mark_children`] +
    ///    record the parse-open position.
    /// 2. Dispatch the leftmost operand through the grammar's
    ///    value-position dispatcher; the operand's records land
    ///    inside the outer compound's child run.
    /// 3. Loop: peek the next byte; consult `PRECEDENCE_LUT`; when
    ///    zero, break; when nonzero:
    ///    a. Reduce every top-of-op-stack entry whose precedence
    ///       exceeds the new byte's (or ties + left-assoc); each
    ///       reduce emits a `TapeKind::Rule` reducer compound via
    ///       [`::bbnf::runtime::tape::emit_reducer_compound`].
    ///    b. Emit a `TapeKind::Span` op leaf carrying the operator
    ///       byte's u8 discriminant into `pay_narrow` directly via
    ///       `push_leaf_with(InlineScalar)` (AY.W1.4 Pratt Option C
    ///       inline; bypasses the `arena_mut().push` round-trip
    ///       AX.W0a.2.l routed through).
    ///    c. Push the operator onto the local op stack with its
    ///       `(precedence, associativity, lhs_idx, lhs_span_lo)`.
    ///    d. Advance past the op bytes (1 or 2 for two-byte ops).
    ///    e. Re-dispatch the RHS operand.
    /// 4. On EOF-operator: drain the op stack — every remaining
    ///    entry reduces into a terminal compound. The final
    ///    `this_operand_root` is stamped onto the outer Rule
    ///    compound's `child_off` (overriding the default
    ///    `mark_children` index) so the cursor's pre-order walk
    ///    surfaces the reduced tree root as the compound's first
    ///    child.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
    /// recursion rationale.
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_assignments,
        unused_mut,
        unused_variables
    )]
    pub fn parse_pratt_GoogleSheetsParser_comparison_expr(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        struct LocalOpEntry {
            op_discriminant: u8,
            precedence: u8,
            associativity_is_left: bool,
            lhs_idx: u32,
            lhs_span_lo: u32,
        }
        let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        let outer_span_lo = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Rule,
                outer_span_lo,
                11u8,
                0u8,
                0u8,
                0u16,
            );
        let _operand_off = ({
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            parse_pratt_GoogleSheetsParser_concat_expr(input, p, state, builder)
        })?;
        let mut this_operand_root: u32 = _operand_off.0;
        const OP_STACK_CAP: usize = 16;
        let mut op_stack: [LocalOpEntry; OP_STACK_CAP] = ::core::array::from_fn(|_| LocalOpEntry {
            op_discriminant: 0,
            precedence: 0,
            associativity_is_left: false,
            lhs_idx: 0,
            lhs_span_lo: 0,
        });
        let mut op_stack_len: usize = 0;
        loop {
            let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
            let mut lut_byte: u8 = PRECEDENCE_LUT_comparison_expr[op_byte as usize];
            if lut_byte == 0 {
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                op_byte = input.get(*p).copied().unwrap_or(0);
                lut_byte = PRECEDENCE_LUT_comparison_expr[op_byte as usize];
            }
            let new_prec: ::core::option::Option<u8> = if lut_byte == 0 {
                ::core::option::Option::None
            } else {
                ::core::option::Option::Some(lut_byte & 0x0Fu8)
            };
            loop {
                if op_stack_len == 0 {
                    break;
                }
                let top_op = &op_stack[op_stack_len - 1];
                let should_reduce = match new_prec {
                    ::core::option::Option::None => true,
                    ::core::option::Option::Some(p_new) => {
                        top_op.precedence > p_new
                            || (top_op.precedence == p_new
                                && top_op.associativity_is_left)
                    }
                };
                if !should_reduce {
                    break;
                }
                let lhs_idx = top_op.lhs_idx;
                let lhs_span_lo = top_op.lhs_span_lo;
                let op_discriminant = top_op.op_discriminant;
                op_stack_len -= 1;
                let reducer_span_hi = *p as u32;
                let compound_idx = builder
                    .begin_compound(
                        ::bbnf::runtime::tape::TapeKind::Rule,
                        lhs_span_lo,
                        op_discriminant,
                        0u8,
                        0u8,
                        0u16,
                    );
                builder
                    .end_compound_post_order(
                        compound_idx,
                        reducer_span_hi,
                        ::bbnf::runtime::tape::TapeOffset(lhs_idx),
                    );
                this_operand_root = compound_idx;
            }
            if lut_byte == 0 {
                break;
            }
            let precedence: u8 = lut_byte & 0x0Fu8;
            let assoc_bit: u8 = (lut_byte >> 4) & 0x01u8;
            let associativity_is_left: bool = assoc_bit == 0;
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
            let op_lo: u32 = *p as u32;
            *p = (*p).saturating_add(op_width as usize);
            let op_hi: u32 = *p as u32;
            let _op_rec = builder
                .push_leaf_with(
                    ::bbnf::runtime::tape::TapeKind::Span,
                    op_lo,
                    op_hi,
                    0,
                    0,
                    ::bbnf::runtime::tape::PayloadData::InlineScalar(
                        op_discriminant as u32,
                    ),
                );
            let lhs_span_lo: u32 = if (this_operand_root as usize)
                < builder.columns().len()
            {
                builder.columns().span_lo_at(this_operand_root)
            } else {
                op_hi
            };
            debug_assert!(
                op_stack_len < OP_STACK_CAP,
                "Pratt op_stack overflow at depth {} (cap {})", op_stack_len,
                OP_STACK_CAP,
            );
            op_stack[op_stack_len] = LocalOpEntry {
                op_discriminant,
                precedence,
                associativity_is_left,
                lhs_idx: this_operand_root,
                lhs_span_lo,
            };
            op_stack_len += 1;
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            let _rhs_off = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_pratt_GoogleSheetsParser_concat_expr(input, p, state, builder)
            })?;
            this_operand_root = _rhs_off.0;
        }
        let outer_span_hi = *p as u32;
        builder.end_compound(outer_off, outer_span_hi);
        builder
            .columns_mut()
            .set_child_off_at(
                outer_off,
                ::bbnf::runtime::tape::TapeOffset(this_operand_root),
            );
        Ok(::bbnf::runtime::tape::TapeOffset(outer_off))
    }
    /// AW-V.W4.1 — per-grammar Pratt-shape parse function.
    ///
    /// Runs the operand-led shunting-yard reducer bounded by the
    /// emitted per-grammar `PRECEDENCE_LUT`. The reducer mirrors
    /// the walker's `DtaState::ShuntingYard` arm — `TapeKind::Rule`
    /// outer compound + per-op reduced binary compounds via
    /// `emit_reducer_compound`.
    ///
    /// # Emitted algorithm
    ///
    /// 1. Reserve an outer Rule compound via
    ///    [`::bbnf::runtime::tape::FusedBuilder::mark_children`] +
    ///    record the parse-open position.
    /// 2. Dispatch the leftmost operand through the grammar's
    ///    value-position dispatcher; the operand's records land
    ///    inside the outer compound's child run.
    /// 3. Loop: peek the next byte; consult `PRECEDENCE_LUT`; when
    ///    zero, break; when nonzero:
    ///    a. Reduce every top-of-op-stack entry whose precedence
    ///       exceeds the new byte's (or ties + left-assoc); each
    ///       reduce emits a `TapeKind::Rule` reducer compound via
    ///       [`::bbnf::runtime::tape::emit_reducer_compound`].
    ///    b. Emit a `TapeKind::Span` op leaf carrying the operator
    ///       byte's u8 discriminant into `pay_narrow` directly via
    ///       `push_leaf_with(InlineScalar)` (AY.W1.4 Pratt Option C
    ///       inline; bypasses the `arena_mut().push` round-trip
    ///       AX.W0a.2.l routed through).
    ///    c. Push the operator onto the local op stack with its
    ///       `(precedence, associativity, lhs_idx, lhs_span_lo)`.
    ///    d. Advance past the op bytes (1 or 2 for two-byte ops).
    ///    e. Re-dispatch the RHS operand.
    /// 4. On EOF-operator: drain the op stack — every remaining
    ///    entry reduces into a terminal compound. The final
    ///    `this_operand_root` is stamped onto the outer Rule
    ///    compound's `child_off` (overriding the default
    ///    `mark_children` index) so the cursor's pre-order walk
    ///    surfaces the reduced tree root as the compound's first
    ///    child.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
    /// recursion rationale.
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_assignments,
        unused_mut,
        unused_variables
    )]
    pub fn parse_pratt_GoogleSheetsParser_concat_expr(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        struct LocalOpEntry {
            op_discriminant: u8,
            precedence: u8,
            associativity_is_left: bool,
            lhs_idx: u32,
            lhs_span_lo: u32,
        }
        let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        let outer_span_lo = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Rule,
                outer_span_lo,
                12u8,
                0u8,
                0u8,
                0u16,
            );
        let _operand_off = ({
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            parse_pratt_GoogleSheetsParser_add_expr(input, p, state, builder)
        })?;
        let mut this_operand_root: u32 = _operand_off.0;
        const OP_STACK_CAP: usize = 16;
        let mut op_stack: [LocalOpEntry; OP_STACK_CAP] = ::core::array::from_fn(|_| LocalOpEntry {
            op_discriminant: 0,
            precedence: 0,
            associativity_is_left: false,
            lhs_idx: 0,
            lhs_span_lo: 0,
        });
        let mut op_stack_len: usize = 0;
        loop {
            let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
            let mut lut_byte: u8 = PRECEDENCE_LUT_concat_expr[op_byte as usize];
            if lut_byte == 0 {
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                op_byte = input.get(*p).copied().unwrap_or(0);
                lut_byte = PRECEDENCE_LUT_concat_expr[op_byte as usize];
            }
            let new_prec: ::core::option::Option<u8> = if lut_byte == 0 {
                ::core::option::Option::None
            } else {
                ::core::option::Option::Some(lut_byte & 0x0Fu8)
            };
            loop {
                if op_stack_len == 0 {
                    break;
                }
                let top_op = &op_stack[op_stack_len - 1];
                let should_reduce = match new_prec {
                    ::core::option::Option::None => true,
                    ::core::option::Option::Some(p_new) => {
                        top_op.precedence > p_new
                            || (top_op.precedence == p_new
                                && top_op.associativity_is_left)
                    }
                };
                if !should_reduce {
                    break;
                }
                let lhs_idx = top_op.lhs_idx;
                let lhs_span_lo = top_op.lhs_span_lo;
                let op_discriminant = top_op.op_discriminant;
                op_stack_len -= 1;
                let reducer_span_hi = *p as u32;
                let compound_idx = builder
                    .begin_compound(
                        ::bbnf::runtime::tape::TapeKind::Rule,
                        lhs_span_lo,
                        op_discriminant,
                        0u8,
                        0u8,
                        0u16,
                    );
                builder
                    .end_compound_post_order(
                        compound_idx,
                        reducer_span_hi,
                        ::bbnf::runtime::tape::TapeOffset(lhs_idx),
                    );
                this_operand_root = compound_idx;
            }
            if lut_byte == 0 {
                break;
            }
            let precedence: u8 = lut_byte & 0x0Fu8;
            let assoc_bit: u8 = (lut_byte >> 4) & 0x01u8;
            let associativity_is_left: bool = assoc_bit == 0;
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
            let op_lo: u32 = *p as u32;
            *p = (*p).saturating_add(op_width as usize);
            let op_hi: u32 = *p as u32;
            let _op_rec = builder
                .push_leaf_with(
                    ::bbnf::runtime::tape::TapeKind::Span,
                    op_lo,
                    op_hi,
                    0,
                    0,
                    ::bbnf::runtime::tape::PayloadData::InlineScalar(
                        op_discriminant as u32,
                    ),
                );
            let lhs_span_lo: u32 = if (this_operand_root as usize)
                < builder.columns().len()
            {
                builder.columns().span_lo_at(this_operand_root)
            } else {
                op_hi
            };
            debug_assert!(
                op_stack_len < OP_STACK_CAP,
                "Pratt op_stack overflow at depth {} (cap {})", op_stack_len,
                OP_STACK_CAP,
            );
            op_stack[op_stack_len] = LocalOpEntry {
                op_discriminant,
                precedence,
                associativity_is_left,
                lhs_idx: this_operand_root,
                lhs_span_lo,
            };
            op_stack_len += 1;
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            let _rhs_off = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_pratt_GoogleSheetsParser_add_expr(input, p, state, builder)
            })?;
            this_operand_root = _rhs_off.0;
        }
        let outer_span_hi = *p as u32;
        builder.end_compound(outer_off, outer_span_hi);
        builder
            .columns_mut()
            .set_child_off_at(
                outer_off,
                ::bbnf::runtime::tape::TapeOffset(this_operand_root),
            );
        Ok(::bbnf::runtime::tape::TapeOffset(outer_off))
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
    pub fn parse_keyword_GoogleSheetsParser_add_op(
        input: &[u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let _ = state;
        match first_byte {
            43u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [43u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    let __arena_off: u32 = builder.arena_mut().len() as u32;
                    builder.arena_mut().push((0u32) as u8);
                    let off = builder
                        .push_leaf_with_arena_payload(
                            ::bbnf::runtime::tape::TapeKind::KvPair,
                            at as u32,
                            end as u32,
                            13u8,
                            0u8,
                            __arena_off,
                            1u32,
                        );
                    return Ok(off);
                }
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            45u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [45u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    let __arena_off: u32 = builder.arena_mut().len() as u32;
                    builder.arena_mut().push((1u32) as u8);
                    let off = builder
                        .push_leaf_with_arena_payload(
                            ::bbnf::runtime::tape::TapeKind::KvPair,
                            at as u32,
                            end as u32,
                            13u8,
                            0u8,
                            __arena_off,
                            1u32,
                        );
                    return Ok(off);
                }
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            _ => {
                Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                })
            }
        }
    }
    /// AW-V.W4.1 — per-grammar Pratt-shape parse function.
    ///
    /// Runs the operand-led shunting-yard reducer bounded by the
    /// emitted per-grammar `PRECEDENCE_LUT`. The reducer mirrors
    /// the walker's `DtaState::ShuntingYard` arm — `TapeKind::Rule`
    /// outer compound + per-op reduced binary compounds via
    /// `emit_reducer_compound`.
    ///
    /// # Emitted algorithm
    ///
    /// 1. Reserve an outer Rule compound via
    ///    [`::bbnf::runtime::tape::FusedBuilder::mark_children`] +
    ///    record the parse-open position.
    /// 2. Dispatch the leftmost operand through the grammar's
    ///    value-position dispatcher; the operand's records land
    ///    inside the outer compound's child run.
    /// 3. Loop: peek the next byte; consult `PRECEDENCE_LUT`; when
    ///    zero, break; when nonzero:
    ///    a. Reduce every top-of-op-stack entry whose precedence
    ///       exceeds the new byte's (or ties + left-assoc); each
    ///       reduce emits a `TapeKind::Rule` reducer compound via
    ///       [`::bbnf::runtime::tape::emit_reducer_compound`].
    ///    b. Emit a `TapeKind::Span` op leaf carrying the operator
    ///       byte's u8 discriminant into `pay_narrow` directly via
    ///       `push_leaf_with(InlineScalar)` (AY.W1.4 Pratt Option C
    ///       inline; bypasses the `arena_mut().push` round-trip
    ///       AX.W0a.2.l routed through).
    ///    c. Push the operator onto the local op stack with its
    ///       `(precedence, associativity, lhs_idx, lhs_span_lo)`.
    ///    d. Advance past the op bytes (1 or 2 for two-byte ops).
    ///    e. Re-dispatch the RHS operand.
    /// 4. On EOF-operator: drain the op stack — every remaining
    ///    entry reduces into a terminal compound. The final
    ///    `this_operand_root` is stamped onto the outer Rule
    ///    compound's `child_off` (overriding the default
    ///    `mark_children` index) so the cursor's pre-order walk
    ///    surfaces the reduced tree root as the compound's first
    ///    child.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
    /// recursion rationale.
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_assignments,
        unused_mut,
        unused_variables
    )]
    pub fn parse_pratt_GoogleSheetsParser_add_expr(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        struct LocalOpEntry {
            op_discriminant: u8,
            precedence: u8,
            associativity_is_left: bool,
            lhs_idx: u32,
            lhs_span_lo: u32,
        }
        let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        let outer_span_lo = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Rule,
                outer_span_lo,
                14u8,
                0u8,
                0u8,
                0u16,
            );
        let _operand_off = ({
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            parse_pratt_GoogleSheetsParser_mul_expr(input, p, state, builder)
        })?;
        let mut this_operand_root: u32 = _operand_off.0;
        const OP_STACK_CAP: usize = 16;
        let mut op_stack: [LocalOpEntry; OP_STACK_CAP] = ::core::array::from_fn(|_| LocalOpEntry {
            op_discriminant: 0,
            precedence: 0,
            associativity_is_left: false,
            lhs_idx: 0,
            lhs_span_lo: 0,
        });
        let mut op_stack_len: usize = 0;
        loop {
            let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
            let mut lut_byte: u8 = PRECEDENCE_LUT_add_expr[op_byte as usize];
            if lut_byte == 0 {
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                op_byte = input.get(*p).copied().unwrap_or(0);
                lut_byte = PRECEDENCE_LUT_add_expr[op_byte as usize];
            }
            let new_prec: ::core::option::Option<u8> = if lut_byte == 0 {
                ::core::option::Option::None
            } else {
                ::core::option::Option::Some(lut_byte & 0x0Fu8)
            };
            loop {
                if op_stack_len == 0 {
                    break;
                }
                let top_op = &op_stack[op_stack_len - 1];
                let should_reduce = match new_prec {
                    ::core::option::Option::None => true,
                    ::core::option::Option::Some(p_new) => {
                        top_op.precedence > p_new
                            || (top_op.precedence == p_new
                                && top_op.associativity_is_left)
                    }
                };
                if !should_reduce {
                    break;
                }
                let lhs_idx = top_op.lhs_idx;
                let lhs_span_lo = top_op.lhs_span_lo;
                let op_discriminant = top_op.op_discriminant;
                op_stack_len -= 1;
                let reducer_span_hi = *p as u32;
                let compound_idx = builder
                    .begin_compound(
                        ::bbnf::runtime::tape::TapeKind::Rule,
                        lhs_span_lo,
                        op_discriminant,
                        0u8,
                        0u8,
                        0u16,
                    );
                builder
                    .end_compound_post_order(
                        compound_idx,
                        reducer_span_hi,
                        ::bbnf::runtime::tape::TapeOffset(lhs_idx),
                    );
                this_operand_root = compound_idx;
            }
            if lut_byte == 0 {
                break;
            }
            let precedence: u8 = lut_byte & 0x0Fu8;
            let assoc_bit: u8 = (lut_byte >> 4) & 0x01u8;
            let associativity_is_left: bool = assoc_bit == 0;
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
            let op_lo: u32 = *p as u32;
            *p = (*p).saturating_add(op_width as usize);
            let op_hi: u32 = *p as u32;
            let _op_rec = builder
                .push_leaf_with(
                    ::bbnf::runtime::tape::TapeKind::Span,
                    op_lo,
                    op_hi,
                    0,
                    0,
                    ::bbnf::runtime::tape::PayloadData::InlineScalar(
                        op_discriminant as u32,
                    ),
                );
            let lhs_span_lo: u32 = if (this_operand_root as usize)
                < builder.columns().len()
            {
                builder.columns().span_lo_at(this_operand_root)
            } else {
                op_hi
            };
            debug_assert!(
                op_stack_len < OP_STACK_CAP,
                "Pratt op_stack overflow at depth {} (cap {})", op_stack_len,
                OP_STACK_CAP,
            );
            op_stack[op_stack_len] = LocalOpEntry {
                op_discriminant,
                precedence,
                associativity_is_left,
                lhs_idx: this_operand_root,
                lhs_span_lo,
            };
            op_stack_len += 1;
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            let _rhs_off = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_pratt_GoogleSheetsParser_mul_expr(input, p, state, builder)
            })?;
            this_operand_root = _rhs_off.0;
        }
        let outer_span_hi = *p as u32;
        builder.end_compound(outer_off, outer_span_hi);
        builder
            .columns_mut()
            .set_child_off_at(
                outer_off,
                ::bbnf::runtime::tape::TapeOffset(this_operand_root),
            );
        Ok(::bbnf::runtime::tape::TapeOffset(outer_off))
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
    pub fn parse_keyword_GoogleSheetsParser_mul_op(
        input: &[u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let _ = state;
        match first_byte {
            42u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [42u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    let __arena_off: u32 = builder.arena_mut().len() as u32;
                    builder.arena_mut().push((0u32) as u8);
                    let off = builder
                        .push_leaf_with_arena_payload(
                            ::bbnf::runtime::tape::TapeKind::KvPair,
                            at as u32,
                            end as u32,
                            15u8,
                            0u8,
                            __arena_off,
                            1u32,
                        );
                    return Ok(off);
                }
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            47u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [47u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    let __arena_off: u32 = builder.arena_mut().len() as u32;
                    builder.arena_mut().push((1u32) as u8);
                    let off = builder
                        .push_leaf_with_arena_payload(
                            ::bbnf::runtime::tape::TapeKind::KvPair,
                            at as u32,
                            end as u32,
                            15u8,
                            0u8,
                            __arena_off,
                            1u32,
                        );
                    return Ok(off);
                }
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            _ => {
                Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                })
            }
        }
    }
    /// AW-V.W4.1 — per-grammar Pratt-shape parse function.
    ///
    /// Runs the operand-led shunting-yard reducer bounded by the
    /// emitted per-grammar `PRECEDENCE_LUT`. The reducer mirrors
    /// the walker's `DtaState::ShuntingYard` arm — `TapeKind::Rule`
    /// outer compound + per-op reduced binary compounds via
    /// `emit_reducer_compound`.
    ///
    /// # Emitted algorithm
    ///
    /// 1. Reserve an outer Rule compound via
    ///    [`::bbnf::runtime::tape::FusedBuilder::mark_children`] +
    ///    record the parse-open position.
    /// 2. Dispatch the leftmost operand through the grammar's
    ///    value-position dispatcher; the operand's records land
    ///    inside the outer compound's child run.
    /// 3. Loop: peek the next byte; consult `PRECEDENCE_LUT`; when
    ///    zero, break; when nonzero:
    ///    a. Reduce every top-of-op-stack entry whose precedence
    ///       exceeds the new byte's (or ties + left-assoc); each
    ///       reduce emits a `TapeKind::Rule` reducer compound via
    ///       [`::bbnf::runtime::tape::emit_reducer_compound`].
    ///    b. Emit a `TapeKind::Span` op leaf carrying the operator
    ///       byte's u8 discriminant into `pay_narrow` directly via
    ///       `push_leaf_with(InlineScalar)` (AY.W1.4 Pratt Option C
    ///       inline; bypasses the `arena_mut().push` round-trip
    ///       AX.W0a.2.l routed through).
    ///    c. Push the operator onto the local op stack with its
    ///       `(precedence, associativity, lhs_idx, lhs_span_lo)`.
    ///    d. Advance past the op bytes (1 or 2 for two-byte ops).
    ///    e. Re-dispatch the RHS operand.
    /// 4. On EOF-operator: drain the op stack — every remaining
    ///    entry reduces into a terminal compound. The final
    ///    `this_operand_root` is stamped onto the outer Rule
    ///    compound's `child_off` (overriding the default
    ///    `mark_children` index) so the cursor's pre-order walk
    ///    surfaces the reduced tree root as the compound's first
    ///    child.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
    /// recursion rationale.
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_assignments,
        unused_mut,
        unused_variables
    )]
    pub fn parse_pratt_GoogleSheetsParser_mul_expr(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        struct LocalOpEntry {
            op_discriminant: u8,
            precedence: u8,
            associativity_is_left: bool,
            lhs_idx: u32,
            lhs_span_lo: u32,
        }
        let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        let outer_span_lo = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Rule,
                outer_span_lo,
                16u8,
                0u8,
                0u8,
                0u16,
            );
        let _operand_off = ({
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            parse_pratt_GoogleSheetsParser_exp_expr(input, p, state, builder)
        })?;
        let mut this_operand_root: u32 = _operand_off.0;
        const OP_STACK_CAP: usize = 16;
        let mut op_stack: [LocalOpEntry; OP_STACK_CAP] = ::core::array::from_fn(|_| LocalOpEntry {
            op_discriminant: 0,
            precedence: 0,
            associativity_is_left: false,
            lhs_idx: 0,
            lhs_span_lo: 0,
        });
        let mut op_stack_len: usize = 0;
        loop {
            let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
            let mut lut_byte: u8 = PRECEDENCE_LUT_mul_expr[op_byte as usize];
            if lut_byte == 0 {
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                op_byte = input.get(*p).copied().unwrap_or(0);
                lut_byte = PRECEDENCE_LUT_mul_expr[op_byte as usize];
            }
            let new_prec: ::core::option::Option<u8> = if lut_byte == 0 {
                ::core::option::Option::None
            } else {
                ::core::option::Option::Some(lut_byte & 0x0Fu8)
            };
            loop {
                if op_stack_len == 0 {
                    break;
                }
                let top_op = &op_stack[op_stack_len - 1];
                let should_reduce = match new_prec {
                    ::core::option::Option::None => true,
                    ::core::option::Option::Some(p_new) => {
                        top_op.precedence > p_new
                            || (top_op.precedence == p_new
                                && top_op.associativity_is_left)
                    }
                };
                if !should_reduce {
                    break;
                }
                let lhs_idx = top_op.lhs_idx;
                let lhs_span_lo = top_op.lhs_span_lo;
                let op_discriminant = top_op.op_discriminant;
                op_stack_len -= 1;
                let reducer_span_hi = *p as u32;
                let compound_idx = builder
                    .begin_compound(
                        ::bbnf::runtime::tape::TapeKind::Rule,
                        lhs_span_lo,
                        op_discriminant,
                        0u8,
                        0u8,
                        0u16,
                    );
                builder
                    .end_compound_post_order(
                        compound_idx,
                        reducer_span_hi,
                        ::bbnf::runtime::tape::TapeOffset(lhs_idx),
                    );
                this_operand_root = compound_idx;
            }
            if lut_byte == 0 {
                break;
            }
            let precedence: u8 = lut_byte & 0x0Fu8;
            let assoc_bit: u8 = (lut_byte >> 4) & 0x01u8;
            let associativity_is_left: bool = assoc_bit == 0;
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
            let op_lo: u32 = *p as u32;
            *p = (*p).saturating_add(op_width as usize);
            let op_hi: u32 = *p as u32;
            let _op_rec = builder
                .push_leaf_with(
                    ::bbnf::runtime::tape::TapeKind::Span,
                    op_lo,
                    op_hi,
                    0,
                    0,
                    ::bbnf::runtime::tape::PayloadData::InlineScalar(
                        op_discriminant as u32,
                    ),
                );
            let lhs_span_lo: u32 = if (this_operand_root as usize)
                < builder.columns().len()
            {
                builder.columns().span_lo_at(this_operand_root)
            } else {
                op_hi
            };
            debug_assert!(
                op_stack_len < OP_STACK_CAP,
                "Pratt op_stack overflow at depth {} (cap {})", op_stack_len,
                OP_STACK_CAP,
            );
            op_stack[op_stack_len] = LocalOpEntry {
                op_discriminant,
                precedence,
                associativity_is_left,
                lhs_idx: this_operand_root,
                lhs_span_lo,
            };
            op_stack_len += 1;
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            let _rhs_off = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_pratt_GoogleSheetsParser_exp_expr(input, p, state, builder)
            })?;
            this_operand_root = _rhs_off.0;
        }
        let outer_span_hi = *p as u32;
        builder.end_compound(outer_off, outer_span_hi);
        builder
            .columns_mut()
            .set_child_off_at(
                outer_off,
                ::bbnf::runtime::tape::TapeOffset(this_operand_root),
            );
        Ok(::bbnf::runtime::tape::TapeOffset(outer_off))
    }
    /// AW-V.W4.1 — per-grammar Pratt-shape parse function.
    ///
    /// Runs the operand-led shunting-yard reducer bounded by the
    /// emitted per-grammar `PRECEDENCE_LUT`. The reducer mirrors
    /// the walker's `DtaState::ShuntingYard` arm — `TapeKind::Rule`
    /// outer compound + per-op reduced binary compounds via
    /// `emit_reducer_compound`.
    ///
    /// # Emitted algorithm
    ///
    /// 1. Reserve an outer Rule compound via
    ///    [`::bbnf::runtime::tape::FusedBuilder::mark_children`] +
    ///    record the parse-open position.
    /// 2. Dispatch the leftmost operand through the grammar's
    ///    value-position dispatcher; the operand's records land
    ///    inside the outer compound's child run.
    /// 3. Loop: peek the next byte; consult `PRECEDENCE_LUT`; when
    ///    zero, break; when nonzero:
    ///    a. Reduce every top-of-op-stack entry whose precedence
    ///       exceeds the new byte's (or ties + left-assoc); each
    ///       reduce emits a `TapeKind::Rule` reducer compound via
    ///       [`::bbnf::runtime::tape::emit_reducer_compound`].
    ///    b. Emit a `TapeKind::Span` op leaf carrying the operator
    ///       byte's u8 discriminant into `pay_narrow` directly via
    ///       `push_leaf_with(InlineScalar)` (AY.W1.4 Pratt Option C
    ///       inline; bypasses the `arena_mut().push` round-trip
    ///       AX.W0a.2.l routed through).
    ///    c. Push the operator onto the local op stack with its
    ///       `(precedence, associativity, lhs_idx, lhs_span_lo)`.
    ///    d. Advance past the op bytes (1 or 2 for two-byte ops).
    ///    e. Re-dispatch the RHS operand.
    /// 4. On EOF-operator: drain the op stack — every remaining
    ///    entry reduces into a terminal compound. The final
    ///    `this_operand_root` is stamped onto the outer Rule
    ///    compound's `child_off` (overriding the default
    ///    `mark_children` index) so the cursor's pre-order walk
    ///    surfaces the reduced tree root as the compound's first
    ///    child.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
    /// recursion rationale.
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_assignments,
        unused_mut,
        unused_variables
    )]
    pub fn parse_pratt_GoogleSheetsParser_exp_expr(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        struct LocalOpEntry {
            op_discriminant: u8,
            precedence: u8,
            associativity_is_left: bool,
            lhs_idx: u32,
            lhs_span_lo: u32,
        }
        let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        let outer_span_lo = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Rule,
                outer_span_lo,
                17u8,
                0u8,
                0u8,
                0u16,
            );
        let _operand_off = ({
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            parse_flat_GoogleSheetsParser_unary_expr(input, p, state, builder)
        })?;
        let mut this_operand_root: u32 = _operand_off.0;
        const OP_STACK_CAP: usize = 16;
        let mut op_stack: [LocalOpEntry; OP_STACK_CAP] = ::core::array::from_fn(|_| LocalOpEntry {
            op_discriminant: 0,
            precedence: 0,
            associativity_is_left: false,
            lhs_idx: 0,
            lhs_span_lo: 0,
        });
        let mut op_stack_len: usize = 0;
        loop {
            let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
            let mut lut_byte: u8 = PRECEDENCE_LUT_exp_expr[op_byte as usize];
            if lut_byte == 0 {
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                op_byte = input.get(*p).copied().unwrap_or(0);
                lut_byte = PRECEDENCE_LUT_exp_expr[op_byte as usize];
            }
            let new_prec: ::core::option::Option<u8> = if lut_byte == 0 {
                ::core::option::Option::None
            } else {
                ::core::option::Option::Some(lut_byte & 0x0Fu8)
            };
            loop {
                if op_stack_len == 0 {
                    break;
                }
                let top_op = &op_stack[op_stack_len - 1];
                let should_reduce = match new_prec {
                    ::core::option::Option::None => true,
                    ::core::option::Option::Some(p_new) => {
                        top_op.precedence > p_new
                            || (top_op.precedence == p_new
                                && top_op.associativity_is_left)
                    }
                };
                if !should_reduce {
                    break;
                }
                let lhs_idx = top_op.lhs_idx;
                let lhs_span_lo = top_op.lhs_span_lo;
                let op_discriminant = top_op.op_discriminant;
                op_stack_len -= 1;
                let reducer_span_hi = *p as u32;
                let compound_idx = builder
                    .begin_compound(
                        ::bbnf::runtime::tape::TapeKind::Rule,
                        lhs_span_lo,
                        op_discriminant,
                        0u8,
                        0u8,
                        0u16,
                    );
                builder
                    .end_compound_post_order(
                        compound_idx,
                        reducer_span_hi,
                        ::bbnf::runtime::tape::TapeOffset(lhs_idx),
                    );
                this_operand_root = compound_idx;
            }
            if lut_byte == 0 {
                break;
            }
            let precedence: u8 = lut_byte & 0x0Fu8;
            let assoc_bit: u8 = (lut_byte >> 4) & 0x01u8;
            let associativity_is_left: bool = assoc_bit == 0;
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
            let op_lo: u32 = *p as u32;
            *p = (*p).saturating_add(op_width as usize);
            let op_hi: u32 = *p as u32;
            let _op_rec = builder
                .push_leaf_with(
                    ::bbnf::runtime::tape::TapeKind::Span,
                    op_lo,
                    op_hi,
                    0,
                    0,
                    ::bbnf::runtime::tape::PayloadData::InlineScalar(
                        op_discriminant as u32,
                    ),
                );
            let lhs_span_lo: u32 = if (this_operand_root as usize)
                < builder.columns().len()
            {
                builder.columns().span_lo_at(this_operand_root)
            } else {
                op_hi
            };
            debug_assert!(
                op_stack_len < OP_STACK_CAP,
                "Pratt op_stack overflow at depth {} (cap {})", op_stack_len,
                OP_STACK_CAP,
            );
            op_stack[op_stack_len] = LocalOpEntry {
                op_discriminant,
                precedence,
                associativity_is_left,
                lhs_idx: this_operand_root,
                lhs_span_lo,
            };
            op_stack_len += 1;
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            let _rhs_off = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_flat_GoogleSheetsParser_unary_expr(input, p, state, builder)
            })?;
            this_operand_root = _rhs_off.0;
        }
        let outer_span_hi = *p as u32;
        builder.end_compound(outer_off, outer_span_hi);
        builder
            .columns_mut()
            .set_child_off_at(
                outer_off,
                ::bbnf::runtime::tape::TapeOffset(this_operand_root),
            );
        Ok(::bbnf::runtime::tape::TapeOffset(outer_off))
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
    pub fn parse_keyword_GoogleSheetsParser_unary_prefix(
        input: &[u8],
        p: &mut usize,
        first_byte: u8,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let _ = state;
        match first_byte {
            43u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [43u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    let __arena_off: u32 = builder.arena_mut().len() as u32;
                    builder.arena_mut().push((0u32) as u8);
                    let off = builder
                        .push_leaf_with_arena_payload(
                            ::bbnf::runtime::tape::TapeKind::KvPair,
                            at as u32,
                            end as u32,
                            18u8,
                            0u8,
                            __arena_off,
                            1u32,
                        );
                    return Ok(off);
                }
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            45u8 => {
                if input.len() >= *p + 1usize && input[*p..*p + 1usize] == [45u8] {
                    let at = *p;
                    let end = at + 1usize;
                    *p = end;
                    let __arena_off: u32 = builder.arena_mut().len() as u32;
                    builder.arena_mut().push((1u32) as u8);
                    let off = builder
                        .push_leaf_with_arena_payload(
                            ::bbnf::runtime::tape::TapeKind::KvPair,
                            at as u32,
                            end as u32,
                            18u8,
                            0u8,
                            __arena_off,
                            1u32,
                        );
                    return Ok(off);
                }
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            _ => {
                Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
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
    pub fn parse_flat_GoogleSheetsParser_unary_expr(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
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
                    let _ = ({
                        let __first = __shape_support_GoogleSheetsParser::skip_space(
                                input,
                                p,
                                state,
                            )
                            .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                                offset: *p as u32,
                            })?;
                        parse_keyword_GoogleSheetsParser_unary_prefix(
                            input,
                            p,
                            __first,
                            state,
                            builder,
                        )
                    })?;
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
        {
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_flat_GoogleSheetsParser_postfix_expr(input, p, state, builder)
            })?;
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
                span_lo,
                19u8,
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
    pub fn parse_flat_GoogleSheetsParser_postfix_expr(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.columns_mut().len() as u32;
        {
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_wrap_GoogleSheetsParser_primary(input, p, state, builder)
            })?;
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
                    let at = *p;
                    let end = at + 1usize;
                    if input.len() < end || input[at..end] != [37u8] {
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
                            20u8,
                            0,
                            ::bbnf::runtime::tape::PayloadData::None,
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
                20u8,
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
    pub fn parse_wrap_GoogleSheetsParser_primary(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let mut __wrap_chosen_meta: u8 = 0;
        let first = __shape_support_GoogleSheetsParser::skip_space(input, p, state)
            .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        'try_branches: loop {
            match first {
                34u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.columns_mut().len() as u32;
                    match parse_string_GoogleSheetsParser_string(
                        input,
                        p,
                        state,
                        builder,
                    ) {
                        Ok(_) => {
                            __wrap_chosen_meta = 7u8;
                            break 'try_branches;
                        }
                        Err(_) => {
                            *p = attempt_p;
                            builder.rollback_to(attempt_len);
                        }
                    }
                }
                35u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.columns_mut().len() as u32;
                    match parse_flat_GoogleSheetsParser_error_literal(
                        input,
                        p,
                        state,
                        builder,
                    ) {
                        Ok(_) => {
                            __wrap_chosen_meta = 8u8;
                            break 'try_branches;
                        }
                        Err(_) => {
                            *p = attempt_p;
                            builder.rollback_to(attempt_len);
                        }
                    }
                }
                40u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.columns_mut().len() as u32;
                    match parse_flat_GoogleSheetsParser_paren_expr(
                        input,
                        p,
                        state,
                        builder,
                    ) {
                        Ok(_) => {
                            __wrap_chosen_meta = 10u8;
                            break 'try_branches;
                        }
                        Err(_) => {
                            *p = attempt_p;
                            builder.rollback_to(attempt_len);
                        }
                    }
                }
                46u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.columns_mut().len() as u32;
                    match parse_hregex_GoogleSheetsParser_number(
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
                48u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.columns_mut().len() as u32;
                    match parse_hregex_GoogleSheetsParser_number(
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
                49u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.columns_mut().len() as u32;
                    match parse_hregex_GoogleSheetsParser_number(
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
                50u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.columns_mut().len() as u32;
                    match parse_hregex_GoogleSheetsParser_number(
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
                51u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.columns_mut().len() as u32;
                    match parse_hregex_GoogleSheetsParser_number(
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
                52u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.columns_mut().len() as u32;
                    match parse_hregex_GoogleSheetsParser_number(
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
                53u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.columns_mut().len() as u32;
                    match parse_hregex_GoogleSheetsParser_number(
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
                54u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.columns_mut().len() as u32;
                    match parse_hregex_GoogleSheetsParser_number(
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
                55u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.columns_mut().len() as u32;
                    match parse_hregex_GoogleSheetsParser_number(
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
                56u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.columns_mut().len() as u32;
                    match parse_hregex_GoogleSheetsParser_number(
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
                57u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.columns_mut().len() as u32;
                    match parse_hregex_GoogleSheetsParser_number(
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
                70u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.columns_mut().len() as u32;
                    match parse_wrap_GoogleSheetsParser_boolean(
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
                76u8 => {
                    {
                        let attempt_p = *p;
                        let attempt_len = builder.columns_mut().len() as u32;
                        match parse_arglist_GoogleSheetsParser_let_call(
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
                        let attempt_len = builder.columns_mut().len() as u32;
                        match parse_arglist_GoogleSheetsParser_lambda_call(
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
                }
                84u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.columns_mut().len() as u32;
                    match parse_wrap_GoogleSheetsParser_boolean(
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
                102u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.columns_mut().len() as u32;
                    match parse_wrap_GoogleSheetsParser_boolean(
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
                108u8 => {
                    {
                        let attempt_p = *p;
                        let attempt_len = builder.columns_mut().len() as u32;
                        match parse_arglist_GoogleSheetsParser_let_call(
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
                        let attempt_len = builder.columns_mut().len() as u32;
                        match parse_arglist_GoogleSheetsParser_lambda_call(
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
                }
                116u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.columns_mut().len() as u32;
                    match parse_wrap_GoogleSheetsParser_boolean(
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
                123u8 => {
                    let attempt_p = *p;
                    let attempt_len = builder.columns_mut().len() as u32;
                    match parse_flat_GoogleSheetsParser_array_literal(
                        input,
                        p,
                        state,
                        builder,
                    ) {
                        Ok(_) => {
                            __wrap_chosen_meta = 9u8;
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
                let attempt_len = builder.columns_mut().len() as u32;
                match parse_arglist_GoogleSheetsParser_func_call(
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
                let attempt_len = builder.columns_mut().len() as u32;
                match parse_wrap_GoogleSheetsParser_cell_or_range(
                    input,
                    p,
                    state,
                    builder,
                ) {
                    Ok(_) => {
                        __wrap_chosen_meta = 5u8;
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
                let attempt_len = builder.columns_mut().len() as u32;
                match parse_hregex_GoogleSheetsParser_identifier(
                    input,
                    p,
                    state,
                    builder,
                ) {
                    Ok(_) => {
                        __wrap_chosen_meta = 6u8;
                        break 'try_branches;
                    }
                    Err(_) => {
                        *p = attempt_p;
                        builder.rollback_to(attempt_len);
                    }
                }
            }
            return ::core::result::Result::Err(::bbnf::runtime::tape::DtaError::Syntax {
                offset: *p as u32,
                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
            });
        }
        let _ = __wrap_chosen_meta;
        Ok(::bbnf::runtime::tape::TapeOffset::NONE)
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
    pub fn parse_flat_GoogleSheetsParser_paren_expr(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
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
            if input.len() < end || input[at..end] != [40u8] {
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
                    22u8,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
        }
        {
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_pratt_GoogleSheetsParser_comparison_expr(input, p, state, builder)
            })?;
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        }
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [41u8] {
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
                    22u8,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
                span_lo,
                22u8,
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
    pub fn parse_flat_GoogleSheetsParser_func_open(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.columns_mut().len() as u32;
        {
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_hregex_GoogleSheetsParser_identifier(input, p, state, builder)
            })?;
        }
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [40u8] {
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
                    23u8,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
                span_lo,
                23u8,
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
    pub fn parse_flat_GoogleSheetsParser_arg(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
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
            let iter_save_p = *p;
            let iter_save_cols = builder.columns_mut().len() as u32;
            let iter_lo = *p as u32;
            let iter_child = builder.columns_mut().len() as u32;
            let opt_attempt: ::core::result::Result<
                (),
                ::bbnf::runtime::tape::DtaError,
            > = (|| {
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_pratt_GoogleSheetsParser_comparison_expr(
                        input,
                        p,
                        state,
                        builder,
                    )
                })?;
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
                24u8,
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
    pub fn parse_flat_GoogleSheetsParser_func_args(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
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
                    let _ = ({
                        let _ = __shape_support_GoogleSheetsParser::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_flat_GoogleSheetsParser_arg(input, p, state, builder)
                    })?;
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
                        let _ = __shape_support_GoogleSheetsParser::skip_space(
                            input,
                            p,
                            state,
                        );
                        let at = *p;
                        let end = at + 1usize;
                        if input.len() < end || input[at..end] != [44u8] {
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
                                25u8,
                                0,
                                ::bbnf::runtime::tape::PayloadData::None,
                            );
                        let _ = __shape_support_GoogleSheetsParser::skip_space(
                            input,
                            p,
                            state,
                        );
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
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
                span_lo,
                25u8,
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
    /// AW-V.W4-fix — per-grammar ArgList-shape parse function.
    ///
    /// Emits one outer Rule compound over the whole call site.
    /// Head (Literal / Regex / Ref) + optional `(` + body arg
    /// positions (dispatched through the grammar's value-
    /// dispatcher) + `)` literal.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
    /// recursion rationale (see `flat.rs`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_arglist_GoogleSheetsParser_func_call(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.columns_mut().len() as u32;
        {
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_flat_GoogleSheetsParser_func_open(input, p, state, builder)
            })?;
        }
        {
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            let save_p = *p;
            let iter_save_cols = builder.columns_mut().len() as u32;
            let iter_lo = *p as u32;
            let iter_child = builder.columns_mut().len() as u32;
            let attempt = (|| -> ::core::result::Result<
                (),
                ::bbnf::runtime::tape::DtaError,
            > {
                let _ = ({
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
                    );
                    parse_flat_GoogleSheetsParser_func_args(input, p, state, builder)
                })?;
                Ok(())
            })();
            if attempt.is_err() {
                *p = save_p;
                builder.rollback_to(iter_save_cols);
            } else {
                let iter_hi = *p as u32;
                let __iter_off = builder
                    .begin_compound(
                        ::bbnf::runtime::tape::TapeKind::Seq,
                        iter_lo,
                        0,
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
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        }
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [41u8] {
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
                    26u8,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Rule,
                span_lo,
                26u8,
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
    pub fn parse_flat_GoogleSheetsParser_let_binding(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.columns_mut().len() as u32;
        {
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_pratt_GoogleSheetsParser_comparison_expr(input, p, state, builder)
            })?;
        }
        {
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [44u8] {
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
                    27u8,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        }
        {
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_pratt_GoogleSheetsParser_comparison_expr(input, p, state, builder)
            })?;
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
                span_lo,
                27u8,
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
    pub fn parse_flat_GoogleSheetsParser_let_args(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
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
                            28u8,
                            0,
                            ::bbnf::runtime::tape::PayloadData::None,
                        );
                    let _ = __shape_support_GoogleSheetsParser::skip_space(
                        input,
                        p,
                        state,
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
        {
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_pratt_GoogleSheetsParser_comparison_expr(input, p, state, builder)
            })?;
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
                span_lo,
                28u8,
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
    /// AW-V.W4-fix — per-grammar ArgList-shape parse function.
    ///
    /// Emits one outer Rule compound over the whole call site.
    /// Head (Literal / Regex / Ref) + optional `(` + body arg
    /// positions (dispatched through the grammar's value-
    /// dispatcher) + `)` literal.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
    /// recursion rationale (see `flat.rs`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_arglist_GoogleSheetsParser_let_call(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.columns_mut().len() as u32;
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_GoogleSheetsParser(
                    "[lL][eE][tT]\\(",
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
                        29u8,
                        0,
                        ::bbnf::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_flat_GoogleSheetsParser_let_args(input, p, state, builder)
            })?;
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        }
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [41u8] {
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
                    29u8,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Rule,
                span_lo,
                29u8,
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
    pub fn parse_flat_GoogleSheetsParser_lambda_params(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
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
                    let _ = ({
                        let _ = __shape_support_GoogleSheetsParser::skip_space(
                            input,
                            p,
                            state,
                        );
                        parse_pratt_GoogleSheetsParser_comparison_expr(
                            input,
                            p,
                            state,
                            builder,
                        )
                    })?;
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
                        let _ = __shape_support_GoogleSheetsParser::skip_space(
                            input,
                            p,
                            state,
                        );
                        let at = *p;
                        let end = at + 1usize;
                        if input.len() < end || input[at..end] != [44u8] {
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
                                30u8,
                                0,
                                ::bbnf::runtime::tape::PayloadData::None,
                            );
                        let _ = __shape_support_GoogleSheetsParser::skip_space(
                            input,
                            p,
                            state,
                        );
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
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
                span_lo,
                30u8,
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
    /// AW-V.W4-fix — per-grammar ArgList-shape parse function.
    ///
    /// Emits one outer Rule compound over the whole call site.
    /// Head (Literal / Regex / Ref) + optional `(` + body arg
    /// positions (dispatched through the grammar's value-
    /// dispatcher) + `)` literal.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
    /// recursion rationale (see `flat.rs`).
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
    pub fn parse_arglist_GoogleSheetsParser_lambda_call(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.columns_mut().len() as u32;
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_GoogleSheetsParser(
                    "[lL][aA][mM][bB][dD][aA]\\(",
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
                        31u8,
                        0,
                        ::bbnf::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_flat_GoogleSheetsParser_lambda_params(input, p, state, builder)
            })?;
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        }
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [41u8] {
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
                    31u8,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Rule,
                span_lo,
                31u8,
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
    /// AW-V.W4.1 — per-grammar Pratt-shape parse function.
    ///
    /// Runs the operand-led shunting-yard reducer bounded by the
    /// emitted per-grammar `PRECEDENCE_LUT`. The reducer mirrors
    /// the walker's `DtaState::ShuntingYard` arm — `TapeKind::Rule`
    /// outer compound + per-op reduced binary compounds via
    /// `emit_reducer_compound`.
    ///
    /// # Emitted algorithm
    ///
    /// 1. Reserve an outer Rule compound via
    ///    [`::bbnf::runtime::tape::FusedBuilder::mark_children`] +
    ///    record the parse-open position.
    /// 2. Dispatch the leftmost operand through the grammar's
    ///    value-position dispatcher; the operand's records land
    ///    inside the outer compound's child run.
    /// 3. Loop: peek the next byte; consult `PRECEDENCE_LUT`; when
    ///    zero, break; when nonzero:
    ///    a. Reduce every top-of-op-stack entry whose precedence
    ///       exceeds the new byte's (or ties + left-assoc); each
    ///       reduce emits a `TapeKind::Rule` reducer compound via
    ///       [`::bbnf::runtime::tape::emit_reducer_compound`].
    ///    b. Emit a `TapeKind::Span` op leaf carrying the operator
    ///       byte's u8 discriminant into `pay_narrow` directly via
    ///       `push_leaf_with(InlineScalar)` (AY.W1.4 Pratt Option C
    ///       inline; bypasses the `arena_mut().push` round-trip
    ///       AX.W0a.2.l routed through).
    ///    c. Push the operator onto the local op stack with its
    ///       `(precedence, associativity, lhs_idx, lhs_span_lo)`.
    ///    d. Advance past the op bytes (1 or 2 for two-byte ops).
    ///    e. Re-dispatch the RHS operand.
    /// 4. On EOF-operator: drain the op stack — every remaining
    ///    entry reduces into a terminal compound. The final
    ///    `this_operand_root` is stamped onto the outer Rule
    ///    compound's `child_off` (overriding the default
    ///    `mark_children` index) so the cursor's pre-order walk
    ///    surfaces the reduced tree root as the compound's first
    ///    child.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
    /// recursion rationale.
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_assignments,
        unused_mut,
        unused_variables
    )]
    pub fn parse_pratt_GoogleSheetsParser_array_row(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        struct LocalOpEntry {
            op_discriminant: u8,
            precedence: u8,
            associativity_is_left: bool,
            lhs_idx: u32,
            lhs_span_lo: u32,
        }
        let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        let outer_span_lo = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Rule,
                outer_span_lo,
                32u8,
                0u8,
                0u8,
                0u16,
            );
        let _operand_off = ({
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            parse_pratt_GoogleSheetsParser_comparison_expr(input, p, state, builder)
        })?;
        let mut this_operand_root: u32 = _operand_off.0;
        const OP_STACK_CAP: usize = 16;
        let mut op_stack: [LocalOpEntry; OP_STACK_CAP] = ::core::array::from_fn(|_| LocalOpEntry {
            op_discriminant: 0,
            precedence: 0,
            associativity_is_left: false,
            lhs_idx: 0,
            lhs_span_lo: 0,
        });
        let mut op_stack_len: usize = 0;
        loop {
            let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
            let mut lut_byte: u8 = PRECEDENCE_LUT_array_row[op_byte as usize];
            if lut_byte == 0 {
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                op_byte = input.get(*p).copied().unwrap_or(0);
                lut_byte = PRECEDENCE_LUT_array_row[op_byte as usize];
            }
            let new_prec: ::core::option::Option<u8> = if lut_byte == 0 {
                ::core::option::Option::None
            } else {
                ::core::option::Option::Some(lut_byte & 0x0Fu8)
            };
            loop {
                if op_stack_len == 0 {
                    break;
                }
                let top_op = &op_stack[op_stack_len - 1];
                let should_reduce = match new_prec {
                    ::core::option::Option::None => true,
                    ::core::option::Option::Some(p_new) => {
                        top_op.precedence > p_new
                            || (top_op.precedence == p_new
                                && top_op.associativity_is_left)
                    }
                };
                if !should_reduce {
                    break;
                }
                let lhs_idx = top_op.lhs_idx;
                let lhs_span_lo = top_op.lhs_span_lo;
                let op_discriminant = top_op.op_discriminant;
                op_stack_len -= 1;
                let reducer_span_hi = *p as u32;
                let compound_idx = builder
                    .begin_compound(
                        ::bbnf::runtime::tape::TapeKind::Rule,
                        lhs_span_lo,
                        op_discriminant,
                        0u8,
                        0u8,
                        0u16,
                    );
                builder
                    .end_compound_post_order(
                        compound_idx,
                        reducer_span_hi,
                        ::bbnf::runtime::tape::TapeOffset(lhs_idx),
                    );
                this_operand_root = compound_idx;
            }
            if lut_byte == 0 {
                break;
            }
            let precedence: u8 = lut_byte & 0x0Fu8;
            let assoc_bit: u8 = (lut_byte >> 4) & 0x01u8;
            let associativity_is_left: bool = assoc_bit == 0;
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
            let op_lo: u32 = *p as u32;
            *p = (*p).saturating_add(op_width as usize);
            let op_hi: u32 = *p as u32;
            let _op_rec = builder
                .push_leaf_with(
                    ::bbnf::runtime::tape::TapeKind::Span,
                    op_lo,
                    op_hi,
                    0,
                    0,
                    ::bbnf::runtime::tape::PayloadData::InlineScalar(
                        op_discriminant as u32,
                    ),
                );
            let lhs_span_lo: u32 = if (this_operand_root as usize)
                < builder.columns().len()
            {
                builder.columns().span_lo_at(this_operand_root)
            } else {
                op_hi
            };
            debug_assert!(
                op_stack_len < OP_STACK_CAP,
                "Pratt op_stack overflow at depth {} (cap {})", op_stack_len,
                OP_STACK_CAP,
            );
            op_stack[op_stack_len] = LocalOpEntry {
                op_discriminant,
                precedence,
                associativity_is_left,
                lhs_idx: this_operand_root,
                lhs_span_lo,
            };
            op_stack_len += 1;
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            let _rhs_off = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_pratt_GoogleSheetsParser_comparison_expr(input, p, state, builder)
            })?;
            this_operand_root = _rhs_off.0;
        }
        let outer_span_hi = *p as u32;
        builder.end_compound(outer_off, outer_span_hi);
        builder
            .columns_mut()
            .set_child_off_at(
                outer_off,
                ::bbnf::runtime::tape::TapeOffset(this_operand_root),
            );
        Ok(::bbnf::runtime::tape::TapeOffset(outer_off))
    }
    /// AW-V.W4.1 — per-grammar Pratt-shape parse function.
    ///
    /// Runs the operand-led shunting-yard reducer bounded by the
    /// emitted per-grammar `PRECEDENCE_LUT`. The reducer mirrors
    /// the walker's `DtaState::ShuntingYard` arm — `TapeKind::Rule`
    /// outer compound + per-op reduced binary compounds via
    /// `emit_reducer_compound`.
    ///
    /// # Emitted algorithm
    ///
    /// 1. Reserve an outer Rule compound via
    ///    [`::bbnf::runtime::tape::FusedBuilder::mark_children`] +
    ///    record the parse-open position.
    /// 2. Dispatch the leftmost operand through the grammar's
    ///    value-position dispatcher; the operand's records land
    ///    inside the outer compound's child run.
    /// 3. Loop: peek the next byte; consult `PRECEDENCE_LUT`; when
    ///    zero, break; when nonzero:
    ///    a. Reduce every top-of-op-stack entry whose precedence
    ///       exceeds the new byte's (or ties + left-assoc); each
    ///       reduce emits a `TapeKind::Rule` reducer compound via
    ///       [`::bbnf::runtime::tape::emit_reducer_compound`].
    ///    b. Emit a `TapeKind::Span` op leaf carrying the operator
    ///       byte's u8 discriminant into `pay_narrow` directly via
    ///       `push_leaf_with(InlineScalar)` (AY.W1.4 Pratt Option C
    ///       inline; bypasses the `arena_mut().push` round-trip
    ///       AX.W0a.2.l routed through).
    ///    c. Push the operator onto the local op stack with its
    ///       `(precedence, associativity, lhs_idx, lhs_span_lo)`.
    ///    d. Advance past the op bytes (1 or 2 for two-byte ops).
    ///    e. Re-dispatch the RHS operand.
    /// 4. On EOF-operator: drain the op stack — every remaining
    ///    entry reduces into a terminal compound. The final
    ///    `this_operand_root` is stamped onto the outer Rule
    ///    compound's `child_off` (overriding the default
    ///    `mark_children` index) so the cursor's pre-order walk
    ///    surfaces the reduced tree root as the compound's first
    ///    child.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
    /// recursion rationale.
    #[inline]
    #[allow(
        non_snake_case,
        clippy::too_many_arguments,
        unused_assignments,
        unused_mut,
        unused_variables
    )]
    pub fn parse_pratt_GoogleSheetsParser_array_rows(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        struct LocalOpEntry {
            op_discriminant: u8,
            precedence: u8,
            associativity_is_left: bool,
            lhs_idx: u32,
            lhs_span_lo: u32,
        }
        let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        let outer_span_lo = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Rule,
                outer_span_lo,
                33u8,
                0u8,
                0u8,
                0u16,
            );
        let _operand_off = ({
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            parse_pratt_GoogleSheetsParser_array_row(input, p, state, builder)
        })?;
        let mut this_operand_root: u32 = _operand_off.0;
        const OP_STACK_CAP: usize = 16;
        let mut op_stack: [LocalOpEntry; OP_STACK_CAP] = ::core::array::from_fn(|_| LocalOpEntry {
            op_discriminant: 0,
            precedence: 0,
            associativity_is_left: false,
            lhs_idx: 0,
            lhs_span_lo: 0,
        });
        let mut op_stack_len: usize = 0;
        loop {
            let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
            let mut lut_byte: u8 = PRECEDENCE_LUT_array_rows[op_byte as usize];
            if lut_byte == 0 {
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                op_byte = input.get(*p).copied().unwrap_or(0);
                lut_byte = PRECEDENCE_LUT_array_rows[op_byte as usize];
            }
            let new_prec: ::core::option::Option<u8> = if lut_byte == 0 {
                ::core::option::Option::None
            } else {
                ::core::option::Option::Some(lut_byte & 0x0Fu8)
            };
            loop {
                if op_stack_len == 0 {
                    break;
                }
                let top_op = &op_stack[op_stack_len - 1];
                let should_reduce = match new_prec {
                    ::core::option::Option::None => true,
                    ::core::option::Option::Some(p_new) => {
                        top_op.precedence > p_new
                            || (top_op.precedence == p_new
                                && top_op.associativity_is_left)
                    }
                };
                if !should_reduce {
                    break;
                }
                let lhs_idx = top_op.lhs_idx;
                let lhs_span_lo = top_op.lhs_span_lo;
                let op_discriminant = top_op.op_discriminant;
                op_stack_len -= 1;
                let reducer_span_hi = *p as u32;
                let compound_idx = builder
                    .begin_compound(
                        ::bbnf::runtime::tape::TapeKind::Rule,
                        lhs_span_lo,
                        op_discriminant,
                        0u8,
                        0u8,
                        0u16,
                    );
                builder
                    .end_compound_post_order(
                        compound_idx,
                        reducer_span_hi,
                        ::bbnf::runtime::tape::TapeOffset(lhs_idx),
                    );
                this_operand_root = compound_idx;
            }
            if lut_byte == 0 {
                break;
            }
            let precedence: u8 = lut_byte & 0x0Fu8;
            let assoc_bit: u8 = (lut_byte >> 4) & 0x01u8;
            let associativity_is_left: bool = assoc_bit == 0;
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
            let op_lo: u32 = *p as u32;
            *p = (*p).saturating_add(op_width as usize);
            let op_hi: u32 = *p as u32;
            let _op_rec = builder
                .push_leaf_with(
                    ::bbnf::runtime::tape::TapeKind::Span,
                    op_lo,
                    op_hi,
                    0,
                    0,
                    ::bbnf::runtime::tape::PayloadData::InlineScalar(
                        op_discriminant as u32,
                    ),
                );
            let lhs_span_lo: u32 = if (this_operand_root as usize)
                < builder.columns().len()
            {
                builder.columns().span_lo_at(this_operand_root)
            } else {
                op_hi
            };
            debug_assert!(
                op_stack_len < OP_STACK_CAP,
                "Pratt op_stack overflow at depth {} (cap {})", op_stack_len,
                OP_STACK_CAP,
            );
            op_stack[op_stack_len] = LocalOpEntry {
                op_discriminant,
                precedence,
                associativity_is_left,
                lhs_idx: this_operand_root,
                lhs_span_lo,
            };
            op_stack_len += 1;
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            let _rhs_off = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_pratt_GoogleSheetsParser_array_row(input, p, state, builder)
            })?;
            this_operand_root = _rhs_off.0;
        }
        let outer_span_hi = *p as u32;
        builder.end_compound(outer_off, outer_span_hi);
        builder
            .columns_mut()
            .set_child_off_at(
                outer_off,
                ::bbnf::runtime::tape::TapeOffset(this_operand_root),
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
    pub fn parse_flat_GoogleSheetsParser_array_literal(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
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
            if input.len() < end || input[at..end] != [123u8] {
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
                    34u8,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
        }
        {
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_pratt_GoogleSheetsParser_array_rows(input, p, state, builder)
            })?;
            let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        }
        {
            let at = *p;
            let end = at + 1usize;
            if input.len() < end || input[at..end] != [125u8] {
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
                    34u8,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
                span_lo,
                34u8,
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
    pub fn parse_flat_GoogleSheetsParser_formula(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let span_lo = *p as u32;
        let outer_child = builder.columns_mut().len() as u32;
        {
            {
                let span_lo = *p as u32;
                let Some(match_len) = __regex_scan_GoogleSheetsParser("=?", input, *p)
                else {
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
                        35u8,
                        0,
                        ::bbnf::runtime::tape::PayloadData::None,
                    );
            }
        }
        {
            let _ = ({
                let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
                parse_pratt_GoogleSheetsParser_comparison_expr(input, p, state, builder)
            })?;
        }
        let span_hi = *p as u32;
        let outer_off = builder
            .begin_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
                span_lo,
                35u8,
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
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Empty,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 1u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Empty,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 2u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Empty,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(0),
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
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 5u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Sparse,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 6u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Sparse,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(2),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 7u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Sparse,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(2),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 8u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Sparse,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(2),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 9u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Sparse,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 10u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Digraph,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(14),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 11u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Digraph,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(14),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 12u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Dense,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(7),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 13u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Sparse,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(2),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 14u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Dense,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(7),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 15u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Sparse,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(2),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 16u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Dense,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(7),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 17u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Dense,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(7),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 18u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Sparse,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(2),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 19u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Dense,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(7),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 20u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Dense,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(7),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 21u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Dense,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(7),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 22u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Sparse,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(2),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 23u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Sparse,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(2),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 24u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Digraph,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(14),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 25u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Digraph,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(14),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 26u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Sparse,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(2),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 27u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Digraph,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(14),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 28u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Digraph,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(14),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 29u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Empty,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 30u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Digraph,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(14),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 31u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Empty,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(0),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 32u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Digraph,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(14),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 33u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Digraph,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(14),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 34u32,
            alphabet_class: ::bbnf::runtime::tape::ScanAlphabetClass::Sparse,
            activation: ::bbnf::runtime::tape::ScanActivationFlags::from_bits(2),
        },
        ::bbnf::runtime::tape::ScanPolicyEntry {
            rule_id: 35u32,
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
    pub fn parse_GoogleSheetsParser_formula(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        parse_GoogleSheetsParser_formula__value(input, p, state, builder)
    }
    /// AW-V.W3.2 — value-position shape dispatcher. Called both at
    /// the grammar root and from Object / Array compound bodies.
    ///
    /// AX.W0a.2.f — compound; plain `#[inline]`.
    #[inline]
    #[allow(non_snake_case, clippy::too_many_arguments)]
    pub fn parse_GoogleSheetsParser_formula__value(
        input: &[u8],
        p: &mut usize,
        state: &mut __shape_support_GoogleSheetsParser::ScanState,
        builder: &mut ::bbnf::runtime::tape::FusedBuilder,
    ) -> ::core::result::Result<
        ::bbnf::runtime::tape::TapeOffset,
        ::bbnf::runtime::tape::DtaError,
    > {
        let _ = __shape_support_GoogleSheetsParser::skip_space(input, p, state);
        parse_flat_GoogleSheetsParser_formula(input, p, state, builder)
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct numberView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> numberView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> stringView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    pub struct booleanView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> booleanView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> booleanView<'p> {
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
    pub struct error_literalView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> error_literalView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> error_literalView<'p> {
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
        pub fn value(&self) -> u8 {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 1usize) {
                Some(__bytes) => __bytes[0usize],
                None => 0_u8,
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct sheet_prefixView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> sheet_prefixView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> sheet_prefixView<'p> {
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
        pub fn value(&self) -> (u8) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 1usize) {
                Some(__bytes) => (__bytes[0usize]),
                None => (0_u8),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct cell_refView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> cell_refView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> cell_refView<'p> {
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
    pub struct cellView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> cellView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> cellView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<cell_refView<'p>> {
            self.cursor.child(1usize).map(|c| cell_refView::from_cursor(c, self.input))
        }
        ///The `cell_ref` child as a typed view.
        #[inline]
        pub fn cell_ref(&self) -> ::core::option::Option<cell_refView<'p>> {
            self.cursor.child(1usize).map(|c| cell_refView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct range_refView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> range_refView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> range_refView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 2 as a typed view.
        #[inline]
        pub fn child_2(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(2usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 3 as a typed view.
        #[inline]
        pub fn child_3(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(3usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            4usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct cell_or_rangeView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> cell_or_rangeView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> cell_or_rangeView<'p> {
        ///If variant `range_ref` (branch 0) was chosen, return its child view.
        #[inline]
        pub fn as_range_ref(&self) -> ::core::option::Option<range_refView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor.child(0).map(|c| range_refView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `range_ref` (branch 0) was chosen.
        #[inline]
        pub fn is_range_ref(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If variant `cell` (branch 1) was chosen, return its child view.
        #[inline]
        pub fn as_cell(&self) -> ::core::option::Option<cellView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor.child(0).map(|c| cellView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `cell` (branch 1) was chosen.
        #[inline]
        pub fn is_cell(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        /// The chosen branch's child as a generic node view,
        /// regardless of which variant was selected.
        #[inline]
        pub fn chosen(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(0)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct identifierView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> identifierView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct compare_opView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> compare_opView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> compare_opView<'p> {
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
        pub fn value(&self) -> (u8) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 1usize) {
                Some(__bytes) => (__bytes[0usize]),
                None => (0_u8),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct comparison_exprView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> comparison_exprView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> comparison_exprView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            1usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct concat_exprView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> concat_exprView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> concat_exprView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            1usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct add_opView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> add_opView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> add_opView<'p> {
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
        pub fn value(&self) -> (u8) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 1usize) {
                Some(__bytes) => (__bytes[0usize]),
                None => (0_u8),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct add_exprView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> add_exprView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> add_exprView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            1usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct mul_opView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> mul_opView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> mul_opView<'p> {
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
        pub fn value(&self) -> (u8) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 1usize) {
                Some(__bytes) => (__bytes[0usize]),
                None => (0_u8),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct mul_exprView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> mul_exprView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> mul_exprView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            1usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct exp_exprView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> exp_exprView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> exp_exprView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            1usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct unary_prefixView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> unary_prefixView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> unary_prefixView<'p> {
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
        pub fn value(&self) -> (u8) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 1usize) {
                Some(__bytes) => (__bytes[0usize]),
                None => (0_u8),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct unary_exprView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> unary_exprView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> unary_exprView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<postfix_exprView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| postfix_exprView::from_cursor(c, self.input))
        }
        ///The `postfix_expr` child as a typed view.
        #[inline]
        pub fn postfix_expr(&self) -> ::core::option::Option<postfix_exprView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| postfix_exprView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct postfix_exprView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> postfix_exprView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> postfix_exprView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<primaryView<'p>> {
            self.cursor.child(0usize).map(|c| primaryView::from_cursor(c, self.input))
        }
        ///The `primary` child as a typed view.
        #[inline]
        pub fn primary(&self) -> ::core::option::Option<primaryView<'p>> {
            self.cursor.child(0usize).map(|c| primaryView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct primaryView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> primaryView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> primaryView<'p> {
        ///If variant `let_call` (branch 0) was chosen, return its child view.
        #[inline]
        pub fn as_let_call(&self) -> ::core::option::Option<let_callView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor.child(0).map(|c| let_callView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `let_call` (branch 0) was chosen.
        #[inline]
        pub fn is_let_call(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If variant `lambda_call` (branch 1) was chosen, return its child view.
        #[inline]
        pub fn as_lambda_call(&self) -> ::core::option::Option<lambda_callView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor.child(0).map(|c| lambda_callView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `lambda_call` (branch 1) was chosen.
        #[inline]
        pub fn is_lambda_call(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        ///If variant `func_call` (branch 2) was chosen, return its child view.
        #[inline]
        pub fn as_func_call(&self) -> ::core::option::Option<func_callView<'p>> {
            if self.cursor.meta_idx() == 2u8 {
                self.cursor.child(0).map(|c| func_callView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `func_call` (branch 2) was chosen.
        #[inline]
        pub fn is_func_call(&self) -> bool {
            self.cursor.meta_idx() == 2u8
        }
        ///If variant `number` (branch 3) was chosen, return its child view.
        #[inline]
        pub fn as_number(&self) -> ::core::option::Option<numberView<'p>> {
            if self.cursor.meta_idx() == 3u8 {
                self.cursor.child(0).map(|c| numberView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `number` (branch 3) was chosen.
        #[inline]
        pub fn is_number(&self) -> bool {
            self.cursor.meta_idx() == 3u8
        }
        ///If variant `boolean` (branch 4) was chosen, return its child view.
        #[inline]
        pub fn as_boolean(&self) -> ::core::option::Option<booleanView<'p>> {
            if self.cursor.meta_idx() == 4u8 {
                self.cursor.child(0).map(|c| booleanView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `boolean` (branch 4) was chosen.
        #[inline]
        pub fn is_boolean(&self) -> bool {
            self.cursor.meta_idx() == 4u8
        }
        ///If variant `cell_or_range` (branch 5) was chosen, return its child view.
        #[inline]
        pub fn as_cell_or_range(&self) -> ::core::option::Option<cell_or_rangeView<'p>> {
            if self.cursor.meta_idx() == 5u8 {
                self.cursor
                    .child(0)
                    .map(|c| cell_or_rangeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `cell_or_range` (branch 5) was chosen.
        #[inline]
        pub fn is_cell_or_range(&self) -> bool {
            self.cursor.meta_idx() == 5u8
        }
        ///If variant `identifier` (branch 6) was chosen, return its child view.
        #[inline]
        pub fn as_identifier(&self) -> ::core::option::Option<identifierView<'p>> {
            if self.cursor.meta_idx() == 6u8 {
                self.cursor.child(0).map(|c| identifierView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `identifier` (branch 6) was chosen.
        #[inline]
        pub fn is_identifier(&self) -> bool {
            self.cursor.meta_idx() == 6u8
        }
        ///If variant `string` (branch 7) was chosen, return its child view.
        #[inline]
        pub fn as_string(&self) -> ::core::option::Option<stringView<'p>> {
            if self.cursor.meta_idx() == 7u8 {
                self.cursor.child(0).map(|c| stringView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `string` (branch 7) was chosen.
        #[inline]
        pub fn is_string(&self) -> bool {
            self.cursor.meta_idx() == 7u8
        }
        ///If variant `error_literal` (branch 8) was chosen, return its child view.
        #[inline]
        pub fn as_error_literal(&self) -> ::core::option::Option<error_literalView<'p>> {
            if self.cursor.meta_idx() == 8u8 {
                self.cursor
                    .child(0)
                    .map(|c| error_literalView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `error_literal` (branch 8) was chosen.
        #[inline]
        pub fn is_error_literal(&self) -> bool {
            self.cursor.meta_idx() == 8u8
        }
        ///If variant `array_literal` (branch 9) was chosen, return its child view.
        #[inline]
        pub fn as_array_literal(&self) -> ::core::option::Option<array_literalView<'p>> {
            if self.cursor.meta_idx() == 9u8 {
                self.cursor
                    .child(0)
                    .map(|c| array_literalView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `array_literal` (branch 9) was chosen.
        #[inline]
        pub fn is_array_literal(&self) -> bool {
            self.cursor.meta_idx() == 9u8
        }
        ///If variant `paren_expr` (branch 10) was chosen, return its child view.
        #[inline]
        pub fn as_paren_expr(&self) -> ::core::option::Option<paren_exprView<'p>> {
            if self.cursor.meta_idx() == 10u8 {
                self.cursor.child(0).map(|c| paren_exprView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `paren_expr` (branch 10) was chosen.
        #[inline]
        pub fn is_paren_expr(&self) -> bool {
            self.cursor.meta_idx() == 10u8
        }
        ///If sub-variant `primary_0` was chosen (branch 3), return its child view.
        #[inline]
        pub fn as_primary_0(
            &self,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            if self.cursor.meta_idx() == 3u8 {
                self.cursor
                    .child(0)
                    .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_primary_0(&self) -> bool {
            self.cursor.meta_idx() == 3u8
        }
        ///If sub-variant `primary_1` was chosen (branch 4), return its child view.
        #[inline]
        pub fn as_primary_1(
            &self,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            if self.cursor.meta_idx() == 4u8 {
                self.cursor
                    .child(0)
                    .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_primary_1(&self) -> bool {
            self.cursor.meta_idx() == 4u8
        }
        ///If sub-variant `primary_2` was chosen (branch 6), return its child view.
        #[inline]
        pub fn as_primary_2(
            &self,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            if self.cursor.meta_idx() == 6u8 {
                self.cursor
                    .child(0)
                    .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_primary_2(&self) -> bool {
            self.cursor.meta_idx() == 6u8
        }
        ///If sub-variant `primary_2_sv1` was chosen (branch 7), return its child view.
        #[inline]
        pub fn as_primary_2_sv1(
            &self,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            if self.cursor.meta_idx() == 7u8 {
                self.cursor
                    .child(0)
                    .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_primary_2_sv1(&self) -> bool {
            self.cursor.meta_idx() == 7u8
        }
        /// The chosen branch's child as a generic node view,
        /// regardless of which variant was selected.
        #[inline]
        pub fn chosen(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(0)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
    }
    /// Typed value enum — payload-eligible branches carry typed
    /// values directly; non-eligible branches wrap a cursor view.
    #[derive(Clone, Debug)]
    pub enum primaryValue<'p> {
        let_call(GoogleSheetsParserNodeView<'p>),
        lambda_call(GoogleSheetsParserNodeView<'p>),
        func_call(GoogleSheetsParserNodeView<'p>),
        number(f64),
        boolean((bool)),
        cell_or_range(GoogleSheetsParserNodeView<'p>),
        identifier(&'p str),
        string(((u32, u32))),
        error_literal((u8)),
        array_literal(GoogleSheetsParserNodeView<'p>),
        paren_expr(GoogleSheetsParserNodeView<'p>),
    }
    impl<'p> primaryView<'p> {
        /// Decode the chosen branch's value. Payload-eligible
        /// branches return typed scalars/aggregates; other
        /// branches return cursor-wrapped sub-views.
        #[inline]
        pub fn value(&self) -> ::core::option::Option<primaryValue<'p>> {
            match self.cursor.meta_idx() {
                0u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        primaryValue::let_call(
                            GoogleSheetsParserNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                1u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        primaryValue::lambda_call(
                            GoogleSheetsParserNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                2u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        primaryValue::func_call(
                            GoogleSheetsParserNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                3u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = __tape
                        .payload_f64(__rec)
                        .unwrap_or(<f64 as ::core::default::Default>::default());
                    Some(primaryValue::number(__value))
                }
                4u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_bytes(__rec, 1usize) {
                        Some(__bytes) => (__bytes[0usize] != 0),
                        None => (false),
                    };
                    Some(primaryValue::boolean(__value))
                }
                5u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        primaryValue::cell_or_range(
                            GoogleSheetsParserNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                6u8 => {
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
                    Some(primaryValue::identifier(__value))
                }
                7u8 => {
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
                    Some(primaryValue::string(__value))
                }
                8u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_bytes(__rec, 1usize) {
                        Some(__bytes) => (__bytes[0usize]),
                        None => (0_u8),
                    };
                    Some(primaryValue::error_literal(__value))
                }
                9u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        primaryValue::array_literal(
                            GoogleSheetsParserNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                10u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        primaryValue::paren_expr(
                            GoogleSheetsParserNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                _ => None,
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct paren_exprView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> paren_exprView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> paren_exprView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct func_openView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> func_openView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> func_openView<'p> {
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
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct argView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> argView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> argView<'p> {
        /// Iterator over each repetition element as a typed view.
        #[inline]
        pub fn iter(
            &self,
        ) -> impl ::core::iter::Iterator<Item = comparison_exprView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| comparison_exprView::from_cursor(c, input))
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
        pub fn get(&self, i: usize) -> ::core::option::Option<comparison_exprView<'p>> {
            self.cursor.child(i).map(|c| comparison_exprView::from_cursor(c, self.input))
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct func_argsView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> func_argsView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> func_argsView<'p> {
        /// Iterator over each repetition element as a typed view.
        #[inline]
        pub fn iter(&self) -> impl ::core::iter::Iterator<Item = argView<'p>> + 'p {
            let input = self.input;
            self.cursor.children().map(move |c| argView::from_cursor(c, input))
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
        pub fn get(&self, i: usize) -> ::core::option::Option<argView<'p>> {
            self.cursor.child(i).map(|c| argView::from_cursor(c, self.input))
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct func_callView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> func_callView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> func_callView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<func_openView<'p>> {
            self.cursor.child(0usize).map(|c| func_openView::from_cursor(c, self.input))
        }
        ///The `func_open` child as a typed view.
        #[inline]
        pub fn func_open(&self) -> ::core::option::Option<func_openView<'p>> {
            self.cursor.child(0usize).map(|c| func_openView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct let_bindingView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> let_bindingView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> let_bindingView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<comparison_exprView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| comparison_exprView::from_cursor(c, self.input))
        }
        ///The `comparison_expr` child as a typed view.
        #[inline]
        pub fn comparison_expr(
            &self,
        ) -> ::core::option::Option<comparison_exprView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| comparison_exprView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<comparison_exprView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| comparison_exprView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct let_argsView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> let_argsView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> let_argsView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<comparison_exprView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| comparison_exprView::from_cursor(c, self.input))
        }
        ///The `comparison_expr` child as a typed view.
        #[inline]
        pub fn comparison_expr(
            &self,
        ) -> ::core::option::Option<comparison_exprView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| comparison_exprView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct let_callView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> let_callView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> let_callView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct lambda_paramsView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> lambda_paramsView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> lambda_paramsView<'p> {
        /// Iterator over each repetition element as a typed view.
        #[inline]
        pub fn iter(
            &self,
        ) -> impl ::core::iter::Iterator<Item = comparison_exprView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| comparison_exprView::from_cursor(c, input))
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
        pub fn get(&self, i: usize) -> ::core::option::Option<comparison_exprView<'p>> {
            self.cursor.child(i).map(|c| comparison_exprView::from_cursor(c, self.input))
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct lambda_callView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> lambda_callView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> lambda_callView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct array_rowView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> array_rowView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> array_rowView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<comparison_exprView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| comparison_exprView::from_cursor(c, self.input))
        }
        ///The `comparison_expr` child as a typed view.
        #[inline]
        pub fn comparison_expr(
            &self,
        ) -> ::core::option::Option<comparison_exprView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| comparison_exprView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct array_rowsView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> array_rowsView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> array_rowsView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<array_rowView<'p>> {
            self.cursor.child(0usize).map(|c| array_rowView::from_cursor(c, self.input))
        }
        ///The `array_row` child as a typed view.
        #[inline]
        pub fn array_row(&self) -> ::core::option::Option<array_rowView<'p>> {
            self.cursor.child(0usize).map(|c| array_rowView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct array_literalView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> array_literalView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> array_literalView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct formulaView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> formulaView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl<'p> formulaView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<comparison_exprView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| comparison_exprView::from_cursor(c, self.input))
        }
        ///The `comparison_expr` child as a typed view.
        #[inline]
        pub fn comparison_expr(
            &self,
        ) -> ::core::option::Option<comparison_exprView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| comparison_exprView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generic node view over any tape record for this grammar.
    #[derive(Clone, Copy, Debug)]
    pub struct GoogleSheetsParserNodeView<'p> {
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
    pub enum GoogleSheetsParserRuleKind {
        number,
        string,
        boolean,
        error_literal,
        sheet_prefix,
        cell_ref,
        cell,
        range_ref,
        cell_or_range,
        identifier,
        compare_op,
        comparison_expr,
        concat_expr,
        add_op,
        add_expr,
        mul_op,
        mul_expr,
        exp_expr,
        unary_prefix,
        unary_expr,
        postfix_expr,
        primary,
        paren_expr,
        func_open,
        arg,
        func_args,
        func_call,
        let_binding,
        let_args,
        let_call,
        lambda_params,
        lambda_call,
        array_row,
        array_rows,
        array_literal,
        formula,
        error_literal_0,
        error_literal_1,
        compare_op_0,
        compare_op_1,
        primary_0,
        primary_1,
        primary_2,
        /// Fallback for records whose variant_idx is not a
        /// known rule- or sub-variant discriminator.
        Unknown,
    }
    impl<'p> GoogleSheetsParserNodeView<'p> {
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
        pub fn rule_kind(&self) -> GoogleSheetsParserRuleKind {
            match self.variant_idx() {
                0u8 => GoogleSheetsParserRuleKind::number,
                1u8 => GoogleSheetsParserRuleKind::string,
                2u8 => GoogleSheetsParserRuleKind::boolean,
                3u8 => GoogleSheetsParserRuleKind::error_literal,
                4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
                5u8 => GoogleSheetsParserRuleKind::cell_ref,
                6u8 => GoogleSheetsParserRuleKind::cell,
                7u8 => GoogleSheetsParserRuleKind::range_ref,
                8u8 => GoogleSheetsParserRuleKind::cell_or_range,
                9u8 => GoogleSheetsParserRuleKind::identifier,
                10u8 => GoogleSheetsParserRuleKind::compare_op,
                11u8 => GoogleSheetsParserRuleKind::comparison_expr,
                12u8 => GoogleSheetsParserRuleKind::concat_expr,
                13u8 => GoogleSheetsParserRuleKind::add_op,
                14u8 => GoogleSheetsParserRuleKind::add_expr,
                15u8 => GoogleSheetsParserRuleKind::mul_op,
                16u8 => GoogleSheetsParserRuleKind::mul_expr,
                17u8 => GoogleSheetsParserRuleKind::exp_expr,
                18u8 => GoogleSheetsParserRuleKind::unary_prefix,
                19u8 => GoogleSheetsParserRuleKind::unary_expr,
                20u8 => GoogleSheetsParserRuleKind::postfix_expr,
                21u8 => GoogleSheetsParserRuleKind::primary,
                22u8 => GoogleSheetsParserRuleKind::paren_expr,
                23u8 => GoogleSheetsParserRuleKind::func_open,
                24u8 => GoogleSheetsParserRuleKind::arg,
                25u8 => GoogleSheetsParserRuleKind::func_args,
                26u8 => GoogleSheetsParserRuleKind::func_call,
                27u8 => GoogleSheetsParserRuleKind::let_binding,
                28u8 => GoogleSheetsParserRuleKind::let_args,
                29u8 => GoogleSheetsParserRuleKind::let_call,
                30u8 => GoogleSheetsParserRuleKind::lambda_params,
                31u8 => GoogleSheetsParserRuleKind::lambda_call,
                32u8 => GoogleSheetsParserRuleKind::array_row,
                33u8 => GoogleSheetsParserRuleKind::array_rows,
                34u8 => GoogleSheetsParserRuleKind::array_literal,
                35u8 => GoogleSheetsParserRuleKind::formula,
                36u8 => GoogleSheetsParserRuleKind::error_literal_0,
                37u8 => GoogleSheetsParserRuleKind::error_literal_1,
                38u8 => GoogleSheetsParserRuleKind::compare_op_0,
                39u8 => GoogleSheetsParserRuleKind::compare_op_1,
                40u8 => GoogleSheetsParserRuleKind::primary_0,
                41u8 => GoogleSheetsParserRuleKind::primary_1,
                42u8 => GoogleSheetsParserRuleKind::primary_2,
                _ => GoogleSheetsParserRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = GoogleSheetsParserNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| GoogleSheetsParserNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| GoogleSheetsParserNodeView::from_cursor(c, self.input))
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
    impl ::bbnf::runtime::Root for GoogleSheetsParser {
        type View<'p> = formulaView<'p>;
        #[inline]
        fn make_view<'p>(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            root: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self::View<'p> {
            formulaView::new(tape, input, root)
        }
    }
    impl GoogleSheetsParser {
        /// The name of the root rule for this grammar.
        #[inline]
        pub fn root_rule_name() -> &'static str {
            "formula"
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
    pub struct GoogleSheetsParserStringProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: (u32, u32),
    }
    impl GoogleSheetsParserStringProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "string";
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
    pub struct GoogleSheetsParserBooleanProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: bool,
    }
    impl GoogleSheetsParserBooleanProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "boolean";
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
    pub struct GoogleSheetsParserErrorLiteralProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: u8,
    }
    impl GoogleSheetsParserErrorLiteralProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "error_literal";
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
    pub struct GoogleSheetsParserSheetPrefixProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: u8,
    }
    impl GoogleSheetsParserSheetPrefixProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "sheet_prefix";
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
    pub struct GoogleSheetsParserCellRefProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: (u32, u32),
    }
    impl GoogleSheetsParserCellRefProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "cell_ref";
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
    pub struct GoogleSheetsParserCompareOpProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: u8,
    }
    impl GoogleSheetsParserCompareOpProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "compare_op";
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
    pub struct GoogleSheetsParserAddOpProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: u8,
    }
    impl GoogleSheetsParserAddOpProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "add_op";
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
    pub struct GoogleSheetsParserMulOpProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: u8,
    }
    impl GoogleSheetsParserMulOpProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "mul_op";
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
    pub struct GoogleSheetsParserUnaryPrefixProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: u8,
    }
    impl GoogleSheetsParserUnaryPrefixProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "unary_prefix";
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
    pub struct GoogleSheetsParserFuncOpenProjection {
        /// Grammar-declared scalar field at packed-buffer offset
        #[doc = concat!("`", stringify!(0), "` (bytes).")]
        pub field_0: (u32, u32),
    }
    impl GoogleSheetsParserFuncOpenProjection {
        /// Grammar-declared rule that projects into this
        /// struct. Matches the `rule_name` entry in
        /// `PROJECTION_DIRECT_TO_STRUCT`.
        #[doc(hidden)]
        pub const RULE_NAME: &'static str = "func_open";
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
    pub const PROJECTION_DIRECT_TO_STRUCT: &[(&str, &str); 10usize] = &[
        ("string", "GoogleSheetsParserStringProjection"),
        ("boolean", "GoogleSheetsParserBooleanProjection"),
        ("error_literal", "GoogleSheetsParserErrorLiteralProjection"),
        ("sheet_prefix", "GoogleSheetsParserSheetPrefixProjection"),
        ("cell_ref", "GoogleSheetsParserCellRefProjection"),
        ("compare_op", "GoogleSheetsParserCompareOpProjection"),
        ("add_op", "GoogleSheetsParserAddOpProjection"),
        ("mul_op", "GoogleSheetsParserMulOpProjection"),
        ("unary_prefix", "GoogleSheetsParserUnaryPrefixProjection"),
        ("func_open", "GoogleSheetsParserFuncOpenProjection"),
    ];
    /// AY-II.W0.d — grammar-declared `-> Name` bindings, indexed in
    /// lockstep with `PROJECTION_DIRECT_TO_STRUCT`. Empty string for
    /// admissions that did not spell a named type.
    #[doc(hidden)]
    pub const PROJECTION_NAMED_BINDINGS: &[&str; 10usize] = &[
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
    ];
    /// AY-II.W0.d — canonical evidence that every admission has a
    /// matching `materialize_projection_<rule>_<Grammar>` fn.
    /// Indexed in lockstep with `PROJECTION_DIRECT_TO_STRUCT`; the
    /// wire-contract totality test asserts both slices share the
    /// same length per grammar.
    #[doc(hidden)]
    pub const PROJECTION_MATERIALIZERS: &[&str; 10usize] = &[
        "materialize_projection_string_GoogleSheetsParser",
        "materialize_projection_boolean_GoogleSheetsParser",
        "materialize_projection_error_literal_GoogleSheetsParser",
        "materialize_projection_sheet_prefix_GoogleSheetsParser",
        "materialize_projection_cell_ref_GoogleSheetsParser",
        "materialize_projection_compare_op_GoogleSheetsParser",
        "materialize_projection_add_op_GoogleSheetsParser",
        "materialize_projection_mul_op_GoogleSheetsParser",
        "materialize_projection_unary_prefix_GoogleSheetsParser",
        "materialize_projection_func_open_GoogleSheetsParser",
    ];
    /// AY-II.W0.d — canonical evidence that every admission has a
    /// matching `<Grammar>Value::<RuleName>` enum variant
    /// (production consumer). Indexed in lockstep with
    /// `PROJECTION_DIRECT_TO_STRUCT`.
    #[doc(hidden)]
    pub const PROJECTION_CONSUMERS: &[&str; 10usize] = &[
        "GoogleSheetsParserValue::string",
        "GoogleSheetsParserValue::boolean",
        "GoogleSheetsParserValue::error_literal",
        "GoogleSheetsParserValue::sheet_prefix",
        "GoogleSheetsParserValue::cell_ref",
        "GoogleSheetsParserValue::compare_op",
        "GoogleSheetsParserValue::add_op",
        "GoogleSheetsParserValue::mul_op",
        "GoogleSheetsParserValue::unary_prefix",
        "GoogleSheetsParserValue::func_open",
    ];
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_string() -> (&'static str, usize, &'static str) {
        ("string", 1, "")
    }
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_boolean() -> (&'static str, usize, &'static str) {
        ("boolean", 1, "")
    }
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_error_literal() -> (&'static str, usize, &'static str) {
        ("error_literal", 1, "")
    }
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_sheet_prefix() -> (&'static str, usize, &'static str) {
        ("sheet_prefix", 1, "")
    }
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_cell_ref() -> (&'static str, usize, &'static str) {
        ("cell_ref", 1, "")
    }
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_compare_op() -> (&'static str, usize, &'static str) {
        ("compare_op", 1, "")
    }
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_add_op() -> (&'static str, usize, &'static str) {
        ("add_op", 1, "")
    }
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_mul_op() -> (&'static str, usize, &'static str) {
        ("mul_op", 1, "")
    }
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_unary_prefix() -> (&'static str, usize, &'static str) {
        ("unary_prefix", 1, "")
    }
    /// AY-II.W0.d marker — structural evidence that the
    /// layout pass + resolver admitted this rule for
    /// direct-to-struct projection. The returned
    /// `(rule_name, field_count, named_binding)` triple
    /// exposes the admitted shape to the `cargo expand`
    /// hard gate without requiring a runtime compilation.
    #[doc(hidden)]
    #[inline(always)]
    pub fn __grammar_projection_func_open() -> (&'static str, usize, &'static str) {
        ("func_open", 1, "")
    }
    /// AY-II.W0'.b — grammar-emitted value enum. Eager
    /// materialisation target for `Parsed::to_value()`. Variants
    /// enumerate non-transparent rules; admitted rules carry the
    /// matching `<Grammar><RuleCamel>Projection` struct directly,
    /// non-admitted rules carry their shape-classified payload.
    #[derive(Clone, Debug)]
    pub enum GoogleSheetsParserValue<'p> {
        number(f64),
        string(GoogleSheetsParserStringProjection),
        boolean(GoogleSheetsParserBooleanProjection),
        error_literal(GoogleSheetsParserErrorLiteralProjection),
        sheet_prefix(GoogleSheetsParserSheetPrefixProjection),
        cell_ref(GoogleSheetsParserCellRefProjection),
        cell(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        range_ref(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        cell_or_range(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        identifier(&'p str),
        compare_op(GoogleSheetsParserCompareOpProjection),
        comparison_expr(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        concat_expr(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        add_op(GoogleSheetsParserAddOpProjection),
        add_expr(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        mul_op(GoogleSheetsParserMulOpProjection),
        mul_expr(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        exp_expr(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        unary_prefix(GoogleSheetsParserUnaryPrefixProjection),
        unary_expr(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        postfix_expr(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        primary(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        paren_expr(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        func_open(GoogleSheetsParserFuncOpenProjection),
        arg(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        func_args(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        func_call(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        let_binding(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        let_args(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        let_call(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        lambda_params(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        lambda_call(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        array_row(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        array_rows(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        array_literal(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        formula(::std::vec::Vec<GoogleSheetsParserValue<'p>>),
        /// Fallback for records whose `variant_idx` is not a
        /// known rule discriminator (recovered records, stray
        /// sub-variant indices).
        Unknown(GoogleSheetsParserNodeView<'p>),
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
    fn project_rule_kind_GoogleSheetsParser(
        kind: ::bbnf::runtime::tape::TapeKind,
        variant_idx: u8,
    ) -> GoogleSheetsParserRuleKind {
        if variant_idx == 0 && kind.is_compound() {
            return GoogleSheetsParserRuleKind::Unknown;
        }
        match variant_idx {
            0u8 => GoogleSheetsParserRuleKind::number,
            1u8 => GoogleSheetsParserRuleKind::string,
            2u8 => GoogleSheetsParserRuleKind::boolean,
            3u8 => GoogleSheetsParserRuleKind::error_literal,
            4u8 => GoogleSheetsParserRuleKind::sheet_prefix,
            5u8 => GoogleSheetsParserRuleKind::cell_ref,
            6u8 => GoogleSheetsParserRuleKind::cell,
            7u8 => GoogleSheetsParserRuleKind::range_ref,
            8u8 => GoogleSheetsParserRuleKind::cell_or_range,
            9u8 => GoogleSheetsParserRuleKind::identifier,
            10u8 => GoogleSheetsParserRuleKind::compare_op,
            11u8 => GoogleSheetsParserRuleKind::comparison_expr,
            12u8 => GoogleSheetsParserRuleKind::concat_expr,
            13u8 => GoogleSheetsParserRuleKind::add_op,
            14u8 => GoogleSheetsParserRuleKind::add_expr,
            15u8 => GoogleSheetsParserRuleKind::mul_op,
            16u8 => GoogleSheetsParserRuleKind::mul_expr,
            17u8 => GoogleSheetsParserRuleKind::exp_expr,
            18u8 => GoogleSheetsParserRuleKind::unary_prefix,
            19u8 => GoogleSheetsParserRuleKind::unary_expr,
            20u8 => GoogleSheetsParserRuleKind::postfix_expr,
            21u8 => GoogleSheetsParserRuleKind::primary,
            22u8 => GoogleSheetsParserRuleKind::paren_expr,
            23u8 => GoogleSheetsParserRuleKind::func_open,
            24u8 => GoogleSheetsParserRuleKind::arg,
            25u8 => GoogleSheetsParserRuleKind::func_args,
            26u8 => GoogleSheetsParserRuleKind::func_call,
            27u8 => GoogleSheetsParserRuleKind::let_binding,
            28u8 => GoogleSheetsParserRuleKind::let_args,
            29u8 => GoogleSheetsParserRuleKind::let_call,
            30u8 => GoogleSheetsParserRuleKind::lambda_params,
            31u8 => GoogleSheetsParserRuleKind::lambda_call,
            32u8 => GoogleSheetsParserRuleKind::array_row,
            33u8 => GoogleSheetsParserRuleKind::array_rows,
            34u8 => GoogleSheetsParserRuleKind::array_literal,
            35u8 => GoogleSheetsParserRuleKind::formula,
            _ => GoogleSheetsParserRuleKind::Unknown,
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
    fn project_push_children_GoogleSheetsParser<'p>(
        output: &::bbnf::runtime::FusedOutput<GoogleSheetsParser>,
        input: &'p str,
        offset: u32,
        out: &mut ::std::vec::Vec<GoogleSheetsParserValue<'p>>,
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
                project_push_children_GoogleSheetsParser(
                    output,
                    input,
                    __child.offset().0,
                    out,
                );
            }
        } else {
            out.push(project_frame_GoogleSheetsParser(output, input, offset));
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
    fn project_frame_GoogleSheetsParser<'p>(
        output: &::bbnf::runtime::FusedOutput<GoogleSheetsParser>,
        input: &'p str,
        offset: u32,
    ) -> GoogleSheetsParserValue<'p> {
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
        match project_rule_kind_GoogleSheetsParser(__rec.kind(), __rec.variant_idx()) {
            GoogleSheetsParserRuleKind::number => {
                let v: f64 = output
                    .value_frame_at(offset)
                    .and_then(|f| output.value_payload_for(f))
                    .and_then(|p| p.as_f64())
                    .unwrap_or_else(|| {
                        (&input[__rec.span_lo as usize..__rec.span_hi as usize])
                            .parse::<f64>()
                            .unwrap_or(0.0)
                    });
                GoogleSheetsParserValue::number(v)
            }
            GoogleSheetsParserRuleKind::string => {
                let proj = materialize_projection_string_GoogleSheetsParser(
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
                GoogleSheetsParserValue::string(proj)
            }
            GoogleSheetsParserRuleKind::boolean => {
                let proj = materialize_projection_boolean_GoogleSheetsParser(
                        output,
                        input,
                        offset,
                    )
                    .unwrap_or_else(|| {
                        ::core::panic!(
                            "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                            "boolean", offset,
                        );
                    });
                GoogleSheetsParserValue::boolean(proj)
            }
            GoogleSheetsParserRuleKind::error_literal => {
                let proj = materialize_projection_error_literal_GoogleSheetsParser(
                        output,
                        input,
                        offset,
                    )
                    .unwrap_or_else(|| {
                        ::core::panic!(
                            "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                            "error_literal", offset,
                        );
                    });
                GoogleSheetsParserValue::error_literal(proj)
            }
            GoogleSheetsParserRuleKind::sheet_prefix => {
                let proj = materialize_projection_sheet_prefix_GoogleSheetsParser(
                        output,
                        input,
                        offset,
                    )
                    .unwrap_or_else(|| {
                        ::core::panic!(
                            "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                            "sheet_prefix", offset,
                        );
                    });
                GoogleSheetsParserValue::sheet_prefix(proj)
            }
            GoogleSheetsParserRuleKind::cell_ref => {
                let proj = materialize_projection_cell_ref_GoogleSheetsParser(
                        output,
                        input,
                        offset,
                    )
                    .unwrap_or_else(|| {
                        ::core::panic!(
                            "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                            "cell_ref", offset,
                        );
                    });
                GoogleSheetsParserValue::cell_ref(proj)
            }
            GoogleSheetsParserRuleKind::cell => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::cell(children)
            }
            GoogleSheetsParserRuleKind::range_ref => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::range_ref(children)
            }
            GoogleSheetsParserRuleKind::cell_or_range => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::cell_or_range(children)
            }
            GoogleSheetsParserRuleKind::identifier => {
                let span = &input[__rec.span_lo as usize..__rec.span_hi as usize];
                GoogleSheetsParserValue::identifier(span)
            }
            GoogleSheetsParserRuleKind::compare_op => {
                let proj = materialize_projection_compare_op_GoogleSheetsParser(
                        output,
                        input,
                        offset,
                    )
                    .unwrap_or_else(|| {
                        ::core::panic!(
                            "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                            "compare_op", offset,
                        );
                    });
                GoogleSheetsParserValue::compare_op(proj)
            }
            GoogleSheetsParserRuleKind::comparison_expr => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::comparison_expr(children)
            }
            GoogleSheetsParserRuleKind::concat_expr => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::concat_expr(children)
            }
            GoogleSheetsParserRuleKind::add_op => {
                let proj = materialize_projection_add_op_GoogleSheetsParser(
                        output,
                        input,
                        offset,
                    )
                    .unwrap_or_else(|| {
                        ::core::panic!(
                            "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                            "add_op", offset,
                        );
                    });
                GoogleSheetsParserValue::add_op(proj)
            }
            GoogleSheetsParserRuleKind::add_expr => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::add_expr(children)
            }
            GoogleSheetsParserRuleKind::mul_op => {
                let proj = materialize_projection_mul_op_GoogleSheetsParser(
                        output,
                        input,
                        offset,
                    )
                    .unwrap_or_else(|| {
                        ::core::panic!(
                            "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                            "mul_op", offset,
                        );
                    });
                GoogleSheetsParserValue::mul_op(proj)
            }
            GoogleSheetsParserRuleKind::mul_expr => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::mul_expr(children)
            }
            GoogleSheetsParserRuleKind::exp_expr => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::exp_expr(children)
            }
            GoogleSheetsParserRuleKind::unary_prefix => {
                let proj = materialize_projection_unary_prefix_GoogleSheetsParser(
                        output,
                        input,
                        offset,
                    )
                    .unwrap_or_else(|| {
                        ::core::panic!(
                            "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                            "unary_prefix", offset,
                        );
                    });
                GoogleSheetsParserValue::unary_prefix(proj)
            }
            GoogleSheetsParserRuleKind::unary_expr => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::unary_expr(children)
            }
            GoogleSheetsParserRuleKind::postfix_expr => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::postfix_expr(children)
            }
            GoogleSheetsParserRuleKind::primary => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::primary(children)
            }
            GoogleSheetsParserRuleKind::paren_expr => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::paren_expr(children)
            }
            GoogleSheetsParserRuleKind::func_open => {
                let proj = materialize_projection_func_open_GoogleSheetsParser(
                        output,
                        input,
                        offset,
                    )
                    .unwrap_or_else(|| {
                        ::core::panic!(
                            "AY-II.W0'.b: materializer for admitted rule `{}` \
                                 returned None at frame offset {}; admission \
                                 invariant violated",
                            "func_open", offset,
                        );
                    });
                GoogleSheetsParserValue::func_open(proj)
            }
            GoogleSheetsParserRuleKind::arg => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::arg(children)
            }
            GoogleSheetsParserRuleKind::func_args => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::func_args(children)
            }
            GoogleSheetsParserRuleKind::func_call => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::func_call(children)
            }
            GoogleSheetsParserRuleKind::let_binding => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::let_binding(children)
            }
            GoogleSheetsParserRuleKind::let_args => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::let_args(children)
            }
            GoogleSheetsParserRuleKind::let_call => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::let_call(children)
            }
            GoogleSheetsParserRuleKind::lambda_params => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::lambda_params(children)
            }
            GoogleSheetsParserRuleKind::lambda_call => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::lambda_call(children)
            }
            GoogleSheetsParserRuleKind::array_row => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::array_row(children)
            }
            GoogleSheetsParserRuleKind::array_rows => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::array_rows(children)
            }
            GoogleSheetsParserRuleKind::array_literal => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::array_literal(children)
            }
            GoogleSheetsParserRuleKind::formula => {
                let mut children: ::std::vec::Vec<GoogleSheetsParserValue<'p>> = ::std::vec::Vec::new();
                let __cur = ::bbnf::runtime::tape::TapeCursor::new(
                    __tape,
                    ::bbnf::runtime::tape::TapeOffset(offset),
                );
                for __child in __cur.children() {
                    project_push_children_GoogleSheetsParser(
                        output,
                        input,
                        __child.offset().0,
                        &mut children,
                    );
                }
                GoogleSheetsParserValue::formula(children)
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
    fn project_value_GoogleSheetsParser<'p>(
        output: &::bbnf::runtime::FusedOutput<GoogleSheetsParser>,
        input: &'p str,
    ) -> GoogleSheetsParserValue<'p> {
        let root_off = output.value_root_offset();
        project_frame_GoogleSheetsParser(output, input, root_off)
    }
    impl ::bbnf::runtime::ValueRoot for GoogleSheetsParser {
        type Value<'p> = GoogleSheetsParserValue<'p>;
        #[inline]
        fn project_value_output<'p>(
            output: &::bbnf::runtime::FusedOutput<GoogleSheetsParser>,
            input: &'p str,
        ) -> Self::Value<'p>
        where
            Self: 'p,
        {
            project_value_GoogleSheetsParser(output, input)
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
        view: GoogleSheetsParserNodeView<'p>,
        path: ::bbnf::runtime::Path<'_>,
    ) -> ::core::option::Option<GoogleSheetsParserNodeView<'p>> {
        let cur_input = view.input();
        let mut cur = view;
        for seg in path.iter() {
            match seg {
                ::bbnf::runtime::PathSegment::Field(key) => {
                    match cur.rule_kind() {
                        GoogleSheetsParserRuleKind::concat_expr
                        | GoogleSheetsParserRuleKind::add_expr
                        | GoogleSheetsParserRuleKind::mul_expr
                        | GoogleSheetsParserRuleKind::exp_expr
                        | GoogleSheetsParserRuleKind::unary_expr
                        | GoogleSheetsParserRuleKind::postfix_expr
                        | GoogleSheetsParserRuleKind::primary => {
                            let parent = cur.cursor();
                            let (_, parent_end) = parent.span();
                            let mut iter = parent.bounded_lookahead(parent_end);
                            let mut hit: ::core::option::Option<
                                GoogleSheetsParserNodeView<'p>,
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
                                        .map(|c| GoogleSheetsParserNodeView::from_cursor(
                                            c,
                                            cur_input,
                                        ));
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
                        GoogleSheetsParserRuleKind::error_literal
                        | GoogleSheetsParserRuleKind::sheet_prefix
                        | GoogleSheetsParserRuleKind::cell
                        | GoogleSheetsParserRuleKind::range_ref
                        | GoogleSheetsParserRuleKind::cell_or_range
                        | GoogleSheetsParserRuleKind::compare_op
                        | GoogleSheetsParserRuleKind::comparison_expr
                        | GoogleSheetsParserRuleKind::add_op
                        | GoogleSheetsParserRuleKind::mul_op
                        | GoogleSheetsParserRuleKind::unary_prefix
                        | GoogleSheetsParserRuleKind::paren_expr
                        | GoogleSheetsParserRuleKind::func_open
                        | GoogleSheetsParserRuleKind::arg
                        | GoogleSheetsParserRuleKind::func_args
                        | GoogleSheetsParserRuleKind::func_call
                        | GoogleSheetsParserRuleKind::let_binding
                        | GoogleSheetsParserRuleKind::let_args
                        | GoogleSheetsParserRuleKind::lambda_params
                        | GoogleSheetsParserRuleKind::array_row
                        | GoogleSheetsParserRuleKind::array_rows
                        | GoogleSheetsParserRuleKind::array_literal
                        | GoogleSheetsParserRuleKind::formula => {
                            let parent = cur.cursor();
                            let (_, parent_end) = parent.span();
                            let mut iter = parent.bounded_lookahead(parent_end);
                            let mut hit: ::core::option::Option<
                                GoogleSheetsParserNodeView<'p>,
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
                                        GoogleSheetsParserNodeView::from_cursor(v_cur, cur_input),
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
                        GoogleSheetsParserRuleKind::compare_op
                        | GoogleSheetsParserRuleKind::comparison_expr
                        | GoogleSheetsParserRuleKind::concat_expr
                        | GoogleSheetsParserRuleKind::add_expr
                        | GoogleSheetsParserRuleKind::mul_expr
                        | GoogleSheetsParserRuleKind::exp_expr
                        | GoogleSheetsParserRuleKind::unary_expr
                        | GoogleSheetsParserRuleKind::postfix_expr
                        | GoogleSheetsParserRuleKind::primary
                        | GoogleSheetsParserRuleKind::arg
                        | GoogleSheetsParserRuleKind::func_args
                        | GoogleSheetsParserRuleKind::let_binding
                        | GoogleSheetsParserRuleKind::let_args
                        | GoogleSheetsParserRuleKind::lambda_params
                        | GoogleSheetsParserRuleKind::array_row
                        | GoogleSheetsParserRuleKind::array_rows => {
                            let parent = cur.cursor();
                            let (_, parent_end) = parent.span();
                            let scan = parent.scan_structural_bounded(parent_end);
                            cur = match scan.iter().nth(*i) {
                                ::core::option::Option::Some(c) => {
                                    GoogleSheetsParserNodeView::from_cursor(c, cur_input)
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
    impl ::bbnf::runtime::PathQuery<&'static str> for GoogleSheetsParser {
        #[inline]
        fn query<'p>(
            view: Self::View<'p>,
            path: ::bbnf::runtime::Path<'_>,
        ) -> ::core::option::Option<&'static str>
        where
            Self: 'p,
        {
            let node = GoogleSheetsParserNodeView::from_cursor(
                view.cursor(),
                view.input(),
            );
            __path_walk(node, path)?;
            ::core::option::Option::None
        }
    }
    impl ::bbnf::runtime::PathQuery<f64> for GoogleSheetsParser {
        #[inline]
        fn query<'p>(
            view: Self::View<'p>,
            path: ::bbnf::runtime::Path<'_>,
        ) -> ::core::option::Option<f64>
        where
            Self: 'p,
        {
            let node = GoogleSheetsParserNodeView::from_cursor(
                view.cursor(),
                view.input(),
            );
            let hit = __path_walk(node, path)?;
            let tape = hit.cursor().tape();
            let rec = hit.cursor().record();
            if let ::core::option::Option::Some(v) = tape.payload_f64(rec) {
                return ::core::option::Option::Some(v);
            }
            hit.span_text().parse::<f64>().ok()
        }
    }
    impl ::bbnf::runtime::PathQuery<bool> for GoogleSheetsParser {
        #[inline]
        fn query<'p>(
            view: Self::View<'p>,
            path: ::bbnf::runtime::Path<'_>,
        ) -> ::core::option::Option<bool>
        where
            Self: 'p,
        {
            let node = GoogleSheetsParserNodeView::from_cursor(
                view.cursor(),
                view.input(),
            );
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
    pub fn materialize_projection_string_GoogleSheetsParser<'p>(
        output: &::bbnf::runtime::FusedOutput<GoogleSheetsParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<GoogleSheetsParserStringProjection> {
        let _ = input;
        let frame = output.value_frame_at(offset)?;
        let __bytes: &[u8] = &[];
        let _ = __bytes;
        let field_0: (u32, u32) = (frame.span_lo, frame.span_hi);
        ::core::option::Option::Some(GoogleSheetsParserStringProjection {
            field_0,
        })
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
    pub fn materialize_projection_boolean_GoogleSheetsParser<'p>(
        output: &::bbnf::runtime::FusedOutput<GoogleSheetsParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<GoogleSheetsParserBooleanProjection> {
        let _ = input;
        let frame = output.value_frame_at(offset)?;
        let __tape = output.tape();
        let __tape_rec = __tape.try_get(::bbnf::runtime::tape::TapeOffset(offset))?;
        let __bytes = __tape.payload_bytes(__tape_rec, 1)?;
        let field_0: bool = {
            let __b = *__bytes.get(0)?;
            let _ = 1;
            __b != 0
        };
        ::core::option::Option::Some(GoogleSheetsParserBooleanProjection {
            field_0,
        })
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
    pub fn materialize_projection_error_literal_GoogleSheetsParser<'p>(
        output: &::bbnf::runtime::FusedOutput<GoogleSheetsParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<GoogleSheetsParserErrorLiteralProjection> {
        let _ = input;
        let frame = output.value_frame_at(offset)?;
        let __tape = output.tape();
        let __tape_rec = __tape.try_get(::bbnf::runtime::tape::TapeOffset(offset))?;
        let __bytes = __tape.payload_bytes(__tape_rec, 1)?;
        let field_0: u8 = {
            let __b = *__bytes.get(0)?;
            let _ = 1;
            __b as u8
        };
        ::core::option::Option::Some(GoogleSheetsParserErrorLiteralProjection {
            field_0,
        })
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
    pub fn materialize_projection_sheet_prefix_GoogleSheetsParser<'p>(
        output: &::bbnf::runtime::FusedOutput<GoogleSheetsParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<GoogleSheetsParserSheetPrefixProjection> {
        let _ = input;
        let frame = output.value_frame_at(offset)?;
        let __tape = output.tape();
        let __tape_rec = __tape.try_get(::bbnf::runtime::tape::TapeOffset(offset))?;
        let __bytes = __tape.payload_bytes(__tape_rec, 1)?;
        let field_0: u8 = {
            let __b = *__bytes.get(0)?;
            let _ = 1;
            __b as u8
        };
        ::core::option::Option::Some(GoogleSheetsParserSheetPrefixProjection {
            field_0,
        })
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
    pub fn materialize_projection_cell_ref_GoogleSheetsParser<'p>(
        output: &::bbnf::runtime::FusedOutput<GoogleSheetsParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<GoogleSheetsParserCellRefProjection> {
        let _ = input;
        let frame = output.value_frame_at(offset)?;
        let __bytes: &[u8] = &[];
        let _ = __bytes;
        let field_0: (u32, u32) = (frame.span_lo, frame.span_hi);
        ::core::option::Option::Some(GoogleSheetsParserCellRefProjection {
            field_0,
        })
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
    pub fn materialize_projection_compare_op_GoogleSheetsParser<'p>(
        output: &::bbnf::runtime::FusedOutput<GoogleSheetsParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<GoogleSheetsParserCompareOpProjection> {
        let _ = input;
        let frame = output.value_frame_at(offset)?;
        let __tape = output.tape();
        let __tape_rec = __tape.try_get(::bbnf::runtime::tape::TapeOffset(offset))?;
        let __bytes = __tape.payload_bytes(__tape_rec, 1)?;
        let field_0: u8 = {
            let __b = *__bytes.get(0)?;
            let _ = 1;
            __b as u8
        };
        ::core::option::Option::Some(GoogleSheetsParserCompareOpProjection {
            field_0,
        })
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
    pub fn materialize_projection_add_op_GoogleSheetsParser<'p>(
        output: &::bbnf::runtime::FusedOutput<GoogleSheetsParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<GoogleSheetsParserAddOpProjection> {
        let _ = input;
        let frame = output.value_frame_at(offset)?;
        let __tape = output.tape();
        let __tape_rec = __tape.try_get(::bbnf::runtime::tape::TapeOffset(offset))?;
        let __bytes = __tape.payload_bytes(__tape_rec, 1)?;
        let field_0: u8 = {
            let __b = *__bytes.get(0)?;
            let _ = 1;
            __b as u8
        };
        ::core::option::Option::Some(GoogleSheetsParserAddOpProjection {
            field_0,
        })
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
    pub fn materialize_projection_mul_op_GoogleSheetsParser<'p>(
        output: &::bbnf::runtime::FusedOutput<GoogleSheetsParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<GoogleSheetsParserMulOpProjection> {
        let _ = input;
        let frame = output.value_frame_at(offset)?;
        let __tape = output.tape();
        let __tape_rec = __tape.try_get(::bbnf::runtime::tape::TapeOffset(offset))?;
        let __bytes = __tape.payload_bytes(__tape_rec, 1)?;
        let field_0: u8 = {
            let __b = *__bytes.get(0)?;
            let _ = 1;
            __b as u8
        };
        ::core::option::Option::Some(GoogleSheetsParserMulOpProjection {
            field_0,
        })
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
    pub fn materialize_projection_unary_prefix_GoogleSheetsParser<'p>(
        output: &::bbnf::runtime::FusedOutput<GoogleSheetsParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<GoogleSheetsParserUnaryPrefixProjection> {
        let _ = input;
        let frame = output.value_frame_at(offset)?;
        let __tape = output.tape();
        let __tape_rec = __tape.try_get(::bbnf::runtime::tape::TapeOffset(offset))?;
        let __bytes = __tape.payload_bytes(__tape_rec, 1)?;
        let field_0: u8 = {
            let __b = *__bytes.get(0)?;
            let _ = 1;
            __b as u8
        };
        ::core::option::Option::Some(GoogleSheetsParserUnaryPrefixProjection {
            field_0,
        })
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
    pub fn materialize_projection_func_open_GoogleSheetsParser<'p>(
        output: &::bbnf::runtime::FusedOutput<GoogleSheetsParser>,
        input: &'p str,
        offset: u32,
    ) -> ::core::option::Option<GoogleSheetsParserFuncOpenProjection> {
        let _ = input;
        let frame = output.value_frame_at(offset)?;
        let __bytes: &[u8] = &[];
        let _ = __bytes;
        let field_0: (u32, u32) = (frame.span_lo, frame.span_hi);
        ::core::option::Option::Some(GoogleSheetsParserFuncOpenProjection {
            field_0,
        })
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
                    let __byte = match state.src_bytes.get(state.offset) {
                        Some(&b) => b,
                        None => return false,
                    };
                    match __byte {
                        b'\'' => {
                            {
                                let __start = state.offset;
                                if {
                                    let __start = state.offset;
                                    let __result: Option<()> = (|| {
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
                        b'A' | b'B' | b'C' | b'D' | b'E' | b'F' | b'G' | b'H' | b'I'
                        | b'J' | b'K' | b'L' | b'M' | b'N' | b'O' | b'P' | b'Q' | b'R'
                        | b'S' | b'T' | b'U' | b'V' | b'W' | b'X' | b'Y' | b'Z' | b'_'
                        | b'a' | b'b' | b'c' | b'd' | b'e' | b'f' | b'g' | b'h' | b'i'
                        | b'j' | b'k' | b'l' | b'm' | b'n' | b'o' | b'p' | b'q' | b'r'
                        | b's' | b't' | b'u' | b'v' | b'w' | b'x' | b'y' | b'z' => {
                            {
                                let __start = state.offset;
                                if {
                                    let __start = state.offset;
                                    let __result: Option<()> = (|| {
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
        fn __cell_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let _ = {
                            let __pretty_cp11 = state.offset;
                            let __pretty_bcp12 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                if !Self::__sheet_prefix_prettify(state, __builder) {
                                    return false;
                                }
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp11;
                                __builder.restore(__pretty_bcp12);
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
        fn __range_ref_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let _ = {
                            let __pretty_cp13 = state.offset;
                            let __pretty_bcp14 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                if !Self::__sheet_prefix_prettify(state, __builder) {
                                    return false;
                                }
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp13;
                                __builder.restore(__pretty_bcp14);
                            }
                            __ok
                        };
                        true
                    };
                    {
                        if !{
                            let __pretty_cp17 = state.offset;
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
                                state.offset = __pretty_cp17;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp16 = state.offset;
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
                                        state.offset = __pretty_cp16;
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp15 = state.offset;
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
                                                state.offset = __pretty_cp15;
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
                            let __pretty_cp20 = state.offset;
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
                                state.offset = __pretty_cp20;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp19 = state.offset;
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
                                        state.offset = __pretty_cp19;
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp18 = state.offset;
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
                                                state.offset = __pretty_cp18;
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
                        let __pretty_cp21 = state.offset;
                        let __pretty_bcp22 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__range_ref_prettify(state, __builder) {
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
        fn __identifier_prettify<'a>(
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
                                    if (__b == b'.' || (__b >= b'0' && __b <= b'9')
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
                        let __pretty_cp27 = state.offset;
                        let __pretty_bcp28 = __builder.checkpoint();
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
                            state.offset = __pretty_cp27;
                            __builder.restore(__pretty_bcp28);
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp26 = state.offset;
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
                                    state.offset = __pretty_cp26;
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp25 = state.offset;
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
                                            state.offset = __pretty_cp25;
                                        }
                                        __ok
                                    } {
                                        {
                                            if !{
                                                let __pretty_cp24 = state.offset;
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
                                                    state.offset = __pretty_cp24;
                                                }
                                                __ok
                                            } {
                                                {
                                                    if !{
                                                        let __pretty_cp23 = state.offset;
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
                                                            state.offset = __pretty_cp23;
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
        fn __concat_expr_prettify<'a>(
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
                                    if !Self::__add_expr_prettify(state, __builder) {
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
                        let mut __rep_count58 = 0usize;
                        while __rep_count58 < 4294967295 {
                            let __rep_cp59 = state.offset;
                            if !{
                                let __pretty_cp56 = state.offset;
                                let __pretty_bcp57 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            let __ows49 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            let __ows50 = state.offset;
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'&')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b'&');
                                            };
                                            __builder.text_inline_ws(&state.src[__ows49..__ows50]);
                                            let __ows51 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder.text_inline_ws(&state.src[__ows51..state.offset]);
                                        };
                                        {
                                            if !{
                                                let __pretty_cp54 = state.offset;
                                                let __pretty_bcp55 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows52 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows52..state.offset]);
                                                        if !Self::__add_expr_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        let __ows53 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows53..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp54;
                                                    __builder.restore(__pretty_bcp55);
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
                                    state.offset = __pretty_cp56;
                                    __builder.restore(__pretty_bcp57);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp59;
                                break;
                            }
                            if state.offset == __rep_cp59 {
                                break;
                            }
                            __rep_count58 += 1;
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
        fn __add_expr_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        if !{
                            let __pretty_cp62 = state.offset;
                            let __pretty_bcp63 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows60 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder.text_inline_ws(&state.src[__ows60..state.offset]);
                                    if !Self::__mul_expr_prettify(state, __builder) {
                                        return false;
                                    }
                                    let __ows61 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder.text_inline_ws(&state.src[__ows61..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp62;
                                __builder.restore(__pretty_bcp63);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        let mut __rep_count74 = 0usize;
                        while __rep_count74 < 4294967295 {
                            let __rep_cp75 = state.offset;
                            if !{
                                let __pretty_cp72 = state.offset;
                                let __pretty_bcp73 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            if !{
                                                let __pretty_cp66 = state.offset;
                                                let __pretty_bcp67 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows64 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows64..state.offset]);
                                                        if !Self::__add_op_prettify(state, __builder) {
                                                            return false;
                                                        }
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
                                            if !{
                                                let __pretty_cp70 = state.offset;
                                                let __pretty_bcp71 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows68 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows68..state.offset]);
                                                        if !Self::__mul_expr_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        let __ows69 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows69..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp70;
                                                    __builder.restore(__pretty_bcp71);
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
                                    state.offset = __pretty_cp72;
                                    __builder.restore(__pretty_bcp73);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp75;
                                break;
                            }
                            if state.offset == __rep_cp75 {
                                break;
                            }
                            __rep_count74 += 1;
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
        fn __mul_expr_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        if !{
                            let __pretty_cp78 = state.offset;
                            let __pretty_bcp79 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows76 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder.text_inline_ws(&state.src[__ows76..state.offset]);
                                    if !Self::__exp_expr_prettify(state, __builder) {
                                        return false;
                                    }
                                    let __ows77 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder.text_inline_ws(&state.src[__ows77..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp78;
                                __builder.restore(__pretty_bcp79);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        let mut __rep_count90 = 0usize;
                        while __rep_count90 < 4294967295 {
                            let __rep_cp91 = state.offset;
                            if !{
                                let __pretty_cp88 = state.offset;
                                let __pretty_bcp89 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            if !{
                                                let __pretty_cp82 = state.offset;
                                                let __pretty_bcp83 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows80 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows80..state.offset]);
                                                        if !Self::__mul_op_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        let __ows81 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows81..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp82;
                                                    __builder.restore(__pretty_bcp83);
                                                }
                                                __ok
                                            } {
                                                return false;
                                            }
                                        };
                                        {
                                            if !{
                                                let __pretty_cp86 = state.offset;
                                                let __pretty_bcp87 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows84 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows84..state.offset]);
                                                        if !Self::__exp_expr_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        let __ows85 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows85..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp86;
                                                    __builder.restore(__pretty_bcp87);
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
                                    state.offset = __pretty_cp88;
                                    __builder.restore(__pretty_bcp89);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp91;
                                break;
                            }
                            if state.offset == __rep_cp91 {
                                break;
                            }
                            __rep_count90 += 1;
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
        fn __exp_expr_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        if !{
                            let __pretty_cp94 = state.offset;
                            let __pretty_bcp95 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows92 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder.text_inline_ws(&state.src[__ows92..state.offset]);
                                    if !Self::__unary_expr_prettify(state, __builder) {
                                        return false;
                                    }
                                    let __ows93 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder.text_inline_ws(&state.src[__ows93..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp94;
                                __builder.restore(__pretty_bcp95);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        let mut __rep_count105 = 0usize;
                        while __rep_count105 < 4294967295 {
                            let __rep_cp106 = state.offset;
                            if !{
                                let __pretty_cp103 = state.offset;
                                let __pretty_bcp104 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            let __ows96 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            let __ows97 = state.offset;
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'^')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b'^');
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
                                                        if !Self::__unary_expr_prettify(state, __builder) {
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
        fn __unary_expr_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let mut __rep_count109 = 0usize;
                        while __rep_count109 < 4294967295 {
                            let __rep_cp110 = state.offset;
                            if !{
                                let __pretty_cp107 = state.offset;
                                let __pretty_bcp108 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    if !Self::__unary_prefix_prettify(state, __builder) {
                                        return false;
                                    }
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp107;
                                    __builder.restore(__pretty_bcp108);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp110;
                                break;
                            }
                            if state.offset == __rep_cp110 {
                                break;
                            }
                            __rep_count109 += 1;
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
                        let mut __rep_count113 = 0usize;
                        while __rep_count113 < 4294967295 {
                            let __rep_cp114 = state.offset;
                            if !{
                                let __pretty_cp111 = state.offset;
                                let __pretty_bcp112 = __builder.checkpoint();
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
                                    state.offset = __pretty_cp111;
                                    __builder.restore(__pretty_bcp112);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp114;
                                break;
                            }
                            if state.offset == __rep_cp114 {
                                break;
                            }
                            __rep_count113 += 1;
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
        fn __primary_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp132 = state.offset;
                        let __pretty_bcp133 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__let_call_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp132;
                            __builder.restore(__pretty_bcp133);
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp130 = state.offset;
                                let __pretty_bcp131 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    if !Self::__lambda_call_prettify(state, __builder) {
                                        return false;
                                    }
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp130;
                                    __builder.restore(__pretty_bcp131);
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp128 = state.offset;
                                        let __pretty_bcp129 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            if !Self::__func_call_prettify(state, __builder) {
                                                return false;
                                            }
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp128;
                                            __builder.restore(__pretty_bcp129);
                                        }
                                        __ok
                                    } {
                                        {
                                            if !{
                                                let __pretty_cp127 = state.offset;
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
                                                    state.offset = __pretty_cp127;
                                                }
                                                __ok
                                            } {
                                                {
                                                    if !{
                                                        let __pretty_cp125 = state.offset;
                                                        let __pretty_bcp126 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            if !Self::__boolean_prettify(state, __builder) {
                                                                return false;
                                                            }
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp125;
                                                            __builder.restore(__pretty_bcp126);
                                                        }
                                                        __ok
                                                    } {
                                                        {
                                                            if !{
                                                                let __pretty_cp123 = state.offset;
                                                                let __pretty_bcp124 = __builder.checkpoint();
                                                                let __ok = (|| -> bool {
                                                                    if !Self::__cell_or_range_prettify(state, __builder) {
                                                                        return false;
                                                                    }
                                                                    true
                                                                })();
                                                                if !__ok {
                                                                    state.offset = __pretty_cp123;
                                                                    __builder.restore(__pretty_bcp124);
                                                                }
                                                                __ok
                                                            } {
                                                                {
                                                                    if !{
                                                                        let __pretty_cp122 = state.offset;
                                                                        let __ok = (|| -> bool {
                                                                            {
                                                                                let __start = state.offset;
                                                                                if {
                                                                                    let __start = state.offset;
                                                                                    let __result: Option<()> = (|| {
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
                                                                                                if (__b == b'.' || (__b >= b'0' && __b <= b'9')
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
                                                                            true
                                                                        })();
                                                                        if !__ok {
                                                                            state.offset = __pretty_cp122;
                                                                        }
                                                                        __ok
                                                                    } {
                                                                        {
                                                                            if !{
                                                                                let __pretty_cp121 = state.offset;
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
                                                                                    state.offset = __pretty_cp121;
                                                                                }
                                                                                __ok
                                                                            } {
                                                                                {
                                                                                    if !{
                                                                                        let __pretty_cp119 = state.offset;
                                                                                        let __pretty_bcp120 = __builder.checkpoint();
                                                                                        let __ok = (|| -> bool {
                                                                                            if !Self::__error_literal_prettify(state, __builder) {
                                                                                                return false;
                                                                                            }
                                                                                            true
                                                                                        })();
                                                                                        if !__ok {
                                                                                            state.offset = __pretty_cp119;
                                                                                            __builder.restore(__pretty_bcp120);
                                                                                        }
                                                                                        __ok
                                                                                    } {
                                                                                        {
                                                                                            if !{
                                                                                                let __pretty_cp117 = state.offset;
                                                                                                let __pretty_bcp118 = __builder.checkpoint();
                                                                                                let __ok = (|| -> bool {
                                                                                                    if !Self::__array_literal_prettify(state, __builder) {
                                                                                                        return false;
                                                                                                    }
                                                                                                    true
                                                                                                })();
                                                                                                if !__ok {
                                                                                                    state.offset = __pretty_cp117;
                                                                                                    __builder.restore(__pretty_bcp118);
                                                                                                }
                                                                                                __ok
                                                                                            } {
                                                                                                {
                                                                                                    if !{
                                                                                                        let __pretty_cp115 = state.offset;
                                                                                                        let __pretty_bcp116 = __builder.checkpoint();
                                                                                                        let __ok = (|| -> bool {
                                                                                                            if !Self::__paren_expr_prettify(state, __builder) {
                                                                                                                return false;
                                                                                                            }
                                                                                                            true
                                                                                                        })();
                                                                                                        if !__ok {
                                                                                                            state.offset = __pretty_cp115;
                                                                                                            __builder.restore(__pretty_bcp116);
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
                            let __pretty_cp136 = state.offset;
                            let __pretty_bcp137 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows134 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows134..state.offset]);
                                    if !Self::__comparison_expr_prettify(state, __builder) {
                                        return false;
                                    }
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
        fn __func_open_prettify<'a>(
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
                                        if (__b == b'.' || (__b >= b'0' && __b <= b'9')
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
        fn __arg_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let _ = {
                        let __pretty_cp138 = state.offset;
                        let __pretty_bcp139 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__comparison_expr_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp138;
                            __builder.restore(__pretty_bcp139);
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
                        let __rep_start150 = state.offset;
                        let __rep_bcp151 = __builder.checkpoint();
                        let mut __rep_count148 = 0usize;
                        while __rep_count148 < 4294967295 {
                            let __rep_cp149 = state.offset;
                            let __iter_cp = if __rep_count148 > 0 {
                                Some(__builder.checkpoint())
                            } else {
                                None
                            };
                            if __rep_count148 > 0 {
                                __builder.sep(", ", "");
                            }
                            if !{
                                let __pretty_cp147 = state.offset;
                                let __ok = (|| -> bool {
                                    {
                                        if !Self::__arg_prettify(state, __builder) {
                                            return false;
                                        }
                                        {
                                            let __silent_cp145 = state.offset;
                                            let __silent_bcp146 = __builder.light_checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let _ = {
                                                        let __pretty_cp143 = state.offset;
                                                        let __pretty_bcp144 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            {
                                                                let __ows140 = state.offset;
                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                let __ows141 = state.offset;
                                                                {
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                    {
                                                                        return false;
                                                                    }
                                                                    state.offset += 1;
                                                                    __builder.char(b',');
                                                                };
                                                                __builder.text_inline_ws(&state.src[__ows140..__ows141]);
                                                                let __ows142 = state.offset;
                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                __builder
                                                                    .text_inline_ws(&state.src[__ows142..state.offset]);
                                                            };
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp143;
                                                            __builder.restore(__pretty_bcp144);
                                                        }
                                                        __ok
                                                    };
                                                    true
                                                };
                                                true
                                            })();
                                            __builder.light_restore(__silent_bcp146);
                                            if !__ok {
                                                state.offset = __silent_cp145;
                                                return false;
                                            }
                                        };
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp147;
                                }
                                __ok
                            } {
                                state.offset = __rep_cp149;
                                if let Some(__bcp) = __iter_cp {
                                    __builder.restore(__bcp);
                                }
                                break;
                            }
                            if state.offset == __rep_cp149 {
                                if let Some(__bcp) = __iter_cp {
                                    __builder.restore(__bcp);
                                }
                                break;
                            }
                            __rep_count148 += 1;
                        }
                        if __rep_count148 < 1 {
                            state.offset = __rep_start150;
                            __builder.restore(__rep_bcp151);
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
                                let __pretty_cp156 = state.offset;
                                let __pretty_bcp157 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        let __ows154 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows154..state.offset]);
                                        {
                                            let _ = {
                                                let __pretty_cp152 = state.offset;
                                                let __pretty_bcp153 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    if !Self::__func_args_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp152;
                                                    __builder.restore(__pretty_bcp153);
                                                }
                                                __ok
                                            };
                                            true
                                        };
                                        let __ows155 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows155..state.offset]);
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp156;
                                    __builder.restore(__pretty_bcp157);
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
                            if !Self::__comparison_expr_prettify(state, __builder) {
                                return false;
                            }
                            {
                                let __ows158 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                let __ows159 = state.offset;
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b',')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b',');
                                };
                                __builder.text_inline_ws(&state.src[__ows158..__ows159]);
                                let __ows160 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows160..state.offset]);
                            };
                        };
                        if !Self::__comparison_expr_prettify(state, __builder) {
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
        fn __let_args_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let mut __rep_count165 = 0usize;
                        while __rep_count165 < 4294967295 {
                            let __rep_cp166 = state.offset;
                            let __iter_cp = if __rep_count165 > 0 {
                                Some(__builder.checkpoint())
                            } else {
                                None
                            };
                            if __rep_count165 > 0 {
                                __builder.hardline();
                            }
                            if !{
                                let __pretty_cp164 = state.offset;
                                let __ok = (|| -> bool {
                                    {
                                        if !Self::__let_binding_prettify(state, __builder) {
                                            return false;
                                        }
                                        {
                                            let __ows161 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            let __ows162 = state.offset;
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b',');
                                            };
                                            __builder.text_inline_ws(&state.src[__ows161..__ows162]);
                                            let __ows163 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder
                                                .text_inline_ws(&state.src[__ows163..state.offset]);
                                        };
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp164;
                                }
                                __ok
                            } {
                                state.offset = __rep_cp166;
                                if let Some(__bcp) = __iter_cp {
                                    __builder.restore(__bcp);
                                }
                                break;
                            }
                            if state.offset == __rep_cp166 {
                                if let Some(__bcp) = __iter_cp {
                                    __builder.restore(__bcp);
                                }
                                break;
                            }
                            __rep_count165 += 1;
                        }
                    };
                    if !Self::__comparison_expr_prettify(state, __builder) {
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
                                let __pretty_cp169 = state.offset;
                                let __pretty_bcp170 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        let __ows167 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows167..state.offset]);
                                        if !Self::__let_args_prettify(state, __builder) {
                                            return false;
                                        }
                                        let __ows168 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows168..state.offset]);
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp169;
                                    __builder.restore(__pretty_bcp170);
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
        fn __lambda_params_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            __builder.indent_open();
            let __pretty_ok = {
                {
                    {
                        let __rep_start181 = state.offset;
                        let __rep_bcp182 = __builder.checkpoint();
                        let mut __rep_count179 = 0usize;
                        while __rep_count179 < 4294967295 {
                            let __rep_cp180 = state.offset;
                            let __iter_cp = if __rep_count179 > 0 {
                                Some(__builder.checkpoint())
                            } else {
                                None
                            };
                            if __rep_count179 > 0 {
                                __builder.sep(", ", "");
                            }
                            if !{
                                let __pretty_cp178 = state.offset;
                                let __ok = (|| -> bool {
                                    {
                                        if !Self::__comparison_expr_prettify(state, __builder) {
                                            return false;
                                        }
                                        {
                                            let __silent_cp176 = state.offset;
                                            let __silent_bcp177 = __builder.light_checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let _ = {
                                                        let __pretty_cp174 = state.offset;
                                                        let __pretty_bcp175 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            {
                                                                let __ows171 = state.offset;
                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                let __ows172 = state.offset;
                                                                {
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                    {
                                                                        return false;
                                                                    }
                                                                    state.offset += 1;
                                                                    __builder.char(b',');
                                                                };
                                                                __builder.text_inline_ws(&state.src[__ows171..__ows172]);
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
                                                    };
                                                    true
                                                };
                                                true
                                            })();
                                            __builder.light_restore(__silent_bcp177);
                                            if !__ok {
                                                state.offset = __silent_cp176;
                                                return false;
                                            }
                                        };
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp178;
                                }
                                __ok
                            } {
                                state.offset = __rep_cp180;
                                if let Some(__bcp) = __iter_cp {
                                    __builder.restore(__bcp);
                                }
                                break;
                            }
                            if state.offset == __rep_cp180 {
                                if let Some(__bcp) = __iter_cp {
                                    __builder.restore(__bcp);
                                }
                                break;
                            }
                            __rep_count179 += 1;
                        }
                        if __rep_count179 < 1 {
                            state.offset = __rep_start181;
                            __builder.restore(__rep_bcp182);
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
                                let __pretty_cp185 = state.offset;
                                let __pretty_bcp186 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        let __ows183 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows183..state.offset]);
                                        if !Self::__lambda_params_prettify(state, __builder) {
                                            return false;
                                        }
                                        let __ows184 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows184..state.offset]);
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp185;
                                    __builder.restore(__pretty_bcp186);
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
        fn __array_row_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !Self::__comparison_expr_prettify(state, __builder) {
                        return false;
                    }
                    {
                        let mut __rep_count194 = 0usize;
                        while __rep_count194 < 4294967295 {
                            let __rep_cp195 = state.offset;
                            if !{
                                let __pretty_cp192 = state.offset;
                                let __pretty_bcp193 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        __builder.sep(", ", "");
                                        {
                                            let __silent_cp190 = state.offset;
                                            let __silent_bcp191 = __builder.light_checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows187 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    let __ows188 = state.offset;
                                                    {
                                                        if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                        {
                                                            return false;
                                                        }
                                                        state.offset += 1;
                                                        __builder.char(b',');
                                                    };
                                                    __builder.text_inline_ws(&state.src[__ows187..__ows188]);
                                                    let __ows189 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows189..state.offset]);
                                                };
                                                true
                                            })();
                                            __builder.light_restore(__silent_bcp191);
                                            if !__ok {
                                                state.offset = __silent_cp190;
                                                return false;
                                            }
                                        };
                                        if !Self::__comparison_expr_prettify(state, __builder) {
                                            return false;
                                        }
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp192;
                                    __builder.restore(__pretty_bcp193);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp195;
                                break;
                            }
                            if state.offset == __rep_cp195 {
                                break;
                            }
                            __rep_count194 += 1;
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
                            let mut __rep_count203 = 0usize;
                            while __rep_count203 < 4294967295 {
                                let __rep_cp204 = state.offset;
                                if !{
                                    let __pretty_cp201 = state.offset;
                                    let __pretty_bcp202 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            __builder.sep("; ", "");
                                            {
                                                let __silent_cp199 = state.offset;
                                                let __silent_bcp200 = __builder.light_checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows196 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        let __ows197 = state.offset;
                                                        {
                                                            if state.src_bytes.get(state.offset).copied() != Some(b';')
                                                            {
                                                                return false;
                                                            }
                                                            state.offset += 1;
                                                            __builder.char(b';');
                                                        };
                                                        __builder.text_inline_ws(&state.src[__ows196..__ows197]);
                                                        let __ows198 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows198..state.offset]);
                                                    };
                                                    true
                                                })();
                                                __builder.light_restore(__silent_bcp200);
                                                if !__ok {
                                                    state.offset = __silent_cp199;
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
                                        state.offset = __pretty_cp201;
                                        __builder.restore(__pretty_bcp202);
                                    }
                                    __ok
                                } {
                                    state.offset = __rep_cp204;
                                    break;
                                }
                                if state.offset == __rep_cp204 {
                                    break;
                                }
                                __rep_count203 += 1;
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
                                let __pretty_cp207 = state.offset;
                                let __pretty_bcp208 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        let __ows205 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows205..state.offset]);
                                        if !Self::__array_rows_prettify(state, __builder) {
                                            return false;
                                        }
                                        let __ows206 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows206..state.offset]);
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp207;
                                    __builder.restore(__pretty_bcp208);
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
                    if !Self::__comparison_expr_prettify(state, __builder) {
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
        pub fn serialize_number<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_string<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_boolean<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_error_literal<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_sheet_prefix<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_cell_ref<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_cell<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_range_ref<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_cell_or_range<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_identifier<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_compare_op<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_comparison_expr<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_concat_expr<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_add_op<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_add_expr<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_mul_op<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_mul_expr<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_exp_expr<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_unary_prefix<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_unary_expr<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_postfix_expr<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_primary<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_paren_expr<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_func_open<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_arg<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_func_args<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_func_call<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_let_binding<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_let_args<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_let_call<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_lambda_params<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_lambda_call<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_array_row<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_array_rows<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_array_literal<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        pub fn serialize_formula<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            __ser.text(__v.span_text());
        }
        fn __dispatch_serialize<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            match __v.variant_idx() {
                0u8 => {
                    Self::serialize_number(__v, __ser);
                }
                1u8 => {
                    Self::serialize_string(__v, __ser);
                }
                2u8 => {
                    Self::serialize_boolean(__v, __ser);
                }
                3u8 => {
                    Self::serialize_error_literal(__v, __ser);
                }
                4u8 => {
                    Self::serialize_sheet_prefix(__v, __ser);
                }
                5u8 => {
                    Self::serialize_cell_ref(__v, __ser);
                }
                6u8 => {
                    Self::serialize_cell(__v, __ser);
                }
                7u8 => {
                    Self::serialize_range_ref(__v, __ser);
                }
                8u8 => {
                    Self::serialize_cell_or_range(__v, __ser);
                }
                9u8 => {
                    Self::serialize_identifier(__v, __ser);
                }
                10u8 => {
                    Self::serialize_compare_op(__v, __ser);
                }
                11u8 => {
                    Self::serialize_comparison_expr(__v, __ser);
                }
                12u8 => {
                    Self::serialize_concat_expr(__v, __ser);
                }
                13u8 => {
                    Self::serialize_add_op(__v, __ser);
                }
                14u8 => {
                    Self::serialize_add_expr(__v, __ser);
                }
                15u8 => {
                    Self::serialize_mul_op(__v, __ser);
                }
                16u8 => {
                    Self::serialize_mul_expr(__v, __ser);
                }
                17u8 => {
                    Self::serialize_exp_expr(__v, __ser);
                }
                18u8 => {
                    Self::serialize_unary_prefix(__v, __ser);
                }
                19u8 => {
                    Self::serialize_unary_expr(__v, __ser);
                }
                20u8 => {
                    Self::serialize_postfix_expr(__v, __ser);
                }
                21u8 => {
                    Self::serialize_primary(__v, __ser);
                }
                22u8 => {
                    Self::serialize_paren_expr(__v, __ser);
                }
                23u8 => {
                    Self::serialize_func_open(__v, __ser);
                }
                24u8 => {
                    Self::serialize_arg(__v, __ser);
                }
                25u8 => {
                    Self::serialize_func_args(__v, __ser);
                }
                26u8 => {
                    Self::serialize_func_call(__v, __ser);
                }
                27u8 => {
                    Self::serialize_let_binding(__v, __ser);
                }
                28u8 => {
                    Self::serialize_let_args(__v, __ser);
                }
                29u8 => {
                    Self::serialize_let_call(__v, __ser);
                }
                30u8 => {
                    Self::serialize_lambda_params(__v, __ser);
                }
                31u8 => {
                    Self::serialize_lambda_call(__v, __ser);
                }
                32u8 => {
                    Self::serialize_array_row(__v, __ser);
                }
                33u8 => {
                    Self::serialize_array_rows(__v, __ser);
                }
                34u8 => {
                    Self::serialize_array_literal(__v, __ser);
                }
                35u8 => {
                    Self::serialize_formula(__v, __ser);
                }
                _ => {
                    __ser.text(__v.span_text());
                }
            }
        }
        pub fn serialize_compact<'a>(__v: GoogleSheetsParserNodeView<'a>) -> String {
            let mut __ser = ::bbnf_ser::StringSerializer::new();
            Self::serialize_formula(__v, &mut __ser);
            __ser.finish()
        }
        pub fn serialize<'a, __S: ::bbnf_ser::Serializer<'a>>(
            __v: GoogleSheetsParserNodeView<'a>,
            __ser: &mut __S,
        ) {
            Self::serialize_formula(__v, __ser);
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
            let mut state = __shape_support_GoogleSheetsParser::ScanState::new();
            let mut builder = ::bbnf::runtime::tape::FusedBuilder::with_capacity(
                GRAMMAR_PROFILE.capacity_for(input.len()),
            );
            let root_off = {
                let mut pos: usize = 0;
                let off = parse_GoogleSheetsParser_formula(
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
                let _ = __shape_support_GoogleSheetsParser::skip_space(
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
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    ) -> &'p str {
        match cst_find_identifier_cursor(cursor, 9u8) {
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
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        _input: &'p str,
    ) -> (u32, u32) {
        cst_find_identifier_cursor(cursor, 9u8).map(|c| c.span()).unwrap_or((0, 0))
    }
    /// DFS helper shared by `cst_identifier_text` and
    /// `cst_identifier_span`. Returns the first cursor under
    /// `start` whose `variant_idx` matches `target_idx`.
    #[inline]
    fn cst_find_identifier_cursor<'p>(
        start: ::bbnf::runtime::tape::TapeCursor<'p>,
        target_idx: u8,
    ) -> ::core::option::Option<::bbnf::runtime::tape::TapeCursor<'p>> {
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
pub use __googlesheetsparser_emit_impl::*;
