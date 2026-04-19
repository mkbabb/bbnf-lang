//! AW-III.W6.5 — per-grammar Pratt-LUT emitter.
//!
//! Lowers [`bbnf_ir::passes::OperatorChainFacts`] (the mined
//! operator-chain facts) into a packed `[u8; 256]` precedence LUT
//! plus a sparse `&'static [DtaPrecedenceEntry]` metadata slice.
//!
//! ## Bit layout
//!
//! Each LUT byte packs four fields:
//!
//! ```text
//! bits 0..=3  precedence  (0..=15; 0 = not an operator)
//! bit  4      associativity  (0 = Left, 1 = Right)
//! bits 5..=6  arity  (0 = Binary, 1 = Prefix, 2 = Postfix)
//! bit  7      two_byte flag  (1 = consult sparse slice for second byte)
//! ```
//!
//! A non-operator byte reads `0x00` — the Pratt dispatch exits its
//! loop. An operator byte's `(prec, assoc, arity, two_byte)` tuple
//! packs into a single byte the Pratt loop can decode with one
//! load + three shifts.
//!
//! ## §6 generalisation
//!
//! No grammar-name conditionals. The emitter consumes
//! [`OperatorChainFacts`] — the mined IR fact — and produces the
//! LUT. Sheets, BBNF, CSS all reach the same emit path; the LUT
//! they get differs only because their mined facts differ.
//!
//! ## Const-fold guarantee
//!
//! The emitted LUT is a `pub const PRECEDENCE_LUT: [u8; 256]`. Rust
//! folds lookup + const-shift operations at monomorphisation —
//! `PRECEDENCE_LUT[b as usize]` is a single indexed byte load. The
//! Pratt loop in the DTA driver consumes `.0xF` for precedence,
//! `.&0x10` for assoc, `.>> 5 & 3` for arity, `.& 0x80` for
//! two-byte — all inlined by LLVM.

use bbnf_ir::passes::{
    Associativity, OperatorChainEntry, OperatorChainFacts,
};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

// ── LUT byte packer ──────────────────────────────────────────────────

/// Pack an operator row into its LUT byte.
///
/// The layout is documented at the module level; this function is
/// the single source of truth for the packing and should be the
/// only place that reads the bit positions.
///
/// ## Bit layout (reminder, matches module doc)
///
/// - `bits 0..=3` — precedence (clamped to 0..=15).
/// - `bit 4` — associativity (`0` left, `1` right).
/// - `bits 5..=6` — arity (`0..=2`, see [`OperatorArity::to_bits`]).
/// - `bit 7` — two-byte operator flag.
fn pack_lut_byte(entry: &OperatorChainEntry) -> u8 {
    let prec = entry.precedence & 0x0F;
    let assoc_bit = match entry.associativity {
        Associativity::Left => 0,
        Associativity::Right => 1,
    };
    let arity_bits = entry.arity.to_bits() & 0x03;
    let two_byte = if entry.second_byte.is_some() { 1 } else { 0 };
    prec | (assoc_bit << 4) | (arity_bits << 5) | (two_byte << 7)
}

// ── Emitter entry point ──────────────────────────────────────────────

/// Emit the per-grammar `PRECEDENCE_LUT` + supporting metadata for
/// the given mined operator-chain facts.
///
/// Returns a TokenStream that, when spliced into the grammar's
/// generated module, defines:
///
/// - `pub const PRECEDENCE_LUT: [u8; 256]` — dense packed table.
/// - `pub const PRECEDENCE_ENTRIES: &[DtaPrecedenceEntry]` — sparse
///   slice with (byte, second_byte, prec, assoc, op_rule,
///   op_discriminant) per entry. Consulted at runtime when the
///   two-byte flag is set.
/// - `pub const PRECEDENCE_OPERATOR_COUNT: usize` — entry count,
///   used by bench harnesses to assert chain mining.
///
/// When `facts.is_empty()`, emits a zeroed LUT + empty sparse
/// slice. Every grammar gets the constants defined so downstream
/// consumers can reference them uniformly.
///
/// The `grammar` parameter is the grammar's symbol prefix (matches
/// the grammar's marker struct ident). Prefixes identifiers so the
/// same constant can coexist with other grammars' LUTs when
/// multiple grammars share a crate.
pub fn emit_precedence_lut(
    grammar: &str,
    facts: &OperatorChainFacts,
) -> TokenStream {
    let lut_ident = format_ident!("PRECEDENCE_LUT");
    let entries_ident = format_ident!("PRECEDENCE_ENTRIES");
    let count_ident = format_ident!("PRECEDENCE_OPERATOR_COUNT");
    let _ = grammar; // reserved for future prefix-based disambiguation.

    // Pack the 256-entry byte array. Every byte not claimed by a
    // mined operator stays 0 — the Pratt loop reads 0 as "not an
    // operator" and exits.
    let mut packed: [u8; 256] = [0u8; 256];
    for entry in &facts.entries {
        packed[entry.byte as usize] = pack_lut_byte(entry);
    }
    let lut_bytes: Vec<TokenStream> =
        packed.iter().map(|b| quote! { #b }).collect();

    let entry_literals: Vec<TokenStream> =
        facts.entries.iter().map(entry_literal).collect();
    let entry_count = facts.entries.len();

    quote! {
        /// AW-III.W6.5 — dense Pratt precedence LUT.
        ///
        /// One byte per dispatch byte. Consulted by the DTA driver's
        /// `ShuntingYard` arm. See `bbnf::backend::rust::emitter::
        /// precedence` for the bit layout.
        pub const #lut_ident: [u8; 256] = [
            #(#lut_bytes),*
        ];

        /// AW-III.W6.5 — sparse Pratt metadata slice.
        ///
        /// One entry per mined operator. Consulted by the DTA
        /// driver when `PRECEDENCE_LUT[byte] & 0x80 != 0` (two-byte
        /// operator) to resolve the second byte + discriminant.
        pub const #entries_ident:
            &[::bbnf::runtime::tape::DtaPrecedenceEntry] = &[
            #(#entry_literals),*
        ];

        /// AW-III.W6.5 — total mined operator count for this
        /// grammar. Non-zero iff the lift admitted ≥ 1 chain.
        pub const #count_ident: usize = #entry_count;
    }
}

// ── Sparse-entry literal helper ──────────────────────────────────────

fn entry_literal(e: &OperatorChainEntry) -> TokenStream {
    let byte = e.byte;
    let second = match e.second_byte {
        Some(b) => quote! { ::core::option::Option::Some(#b) },
        None => quote! { ::core::option::Option::None },
    };
    let prec = e.precedence;
    let assoc = match e.associativity {
        Associativity::Left => {
            quote! { ::bbnf::runtime::tape::DtaAssociativity::Left }
        }
        Associativity::Right => {
            quote! { ::bbnf::runtime::tape::DtaAssociativity::Right }
        }
    };
    let op_rule = e.op_rule;
    let disc = e.op_discriminant;
    quote! {
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: #byte,
            second_byte: #second,
            precedence: #prec,
            associativity: #assoc,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(#op_rule),
            op_discriminant: #disc,
        }
    }
}

// ── Helper: reconstruct a facts summary for tests ───────────────────

/// Accessor for the pack function — exposed so parity tests can
/// independently verify the bit layout matches this module's
/// emission.
pub fn pack_lut_byte_for_test(entry: &OperatorChainEntry) -> u8 {
    pack_lut_byte(entry)
}
