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
//! ## Per-rule LUT emission (AX.W0a.2.l)
//!
//! Each Pratt-classified rule emits its OWN `PRECEDENCE_LUT_<rule>`
//! plus `PRECEDENCE_ENTRIES_<rule>` pair, keyed by the rule name.
//! The Pratt shape-emitter body dispatches on the rule's private
//! LUT so cross-rule first-byte collisions (BBNF's `||` in `value_or`
//! vs `<<` in `binary_factor`) don't leak into one another's byte
//! dispatch. The aggregate `PRECEDENCE_LUT` + `PRECEDENCE_ENTRIES`
//! slices remain (populated from the flat union of every rule's
//! entries, last-write-wins per byte) for walker cold-path
//! consumers until W0b retires the walker.
//!
//! ## Const-fold guarantee
//!
//! Each emitted LUT is a `pub const PRECEDENCE_LUT_<rule>: [u8; 256]`.
//! Rust folds lookup + const-shift operations at monomorphisation —
//! `PRECEDENCE_LUT_value_add[b as usize]` is a single indexed byte
//! load. The Pratt loop consumes `.0xF` for precedence, `.&0x10`
//! for assoc, `.>> 5 & 3` for arity, `.& 0x80` for two-byte — all
//! inlined by LLVM.

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

/// Emit the per-grammar precedence LUT family for the given mined
/// operator-chain facts.
///
/// Returns a TokenStream that, when spliced into the grammar's
/// generated module, defines:
///
/// - `pub const PRECEDENCE_LUT_<rule>: [u8; 256]` — one per Pratt
///   rule, dense packed table for that rule's operator alphabet.
/// - `pub const PRECEDENCE_ENTRIES_<rule>: &[DtaPrecedenceEntry]` —
///   one per Pratt rule, sparse metadata slice.
/// - `pub const PRECEDENCE_LUT: [u8; 256]` — aggregate (union of
///   every rule's packed bytes, last-write-wins) for walker cold-
///   path compat.
/// - `pub const PRECEDENCE_ENTRIES: &[DtaPrecedenceEntry]` — flat
///   aggregate slice; used by the walker's indirect dispatch until
///   W0b retires it.
/// - `pub const PRECEDENCE_OPERATOR_COUNT: usize` — aggregate entry
///   count, used by bench harnesses to assert chain mining.
///
/// When `facts.is_empty()`, emits only the aggregate zeroed LUT +
/// empty sparse slice. Every grammar gets the aggregate constants
/// defined so downstream consumers can reference them uniformly.
///
/// The `grammar` parameter is the grammar's symbol prefix (matches
/// the grammar's marker struct ident). Prefixes identifiers so the
/// same constant can coexist with other grammars' LUTs when
/// multiple grammars share a crate.
pub fn emit_precedence_lut(
    grammar: &str,
    facts: &OperatorChainFacts,
) -> TokenStream {
    let _ = grammar; // reserved for future prefix-based disambiguation.

    // ── Per-rule consts ──────────────────────────────────────────────
    //
    // Each Pratt-classified rule emits its own `PRECEDENCE_LUT_<rule>`
    // + `PRECEDENCE_ENTRIES_<rule>` pair. Rules with empty entries
    // still emit a zeroed LUT so the per-rule `parse_pratt_<rule>`
    // body's reference resolves.
    let per_rule: Vec<TokenStream> = facts
        .rules
        .iter()
        .map(|rule| {
            let rule_lut_ident = format_ident!("PRECEDENCE_LUT_{}", rule.rule_name);
            let rule_entries_ident =
                format_ident!("PRECEDENCE_ENTRIES_{}", rule.rule_name);

            // AX.W0a.2.o — when multiple entries share a first byte
            // (Sheets `compare_op` shares byte 60 across `"<>" / "<=" /
            // "<"`), the LUT's one-byte-per-first-byte encoding must
            // set bit-7 `two_byte` whenever ANY entry on that first
            // byte carries a `second_byte`. Otherwise the single-byte
            // entry — written last when iteration hits `"<"` after
            // `"<>"` / `"<="` — clobbers the two-byte flag, and the
            // runtime's `shapes/pratt.rs` op_matched path never enters
            // the two-byte lookup branch, silently treating `"<="` as
            // a bare `"<"`. Merge by OR-ing the packed byte with the
            // prior write's value so every `two_byte` bit on the same
            // first byte lights the flag.
            let mut packed: [u8; 256] = [0u8; 256];
            for entry in &rule.entries {
                let byte = pack_lut_byte(entry);
                let slot = &mut packed[entry.byte as usize];
                *slot = if *slot == 0 {
                    byte
                } else {
                    // Same first byte already packed — merge. The
                    // (precedence, associativity, arity) low bits
                    // agree across same-first-byte entries by the
                    // miner's admission check (see operator_chain.rs
                    // first_byte_triplet); only the two_byte bit
                    // differs when one entry has `second_byte` and
                    // another does not. OR the two_byte bit so the
                    // runtime attempts two-byte lookup, falling back
                    // to single-byte when the pair misses.
                    *slot | (byte & 0x80)
                };
            }
            let lut_bytes: Vec<TokenStream> =
                packed.iter().map(|b| quote! { #b }).collect();

            let entry_literals: Vec<TokenStream> =
                rule.entries.iter().map(entry_literal).collect();

            quote! {
                /// AX.W0a.2.l — per-rule dense Pratt precedence LUT.
                ///
                /// One byte per dispatch byte for this Pratt rule's
                /// operator alphabet. Consulted inline by the rule's
                /// emitted `parse_pratt_*` body. See `bbnf::backend::
                /// rust::emitter::precedence` for the bit layout.
                pub const #rule_lut_ident: [u8; 256] = [
                    #(#lut_bytes),*
                ];

                /// AX.W0a.2.l — per-rule sparse Pratt metadata slice.
                ///
                /// One entry per mined operator for this rule.
                /// Consulted by the rule's emitted `parse_pratt_*`
                /// body when the LUT byte's bit-7 two-byte flag is
                /// set, to resolve the second byte + discriminant.
                pub const #rule_entries_ident:
                    &[crate::runtime::tape::DtaPrecedenceEntry] = &[
                    #(#entry_literals),*
                ];
            }
        })
        .collect();

    // ── Aggregate consts (walker cold-path compat) ───────────────────
    //
    // Union of every rule's packed bytes (last-write-wins per byte —
    // the chain detector enforces within-rule disjointness, so the
    // only cross-rule collisions arise from two Pratt rules sharing
    // a first byte; the walker's aggregate dispatch accepts the
    // deterministic last-writer value).
    let lut_ident = format_ident!("PRECEDENCE_LUT");
    let entries_ident = format_ident!("PRECEDENCE_ENTRIES");
    let count_ident = format_ident!("PRECEDENCE_OPERATOR_COUNT");

    let mut aggregate_packed: [u8; 256] = [0u8; 256];
    let mut aggregate_entries: Vec<&OperatorChainEntry> = Vec::new();
    for entry in facts.entries_flat() {
        aggregate_packed[entry.byte as usize] = pack_lut_byte(entry);
        aggregate_entries.push(entry);
    }
    let aggregate_lut_bytes: Vec<TokenStream> =
        aggregate_packed.iter().map(|b| quote! { #b }).collect();
    let aggregate_entry_literals: Vec<TokenStream> =
        aggregate_entries.iter().map(|e| entry_literal(e)).collect();
    let aggregate_entry_count = aggregate_entries.len();

    quote! {
        #(#per_rule)*

        /// AW-III.W6.5 — aggregate dense Pratt precedence LUT.
        ///
        /// Union of every Pratt rule's packed LUT (last-write-wins
        /// per byte). Consulted by the walker cold-path's
        /// `ShuntingYard` arm until W0b retires the walker. See
        /// `bbnf::backend::rust::emitter::precedence` for the bit
        /// layout.
        pub const #lut_ident: [u8; 256] = [
            #(#aggregate_lut_bytes),*
        ];

        /// AW-III.W6.5 — aggregate sparse Pratt metadata slice.
        ///
        /// Flat union of every rule's mined operator entries.
        /// Consulted by the walker cold-path until W0b retires it.
        pub const #entries_ident:
            &[crate::runtime::tape::DtaPrecedenceEntry] = &[
            #(#aggregate_entry_literals),*
        ];

        /// AW-III.W6.5 — total mined operator count for this
        /// grammar. Non-zero iff the lift admitted ≥ 1 chain OR the
        /// shape classifier admitted ≥ 1 single-rung Pratt rule.
        pub const #count_ident: usize = #aggregate_entry_count;
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
            quote! { crate::runtime::tape::DtaAssociativity::Left }
        }
        Associativity::Right => {
            quote! { crate::runtime::tape::DtaAssociativity::Right }
        }
    };
    let op_rule = e.op_rule;
    let disc = e.op_discriminant;
    quote! {
        crate::runtime::tape::DtaPrecedenceEntry {
            byte: #byte,
            second_byte: #second,
            precedence: #prec,
            associativity: #assoc,
            op_rule: crate::runtime::tape::DtaRuleId(#op_rule),
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
