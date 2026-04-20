//! Shared cross-rule PHF keyword tables.
//!
//! AY.W4.3 — CSS L4 emits property-name PHF dispatch in roughly
//! ~20 rules whose keyword sets overlap (e.g. `border-color`,
//! `border-width`, `border-style` share prefixes; the same color
//! keywords appear in multiple `<color>`-typed declarations).
//!
//! The per-rule emitter
//! ([`crate::backend::rust::emitter::keyword_dispatch`]) emits one
//! `__PHF_<grammar>_<rule_id>_KW` table per Alt site. Across CSS L4
//! that is ~20 distinct tables, but the keyword bytes themselves
//! repeat: a `&[u8]` literal `b"red"` appears in N tables, paying N×
//! the bytes plus N× rodata-symbol overhead.
//!
//! This module generates a single grammar-wide deduplicated keyword
//! interning table — one `__PHF_SHARED_<grammar>_BYTES: &[&[u8]]`
//! covering every distinct keyword across all rules, plus per-rule
//! `__PHF_<rule>_INDICES: &[u16]` slices indexing into the shared
//! table. The dispatch helper performs the same binary search; only
//! the storage is deduplicated.
//!
//! # When this fires
//!
//! Activation gates on the bytes-saved threshold:
//!
//! - At least 2 rules with PHF tables (otherwise no sharing).
//! - At least 30% byte overlap across the union (otherwise the
//!   indirection's overhead exceeds the rodata savings).
//!
//! Below the threshold the per-rule emitter's output is left intact.

use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};
use std::collections::BTreeMap;

/// One contributing rule's keyword set, captured for shared-table
/// emission.
#[derive(Clone, Debug)]
pub struct RuleKeywordSet {
    /// Per-rule discriminant (the rule's `id` in the GrammarIR).
    pub rule_id: u32,
    /// Keyword bytes + per-keyword branch index pairs.
    pub entries: Vec<(Vec<u8>, u8)>,
}

/// A grammar-wide shared keyword table — one rodata block carrying
/// every distinct keyword across all participating rules.
pub struct SharedKeywordTable<'a> {
    /// Shared keyword vocabulary, sorted lexicographically.
    pub vocab: Vec<&'a [u8]>,
    /// Per-rule (rule_id, [(vocab_idx, branch_idx)]) maps. The
    /// per-rule dispatcher binary-searches within its assigned
    /// vocab subset.
    pub rule_assignments: Vec<(u32, Vec<(u16, u8)>)>,
}

/// Build a [`SharedKeywordTable`] from per-rule keyword sets when
/// the savings threshold is met. Returns `None` to signal the
/// per-rule emitter should keep ownership.
pub fn try_build_shared_table(rules: &[RuleKeywordSet]) -> Option<SharedKeywordTable<'_>> {
    if rules.len() < 2 {
        return None;
    }

    // Build the keyword → first-seen-rule discriminant map. We use
    // a BTreeMap so the vocab walk is deterministic (lexicographic
    // by bytes).
    let mut bytes_to_first: BTreeMap<&[u8], usize> = BTreeMap::new();
    let mut total_bytes_per_rule = 0usize;
    let mut distinct_bytes = 0usize;

    for r in rules {
        for (kw, _) in &r.entries {
            total_bytes_per_rule += kw.len();
            let entry = bytes_to_first.entry(kw.as_slice());
            if matches!(entry, std::collections::btree_map::Entry::Vacant(_)) {
                distinct_bytes += kw.len();
            }
            entry.or_insert(0);
        }
    }

    if total_bytes_per_rule == 0 || distinct_bytes == 0 {
        return None;
    }

    // Threshold: at least 30% bytes saved by deduplication.
    let dedup_savings_pct = 100 - (distinct_bytes * 100 / total_bytes_per_rule);
    if dedup_savings_pct < 30 {
        return None;
    }

    // Assign each keyword its sorted vocab index.
    let vocab: Vec<&[u8]> = bytes_to_first.keys().copied().collect();
    let kw_to_idx: BTreeMap<&[u8], u16> = vocab
        .iter()
        .enumerate()
        .map(|(i, k)| (*k, i as u16))
        .collect();

    let rule_assignments: Vec<(u32, Vec<(u16, u8)>)> = rules
        .iter()
        .map(|r| {
            let pairs: Vec<(u16, u8)> = r
                .entries
                .iter()
                .filter_map(|(kw, idx)| {
                    kw_to_idx.get(kw.as_slice()).map(|v| (*v, *idx))
                })
                .collect();
            (r.rule_id, pairs)
        })
        .collect();

    Some(SharedKeywordTable {
        vocab,
        rule_assignments,
    })
}

/// Emit the shared keyword table + per-rule index/dispatch helpers.
pub fn emit_shared_table(grammar: &str, table: &SharedKeywordTable<'_>) -> TokenStream {
    let grammar_tag = sanitise_ident(grammar);
    let vocab_ident = format_ident!("__PHF_SHARED_{}_BYTES", grammar_tag);
    let n_vocab = table.vocab.len();

    let vocab_lits = table.vocab.iter().map(|b| {
        let lit = Literal::byte_string(b);
        quote! { #lit as &[u8] }
    });

    let n_lit = Literal::usize_unsuffixed(n_vocab);

    let per_rule_arms = table.rule_assignments.iter().map(|(rule_id, pairs)| {
        let rule_idx_ident = format_ident!("__PHF_SHARED_{}_RULE_{}_IDX", grammar_tag, rule_id);
        let rule_disc_ident = format_ident!("__PHF_SHARED_{}_RULE_{}_DISC", grammar_tag, rule_id);
        let n_pairs = pairs.len();
        let n_pairs_lit = Literal::usize_unsuffixed(n_pairs);
        let mut sorted_pairs = pairs.clone();
        sorted_pairs.sort_by_key(|(vidx, _)| *vidx);
        let idx_lits = sorted_pairs.iter().map(|(vidx, _)| Literal::u16_unsuffixed(*vidx));
        let disc_lits = sorted_pairs.iter().map(|(_, disc)| Literal::u8_unsuffixed(*disc));

        quote! {
            /// Vocab indices admissible to this rule (sorted).
            #[allow(dead_code)]
            pub(crate) const #rule_idx_ident: [u16; #n_pairs_lit] = [#(#idx_lits),*];
            /// Branch discriminants per admissible vocab entry.
            #[allow(dead_code)]
            pub(crate) const #rule_disc_ident: [u8; #n_pairs_lit] = [#(#disc_lits),*];
        }
    });

    quote! {
        /// AY.W4.3 — shared cross-rule keyword vocabulary. One
        /// rodata block; per-rule indices reference into it via
        /// `[u16; N]` slices, deduplicating the byte storage.
        #[allow(dead_code)]
        pub(crate) const #vocab_ident: [&[u8]; #n_lit] = [#(#vocab_lits),*];

        #(#per_rule_arms)*
    }
}

/// Compose the shared-vocab static ident for a grammar.
pub fn shared_vocab_ident(grammar: &str) -> proc_macro2::Ident {
    let grammar_tag = sanitise_ident(grammar);
    format_ident!("__PHF_SHARED_{}_BYTES", grammar_tag)
}

fn sanitise_ident(name: &str) -> String {
    let mut out = String::with_capacity(name.len());
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            out.push(ch);
        } else {
            out.push('_');
        }
    }
    out
}
