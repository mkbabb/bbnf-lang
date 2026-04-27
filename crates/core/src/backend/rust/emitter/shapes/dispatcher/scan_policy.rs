//! Grammar-activated structural-scan policy table.
//!
//! For each grammar the emitter produces a single module-scope const
//! `STRUCTURAL_SCAN_POLICY: &[ScanPolicyEntry]` — one entry per non-
//! transparent rule, populated from CSP-inferred FIRST-set facts
//! intersected with the grammar's mined `structural_alphabet` +
//! `structural_digraph_mask`.
//!
//! The const is consumed at emission time by the emitter's
//! structural-scan-admitting shapes (`object_key_seek` inlining in
//! `__path_walk`, `bounded_lookahead` in regex-scan adapters). There
//! is no runtime flag and no hand-routed grammar specialisation —
//! every decision resolves at codegen against grammar-derived facts.
//!
//! Schema (mirroring `crate::runtime::tape::ScanPolicyEntry`):
//!
//!   ScanPolicyEntry {
//!       rule_id: u32,                       // IR RuleId
//!       alphabet_class: ScanAlphabetClass,  // Empty / Sparse / Dense / Digraph
//!       activation: ScanActivationFlags,    // bitmap of admitted primitives
//!   }
//!
//! Sample entries (shape per grammar):
//!
//! - JSON `object`: class=Dense (FIRST ∩ alphabet = `{`, `:`, `,`, `}`
//!   — 4 bytes), flags=OBJECT_KEY_SEEK | BOUNDED_LOOKAHEAD |
//!   SCAN_STRUCTURAL_BOUNDED.
//! - CSS L4 `declaration`: class=Dense (`:`, `;`, `/`), flags=
//!   BOUNDED_LOOKAHEAD | SCAN_STRUCTURAL_BOUNDED | DIGRAPH_ADMIT
//!   (comment digraph `/*`).
//! - Sheets `cell_ref`: class=Sparse (`:` for range), flags=
//!   BOUNDED_LOOKAHEAD.
//! - BBNF `rule`: class=Digraph (digraph `->`, `(*`, `*)`), flags=
//!   BOUNDED_LOOKAHEAD | DIGRAPH_ADMIT.

use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/// Emit the per-grammar `STRUCTURAL_SCAN_POLICY` const table — one
/// [`crate::runtime::tape::ScanPolicyEntry`] per non-transparent
/// rule, derived from FIRST-set facts + the grammar's mined
/// `structural_alphabet` + `structural_digraph_mask`.
///
/// The emitted stream lives at module scope alongside
/// `GRAMMAR_PROFILE`, immediately after the support module the
/// shape dispatcher wires in. Consumers (emitter shapes that admit
/// structural-scan primitives) index the slice by `rule_id` at
/// emission time — no runtime lookup.
///
/// Returns an empty stream when the grammar has no non-transparent
/// rules (i.e. every rule is a transparent alias).
pub fn emit_structural_scan_policy(
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    use crate::generate::regex::byte_class::classify_rule_alphabet;
    use tape::{ScanActivationFlags, ScanAlphabetClass};
    use bbnf_ir::IrNode;

    let profile = ir.profile();
    let structural_alphabet = profile.structural_alphabet.to_vec();
    let structural_digraph_mask = profile.structural_digraph_mask;

    // Build per-rule entries. We emit an entry for every non-
    // transparent rule so the consumer's `rule_id == x` probe is
    // uniform; rules with `Empty` class + no activation flags carry
    // `ScanPolicyEntry::EMPTY` semantics but distinct rule_ids.
    let mut entries: Vec<TokenStream> = Vec::new();
    for rule in &ir.rules {
        if rule.meta.is_transparent {
            continue;
        }

        // Materialise the rule's FIRST set as a byte slice.
        // `CharSet128::iter()` yields bytes in ascending order; the
        // classifier accepts an unsorted slice so the iteration
        // order is immaterial.
        let first_bytes: Vec<u8> = rule.meta.first_set.iter().collect();

        // A rule is "compound" for our purposes iff its body node
        // produces children the substrate emits as a compound record
        // (Seq / Alt / Repeat / top-level Rule / TokenDispatch).
        // Leaves (Literal / Regex / Epsilon / Ref-only / lookahead)
        // carry no children to scan.
        let is_compound = matches!(
            &rule.body,
            IrNode::Seq(_)
                | IrNode::Alt(_, _)
                | IrNode::Repeat { .. }
                | IrNode::TokenDispatch { .. }
        );

        let facts = classify_rule_alphabet(
            &first_bytes,
            &structural_alphabet,
            &structural_digraph_mask,
            is_compound,
        );

        // Derive the alphabet class from the intersection count +
        // digraph admission + compound-ness.
        let class = if facts.admits_digraph && !structural_alphabet.is_empty() {
            // Digraph-aware rules take precedence over dense — the
            // emitter needs the digraph-opener probe unconditionally
            // when the rule admits a multi-byte structural marker.
            ScanAlphabetClass::Digraph
        } else if facts.alphabet_intersection_count >= 4 {
            ScanAlphabetClass::Dense
        } else if facts.alphabet_intersection_count >= 1 {
            ScanAlphabetClass::Sparse
        } else {
            ScanAlphabetClass::Empty
        };

        // Derive activation flags from the class + is_compound.
        // Leaf rules never admit structural-scan primitives that
        // require children to walk.
        let mut flags: u8 = 0;
        if facts.is_compound {
            match class {
                ScanAlphabetClass::Dense => {
                    flags |= ScanActivationFlags::OBJECT_KEY_SEEK;
                    flags |= ScanActivationFlags::BOUNDED_LOOKAHEAD;
                    flags |= ScanActivationFlags::SCAN_STRUCTURAL_BOUNDED;
                }
                ScanAlphabetClass::Sparse => {
                    flags |= ScanActivationFlags::BOUNDED_LOOKAHEAD;
                }
                ScanAlphabetClass::Digraph => {
                    flags |= ScanActivationFlags::BOUNDED_LOOKAHEAD;
                    flags |= ScanActivationFlags::SCAN_STRUCTURAL_BOUNDED;
                    flags |= ScanActivationFlags::DIGRAPH_ADMIT;
                }
                ScanAlphabetClass::Empty => {}
            }
        }

        let class_tokens = match class {
            ScanAlphabetClass::Empty => {
                quote! { crate::runtime::tape::ScanAlphabetClass::Empty }
            }
            ScanAlphabetClass::Sparse => {
                quote! { crate::runtime::tape::ScanAlphabetClass::Sparse }
            }
            ScanAlphabetClass::Dense => {
                quote! { crate::runtime::tape::ScanAlphabetClass::Dense }
            }
            ScanAlphabetClass::Digraph => {
                quote! { crate::runtime::tape::ScanAlphabetClass::Digraph }
            }
        };

        let rule_id = rule.id;
        let flags_lit = proc_macro2::Literal::u8_unsuffixed(flags);
        entries.push(quote! {
            crate::runtime::tape::ScanPolicyEntry {
                rule_id: #rule_id,
                alphabet_class: #class_tokens,
                activation: crate::runtime::tape::ScanActivationFlags::from_bits(#flags_lit),
            }
        });
    }

    if entries.is_empty() {
        return quote! {};
    }

    let policy_ident = format_ident!("STRUCTURAL_SCAN_POLICY");
    let _ = grammar_suffix; // The const is module-scoped; name is grammar-agnostic.

    quote! {
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
        pub const #policy_ident: &[crate::runtime::tape::ScanPolicyEntry] = &[
            #(#entries),*
        ];
    }
}

/// Look up a [`crate::runtime::tape::ScanPolicyEntry`] for a rule by
/// id within the `STRUCTURAL_SCAN_POLICY` slice — emission-time
/// helper that resolves during codegen so the generated call site
/// inlines the matching entry's class + activation bitmap without
/// a runtime search.
///
/// Returns `None` when the rule carries no structural-scan admission
/// (e.g. leaf rules, transparent aliases omitted from the policy
/// table).
pub fn lookup_scan_policy<'ir>(
    ir: &'ir GrammarIR,
    rule_id: bbnf_ir::RuleId,
) -> Option<(tape::ScanAlphabetClass, tape::ScanActivationFlags)> {
    use crate::generate::regex::byte_class::classify_rule_alphabet;
    use tape::{ScanActivationFlags, ScanAlphabetClass};
    use bbnf_ir::IrNode;

    let rule = ir.rules.iter().find(|r| r.id == rule_id)?;
    if rule.meta.is_transparent {
        return None;
    }

    let profile = ir.profile();
    let structural_alphabet = profile.structural_alphabet.to_vec();
    let structural_digraph_mask = profile.structural_digraph_mask;

    let first_bytes: Vec<u8> = rule.meta.first_set.iter().collect();
    let is_compound = matches!(
        &rule.body,
        IrNode::Seq(_)
            | IrNode::Alt(_, _)
            | IrNode::Repeat { .. }
            | IrNode::TokenDispatch { .. }
    );

    let facts = classify_rule_alphabet(
        &first_bytes,
        &structural_alphabet,
        &structural_digraph_mask,
        is_compound,
    );

    let class = if facts.admits_digraph && !structural_alphabet.is_empty() {
        ScanAlphabetClass::Digraph
    } else if facts.alphabet_intersection_count >= 4 {
        ScanAlphabetClass::Dense
    } else if facts.alphabet_intersection_count >= 1 {
        ScanAlphabetClass::Sparse
    } else {
        ScanAlphabetClass::Empty
    };

    let mut flags: u8 = 0;
    if facts.is_compound {
        match class {
            ScanAlphabetClass::Dense => {
                flags |= ScanActivationFlags::OBJECT_KEY_SEEK;
                flags |= ScanActivationFlags::BOUNDED_LOOKAHEAD;
                flags |= ScanActivationFlags::SCAN_STRUCTURAL_BOUNDED;
            }
            ScanAlphabetClass::Sparse => {
                flags |= ScanActivationFlags::BOUNDED_LOOKAHEAD;
            }
            ScanAlphabetClass::Digraph => {
                flags |= ScanActivationFlags::BOUNDED_LOOKAHEAD;
                flags |= ScanActivationFlags::SCAN_STRUCTURAL_BOUNDED;
                flags |= ScanActivationFlags::DIGRAPH_ADMIT;
            }
            ScanAlphabetClass::Empty => {}
        }
    }

    Some((class, ScanActivationFlags::from_bits(flags)))
}
