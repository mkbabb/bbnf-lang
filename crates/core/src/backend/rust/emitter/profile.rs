//! Tranche AV Phase 1 — `GRAMMAR_PROFILE` const-literal emitter.
//!
//! Lowers [`bbnf_ir::passes::GrammarProfile`] (the owned precursor)
//! to a `const GRAMMAR_PROFILE: bbnf::runtime::tape::GrammarProfile =
//! GrammarProfile { ... };` declaration embedded in each grammar's
//! `generated.rs`, next to the grammar string array.
//!
//! The emitted literal is fully `const`-evaluable. Slice fields
//! reference `static` byte/struct arrays emitted immediately above
//! the profile literal in the same module — no runtime allocation,
//! no lazy initialisation. Every per-grammar codegen decision that
//! grammars differ on (tape capacity, structural alphabet, column
//! set, list rules, keyword tables, shape dict, branch priors,
//! dedup set, reorder visitors) reads the matching field.
//!
//! ## Slot population by wave
//!
//! | Slot | Populated in |
//! |------|--------------|
//! | push counts + per-byte density | V1 |
//! | structural alphabet + digraphs | V1 |
//! | list_rules | V6 (AW-IV.W1.δ projection live; mining in W4.4) |
//! | shape_dict | V5 (AV.5.4 mining; AW-IV.W1.δ projection) |
//!
//! AX.W0b.A — seven dead slots retired with the walker
//! (`active_columns`, `branch_priors`, `reorder_unroll_visitors`,
//! `keyword_tables`, `dedup_eligible_rules`,
//! `payload_bytes_per_input_byte`, `expected_ns_per_byte`). Each
//! shipped substrate-side without a live consumer; W9 reintroduces
//! the predictive ones from the surviving density fields.

use bbnf_ir::passes::{GrammarProfile, ShapeEntryIr};
use proc_macro2::{Literal, TokenStream};
use quote::quote;

/// Emit the `GRAMMAR_PROFILE` const literal + its supporting
/// `static` arrays for one grammar.
///
/// Lives at module scope in `generated.rs`, immediately after the
/// `GRAMMAR_<Ident>` string array and before the view types + rule
/// functions.
///
/// AW-IV.W1.δ — every consumer slot in the emitted literal now
/// references a `static` array (populated from the IR projection)
/// or `&[]` (genuinely empty IR-side). The wire contract from IR
/// mining → [`GrammarIR::profile`] → this emitter → the `pub const
/// GRAMMAR_PROFILE` literal → runtime consumer is closed.
pub fn emit_grammar_profile(profile: &GrammarProfile) -> TokenStream {
    let push_compound_count = profile.push_compound_count;
    let push_leaf_count = profile.push_leaf_count;
    let push_leaf_with_count = profile.push_leaf_with_count;

    let compounds_per_input_byte = f32_literal(profile.compounds_per_input_byte);
    let leaves_per_input_byte = f32_literal(profile.leaves_per_input_byte);
    let parallel_break_even_bytes = profile.parallel_break_even_bytes;

    // Static byte arrays for the slice-valued fields. Empty alphabets
    // reference the shared `&[]` rather than an empty `static` — the
    // compiler accepts `&[]` as a `const` in the struct literal
    // without needing a placeholder array.
    let (alphabet_decl, alphabet_ref) = if profile.structural_alphabet.is_empty() {
        (TokenStream::new(), quote! { &[] })
    } else {
        let bytes: Vec<Literal> = profile
            .structural_alphabet
            .iter()
            .map(|b| Literal::u8_unsuffixed(*b))
            .collect();
        let len = profile.structural_alphabet.len();
        (
            quote! {
                static __GRAMMAR_PROFILE_ALPHABET: [u8; #len] = [#(#bytes),*];
            },
            quote! { &__GRAMMAR_PROFILE_ALPHABET },
        )
    };

    // AW-III.W5.d — `(u8, u8)` tuple shape so the same static feeds
    // both the tape-side profile field and the SIMD scanner alphabet's
    // `digraph_pairs` without a shim layer.
    let (digraphs_decl, digraphs_ref) = if profile.structural_digraphs.is_empty() {
        (TokenStream::new(), quote! { &[] })
    } else {
        let pairs = profile.structural_digraphs.iter().map(|(a, b)| {
            let a_lit = Literal::u8_unsuffixed(*a);
            let b_lit = Literal::u8_unsuffixed(*b);
            quote! { (#a_lit, #b_lit) }
        });
        let len = profile.structural_digraphs.len();
        (
            quote! {
                static __GRAMMAR_PROFILE_DIGRAPHS: [(u8, u8); #len] = [#(#pairs),*];
            },
            quote! { &__GRAMMAR_PROFILE_DIGRAPHS },
        )
    };

    // AW-III.W5.a — pre-computed digraph first-byte bitmap. The
    // `[u64; 4]` lays out as a const-evaluable inline literal; no
    // supporting `static`.
    let digraph_mask_words = profile
        .structural_digraph_mask
        .iter()
        .map(|w| Literal::u64_unsuffixed(*w));
    let digraph_mask_ref = quote! { [#(#digraph_mask_words),*] };

    // AW-III.W5.a — quote-class bytes (sorted, ASCII range).
    let (quote_classes_decl, quote_classes_ref) = if profile.structural_quote_classes.is_empty() {
        (TokenStream::new(), quote! { &[] })
    } else {
        let bytes: Vec<Literal> = profile
            .structural_quote_classes
            .iter()
            .map(|b| Literal::u8_unsuffixed(*b))
            .collect();
        let len = profile.structural_quote_classes.len();
        (
            quote! {
                static __GRAMMAR_PROFILE_QUOTE_CLASSES: [u8; #len] = [#(#bytes),*];
            },
            quote! { &__GRAMMAR_PROFILE_QUOTE_CLASSES },
        )
    };

    // AW-IV.W1.δ — `list_rules` projection. Mining lands in W4.4
    // (document-level parallel parse); IR-side empty today.
    let (list_rules_decl, list_rules_ref) = emit_list_rules(&profile.list_rules);

    // AW-IV.W1.δ — `shape_dict` projection. Populated from the
    // `ShapeDictMiner` + `solve_shape_dict_selection` via
    // [`GrammarIR::profile`]. Each entry references per-entry
    // `static __GRAMMAR_PROFILE_SHAPE_<i>_KINDS: [u8; N]` +
    // `__GRAMMAR_PROFILE_SHAPE_<i>_OFFSETS: [u16; N]` arrays.
    let (shape_dict_decl, shape_dict_ref) = emit_shape_dict(&profile.shape_dict);

    quote! {
        #alphabet_decl
        #digraphs_decl
        #quote_classes_decl
        #list_rules_decl
        #shape_dict_decl

        /// Per-grammar codegen fingerprint — consolidated static
        /// profile emitted by Tranche AV Phase 1. Every downstream
        /// consumer (tape capacity, scanner dispatch, shape
        /// dictionary) reads the matching field.
        pub const GRAMMAR_PROFILE: ::bbnf::runtime::tape::GrammarProfile =
            ::bbnf::runtime::tape::GrammarProfile {
                push_compound_count: #push_compound_count,
                push_leaf_count: #push_leaf_count,
                push_leaf_with_count: #push_leaf_with_count,
                compounds_per_input_byte: #compounds_per_input_byte,
                leaves_per_input_byte: #leaves_per_input_byte,
                parallel_break_even_bytes: #parallel_break_even_bytes,
                structural_alphabet: #alphabet_ref,
                structural_digraphs: #digraphs_ref,
                structural_digraph_mask: #digraph_mask_ref,
                structural_quote_classes: #quote_classes_ref,
                list_rules: #list_rules_ref,
                shape_dict: #shape_dict_ref,
            };
    }
}

// ── Per-slot emitters ─────────────────────────────────────────────────

/// AW-IV.W1.δ — emit `static __GRAMMAR_PROFILE_LIST_RULES:
/// [RuleId; N] = [...]` and a `&__GRAMMAR_PROFILE_LIST_RULES`
/// reference, or `(empty, &[])` when the IR-side slot is empty.
fn emit_list_rules(rules: &[u32]) -> (TokenStream, TokenStream) {
    if rules.is_empty() {
        return (TokenStream::new(), quote! { &[] });
    }
    let ids = rules.iter().map(|r| {
        let lit = Literal::u32_unsuffixed(*r);
        quote! { ::bbnf::runtime::tape::RuleId(#lit) }
    });
    let len = rules.len();
    (
        quote! {
            static __GRAMMAR_PROFILE_LIST_RULES:
                [::bbnf::runtime::tape::RuleId; #len] = [#(#ids),*];
        },
        quote! { &__GRAMMAR_PROFILE_LIST_RULES },
    )
}

/// AW-IV.W1.δ — emit `static __GRAMMAR_PROFILE_SHAPE_DICT:
/// [ShapeEntry; N]` plus per-entry `__GRAMMAR_PROFILE_SHAPE_<i>_KINDS`
/// and `__GRAMMAR_PROFILE_SHAPE_<i>_OFFSETS` support arrays, and a
/// reference into the table. Empty IR-side slot → `(empty, &[])`.
///
/// Mirrors the existing `emit_shape_dict_arrays` in
/// [`super::dta`] — the shape is canonical. Both emitters walk the
/// same IR source data (`shape_dict_selection` → `shape_dict_templates`)
/// so they produce identical entries; the profile's reference goes
/// through this dedicated projection (and its local statics) rather
/// than reaching cross-module for the existing `SHAPE_DICT`, to keep
/// `emit_grammar_profile` self-contained. When both emitters run for
/// the same grammar the duplicate data lives in `.rodata` without
/// runtime cost.
fn emit_shape_dict(entries: &[ShapeEntryIr]) -> (TokenStream, TokenStream) {
    if entries.is_empty() {
        return (TokenStream::new(), quote! { &[] });
    }
    let mut support = TokenStream::new();
    let mut entry_literals: Vec<TokenStream> = Vec::with_capacity(entries.len());
    for (idx, entry) in entries.iter().enumerate() {
        let kinds_ident = quote::format_ident!("__GRAMMAR_PROFILE_SHAPE_{}_KINDS", idx);
        let offsets_ident = quote::format_ident!("__GRAMMAR_PROFILE_SHAPE_{}_OFFSETS", idx);
        let kinds_len = entry.child_kinds.len();
        let offsets_len = entry.leaf_payload_offsets.len();
        let kind_lits = entry.child_kinds.iter().map(|b| Literal::u8_unsuffixed(*b));
        let offset_lits = entry
            .leaf_payload_offsets
            .iter()
            .map(|o| Literal::u16_unsuffixed(*o));
        support.extend(quote! {
            static #kinds_ident: [u8; #kinds_len] = [#(#kind_lits),*];
            static #offsets_ident: [u16; #offsets_len] = [#(#offset_lits),*];
        });
        let shape_hash_lit = Literal::u64_unsuffixed(entry.shape_hash);
        let rule_lit = Literal::u32_unsuffixed(entry.rule_id);
        let payload_bytes_lit = Literal::u16_unsuffixed(entry.payload_bytes);
        entry_literals.push(quote! {
            ::bbnf::runtime::tape::ShapeEntry {
                shape_hash: #shape_hash_lit,
                rule: ::bbnf::runtime::tape::RuleId(#rule_lit),
                child_kinds: &#kinds_ident,
                leaf_payload_offsets: &#offsets_ident,
                payload_bytes: #payload_bytes_lit,
            }
        });
    }
    let table_len = entry_literals.len();
    (
        quote! {
            #support
            static __GRAMMAR_PROFILE_SHAPE_DICT:
                [::bbnf::runtime::tape::ShapeEntry; #table_len] = [
                #(#entry_literals),*
            ];
        },
        quote! { &__GRAMMAR_PROFILE_SHAPE_DICT },
    )
}

/// Emit a `f32` literal that round-trips through `proc_macro2` into
/// generated code without losing precision. `quote!` renders
/// `f32` values as untyped floats which trip "ambiguous numeric
/// type" errors inside the const literal; an explicit typed suffix
/// pins the type.
fn f32_literal(value: f32) -> TokenStream {
    let lit = Literal::f32_suffixed(value);
    quote! { #lit }
}
