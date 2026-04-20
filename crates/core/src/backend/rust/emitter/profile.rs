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
//! grammars differ on (tape capacity, structural alphabet) reads the
//! matching field.
//!
//! ## Surviving slots
//!
//! | Slot | Populated in |
//! |------|--------------|
//! | per-byte density       | V1 |
//! | structural alphabet    | V1 |
//! | structural digraphs    | V1 |
//! | quote-class bytes      | V1 |
//! | parallel-break-even    | V6 (AW-IV.W4.4) |
//!
//! AX.W0b.A retired the walker's seven dead slots (`active_columns`,
//! `branch_priors`, `reorder_unroll_visitors`, `keyword_tables`,
//! `dedup_eligible_rules`, `payload_bytes_per_input_byte`,
//! `expected_ns_per_byte`); AY.W0.4 retires five further slots
//! (`push_compound_count`, `push_leaf_count`, `push_leaf_with_count`,
//! `list_rules`, `shape_dict`) — each shipped substrate-side at the
//! emitter without a downstream runtime consumer.

use bbnf_ir::passes::GrammarProfile;
use proc_macro2::{Literal, TokenStream};
use quote::quote;

/// Emit the `GRAMMAR_PROFILE` const literal + its supporting
/// `static` arrays for one grammar.
///
/// Lives at module scope in `generated.rs`, immediately after the
/// `GRAMMAR_<Ident>` string array and before the view types + rule
/// functions.
pub fn emit_grammar_profile(profile: &GrammarProfile) -> TokenStream {
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

    quote! {
        #alphabet_decl
        #digraphs_decl
        #quote_classes_decl

        /// Per-grammar codegen fingerprint — consolidated static
        /// profile emitted by Tranche AV Phase 1. Every downstream
        /// consumer (tape capacity, scanner dispatch) reads the
        /// matching field.
        pub const GRAMMAR_PROFILE: ::bbnf::runtime::tape::GrammarProfile =
            ::bbnf::runtime::tape::GrammarProfile {
                compounds_per_input_byte: #compounds_per_input_byte,
                leaves_per_input_byte: #leaves_per_input_byte,
                parallel_break_even_bytes: #parallel_break_even_bytes,
                structural_alphabet: #alphabet_ref,
                structural_digraphs: #digraphs_ref,
                structural_digraph_mask: #digraph_mask_ref,
                structural_quote_classes: #quote_classes_ref,
            };
    }
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
