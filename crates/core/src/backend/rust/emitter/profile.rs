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
//! | push counts + per-byte density | V1 (this phase) |
//! | structural alphabet + digraphs | V1 (this phase) |
//! | active_columns | V2 |
//! | list_rules | V6 |
//! | keyword_tables | V7 |
//! | shape_dict | V5 |
//! | branch_priors | V4 |
//! | dedup_eligible_rules | V8 |
//! | reorder_unroll_visitors | V2 |
//!
//! Empty slices land as `&[]` in the emitted literal at V1; waves
//! V2–V9 swap each `&[]` for a concrete `&SOME_STATIC_ARRAY`.

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
    let push_compound_count = profile.push_compound_count;
    let push_leaf_count = profile.push_leaf_count;
    let push_leaf_with_count = profile.push_leaf_with_count;

    let compounds_per_input_byte = f32_literal(profile.compounds_per_input_byte);
    let leaves_per_input_byte = f32_literal(profile.leaves_per_input_byte);
    let payload_bytes_per_input_byte = f32_literal(profile.payload_bytes_per_input_byte);
    let expected_ns_per_byte = f32_literal(profile.expected_ns_per_byte);
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

    let (digraphs_decl, digraphs_ref) = if profile.structural_digraphs.is_empty() {
        (TokenStream::new(), quote! { &[] })
    } else {
        let pairs = profile.structural_digraphs.iter().map(|pair| {
            let a = Literal::u8_unsuffixed(pair[0]);
            let b = Literal::u8_unsuffixed(pair[1]);
            quote! { [#a, #b] }
        });
        let len = profile.structural_digraphs.len();
        (
            quote! {
                static __GRAMMAR_PROFILE_DIGRAPHS: [[u8; 2]; #len] = [#(#pairs),*];
            },
            quote! { &__GRAMMAR_PROFILE_DIGRAPHS },
        )
    };

    quote! {
        #alphabet_decl
        #digraphs_decl

        /// Per-grammar codegen fingerprint — consolidated static
        /// profile emitted by Tranche AV Phase 1. Every downstream
        /// consumer (tape capacity, scanner dispatch, column-set
        /// selection, reorder visitors, keyword tables, shape
        /// dictionary, runtime dedup) reads the matching field.
        pub const GRAMMAR_PROFILE: ::bbnf::runtime::tape::GrammarProfile =
            ::bbnf::runtime::tape::GrammarProfile {
                push_compound_count: #push_compound_count,
                push_leaf_count: #push_leaf_count,
                push_leaf_with_count: #push_leaf_with_count,
                compounds_per_input_byte: #compounds_per_input_byte,
                leaves_per_input_byte: #leaves_per_input_byte,
                payload_bytes_per_input_byte: #payload_bytes_per_input_byte,
                expected_ns_per_byte: #expected_ns_per_byte,
                parallel_break_even_bytes: #parallel_break_even_bytes,
                structural_alphabet: #alphabet_ref,
                structural_digraphs: #digraphs_ref,
                active_columns: &[],
                list_rules: &[],
                keyword_tables: &[],
                shape_dict: &[],
                branch_priors: &[],
                dedup_eligible_rules: &[],
                reorder_unroll_visitors: &[],
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
