//! Tranche AV Phase 1 — grammar-local structural constant emitter.
//!
//! Lowers the structural slots of [`bbnf_ir::passes::GrammarProfile`]
//! into plain generated constants embedded next to the grammar string
//! array. The generated runtime surface must not depend on the tape
//! crate's profile carrier.
//!
//! The emitted constants are fully `const`-evaluable. Slice fields
//! reference `static` byte/struct arrays emitted immediately above
//! the public constants in the same module: no runtime allocation, no
//! lazy initialisation, and no `GrammarProfile` construction.

use bbnf_ir::passes::GrammarProfile;
use proc_macro2::{Literal, TokenStream};
use quote::quote;

/// Emit structural-scan constants plus supporting `static` arrays for
/// one grammar.
///
/// Lives at module scope in `generated.rs`, immediately after the
/// `GRAMMAR_<Ident>` string array and before the view types + rule
/// functions.
pub fn emit_grammar_profile(profile: &GrammarProfile) -> TokenStream {
    if profile.structural_alphabet.is_empty() {
        return TokenStream::new();
    }

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
                static __GRAMMAR_STRUCTURAL_ALPHABET: [u8; #len] = [#(#bytes),*];
            },
            quote! { &__GRAMMAR_STRUCTURAL_ALPHABET },
        )
    };

    // AW-III.W5.d — `(u8, u8)` tuple shape feeds the SIMD scanner
    // alphabet's `digraph_pairs` without a shim layer.
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
                static __GRAMMAR_STRUCTURAL_DIGRAPHS: [(u8, u8); #len] = [#(#pairs),*];
            },
            quote! { &__GRAMMAR_STRUCTURAL_DIGRAPHS },
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
                static __GRAMMAR_STRUCTURAL_QUOTE_CLASSES: [u8; #len] = [#(#bytes),*];
            },
            quote! { &__GRAMMAR_STRUCTURAL_QUOTE_CLASSES },
        )
    };

    quote! {
        #alphabet_decl
        #digraphs_decl
        #quote_classes_decl

        pub const GRAMMAR_STRUCTURAL_ALPHABET: &[u8] = #alphabet_ref;
        pub const GRAMMAR_STRUCTURAL_DIGRAPHS: &[(u8, u8)] = #digraphs_ref;
        pub const GRAMMAR_STRUCTURAL_DIGRAPH_MASK: [u64; 4] = #digraph_mask_ref;
        pub const GRAMMAR_STRUCTURAL_QUOTE_CLASSES: &[u8] = #quote_classes_ref;
    }
}
