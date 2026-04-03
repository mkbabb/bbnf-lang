//! Rust parser code generation from BBNF grammars.
//!
//! This module is a thin facade over the codegen emitters.

// ── Codegen modules ────────────────────────────────────────────────────────
pub mod regex;

// Backward-compat alias — existing callers that reference regex_classify.
pub use regex::classify as regex_classify;

// Rust codegen lives in backend::rust; re-export for backward-compat paths.
pub use crate::backend::rust as codegen;

pub use codegen::ir_enums;
pub use codegen::ir_types;
pub use codegen::ir_types::ParserAttributes;

use crate::backend::PreparedGrammar;
use quote::quote;

// ── Entry point ────────────────────────────────────────────────────────────

/// Generate all parser code from a prepared AOT bundle: enum, parser methods,
/// and optionally prettify.
pub fn generate_all(
    prepared: &PreparedGrammar,
    parser_attrs: &ParserAttributes,
    ident: &syn::Ident,
) -> proc_macro2::TokenStream {
    let ir = &prepared.ir;

    // ── Slab-mode monolithic methods (the only data-producing path) ───────

    let mut ctx = ir_types::IrCodegenCtx::new(
        ir,
        ident,
        parser_attrs,
        prepared.prep.effective_prettify,
    );

    // Copy prep analysis into the codegen context.
    ctx.sp_method_rules = prepared.prep.analysis.sp_method_rules.clone();
    ctx.fused_number_rules = prepared.prep.analysis.fused_number_rules.clone();
    ctx.operator_chain_rules = prepared.prep.analysis.operator_chain_rules.clone();

    let grammar_arr = ir_enums::generate_grammar_arr(parser_attrs, ident);

    // Data-producing codegen: enum + parser methods + slab context.
    // Always generated — prettify parsers also need the data path for SlabCtx.
    let grammar_enum = ir_enums::generate_enum(&ctx);
    let parser_methods = codegen::generate_monolithic(ir, &ctx);

    let has_recovers = ctx
        .ir
        .rules
        .iter()
        .any(|r| r.meta.directives.recover.is_some())
        && !ctx.parser_attrs.skip_recover;
    let enum_ident = &ctx.enum_ident;
    let recovered_static = if has_recovers {
        let recovered_ident = ctx.recovered_static_ident();
        quote! {
            static #recovered_ident: #enum_ident<'static> = #enum_ident::Recovered;
        }
    } else {
        quote! {}
    };

    let (alloc_ctx_struct, alloc_ctx_helper) = ctx.generate_alloc_ctx();
    let alloc_helper_code = quote! { #alloc_ctx_struct #alloc_ctx_helper };

    // ── Fused parse+format ──────────────────────────────────────────────────

    let prettify_methods = if prepared.prep.effective_prettify {
        codegen::prettify::generate_monolithic_prettify(ir, &ctx)
    } else {
        quote! {}
    };

    quote! {
        use ::parse_that::*;

        #grammar_arr

        #grammar_enum
        #alloc_helper_code
        #recovered_static

        impl #ident {
            #parser_methods
            #prettify_methods
        }
    }
}
