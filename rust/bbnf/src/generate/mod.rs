//! Rust parser code generation from BBNF grammars.

pub mod regex;
pub use regex::classify as regex_classify;
pub use crate::backend::rust as codegen;
pub use codegen::ir_enums;
pub use codegen::ir_types;
pub use codegen::ir_types::ParserAttributes;

use crate::backend::PreparedGrammar;
use quote::quote;

/// Generate all parser code from a prepared AOT bundle.
pub fn generate_all(
    prepared: &PreparedGrammar,
    parser_attrs: &ParserAttributes,
    ident: &syn::Ident,
) -> proc_macro2::TokenStream {
    let ir = &prepared.ir;

    let mut ctx = ir_types::IrCodegenCtx::new(ir, ident, parser_attrs, prepared.prep.effective_prettify);
    ctx.sp_method_rules = prepared.prep.analysis.sp_method_rules.clone();
    ctx.fused_number_rules = prepared.prep.analysis.fused_number_rules.clone();
    ctx.operator_chain_rules = prepared.prep.analysis.operator_chain_rules.clone();

    let grammar_arr = ir_enums::generate_grammar_arr(parser_attrs, ident);
    let grammar_enum = ir_enums::generate_enum(&ctx);
    // Monolithic path — the shared driver path is not yet complete for Rust.
    let parser_methods = codegen::generate_monolithic(ir, &ctx);

    let has_recovers = ctx.ir.rules.iter().any(|r| r.meta.directives.recover.is_some())
        && !ctx.parser_attrs.skip_recover;
    let enum_ident = &ctx.enum_ident;
    let recovered_static = if has_recovers {
        let recovered_ident = ctx.recovered_static_ident();
        quote! { static #recovered_ident: #enum_ident<'static> = #enum_ident::Recovered; }
    } else {
        quote! {}
    };

    let (alloc_ctx_struct, alloc_ctx_helper) = ctx.generate_alloc_ctx();

    let prettify_methods = if prepared.prep.effective_prettify {
        codegen::prettify::generate_monolithic_prettify(ir, &ctx)
    } else {
        quote! {}
    };

    quote! {
        use ::parse_that::*;
        #grammar_arr
        #grammar_enum
        #alloc_ctx_struct
        #alloc_ctx_helper
        #recovered_static
        impl #ident {
            #parser_methods
            #prettify_methods
        }
    }
}
