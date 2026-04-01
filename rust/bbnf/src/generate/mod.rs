//! Rust parser code generation from BBNF grammars.
//!
//! All codegen goes through the monolithic arena path.

// ── Codegen modules ────────────────────────────────────────────────────────
pub mod regex;

// Backward-compat alias — existing callers that reference regex_classify.
pub use regex::classify as regex_classify;

pub mod codegen;

pub use codegen::ir_enums;
pub use codegen::ir_types;
pub use codegen::ir_types::ParserAttributes;

use quote::quote;


// ── Entry point ────────────────────────────────────────────────────────────

/// Generate all parser code from IR: enum, parser methods, and optionally prettify.
///
/// All codegen goes through the monolithic arena path.
pub fn generate_all(
    ir: &mut bbnf_ir::GrammarIR,
    parser_attrs: &ParserAttributes,
    ident: &syn::Ident,
) -> proc_macro2::TokenStream {
    // When prettify is not enabled, clear @pretty metadata so that
    // pretty_preserve is not applied — this allows span compression
    // in Seq codegen, which is critical for throughput.
    if !parser_attrs.prettify {
        for rule in &mut ir.rules {
            rule.meta.directives.pretty = None;
        }
    }

    // Enable B.1 span collapse when prettify is disabled — allows Seqs of
    // simple Span children to collapse to a single Span, eliminating arena allocs.
    ir.b1_span_collapse = !parser_attrs.prettify;

    // Compute sp_method_rules via iterative fixed-point BEFORE type inference,
    // so that infer_types uses the correct has_sp_method flags for B.1 override.
    bbnf_ir::passes::compute_sp_method_rules(ir);
    // Run type inference with correct sp_method info.
    bbnf_ir::passes::infer_types(ir);

    // ── Arena-mode monolithic methods (the only data-producing path) ──────

    let mut ctx = ir_types::IrCodegenCtx::new(ir, ident, parser_attrs);

    // Copy has_sp_method from IR metadata to ctx.sp_method_rules for codegen.
    ctx.sp_method_rules = ir
        .rules
        .iter()
        .filter(|r| r.meta.has_sp_method)
        .map(|r| ir.get_string(r.name).to_string())
        .collect();

    // Detect fused number rules for enum variant override.
    if !parser_attrs.prettify {
        for rule in &ir.rules {
            if let bbnf_ir::IrNode::Regex(sid) = &rule.body {
                if regex::is_fused_number_regex(ir.get_string(*sid)) {
                    ctx.fused_number_rules.insert(rule.id);
                }
            }
        }
    }

    let grammar_arr = ir_enums::generate_grammar_arr(parser_attrs, ident);

    // Data-producing codegen (enum + parser methods + arena context).
    // Skipped for prettify-only parsers — they use _prettify() methods exclusively.
    let needs_data_codegen = parser_attrs.arena || !parser_attrs.prettify;
    let (grammar_enum, parser_methods, recovered_static, arena_helper_code) = if needs_data_codegen
    {
        let grammar_enum = ir_enums::generate_enum(&ctx);
        let parser_methods = codegen::generate_monolithic(ir, &ctx);

        let has_recovers = ctx.ir.rules.iter().any(|r| r.meta.directives.recover.is_some())
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

        let arena_helper_code = if !parser_attrs.prettify {
            let (arena_ctx_struct, arena_ctx_helper) = ctx.generate_arena_ctx();
            quote! { #arena_ctx_struct #arena_ctx_helper }
        } else {
            let arena_helper_ident = ctx.arena_helper_ident();
            let enum_ident = &ctx.enum_ident;
            quote! {
                #[allow(non_snake_case)]
                #[inline(always)]
                fn #arena_helper_ident<'a>(
                    state: &::parse_that::ParserState<'a>,
                ) -> &'a ::parse_that::BumpArena<#enum_ident<'a>> {
                    debug_assert!(!state.context_ptr.is_null(), "arena parser requires parse_with_context()");
                    unsafe { &*(state.context_ptr as *const ::parse_that::BumpArena<#enum_ident<'a>>) }
                }
            }
        };

        (grammar_enum, parser_methods, recovered_static, arena_helper_code)
    } else {
        (quote! {}, quote! {}, quote! {}, quote! {})
    };

    // ── Fused parse+format ──────────────────────────────────────────────────

    let prettify_methods = if parser_attrs.prettify {
        codegen::prettify::generate_monolithic_prettify(ir, &ctx)
    } else {
        quote! {}
    };

    quote! {
        use ::parse_that::*;

        #grammar_arr

        #grammar_enum
        #arena_helper_code
        #recovered_static

        impl #ident {
            #parser_methods
            #prettify_methods
        }
    }
}
