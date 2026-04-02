//! Rust parser code generation from BBNF grammars.
//!
//! All codegen goes through the monolithic monolithic path.

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
/// All codegen goes through the monolithic monolithic path.
pub fn generate_all(
    ir: &mut bbnf_ir::GrammarIR,
    parser_attrs: &ParserAttributes,
    ident: &syn::Ident,
) -> proc_macro2::TokenStream {
    // When prettify is not enabled, clear @pretty metadata so that
    // preserve_spans is not applied — this allows span compression
    // in Seq codegen, which is critical for throughput.
    if !parser_attrs.prettify {
        for rule in &mut ir.rules {
            rule.meta.directives.pretty = None;
        }
    }

    // Enable simple Span collapse when prettify is disabled — allows Seqs of
    // simple Span children to collapse to a single Span, eliminating slab allocs.
    ir.collapse_simple_spans = !parser_attrs.prettify;

    // Compute sp_method_rules via iterative fixed-point BEFORE type inference,
    // so that project_types uses the correct has_sp_method flags for span-method override.
    bbnf_ir::passes::compute_sp_method_rules(ir);
    // Run type inference with correct sp_method info.
    bbnf_ir::passes::project_types(ir);

    // ── Slab-mode monolithic methods (the only data-producing path) ───────

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

    // Data-producing codegen: enum + parser methods + slab context.
    // Always generated — prettify parsers also need the data path for SlabCtx.
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

    let (alloc_ctx_struct, alloc_ctx_helper) = ctx.generate_alloc_ctx();
    let alloc_helper_code = quote! { #alloc_ctx_struct #alloc_ctx_helper };

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
        #alloc_helper_code
        #recovered_static

        impl #ident {
            #parser_methods
            #prettify_methods
        }
    }
}
