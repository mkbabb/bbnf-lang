//! Rust parser code generation from BBNF grammars.
//!
//! All codegen goes through the monolithic path. Arena and Owned modes differ
//! only in allocation strategy (arena.alloc vs Box::new).

// ── Codegen modules ────────────────────────────────────────────────────────
pub mod regex_ir;

// Backward-compat alias — all call sites use `regex_classify::RegexClass` and `classify_regex()`.
pub use regex_ir::classify as regex_classify;

pub mod codegen;

pub use codegen::ir_enums;
pub use codegen::ir_types;
pub use codegen::ir_types::ParserAttributes;

use quote::quote;

use self::ir_types::StorageMode;

// ── Entry point ────────────────────────────────────────────────────────────

/// Generate all parser code from IR: enum, parser methods, and optionally prettify.
///
/// All codegen goes through the monolithic path. Arena and Owned modes differ
/// only in allocation strategy (arena.alloc vs Box::new).
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
            rule.meta.pretty = None;
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

    // ── Owned-mode monolithic methods ───────────────────────────────────────

    let mut ctx = ir_types::IrCodegenCtx::new(ir, ident, parser_attrs, StorageMode::Owned);

    // Copy has_sp_method from IR metadata to ctx.sp_method_rules for codegen.
    ctx.sp_method_rules = ir
        .rules
        .iter()
        .filter(|r| r.meta.has_sp_method)
        .map(|r| ir.get_string(r.name).to_string())
        .collect();

    let grammar_arr = ir_enums::generate_grammar_arr(parser_attrs, ident);
    let grammar_enum = ir_enums::generate_enum(&ctx);
    let parser_methods = codegen::generate_monolithic(ir, &ctx);

    // ── Arena-mode monolithic methods (optional) ────────────────────────────

    let (arena_enum, arena_helper, arena_methods, arena_recovered) = if parser_attrs.arena {
        let mut arena_ctx =
            ir_types::IrCodegenCtx::new(ir, ident, parser_attrs, StorageMode::Arena);
        arena_ctx.sp_method_rules = ctx.sp_method_rules.clone();

        // Detect fused number rules for arena-specific enum variant override.
        if !parser_attrs.prettify {
            for rule in &ir.rules {
                if let bbnf_ir::IrNode::Regex(sid) = &rule.body {
                    if regex_ir::fast_paths::is_fused_number_regex(ir.get_string(*sid)) {
                        arena_ctx.fused_number_rules.insert(rule.id);
                    }
                }
            }
        }
        let has_recovers = arena_ctx.ir.rules.iter().any(|r| r.meta.recover.is_some())
            && !arena_ctx.parser_attrs.skip_recover;
        let arena_enum_ident = &arena_ctx.enum_ident;
        let recovered_static = if has_recovers {
            let recovered_ident = arena_ctx.recovered_static_ident();
            quote! {
                static #recovered_ident: #arena_enum_ident<'static> = #arena_enum_ident::Recovered;
            }
        } else {
            quote! {}
        };
        let arena_helper_code = if !parser_attrs.prettify {
            let (arena_ctx_struct, arena_ctx_helper) = arena_ctx.generate_arena_ctx();
            quote! { #arena_ctx_struct #arena_ctx_helper }
        } else {
            // Prettify+arena: use BumpArena directly until IR fusion wrapping is fixed.
            let arena_helper_ident = arena_ctx.arena_helper_ident();
            let arena_enum_ident = &arena_ctx.enum_ident;
            quote! {
                #[allow(non_snake_case)]
                #[inline(always)]
                fn #arena_helper_ident<'a>(
                    state: &::parse_that::ParserState<'a>,
                ) -> &'a ::parse_that::BumpArena<#arena_enum_ident<'a>> {
                    debug_assert!(!state.context_ptr.is_null(), "arena parser requires parse_with_context()");
                    unsafe { &*(state.context_ptr as *const ::parse_that::BumpArena<#arena_enum_ident<'a>>) }
                }
            }
        };
        (
            ir_enums::generate_enum(&arena_ctx),
            arena_helper_code,
            codegen::generate_monolithic(ir, &arena_ctx),
            recovered_static,
        )
    } else {
        (quote! {}, quote! {}, quote! {}, quote! {})
    };

    // ── Span-only monolithic mode ───────────────────────────────────────────

    let span_methods = if parser_attrs.span {
        codegen::span::generate_monolithic_span(ir, &ctx)
    } else {
        quote! {}
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
        #arena_enum
        #arena_helper
        #arena_recovered

        impl #ident {
            #parser_methods
            #arena_methods
            #span_methods
            #prettify_methods
        }
    }
}
