//! Rust parser code generation from BBNF grammars.
//!
//! This module translates a parsed and analysed BBNF grammar into
//! `proc_macro2::TokenStream` parser combinator code via the IR pipeline.

mod types;
pub mod prettify;

// ── IR-based codegen modules ────────────────────────────────────────────────
pub mod fast_paths;
pub mod ir_types;
pub mod ir_enums;
pub mod ir_codegen;
pub mod ir_span;
pub mod ir_pretty;

pub use types::*;

use quote::{format_ident, quote};

// ── IR-based generate_all entry point ───────────────────────────────────────

/// Generate all parser code from IR: enum, parser methods, and optionally prettify.
///
/// This is the IR-based replacement for the legacy AST pipeline.
pub fn generate_all(
    ir: &mut bbnf_ir::GrammarIR,
    parser_attrs: &ParserAttributes,
    ident: &syn::Ident,
) -> proc_macro2::TokenStream {
    // When prettify is not enabled, clear @pretty metadata so that
    // no_collapse is not applied for @pretty rules — this allows span
    // compression in Seq codegen, which is critical for throughput.
    if !parser_attrs.prettify {
        for rule in &mut ir.rules {
            rule.meta.pretty = None;
        }
    }

    // Compute sp_method_rules via iterative fixed-point BEFORE type inference,
    // so that infer_types uses the correct has_sp_method flags for B.1 override.
    bbnf_ir::passes::compute_sp_method_rules(ir);
    // Run type inference with correct sp_method info.
    bbnf_ir::passes::infer_types(ir);

    let mut ctx = ir_types::IrCodegenCtx::new(ir, ident, parser_attrs);

    // Copy has_sp_method from IR metadata to ctx.sp_method_rules for codegen.
    ctx.sp_method_rules = ir.rules.iter()
        .filter(|r| r.meta.has_sp_method)
        .map(|r| ir.get_string(r.name).to_string())
        .collect();

    let grammar_arr = ir_enums::generate_grammar_arr(parser_attrs, ident);
    let grammar_enum = ir_enums::generate_enum(&ctx);
    let parser_methods = generate_ir_parser_methods(ir, &ctx);

    // Generate prettify (to_doc + source_range) if enabled.
    let prettify_impl = if parser_attrs.prettify {
        ir_pretty::generate_prettify_ir(&ctx)
    } else {
        quote! {}
    };

    quote! {
        use ::parse_that::*;

        #grammar_arr

        #grammar_enum

        impl #ident {
            #parser_methods
        }

        #prettify_impl
    }
}

/// Generate parser methods (+ _sp methods) for all rules from IR.
fn generate_ir_parser_methods(
    ir: &bbnf_ir::GrammarIR,
    ctx: &ir_types::IrCodegenCtx<'_>,
) -> proc_macro2::TokenStream {
    let mut methods: Vec<proc_macro2::TokenStream> = Vec::new();

    for rule in &ir.rules {
        let name = ir.get_string(rule.name);
        let ident = format_ident!("{}", name);

        // Determine return type.
        let ty = ctx.rule_return_type(rule.id);

        // Generate the parser body using inline direct-dispatch codegen.
        // The enum variant wrapping is absorbed into the inline closure,
        // eliminating an outer `.map()` Box allocation.
        let enum_wrap = if !rule.meta.is_transparent {
            let variant_ident = format_ident!("{}", name);
            Some(variant_ident)
        } else {
            None
        };

        // Set no_collapse for rules with @pretty or @no_collapse — prevents
        // Span compression in Seq so the codegen type matches the IR type.
        ctx.no_collapse.set(rule.meta.no_collapse || rule.meta.pretty.is_some());

        let mut parser = ir_codegen::emit_rule_body_inline(
            &rule.body,
            ctx,
            enum_wrap
                .as_ref()
                .map(|v| (&ctx.enum_ident as &syn::Ident, v as &syn::Ident)),
        );

        // Cyclic → lazy().
        if rule.meta.is_cyclic {
            parser = quote! { ::parse_that::lazy(|| #parser) };
        }

        // Memoization → .memoize().
        if matches!(
            rule.meta.memo,
            bbnf_ir::MemoStrategy::Full | bbnf_ir::MemoStrategy::Selective
        ) {
            parser = quote! { #parser.memoize() };
        }

        // Recovery → .recover().
        if let Some(ref sync) = rule.meta.recover {
            if !ctx.parser_attrs.skip_recover {
                let sync_ts = ir_codegen::ir_node_to_tokens(sync, ctx);
                let sentinel = ctx.recover_sentinel(rule.id);
                parser =
                    quote! { #parser.recover(#sync_ts.map(|_| ()), #sentinel) };
            }
        }

        methods.push(quote! {
            pub fn #ident<'a>() -> Parser<'a, #ty> {
                #parser
            }
        });

        // Unboxed variant for transparent rules — used in Vec contexts where
        // Box indirection is unnecessary (Vec provides heap indirection).
        // Returns Enum directly instead of Box<Enum>.
        if rule.meta.is_transparent {
            let unboxed_ident = format_ident!("{}_unboxed", name);
            let unboxed_ty = ctx.enum_type.clone();

            // Generate the body with in_vec=true so inner refs skip boxing.
            let mut unboxed_parser =
                ir_codegen::ir_node_to_tokens_vec(&rule.body, ctx, true);

            if rule.meta.is_cyclic {
                unboxed_parser = quote! { ::parse_that::lazy(|| #unboxed_parser) };
            }

            if matches!(
                rule.meta.memo,
                bbnf_ir::MemoStrategy::Full | bbnf_ir::MemoStrategy::Selective
            ) {
                unboxed_parser = quote! { #unboxed_parser.memoize() };
            }

            methods.push(quote! {
                #[inline(always)]
                pub fn #unboxed_ident<'a>() -> Parser<'a, #unboxed_ty> {
                    #unboxed_parser
                }
            });
        }

        // SpanParser _sp() method.
        if rule.meta.span_eligible {
            if let Some(sp) = ir_span::try_ir_span_parser(rule.id, ctx) {
                let sp_ident = format_ident!("{}_sp", name);
                methods.push(quote! {
                    #[inline(always)]
                    pub fn #sp_ident<'a>() -> ::parse_that::SpanParser<'a> {
                        #sp
                    }
                });
            }
        }
    }

    quote! { #(#methods)* }
}
