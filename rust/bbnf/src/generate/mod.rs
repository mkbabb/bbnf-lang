//! Rust parser code generation from BBNF grammars.
//!
//! This module translates a parsed and analysed BBNF grammar into
//! `proc_macro2::TokenStream` parser combinator code via the IR pipeline.

pub mod prettify;
mod types;

// ── IR-based codegen modules ────────────────────────────────────────────────
pub mod fast_paths;
pub mod regex_classify;
pub mod regex_emit;
pub mod ir_codegen;
pub mod ir_enums;
pub mod ir_pretty;
pub mod ir_span;
pub mod ir_types;

pub use types::*;

use quote::{format_ident, quote};

use self::ir_types::StorageMode;

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

    // Enable B.1 span collapse when prettify is disabled — allows Seqs of
    // simple Span children to collapse to a single Span, eliminating arena allocs.
    ir.b1_span_collapse = !parser_attrs.prettify;

    // Compute sp_method_rules via iterative fixed-point BEFORE type inference,
    // so that infer_types uses the correct has_sp_method flags for B.1 override.
    bbnf_ir::passes::compute_sp_method_rules(ir);
    // Run type inference with correct sp_method info.
    bbnf_ir::passes::infer_types(ir);

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
    let parser_methods = generate_ir_parser_methods(ir, &ctx);

    // Generate prettify (to_doc + source_range) if enabled.
    let prettify_impl = if parser_attrs.prettify {
        ir_pretty::generate_prettify_ir(&ctx)
    } else {
        quote! {}
    };

    let (arena_enum, arena_helper, arena_methods, arena_prettify, arena_recovered) = if parser_attrs
        .arena
    {
        let result = {
            let mut arena_ctx =
                ir_types::IrCodegenCtx::new(ir, ident, parser_attrs, StorageMode::Arena);
            arena_ctx.sp_method_rules = ctx.sp_method_rules.clone();

            // Detect fused number rules for arena-specific enum variant override.
            for rule in &ir.rules {
                if let bbnf_ir::IrNode::Regex(sid) = &rule.body {
                    if fast_paths::is_fused_number_regex(ir.get_string(*sid)) {
                        arena_ctx.fused_number_rules.insert(rule.id);
                    }
                }
            }
            let has_recovers = arena_ctx.ir.rules.iter().any(|r| r.meta.recover.is_some())
                && !arena_ctx.parser_attrs.skip_recover;
            let arena_helper_ident = arena_ctx.arena_helper_ident();
            let arena_enum_ident = &arena_ctx.enum_ident;
            let recovered_static = if has_recovers {
                let recovered_ident = arena_ctx.recovered_static_ident();
                quote! {
                    static #recovered_ident: #arena_enum_ident<'static> = #arena_enum_ident::Recovered;
                }
            } else {
                quote! {}
            };
            (
                ir_enums::generate_enum(&arena_ctx),
                quote! {
                    #[allow(non_snake_case)]
                    #[inline(always)]
                    fn #arena_helper_ident<'a>(
                        state: &::parse_that::ParserState<'a>,
                    ) -> &'a ::parse_that::BumpArena<#arena_enum_ident<'a>> {
                        debug_assert!(!state.context_ptr.is_null(), "arena parser requires parse_with_context()");
                        unsafe {
                            &*(state.context_ptr as *const ::parse_that::BumpArena<#arena_enum_ident<'a>>)
                        }
                    }
                },
                ir_codegen::monolithic::generate_monolithic_arena(ir, &arena_ctx),
                if parser_attrs.prettify {
                    ir_pretty::generate_prettify_ir(&arena_ctx)
                } else {
                    quote! {}
                },
                recovered_static,
            )
        };
        result
    } else {
        (quote! {}, quote! {}, quote! {}, quote! {}, quote! {})
    };

    // Span-only monolithic mode: emit fn __rule_span(state) -> Option<Span>.
    // Zero allocations. Requires no custom Map functions in grammar.
    let span_methods = if parser_attrs.span {
        ir_codegen::monolithic::span::generate_monolithic_span(ir, &ctx)
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
        }

        #prettify_impl
        #arena_prettify
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
        let ident = ctx.method_ident_for_name(name);

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
        ctx.no_collapse
            .set(rule.meta.no_collapse || rule.meta.pretty.is_some());

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
                parser = quote! { #parser.recover(#sync_ts.map(|_| ()), #sentinel) };
            }
        }

        // Debug → .debug("name") (parse_that combinator, feature-gated).
        if rule.meta.debug || ctx.parser_attrs.debug {
            parser = ir_codegen::trace::emit_combinator_debug(parser, name);
        }

        methods.push(quote! {
            pub fn #ident<'a>() -> Parser<'a, #ty> {
                #parser
            }
        });

        // Unboxed variant for transparent rules — used in Vec, Optional, and
        // discarded contexts where Box indirection is unnecessary.
        // Non-transparent rules don't need _unboxed() — their normal method
        // already returns Enum; boxing happens at the call site.
        if rule.meta.is_transparent {
            let unboxed_ident = ctx.unboxed_method_ident_for_name(name);
            let unboxed_ty = ctx.enum_type.clone();

            // Check if elide_box codegen is safe for this rule's body.
            // Heterogeneous Alts produce BoxedEnum even with elide_box=true,
            // so we fall back to unboxing the normal method's result.
            let body_ty = ir_codegen::infer::infer_node_type_elide_box(&rule.body, &ctx);
            let mut unboxed_parser = if body_ty == bbnf_ir::TypeDesc::BoxedEnum {
                // Heterogeneous Alt: elide_box won't work. Delegate to the
                // normal (boxed) method and dereference the Box.
                quote! { Self::#ident().map(|x| *x) }
            } else {
                ir_codegen::ir_node_to_tokens_elide(&rule.body, ctx, true)
            };

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
        if rule.meta.span_eligible && !ctx.uses_arena() {
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
