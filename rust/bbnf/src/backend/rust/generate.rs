//! Monolithic entry-point generation.
//!
//! Shared codegen analysis lives in `analysis/*`; this module only orchestrates
//! per-rule emission from prepared decisions.

use bbnf_ir::GrammarIR;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::analysis::inline::analyze_parse_inline_plan;
use super::helpers::mono_fn_ident;
use super::ir_types::IrCodegenCtx;
use super::{MonoCtx, emit_mono_expr};

// ── Entry Point ──────────────────────────────────────────────────────────────

/// Generate all monolithic methods for all rules.
///
/// Slab-only: `fn __rule<'a>(state) -> Option<Enum<'a>>` with slab.alloc.
///
/// For each rule, emits:
/// 1. A private associated fn (internal dispatch)
/// 2. A public method returning `Parser<'a, ReturnType>`
/// 3. For transparent rules: an unboxed variant
pub fn generate_monolithic(ir: &GrammarIR, ctx: &IrCodegenCtx<'_>) -> TokenStream {
    let mut methods: Vec<TokenStream> = Vec::new();
    let enum_type = &ctx.enum_type;

    let inline_plan = analyze_parse_inline_plan(ir, &ctx.operator_chain_rules);

    for rule in &ir.rules {
        let name = ir.get_string(rule.name);
        let fn_ident = mono_fn_ident(name);
        let pub_ident = ctx.method_ident_for_name(name);
        let return_type = ctx.rule_return_type(rule.id);

        // ── Generate internal function body ──────────────────────────────

        let mut mctx = MonoCtx::new(inline_plan.parse_call_modes.clone());
        mctx.current_rule_id = Some(rule.id);

        // Fused number scan+convert: if the rule body is a JSON number regex,
        // emit number_scan_convert which returns (Span, f64) in one pass.
        // The enum variant stores (Span<'a>, f64) instead of plain Span.
        // Fused number: bare JSON number regex → (Span, f64) enum variant.
        // NumberConvert (from -> f64 map) is handled separately by emit_mono_map —
        // it produces f64 directly, NOT (Span, f64).
        // Skip when prettify is enabled — formatters only need Spans.
        let is_fused_number = ctx.fused_number_rules.contains(&rule.id);

        // All internal fns return Option<Enum<'a>>.
        // Transparent rules: body emitted with elide_box=true (returns Enum directly).
        // Non-transparent rules: body emitted with elide_box=false, wrapped in enum variant.
        let body_expr = if is_fused_number && !rule.meta.is_transparent {
            let variant_ident = format_ident!("{}", name);
            let enum_ident = &ctx.enum_ident;
            quote! {
                ::parse_that::number_scan_convert(state)
                    .map(|__x| #enum_ident::#variant_ident(__x))
            }
        } else if let Some(chain_expr) =
            super::operator_chain::emit_operator_chain_rule(rule.id, ctx, &mut mctx)
        {
            if rule.meta.is_transparent {
                chain_expr
            } else {
                let variant_ident = format_ident!("{}", name);
                let enum_ident = &ctx.enum_ident;
                quote! { #chain_expr.map(|__x| #enum_ident::#variant_ident(__x)) }
            }
        } else if rule.meta.is_transparent {
            emit_mono_expr(&rule.body, ctx, &mut mctx, true)
        } else {
            let variant_ident = format_ident!("{}", name);
            let enum_ident = &ctx.enum_ident;
            let inner = emit_mono_expr(&rule.body, ctx, &mut mctx, false);
            quote! { #inner.map(|__x| #enum_ident::#variant_ident(__x)) }
        };

        let hoisted = &mctx.hoisted;

        let fn_body = quote! {
            #(#hoisted)*
            #body_expr
        };

        // ── Emit internal function ───────────────────────────────────────

        let rule_debug = ir.debug_all || rule.meta.directives.debug;
        let instrumented_body = if rule_debug {
            let trace_entry = super::trace::emit_trace_entry(name);
            let result_ident = syn::Ident::new("__trace_result", proc_macro2::Span::call_site());
            let trace_exit = super::trace::emit_trace_exit(name, &result_ident);
            quote! {
                #trace_entry
                let #result_ident = (|| -> Option<#enum_type> { #fn_body })();
                #trace_exit
                #result_ident
            }
        } else {
            fn_body
        };

        methods.push(quote! {
            #[allow(non_snake_case)]
            fn #fn_ident<'a>(
                state: &mut ::parse_that::ParserState<'a>,
            ) -> Option<#enum_type> {
                #instrumented_body
            }
        });

        // ── Emit sync function + recovery wrapping ─────────────────────

        let has_recover = rule.meta.directives.recover.is_some() && !ctx.parser_attrs.skip_recover;

        if let Some(ref sync_node) = rule.meta.directives.recover {
            if !ctx.parser_attrs.skip_recover {
                let sync_ident = format_ident!("__sync_{}", name);
                let mut sync_mctx = MonoCtx::new(inline_plan.parse_call_modes.clone());
                let sync_body = emit_mono_expr(sync_node, ctx, &mut sync_mctx, false);
                let sync_hoisted = &sync_mctx.hoisted;
                methods.push(quote! {
                    #[allow(non_snake_case)]
                    fn #sync_ident<'a>(
                        state: &mut ::parse_that::ParserState<'a>,
                    ) -> Option<()> {
                        #(#sync_hoisted)*
                        (#sync_body).map(|_| ())
                    }
                });
            }
        }

        // ── Emit public method(s) ────────────────────────────────────────

        if rule.meta.is_transparent {
            // Transparent: public method wraps result in Box (Owned) or slab.alloc (Slab).
            let alloc_code = ctx.emit_alloc(&quote! { __v });

            let mut pub_parser = quote! {
                Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                    let __v = Self::#fn_ident(state)?;
                    Some(#alloc_code)
                })
            };

            if has_recover {
                let sync_ident = format_ident!("__sync_{}", name);
                let sentinel = ctx.recover_sentinel(rule.id);
                pub_parser = quote! {
                    #pub_parser.recover(Parser::new(Self::#sync_ident), #sentinel)
                };
            }

            methods.push(quote! {
                pub fn #pub_ident<'a>() -> Parser<'a, #return_type> {
                    #pub_parser
                }
            });

            // Unboxed variant: direct delegation (no recovery wrapping — unboxed
            // is used internally, recovery is on the public boxed method).
            let unboxed_ident = ctx.unboxed_method_ident_for_name(name);
            methods.push(quote! {
                #[inline(always)]
                pub fn #unboxed_ident<'a>() -> Parser<'a, #enum_type> {
                    Parser::new(Self::#fn_ident)
                }
            });
        } else {
            // Non-transparent: direct delegation (fn already returns Enum).
            let mut pub_parser = quote! { Parser::new(Self::#fn_ident) };

            if has_recover {
                let sync_ident = format_ident!("__sync_{}", name);
                let sentinel = ctx.recover_sentinel(rule.id);
                pub_parser = quote! {
                    #pub_parser.recover(Parser::new(Self::#sync_ident), #sentinel)
                };
            }

            methods.push(quote! {
                pub fn #pub_ident<'a>() -> Parser<'a, #return_type> {
                    #pub_parser
                }
            });
        }
    }

    // Emit the thread-local depth counter if any rule is debug-instrumented.
    let has_debug = ir.debug_all || ir.rules.iter().any(|r| r.meta.directives.debug);
    let depth_counter = if has_debug {
        super::trace::emit_depth_counter()
    } else {
        quote! {}
    };

    quote! {
        #depth_counter
        #(#methods)*
    }
}
