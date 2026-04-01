//! Span-only monolithic code generation.
//!
//! Emits `fn __rule_span(state) -> Option<Span<'a>>` for every rule — direct
//! recursive functions that return raw Span values with zero allocations.
//! No enum variants, no arena, no Vec collection for Repeat.
//!
//! This codegen path is activated by `#[parser(span)]` and requires that the
//! grammar has no custom Map functions (all rules are span-compatible).
//!
//! Sub-modules mirror the arena monolithic structure:
//! - `alt`: dispatch-table and flat checkpoint alternation
//! - `repeat`: quantifiers, sep_by, optional
//! - `expr`: leaf, ref, seq, skip/next/wrap, OW, sep_by_ws variants

mod alt;
mod expr;
mod repeat;

use bbnf_ir::{GrammarIR, IrNode};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::regex_ir::fast_paths;
use super::ir_types::IrCodegenCtx;
use super::unescape_literal;
use super::{MonoCtx, emit_ws_trim};

/// Function name for a span-only rule: `__rule_span`.
pub(in crate::generate) fn span_fn_ident(name: &str) -> syn::Ident {
    format_ident!("__{}_span", name)
}

/// Generate all span-only monolithic methods for all rules.
pub fn generate_monolithic_span(ir: &GrammarIR, ctx: &IrCodegenCtx<'_>) -> TokenStream {
    let mut methods: Vec<TokenStream> = Vec::new();

    let fusion_eligible: Vec<bool> = ir
        .rules
        .iter()
        .map(|rule| {
            rule.meta.directives.token
                || (!rule.meta.is_cyclic
                    && rule.meta.directives.recover.is_none()
                    && rule.meta.directives.pretty.is_none())
        })
        .collect();

    let single_site_inline = super::compute_single_site_inline(ir);

    for rule in &ir.rules {
        let name = ir.get_string(rule.name);
        let fn_ident = span_fn_ident(name);
        let pub_ident = format_ident!("{}_span", name);

        let mut mctx = MonoCtx::new(fusion_eligible.clone(), single_site_inline.clone());
        mctx.current_rule_name = Some(name.to_string());

        let body_expr = emit_span_expr(&rule.body, ir, ctx, &mut mctx);
        let hoisted = &mctx.hoisted;

        let fn_body = quote! { #(#hoisted)* #body_expr };

        let rule_debug = ir.debug_all || rule.meta.directives.debug;
        let instrumented_body = if rule_debug {
            let trace_entry = super::trace::emit_trace_entry(name);
            let result_ident = syn::Ident::new("__trace_result", proc_macro2::Span::call_site());
            let trace_exit = super::trace::emit_trace_exit(name, &result_ident);
            quote! {
                #trace_entry
                let #result_ident = (|| -> Option<::parse_that::Span<'a>> { #fn_body })();
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
            ) -> Option<::parse_that::Span<'a>> {
                #instrumented_body
            }
        });

        methods.push(quote! {
            pub fn #pub_ident<'a>() -> Parser<'a, ::parse_that::Span<'a>> {
                Parser::new(Self::#fn_ident)
            }
        });
    }

    // Emit thread-local depth counter if any rule is debug-instrumented.
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

/// Emit span-only code for an IrNode. Returns `Option<Span<'a>>`.
pub(super) fn emit_span_expr(
    node: &IrNode,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    match node {
        IrNode::Literal(sid) => {
            let raw = ir.get_string(*sid);
            let unescaped = unescape_literal(raw);
            let bytes = unescaped.as_bytes();
            if bytes.len() == 1 && mctx.dispatch_guaranteed_byte == Some(bytes[0]) {
                mctx.dispatch_guaranteed_byte = None;
                return quote! {
                    {
                        let __start = state.offset;
                        state.offset += 1;
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    }
                };
            }
            super::emit_literal_inline(&unescaped, true)
        }

        IrNode::Regex(sid) => {
            let pattern = ir.get_string(*sid);
            // Span-only: no fused number conversion (emit_regex_direct_call
            // without fuse returns number_span_fast for JSON numbers).
            // 1. Try known fast paths (css_ident_fast, number_span_fast, etc.)
            if let Some(direct) = fast_paths::emit_regex_direct_call(pattern) {
                direct
            }
            // 2. Try HIR-based inline compilation
            else if let Some(inline) = super::super::regex_ir::try_emit_regex_inline(pattern) {
                inline
            }
            // 3. Try DFA-based inline compilation
            else if let Some(dfa_code) = super::super::regex_ir::try_emit_dfa_inline(pattern) {
                dfa_code
            }
            // 4. Unsupported pattern — compile-time error
            else {
                super::super::regex_ir::emit_regex_unsupported(pattern)
            }
        }

        IrNode::Epsilon => {
            quote! { Some(::parse_that::Span::new(state.offset, state.offset, state.src)) }
        }

        IrNode::Ref(rule_id) => expr::emit_span_ref(*rule_id, ir, ctx, mctx),
        IrNode::Seq(children) => expr::emit_span_seq(children, ir, ctx, mctx),

        IrNode::Alt(branches, dispatch) => {
            alt::emit_span_alt(branches, dispatch.as_ref(), ir, ctx, mctx)
        }

        IrNode::Repeat { inner, lo, hi } => {
            if *lo == 0 && *hi == 1 {
                repeat::emit_span_optional(inner, ir, ctx, mctx)
            } else {
                repeat::emit_span_repeat(inner, *lo, ir, ctx, mctx)
            }
        }

        IrNode::Skip(left, right) => expr::emit_span_skip(left, right, ir, ctx, mctx),
        IrNode::Next(left, right) => expr::emit_span_next(left, right, ir, ctx, mctx),
        IrNode::Minus(main, excluded) => expr::emit_span_minus(main, excluded, ir, ctx, mctx),
        IrNode::Negate(inner) => expr::emit_span_negate(inner, ir, ctx, mctx),
        IrNode::Map { inner, .. } => emit_span_expr(inner, ir, ctx, mctx),
        IrNode::OptionalWhitespace(inner) => expr::emit_span_ow(inner, ir, ctx, mctx),
        IrNode::TokenDispatch { .. } => {
            todo!("TokenDispatch span codegen not yet implemented")
        }
    }
}

/// Emit a discarded span expression (value thrown away, just advance offset).
pub(super) fn emit_span_discarded(
    node: &IrNode,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    match node {
        IrNode::Map { inner, .. } => emit_span_discarded(inner, ir, ctx, mctx),

        IrNode::OptionalWhitespace(inner) => {
            let ws_trim = emit_ws_trim(ctx, mctx);
            let inner_d = emit_span_discarded(inner, ir, ctx, mctx);

            // Loop invariant hoisting: skip redundant trailing trim when
            // inner already ends with OW.
            if super::expr::ends_with_ow(inner) {
                quote! {
                    {
                        #ws_trim
                        #inner_d
                    }
                }
            } else {
                let ws2 = ws_trim.clone();
                let var = mctx.fresh("owd");
                quote! {
                    {
                        #ws_trim
                        let #var = #inner_d;
                        if #var.is_some() { #ws2 }
                        #var
                    }
                }
            }
        }

        IrNode::Literal(sid) => {
            let raw = ir.get_string(*sid);
            let unescaped = unescape_literal(raw);
            let bytes = unescaped.as_bytes();
            if bytes.len() == 1 && mctx.dispatch_guaranteed_byte == Some(bytes[0]) {
                mctx.dispatch_guaranteed_byte = None;
                return quote! { { state.offset += 1; Some(()) } };
            }
            super::emit_literal_inline(&unescaped, false)
        }

        IrNode::Ref(rule_id) => {
            let can_inline = mctx.fusion_eligible.get(*rule_id as usize).copied() == Some(true)
                || mctx.single_site_inline.get(*rule_id as usize).copied() == Some(true);
            if can_inline {
                let rule = &ir.rules[*rule_id as usize];
                return emit_span_discarded(&rule.body, ir, ctx, mctx);
            }
            let rule = &ir.rules[*rule_id as usize];
            let name = ir.get_string(rule.name);
            let fn_ident = span_fn_ident(name);
            quote! { Self::#fn_ident(state).map(|_| ()) }
        }

        _ => {
            let expr = emit_span_expr(node, ir, ctx, mctx);
            quote! { (#expr).map(|_| ()) }
        }
    }
}
