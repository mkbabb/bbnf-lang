//! Monolithic codegen helper utilities.
//!
//! Small utility functions used across multiple monolithic codegen sub-modules:
//! literal emission, discarded expression emission, fallback combinator hoisting,
//! and rule function naming.

use bbnf_ir::{FnDescriptor, IrNode};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::super::ir_types::IrCodegenCtx;
use super::super::unescape_literal;
use super::{emit_mono_expr, emit_ws_trim, MonoCtx};

/// Internal function name for a rule: `__rule_arena`.
pub(crate) fn mono_fn_ident(name: &str) -> syn::Ident {
    format_ident!("__{}_arena", name)
}

/// Emit a discarded expression (separator, open/close delimiter).
///
/// The value is thrown away, so skip enum/box wrapping. Returns `Option<_>`.
pub(crate) fn emit_mono_discarded(
    node: &IrNode,
    strip_ow: bool,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    match node {
        // Strip Map wrappers (value is discarded).
        IrNode::Map { inner, fn_id } => {
            let fd = &ctx.ir.fns[*fn_id as usize];
            match fd {
                FnDescriptor::EnumWrap { .. }
                | FnDescriptor::BoxWrap
                | FnDescriptor::Constant { .. } => {
                    emit_mono_discarded(inner, strip_ow, ctx, mctx)
                }
                _ => emit_mono_expr(node, ctx, mctx, false),
            }
        }
        // Strip OW in sep_by_ws context.
        IrNode::OptionalWhitespace(inner) if strip_ow => {
            emit_mono_discarded(inner, strip_ow, ctx, mctx)
        }
        // Phase 10: OW in discarded context — skip Span construction,
        // just trim ws and check inner (returns Option<()>).
        // Uses custom @ws pattern if configured.
        IrNode::OptionalWhitespace(inner) => {
            let ws_trim = emit_ws_trim(ctx, mctx);
            let inner_discarded = emit_mono_discarded(inner, false, ctx, mctx);

            // Loop invariant hoisting: skip redundant trailing trim when
            // inner already ends with OW.
            if super::expr::ends_with_ow(inner) {
                quote! {
                    {
                        #ws_trim
                        #inner_discarded
                    }
                }
            } else {
                let result_var = mctx.fresh("owd");
                let ws2 = ws_trim.clone();
                quote! {
                    {
                        #ws_trim
                        let #result_var = #inner_discarded;
                        if #result_var.is_some() {
                            #ws2
                        }
                        #result_var
                    }
                }
            }
        }
        // Literal: direct byte check, no Span construction.
        IrNode::Literal(sid) => {
            let raw = ctx.ir.get_string(*sid);
            let unescaped = unescape_literal(raw);
            let bytes = unescaped.as_bytes();
            // Dispatch guaranteed byte: skip bounds check if the byte is already
            // verified by the enclosing dispatch table match.
            if bytes.len() == 1 && mctx.dispatch_guaranteed_byte == Some(bytes[0]) {
                mctx.dispatch_guaranteed_byte = None;
                return quote! { { state.offset += 1; Some(()) } };
            }
            emit_literal_inline(&unescaped, false)
        }
        // For Ref: try fusion first (inline body in discarded context), then _sp path.
        IrNode::Ref(rule_id) => {
            // Fusion: inline discarded body of non-cyclic or single-site cyclic rules.
            let can_inline = mctx.fusion_eligible.get(*rule_id as usize).copied() == Some(true)
                || mctx
                    .single_site_inline
                    .get(*rule_id as usize)
                    .copied()
                    == Some(true);
            if can_inline {
                let rule = &ctx.ir.rules[*rule_id as usize];
                let result = emit_mono_discarded(&rule.body, strip_ow, ctx, mctx);
                return result;
            }
            // Always use monolithic fn call — never construct SpanParser combinators.
            // The monolithic function does the same parsing work without combinator overhead.
            let fn_ident = mono_fn_ident(ctx.resolve_rule_name(*rule_id));
            quote! { Self::#fn_ident(state) }
        }
        // Regex/other — emit via standard path.
        _ => emit_mono_expr(node, ctx, mctx, false),
    }
}

/// Emit direct byte-matching code for a literal string.
///
/// When `need_span` is true, returns `Option<Span<'a>>`.
/// When `need_span` is false (discarded context), returns `Option<()>`.
///
/// Single-byte literals (`:`, `,`, `{`, etc.) compile to a single byte comparison.
/// Multi-byte literals compile to a slice comparison.
pub(crate) fn emit_literal_inline(unescaped: &str, need_span: bool) -> TokenStream {
    let bytes = unescaped.as_bytes();
    if bytes.is_empty() {
        if need_span {
            return quote! { Some(::parse_that::Span::new(state.offset, state.offset, state.src)) };
        } else {
            return quote! { Some(()) };
        }
    }
    if bytes.len() == 1 {
        let b_lit = proc_macro2::Literal::byte_character(bytes[0]);
        if need_span {
            quote! {
                {
                    if state.src_bytes.get(state.offset).copied() == Some(#b_lit) {
                        let __start = state.offset;
                        state.offset += 1;
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    } else {
                        None
                    }
                }
            }
        } else {
            quote! {
                if state.src_bytes.get(state.offset).copied() == Some(#b_lit) {
                    state.offset += 1;
                    Some(())
                } else {
                    None
                }
            }
        }
    } else {
        let len = bytes.len();
        let byte_lits: Vec<proc_macro2::Literal> =
            bytes.iter().map(|b| proc_macro2::Literal::byte_character(*b)).collect();
        if need_span {
            quote! {
                {
                    let __end = state.offset + #len;
                    if state.src_bytes.get(state.offset..__end) == Some(&[#(#byte_lits),*]) {
                        let __start = state.offset;
                        state.offset = __end;
                        Some(::parse_that::Span::new(__start, __end, state.src))
                    } else {
                        None
                    }
                }
            }
        } else {
            quote! {
                {
                    let __end = state.offset + #len;
                    if state.src_bytes.get(state.offset..__end) == Some(&[#(#byte_lits),*]) {
                        state.offset = __end;
                        Some(())
                    } else {
                        None
                    }
                }
            }
        }
    }
}

/// Phase 11: Emit an unchecked single-byte literal check for use in separator positions
/// where `offset < end` is guaranteed by a preceding successful parse.
///
/// Returns `Option<()>` — discarded context only.
pub(crate) fn emit_literal_inline_unchecked(byte: u8) -> TokenStream {
    let b_lit = proc_macro2::Literal::byte_character(byte);
    quote! {
        if unsafe { *state.src_bytes.get_unchecked(state.offset) } == #b_lit {
            state.offset += 1;
            Some(())
        } else {
            None
        }
    }
}
