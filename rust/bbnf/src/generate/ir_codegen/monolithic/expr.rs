//! Monolithic expression helpers: Ref, Skip/Next, Wrap, Map, OptionalWhitespace.

use bbnf_ir::{FnDescriptor, IrNode};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::super::ir_types::IrCodegenCtx;
use super::super::repeat as combinator_repeat;
use super::super::unescape_literal;
use super::repeat::{emit_mono_sep_by_ws, emit_mono_sep_by_core, try_unchecked_sep, SepByConfig};
use super::{emit_mono_discarded, emit_mono_expr, mono_fn_ident, MonoCtx};

// ── Ref ──────────────────────────────────────────────────────────────────────

/// Emit a monolithic Ref — direct function call, or inline body for fusion-eligible rules.
///
/// All internal fns return `Option<ArenaEnum<'a>>`.
/// - `elide_box = true`:  return ArenaEnum directly
/// - `elide_box = false`: arena.alloc → `&'a ArenaEnum<'a>`
///
/// **Fusion**: When the target rule is fusion-eligible (non-cyclic, no @recover/@pretty/@no_collapse),
/// its body is inlined at the call site. This lets LLVM see through the code and hoists
/// the inlined body's leaf parsers into the caller's scope (created once, reused per iteration).
pub(super) fn emit_mono_ref(
    rule_id: bbnf_ir::RuleId,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    elide_box: bool,
) -> TokenStream {
    // Phase 3: Inline fusion-eligible rule bodies at call sites (non-cyclic rules).
    // Phase 9: Also inline single-site cyclic rules (e.g. `pair` called only from `object`).
    let can_inline = mctx.fusion_eligible.get(rule_id as usize).copied() == Some(true)
        || mctx
            .single_site_inline
            .get(rule_id as usize)
            .copied()
            == Some(true);
    if can_inline {
        let rule = &ctx.ir.rules[rule_id as usize];

        // Save/restore no_collapse: fusion-eligible rules always have no_collapse=false,
        // but the caller might have it set (e.g., @pretty rule calling a leaf rule).
        let saved_no_collapse = ctx.no_collapse.get();
        ctx.no_collapse.set(false);

        let result = if rule.meta.is_transparent {
            // Transparent: body emitted with elide_box=true, returns inner type.
            let body = emit_mono_expr(&rule.body, ctx, mctx, true);
            if elide_box {
                body
            } else {
                let helper = ctx.arena_helper_ident();
                quote! {
                    #body.map(|__v| {
                        let __alloc = #helper(state).alloc(__v);
                        &*__alloc
                    })
                }
            }
        } else {
            // Non-transparent: body emitted with elide_box=false, wrapped in enum variant.
            let inner = emit_mono_expr(&rule.body, ctx, mctx, false);
            let name = ctx.ir.get_string(rule.name);
            let variant_ident = format_ident!("{}", name);
            let enum_ident = &ctx.enum_ident;
            if elide_box {
                quote! { #inner.map(|__x| #enum_ident::#variant_ident(__x)) }
            } else {
                let helper = ctx.arena_helper_ident();
                quote! {
                    #inner.map(|__x| {
                        let __alloc = #helper(state).alloc(#enum_ident::#variant_ident(__x));
                        &*__alloc
                    })
                }
            }
        };

        ctx.no_collapse.set(saved_no_collapse);
        return result;
    }

    // Standard path: emit function call.
    let fn_ident = mono_fn_ident(ctx.resolve_rule_name(rule_id));

    if elide_box {
        // Direct call → ArenaEnum<'a>
        quote! { Self::#fn_ident(state) }
    } else {
        // Call + arena.alloc → Option<&'a ArenaEnum<'a>>
        let helper = ctx.arena_helper_ident();
        quote! {
            Self::#fn_ident(state).map(|__v| {
                let __alloc = #helper(state).alloc(__v);
                &*__alloc
            })
        }
    }
}

// ── Skip / Next ──────────────────────────────────────────────────────────────

/// Emit monolithic Skip: parse left (keep), parse right (discard).
pub(super) fn emit_mono_skip(
    left: &IrNode,
    right: &IrNode,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    elide_box: bool,
) -> TokenStream {
    // Wrap detection: Skip(Next(open, middle), close).
    if let IrNode::Next(open, middle) = left {
        return emit_mono_wrap(open.as_ref(), middle.as_ref(), right, ctx, mctx, elide_box);
    }
    let left_expr = emit_mono_expr(left, ctx, mctx, elide_box);
    let right_expr = emit_mono_discarded(right, false, ctx, mctx);
    let left_var = mctx.fresh("skip");
    quote! {
        {
            let #left_var = #left_expr?;
            #right_expr?;
            Some(#left_var)
        }
    }
}

/// Emit monolithic Next: parse left (discard), parse right (keep).
pub(super) fn emit_mono_next(
    left: &IrNode,
    right: &IrNode,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    elide_box: bool,
) -> TokenStream {
    // Wrap detection: Next(open, Skip(middle, close)).
    if let IrNode::Skip(middle, close) = right {
        return emit_mono_wrap(left, middle.as_ref(), close.as_ref(), ctx, mctx, elide_box);
    }
    let left_expr = emit_mono_discarded(left, false, ctx, mctx);
    let right_expr = emit_mono_expr(right, ctx, mctx, elide_box);
    quote! {
        {
            #left_expr?;
            #right_expr
        }
    }
}

// ── Wrap ─────────────────────────────────────────────────────────────────────

/// Emit a monolithic wrap pattern: `open >> middle << close`.
pub(super) fn emit_mono_wrap(
    open: &IrNode,
    middle: &IrNode,
    close: &IrNode,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    elide_box: bool,
) -> TokenStream {
    // sep_by_ws_until optimization: close is Literal, middle is OW(Repeat(sep_by)).
    if let IrNode::Literal(close_sid) = close {
        if let IrNode::OptionalWhitespace(ow_inner) = middle {
            if let IrNode::Repeat {
                inner: rep_inner,
                lo,
                hi,
            } = ow_inner.as_ref()
            {
                if !(*lo == 0 && *hi == 1) {
                    if let Some((element, separator)) = combinator_repeat::try_sep_by(rep_inner) {
                        let close_lit = ctx.ir.get_string(*close_sid);
                        let close_unescaped = unescape_literal(close_lit);
                        let close_bytes: Vec<u8> = close_unescaped.bytes().collect();

                        let open_expr = emit_mono_discarded(open, false, ctx, mctx);
                        let close_expr = emit_mono_discarded(close, false, ctx, mctx);
                        let unchecked = try_unchecked_sep(separator, ctx);

                        return emit_mono_sep_by_core(
                            element,
                            separator,
                            *lo,
                            &SepByConfig {
                                ws: true,
                                open_expr: Some(open_expr),
                                close_expr: Some(close_expr),
                                terminator_bytes: Some(close_bytes),
                                unchecked_sep: unchecked,
                            },
                            ctx,
                            mctx,
                        );
                    }
                }
            }
        }
    }

    // General wrap: open >> middle << close.
    let open_expr = emit_mono_discarded(open, false, ctx, mctx);
    let middle_expr = emit_mono_expr(middle, ctx, mctx, elide_box);
    let close_expr = emit_mono_discarded(close, false, ctx, mctx);
    let mid_var = mctx.fresh("mid");

    quote! {
        {
            #open_expr?;
            let #mid_var = #middle_expr?;
            #close_expr?;
            Some(#mid_var)
        }
    }
}

// ── Map ──────────────────────────────────────────────────────────────────────

/// Emit a monolithic Map expression.
pub(super) fn emit_mono_map(
    inner: &IrNode,
    fn_id: u32,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    elide_box: bool,
) -> TokenStream {
    // Map fusion: Map { Map { inner2, fn2 }, fn1 }.
    if let IrNode::Map {
        inner: inner2,
        fn_id: fn_id2,
    } = inner
    {
        let inner_fd = &ctx.ir.fns[*fn_id2 as usize];
        let outer_fd = &ctx.ir.fns[fn_id as usize];
        match (inner_fd, outer_fd) {
            (FnDescriptor::EnumWrap { variant }, FnDescriptor::BoxWrap) => {
                let inner_expr = emit_mono_expr(inner2.as_ref(), ctx, mctx, elide_box);
                let vname = ctx.ir.get_string(*variant);
                let vident = format_ident!("{}", vname);
                let enum_ident = &ctx.enum_ident;
                if elide_box {
                    return quote! { #inner_expr.map(|__x| #enum_ident::#vident(__x)) };
                } else {
                    let helper = ctx.arena_helper_ident();
                    return quote! {
                        #inner_expr.map(|__x| {
                            let __alloc = #helper(state).alloc(#enum_ident::#vident(__x));
                            &*__alloc
                        })
                    };
                }
            }
            (FnDescriptor::BoxWrap, FnDescriptor::EnumWrap { variant }) => {
                let inner_expr = emit_mono_expr(inner2.as_ref(), ctx, mctx, elide_box);
                let vname = ctx.ir.get_string(*variant);
                let vident = format_ident!("{}", vname);
                let enum_ident = &ctx.enum_ident;
                let helper = ctx.arena_helper_ident();
                return quote! {
                    #inner_expr.map(|__x| {
                        #enum_ident::#vident(&*#helper(state).alloc(__x))
                    })
                };
            }
            _ => {}
        }
    }

    let inner_expr = emit_mono_expr(inner, ctx, mctx, elide_box);
    let fd = &ctx.ir.fns[fn_id as usize];
    match fd {
        FnDescriptor::EnumWrap { variant } => {
            let vname = ctx.ir.get_string(*variant);
            let vident = format_ident!("{}", vname);
            let enum_ident = &ctx.enum_ident;
            quote! { #inner_expr.map(|__x| #enum_ident::#vident(__x)) }
        }
        FnDescriptor::BoxWrap => {
            if elide_box {
                inner_expr
            } else {
                let helper = ctx.arena_helper_ident();
                quote! {
                    #inner_expr.map(|__x| {
                        let __alloc = #helper(state).alloc(__x);
                        &*__alloc
                    })
                }
            }
        }
        FnDescriptor::Custom { source, .. } => {
            let closure_src = ctx.ir.get_string(*source);
            let closure: syn::ExprClosure = syn::parse_str(closure_src).unwrap_or_else(|e| {
                panic!(
                    "Invalid mapping closure `{}` in monolithic codegen: {}",
                    closure_src, e
                )
            });
            quote! { #inner_expr.map(#closure) }
        }
    }
}

// ── OptionalWhitespace ───────────────────────────────────────────────────────

/// Emit monolithic OptionalWhitespace.
pub(super) fn emit_mono_ow(
    inner: &IrNode,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    elide_box: bool,
) -> TokenStream {
    // sep_by_ws detection.
    if let IrNode::Repeat {
        inner: rep_inner,
        lo,
        hi,
    } = inner
    {
        if !(*lo == 0 && *hi == 1) {
            if let Some((element, separator)) = combinator_repeat::try_sep_by(rep_inner) {
                return emit_mono_sep_by_ws(element, separator, *lo, ctx, mctx);
            }
        }
    }

    // Inline whitespace trimming (uses custom @ws pattern if configured).
    let ws_trim = super::emit_ws_trim(ctx, mctx);
    let inner_expr = emit_mono_expr(inner, ctx, mctx, elide_box);
    let result_var = mctx.fresh("ow");
    let ws2 = ws_trim.clone();
    quote! {
        {
            #ws_trim
            let #result_var = #inner_expr;
            if #result_var.is_some() {
                #ws2
            }
            #result_var
        }
    }
}
