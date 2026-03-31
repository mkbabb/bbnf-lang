//! Monolithic expression helpers: Ref, Skip/Next, Wrap, Map, OptionalWhitespace.

use bbnf_ir::{FnDescriptor, IrNode};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::ir_types::IrCodegenCtx;
use super::unescape_literal;
use super::helpers::try_sep_by;
use super::repeat::{emit_mono_sep_by_ws, emit_mono_sep_by_core, try_unchecked_sep, SepByConfig};
use super::{emit_mono_discarded, emit_mono_expr, mono_fn_ident, MonoCtx};

// ── Ref ──────────────────────────────────────────────────────────────────────

/// Emit a monolithic Ref — direct function call, or inline body for fusion-eligible rules.
///
/// All internal fns return `Option<ArenaEnum<'a>>`.
/// - `elide_box = true`:  return ArenaEnum directly
/// - `elide_box = false`: arena.alloc → `&'a ArenaEnum<'a>`
///
/// **Fusion**: When the target rule is fusion-eligible (non-cyclic, no @recover/@pretty),
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

        let result = if rule.meta.is_transparent {
            // Transparent: body emitted with elide_box=true, returns inner type.
            let body = emit_mono_expr(&rule.body, ctx, mctx, true);
            if elide_box {
                body
            } else {
                let alloc_code = ctx.emit_box_alloc_let(&quote! { __v });
                quote! {
                    #body.map(|__v| {
                        #alloc_code
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
                let alloc_code = ctx.emit_box_alloc_let(&quote! { #enum_ident::#variant_ident(__x) });
                quote! {
                    #inner.map(|__x| {
                        #alloc_code
                    })
                }
            }
        };

        return result;
    }

    // Standard path: emit function call.
    let fn_ident = mono_fn_ident(ctx.resolve_rule_name(rule_id));

    if elide_box {
        // Direct call → ArenaEnum<'a>
        quote! { Self::#fn_ident(state) }
    } else {
        // Call + alloc → Option<&'a ArenaEnum<'a>> / Option<Box<Enum>>
        let alloc_code = ctx.emit_box_alloc_let(&quote! { __v });
        quote! {
            Self::#fn_ident(state).map(|__v| {
                #alloc_code
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
                    if let Some((element, separator)) = try_sep_by(rep_inner) {
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

    // Delimiter-scan optimization for arena path.
    if let Some(ts) = super::delim_scan::try_emit_arena_wrap(open, middle, close, ctx.ir, ctx, mctx) {
        return ts;
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
                    let alloc_code = ctx.emit_box_alloc_let(&quote! { #enum_ident::#vident(__x) });
                    return quote! {
                        #inner_expr.map(|__x| {
                            #alloc_code
                        })
                    };
                }
            }
            (FnDescriptor::BoxWrap, FnDescriptor::EnumWrap { variant }) => {
                let inner_expr = emit_mono_expr(inner2.as_ref(), ctx, mctx, elide_box);
                let vname = ctx.ir.get_string(*variant);
                let vident = format_ident!("{}", vname);
                let enum_ident = &ctx.enum_ident;
                let alloc_code = ctx.emit_box_alloc(&quote! { __x });
                return quote! {
                    #inner_expr.map(|__x| {
                        #enum_ident::#vident(#alloc_code)
                    })
                };
            }
            (FnDescriptor::NumberConvert, FnDescriptor::EnumWrap { variant }) => {
                let vname = ctx.ir.get_string(*variant);
                let vident = format_ident!("{}", vname);
                let enum_ident = &ctx.enum_ident;
                if elide_box {
                    return quote! {
                        ::parse_that::scan_number_f64(state).map(|__x| #enum_ident::#vident(__x))
                    };
                } else {
                    let alloc_code = ctx.emit_box_alloc_let(&quote! { #enum_ident::#vident(__x) });
                    return quote! {
                        ::parse_that::scan_number_f64(state).map(|__x| {
                            #alloc_code
                        })
                    };
                }
            }
            (FnDescriptor::NumberConvert, FnDescriptor::BoxWrap) => {
                if elide_box {
                    return quote! { ::parse_that::scan_number_f64(state) };
                } else {
                    let alloc_code = ctx.emit_box_alloc_let(&quote! { __x });
                    return quote! {
                        ::parse_that::scan_number_f64(state).map(|__x| {
                            #alloc_code
                        })
                    };
                }
            }
            (FnDescriptor::Constant { value, .. }, FnDescriptor::EnumWrap { variant }) => {
                let val_src = ctx.ir.get_string(*value);
                let val_expr: syn::Expr = syn::parse_str(val_src).unwrap();
                let vident = format_ident!("{}", ctx.ir.get_string(*variant));
                let enum_ident = &ctx.enum_ident;
                let inner_expr = emit_mono_expr(inner2.as_ref(), ctx, mctx, elide_box);
                if elide_box {
                    return quote! { #inner_expr.map(|_| #enum_ident::#vident(#val_expr)) };
                } else {
                    let alloc_code = ctx.emit_box_alloc_let(&quote! { #enum_ident::#vident(#val_expr) });
                    return quote! {
                        #inner_expr.map(|_| {
                            #alloc_code
                        })
                    };
                }
            }
            _ => {}
        }
    }

    let fd = &ctx.ir.fns[fn_id as usize];
    match fd {
        FnDescriptor::NumberConvert => {
            // Strength reduction: direct fused CSS number scanner → f64
            // No regex, no Span, no closure overhead
            quote! { ::parse_that::scan_number_f64(state) }
        }
        FnDescriptor::HexConvert { fn_path } => {
            let inner_expr = emit_mono_expr(inner, ctx, mctx, elide_box);
            let fn_src = ctx.ir.get_string(*fn_path);
            let fn_expr: syn::Expr = syn::parse_str(fn_src).unwrap_or_else(|e| {
                panic!("Invalid HexConvert function `{}`: {}", fn_src, e)
            });
            // Inner produces Span; the user function expects &str.
            quote! { #inner_expr.map(|__s| #fn_expr(__s.as_str())) }
        }
        _ => {
            let inner_expr = emit_mono_expr(inner, ctx, mctx, elide_box);
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
                        let alloc_code = ctx.emit_box_alloc_let(&quote! { __x });
                        quote! {
                            #inner_expr.map(|__x| {
                                #alloc_code
                            })
                        }
                    }
                }
                FnDescriptor::Custom { source, .. } => {
                    let closure_src = ctx.ir.get_string(*source);
                    let closure: syn::ExprClosure =
                        syn::parse_str(closure_src).unwrap_or_else(|e| {
                            panic!(
                                "Invalid mapping closure `{}` in monolithic codegen: {}",
                                closure_src, e
                            )
                        });
                    quote! { #inner_expr.map(#closure) }
                }
                FnDescriptor::Constant { value, .. } => {
                    let val_src = ctx.ir.get_string(*value);
                    let val_expr: syn::Expr = syn::parse_str(val_src).unwrap_or_else(|e| {
                        panic!(
                            "Invalid constant expression `{}` in monolithic codegen: {}",
                            val_src, e
                        )
                    });
                    quote! { #inner_expr.map(|_| #val_expr) }
                }
                FnDescriptor::SpanCapture => {
                    // @{expr}: parse inner for validation, discard result, return Span.
                    quote! {
                        {
                            let __start = state.offset;
                            let __result: Option<()> = (|| { let _ = #inner_expr; Some(()) })();
                            if __result.is_some() {
                                Some(::parse_that::Span::new(__start, state.offset, state.src))
                            } else {
                                None
                            }
                        }
                    }
                }
                // Already handled above
                FnDescriptor::NumberConvert | FnDescriptor::HexConvert { .. } => unreachable!(),
            }
        }
    }
}

// ── OptionalWhitespace ───────────────────────────────────────────────────────

/// Check whether an IrNode structurally ends with OptionalWhitespace.
///
/// When `OW(inner)` wraps an expression whose last action is already a whitespace
/// trim, the post-trim in `emit_mono_ow` is redundant — the inner expression already
/// consumed trailing whitespace. This avoids 1-2 extra `scan_ws_block_comments` (or
/// `trim_leading_whitespace_mut`) calls per iteration in hot loops like
/// `blockContent = ((declaration | ruleItem) ?w) *`.
pub(super) fn ends_with_ow(node: &IrNode) -> bool {
    match node {
        IrNode::OptionalWhitespace(_) => true,
        IrNode::Map { inner, .. } => ends_with_ow(inner),
        IrNode::Alt(branches, _) => branches.iter().all(|b| ends_with_ow(&b.node)),
        IrNode::Seq(children) => children.last().is_some_and(|c| ends_with_ow(c)),
        IrNode::Skip(left, _) => ends_with_ow(left), // Skip keeps left, which might end with OW
        _ => false,
    }
}

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
            if let Some((element, separator)) = try_sep_by(rep_inner) {
                return emit_mono_sep_by_ws(element, separator, *lo, ctx, mctx);
            }
        }
    }

    // Inline whitespace trimming (uses custom @ws pattern if configured).
    let ws_trim = super::emit_ws_trim(ctx, mctx);
    let inner_expr = emit_mono_expr(inner, ctx, mctx, elide_box);

    // Loop invariant hoisting: if the inner expression already ends with a
    // whitespace trim (e.g., inner is itself OW-wrapped, or an Alt where every
    // branch ends with OW), skip the redundant trailing trim.
    if ends_with_ow(inner) {
        quote! {
            {
                #ws_trim
                #inner_expr
            }
        }
    } else {
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
}
