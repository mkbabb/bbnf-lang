//! Repeat and sep_by emission.
//!
//! Handles repeat quantifiers (optional, many, many1), sep_by pattern detection,
//! and discarded-separator optimization (skip enum/box wrapping for separators
//! whose values are thrown away).

use bbnf_ir::{FnDescriptor, IrNode, TypeDesc};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::ir_types::IrCodegenCtx;
use super::infer::{infer_node_type, infer_node_type_in_vec};
use super::{ir_node_to_tokens, ir_node_to_tokens_vec};

/// Emit a Repeat expression.
///
/// `in_vec` indicates the Repeat's result will be stored in a Vec (from a parent
/// context). For Optional, this means the Option inner can also skip boxing.
/// For Many/Many1, `in_vec=true` is always set regardless of the parameter
/// since Vec provides heap indirection.
pub fn emit_repeat(inner: &IrNode, lo: u32, hi: u32, ctx: &IrCodegenCtx<'_>, in_vec: bool) -> TokenStream {
    // sep_by detection: Repeat { inner: Skip(element, Repeat { separator, 0, 1 }) }
    // Only for non-optional repeats (not lo=0, hi=1 which is just Optional).
    if !(lo == 0 && hi == 1) {
        if let Some((element, separator)) = try_sep_by(inner) {
            let sep_ts = emit_discarded_separator(separator, false, ctx);
            let elem_ty = infer_node_type_in_vec(element, ctx);
            let sep_is_span = separator_is_span(separator, ctx);
            let both_span = elem_ty == TypeDesc::Span && sep_is_span;

            // Vec-producing: pass in_vec=true for element emission.
            let elem_ts = ir_node_to_tokens_vec(element, ctx, true);

            let lo_usize = lo as usize;
            return if both_span {
                quote! { #elem_ts.sep_by_span(#sep_ts, #lo_usize..) }
            } else {
                quote! { #elem_ts.sep_by(#sep_ts, #lo_usize..) }
            };
        }
    }

    if lo == 0 && hi == 1 {
        // Optional: NOT a Vec context — use standard (boxed) emission.
        let inner_ty = infer_node_type(inner, ctx);
        let is_span = inner_ty == TypeDesc::Span;

        // Phase 1a: transparent refs use _unboxed() to skip Box allocation.
        // Option<Enum> instead of Option<Box<Enum>>.
        if let IrNode::Ref(rule_id) = inner {
            let rule = &ctx.ir.rules[*rule_id as usize];
            if rule.meta.is_transparent {
                let resolved_name = ctx.resolve_rule_name(*rule_id);
                let unboxed_ident = format_ident!("{}_unboxed", resolved_name);
                return quote! { Self::#unboxed_ident().opt() };
            }
        }

        let inner_ts = ir_node_to_tokens(inner, ctx);
        if is_span {
            quote! { #inner_ts.opt_span() }
        } else {
            quote! { #inner_ts.opt() }
        }
    } else {
        // Vec-producing: pass in_vec=true to skip Box wrapping.
        let inner_ty = infer_node_type_in_vec(inner, ctx);
        let is_span = inner_ty == TypeDesc::Span;
        let inner_ts = ir_node_to_tokens_vec(inner, ctx, true);
        if lo == 0 {
            if is_span {
                quote! { #inner_ts.many_span(..) }
            } else {
                quote! { #inner_ts.many(..) }
            }
        } else if lo == 1 {
            if is_span {
                quote! { #inner_ts.many_span(1..) }
            } else {
                quote! { #inner_ts.many(1..) }
            }
        } else {
            let lo = lo as usize;
            quote! { #inner_ts.many(#lo..) }
        }
    }
}

/// Emit sep_by_ws from OptionalWhitespace(Repeat { sep_by pattern }).
///
/// Called from the OptionalWhitespace handler in `ir_node_to_tokens`.
pub fn emit_sep_by_ws(
    element: &IrNode,
    separator: &IrNode,
    lo: u32,
    ctx: &IrCodegenCtx<'_>,
) -> TokenStream {
    // Fix 5+6: emit separator without discarded overhead.
    // sep_by discards separator value → skip enum/box wrapping.
    // sep_by_ws handles whitespace → skip OW trimming.
    // Vec-producing: pass in_vec=true for element emission.
    let elem_ts = ir_node_to_tokens_vec(element, ctx, true);
    let sep_ts = emit_discarded_separator(separator, true, ctx);

    // After stripping, check if separator is effectively Span
    // (e.g. a Ref with _sp method) for sep_by_ws_span upgrade.
    let elem_ty = infer_node_type_in_vec(element, ctx);
    let sep_is_span = separator_is_span(separator, ctx);
    let both_span = elem_ty == TypeDesc::Span && sep_is_span;

    let lo_usize = lo as usize;
    if both_span {
        quote! { #elem_ts.sep_by_ws_span(#sep_ts, #lo_usize..) }
    } else {
        quote! { #elem_ts.sep_by_ws(#sep_ts, #lo_usize..) }
    }
}

/// Try to detect a `sep_by` pattern inside a Repeat node.
///
/// Pattern: `Repeat { inner: Skip(element, Repeat { inner: separator, lo: 0, hi: 1 }), .. }`
/// Represents `(element << separator?)*` → `element.sep_by(separator, lo..)`
///
/// Returns `(element, separator)` nodes if the pattern matches.
pub fn try_sep_by(inner: &IrNode) -> Option<(&IrNode, &IrNode)> {
    if let IrNode::Skip(element, opt_sep) = inner {
        if let IrNode::Repeat {
            inner: separator,
            lo: 0,
            hi: 1,
        } = opt_sep.as_ref()
        {
            return Some((element.as_ref(), separator.as_ref()));
        }
    }
    None
}

/// Emit a parser expression for a separator whose value will be discarded by sep_by.
///
/// Since sep_by throws away the separator result (Output2), we can skip:
/// - Enum wrapping (EnumWrap maps)
/// - Boxing (BoxWrap maps, emit_ref boxing)
/// - Whitespace trimming redundant with sep_by_ws context
///
/// For Ref nodes, uses `_sp().into_parser()` when available (cheapest path),
/// otherwise emits `Self::rule()` without the usual `.map(|x| Box::new(x))` boxing.
fn emit_discarded_separator(
    node: &IrNode,
    strip_ow: bool,
    ctx: &IrCodegenCtx<'_>,
) -> TokenStream {
    match node {
        // Strip Map wrappers (EnumWrap, BoxWrap) — their output is discarded.
        IrNode::Map { inner, fn_id } => {
            let fd = &ctx.ir.fns[*fn_id as usize];
            match fd {
                FnDescriptor::EnumWrap { .. } | FnDescriptor::BoxWrap => {
                    emit_discarded_separator(inner, strip_ow, ctx)
                }
                _ => ir_node_to_tokens(node, ctx), // Custom maps may have side effects.
            }
        }
        // Strip OptionalWhitespace in sep_by_ws context (sep_by_ws handles WS).
        IrNode::OptionalWhitespace(inner) if strip_ow => {
            emit_discarded_separator(inner, strip_ow, ctx)
        }
        // For Ref nodes, skip boxing and prefer _sp path.
        IrNode::Ref(rule_id) => {
            let rule = &ctx.ir.rules[*rule_id as usize];
            let name = ctx.ir.get_string(rule.name);
            if ctx.has_sp_method(name) {
                // Use SpanParser path — cheapest, no enum/box overhead.
                let sp_ident = format_ident!("{}_sp", name);
                quote! { Self::#sp_ident().into_parser() }
            } else if rule.meta.is_transparent {
                // Phase 1b: transparent refs use _unboxed() — value is discarded
                // anyway, so skip the internal boxing that transparent rules do.
                let resolved_name = ctx.resolve_rule_name(*rule_id);
                let unboxed_ident = format_ident!("{}_unboxed", resolved_name);
                quote! { Self::#unboxed_ident() }
            } else {
                // Emit Self::rule() WITHOUT the .map(|x| Box::new(x)) boxing
                // that emit_ref normally adds. The value is discarded anyway.
                let resolved_name = ctx.resolve_rule_name(*rule_id);
                let ident = format_ident!("{}", resolved_name);
                quote! { Self::#ident() }
            }
        }
        // Everything else: fall through to normal emission.
        _ => ir_node_to_tokens(node, ctx),
    }
}

/// Check if a separator node would produce Span after discarding Map/OW wrappers.
/// Used to determine sep_by_span vs sep_by upgrade.
fn separator_is_span(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> bool {
    match node {
        IrNode::Map { inner, fn_id } => {
            let fd = &ctx.ir.fns[*fn_id as usize];
            match fd {
                FnDescriptor::EnumWrap { .. } | FnDescriptor::BoxWrap => {
                    separator_is_span(inner, ctx)
                }
                _ => infer_node_type(node, ctx) == TypeDesc::Span,
            }
        }
        IrNode::OptionalWhitespace(inner) => separator_is_span(inner, ctx),
        IrNode::Ref(rule_id) => {
            let rule = &ctx.ir.rules[*rule_id as usize];
            let name = ctx.ir.get_string(rule.name);
            // If _sp method exists, the discarded separator emits into_parser() → Span.
            ctx.has_sp_method(name)
        }
        _ => infer_node_type(node, ctx) == TypeDesc::Span,
    }
}
