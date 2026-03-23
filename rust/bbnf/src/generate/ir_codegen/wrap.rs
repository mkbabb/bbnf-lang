//! Wrap and regex coalescing emission.
//!
//! Handles the `open >> middle << close` pattern and fuses
//! `Literal + Regex-repeat + Literal` into a single regex.

use bbnf_ir::IrNode;

use proc_macro2::TokenStream;
use quote::quote;

use super::super::ir_types::IrCodegenCtx;
use super::repeat;
use super::{ir_node_to_tokens, ir_node_to_tokens_elide};

/// Emit a `wrap` / `wrap_span` expression for the pattern `open >> middle << close`.
///
/// Also attempts regex coalescing: if the pattern is `Literal + Regex-repeat + Literal`,
/// fuse into a single `sp_regex(combined_pattern)` (Fix 4).
pub fn emit_wrap(
    open: &IrNode,
    middle: &IrNode,
    close: &IrNode,
    ctx: &IrCodegenCtx<'_>,
    elide_box: bool,
) -> TokenStream {
    // Fix 4: Regex coalescing — Literal(open) >> Regex(pattern)* << Literal(close)
    // Fuse into a single regex: sp_regex("open_lit" + pattern + "close_lit").
    if let (IrNode::Literal(open_sid), IrNode::Literal(close_sid)) = (open, close) {
        if let Some(fused) = try_regex_coalesce(
            ctx.ir.get_string(*open_sid),
            middle,
            ctx.ir.get_string(*close_sid),
            ctx,
        ) {
            return fused;
        }
    }

    // Step 5: FOLLOW-set speculative termination.
    // When middle is OW(Repeat(sep_by)) and close is a Literal, emit
    // sep_by_ws_small_until with the close delimiter's bytes as terminator.
    if let IrNode::Literal(close_sid) = close {
        if let IrNode::OptionalWhitespace(ow_inner) = middle {
            if let IrNode::Repeat {
                inner: rep_inner,
                lo,
                hi,
            } = ow_inner.as_ref()
            {
                if !(*lo == 0 && *hi == 1) {
                    if let Some((element, separator)) = repeat::try_sep_by(rep_inner) {
                        let close_lit = ctx.ir.get_string(*close_sid);
                        let close_bytes: Vec<u8> = close_lit.bytes().collect();
                        let open_ts = ir_node_to_tokens(open, ctx);
                        let close_ts = ir_node_to_tokens(close, ctx);
                        let sep_ts = repeat::emit_sep_by_ws_until(
                            element,
                            separator,
                            *lo,
                            &close_bytes,
                            ctx,
                        );
                        return quote! { #sep_ts.wrap(#open_ts, #close_ts) };
                    }
                }
            }
        }
    }

    let open_ts = ir_node_to_tokens(open, ctx);
    let middle_ts = ir_node_to_tokens_elide(middle, ctx, elide_box);
    let close_ts = ir_node_to_tokens(close, ctx);

    // Always use `.wrap()` in the Parser context — `.wrap_span()` converts to
    // SpanParser which changes the output type. The SpanParser context in
    // ir_span.rs handles `wrap_span` separately.
    quote! { #middle_ts.wrap(#open_ts, #close_ts) }
}

/// Try to fuse `"open" >> regex_body << "close"` into a single regex SpanParser.
///
/// Handles the case where `middle` is a `Repeat { inner: Regex(pattern), .. }`.
/// The fused pattern is `open_lit + regex_pattern + close_lit` wrapped in a single
/// `sp_regex(...)` call.
fn try_regex_coalesce(
    open_lit: &str,
    middle: &IrNode,
    close_lit: &str,
    _ctx: &IrCodegenCtx<'_>,
) -> Option<TokenStream> {
    // Match Repeat { inner: Regex(pattern), lo, hi }.
    let (pattern, lo, hi) = match middle {
        IrNode::Repeat { inner, lo, hi } => {
            if let IrNode::Regex(sid) = inner.as_ref() {
                (_ctx.ir.get_string(*sid), *lo, *hi)
            } else {
                return None;
            }
        }
        IrNode::Regex(sid) => {
            // Bare regex (not repeated) — still coalesceable.
            (_ctx.ir.get_string(*sid), 1, 1)
        }
        _ => return None,
    };

    let open_escaped = regex_escape_literal(open_lit);
    let close_escaped = regex_escape_literal(close_lit);

    let quantifier = match (lo, hi) {
        (0, 1) => "?",
        (0, _) => "*",
        (1, _) => "+",
        _ => return None, // Non-standard quantifier — don't coalesce.
    };

    // Wrap the inner pattern in a non-capturing group if it contains alternation.
    let inner_pattern = if pattern.contains('|') && !pattern.starts_with("(?:") {
        format!("(?:{}){}", pattern, quantifier)
    } else {
        format!("{}{}", pattern, quantifier)
    };

    // For the special case of exactly-once (lo=1, hi=1), don't add a quantifier.
    let inner_pattern = if lo == 1 && hi == 1 {
        pattern.to_string()
    } else {
        inner_pattern
    };

    let fused = format!("{}{}{}", open_escaped, inner_pattern, close_escaped);
    Some(quote! { ::parse_that::sp_regex(#fused).into_parser() })
}

/// Escape a literal string for use in a regex pattern.
fn regex_escape_literal(s: &str) -> String {
    let mut result = String::with_capacity(s.len() * 2);
    for c in s.chars() {
        if r"\.+*?()[]{}|^$".contains(c) {
            result.push('\\');
        }
        result.push(c);
    }
    result
}
