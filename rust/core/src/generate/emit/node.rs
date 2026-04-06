//! Main per-node emit dispatch.
//!
//! Each `IrNode` variant is handled according to its role:
//! - **Structural** nodes (Literal, Epsilon, OW, Negate) emit content from the IR.
//! - **Value** nodes (Regex, Ref, Map) emit from the typed `value` binding.
//! - **Composite** nodes (Seq, Alt, Repeat, Skip, Next) decompose both.

use bbnf_ir::{GrammarIR, IrNode};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::generate::ir_types::IrCodegenCtx;

/// Generate emission code for an IR node.
///
/// `val`: TokenStream expression for the current value binding (e.g., `__v`, `__v.0`).
/// The generated code uses `__sink` as the EmitSink binding (in scope from the enclosing fn).
pub fn emit_node(
    node: &IrNode,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    match node {
        // ── Structural (emit from IR, no value consumed) ─────────────

        IrNode::Literal(sid) => {
            let s = ir.get_string(*sid);
            if s.len() == 1 {
                let byte = s.as_bytes()[0];
                quote! { __sink.char(#byte); }
            } else {
                quote! { __sink.text(#s); }
            }
        }

        IrNode::Epsilon => quote! {},

        IrNode::Negate(_) => quote! {},

        IrNode::OptionalWhitespace(inner) => {
            // In compact mode, whitespace is suppressed (EmitSink no-ops).
            // In pretty mode, @pretty hints produce group/softline calls.
            // For now: just emit the inner node (ws is structural, not valued).
            emit_node(inner, val, ir, ctx)
        }

        // ── Value leaves ─────────────────────────────────────────────

        IrNode::Regex(_) => {
            // Regex always produces Span. Emit its text content.
            quote! { __sink.text(#val.as_str()); }
        }

        IrNode::Ref(rule_id) => {
            let ref_rule = &ir.rules[*rule_id as usize];
            if ref_rule.meta.is_transparent {
                // Transparent rules are inlined — recurse into their body.
                emit_node(&ref_rule.body, val, ir, ctx)
            } else {
                let ref_name = ir.get_string(ref_rule.name);
                let emit_fn = format_ident!("{}_emit", ref_name);
                quote! { Self::#emit_fn(#val, __sink); }
            }
        }

        // ── Composite ────────────────────────────────────────────────

        IrNode::Seq(children) => {
            super::seq::emit_seq(children, val, ir, ctx)
        }

        IrNode::Alt(branches, _dispatch) => {
            super::alt::emit_alt(branches, val, ir, ctx)
        }

        IrNode::Repeat { inner, lo, hi } => {
            super::repeat::emit_repeat(inner, *lo, *hi, val, ir, ctx)
        }

        // ── Binary (kept vs structural) ──────────────────────────────

        IrNode::Skip(kept, structural) => {
            // Skip(left, right): parse both, keep LEFT. Value is left's type.
            // Emit: value (left), then structural content (right).
            let kept_emit = emit_node(kept, val, ir, ctx);
            let structural_emit = emit_structural(structural, ir);
            quote! { #kept_emit #structural_emit }
        }

        IrNode::Next(structural, kept) => {
            // Next(left, right): parse both, keep RIGHT. Value is right's type.
            // Emit: structural content (left), then value (right).
            let structural_emit = emit_structural(structural, ir);
            let kept_emit = emit_node(kept, val, ir, ctx);
            quote! { #structural_emit #kept_emit }
        }

        IrNode::Minus(lhs, _rhs) => {
            // Set difference: value IS the lhs match.
            emit_node(lhs, val, ir, ctx)
        }

        // ── Map (reverse the transformation) ─────────────────────────

        IrNode::Map { inner, fn_id } => {
            super::map::emit_map(inner, *fn_id, val, ir, ctx)
        }

        // ── TokenDispatch ────────────────────────────────────────────

        IrNode::TokenDispatch { .. } => {
            // TokenDispatch is a parse-time optimization for identifier-led alts.
            // The parsed value's type depends on which arm matched. For emit,
            // the value is already typed — delegate to the enclosing Alt's match.
            // As a leaf fallback, emit the value as text (assumes Span).
            quote! { __sink.text(#val.as_str()); }
        }
    }
}

/// Emit structural content from an IR node, ignoring values entirely.
///
/// Used for the discarded side of Skip/Next — these are delimiters,
/// whitespace, or other structural content that the parser consumed
/// but didn't keep. We reproduce them from the IR.
fn emit_structural(node: &IrNode, ir: &GrammarIR) -> TokenStream {
    match node {
        IrNode::Literal(sid) => {
            let s = ir.get_string(*sid);
            if s.len() == 1 {
                let byte = s.as_bytes()[0];
                quote! { __sink.char(#byte); }
            } else {
                quote! { __sink.text(#s); }
            }
        }
        IrNode::Epsilon | IrNode::Negate(_) => quote! {},
        IrNode::OptionalWhitespace(inner) => emit_structural(inner, ir),
        IrNode::Seq(children) => {
            let parts: Vec<_> = children.iter().map(|c| emit_structural(c, ir)).collect();
            quote! { #(#parts)* }
        }
        IrNode::Repeat { inner, .. } => emit_structural(inner, ir),
        // For structural nodes that are unexpectedly value-producing,
        // emit nothing rather than generate broken code.
        _ => quote! {},
    }
}
