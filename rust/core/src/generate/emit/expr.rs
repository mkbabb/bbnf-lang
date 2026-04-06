//! Per-expression emit codegen.
//!
//! Each `IrNode` variant produces `TokenStream` that calls `EmitSink` methods
//! to write the corresponding output.

use bbnf_ir::{FnDescriptor, GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::generate::ir_types::IrCodegenCtx;

/// Generate emission code for an IR node.
///
/// `value_expr` is implicit — the generated code assumes a binding named `value`
/// (or destructured fields) is in scope from the enclosing match/let.
pub fn emit_node(
    node: &IrNode,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
    rule: &IrRule,
) -> TokenStream {
    match node {
        IrNode::Literal(sid) => {
            let s = ir.get_string(*sid);
            if s.len() == 1 {
                let byte = s.as_bytes()[0];
                quote! { sink.char(#byte); }
            } else {
                quote! { sink.text(#s); }
            }
        }

        IrNode::Regex(_sid) => {
            // Regex leaves produce Span values. Emit the span text.
            quote! { sink.text(value.as_str()); }
        }

        IrNode::Epsilon => {
            quote! {}
        }

        IrNode::Seq(children) => {
            let child_emits: Vec<TokenStream> = children
                .iter()
                .enumerate()
                .filter_map(|(i, child)| {
                    emit_seq_child(child, i, children.len(), ir, ctx, rule)
                })
                .collect();
            quote! { #(#child_emits)* }
        }

        IrNode::Alt(branches, _dispatch) => {
            super::alt::emit_alt(branches, ir, ctx, rule)
        }

        IrNode::Repeat { inner, lo, hi } => {
            super::repeat::emit_repeat(inner, *lo, *hi, ir, ctx, rule)
        }

        IrNode::Ref(rule_id) => {
            let ref_rule = &ir.rules[*rule_id as usize];
            let ref_name = ir.get_string(ref_rule.name);
            let emit_fn = format_ident!("{}_emit", ref_name);
            let parser_ident = &ctx.ident;
            quote! {
                #parser_ident::#emit_fn(value, sink);
            }
        }

        IrNode::Skip(left, right) => {
            // Skip: parse both, keep left. For emit: emit left (the value we kept).
            // The right side was discarded during parsing, but for round-trip fidelity
            // we need to emit it too (e.g., whitespace, delimiters).
            let left_emit = emit_node(left, ir, ctx, rule);
            let right_emit = emit_node(right, ir, ctx, rule);
            quote! {
                #left_emit
                #right_emit
            }
        }

        IrNode::Next(left, right) => {
            // Next: parse both, keep right. Emit left for side-effects, then right as value.
            let left_emit = emit_node(left, ir, ctx, rule);
            let right_emit = emit_node(right, ir, ctx, rule);
            quote! {
                #left_emit
                #right_emit
            }
        }

        IrNode::Minus(lhs, _rhs) => {
            // Minus: set difference. The value IS the lhs match. Just emit it.
            emit_node(lhs, ir, ctx, rule)
        }

        IrNode::Negate(_inner) => {
            // Negative lookahead: consumes nothing, produces nothing.
            quote! {}
        }

        IrNode::Map { inner, fn_id } => {
            emit_map(inner, *fn_id, ir, ctx, rule)
        }

        IrNode::OptionalWhitespace(inner) => {
            // Whitespace is formatting — for compact emit, skip it.
            // For pretty emit, the @pretty hints handle it via EmitSink methods.
            emit_node(inner, ir, ctx, rule)
        }

        IrNode::TokenDispatch { token, arms, fallback } => {
            // Token dispatch is a parsing optimization.
            // For emit, we just emit the value directly (it's already matched).
            quote! { sink.text(value.as_str()); }
        }
    }
}

/// Emit a single child of a Seq node.
fn emit_seq_child(
    child: &IrNode,
    index: usize,
    total: usize,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
    rule: &IrRule,
) -> Option<TokenStream> {
    match child {
        // Literals in sequences are structural — emit them directly.
        IrNode::Literal(sid) => {
            let s = ir.get_string(*sid);
            if s.len() == 1 {
                let byte = s.as_bytes()[0];
                Some(quote! { sink.char(#byte); })
            } else {
                Some(quote! { sink.text(#s); })
            }
        }

        // Epsilon produces nothing.
        IrNode::Epsilon => None,

        // OptionalWhitespace in sequences: emit a space for compact readability,
        // or delegate to sink for formatted output.
        IrNode::OptionalWhitespace(_) => {
            // Compact: no whitespace. Pretty: handled by @pretty hints.
            None
        }

        // Other children: delegate.
        _ => Some(emit_node(child, ir, ctx, rule)),
    }
}

/// Emit code for a Map node (value conversion with reverse FnDescriptor).
fn emit_map(
    inner: &IrNode,
    fn_id: u32,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
    rule: &IrRule,
) -> TokenStream {
    let fn_desc = &ir.fns[fn_id as usize];

    match fn_desc {
        FnDescriptor::NumberConvert => {
            // Reverse: f64 → text via ryu.
            quote! { sink.f64(*value); }
        }

        FnDescriptor::HexConvert { .. } => {
            // Reverse: emit hex representation.
            quote! {
                {
                    use std::fmt::Write as _;
                    let mut buf = String::new();
                    write!(buf, "{:x}", value).unwrap();
                    sink.text(&buf);
                }
            }
        }

        FnDescriptor::Expr { expr, .. } => {
            // Generic map expression — can't automatically reverse.
            // Emit the inner value and hope the type is Display-able.
            // TODO: support `emit_with` attribute for explicit reverse functions.
            emit_node(inner, ir, ctx, rule)
        }

        _ => {
            // Fallback: emit inner.
            emit_node(inner, ir, ctx, rule)
        }
    }
}
