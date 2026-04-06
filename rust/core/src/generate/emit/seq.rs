//! Seq emit codegen with TypeMap-driven child classification.
//!
//! Uses `ctx.seq_child_types()` to determine which children are structural
//! (Span-typed literals, whitespace) vs value-producing. Value children are
//! destructured from the tuple; structural children emit from the IR.

use bbnf_ir::{GrammarIR, IrNode, TypeDesc};
use proc_macro2::TokenStream;
use quote::quote;
use syn::Index;

use crate::generate::ir_types::IrCodegenCtx;

/// Generate emit code for a Seq node.
pub fn emit_seq(
    children: &[IrNode],
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    // Get per-child types from the TypeMap.
    let child_types = ctx.seq_child_types(children)
        .unwrap_or_else(|| {
            children.iter().map(|c| ctx.node_type(c)).collect()
        });

    // Identify value-producing children (non-structural).
    // A child is structural if it's a Literal, Epsilon, or OptionalWhitespace wrapping those.
    let value_indices: Vec<usize> = children.iter()
        .enumerate()
        .filter(|(_, child)| is_value_producing(child))
        .map(|(i, _)| i)
        .collect();

    let value_count = value_indices.len();

    // Generate emission for each child in order.
    let mut parts = Vec::with_capacity(children.len());

    for (i, child) in children.iter().enumerate() {
        if !is_value_producing(child) {
            // Structural: emit from IR directly.
            parts.push(super::node::emit_node(child, val, ir, ctx));
        } else {
            // Value-producing: extract from destructured value.
            let value_pos = value_indices.iter().position(|&vi| vi == i).unwrap();
            let child_val = if value_count == 1 {
                // Single value child: the whole value IS this child.
                val.clone()
            } else {
                // Multiple value children: tuple destructuring.
                // Use & to get a reference to the tuple element.
                let idx = Index::from(value_pos);
                quote! { &(#val.#idx) }
            };
            parts.push(super::node::emit_node(child, &child_val, ir, ctx));
        }
    }

    quote! { #(#parts)* }
}

/// Is this IR node value-producing (contributes to the Seq's result type)?
///
/// Structural nodes (Literal, Epsilon, OptionalWhitespace wrapping structural)
/// are consumed during parsing but don't produce values in the result tuple.
fn is_value_producing(node: &IrNode) -> bool {
    match node {
        IrNode::Literal(_) | IrNode::Epsilon => false,
        IrNode::OptionalWhitespace(inner) => is_value_producing(inner),
        IrNode::Negate(_) => false,
        _ => true,
    }
}
