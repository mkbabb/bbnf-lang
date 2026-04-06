//! Repetition emit codegen.
//!
//! Generates iteration over Vec/Option values with separator emission.

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::quote;

use crate::generate::ir_types::IrCodegenCtx;

/// Generate emit code for a Repeat node.
pub fn emit_repeat(
    inner: &IrNode,
    lo: u32,
    hi: u32,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
    rule: &IrRule,
) -> TokenStream {
    let inner_emit = super::expr::emit_node(inner, ir, ctx, rule);

    if lo == 0 && hi == 1 {
        // Optional: if Some, emit.
        quote! {
            if let Some(value) = value {
                #inner_emit
            }
        }
    } else {
        // Vec repetition: iterate and emit.
        // TODO: detect separator from the IR (e.g., Seq with literal separator between items).
        // For now, emit each item directly.
        quote! {
            for value in value.iter() {
                #inner_emit
            }
        }
    }
}
