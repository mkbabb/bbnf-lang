//! Prettify Seq emission — direct concatenation of children.

use bbnf_ir::{GrammarIR, IrNode};

use proc_macro2::TokenStream;
use quote::quote;

use super::super::super::super::ir_types::IrCodegenCtx;
use super::super::MonoCtx;
use super::emit_prettify_expr;

/// Emit a Seq for prettify: parse children in order, emit FmtOps for each.
pub(super) fn emit_prettify_seq(
    children: &[IrNode],
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    if children.is_empty() {
        return quote! {};
    }
    if children.len() == 1 {
        return emit_prettify_expr(&children[0], ir, ctx, mctx);
    }

    let mut stmts: Vec<TokenStream> = Vec::new();
    for child in children {
        let expr = emit_prettify_expr(child, ir, ctx, mctx);
        stmts.push(expr);
    }

    quote! { { #(#stmts;)* } }
}
