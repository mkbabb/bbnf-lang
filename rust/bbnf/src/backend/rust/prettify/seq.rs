//! Prettify Seq emission - direct concatenation of children.

use bbnf_ir::IrNode;

use proc_macro2::TokenStream;
use quote::quote;

use super::super::MonoCtx;
use super::emit_prettify_expr;
use super::policy::PrettifyCtx;

/// Emit a Seq for prettify: parse children in order, emit FmtOps for each.
pub(super) fn emit_prettify_seq(
    children: &[IrNode],
    pctx: &PrettifyCtx<'_>,
    current_rule: bbnf_ir::RuleId,
    mctx: &mut MonoCtx,
) -> TokenStream {
    if children.is_empty() {
        return quote! {};
    }
    if children.len() == 1 {
        return emit_prettify_expr(&children[0], pctx, current_rule, mctx);
    }

    let mut stmts: Vec<TokenStream> = Vec::new();
    for child in children {
        let expr = emit_prettify_expr(child, pctx, current_rule, mctx);
        stmts.push(expr);
    }

    quote! { { #(#stmts;)* } }
}
