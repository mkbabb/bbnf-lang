//! Alternation emit codegen.
//!
//! Generates a `match` on the enum variant, emitting the corresponding branch.

use bbnf_ir::{AltBranch, GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::generate::ir_types::IrCodegenCtx;

/// Generate emit code for an Alt node.
///
/// Produces a `match value { Variant(inner) => emit_branch(inner, sink), ... }`.
pub fn emit_alt(
    branches: &[AltBranch],
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
    rule: &IrRule,
) -> TokenStream {
    let enum_ident = &ctx.enum_ident;
    let rule_name = ir.get_string(rule.name);

    let arms: Vec<TokenStream> = branches
        .iter()
        .enumerate()
        .map(|(i, branch)| {
            let variant_ident = variant_ident_for_branch(&branch.node, i, ir, ctx);
            let body_emit = super::expr::emit_node(&branch.node, ir, ctx, rule);

            quote! {
                #enum_ident::#variant_ident(value) => {
                    #body_emit
                }
            }
        })
        .collect();

    quote! {
        match value {
            #(#arms)*
            _ => {}
        }
    }
}

/// Derive the enum variant identifier for a branch.
fn variant_ident_for_branch(
    body: &IrNode,
    index: usize,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> proc_macro2::Ident {
    match body {
        IrNode::Ref(rule_id) => {
            let ref_name = ir.get_string(ir.rules[*rule_id as usize].name);
            format_ident!("{}", ref_name)
        }
        _ => {
            // Anonymous sub-variant: use rule_name + index pattern.
            format_ident!("_{}", index)
        }
    }
}
