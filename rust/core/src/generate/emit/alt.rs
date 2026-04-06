//! Alt emit codegen using global_sub_variants for variant name resolution.
//!
//! Generates `match value { Variant(inner) => { ... } }` where variant names
//! are looked up via the same `global_sub_variants` map that ir_enums.rs uses
//! to generate the enum definition.

use bbnf_ir::{AltBranch, GrammarIR, IrNode, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::generate::ir_types::IrCodegenCtx;

/// Generate emit code for an Alt node.
pub fn emit_alt(
    branches: &[AltBranch],
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    let enum_ident = &ctx.enum_ident;

    let arms: Vec<TokenStream> = branches.iter().map(|branch| {
        let inner_val = quote! { __inner };

        // Resolve variant name: Ref → rule name, otherwise → global_sub_variants by type.
        let variant_ident = resolve_variant(&branch.node, ir, ctx);
        let body_emit = super::node::emit_node(&branch.node, &inner_val, ir, ctx);

        quote! {
            #enum_ident::#variant_ident(#inner_val) => {
                #body_emit
            }
        }
    }).collect();

    quote! {
        match #val {
            #(#arms)*
            _ => {}
        }
    }
}

/// Resolve the enum variant name for an Alt branch.
///
/// Priority:
/// 1. Ref → referenced rule's name (matches ir_enums.rs rule variant generation)
/// 2. TypeDesc lookup in global_sub_variants (matches ir_enums.rs sub-variant generation)
/// 3. Fallback to Enum/BoxedEnum catch-all
fn resolve_variant(
    node: &IrNode,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> proc_macro2::Ident {
    // Case 1: direct Ref → use the rule name.
    if let IrNode::Ref(rule_id) = node {
        let ref_rule = &ir.rules[*rule_id as usize];
        let ref_name = ir.get_string(ref_rule.name);
        return format_ident!("{}", ref_name);
    }

    // Case 2: look up the node's type in global_sub_variants.
    let node_type = ctx.node_type(node);
    if let Some(variant_name) = ctx.global_sub_variants.get(&node_type) {
        return format_ident!("{}", variant_name);
    }

    // Normalize: BoxedEnum → Enum for lookup.
    let normalized = match &node_type {
        TypeDesc::BoxedEnum => TypeDesc::Enum,
        other => other.clone(),
    };
    if let Some(variant_name) = ctx.global_sub_variants.get(&normalized) {
        return format_ident!("{}", variant_name);
    }

    // Fallback: this branch's type wasn't registered as a sub-variant.
    // This happens for Enum/BoxedEnum branches that share the main enum type.
    // Use a placeholder that will produce a compile error if wrong.
    format_ident!("__unknown_variant")
}
