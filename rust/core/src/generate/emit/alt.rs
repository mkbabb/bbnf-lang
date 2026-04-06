//! Alt emit codegen.
//!
//! Two strategies:
//! 1. **Enum dispatch**: branches produce enum variants → match on variant name.
//!    Variant names from `global_sub_variants` or rule name for Ref branches.
//! 2. **Constant reverse**: all branches are Map-to-constant → match on the
//!    mapped value and emit the original literal.

use bbnf_ir::{AltBranch, FnDescriptor, GrammarIR, IrNode, MapExpr, TypeDesc};
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
    // Strategy 1: all branches map to constants → value-based match.
    if let Some(ts) = try_emit_constant_alt(branches, val, ir) {
        return ts;
    }

    // Strategy 2: enum variant dispatch.
    emit_enum_alt(branches, val, ir, ctx)
}

/// Try to emit a constant-reverse Alt.
///
/// Pattern: all branches are `Map(Literal(s), fn_id)` where fn_id resolves to
/// an `Expr` with a constant value. Emit: match on the constant, emit the literal.
fn try_emit_constant_alt(
    branches: &[AltBranch],
    val: &TokenStream,
    ir: &GrammarIR,
) -> Option<TokenStream> {
    let mut arms = Vec::new();

    for branch in branches {
        let IrNode::Map { inner, fn_id } = &branch.node else { return None };
        let IrNode::Literal(sid) = inner.as_ref() else { return None };
        let FnDescriptor::Expr { expr, .. } = &ir.fns[*fn_id as usize] else { return None };

        let literal_str = ir.get_string(*sid);

        let pattern = match expr {
            MapExpr::BoolLit(true) => quote! { true },
            MapExpr::BoolLit(false) => quote! { false },
            MapExpr::IntLit(n) => {
                let lit = proc_macro2::Literal::i64_unsuffixed(*n);
                quote! { #lit }
            }
            MapExpr::FloatLit(f) => {
                let lit = proc_macro2::Literal::f64_unsuffixed(*f);
                quote! { #lit }
            }
            _ => return None,
        };

        arms.push(quote! {
            #pattern => { __sink.text(#literal_str); }
        });
    }

    Some(quote! {
        match *#val {
            #(#arms)*
            _ => {}
        }
    })
}

/// Emit an enum-variant dispatch Alt.
fn emit_enum_alt(
    branches: &[AltBranch],
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    let enum_ident = &ctx.enum_ident;

    let arms: Vec<TokenStream> = branches.iter().map(|branch| {
        let inner_val = quote! { __inner };
        let variant_ident = resolve_variant(&branch.node, ir, ctx);

        // For Ref branches: the Alt already unwrapped the enum variant.
        // Call the rule's emit directly with the unwrapped value.
        let body_emit = if let IrNode::Ref(rule_id) = &branch.node {
            let ref_rule = &ir.rules[*rule_id as usize];
            if ref_rule.meta.is_transparent {
                // Transparent: inline the body.
                super::node::emit_node(&ref_rule.body, &inner_val, ir, ctx)
            } else {
                // Non-transparent: call emit fn directly (variant already unwrapped).
                let ref_name = ir.get_string(ref_rule.name);
                let emit_fn = format_ident!("{}_emit", ref_name);
                quote! { Self::#emit_fn(#inner_val, __sink); }
            }
        } else {
            // Non-Ref branch: emit the node with the unwrapped value.
            super::node::emit_node(&branch.node, &inner_val, ir, ctx)
        };

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
fn resolve_variant(
    node: &IrNode,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> proc_macro2::Ident {
    if let IrNode::Ref(rule_id) = node {
        let ref_rule = &ir.rules[*rule_id as usize];
        let ref_name = ir.get_string(ref_rule.name);
        return format_ident!("{}", ref_name);
    }

    let node_type = ctx.node_type(node);
    if let Some(variant_name) = ctx.global_sub_variants.get(&node_type) {
        return format_ident!("{}", variant_name);
    }

    let normalized = match &node_type {
        TypeDesc::BoxedEnum => TypeDesc::Enum,
        other => other.clone(),
    };
    if let Some(variant_name) = ctx.global_sub_variants.get(&normalized) {
        return format_ident!("{}", variant_name);
    }

    format_ident!("__unknown_variant")
}
