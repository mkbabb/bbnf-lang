//! Structural-type-driven emit codegen.
//!
//! One recursive function. Dispatches on `ctx.structural_type(node)` — the
//! pre-collapse TypeDesc that reflects the actual runtime value topology.
//! No plan. No decision queries. The structural TypeMap IS the plan.

use bbnf_ir::{AltBranch, FnDescriptor, GrammarIR, IrNode, MapExpr, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Index;

use crate::backend::decisions;
use crate::generate::ir_types::IrCodegenCtx;

/// Generate emit code for an IR node using its structural type.
pub fn emit_node(
    node: &IrNode,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    let ty = ctx.structural_type(node);
    emit_typed(node, &ty, val, ir, ctx)
}

/// Emit code driven by a resolved TypeDesc.
fn emit_typed(
    node: &IrNode,
    ty: &TypeDesc,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    match ty {
        TypeDesc::Span => quote! { __sink.text(#val.as_str()); },
        TypeDesc::F64 => quote! { __sink.f64(*#val); },
        TypeDesc::U32 => quote! {
            { use ::std::fmt::Write as _; let mut __b = String::new();
              let _ = write!(__b, "{}", #val); __sink.text(&__b); }
        },

        TypeDesc::Option(inner) => {
            let inner_val = quote! { __opt_v };
            let inner_emit = emit_inner(node, inner, &inner_val, ir, ctx);
            quote! { if let Some(#inner_val) = #val { #inner_emit } }
        }

        TypeDesc::Vec(inner) => {
            let item_val = quote! { __item };
            let (sep, elem_node) = extract_separator_and_element(node, ir);
            let item_emit = emit_vec_item(elem_node, inner, &item_val, ir, ctx);
            if !sep.is_empty() {
                let sep_emit = emit_structural_frags(&sep, ir);
                quote! {
                    let mut __first = true;
                    for #item_val in #val.iter() {
                        if !__first { #sep_emit }
                        __first = false;
                        #item_emit
                    }
                }
            } else {
                quote! { for #item_val in #val.iter() { #item_emit } }
            }
        }

        TypeDesc::Tuple(elems) => {
            // Each element has a value at val.{index}. Use the IR children for
            // structural content (Literals, Epsilon).
            emit_tuple(node, elems, val, ir, ctx)
        }

        TypeDesc::Enum | TypeDesc::BoxedEnum => {
            emit_enum_dispatch(node, val, ir, ctx)
        }

        TypeDesc::Named(_) => {
            quote! {
                { use ::std::fmt::Write as _; let mut __b = String::new();
                  let _ = write!(__b, "{}", #val); __sink.text(&__b); }
            }
        }
    }
}

// ── Helpers ──────────────────────────────────────────────────────────────────

/// Emit for the inner value of an Optional/Vec, using the IR node structure.
fn emit_inner(
    node: &IrNode,
    inner_ty: &TypeDesc,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    // The IR node is Repeat(inner_node, 0, 1). Use the inner_node for structure.
    if let IrNode::Repeat { inner, .. } = node {
        return emit_typed(inner, inner_ty, val, ir, ctx);
    }
    emit_typed(node, inner_ty, val, ir, ctx)
}

/// Emit a Tuple by walking the IR Seq children in order.
fn emit_tuple(
    node: &IrNode,
    elems: &[TypeDesc],
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    let IrNode::Seq(children) = node else {
        // Not a Seq — single-element "Tuple" (shouldn't normally happen).
        if elems.len() == 1 {
            return emit_typed(node, &elems[0], val, ir, ctx);
        }
        return quote! {};
    };

    // Match children to Tuple elements. Use IR for structural content.
    // The structural TypeMap gives elems.len() == children.len() (no compression).
    let mut parts = Vec::new();

    if elems.len() == children.len() {
        // 1:1: each child maps to one Tuple element.
        for (i, (child, elem_ty)) in children.iter().zip(elems.iter()).enumerate() {
            let idx = Index::from(i);
            let child_val = quote! { #val.#idx };
            if *elem_ty == TypeDesc::Span && is_structural_node(child) {
                // Structural Span (Literal, OW): emit from the value (span text).
                parts.push(quote! { __sink.text(#child_val.as_str()); });
            } else {
                parts.push(emit_typed(child, elem_ty, &child_val, ir, ctx));
            }
        }
    } else {
        // Mismatch: shouldn't happen with structural types, but handle gracefully.
        for (i, elem_ty) in elems.iter().enumerate() {
            let idx = Index::from(i);
            let child_val = quote! { #val.#idx };
            parts.push(emit_typed(node, elem_ty, &child_val, ir, ctx));
        }
    }

    quote! { #(#parts)* }
}

/// Emit an enum dispatch by matching on variant names.
fn emit_enum_dispatch(
    node: &IrNode,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    // Find the Alt branches (walk through transparent wrappers).
    let branches = find_alt_branches(node, ir);
    if branches.is_empty() {
        // Not an Alt — probably a single Ref. Call its emit directly.
        if let IrNode::Ref(rule_id) = node {
            let ref_rule = &ir.rules[*rule_id as usize];
            if ref_rule.meta.is_transparent {
                return emit_node(&ref_rule.body, val, ir, ctx);
            }
            let name = ir.get_string(ref_rule.name);
            let emit_fn = format_ident!("{}_emit", name);
            let variant = format_ident!("{}", name);
            let enum_ident = &ctx.enum_ident;
            return quote! {
                if let #enum_ident::#variant(__inner) = #val {
                    Self::#emit_fn(__inner, __sink);
                }
            };
        }
        return quote! {};
    }

    // Check constant-reverse: all branches are Map(Literal, constant).
    if let Some(cr) = try_constant_reverse(&branches, val, ir) {
        return cr;
    }

    // Check all-Span Alt: every branch is Span → emit as Span text.
    let all_span = branches.iter().all(|b| {
        ir.type_map.as_ref()
            .and_then(|tm| tm.node_type(&b.node).cloned())
            .unwrap_or(TypeDesc::Span) == TypeDesc::Span
    });
    if all_span {
        return quote! { __sink.text(#val.as_str()); };
    }

    // Enum variant dispatch.
    let enum_ident = &ctx.enum_ident;
    let mut arms = Vec::new();

    for branch in branches {
        if let IrNode::Ref(rule_id) = &branch.node {
            let ref_rule = &ir.rules[*rule_id as usize];
            if ref_rule.meta.is_transparent {
                // Transparent Ref: recurse — lift its branches.
                let inner_arms = emit_enum_dispatch(&ref_rule.body, val, ir, ctx);
                // Can't easily extract arms from TokenStream, so just inline.
                arms.push(inner_arms);
                continue;
            }
            let name = ir.get_string(ref_rule.name);
            let variant = format_ident!("{}", name);
            let emit_fn = format_ident!("{}_emit", name);
            arms.push(quote! {
                #enum_ident::#variant(__inner) => {
                    Self::#emit_fn(__inner, __sink);
                }
            });
        } else {
            // Non-Ref branch: look up sub-variant name.
            let branch_ty = ctx.node_type(&branch.node);
            let variant_name = ctx.global_sub_variants.get(&branch_ty)
                .or_else(|| {
                    let norm = match &branch_ty {
                        TypeDesc::BoxedEnum => TypeDesc::Enum,
                        other => other.clone(),
                    };
                    ctx.global_sub_variants.get(&norm)
                });
            if let Some(name) = variant_name {
                let variant = format_ident!("{}", name);
                let inner_val = quote! { __inner };
                let body = emit_node(&branch.node, &inner_val, ir, ctx);
                arms.push(quote! {
                    #enum_ident::#variant(#inner_val) => { #body }
                });
            }
        }
    }

    if arms.is_empty() {
        quote! {}
    } else {
        quote! { match #val { #(#arms)* _ => {} } }
    }
}

/// Emit a single Vec item, unwrapping enum variants for non-transparent Refs.
fn emit_vec_item(
    node: &IrNode,
    elem_ty: &TypeDesc,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    // If elem is Enum/BoxedEnum, dispatch on variant.
    if matches!(elem_ty, TypeDesc::Enum | TypeDesc::BoxedEnum) {
        return emit_enum_dispatch(node, val, ir, ctx);
    }
    emit_typed(node, elem_ty, val, ir, ctx)
}

/// Extract separator and element node from a Repeat's inner structure.
fn extract_separator_and_element<'a>(node: &'a IrNode, ir: &'a GrammarIR) -> (Vec<&'a IrNode>, &'a IrNode) {
    if let IrNode::Repeat { inner, .. } = node {
        // sep_by: Skip(elem, Repeat(sep, 0, 1))
        if let Some((elem, sep)) = decisions::detect_sep_by(inner) {
            return (vec![sep], elem);
        }
        // Next(sep, elem) in plain repeat
        if let IrNode::Next(sep, elem) = inner.as_ref() {
            return (vec![sep], elem);
        }
        return (vec![], inner.as_ref());
    }
    (vec![], node)
}

/// Emit structural content from IR nodes (separators, delimiters).
fn emit_structural_frags(nodes: &[&IrNode], ir: &GrammarIR) -> TokenStream {
    let parts: Vec<_> = nodes.iter().map(|n| emit_structural(n, ir)).collect();
    quote! { #(#parts)* }
}

/// Emit structural content from a single IR node.
fn emit_structural(node: &IrNode, ir: &GrammarIR) -> TokenStream {
    match node {
        IrNode::Literal(sid) => {
            let s = ir.get_string(*sid);
            if s.len() == 1 {
                let b = s.as_bytes()[0];
                quote! { __sink.char(#b); }
            } else {
                quote! { __sink.text(#s); }
            }
        }
        IrNode::Ref(rule_id) => emit_structural(&ir.rules[*rule_id as usize].body, ir),
        IrNode::Seq(children) => {
            let parts: Vec<_> = children.iter().map(|c| emit_structural(c, ir)).collect();
            quote! { #(#parts)* }
        }
        IrNode::OptionalWhitespace(inner) => emit_structural(inner, ir),
        IrNode::Skip(l, _) | IrNode::Next(_, l) => emit_structural(l, ir),
        _ => quote! {},
    }
}

/// Find Alt branches, walking through Map wrappers.
fn find_alt_branches<'a>(node: &'a IrNode, ir: &'a GrammarIR) -> &'a [AltBranch] {
    match node {
        IrNode::Alt(branches, _) => branches,
        IrNode::Ref(rule_id) if ir.rules[*rule_id as usize].meta.is_transparent => {
            find_alt_branches(&ir.rules[*rule_id as usize].body, ir)
        }
        _ => &[],
    }
}

fn is_structural_node(node: &IrNode) -> bool {
    matches!(node,
        IrNode::Literal(_) | IrNode::Epsilon | IrNode::Negate(_) |
        IrNode::OptionalWhitespace(_)
    )
}

fn try_constant_reverse(branches: &[AltBranch], val: &TokenStream, ir: &GrammarIR) -> Option<TokenStream> {
    let mut arms = Vec::new();
    for branch in branches {
        let IrNode::Map { inner, fn_id } = &branch.node else { return None };
        let IrNode::Literal(sid) = inner.as_ref() else { return None };
        let FnDescriptor::Expr { expr, .. } = &ir.fns[*fn_id as usize] else { return None };
        let lit = ir.get_string(*sid);
        let pat = match expr {
            MapExpr::BoolLit(true) => quote! { true },
            MapExpr::BoolLit(false) => quote! { false },
            MapExpr::IntLit(n) => { let l = proc_macro2::Literal::i64_unsuffixed(*n); quote! { #l } }
            MapExpr::FloatLit(f) => { let l = proc_macro2::Literal::f64_unsuffixed(*f); quote! { #l } }
            _ => return None,
        };
        arms.push(quote! { #pat => { __sink.text(#lit); } });
    }
    Some(quote! { match *#val { #(#arms)* _ => {} } })
}
