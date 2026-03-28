//! Ident-dispatch optimization for property-name-group Alt patterns.
//!
//! Pattern: Alt where ALL branches are Seq nodes starting with a literal-set
//! (property name group) followed by a common literal (":"), then shared suffix.
//! The last branch may start with a regex (catch-all like genericDecl).
//!
//! Instead of trying all N branches sequentially, parse `ident` once, then
//! dispatch on the consumed string via byte comparisons.

use bbnf_ir::{AltBranch, IrNode, TypeDesc};

use proc_macro2::TokenStream;
use quote::quote;

use super::super::super::super::ir_types::{self, IrCodegenCtx};
use super::super::super::unescape_literal;
use super::super::{emit_mono_expr, MonoCtx};
use super::{coerce_mono_branch, coerce_mono_branch_by_value};

/// Extract literal strings from a branch's leading node.
/// Returns None if the leading node isn't a pure literal or literal-Alt.
pub(in super::super) fn extract_leading_literals(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> Option<Vec<String>> {
    match node {
        IrNode::Seq(children) if !children.is_empty() => {
            extract_leading_literals(&children[0], ctx)
        }
        IrNode::Ref(rule_id) => {
            let rule = &ctx.ir.rules[*rule_id as usize];
            extract_leading_literals(&rule.body, ctx)
        }
        IrNode::Literal(sid) => {
            Some(vec![unescape_literal(ctx.ir.get_string(*sid))])
        }
        IrNode::Alt(branches, _) => {
            let mut lits = Vec::new();
            for b in branches {
                lits.extend(extract_leading_literals(&b.node, ctx)?);
            }
            Some(lits)
        }
        IrNode::Map { inner, .. } => extract_leading_literals(inner, ctx),
        IrNode::OptionalWhitespace(inner) => extract_leading_literals(inner, ctx),
        _ => None,
    }
}

/// Check if a node's leading position is a regex (catch-all ident pattern).
pub(in super::super) fn is_leading_regex(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> bool {
    match node {
        IrNode::Seq(children) if !children.is_empty() => is_leading_regex(&children[0], ctx),
        IrNode::Ref(rule_id) => {
            is_leading_regex(&ctx.ir.rules[*rule_id as usize].body, ctx)
        }
        IrNode::Regex(_) => true,
        IrNode::Alt(branches, _) => branches.iter().any(|b| is_leading_regex(&b.node, ctx)),
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => is_leading_regex(inner, ctx),
        _ => false,
    }
}

/// Check if a Seq has `:` as its second element (after the property name).
pub(in super::super) fn has_colon_separator(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> bool {
    match node {
        IrNode::Seq(children) if children.len() >= 2 => {
            matches!(&children[1], IrNode::Literal(sid) if ctx.ir.get_string(*sid) == ":")
        }
        IrNode::Ref(rule_id) => {
            has_colon_separator(&ctx.ir.rules[*rule_id as usize].body, ctx)
        }
        IrNode::OptionalWhitespace(inner) => has_colon_separator(inner, ctx),
        _ => false,
    }
}

/// Try to emit ident-dispatch for a property-name-group Alt pattern.
///
/// Detects: Alt where branches 0..N-2 each start with literal property names
/// followed by ":", and branch N-1 starts with a regex (genericDecl catch-all).
/// Emits: parse ident once -> byte-match the consumed name -> route to branch.
pub(in super::super) fn try_emit_ident_dispatch(
    branches: &[AltBranch],
    branch_tys: &[TypeDesc],
    needs_coercion: bool,
    local_sub_variants: &[Option<String>],
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    elide_box: bool,
) -> Option<TokenStream> {
    // Need at least 3 branches (2 typed + 1 fallback).
    if branches.len() < 3 {
        return None;
    }

    let fallback_idx = branches.len() - 1;

    // Last branch must have a regex (catch-all ident like genericDecl).
    if !is_leading_regex(&branches[fallback_idx].node, ctx) {
        return None;
    }

    // ALL typed branches must start with CSS-ident-like literals
    // (no @-prefixed strings, no punctuation).
    let mut branch_literals: Vec<Vec<String>> = Vec::new();
    for branch in &branches[..fallback_idx] {
        let lits = extract_leading_literals(&branch.node, ctx)?;
        if lits.is_empty() {
            return None;
        }
        // Every literal must be a valid CSS ident (starts with [a-zA-Z_-]).
        for lit in &lits {
            let name = lit.strip_suffix(':').unwrap_or(lit);
            match name.as_bytes().first() {
                Some(b'a'..=b'z') | Some(b'A'..=b'Z') | Some(b'_') | Some(b'-') => {}
                _ => return None,
            }
        }
        branch_literals.push(lits);
    }

    // Build if-else chain: parse ident once, then compare against literal sets.
    let cp_var = mctx.fresh("id_cp");
    let mut if_arms: Vec<TokenStream> = Vec::new();

    for (idx, lits) in branch_literals.iter().enumerate() {
        let branch_expr = emit_mono_expr(&branches[idx].node, ctx, mctx, elide_box);
        let coerced = if needs_coercion {
            let sv_name = local_sub_variants.get(idx).and_then(|s| s.as_deref());
            if elide_box {
                coerce_mono_branch_by_value(branch_expr, &branch_tys[idx], sv_name, ctx)
            } else {
                coerce_mono_branch(branch_expr, &branch_tys[idx], sv_name, ctx)
            }
        } else {
            branch_expr
        };

        // Generate byte comparisons for each literal in this branch's set.
        let comparisons: Vec<TokenStream> = lits.iter().map(|lit| {
            let name = lit.strip_suffix(':').unwrap_or(lit);
            let bytes = name.as_bytes();
            let byte_lits: Vec<proc_macro2::Literal> =
                bytes.iter().map(|b| proc_macro2::Literal::byte_character(*b)).collect();
            let len = bytes.len();
            quote! { (__id_len == #len && __id_bytes == &[#(#byte_lits),*]) }
        }).collect();

        // Emit: if property name matches, restore offset and run the branch.
        if_arms.push(quote! {
            if #(#comparisons)||* {
                state.offset = #cp_var;
                return #coerced;
            }
        });
    }

    // Fallback branch.
    let fb_expr = emit_mono_expr(&branches[fallback_idx].node, ctx, mctx, elide_box);
    let fb_coerced = if needs_coercion {
        let sv_name = local_sub_variants.get(fallback_idx).and_then(|s| s.as_deref());
        if elide_box {
            coerce_mono_branch_by_value(fb_expr, &branch_tys[fallback_idx], sv_name, ctx)
        } else {
            coerce_mono_branch(fb_expr, &branch_tys[fallback_idx], sv_name, ctx)
        }
    } else {
        fb_expr
    };

    let return_type = if needs_coercion {
        if elide_box {
            ctx.enum_type.clone()
        } else {
            ctx.boxed_enum_type.clone()
        }
    } else if branch_tys[0] == TypeDesc::BoxedEnum {
        ctx.boxed_enum_type.clone()
    } else {
        ir_types::type_desc_to_syn(&branch_tys[0], ctx)
    };

    Some(quote! {
        (|| -> Option<#return_type> {
            let #cp_var = state.offset;
            // Parse ident to determine which branch to try.
            if let Some(ref __id_s) = ::parse_that::scan_ident(state) {
                let __id_bytes = &state.src_bytes[__id_s.start..__id_s.end];
                let __id_len = __id_bytes.len();
                #(#if_arms)*
            }
            // No typed match or not an ident -- try fallback.
            state.offset = #cp_var;
            #fb_coerced
        })()
    })
}
