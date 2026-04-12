//! Tape-first serialize emission.
//!
//! Under the tape-first architecture every rule's data lives on the
//! tape.  Per-rule `serialize_*` functions emit `span_text()` for
//! exact round-trip reproduction.  `__dispatch_serialize` routes on
//! `variant_idx()` to the correct per-rule function.  Alt-bodied
//! rules dispatch on the branch index to reconstruct constant-mapped
//! literals from the IR and delegate compound branches to their
//! child's `serialize_*` function via `child(0)`.

use bbnf_ir::{FnDescriptor, GrammarIR, IrNode, IrRule, MapExpr};
use bbnf_ir::passes::MaterializationClass;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::rust::ir_types::IrCodegenCtx;

// ─── Public ──────────────────────────────────────────────────────

/// Generate the variant_idx match arms for `__dispatch_serialize`.
pub fn generate_dispatch_arms(ir: &GrammarIR, _ctx: &IrCodegenCtx) -> Vec<TokenStream> {
    let mut arms = Vec::new();
    for rule in &ir.rules {
        if rule.meta.is_transparent || !rule_pushes_tape_record(ir, rule) {
            continue;
        }
        let idx = (rule.id & 0xFF) as u8;
        let emit_fn = format_ident!("serialize_{}", ir.get_string(rule.name));
        arms.push(quote! { #idx => { Self::#emit_fn(__v, __ser); } });
    }
    arms
}

/// Check if a rule produces a tape record (MustTape or TapeSpanOnly).
pub fn rule_pushes_tape_record(ir: &GrammarIR, rule: &IrRule) -> bool {
    rule_materialization(ir, rule) != MaterializationClass::TransparentElide
}

/// Emit serialize code for an Alt-bodied rule.
///
/// Dispatches on `variant_idx()` to select the chosen branch, then
/// emits structural content from the IR for constant-mapped branches
/// and delegates to child `serialize_*` functions for compound
/// branches.
pub fn emit_alt_body(
    branches: &[bbnf_ir::AltBranch],
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    // If every branch is a constant-mapped literal, just emit
    // span_text — it IS the original literal.
    if let Some(cr) = try_constant_reverse(branches, ir) {
        return cr;
    }

    let nv = node_view_ctor(ctx);
    let mut arms = Vec::new();

    for (branch_idx, branch) in branches.iter().enumerate() {
        let idx = branch_idx as u8;
        let branch_emit = emit_alt_branch(&branch.node, ir, ctx, &nv);
        arms.push(quote! { #idx => { #branch_emit } });
    }

    quote! {
        match __v.variant_idx() {
            #(#arms)*
            _ => { __ser.text(__v.span_text()); }
        }
    }
}

// ─── Alt branch emission ─────────────────────────────────────────

/// Emit serialization for a single Alt branch.
///
/// - Constant-mapped literals: emit the literal from the IR.
/// - Ref to a tape-pushing rule: `child(0)` + delegate; span_text
///   fallback.
/// - Everything else: `span_text()`.
fn emit_alt_branch(
    node: &IrNode,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
    nv: &syn::Ident,
) -> TokenStream {
    if let IrNode::OptionalWhitespace(inner) = node {
        return emit_alt_branch(inner, ir, ctx, nv);
    }

    // Map(Literal, Constant): emit the original literal.
    if let IrNode::Map { inner, fn_id } = node {
        if is_constant_map(&ir.fns[*fn_id as usize]) {
            return serialize_structural(inner, ir);
        }
        if is_transparent_map(&ir.fns[*fn_id as usize]) {
            return emit_alt_branch(inner, ir, ctx, nv);
        }
    }

    // Ref to a tape-pushing rule: child(0) + delegate.
    if let IrNode::Ref(rule_id) = node {
        let ref_rule = &ir.rules[*rule_id as usize];
        if !ref_rule.meta.is_transparent && rule_pushes_tape_record(ir, ref_rule) {
            let fn_ident = format_ident!("serialize_{}", ir.get_string(ref_rule.name));
            return quote! {
                if let Some(__child_cursor) = __v.cursor().child(0) {
                    let __v = #nv::from_cursor(__child_cursor, __v.input());
                    Self::#fn_ident(__v, __ser);
                } else {
                    __ser.text(__v.span_text());
                }
            };
        }
        // Transparent/elided Ref: recurse on its body.
        return emit_alt_branch(&ref_rule.body, ir, ctx, nv);
    }

    // Literal: emit from IR.
    if let IrNode::Literal(sid) = node {
        let s = ir.get_string(*sid);
        return quote! { __ser.text(#s); };
    }

    // Default: span_text covers the matched region.
    quote! { __ser.text(__v.span_text()); }
}

// ─── Constant reverse ────────────────────────────────────────────

fn try_constant_reverse(
    branches: &[bbnf_ir::AltBranch],
    ir: &GrammarIR,
) -> Option<TokenStream> {
    for branch in branches {
        let IrNode::Map { inner, fn_id } = &branch.node else { return None };
        let IrNode::Literal(_) = inner.as_ref() else { return None };
        let FnDescriptor::Expr { expr, .. } = &ir.fns[*fn_id as usize] else { return None };
        match expr {
            MapExpr::BoolLit(_) | MapExpr::IntLit(_) | MapExpr::FloatLit(_) => {}
            _ => return None,
        }
    }
    Some(quote! { __ser.text(__v.span_text()); })
}

// ─── Structural emission ─────────────────────────────────────────

/// Emit structural content from an IR node — literals, separators,
/// and other tokens that don't carry semantic data.
fn serialize_structural(node: &IrNode, ir: &GrammarIR) -> TokenStream {
    match node {
        IrNode::Literal(sid) => {
            let s = ir.get_string(*sid);
            if s.len() == 1 {
                let b = s.as_bytes()[0];
                quote! { __ser.char(#b); }
            } else {
                quote! { __ser.text(#s); }
            }
        }
        IrNode::Ref(rule_id) => serialize_structural(&ir.rules[*rule_id as usize].body, ir),
        IrNode::Seq(children) => {
            let parts: Vec<_> = children.iter().map(|c| serialize_structural(c, ir)).collect();
            quote! { #(#parts)* }
        }
        IrNode::OptionalWhitespace(inner) => serialize_structural(inner, ir),
        IrNode::Skip(_, structural) | IrNode::Next(structural, _) => {
            serialize_structural(structural, ir)
        }
        IrNode::Repeat { inner, .. } => serialize_structural(inner, ir),
        _ => quote! {},
    }
}

// ─── Helpers ─────────────────────────────────────────────────────

fn node_view_ctor(ctx: &IrCodegenCtx) -> syn::Ident {
    format_ident!("{}NodeView", ctx.ident)
}

fn is_constant_map(fd: &FnDescriptor) -> bool {
    matches!(fd, FnDescriptor::Expr {
        expr: MapExpr::IntLit(_) | MapExpr::FloatLit(_)
            | MapExpr::StringLit(_) | MapExpr::BoolLit(_), ..
    })
}

fn is_transparent_map(fd: &FnDescriptor) -> bool {
    matches!(fd, FnDescriptor::EnumWrap { .. } | FnDescriptor::BoxWrap)
}

fn rule_materialization(ir: &GrammarIR, rule: &IrRule) -> MaterializationClass {
    if rule.meta.preserve_identity {
        return MaterializationClass::MustTape;
    }
    if let Some(dag) = ir.dag.as_ref() {
        if let Some(node_id) = dag.node_for(&rule.body) {
            if let Some(class) = ir.materialization.get(&node_id) {
                return *class;
            }
        }
    }
    MaterializationClass::MustTape
}
