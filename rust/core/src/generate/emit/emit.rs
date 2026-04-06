//! Core type-driven emit dispatch.
//!
//! Every function queries `ctx.node_type(node)` to determine the value's type,
//! then generates type-appropriate destructuring code. The IR provides structural
//! content (literals, separators); the TypeDesc provides the traversal strategy.

use bbnf_ir::{AltBranch, FnDescriptor, GrammarIR, IrNode, MapExpr, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Index;

use super::super::ir_types::type_desc_to_syn;

use crate::generate::ir_types::IrCodegenCtx;

// ─── Main dispatch ───────────────────────────────────────────────────────────

/// Generate emit code for an IR node. Type-driven: queries TypeMap for `node`'s
/// type and generates matching destructuring code.
pub fn emit_node(
    node: &IrNode,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    match node {
        // ── Structural (no value consumed, emit from IR) ─────────────
        IrNode::Literal(sid) => emit_literal(*sid, ir),
        IrNode::Epsilon => quote! {},
        IrNode::Negate(_) => quote! {},
        IrNode::OptionalWhitespace(inner) => emit_node(inner, val, ir, ctx),

        // ── Value leaves ─────────────────────────────────────────────
        IrNode::Regex(_) => quote! { __sink.text(#val.as_str()); },

        IrNode::Ref(rule_id) => {
            let ref_rule = &ir.rules[*rule_id as usize];
            if ref_rule.meta.is_transparent {
                // Transparent: inline the body with the current value.
                emit_node(&ref_rule.body, val, ir, ctx)
            } else {
                // Non-transparent: call the rule's emit function with a typed
                // let binding that coerces the value to &RuleType via deref.
                let rule_type = ctx.rule_types.get(&ref_rule.id)
                    .cloned()
                    .unwrap_or_else(|| ctx.enum_type.clone());
                let name = ir.get_string(ref_rule.name);
                let emit_fn = format_ident!("{}_emit", name);
                quote! {
                    {
                        let __ref: &#rule_type = &#val;
                        Self::#emit_fn(__ref, __sink);
                    }
                }
            }
        }

        // ── Composites (type-driven) ─────────────────────────────────
        IrNode::Seq(children) => emit_seq(children, val, ir, ctx),
        IrNode::Alt(branches, _) => emit_alt(branches, val, ir, ctx),
        IrNode::Repeat { inner, lo, hi } => emit_repeat(inner, *lo, *hi, val, ir, ctx),

        // ── Binary (kept vs structural) ──────────────────────────────
        IrNode::Skip(kept, structural) => {
            let k = emit_node(kept, val, ir, ctx);
            let s = emit_structural(structural, ir);
            quote! { #k #s }
        }
        IrNode::Next(structural, kept) => {
            let s = emit_structural(structural, ir);
            let k = emit_node(kept, val, ir, ctx);
            quote! { #s #k }
        }
        IrNode::Minus(lhs, _) => emit_node(lhs, val, ir, ctx),

        // ── Map (reverse FnDescriptor) ───────────────────────────────
        IrNode::Map { inner, fn_id } => emit_map(inner, *fn_id, val, ir, ctx),

        // ── TokenDispatch (leaf fallback) ────────────────────────────
        IrNode::TokenDispatch { .. } => quote! { __sink.text(#val.as_str()); },
    }
}

/// Emit structural content from IR (no value consumed).
pub fn emit_structural(node: &IrNode, ir: &GrammarIR) -> TokenStream {
    match node {
        IrNode::Literal(sid) => emit_literal(*sid, ir),
        IrNode::Ref(rule_id) => {
            emit_structural(&ir.rules[*rule_id as usize].body, ir)
        }
        IrNode::Epsilon | IrNode::Negate(_) => quote! {},
        IrNode::OptionalWhitespace(inner) => emit_structural(inner, ir),
        IrNode::Seq(children) => {
            let parts: Vec<_> = children.iter().map(|c| emit_structural(c, ir)).collect();
            quote! { #(#parts)* }
        }
        IrNode::Skip(l, _) | IrNode::Next(_, l) => emit_structural(l, ir),
        IrNode::Repeat { inner, .. } => emit_structural(inner, ir),
        _ => quote! {},
    }
}

fn emit_literal(sid: u32, ir: &GrammarIR) -> TokenStream {
    let s = ir.get_string(sid);
    if s.len() == 1 {
        let b = s.as_bytes()[0];
        quote! { __sink.char(#b); }
    } else {
        quote! { __sink.text(#s); }
    }
}

// ─── Seq: type-driven decomposition ─────────────────────────────────────────

fn emit_seq(
    children: &[IrNode],
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    // Authoritative types from TypeMap.
    let mut child_types: Vec<TypeDesc> = ctx.seq_child_types(children)
        .unwrap_or_else(|| children.iter().map(|c| ctx.node_type(c)).collect());

    let result_type = ctx.seq_result_type(children)
        .unwrap_or_else(|| TypeDesc::Span);

    // Override with result Tuple elements (driver.rs:409-419).
    if let TypeDesc::Tuple(ref elems) = result_type {
        if elems.len() == child_types.len() {
            child_types = elems.clone();
        }
    }

    // All-Span: pure structural emission.
    if result_type == TypeDesc::Span {
        let parts: Vec<_> = children.iter().map(|c| emit_structural(c, ir)).collect();
        return quote! { #(#parts)* };
    }

    // Flattened Vec: (T, Vec<T>) → Vec<T). Iterate flat.
    if matches!(&result_type, TypeDesc::Vec(_)) && child_types.len() == 2 {
        return emit_flattened_seq(children, &child_types, val, ir, ctx);
    }

    // Mixed: Span children are structural, non-Span are value.
    // The TYPE determines which is which — not the IR node kind.
    let non_span: Vec<usize> = child_types.iter().enumerate()
        .filter(|(_, t)| **t != TypeDesc::Span)
        .map(|(i, _)| i)
        .collect();

    let n_values = non_span.len();
    // Result type determines destructuring strategy:
    // - Tuple: index ALL elements (Span and non-Span both have positions).
    //   Span children are structural (emit from IR), non-Span are value.
    // - Non-Tuple with 1 value child: val IS the child (no indexing).
    // - Non-Tuple with >1 value: val_idx among value children only.

    let mut parts = Vec::new();

    if let TypeDesc::Tuple(ref elems) = result_type {
        // Tuple: ALL children have values at tuple positions.
        // Span children: emit text from the value (val.idx.as_str()).
        // Non-Span children: emit via emit_node for full traversal.
        for (i, (child, ty)) in children.iter().zip(elems.iter()).enumerate() {
            let idx = Index::from(i);
            let child_val = quote! { #val.#idx };
            if *ty == TypeDesc::Span {
                parts.push(quote! { __sink.text(#child_val.as_str()); });
            } else {
                parts.push(emit_node(child, &child_val, ir, ctx));
            }
        }
    } else if n_values == 1 {
        // Single value child: val IS the child directly.
        for (child, ty) in children.iter().zip(child_types.iter()) {
            if *ty == TypeDesc::Span {
                parts.push(emit_structural(child, ir));
            } else {
                parts.push(emit_node(child, val, ir, ctx));
            }
        }
    } else {
        // Multiple value children, non-Tuple result: index by value position.
        let mut val_idx = 0usize;
        for (child, ty) in children.iter().zip(child_types.iter()) {
            if *ty == TypeDesc::Span {
                parts.push(emit_structural(child, ir));
            } else {
                let idx = Index::from(val_idx);
                let child_val = quote! { #val.#idx };
                parts.push(emit_node(child, &child_val, ir, ctx));
                val_idx += 1;
            }
        }
    }

    quote! { #(#parts)* }
}

fn emit_flattened_seq(
    children: &[IrNode],
    child_types: &[TypeDesc],
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    // (head, Vec<tail>) or (Vec<head>, tail) → flat Vec.
    // Extract separator from the Repeat's inner structure.
    let repeat_idx = if matches!(&child_types[1], TypeDesc::Vec(_)) { 1 } else { 0 };
    let sep = extract_repeat_separator(&children[repeat_idx], ir);
    let item_node = &children[1 - repeat_idx]; // the non-Repeat child

    let item_emit = emit_item_unwrapped(item_node, &quote! { __item }, ir, ctx);

    quote! {
        let mut __first = true;
        for __item in #val.iter() {
            if !__first { #sep }
            __first = false;
            #item_emit
        }
    }
}

// ─── Alt: enum variant dispatch ─────────────────────────────────────────────

fn emit_alt(
    branches: &[AltBranch],
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    // Constant-reverse: all branches are Map(Literal, constant).
    if let Some(ts) = try_constant_alt(branches, val, ir) {
        return ts;
    }

    // Enum variant dispatch.
    let enum_ident = &ctx.enum_ident;
    let arms: Vec<_> = branches.iter().map(|branch| {
        let inner = quote! { __inner };
        let variant = resolve_variant(&branch.node, ir, ctx);

        // Inside the match arm, __inner IS the unwrapped value.
        // For Ref branches: call the rule's emit directly (already unwrapped).
        let body = if let IrNode::Ref(rule_id) = &branch.node {
            let ref_rule = &ir.rules[*rule_id as usize];
            if ref_rule.meta.is_transparent {
                emit_node(&ref_rule.body, &inner, ir, ctx)
            } else {
                let rule_type = ctx.rule_types.get(&ref_rule.id)
                    .cloned()
                    .unwrap_or_else(|| ctx.enum_type.clone());
                let name = ir.get_string(ref_rule.name);
                let emit_fn = format_ident!("{}_emit", name);
                quote! {
                    {
                        let __ref: &#rule_type = &#inner;
                        Self::#emit_fn(__ref, __sink);
                    }
                }
            }
        } else {
            emit_node(&branch.node, &inner, ir, ctx)
        };

        quote! { #enum_ident::#variant(#inner) => { #body } }
    }).collect();

    quote! { match #val { #(#arms)* _ => {} } }
}

fn try_constant_alt(
    branches: &[AltBranch],
    val: &TokenStream,
    ir: &GrammarIR,
) -> Option<TokenStream> {
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

fn resolve_variant(node: &IrNode, ir: &GrammarIR, ctx: &IrCodegenCtx) -> proc_macro2::Ident {
    if let IrNode::Ref(rule_id) = node {
        return format_ident!("{}", ir.get_string(ir.rules[*rule_id as usize].name));
    }
    let ty = ctx.node_type(node);
    if let Some(name) = ctx.global_sub_variants.get(&ty) {
        return format_ident!("{}", name);
    }
    if let Some(name) = ctx.global_sub_variants.get(&match &ty {
        TypeDesc::BoxedEnum => TypeDesc::Enum,
        other => other.clone(),
    }) {
        return format_ident!("{}", name);
    }
    format_ident!("__unknown_variant")
}

// ─── Repeat: Vec/Option iteration ───────────────────────────────────────────

fn emit_repeat(
    inner: &IrNode,
    lo: u32,
    hi: u32,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    if lo == 0 && hi == 1 {
        // Optional.
        let inner_emit = emit_node(inner, &quote! { __opt_v }, ir, ctx);
        return quote! { if let Some(__opt_v) = #val { #inner_emit } };
    }

    // Vec: detect sep_by.
    if let Some((elem, sep)) = detect_sep_by(inner) {
        let sep_emit = emit_structural(sep, ir);
        let elem_emit = emit_item_unwrapped(elem, &quote! { __item }, ir, ctx);
        return quote! {
            let mut __first = true;
            for __item in #val.iter() {
                if !__first { #sep_emit }
                __first = false;
                #elem_emit
            }
        };
    }

    // Plain repetition.
    let item_emit = emit_item_unwrapped(inner, &quote! { __item }, ir, ctx);
    quote! { for __item in #val.iter() { #item_emit } }
}

fn detect_sep_by(inner: &IrNode) -> Option<(&IrNode, &IrNode)> {
    if let IrNode::Skip(elem, opt_sep) = inner {
        if let IrNode::Repeat { inner: sep, lo: 0, hi: 1 } = opt_sep.as_ref() {
            return Some((elem.as_ref(), sep.as_ref()));
        }
    }
    None
}

/// Emit a Vec item, unwrapping enum variants for non-transparent Ref nodes.
/// Also handles Next/Skip wrappers around Refs.
fn emit_item_unwrapped(
    node: &IrNode,
    item_val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    // Direct Ref.
    if let IrNode::Ref(rule_id) = node {
        return emit_ref_item(*rule_id, item_val, ir, ctx);
    }
    // Next(structural, Ref): emit structural, then unwrap Ref.
    if let IrNode::Next(structural, kept) = node {
        if let IrNode::Ref(rule_id) = kept.as_ref() {
            let s = emit_structural(structural, ir);
            let r = emit_ref_item(*rule_id, item_val, ir, ctx);
            return quote! { #s #r };
        }
    }
    // Skip(Ref, structural): unwrap Ref, then emit structural.
    if let IrNode::Skip(kept, structural) = node {
        if let IrNode::Ref(rule_id) = kept.as_ref() {
            let r = emit_ref_item(*rule_id, item_val, ir, ctx);
            let s = emit_structural(structural, ir);
            return quote! { #r #s };
        }
    }
    emit_node(node, item_val, ir, ctx)
}

fn emit_ref_item(
    rule_id: u32,
    item_val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    let ref_rule = &ir.rules[rule_id as usize];
    if ref_rule.meta.is_transparent {
        return emit_node(&ref_rule.body, item_val, ir, ctx);
    }
    let name = ir.get_string(ref_rule.name);
    let emit_fn = format_ident!("{}_emit", name);
    let variant = format_ident!("{}", name);
    let enum_ident = &ctx.enum_ident;
    let rule_type = ctx.rule_types.get(&ref_rule.id)
        .cloned()
        .unwrap_or_else(|| ctx.enum_type.clone());
    quote! {
        if let #enum_ident::#variant(__ref_inner) = #item_val {
            let __ref: &#rule_type = __ref_inner;
            Self::#emit_fn(__ref, __sink);
        }
    }
}

fn extract_repeat_separator(node: &IrNode, ir: &GrammarIR) -> TokenStream {
    if let IrNode::Repeat { inner, .. } = node {
        if let IrNode::Next(sep, _) = inner.as_ref() {
            return emit_structural(sep, ir);
        }
        if let IrNode::Skip(_, opt) = inner.as_ref() {
            if let IrNode::Repeat { inner: sep, lo: 0, hi: 1 } = opt.as_ref() {
                return emit_structural(sep, ir);
            }
        }
    }
    quote! {}
}

// ─── Map: reverse FnDescriptor ──────────────────────────────────────────────

fn emit_map(
    inner: &IrNode,
    fn_id: u32,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    match &ir.fns[fn_id as usize] {
        FnDescriptor::NumberConvert => quote! { __sink.f64(*#val); },
        FnDescriptor::HexConvert { .. } => quote! {
            { use ::std::fmt::Write as _; let mut __b = String::new();
              let _ = write!(__b, "{:x}", #val); __sink.text(&__b); }
        },
        FnDescriptor::SpanCapture => quote! { __sink.text(#val.as_str()); },
        FnDescriptor::EnumWrap { .. } | FnDescriptor::BoxWrap => {
            emit_node(inner, val, ir, ctx)
        }
        FnDescriptor::Expr { expr, .. } => match expr {
            MapExpr::IntLit(_) | MapExpr::FloatLit(_) | MapExpr::StringLit(_)
            | MapExpr::BoolLit(_) => emit_node(inner, val, ir, ctx),
            MapExpr::Input => emit_node(inner, val, ir, ctx),
            _ => quote! {
                { use ::std::fmt::Write as _; let mut __b = String::new();
                  let _ = write!(__b, "{}", #val); __sink.text(&__b); }
            },
        },
    }
}
