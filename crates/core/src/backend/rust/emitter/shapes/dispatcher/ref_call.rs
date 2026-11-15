//! Per-Ref value-position routing (AW-V.W5.2 — Approach B).
//!
//! The dispatcher's `__value` body byte-dispatches over Alt branches
//! — it only works when the root rule is `Alt(Ref, Ref, …)` of
//! classified branches (JSON's `value`). Non-Alt-rooted grammars
//! (CSS `stylesheet` Array, Sheets `formula` Flat, BBNF `grammar`
//! Array) cannot route per-Ref recursion through `__value` because
//! the root shape fn IS the target of the delegating `__value` —
//! recursing would loop back into the root.
//!
//! W5.2 resolves the target rule's shape at emission time and emits a
//! direct call to that shape's per-rule fn, inlining through LLVM's
//! per-site specialisation. Every shape emitter that previously
//! called `#dispatcher_ident(input, p, state, builder)?` for a Ref-
//! position recursion now receives the Ref's target `RuleId` at
//! emission time and emits via [`emit_ref_call_shape`].
//!
//! The helpers return `None` when the target is unclassified
//! (`ShapeTag::None`) — in that case the grammar is not admissible
//! for `parse()` routing via the shape dispatcher entrypoint, and
//! [`super::super::has_shape_dispatcher_entrypoint`] gates
//! accordingly.

use bbnf_ir::GrammarIR;
use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
use proc_macro2::TokenStream;
use quote::quote;

use super::support_mod_ident;
use super::symbol_composition::shape_fn_ident;

/// Direct shape Ref-call emitter: resolves `target_rid`'s shape and
/// emits the direct call. Returns `None` when the target is
/// unclassified.
///
/// Used by every shape emitter at value-position Ref sites. The emitted
/// stream is a single expression ending in a `Result`; the caller wraps
/// with `?` or `.map(|_| ...)` as appropriate.
pub fn emit_ref_call_shape(
    grammar_suffix: &str,
    target_rid: bbnf_ir::RuleId,
    ir: &GrammarIR,
) -> Option<TokenStream> {
    let target = ir.rules.iter().find(|r| r.id == target_rid)?;
    let tag = ir.shape_assignments.get(target_rid);
    let shape_name = match tag {
        ShapeTag::Object => "object",
        ShapeTag::Array => "array",
        ShapeTag::String => "string",
        ShapeTag::Number => "number",
        ShapeTag::Keyword => "keyword",
        ShapeTag::Scalar => "scalar",
        ShapeTag::Pratt => "pratt",
        ShapeTag::Unordered => "unordered",
        ShapeTag::ArgList => "arglist",
        ShapeTag::Flat => "flat",
        ShapeTag::Wrap => "wrap",
        ShapeTag::HRegex => "hregex",
        ShapeTag::AltDispatch => "altdispatch",
        ShapeTag::None => return None,
    };
    let target_fn = shape_fn_ident(shape_name, grammar_suffix, ir.get_string(target.name));
    let support_mod = support_mod_ident(grammar_suffix);
    // sk-v27 LW3.ORC2-WS-C (RULINGS 2026-11-14f PERF1D-B2 (c); the rule
    // in the LEDGER): a Ref reads no whitespace of its own. The call
    // site skips only where the grammar's text reads `?w`: around a
    // target whose body is `x ?w` (through `->`), which every shape fn
    // may have unwrapped. Every other `?w` is read where it is written.
    let (pre, post) = if super::super::ws_read::root_reads_ow(&target.body) {
        (
            quote! { let _ = #support_mod::skip_space(input, p, state); },
            quote! {
                if __r.is_ok() {
                    let _ = #support_mod::skip_space(input, p, state);
                }
            },
        )
    } else {
        (quote! {}, quote! {})
    };
    // AX.W0a.2.g — Keyword's signature gained a `state` parameter so
    // Ref-led Alt branches can delegate via this helper. Number keeps
    // the `(input, p, first, builder)` shape since its body never
    // recurses.
    let call = match tag {
        ShapeTag::Number => quote! {
            {
                let __first = input.get(*p).copied()
                    .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                        offset: *p as u32,
                    })?;
                #target_fn(input, p, __first, builder)
            }
        },
        ShapeTag::Keyword => quote! {
            {
                let __first = input.get(*p).copied()
                    .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                        offset: *p as u32,
                    })?;
                #target_fn(input, p, __first, state, builder, cursor)
            }
        },
        _ => quote! {
            #target_fn(input, p, state, builder, cursor)
        },
    };
    let expr = if super::super::ws_read::root_reads_ow(&target.body) {
        quote! {
            {
                #pre
                let __r = #call;
                #post
                __r
            }
        }
    } else {
        quote! { { #call } }
    };
    Some(expr)
}

/// Walk `node` and collect the target `RuleId` of every `Ref(rid)` at a
/// value position reachable from `node`. Used by detector-side
/// admission gates to check whether every value-position Ref in a
/// shape body routes to a classified rule.
///
/// Traverses through Map / OptionalWhitespace / Seq / Next / Skip /
/// Alt / Repeat / Minus / Negate / TokenDispatch — every syntactic
/// wrapper that holds a value-position child. Leaf nodes (Literal /
/// Regex / Epsilon) contribute no Refs.
pub fn collect_value_refs(node: &bbnf_ir::IrNode) -> Vec<bbnf_ir::RuleId> {
    use bbnf_ir::IrNode;
    let mut refs = Vec::new();
    fn walk(node: &IrNode, refs: &mut Vec<bbnf_ir::RuleId>) {
        match node {
            IrNode::Ref(rid) => refs.push(*rid),
            IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
                walk(inner, refs);
            }
            IrNode::Seq(children) => {
                for c in children {
                    walk(c, refs);
                }
            }
            IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
                walk(lhs, refs);
                walk(rhs, refs);
            }
            IrNode::Alt(branches, _) => {
                for b in branches {
                    walk(&b.node, refs);
                }
            }
            IrNode::Repeat { inner, .. } => walk(inner, refs),
            IrNode::Minus(lhs, rhs) => {
                walk(lhs, refs);
                walk(rhs, refs);
            }
            IrNode::Negate(inner) => walk(inner, refs),
            IrNode::TokenDispatch { .. }
            | IrNode::Literal(_)
            | IrNode::Regex(_)
            | IrNode::Epsilon => {}
        }
    }
    walk(node, &mut refs);
    refs
}
