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
//! emission time and emits via [`emit_ref_call_shape`] /
//! [`emit_ref_call_visitor`].
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
use super::symbol_composition::{shape_fn_ident, visitor_shape_fn_ident};

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
    // AX.W0a.2.s — Rules whose body admits whitespace as a leading
    // byte (CSS `combinator = /\s*>\s*/ | /\s*\+\s*/ | /\s*~\s*/ |
    // /\s+/`) handle whitespace internally. Pre-skipping at the Ref
    // call site erases the significant whitespace the rule needs —
    // `/\s+/` loses its match input and the descendant combinator
    // silently fails (bootstrap.css offset 8163, tailwind.css deep
    // selector sites). Suppress pre-skip when the target's leading
    // byte set includes any ASCII whitespace byte.
    let pre_skip_needed = !target_rule_accepts_leading_ws(&target.body, ir);
    // AX.W0a.2.g — Keyword's signature gained a `state` parameter so
    // Ref-led Alt branches can delegate via this helper. Number keeps
    // the `(input, p, first, builder)` shape since its body never
    // recurses.
    let expr = match tag {
        ShapeTag::Number => quote! {
            {
                let __first = #support_mod::skip_space(input, p, state)
                    .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                        offset: *p as u32,
                    })?;
                #target_fn(input, p, __first, builder)
            }
        },
        ShapeTag::Keyword => quote! {
            {
                let __first = #support_mod::skip_space(input, p, state)
                    .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                        offset: *p as u32,
                    })?;
                #target_fn(input, p, __first, state, builder)
            }
        },
        _ if pre_skip_needed => quote! {
            {
                let _ = #support_mod::skip_space(input, p, state);
                #target_fn(input, p, state, builder)
            }
        },
        _ => quote! {
            {
                #target_fn(input, p, state, builder)
            }
        },
    };
    Some(expr)
}

/// Returns `true` if `body` can match an ASCII-whitespace byte as its
/// first input byte. Used by [`emit_ref_call_shape`] to suppress the
/// pre-skip ws step when the rule itself handles whitespace (CSS
/// `combinator`, where `/\s+/` IS a combinator branch).
///
/// Walks `Alt`, `OptionalWhitespace`, `Map`, and transparent `Seq` /
/// `Next` / `Skip` / `Repeat` structures to find the leading node in
/// each path. `Regex` nodes consult `ir.regex_info[sid].first_chars`;
/// `Literal` nodes check the first byte; `Ref` nodes recurse once
/// into the referenced rule.
fn target_rule_accepts_leading_ws(body: &bbnf_ir::IrNode, ir: &GrammarIR) -> bool {
    // Conservative recursion bound: `Ref` cycles are possible through
    // mutual recursion. One-level follow is sufficient for the CSS
    // `combinator`-shaped case; deeper chains fall back to the
    // existing pre-skip behaviour.
    target_rule_accepts_leading_ws_bounded(body, ir, 3)
}

fn target_rule_accepts_leading_ws_bounded(
    body: &bbnf_ir::IrNode,
    ir: &GrammarIR,
    budget: usize,
) -> bool {
    use bbnf_ir::IrNode;
    if budget == 0 {
        return false;
    }
    match body {
        IrNode::Literal(sid) => {
            let bytes = ir.strings[*sid as usize].as_bytes();
            bytes
                .first()
                .map(|&b| matches!(b, b' ' | b'\t' | b'\n' | b'\r' | 0x0C))
                .unwrap_or(false)
        }
        IrNode::Regex(sid) => {
            if let Some(info) = ir.regex_info.get(sid) {
                // Check whether any whitespace byte is in the
                // first-byte set of the pattern.
                info.first_chars.has(b' ')
                    || info.first_chars.has(b'\t')
                    || info.first_chars.has(b'\n')
                    || info.first_chars.has(b'\r')
            } else {
                false
            }
        }
        IrNode::Alt(branches, _) => branches
            .iter()
            .any(|b| target_rule_accepts_leading_ws_bounded(&b.node, ir, budget)),
        IrNode::Seq(children) => children
            .first()
            .map(|c| target_rule_accepts_leading_ws_bounded(c, ir, budget))
            .unwrap_or(false),
        IrNode::Next(a, _) | IrNode::Skip(a, _) => {
            target_rule_accepts_leading_ws_bounded(a, ir, budget)
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            target_rule_accepts_leading_ws_bounded(inner, ir, budget)
        }
        IrNode::Repeat { inner, lo, .. } if *lo > 0 => {
            target_rule_accepts_leading_ws_bounded(inner, ir, budget)
        }
        IrNode::Ref(rid) => {
            let Some(target) = ir.rules.iter().find(|r| r.id == *rid) else {
                return false;
            };
            target_rule_accepts_leading_ws_bounded(&target.body, ir, budget - 1)
        }
        _ => false,
    }
}

/// Visitor-path Ref-call emitter: resolves `target_rid`'s shape and
/// emits the direct visitor-path call. Returns `None` when the target
/// is unclassified.
pub fn emit_ref_call_visitor(
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
    let target_fn = visitor_shape_fn_ident(shape_name, grammar_suffix, ir.get_string(target.name));
    let support_mod = support_mod_ident(grammar_suffix);
    // AX.W0a.2.g — visitor-path Keyword signature extended with
    // `state` (see direct shape emit_ref_call_shape).
    let expr = match tag {
        ShapeTag::Number => quote! {
            {
                let __first = #support_mod::skip_space(input, p, state)
                    .ok_or(crate::runtime::ParseErr::Syntax {
                        offset: *p as u32, rule: None,
                    })?;
                #target_fn(input, p, __first, visitor)
            }
        },
        ShapeTag::Keyword => quote! {
            {
                let __first = #support_mod::skip_space(input, p, state)
                    .ok_or(crate::runtime::ParseErr::Syntax {
                        offset: *p as u32, rule: None,
                    })?;
                #target_fn(input, p, __first, state, visitor)
            }
        },
        ShapeTag::String => quote! {
            {
                let _ = #support_mod::skip_space(input, p, state);
                #target_fn(input, p, state, visitor, /*is_key=*/ false)
            }
        },
        _ => quote! {
            {
                let _ = #support_mod::skip_space(input, p, state);
                #target_fn(input, p, state, visitor)
            }
        },
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
