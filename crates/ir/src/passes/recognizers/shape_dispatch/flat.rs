//! Flat-shape detector — typed `Seq(literal_head, body+)` with a
//! literal or keyword head.
//!
//! # Predicate
//!
//! A rule is Flat-shaped when its body resolves to a Seq whose
//! leading structural position is a `Literal` (fixed keyword) or a
//! short Alt of literals (keyword set), followed by at least one
//! typed-body position — a Literal pivot (e.g. `:`), a Ref to a
//! typed sub-rule, or a Regex leaf.
//!
//! # Canonical sources
//!
//! - CSS 28 `*Decl` rules per `grammar/css/l4/properties.bbnf:161-197`
//!   — each has a literal property-name head (`"display"`,
//!   `"position"`, …) followed by `":"`, a value-rule repeat,
//!   optional `!important` suffix, optional trailing `;`.
//! - CSS media feature rules (`@media`, `@supports`, …) —
//!   `@`-literal + parenthesised body.
//! - BBNF 7 `*_directive` rules (`@import`, `@recover`, `@pretty`,
//!   `@ws`, `@token`, `@debug`, `@host`) per
//!   `grammar/bbnf/bbnf.bbnf:60-73` — literal directive-name head +
//!   ref-typed body + terminator.
//! - BBNF `rule = lhs, "=" ?w, rhs ?w, ( ";" | "." )` at
//!   `grammar/bbnf/bbnf.bbnf:56` — structural head (`lhs` ref) + `=`
//!   pivot + typed body.
//!
//! # Exclusion guards
//!
//! Flat explicitly excludes:
//!
//! - Bodies whose first position is a structural delimiter (`(`,
//!   `[`, `{`, `"` — those are Wrap / ArgList / Array / String
//!   shapes).
//! - Bodies whose structure is an operator-chain Seq (those are
//!   Pratt).
//! - Bodies whose structure is a Wrap (those are Object / Array).
//! - Single-position Seqs (Scalar / Keyword / String / HRegex catch
//!   those).
//! - Alt-rooted bodies (Keyword / Wrap).
//! - Repeat-rooted bodies (Unordered / Array).
//!
//! # Projection
//!
//! Pure structural inspection of the rule body. No new mining.

use crate::passes::inspect::{single_byte_literal, unwrap_map_ow, unwrap_wrap};
use crate::types::{GrammarIR, IrNode, RuleId};

/// Detect Flat-shape: typed Seq starting with a literal or short
/// keyword Alt, with at least one typed-body position following.
pub fn detect_flat(rule_id: RuleId, ir: &GrammarIR) -> bool {
    let rule = &ir.rules[rule_id as usize];
    let body = unwrap_map_ow(&rule.body);
    classify_flat(body, ir)
}

/// Return true when `node` matches the Flat-shape predicate.
fn classify_flat(node: &IrNode, ir: &GrammarIR) -> bool {
    // Guards: reject shapes we hand to other detectors.
    match node {
        IrNode::Alt(_, _) => return false,
        IrNode::Repeat { .. } => return false,
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Ref(_) => return false,
        _ => {}
    }
    // Reject Wrap-shape first: a `Wrap(open, body, close)` at the
    // outer level is Object / Array territory.
    if unwrap_wrap(node).is_some() {
        return false;
    }

    // Flatten the Seq / Next / Skip chain.
    let mut positions: Vec<&IrNode> = Vec::new();
    flatten_seq(node, &mut positions);
    if positions.len() < 2 {
        return false;
    }

    // Head must be a literal-led name. Accept:
    //   - `Literal(...)` directly (e.g. `"display"`).
    //   - `Alt([Literal, Literal, ...])` — e.g. CSS
    //     `borderWidthDecl`'s `("border-top-width" |
    //     "border-right-width" | ...)` head.
    //   - `Ref(rid)` where `rid`'s body is a literal-led Alt (keyword
    //     set). CSS `colorDecl = colorProps, ":" ...` routes through
    //     `colorProps` which is a keyword-Alt.
    let head = unwrap_map_ow(positions[0]);
    if !head_is_literal_or_kw(head, ir) {
        return false;
    }

    // Disallow a first-position structural opening delimiter — those
    // are Wrap / ArgList / Array territory.
    if let Some(byte) = single_byte_literal(head, ir) {
        if matches!(byte, b'(' | b'[' | b'{' | b'"' | b'\'') {
            return false;
        }
    }

    // At least one typed body position must follow the head.
    // Typed body positions are Ref / Regex / Repeat / Literal pivots
    // / nested Seq.
    positions[1..].iter().any(|pos| {
        let inner = unwrap_map_ow(pos);
        !matches!(inner, IrNode::Epsilon)
    })
}

/// Flatten Next / Skip chains into a positional list. Strips
/// OptionalWhitespace / Map trivia and Epsilon nodes.
fn flatten_seq<'a>(node: &'a IrNode, out: &mut Vec<&'a IrNode>) {
    match unwrap_map_ow(node) {
        IrNode::Seq(children) => {
            for child in children {
                flatten_seq(child, out);
            }
        }
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            flatten_seq(lhs, out);
            flatten_seq(rhs, out);
        }
        IrNode::Epsilon => {}
        other => out.push(other),
    }
}

/// Return true when `node` is a Literal, a Ref to a keyword / literal
/// rule, or a short Alt whose every branch is a Literal. Used to
/// identify a Flat-shape head.
fn head_is_literal_or_kw(node: &IrNode, ir: &GrammarIR) -> bool {
    match unwrap_map_ow(node) {
        IrNode::Literal(_) => true,
        IrNode::Alt(branches, _) => branches
            .iter()
            .all(|b| matches!(unwrap_map_ow(&b.node), IrNode::Literal(_))),
        IrNode::Ref(rid) => {
            let Some(rule) = ir.rules.iter().find(|r| r.id == *rid) else {
                return false;
            };
            let body = unwrap_map_ow(&rule.body);
            matches!(body, IrNode::Literal(_))
                || matches!(
                    body,
                    IrNode::Alt(branches, _)
                        if branches
                            .iter()
                            .all(|b| matches!(unwrap_map_ow(&b.node), IrNode::Literal(_)))
                )
        }
        _ => false,
    }
}
