//! Pure structural recognizers used by the factoring pass:
//!
//! - [`is_keyword_node`] — does this node match a keyword shape?
//! - [`starts_with_different_token`] — does this branch start with a
//!   `@token` rule that *isn't* the one we're factoring under?
//! - [`leading_first_set`] — FIRST set of a continuation node.
//! - [`strip_leading_keyword`] — strip the leading keyword from a body
//!   (follows `Ref`s into property-group rules).

use std::collections::HashSet;

use crate::regex_first;
use crate::{CharSet128, IrNode};

/// Check if a node's leading expression is a Ref to a `@token` rule other
/// than `exclude_token_id`.
///
/// Branches that start with a different `@token` rule have distinct scanning
/// semantics (e.g., `selectorSpan` uses `[^{};]+` while `propertyName` uses
/// `[a-zA-Z-]+`). Folding such branches under the wrong token's regex
/// replaces their scanner, causing parse failures.
pub(super) fn starts_with_different_token(
    node: &IrNode,
    exclude_token_id: u32,
    token_rule_ids: &HashSet<u32>,
    rule_bodies: &[(u32, IrNode)],
) -> bool {
    match node {
        IrNode::Ref(id) => {
            if token_rule_ids.contains(id) && *id != exclude_token_id {
                return true;
            }
            // Follow Ref to check if the referenced rule's body starts with a different token.
            if let Some((_, ref_body)) = rule_bodies.iter().find(|(rid, _)| *rid == *id) {
                return starts_with_different_token(
                    ref_body,
                    exclude_token_id,
                    token_rule_ids,
                    rule_bodies,
                );
            }
            false
        }
        IrNode::Seq(children) if !children.is_empty() => {
            starts_with_different_token(&children[0], exclude_token_id, token_rule_ids, rule_bodies)
        }
        IrNode::OptionalWhitespace(inner) => {
            starts_with_different_token(inner, exclude_token_id, token_rule_ids, rule_bodies)
        }
        _ => false,
    }
}

/// Check whether an IR node is a keyword pattern (ident-like literals,
/// regex, or a `Ref` to a rule whose body is a keyword pattern).
pub(super) fn is_keyword_node(
    node: &IrNode,
    strings: &[String],
    rule_bodies: &[(u32, IrNode)],
) -> bool {
    match node {
        IrNode::Literal(sid) => {
            let s = &strings[*sid as usize];
            s.bytes()
                .all(|b| b.is_ascii_alphanumeric() || b == b'-' || b == b'_')
        }
        IrNode::Alt(alts, _) => alts
            .iter()
            .all(|b| is_keyword_node(&b.node, strings, rule_bodies)),
        // Seq of keywords is a keyword (e.g., after prefix factoring:
        // Seq(Literal("b"), Alt(Literal("order-color"), ...)) → "border-color").
        IrNode::Seq(children) => children
            .iter()
            .all(|c| is_keyword_node(c, strings, rule_bodies)),
        IrNode::Regex(_) => true,
        IrNode::Epsilon => true,
        // Follow Refs: check if the referenced rule body is a keyword pattern.
        IrNode::Ref(id) => {
            if let Some((_, ref_body)) = rule_bodies.iter().find(|(rid, _)| *rid == *id) {
                is_keyword_node(ref_body, strings, rule_bodies)
            } else {
                false
            }
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            is_keyword_node(inner, strings, rule_bodies)
        }
        _ => false,
    }
}

/// Strip the leading keyword (`Literal`, Alt-of-Literals, `Regex`, or `Ref`
/// to a keyword rule) from a rule body. Follows `Ref`s to detect keyword
/// patterns in property group rules like `colorProps = "color" |
/// "background-color" | ...`.
pub(super) fn strip_leading_keyword(
    body: &IrNode,
    strings: &[String],
    rule_bodies: &[(u32, IrNode)],
) -> IrNode {
    // Unwrap OW.
    let inner = match body {
        IrNode::OptionalWhitespace(i) => i.as_ref(),
        other => other,
    };

    match inner {
        IrNode::Seq(children) if children.len() >= 2 => {
            if is_keyword_node(&children[0], strings, rule_bodies) {
                if children.len() == 2 {
                    children[1].clone()
                } else {
                    IrNode::Seq(children[1..].to_vec())
                }
            } else {
                body.clone()
            }
        }
        // Body is a single Regex or Literal → continuation is Epsilon.
        IrNode::Regex(_) | IrNode::Literal(_) => IrNode::Epsilon,
        // Body is an Alt of Literals → continuation is Epsilon.
        IrNode::Alt(alts, _) if alts.iter().all(|b| matches!(&b.node, IrNode::Literal(_))) => {
            IrNode::Epsilon
        }
        _ => body.clone(),
    }
}

/// Compute FIRST set for a continuation node.
pub(super) fn leading_first_set(
    node: &IrNode,
    rule_first_sets: &[CharSet128],
    strings: &[String],
) -> Option<CharSet128> {
    match node {
        IrNode::Literal(sid) => {
            let s = &strings[*sid as usize];
            s.bytes().next().map(|b| {
                let mut cs = CharSet128::new();
                cs.add(b);
                cs
            })
        }
        IrNode::Regex(sid) => regex_first::regex_first_chars(&strings[*sid as usize]),
        IrNode::Ref(id) => rule_first_sets.get(*id as usize).cloned(),
        IrNode::Seq(children) if !children.is_empty() => {
            leading_first_set(&children[0], rule_first_sets, strings)
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            leading_first_set(inner, rule_first_sets, strings)
        }
        // Next(a, b) parses a then b, FIRST = FIRST(a).
        IrNode::Next(a, _) => leading_first_set(a, rule_first_sets, strings),
        // Skip(a, b) parses a then b, FIRST = FIRST(a).
        IrNode::Skip(a, _) => leading_first_set(a, rule_first_sets, strings),
        IrNode::Alt(branches, _) if !branches.is_empty() => {
            // Union of all branch FIRST sets.
            let mut result = CharSet128::new();
            for b in branches {
                if let Some(fs) = leading_first_set(&b.node, rule_first_sets, strings) {
                    result.union(&fs);
                } else {
                    return None; // Nullable branch → unknown.
                }
            }
            Some(result)
        }
        IrNode::Epsilon => None, // Nullable.
        _ => None,
    }
}
