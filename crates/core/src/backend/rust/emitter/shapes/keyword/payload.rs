//! Payload-extraction helpers for Keyword-shape rules.
//!
//! Recovers the leading literal byte-sequence from a branch body
//! (mirroring the recognizer's `leading_literal_rec` walk) and extracts
//! per-branch `-> <const>` payloads from the IR's `FnDescriptor` table.

use bbnf_ir::{GrammarIR, IrNode};

use super::super::alt_dispatch::branches::{ScalarPayload, scalar_payload_from_fn};

/// Extract a leading literal byte-sequence from a branch body.
/// Mirrors the recognizer-side `leading_literal_rec` walk so the emitter
/// can cheaply recover the same prefix the keyword detector admitted.
/// Handles Literal / Seq-prefix / Skip / Next / Map /
/// OptionalWhitespace / Ref (one-step), with a simple depth bound to
/// avoid cyclic Ref chains.
pub(super) fn leading_literal_bytes(node: &IrNode, ir: &GrammarIR) -> Option<Vec<u8>> {
    fn rec(
        node: &IrNode,
        ir: &GrammarIR,
        depth: u32,
        visited: &mut std::collections::HashSet<bbnf_ir::RuleId>,
    ) -> Option<Vec<u8>> {
        if depth > 16 {
            return None;
        }
        match node {
            IrNode::Literal(sid) => Some(ir.get_string(*sid).as_bytes().to_vec()),
            IrNode::Seq(children) if !children.is_empty() => {
                rec(&children[0], ir, depth + 1, visited)
            }
            IrNode::Skip(a, _) | IrNode::Next(a, _) => rec(a, ir, depth + 1, visited),
            IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
                rec(inner, ir, depth + 1, visited)
            }
            IrNode::Ref(rid) => {
                if !visited.insert(*rid) {
                    return None;
                }
                let rule = ir.rules.iter().find(|r| r.id == *rid)?;
                rec(&rule.body, ir, depth + 1, visited)
            }
            _ => None,
        }
    }
    let mut visited = std::collections::HashSet::new();
    rec(node, ir, 0, &mut visited)
}

/// Rule-root payload for a single-literal Keyword body wrapped in
/// `Map { fn_id }`. Single-literal bodies (e.g. JSON's
/// `null = "null" -> 0u8`) carry their typed payload at the rule root
/// rather than per-branch — there is no Alt to distinguish branches —
/// so the payload extraction starts from `rule.body` (BEFORE
/// `unwrap_trivia` strips the Map wrapper) and walks down through
/// trivia wrappers identical to the per-branch walk.
///
/// Returns the width-resolved integer when the rule's body is
/// `Map { fn_id }` over `IntLit`; returns `None` for un-mapped bodies and
/// non-integer descriptors.
pub(super) fn rule_root_payload_value(
    rule_body: &IrNode,
    ir: &GrammarIR,
) -> Result<Option<ScalarPayload>, &'static str> {
    fn find_map_fn(node: &IrNode) -> Option<u32> {
        match node {
            IrNode::Map { fn_id, .. } => Some(*fn_id),
            IrNode::OptionalWhitespace(inner) => find_map_fn(inner),
            _ => None,
        }
    }
    let Some(descriptor) = find_map_fn(rule_body).and_then(|fid| ir.fns.get(fid as usize)) else {
        return Ok(None);
    };
    payload_from_fn(descriptor)
}

/// Rule-root bool payload for a single-literal Keyword body wrapped in
/// `Map { fn_id: ... -> true | -> false }`. Mirrors
/// [`alt_branch_bool_payload`] but starts from `rule.body`.
pub(super) fn rule_root_bool_payload(rule_body: &IrNode, ir: &GrammarIR) -> Option<bool> {
    fn find_map_fn(node: &IrNode) -> Option<u32> {
        match node {
            IrNode::Map { fn_id, .. } => Some(*fn_id),
            IrNode::OptionalWhitespace(inner) => find_map_fn(inner),
            _ => None,
        }
    }
    let fid = find_map_fn(rule_body)?;
    let fn_desc = ir.fns.get(fid as usize)?;
    let bbnf_ir::FnDescriptor::Expr { expr, .. } = fn_desc else {
        return None;
    };
    match expr {
        bbnf_ir::MapExpr::BoolLit(value) => Some(*value),
        _ => None,
    }
}

/// Extract the branch's width-resolved integer payload, if any, from its
/// `Map { fn_id }` annotation. Structural branch identity is deliberately
/// absent from this API.
pub(super) fn alt_branch_payload_value(
    branch: &bbnf_ir::AltBranch,
    ir: &GrammarIR,
) -> Result<Option<ScalarPayload>, &'static str> {
    // Walk the branch body for a Map { fn_id } annotation. Descends
    // through OptionalWhitespace / Map chains; Seq branches take the
    // outermost Map — post-factoring the annotation rides the branch
    // root even when the branch body is a Seq prefix-factored down
    // to one residual.
    fn find_map_fn(node: &IrNode) -> Option<u32> {
        match node {
            IrNode::Map { fn_id, .. } => Some(*fn_id),
            IrNode::OptionalWhitespace(inner) => find_map_fn(inner),
            _ => None,
        }
    }
    let Some(descriptor) = find_map_fn(&branch.node).and_then(|fid| ir.fns.get(fid as usize))
    else {
        return Ok(None);
    };
    payload_from_fn(descriptor)
}

/// Return the boolean literal carried by a branch-level `-> true` /
/// `-> false` mapping.
pub(super) fn alt_branch_bool_payload(branch: &bbnf_ir::AltBranch, ir: &GrammarIR) -> Option<bool> {
    fn find_map_fn(node: &IrNode) -> Option<u32> {
        match node {
            IrNode::Map { fn_id, .. } => Some(*fn_id),
            IrNode::OptionalWhitespace(inner) => find_map_fn(inner),
            _ => None,
        }
    }
    let fid = find_map_fn(&branch.node)?;
    let fn_desc = ir.fns.get(fid as usize)?;
    let bbnf_ir::FnDescriptor::Expr { expr, .. } = fn_desc else {
        return None;
    };
    match expr {
        bbnf_ir::MapExpr::BoolLit(value) => Some(*value),
        _ => None,
    }
}

/// Extract an integer payload from a `FnDescriptor` without losing its
/// declared width.
///
/// Delegates to the shared exact scalar decoder, so declared signedness and
/// width remain identical across Keyword, Alt, Wrap, Flat, and HRegex shapes.
fn payload_from_fn(fn_desc: &bbnf_ir::FnDescriptor) -> Result<Option<ScalarPayload>, &'static str> {
    scalar_payload_from_fn(fn_desc)
}
