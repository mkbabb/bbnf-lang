//! Payload-extraction helpers for Keyword-shape rules.
//!
//! Recovers the leading literal byte-sequence from a branch body
//! (mirroring the recognizer's `leading_literal_rec` walk) and extracts
//! per-branch `-> <const>` payloads from the IR's `FnDescriptor` table.

use bbnf_ir::{GrammarIR, IrNode};
use proc_macro2::TokenStream;
use quote::quote;

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

/// Per-branch payload for an Alt-of-literals. Branch index is passed
/// so we can discriminate (e.g. `true`→1, `false`→0).
/// AX.W0a.2.p — extract the branch's typed-byte payload (if any) from
/// its `Map { fn_id }` annotation.
///
/// Returns the `u32` literal value the `-> Nu8` / `-> bool` produces,
/// or `None` when the branch has no payload. Callers emit the arena
/// staging + `push_leaf_with_arena_payload` when `Some`, and a raw
/// `push_leaf_with(.., PayloadData::None)` when `None`.
pub(super) fn alt_branch_payload_value(
    branch: &bbnf_ir::AltBranch,
    ir: &GrammarIR,
) -> Option<TokenStream> {
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
    find_map_fn(&branch.node)
        .and_then(|fid| ir.fns.get(fid as usize))
        .and_then(|fd| payload_from_fn(fd, ir))
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

/// Extract a `u32` payload value from a `FnDescriptor` when possible.
///
/// Handles the simple cases W3.2 admits (single-literal + 2-branch
/// bool). More nuanced payload typing (F64 / U32 / Aggregate) is
/// out-of-scope for Keyword-shape (numbers route through
/// Number-shape; strings route through String-shape).
fn payload_from_fn(fn_desc: &bbnf_ir::FnDescriptor, ir: &GrammarIR) -> Option<TokenStream> {
    use bbnf_ir::{FnDescriptor, MapExpr};
    let FnDescriptor::Expr { expr, .. } = fn_desc else {
        return None;
    };
    match expr {
        MapExpr::BoolLit(b) => {
            let v = if *b { 1u32 } else { 0u32 };
            Some(quote! { #v })
        }
        MapExpr::IntLit(n) => {
            let v = *n as u32;
            Some(quote! { #v })
        }
        MapExpr::StringLit(sid) => {
            // `"null" -> 0u8` can also lower as IntLit; if it lowers
            // as StringLit we conservatively pick 0.
            let _ = sid;
            let _ = ir;
            Some(quote! { 0u32 })
        }
        _ => None,
    }
}
