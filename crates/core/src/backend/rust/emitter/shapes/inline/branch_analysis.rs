//! First-byte projection, trivia stripping, Seq flattening.
//!
//! Helpers shared by the Alt and TokenDispatch sub-modules to project
//! branch bodies into byte-dispatch arms. `branch_first_bytes`
//! mirrors [`super::super::alt_dispatch::branch_first_bytes`] verbatim
//! so the inline Alt dispatcher and the top-level AltDispatch emitter
//! agree on byte-set projection.

use bbnf_ir::{GrammarIR, IrNode};

/// Compute the first-byte set for a branch body. Returns an empty
/// Vec when the set is unbounded (Regex branches without
/// classification, Refs without `meta.first_set`).
///
/// Mirrors [`super::super::alt_dispatch::branch_first_bytes`] verbatim
/// so the inline Alt dispatcher and the top-level AltDispatch emitter
/// agree on byte-set projection.
pub(super) fn branch_first_bytes(node: &IrNode, ir: &GrammarIR) -> Vec<u8> {
    match unwrap_trivia(node) {
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            if bytes.is_empty() {
                Vec::new()
            } else {
                vec![bytes[0]]
            }
        }
        IrNode::Ref(rid) => {
            let target = match ir.rules.iter().find(|r| r.id == *rid) {
                Some(r) => r,
                None => return Vec::new(),
            };
            target.meta.first_set.iter().collect()
        }
        IrNode::Regex(_) => Vec::new(),
        IrNode::Seq(children) => children
            .first()
            .map(|c| branch_first_bytes(c, ir))
            .unwrap_or_default(),
        IrNode::Next(lhs, _) => branch_first_bytes(lhs, ir),
        IrNode::Skip(lhs, _) => branch_first_bytes(lhs, ir),
        IrNode::Alt(inner_branches, _) => {
            let mut out: std::collections::BTreeSet<u8> = Default::default();
            for b in inner_branches {
                for byte in branch_first_bytes(&b.node, ir) {
                    out.insert(byte);
                }
            }
            out.into_iter().collect()
        }
        _ => Vec::new(),
    }
}

/// Strip Map / OptionalWhitespace trivia.
pub(super) fn unwrap_trivia(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } => unwrap_trivia(inner.as_ref()),
        IrNode::OptionalWhitespace(inner) => unwrap_trivia(inner.as_ref()),
        _ => node,
    }
}

/// Flatten a Seq / Next / Skip chain into its structural children.
/// Used to project a Seq-branch into a sequence of byte-match
/// positions for the prefix-tree-factored keyword pattern.
pub(super) fn flatten<'a>(node: &'a IrNode, out: &mut Vec<&'a IrNode>) {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            flatten(inner, out);
        }
        IrNode::Seq(children) => {
            for c in children {
                flatten(c, out);
            }
        }
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            flatten(lhs, out);
            flatten(rhs, out);
        }
        IrNode::Epsilon => {}
        other => out.push(other),
    }
}
