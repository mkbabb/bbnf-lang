//! Recursive resolvers that look through [`IrNode::Ref`] indirections
//! and trivial wrappers ([`IrNode::Map`] / [`IrNode::OptionalWhitespace`])
//! to find an underlying structural shape.
//!
//! Each resolver carries a `visited: &mut HashSet<RuleId>` cycle
//! guard; entering a rule that is already in the visited set returns
//! `None` so recognizer mining bails on cyclic Ref chains rather than
//! recursing forever.

use std::collections::HashSet;

use crate::{AltBranch, GrammarIR, IrNode, RuleId};

/// Resolve a node through [`IrNode::Ref`] and trivial wrappers
/// ([`IrNode::Map`], [`IrNode::OptionalWhitespace`]) to find the
/// underlying [`IrNode::Seq`] children. Returns `None` when the node
/// does not bottom out in a `Seq`, when a Ref cycle is encountered,
/// or when an opaque variant is hit.
pub fn resolve_to_seq<'a>(
    node: &'a IrNode,
    ir: &'a GrammarIR,
    visited: &mut HashSet<RuleId>,
) -> Option<&'a [IrNode]> {
    match node {
        IrNode::Seq(children) => Some(children),
        IrNode::Ref(rule_id) => {
            if !visited.insert(*rule_id) {
                return None;
            }
            let rule = &ir.rules[*rule_id as usize];
            let result = resolve_to_seq(&rule.body, ir, visited);
            visited.remove(rule_id);
            result
        }
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            resolve_to_seq(inner, ir, visited)
        }
        _ => None,
    }
}

/// Resolve a node to an underlying non-dispatch [`IrNode::Alt`],
/// looking through trivial wrappers, [`IrNode::Ref`] indirections,
/// and one-sided [`IrNode::Skip`] / [`IrNode::Next`] tails.
///
/// Returns `None` when the node bottoms out in something other than
/// an Alt, when an Alt is encountered that already has a dispatch
/// installed (recognizer mining only applies to bare Alts), or when
/// a Ref cycle is encountered.
pub fn unwrap_to_alt<'a>(
    node: &'a IrNode,
    ir: &'a GrammarIR,
    visited: &mut HashSet<RuleId>,
) -> Option<&'a [AltBranch]> {
    match node {
        IrNode::Alt(branches, dispatch) if dispatch.is_none() => Some(branches),
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            unwrap_to_alt(inner, ir, visited)
        }
        IrNode::Ref(rule_id) => {
            if !visited.insert(*rule_id) {
                return None;
            }
            let result = unwrap_to_alt(&ir.rules[*rule_id as usize].body, ir, visited);
            visited.remove(rule_id);
            result
        }
        IrNode::Next(_, b) => unwrap_to_alt(b, ir, visited),
        IrNode::Skip(a, _) => unwrap_to_alt(a, ir, visited),
        _ => None,
    }
}

/// Resolve a node to an underlying zero-bound [`IrNode::Repeat`]
/// (one with `lo == 0`), looking through trivial wrappers,
/// [`IrNode::Ref`] indirections, and one-sided [`IrNode::Skip`] /
/// [`IrNode::Next`] tails. Returns the repeat body's inner node, or
/// `None` when the node does not bottom out in such a Repeat.
pub fn unwrap_to_repeat<'a>(
    node: &'a IrNode,
    ir: &'a GrammarIR,
    visited: &mut HashSet<RuleId>,
) -> Option<&'a IrNode> {
    match node {
        IrNode::Repeat { inner, lo: 0, .. } => Some(inner),
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            unwrap_to_repeat(inner, ir, visited)
        }
        IrNode::Ref(rule_id) => {
            if !visited.insert(*rule_id) {
                return None;
            }
            let result = unwrap_to_repeat(&ir.rules[*rule_id as usize].body, ir, visited);
            visited.remove(rule_id);
            result
        }
        IrNode::Next(_, b) => unwrap_to_repeat(b, ir, visited),
        IrNode::Skip(a, _) => unwrap_to_repeat(a, ir, visited),
        _ => None,
    }
}
