//! Wrap-shape and trivial-wrapper unwrappers.
//!
//! - [`unwrap_wrap`] decomposes a `Wrap(open, body, close)` shape spelled
//!   either as `Skip(Next(open, body), close)` or as
//!   `Next(open, Skip(body, close))`.
//! - [`unwrap_map_ow`] strips trivial wrappers ([`IrNode::Map`] and
//!   [`IrNode::OptionalWhitespace`]) without changing the semantic shape.

use crate::IrNode;

/// Decompose a `Wrap(open, middle, close)` shape spelled in either of
/// the two canonical forms:
///
/// - `Skip(Next(open, middle), close)` — left-associative: `(open >> middle) << close`
/// - `Next(open, Skip(middle, close))` — right-associative: `open >> (middle << close)`
///
/// Returns `None` when the node is neither shape.
pub fn unwrap_wrap(node: &IrNode) -> Option<(&IrNode, &IrNode, &IrNode)> {
    match node {
        IrNode::Skip(left, right) => {
            if let IrNode::Next(open, middle) = left.as_ref() {
                return Some((open, middle, right));
            }
            None
        }
        IrNode::Next(left, right) => {
            if let IrNode::Skip(middle, close) = right.as_ref() {
                return Some((left, middle, close));
            }
            None
        }
        _ => None,
    }
}

/// Strip trivial wrappers ([`IrNode::Map`] and
/// [`IrNode::OptionalWhitespace`]) from a node, returning the
/// underlying inner node.
///
/// Both wrappers preserve the structural shape — `Map` only attaches
/// projection metadata, and `OptionalWhitespace` only inserts ws
/// trimming — so callers that match on shape can look through them
/// freely.
pub fn unwrap_map_ow(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => unwrap_map_ow(inner),
        other => other,
    }
}
