//! Generic IR child walker.
//!
//! [`visit_children_alt`] descends one level into an [`IrNode`],
//! invoking the caller-supplied closure on every direct child. Used
//! by recognizer mining (see [`crate::passes::recognizers`]) and by
//! the strategy CSP (see [`crate::passes::csp_strategy`]) — every
//! consumer that needs a one-level structural recursion shares this
//! single definition.

use crate::IrNode;

/// Walk the children of an [`IrNode`], invoking `f` on each.
///
/// The match arms cover every IR variant that owns child nodes:
///
/// - `Seq` — every element.
/// - `Alt` — the `node` field of every branch (dispatch metadata is
///   not a child).
/// - `Skip` / `Next` / `Minus` — both operands.
/// - `Repeat` / `Negate` / `OptionalWhitespace` / `Map` — the inner
///   sub-expression.
/// - `TokenDispatch` — the leading token, every arm continuation, and
///   the fallback.
///
/// The leaf variants (`Literal`, `Regex`, `Epsilon`, `Ref`) have no
/// children and are no-ops.
pub fn visit_children_alt(node: &IrNode, mut f: impl FnMut(&IrNode)) {
    match node {
        IrNode::Seq(children) => {
            for c in children {
                f(c);
            }
        }
        IrNode::Alt(branches, _) => {
            for b in branches {
                f(&b.node);
            }
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            f(a);
            f(b);
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => {
            f(inner);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            f(token);
            for arm in arms {
                f(&arm.continuation);
            }
            f(fallback);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}
