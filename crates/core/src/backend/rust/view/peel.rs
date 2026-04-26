//! Shared structural-wrapper peel for the Rust view-layer emitters.
//!
//! Several emitters in the view module need to peel through wrapping
//! IR nodes (`Map`, `OptionalWhitespace`, `Skip`, `Next`, `Negate`)
//! to reach the structurally-significant kernel of a rule body.
//! Pre-B5.W0 the function was duplicated as `peel_body` (handling
//! only `Map | OptionalWhitespace`) in `view/value.rs` and
//! `unwrap_structural_wrappers` (handling the full wrapper set) in
//! `view/named_types.rs` and `view/mod.rs`. The two implementations
//! diverged: the value-surface emitter's narrower form misclassified
//! every JSON `Skip` / `Next` body (i.e. `array = "[" >> ... << "]"`)
//! as a `Cursor` variant and emitted an unconditional panic stub.
//!
//! [`unwrap_structural_wrappers`] consolidates the peel into a single
//! routine. Every callsite in the view emitter routes through this
//! module; new wrappers added to the IR extend the peel here once.

use bbnf_ir::IrNode;

/// Strip structural wrappers (`Map`, `OptionalWhitespace`, `Skip`,
/// `Next`, `Negate`) from a node, returning the inner kernel.
///
/// For `Skip` / `Next` the left operand is followed — the right
/// operand is the discarded boundary (closing delimiter, trailing
/// whitespace). The peel is iterative so deeply-nested wrapper
/// chains do not recurse on the host stack.
pub fn unwrap_structural_wrappers(node: &IrNode) -> &IrNode {
    let mut cur = node;
    loop {
        match cur {
            IrNode::Map { inner, .. }
            | IrNode::OptionalWhitespace(inner)
            | IrNode::Negate(inner) => {
                cur = inner.as_ref();
            }
            IrNode::Skip(l, _) | IrNode::Next(l, _) => {
                cur = l.as_ref();
            }
            _ => return cur,
        }
    }
}
