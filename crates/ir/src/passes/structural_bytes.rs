//! `compute_structural_bytes` — derives the set of structural bytes from
//! dispatch tables across the grammar.
//!
//! A "structural byte" is any byte value that appears as a live entry in an
//! `AltDispatch` table. For JSON this produces `{ } [ ] : , "` — the bytes
//! that steer alternation dispatch. When the set is small enough (≤ 16
//! unique bytes, the nibble-LUT limit), codegen can emit a structural
//! pre-scan phase that indexes these bytes across the entire input before
//! recursive descent begins.

use std::collections::BTreeSet;

use crate::{GrammarIR, IrNode};

/// Walk every rule body, collect bytes from dispatch tables, and store
/// the result as `ir.structural_bytes` when the set is within the
/// nibble-LUT window (2..=16 unique bytes).
pub fn compute_structural_bytes(ir: &mut GrammarIR) {
    let mut byte_set = BTreeSet::new();

    for rule in &ir.rules {
        collect_dispatch_bytes(&rule.body, &mut byte_set);
    }

    // Always include quote byte for quote-parity tracking.
    byte_set.insert(b'"');

    // Gate: only set if ≤16 unique bytes (nibble-LUT limit)
    // and at least 2 bytes (trivial sets not worth pre-scanning).
    if byte_set.len() >= 2 && byte_set.len() <= 16 {
        ir.structural_bytes = Some(byte_set.into_iter().collect());
    }
}

/// Recursively walk an `IrNode` tree, extracting live byte entries from
/// any `AltDispatch` tables encountered.
fn collect_dispatch_bytes(node: &IrNode, bytes: &mut BTreeSet<u8>) {
    match node {
        IrNode::Alt(branches, Some(dispatch)) => {
            for (byte_val, &branch_idx) in dispatch.table.iter().enumerate() {
                if branch_idx != 0xFF {
                    bytes.insert(byte_val as u8);
                }
            }
            for b in branches {
                collect_dispatch_bytes(&b.node, bytes);
            }
        }
        IrNode::Alt(branches, None) => {
            for b in branches {
                collect_dispatch_bytes(&b.node, bytes);
            }
        }
        IrNode::Seq(children) => {
            for c in children {
                collect_dispatch_bytes(c, bytes);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Negate(inner) => collect_dispatch_bytes(inner, bytes),
        IrNode::Map { inner, .. } => collect_dispatch_bytes(inner, bytes),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            collect_dispatch_bytes(a, bytes);
            collect_dispatch_bytes(b, bytes);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            collect_dispatch_bytes(token, bytes);
            for arm in arms {
                collect_dispatch_bytes(&arm.continuation, bytes);
            }
            collect_dispatch_bytes(fallback, bytes);
        }
        _ => {}
    }
}
