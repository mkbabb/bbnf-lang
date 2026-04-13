//! `compute_structural_bytes` — derives the set of structural bytes from
//! dispatch tables across the grammar.
//!
//! A "structural byte" is any byte value that appears as a delimiter-like
//! entry in an `AltDispatch` table or as a `TokenDispatch` guard byte.
//! For JSON this produces bytes like `{ } [ ] : , "` — the bytes that
//! steer alternation dispatch at grammar-level structural boundaries.
//!
//! Character-class entries (where many bytes in a dispatch table map to
//! the same branch, e.g., digits `0-9` for a number regex) are excluded:
//! they represent lexical categories, not structural dispatch points.
//!
//! When the set is small enough (2..=16 unique bytes, the nibble-LUT
//! limit), codegen can emit a structural pre-scan phase that indexes
//! these bytes across the entire input before recursive descent begins.

use std::collections::BTreeSet;

use crate::{GrammarIR, IrNode};

/// Maximum number of bytes that may map to a single dispatch branch
/// before that branch is considered a character class rather than a
/// structural delimiter. Branches with more entries than this threshold
/// are excluded from the structural byte set.
///
/// Rationale: a single literal like `"{"` produces exactly 1 byte per
/// branch. A keyword like `"true"` also produces 1 byte (`t`). A regex
/// character class like `[0-9]` produces 10 bytes. The threshold of 3
/// accommodates two-byte sign prefixes (`+`, `-`) while excluding
/// character classes.
const CHAR_CLASS_THRESHOLD: usize = 3;

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

/// Recursively walk an `IrNode` tree, extracting structural byte entries
/// from `AltDispatch` tables and `TokenDispatch` guard bytes.
///
/// For `AltDispatch` tables, only bytes from branches with at most
/// `CHAR_CLASS_THRESHOLD` entries are included. This filters out
/// regex character-class entries (e.g., digits `0-9` mapping to a
/// number branch) that would inflate the structural byte set past
/// the nibble-LUT limit.
fn collect_dispatch_bytes(node: &IrNode, bytes: &mut BTreeSet<u8>) {
    match node {
        IrNode::Alt(branches, Some(dispatch)) => {
            // Count how many bytes map to each branch index.
            let mut branch_counts: [u16; 256] = [0; 256];
            for &branch_idx in &dispatch.table {
                if branch_idx != 0xFF {
                    branch_counts[branch_idx as usize] += 1;
                }
            }

            // Only include bytes from branches with few entries
            // (structural delimiters), not character-class branches.
            for (byte_val, &branch_idx) in dispatch.table.iter().enumerate() {
                if branch_idx != 0xFF
                    && (branch_counts[branch_idx as usize] as usize) <= CHAR_CLASS_THRESHOLD
                {
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
            // Guard bytes are single-byte structural delimiters
            // (e.g., `(` for function calls). Include them.
            for arm in arms {
                if let Some(guard) = arm.guard_byte {
                    bytes.insert(guard);
                }
            }
            collect_dispatch_bytes(token, bytes);
            for arm in arms {
                collect_dispatch_bytes(&arm.continuation, bytes);
            }
            collect_dispatch_bytes(fallback, bytes);
        }
        _ => {}
    }
}
