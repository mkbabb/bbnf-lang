//! AW-III.W6.3 — Disjoint-FIRST miner.
//!
//! Identifies every `IrNode::Alt` whose branches have mutually-
//! disjoint FIRST sets — i.e., each branch starts with bytes that no
//! other branch can match. Such Alts lower to
//! [`DtaState::ClassifyByte`](crate::passes::recognizers::dta::DtaState::ClassifyByte)
//! — a 256-entry LUT dispatched in a single indexed load. The
//! mechanism is general across grammars (CSS `compoundSelector` 5-way,
//! BBNF `directive` `@`-prefix, JSON escape `\`-prefix, Sheets
//! function first-letter all mine the same way); the per-grammar
//! table entries differ because the per-grammar IR differs.
//!
//! # Mining algorithm
//!
//! 1. For each Alt branch, compute its FIRST byte set (single-byte
//!    prefix bytes the branch can start with).
//! 2. If every branch's FIRST set is non-empty AND the pairwise
//!    intersections are all empty, the Alt is disjoint-first.
//! 3. Build the 256-entry table: `table[b]` = branch_idx where
//!    branch `branch_idx`'s FIRST set contains `b`. Unmapped bytes
//!    default to `u8::MAX` (driver sentinel for "no branch admits
//!    this byte").
//!
//! # Output shape
//!
//! `MineOutputs::disjoint_first_tables` — per-Alt-NodeId map of
//! `(branch_idx: u8, table: [u8; 256])`. The codegen emits one
//! `DtaState::ClassifyByte` per table so the walker dispatches with
//! a single indexed load.

use std::collections::HashSet;

use crate::dag::NodeId;
use crate::{AltBranch, GrammarIR, IrNode};

use super::{MineOutputs, RecognizerMineCtx, RecognizerMiner};

/// A mined disjoint-FIRST table for one Alt.
///
/// `table[b]` is the branch discriminant whose FIRST set admits byte
/// `b`, or `u8::MAX` when no branch admits `b`.
#[derive(Clone, Debug)]
pub struct DisjointFirstTable {
    /// 256-entry byte → branch_idx map.
    pub table: [u8; 256],
    /// Number of distinct branches — non-zero when the pass admitted
    /// the Alt (admission fails when any branch has an empty FIRST
    /// set or two branches overlap on any byte).
    pub branch_count: u8,
}

/// Per-Alt-NodeId mined disjoint-FIRST tables.
pub type DisjointFirstMap = std::collections::HashMap<NodeId, DisjointFirstTable>;

/// The disjoint-FIRST mining pass.
pub struct DisjointFirstMiner;

impl RecognizerMiner for DisjointFirstMiner {
    fn inspect(
        &self,
        node: &IrNode,
        node_id: NodeId,
        ctx: &RecognizerMineCtx,
        outputs: &mut MineOutputs,
    ) {
        let IrNode::Alt(branches, _) = node else {
            return;
        };
        if branches.len() < 2 || branches.len() > u8::MAX as usize {
            return;
        }
        if let Some(mined) = mine(branches, ctx.ir) {
            outputs.disjoint_first_tables.insert(node_id, mined);
        }
    }
}

/// Compute the first-byte set of a branch, if determinable.
/// Returns `None` when any byte could be admitted (e.g., unbounded
/// regex without a mined prefix), or when the set is empty (an
/// epsilon-capable branch).
fn branch_first_bytes(node: &IrNode, ir: &GrammarIR) -> Option<HashSet<u8>> {
    match node {
        IrNode::Literal(sid) => {
            let bytes = ir.strings[*sid as usize].as_bytes();
            if bytes.is_empty() {
                None
            } else {
                Some([bytes[0]].into_iter().collect())
            }
        }
        IrNode::Regex(_) => {
            // Regex classes can match any byte; without alphabet
            // refinement we conservatively reject. The pattern-
            // alphabet mining pass (W5-carry) surfaces the tight
            // matchable-byte set; when that's populated this arm can
            // consult it instead of bailing.
            None
        }
        IrNode::Seq(children) if !children.is_empty() => branch_first_bytes(&children[0], ir),
        IrNode::Skip(a, _) | IrNode::Next(a, _) => branch_first_bytes(a, ir),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            branch_first_bytes(inner, ir)
        }
        IrNode::Repeat { inner, lo, .. } if *lo > 0 => branch_first_bytes(inner, ir),
        IrNode::Ref(rule) => {
            // Follow the reference to the target rule's body. Single-
            // level chase; mutually-recursive refs are rare in the
            // disjoint-first admission corpus and the conservative
            // `None` return falls through to AltLinear.
            let target_body = ir
                .rules
                .iter()
                .find(|r| r.id == *rule)
                .map(|r| &r.body);
            match target_body {
                Some(body) => branch_first_bytes(body, ir),
                None => None,
            }
        }
        _ => None,
    }
}

/// Attempt to mine a disjoint-FIRST table. Returns `Some` when every
/// branch has a non-empty FIRST set AND the sets are pairwise-
/// disjoint. Returns `None` otherwise.
fn mine(branches: &[AltBranch], ir: &GrammarIR) -> Option<DisjointFirstTable> {
    let per_branch: Vec<HashSet<u8>> = branches
        .iter()
        .map(|b| branch_first_bytes(&b.node, ir))
        .collect::<Option<Vec<_>>>()?;

    // Every branch must have a non-empty FIRST set.
    if per_branch.iter().any(|s| s.is_empty()) {
        return None;
    }

    // Pairwise disjointness check. O(n²) in branch count; branches
    // are ≤ 256 by the caller's gate so the cost is bounded.
    for i in 0..per_branch.len() {
        for j in (i + 1)..per_branch.len() {
            if !per_branch[i].is_disjoint(&per_branch[j]) {
                return None;
            }
        }
    }

    // Build the 256-entry table. `u8::MAX` = "no branch admits this
    // byte" — the runtime fallback sentinel.
    let mut table = [u8::MAX; 256];
    for (idx, set) in per_branch.iter().enumerate() {
        for &b in set {
            table[b as usize] = idx as u8;
        }
    }

    Some(DisjointFirstTable {
        table,
        branch_count: per_branch.len() as u8,
    })
}
