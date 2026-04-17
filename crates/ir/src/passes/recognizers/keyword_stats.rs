//! AW-III.W6.2 — Keyword-stats miner.
//!
//! Walks every IR Alt node and collects the literal-led branches +
//! their per-branch discriminants. The downstream emitter at
//! [`crate::backend::rust::emitter::keyword_dispatch`] consumes the
//! mined `Vec<LiteralBranch>` per rule and emits a PHF lookup table
//! when the branch count crosses a threshold.
//!
//! # §6 generalisation
//!
//! No per-grammar branches. Every Alt is inspected; every branch
//! that starts with a [`IrNode::Literal`] is admitted. The emission
//! threshold lives in the emitter, not the miner — the miner
//! surfaces the raw facts and the emitter decides which Alts are
//! worth materialising.
//!
//! # Output shape
//!
//! `MineOutputs::keyword_branches` — a per-rule map of
//! `rule_id -> Vec<(bytes, branch_idx)>`. The emitter iterates the
//! map and emits one PHF table per rule that passes the branch-count
//! threshold.
//!
//! Rules whose Alt is transparent (inlined into the caller) still
//! mine; their branches surface under the enclosing rule's entry so
//! the PHF dispatch sits on the callable surface.

use crate::dag::NodeId;
use crate::{AltBranch, IrNode};

use super::{MineOutputs, RecognizerMineCtx, RecognizerMiner};

/// One literal-led branch mined from an Alt.
///
/// `branch_idx` is the branch's position in the enclosing Alt — the
/// same index the walker's Alt frame stamps into `variant_idx` on a
/// successful match. The emitter threads this through its PHF table
/// so a hit routes to the correct branch without a second lookup.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KeywordBranch {
    /// Raw UTF-8 bytes of the leading literal.
    pub bytes: Vec<u8>,
    /// Per-branch discriminant (index into the Alt's branch list).
    pub branch_idx: u8,
}

/// Per-Alt-NodeId mined keyword set.
pub type KeywordBranchMap = std::collections::HashMap<NodeId, Vec<KeywordBranch>>;

/// The keyword-stats mining pass.
///
/// Runs as the final miner in the unified `mine_recognizers` walk;
/// writes to `outputs.keyword_branches`.
pub struct KeywordStatsMiner;

impl RecognizerMiner for KeywordStatsMiner {
    fn inspect(
        &self,
        node: &IrNode,
        node_id: NodeId,
        _ctx: &RecognizerMineCtx,
        outputs: &mut MineOutputs,
    ) {
        let IrNode::Alt(branches, _) = node else {
            return;
        };
        let mined = mine_branches(branches, _ctx);
        if !mined.is_empty() {
            outputs.keyword_branches.insert(node_id, mined);
        }
    }
}

/// Extract the leading literal of a branch. Returns `None` when the
/// branch is not literal-led.
///
/// Admits `Literal(sid)` directly, `Seq([Literal(sid), ...])`
/// prefixes (common post-ws-trim lowering), `Map { inner, .. }`
/// where the inner resolves to a literal, and
/// `OptionalWhitespace(inner)` / `Skip` / `Next` wrappers.
///
/// AW-IV.W3.2 — also follows `Ref(rid)` through to the referenced
/// rule's body. Recursive via [`leading_literal_in_rule`] with cycle
/// protection so `a -> b -> a` circular rule refs don't drive the
/// mining into infinite descent. Bridging Refs is essential for
/// grammars like BBNF's `directive`, where each branch is a rule ref
/// to a literal-led sub-rule (`"@import" ?w ...`). Without it the
/// miner sees 0 literal-led branches and no PHF fires.
fn leading_literal(node: &IrNode, ctx: &RecognizerMineCtx) -> Option<Vec<u8>> {
    let mut visited: std::collections::HashSet<crate::RuleId> =
        std::collections::HashSet::new();
    leading_literal_rec(node, ctx, &mut visited)
}

fn leading_literal_rec(
    node: &IrNode,
    ctx: &RecognizerMineCtx,
    visited: &mut std::collections::HashSet<crate::RuleId>,
) -> Option<Vec<u8>> {
    match node {
        IrNode::Literal(sid) => {
            let s = &ctx.ir.strings[*sid as usize];
            Some(s.as_bytes().to_vec())
        }
        IrNode::Seq(children) if !children.is_empty() => {
            leading_literal_rec(&children[0], ctx, visited)
        }
        IrNode::Skip(a, _) | IrNode::Next(a, _) => {
            leading_literal_rec(a, ctx, visited)
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            leading_literal_rec(inner, ctx, visited)
        }
        IrNode::Ref(rid) => {
            if !visited.insert(*rid) {
                // Already traversed this rule in the current descent —
                // cyclic ref chain. Bail rather than recurse forever.
                return None;
            }
            let rule = ctx.ir.rules.iter().find(|r| r.id == *rid)?;
            leading_literal_rec(&rule.body, ctx, visited)
        }
        _ => None,
    }
}

/// Mine every literal-led branch in an Alt. Branches that are not
/// literal-led contribute nothing; the emitter's fallback path
/// handles them via the existing AltLinear lowering.
fn mine_branches(branches: &[AltBranch], ctx: &RecognizerMineCtx) -> Vec<KeywordBranch> {
    let mut out = Vec::new();
    for (idx, branch) in branches.iter().enumerate() {
        if idx > u8::MAX as usize {
            // AltDispatch's table is u8-indexed; beyond 255 branches
            // the discriminant doesn't fit. The miner bails rather
            // than silently truncating so the emitter can diagnose.
            break;
        }
        if let Some(bytes) = leading_literal(&branch.node, ctx) {
            if !bytes.is_empty() {
                out.push(KeywordBranch {
                    bytes,
                    branch_idx: idx as u8,
                });
            }
        }
    }
    out
}
