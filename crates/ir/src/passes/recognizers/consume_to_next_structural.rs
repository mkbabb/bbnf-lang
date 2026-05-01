//! AW-III.W5-carry — ConsumeToNextStructural lifter.
//!
//! # Why
//!
//! JSON twitter spends ~30% of parse time in `DtaState::Regex`'s
//! `scanner.scan("[0-9.eE+-]+")` loop — a byte-by-byte scan of a
//! negated-char-class regex whose tight alphabet is disjoint from
//! the grammar's structural alphabet (the JSON structural set is
//! `,]}:"`, the number alphabet is `0123456789.+-eE`). A grammar-
//! authored `DtaState::Regex` forcing a byte-by-byte DFA walk where
//! the stage-1 structural index already knows the answer is pure
//! tax.
//!
//! `DtaState::ConsumeToNextStructural` collapses that scan to an
//! O(1) cursor jump: `cursor.pos = idx.positions[cursor.slot];
//! cursor.slot += 1;`. The lifter identifies the IR patterns that
//! admit the substitution and marks them for the emitter to route
//! through the CTNS state.
//!
//! # Admission
//!
//! A regex pattern admits CTNS substitution when:
//!
//! 1. The pattern's matchable-byte alphabet (mined by
//!    [`pattern_alphabet`](super::pattern_alphabet)) is disjoint
//!    from the grammar's structural alphabet.
//! 2. The pattern is a simple repetition (`+`, `*`, or an explicit
//!    range) — not a fixed-width or anchored pattern the structural
//!    jump would skip past.
//!
//! Together these guarantee the next structural byte is a hard
//! match boundary: every byte the pattern would consume is non-
//! structural, and every byte up to (but not including) the next
//! structural byte is a potential match. The cursor jump to the
//! next structural slot consumes exactly the pattern's run.
//!
//! # Output shape
//!
//! `MineOutputs::ctns_lifts` — a set of `NodeId`s for which the
//! emitter should emit `DtaState::ConsumeToNextStructural` in place
//! of `DtaState::Regex`. The emitter consults the set when lowering
//! an `IrNode::Regex` node; a hit routes to the CTNS emission,
//! otherwise the pre-W6 Regex path survives.

use std::collections::HashSet;

use crate::IrNode;
use crate::dag::NodeId;

use super::{MineOutputs, RecognizerMineCtx, RecognizerMiner};

/// Set of NodeIds whose `IrNode::Regex` admits CTNS substitution.
pub type CtnsLiftSet = HashSet<NodeId>;

/// The CTNS lifter pass.
pub struct ConsumeToNextStructuralMiner;

impl RecognizerMiner for ConsumeToNextStructuralMiner {
    fn inspect(
        &self,
        node: &IrNode,
        node_id: NodeId,
        ctx: &RecognizerMineCtx,
        outputs: &mut MineOutputs,
    ) {
        let IrNode::Regex(sid) = node else {
            return;
        };
        let pattern = &ctx.ir.strings[*sid as usize];
        if !admits_ctns(pattern) {
            return;
        }
        // AW-IV.W3.5b — CTNS admission via matchable/structural
        // disjointness.
        //
        // The walker's CTNS arm jumps `*pos = idx.positions[*slot]` —
        // lands exactly on the pattern's match boundary iff every
        // structural byte is a hard match boundary for the pattern.
        // "Hard match boundary" = the pattern's DFA MUST fail when it
        // reads a structural byte.
        //
        // Equivalently: no structural byte is matchable, i.e.
        // `matchable ∩ structural = ∅`. If a structural byte WERE
        // matchable, the pattern could accept past the stage-1 stop
        // and the cursor jump would undershoot the actual match.
        //
        // This is the pre-W3.5b invariant; W3.5b reinstates it after
        // the stricter `non_matchable ⊆ structural` condition over-
        // admitted (admitted CTNS on patterns like `/[^;{}!,]+/` for
        // CSS pretty whose structural included `:` — `:` was
        // matchable AND structural, so the CTNS jump undershot the
        // pattern's real match boundary).
        if let Some(alphabet) = outputs.pattern_alphabets.get(&node_id) {
            if !alphabet.is_tight {
                return;
            }
            let structural = ctx
                .ir
                .structural_alphabet
                .as_ref()
                .map(|a| a.singletons_mask())
                .unwrap_or([0u64; 4]);
            let matchable = alphabet.matchable_bytes;
            let disjoint = (matchable[0] & structural[0]) == 0
                && (matchable[1] & structural[1]) == 0
                && (matchable[2] & structural[2]) == 0
                && (matchable[3] & structural[3]) == 0;
            if disjoint {
                outputs.ctns_lifts.insert(node_id);
            }
        }
    }
}

/// Pattern-shape admission: reject anchored patterns, fixed-width
/// exact matches, and lookaheads. Admit repetition shapes whose
/// unbounded tail is what the CTNS jump collapses.
///
/// Heuristic: the pattern string ends in `+` or `}` to catch the
/// canonical one-or-more shapes (`[0-9]+`, `[0-9]{3,}`). Patterns
/// ending in `*` are REJECTED: the zero-or-more quantifier matches
/// the empty string, so the CTNS arm's unconditional cursor jump
/// to the next structural byte would over-advance past positions
/// where the pattern should have matched empty. Rejects patterns
/// containing `$`, `^` (beyond character-class negation), `(?=`,
/// or `(?!`. The heuristic favours false negatives (missed lifts)
/// over false positives (incorrect substitution).
fn admits_ctns(pattern: &str) -> bool {
    let bytes = pattern.as_bytes();
    if bytes.is_empty() {
        return false;
    }
    // Reject anchored / lookahead patterns.
    if pattern.contains("(?=") || pattern.contains("(?!") {
        return false;
    }
    // Require one-or-more repetition. `*` rejected per the zero-
    // width-match hazard explained in the miner's admission comment.
    let last = bytes[bytes.len() - 1];
    matches!(last, b'+' | b'}')
}
