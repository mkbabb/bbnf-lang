//! AltDispatch-shape detector — generalized `Alt(leaf, leaf, …)`
//! byte-dispatcher (AX.W0a.2.b).
//!
//! # Predicate
//!
//! A rule is AltDispatch-shaped when its body is an `Alt` whose every
//! branch is one of:
//!
//! - `Ref(rid)` to a classified rule (Wrap's existing narrow case).
//! - `Regex(sid)` — inline regex match emits the leaf.
//! - `Literal(sid)` — inline literal match emits the leaf (generalizes
//!   Wrap beyond the original `Ref | Regex` bound).
//!
//! # Relation to Wrap
//!
//! `Wrap` predates AltDispatch and admits only `Alt(Ref | Regex)` —
//! the narrow transparent dispatcher canon (JSON `value`, CSS
//! `color`, BBNF `rhs`). AltDispatch is the strict superset: any
//! rule that admits Wrap also admits AltDispatch. The dispatch chain
//! runs Wrap first to preserve the narrow emitter for the canonical
//! cases; AltDispatch captures the broader pattern where at least
//! one branch is a Literal.
//!
//! # Canonical sources
//!
//! - CSS L4 `value = calcFunction | … | string | globalKeyword |
//!   ident | /[^\s;!}]+/` — 18 branches mixing Refs, Literals (from
//!   `-> <const>` annotations that lower to Map-wrapped Literals in
//!   the IR), and a final catch-all Regex.
//! - CSS L4 `keyframeStop = /\d+%/ -> 0u8 | "from" -> 1u8 | "to" ->
//!   2u8` — 3 Map-wrapped leaves: one Regex, two Literals.
//! - CSS L4 `anPlusB = "odd" -> 0u8 | "even" -> 1u8 | /[-+]?\d*n\s*
//!   [+-]\s*\d+/ | /[-+]?\d*n/ | /[-+]?\d+/` — 2 Literal + 3 Regex
//!   branches.
//! - BBNF `type_name = "u8" | "u16" | "u32" | "u64" | "i32" | "i64"
//!   | "f32" | "f64" | "bool" | "usize" | /[_a-zA-Z][_a-zA-Z0-9]*/` —
//!   10 Literals + 1 Regex (structural-mode). In non-structural mode,
//!   prefix-tree factoring collapses keyword sequences into Seq
//!   branches; those Seqs are all-literal and emit as a prefix match
//!   (see [`super::alt_dispatch::branch_is_admissible`]).
//! - EBNF `letter = "A" | "B" | … | "y" | "z"` — 52 single-byte
//!   Literals. Above `keyword::KEYWORD_MAX_BRANCHES`, so Keyword
//!   rejects; AltDispatch admits because every branch is a Literal
//!   leaf.
//!
//! # Exclusion guards
//!
//! AltDispatch rejects when any branch is a nested decision point —
//! an inline Alt, a pair-shaped Seq, a Repeat-rooted body. The
//! emitter's byte-dispatch loop expects every branch to resolve to a
//! first-byte-keyable leaf or a classified-Ref shape fn; arbitrary
//! structural Seqs belong to Flat / Object / Array territory.
//!
//! # Projection
//!
//! Pure structural inspection. No new mining; consults
//! [`GrammarIR::shape_assignments`] to check that every `Ref` branch
//! targets a classified rule.

use crate::passes::inspect::unwrap_map_ow;
use crate::passes::recognizers::shape_dispatch::ShapeTag;
use crate::types::{GrammarIR, IrNode, RuleId};

/// Detect AltDispatch-shape: a generalized leaf-dispatch Alt.
pub fn detect_alt_dispatch(rule_id: RuleId, ir: &GrammarIR) -> bool {
    let rule = &ir.rules[rule_id as usize];
    let body = unwrap_map_ow(&rule.body);
    let IrNode::Alt(branches, _) = body else {
        return false;
    };
    if branches.is_empty() {
        return false;
    }
    branches.iter().all(|b| branch_is_admissible(&b.node, ir))
}

/// Whether a single Alt branch's body is an admissible AltDispatch
/// leaf. Admits:
///
/// - `Ref(rid)` where `rid`'s shape tag is classified.
/// - `Regex(_)` — emits via inline regex match.
/// - `Literal(_)` — emits via inline byte-sequence match.
/// - A Seq whose positions are ALL literals / regexes — covers
///   prefix-tree factoring (BBNF `type_name` Seqs like
///   `Seq(Literal("u"), Alt(Literal("8"), Literal("16"), ...))`).
pub(super) fn branch_is_admissible(node: &IrNode, ir: &GrammarIR) -> bool {
    match unwrap_map_ow(node) {
        IrNode::Ref(rid) => ir.shape_assignments.get(*rid).is_classified(),
        IrNode::Regex(_) | IrNode::Literal(_) => true,
        // Seq / Next / Skip branches are admissible when every position
        // resolves to a Literal / Regex — this is the prefix-tree
        // factoring case (BBNF `type_name`'s `Seq(Literal("u"),
        // Alt(Literal("8"), …))` after optimization).
        IrNode::Seq(children) => children.iter().all(|c| seq_position_is_leafy(c, ir)),
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            seq_position_is_leafy(lhs, ir) && seq_position_is_leafy(rhs, ir)
        }
        // Any other structure (nested Alt, Repeat, etc.) — reject.
        _ => false,
    }
}

/// Whether a position inside a Seq branch is a leaf-like node (Literal
/// / Regex / nested Seq of leaves / Alt of leaves / Ref-to-classified /
/// Repeat of leafy inner).
fn seq_position_is_leafy(node: &IrNode, ir: &GrammarIR) -> bool {
    match unwrap_map_ow(node) {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => true,
        IrNode::Ref(rid) => ir.shape_assignments.get(*rid).is_classified(),
        IrNode::Alt(branches, _) => branches.iter().all(|b| seq_position_is_leafy(&b.node, ir)),
        IrNode::Seq(children) => children.iter().all(|c| seq_position_is_leafy(c, ir)),
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            seq_position_is_leafy(lhs, ir) && seq_position_is_leafy(rhs, ir)
        }
        // AX.W0a.2.b — `Repeat(lo, hi, inner)` is leafy when its
        // inner is itself leafy. Covers CSS `typeSelector = wqName |
        // nsPrefix? , "*"` whose second branch is
        // `Seq(Repeat(0,1,Ref(nsPrefix)), Literal("*"))`.
        IrNode::Repeat { inner, .. } => seq_position_is_leafy(inner, ir),
        _ => false,
    }
}

/// Static assert that the classification gate is consulted.
#[allow(dead_code)]
const _: fn(ShapeTag) -> bool = ShapeTag::is_classified;
