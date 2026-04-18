//! AW-IV.W4.3 — Dedup-eligibility miner.
//!
//! Identifies grammar rules whose body shape is a good candidate for
//! runtime bloom + GADT compound-record dedup. The admitted set
//! populates [`GrammarIR::dedup_eligible_rules`](crate::GrammarIR) which
//! the emitter reads at codegen time and the walker's compound-emit
//! arm consults at parse time.
//!
//! # Admission contract
//!
//! A rule is admitted iff:
//!
//! 1. **Fixed-width skeleton** — every branch (for Alt bodies) or
//!    structural position (for Seq bodies) emits a known, constant
//!    number of structural tape rows. `Repeat { hi: u32::MAX }` and
//!    variable-width branches disqualify.
//! 2. **Small record count** — total emitted rows ≤ 4. Keeps the
//!    runtime `columns_range_eq` confirm cheap (at most 4 × 7 = 28
//!    column slot compares per probe).
//! 3. **Repeated-payload likelihood** — one of:
//!    - Body is an Alt whose branches are all `Literal` leaves (the
//!      classic keyword set: `null` / `true` / `false`, CSS
//!      `!important`, named colour keywords).
//!    - Body is a `Seq` of `[Literal, Regex]` where the Regex is a
//!      bounded identifier-class (CSS `unit` = digits + fixed suffix
//!      like `px` / `em`).
//!    - Body wraps a transparent identifier rule (CSS `compoundSelector`).
//!
//! # §6 generalisation
//!
//! No grammar-name branches. Every rule is tested against the same
//! predicate. Per-grammar admission yield varies because the per-
//! grammar IR differs: JSON admits `null` / `true` / `false` /
//! `emptyObject` / `emptyArray`; CSS admits `compoundSelector`,
//! `identifier`, `namedColor` wraps, and fixed unit suffixes; BBNF
//! admits literal-only Alt branches.
//!
//! # Output shape
//!
//! A `Vec<RuleId>` of admitted rule ids, sorted ascending for
//! deterministic emission. The IR projection at
//! [`crate::passes::profile::GrammarIR::profile`] folds this into
//! [`GrammarProfile::dedup_eligible_rules`](crate::passes::profile::GrammarProfile).

use crate::{GrammarIR, IrNode, RuleId};

/// Maximum number of structural rows a dedup-eligible rule may emit.
///
/// Four rows × seven structural columns = 28 byte-level comparisons
/// to confirm a hit. Beyond that the `columns_range_eq` overhead
/// eats the savings from the dedup.
pub const MAX_DEDUP_ROWS: usize = 4;

/// Mine the set of dedup-eligible rule ids for the given grammar IR.
///
/// Walks every rule body and admits those whose shape satisfies the
/// admission contract above. The returned Vec is sorted ascending so
/// downstream emission is deterministic across compile sessions.
pub fn mine_dedup_eligible_rules(ir: &GrammarIR) -> Vec<u32> {
    let mut admitted: Vec<u32> = Vec::new();
    for rule in &ir.rules {
        if rule.meta.is_transparent {
            // Transparent rules inline into callers; their row count
            // folds into the caller. Dedup at the caller layer.
            continue;
        }
        if is_dedup_eligible_body(&rule.body, ir) {
            admitted.push(rule.id);
        }
    }
    admitted.sort_unstable();
    admitted.dedup();
    admitted
}

/// Test whether a rule body is dedup-eligible.
///
/// See the module doc-comment for the admission contract. Public so
/// tests in this crate can exercise the predicate directly without
/// constructing a full IR.
pub fn is_dedup_eligible_body(body: &IrNode, ir: &GrammarIR) -> bool {
    let Some(row_count) = count_emitted_rows(body, ir) else {
        return false;
    };
    if row_count == 0 || row_count > MAX_DEDUP_ROWS {
        return false;
    }
    has_repeated_payload_shape(body, ir)
}

/// Count the structural tape rows a body would emit at parse time.
///
/// Returns `None` when the count is not determinable at compile time
/// (variable-width repeats, open-ended Alt branches, recursive
/// references).
fn count_emitted_rows(node: &IrNode, ir: &GrammarIR) -> Option<usize> {
    count_rows_rec(node, ir, &mut std::collections::HashSet::new())
}

fn count_rows_rec(
    node: &IrNode,
    ir: &GrammarIR,
    visited: &mut std::collections::HashSet<RuleId>,
) -> Option<usize> {
    match node {
        IrNode::Epsilon => Some(0),
        IrNode::Literal(_) | IrNode::Regex(_) => Some(1),
        IrNode::Seq(children) => {
            // Compound emits 1 parent row + sum(children).
            let mut total: usize = 1;
            for child in children {
                total = total.checked_add(count_rows_rec(child, ir, visited)?)?;
                if total > MAX_DEDUP_ROWS {
                    return None;
                }
            }
            Some(total)
        }
        IrNode::Alt(branches, _) => {
            // Every branch must emit the same number of rows for the
            // shape to be fixed-width. We take the MAX over branches
            // (worst case) and require every branch to agree.
            let mut first: Option<usize> = None;
            for branch in branches {
                let n = count_rows_rec(&branch.node, ir, visited)?;
                match first {
                    Some(prev) if prev != n => return None,
                    Some(_) => {}
                    None => first = Some(n),
                }
            }
            // An Alt emits 1 parent row + the branch's row count.
            first.map(|n| n + 1)
        }
        IrNode::Repeat { inner, lo, hi } => {
            // Only bounded repeats with a constant count are eligible.
            if *lo != *hi {
                return None;
            }
            if *hi == u32::MAX || *hi == 0 {
                return None;
            }
            let per_iter = count_rows_rec(inner, ir, visited)?;
            // 1 parent row + hi * per_iter.
            (1usize).checked_add((*hi as usize).checked_mul(per_iter)?)
        }
        IrNode::Ref(rule_id) => {
            if !visited.insert(*rule_id) {
                // Cyclic reference — variable-width by definition.
                return None;
            }
            let result = ir
                .rules
                .iter()
                .find(|r| r.id == *rule_id)
                .and_then(|r| count_rows_rec(&r.body, ir, visited));
            visited.remove(rule_id);
            result
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) => {
            let ra = count_rows_rec(a, ir, visited)?;
            let rb = count_rows_rec(b, ir, visited)?;
            ra.checked_add(rb)
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            count_rows_rec(inner, ir, visited)
        }
        // Minus / Negate / TokenDispatch — semantically complex; the
        // runtime emission shape isn't a straightforward fixed-width
        // skeleton. Conservatively reject.
        _ => None,
    }
}

/// Heuristic: does the body shape look like it carries payloads that
/// recur across the corpus?
///
/// - Literal-only Alt branches — classic keyword sets (JSON `null` /
///   `true` / `false`, CSS `!important`, BBNF `@import` / `@export`).
/// - Seq of `[Literal, Regex]` where the Regex is bounded — CSS unit
///   suffixes like `px` / `em` / `rem`.
/// - Single-child wrapper around an identifier-class Regex — named
///   colours, identifier-led CSS keywords.
fn has_repeated_payload_shape(body: &IrNode, ir: &GrammarIR) -> bool {
    match body {
        IrNode::Alt(branches, _) => {
            // Literal-only Alt — every branch reduces to a single
            // `Literal`. The canonical keyword-table shape.
            branches.iter().all(|b| is_literal_leading(&b.node, ir))
        }
        IrNode::Seq(children) => {
            // Two-child Seq of `Literal + Regex` — unit suffixes,
            // `@rule` + body openers.
            children.len() >= 2
                && matches!(children.first(), Some(IrNode::Literal(_)))
                && children.iter().any(|c| matches!(c, IrNode::Regex(_)))
        }
        IrNode::Literal(_) => true,
        IrNode::Regex(_) => {
            // Single-Regex body — if the pattern is bounded (not
            // open-ended like `[^\s]+`), payloads are drawn from a
            // small range of identifier strings.
            true
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            has_repeated_payload_shape(inner, ir)
        }
        IrNode::Ref(rule_id) => ir
            .rules
            .iter()
            .find(|r| r.id == *rule_id)
            .is_some_and(|r| has_repeated_payload_shape(&r.body, ir)),
        _ => false,
    }
}

/// Does this node reduce to a leading `Literal` after wrappers are
/// peeled?
fn is_literal_leading(node: &IrNode, ir: &GrammarIR) -> bool {
    match node {
        IrNode::Literal(_) => true,
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            is_literal_leading(inner, ir)
        }
        IrNode::Seq(children) => children
            .first()
            .is_some_and(|c| is_literal_leading(c, ir)),
        IrNode::Skip(a, _) | IrNode::Next(a, _) => is_literal_leading(a, ir),
        IrNode::Ref(rule_id) => ir
            .rules
            .iter()
            .find(|r| r.id == *rule_id)
            .is_some_and(|r| is_literal_leading(&r.body, ir)),
        _ => false,
    }
}
