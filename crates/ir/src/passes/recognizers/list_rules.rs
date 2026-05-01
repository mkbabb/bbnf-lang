//! AW-IV.W4.4 — Document-parallel fork-candidate miner.
//!
//! # Role
//!
//! The stage-1 structural bitmap (W1 corrected; W1.δ wired) marks every
//! structural boundary in the input. When a grammar's entry rule is a
//! top-level list — `stylesheet = rule*` for CSS, `file = cell*` for
//! Sheets, `grammar = directive* rule*` for BBNF — the input's
//! structural positions partition into contiguous item extents whose
//! parses carry no cross-item state. Handing each contiguous region to
//! a separate worker lets the walker run in parallel; the join phase
//! memcpy-concatenates the per-worker `Columns` and rewrites cross-
//! worker `child_off` offsets by the cumulative pre-worker record
//! count.
//!
//! # Admission
//!
//! A rule is a fork candidate iff:
//!
//! 1. Its body — after stripping transparent wrappers (Map, Skip,
//!    OptionalWhitespace) — is a [`IrNode::Repeat`] whose inner is an
//!    [`IrNode::Alt`] or a single compound (`Ref`, `Seq`).
//! 2. The Repeat's children carry no cross-item state. Satisfied
//!    inductively by the IR shape — the walker's Repeat frame uses a
//!    per-iteration counter + per-iteration savepoint; nothing flows
//!    between siblings beyond the parent frame's span tracking that
//!    the join phase reconstructs by offset rewriting.
//! 3. Each item's byte extent is bounded by a stage-1 structural-
//!    bitmap position. This is guaranteed when the grammar's
//!    structural alphabet covers the item-delimiting bytes — CSS
//!    `{}`, JSON `,`, BBNF `;`, Sheets `\n`. The miner does not
//!    re-check structural coverage here (the W1.2 corrected mining
//!    admits exactly those bytes); it admits every list-shaped rule
//!    and lets the parallel-routing entry point gate on the
//!    stage-1 index at parse time.
//!
//! # Entry-rule restriction
//!
//! The runtime fork dispatches each worker to the grammar's entry
//! walker (`dta_run_<grammar>`), which starts at the entry rule's
//! DTA state. For the worker's shard-parse to be semantically
//! equivalent to parsing a sub-document, the entry rule MUST be a
//! list rule — otherwise a shard wouldn't match the entry's Alt /
//! Seq shape. This miner restricts the admitted set to the entry
//! rule when it matches the list pattern; other list-shaped rules
//! exist (inside the grammar's body) but cannot be the fork point
//! under this architecture (fork-at-sub-rule is an AX successor).
//!
//! # Output
//!
//! [`MineOutputs::list_rules`] — a `Vec<RuleId>` with at most one
//! entry (the entry rule when it admits). The emitter projects this
//! into [`GrammarProfile::list_rules`] and the runtime `parse()`
//! routes through `dta_run_parallel` when the slot is non-empty and
//! `input.len() > parallel_break_even_bytes`.
//!
//! `Columns` and the runtime `child_off` field referenced above live
//! in the generated runtime; this pass produces the IR-side plan only.

use crate::types::{GrammarIR, IrNode};

/// Walk transparent wrappers to reach the structural body.
///
/// `Map`, `Skip`, `OptionalWhitespace`, and top-level `Seq`s whose
/// only non-whitespace child is a Repeat all factor through to the
/// underlying Repeat. The traversal is syntactic — it matches the
/// IR shapes the grammar frontend produces after desugaring `?w`
/// and friends; no DAG consultation required.
fn strip_transparent<'a>(node: &'a IrNode) -> &'a IrNode {
    match node {
        IrNode::Map { inner, .. } => strip_transparent(inner),
        IrNode::OptionalWhitespace(inner) => strip_transparent(inner),
        // `a << b` / `a >> b` are asymmetric — Skip keeps the left,
        // Next keeps the right. For list rules wrapped in boundary
        // whitespace (`?w >> stylesheet_body << ?w`), the payload
        // side carries the Repeat.
        IrNode::Skip(lhs, _) => strip_transparent(lhs),
        IrNode::Next(_, rhs) => strip_transparent(rhs),
        // Seq whose children are boundary-only trivia (epsilons,
        // whitespace placeholders) around a single non-trivial node
        // factor to that node. The frontend lowers `?w rule*` to a
        // 2-child Seq; admit when exactly one child is a Repeat.
        IrNode::Seq(children) => {
            let mut non_trivial: Option<&'a IrNode> = None;
            for child in children {
                match child {
                    IrNode::Epsilon | IrNode::OptionalWhitespace(_) => continue,
                    other => {
                        if non_trivial.is_some() {
                            return node;
                        }
                        non_trivial = Some(other);
                    }
                }
            }
            match non_trivial {
                Some(inner) => strip_transparent(inner),
                None => node,
            }
        }
        _ => node,
    }
}

/// Check whether a rule body matches the fork-candidate shape.
///
/// After stripping transparent wrappers, the body must be a
/// [`IrNode::Repeat`] whose inner is admissible — an `Alt`, `Ref`,
/// `Seq`, or a `Map` over any of those. The lo bound is not
/// constrained (both `*` and `+` parse item sequences the fork
/// handles identically); the hi bound must be unbounded (`u32::MAX`)
/// or large enough that realistic inputs don't run out of iteration
/// budget inside a single shard.
fn admits_list_shape(body: &IrNode) -> bool {
    let stripped = strip_transparent(body);
    let IrNode::Repeat { inner, hi, .. } = stripped else {
        return false;
    };
    // Require unbounded iteration. Bounded repeats (e.g., `rule{3}`)
    // aren't list-shaped in the forkable sense — the item count is
    // a grammar-level constraint the shard-parse can't see locally.
    if *hi != u32::MAX {
        return false;
    }
    let inner_stripped = strip_transparent(inner);
    matches!(
        inner_stripped,
        IrNode::Alt(_, _) | IrNode::Ref(_) | IrNode::Seq(_)
    )
}

/// Mine fork-candidate list rules.
///
/// Returns a `Vec<u32>` of rule ids admitted for document-parallel
/// fork. Per the entry-rule restriction described in the module
/// doc-comment, this contains at most one entry — the grammar's
/// entry rule when it matches the list shape.
pub fn mine_list_rules(ir: &GrammarIR) -> Vec<u32> {
    let Some(entry_rule) = ir.rules.iter().find(|r| r.id == ir.entry) else {
        return Vec::new();
    };
    if admits_list_shape(&entry_rule.body) {
        vec![entry_rule.id]
    } else {
        Vec::new()
    }
}
