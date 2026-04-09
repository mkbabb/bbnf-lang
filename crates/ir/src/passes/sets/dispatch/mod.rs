//! Pass: dispatch table generation from IR FIRST sets.
//!
//! Annotates `Alt` nodes with byte-level dispatch tables when their branches
//! have pairwise disjoint FIRST sets — turning O(branches) backtracking into
//! O(1) byte-indexed lookup.
//!
//! Sub-modules:
//!
//! - [`domain`] — `DispatchDomain` (CSP lattice domain) wrapping a tri-state
//!   `Unknown / Dispatchable / NonDispatchable` decision.
//! - [`constraint`] — `DisjointConstraint` (CSP constraint) that records each
//!   Alt's pairwise-disjoint check result and propagates it through AC-3.
//! - [`first_set`] — `node_first_set`, `node_first_set_nullable_part`, and
//!   `suffix_follow` — pure FIRST/FOLLOW helpers used by both the eligibility
//!   precompute and the tree-walk annotator.
//! - [`eligibility`] — `precompute_dispatch_eligibility` + `collect_alts` +
//!   `is_pairwise_disjoint`. Phase 1 of the pass: walk every Alt, push it
//!   into a CSP, propagate, return a `NodeId → bool` table.
//! - [`annotate`] — `annotate_node` — recursive tree walk that consumes the
//!   eligibility table and installs `AltDispatch` payloads.
//! - [`build`] — `try_build_dispatch` and `try_build_fallback_dispatch` —
//!   actually construct the 128-byte table from a branch list, including
//!   the FOLLOW-aware nullable-branch dispatch and the typed-branches +
//!   superset-fallback escape hatch.
//!
//! Phase 2 (the tree walk) runs in parallel via `rayon` when there are
//! enough rules to justify the overhead.

mod annotate;
mod build;
mod constraint;
mod domain;
mod eligibility;
mod first_set;

use rayon::prelude::*;

use crate::dag::GrammarDag;
use crate::{CharSet128, GrammarIR};

use self::annotate::annotate_node;
use self::eligibility::precompute_dispatch_eligibility;

/// Generate dispatch tables for all eligible Alt nodes in the IR.
///
/// First runs CSP pre-computation to classify all Alts as dispatchable or not,
/// then walks the tree to annotate eligible Alts with `AltDispatch` tables.
///
/// Uses contextual FOLLOW sets for nullable branch dispatch: within a Seq,
/// a nested Alt uses `FIRST(suffix)` (the first set of the remaining sequence
/// elements), not the rule-level FOLLOW. This prevents incorrect dispatch
/// table entries for nullable branches in non-tail positions.
pub fn generate_dispatch_tables(ir: &mut GrammarIR) {
    // Clone follow sets, strings, and rules metadata to avoid borrow conflicts.
    let follow_sets = ir.follow_sets.clone();
    let strings = ir.strings.clone();
    let rule_metas: Vec<(CharSet128, bool)> = ir
        .rules
        .iter()
        .map(|r| (r.meta.first_set.clone(), r.meta.nullable))
        .collect();

    // Pre-compute dispatch eligibility for all Alt nodes via CSP.
    // The durable DAG is built exactly once per compile before any
    // facts/strategy phase runs, so `ir.dag` is always present.
    let eligibility = {
        let dag = ir
            .dag
            .as_ref()
            .expect("generate_dispatch_tables requires ir.dag — built by pipeline::compile");
        precompute_dispatch_eligibility(ir, dag, &rule_metas, &strings)
    };

    // `ir.dag` is a field of `ir` disjoint from `ir.rules`, so we
    // can hold an immutable borrow of the former while the latter
    // is mutably iterated. Bind it in a narrow scope around the
    // iteration to keep the borrow checker happy under `par_iter_mut`.
    if ir.rules.len() >= 16 {
        // `par_iter_mut` on `ir.rules` requires a 'static-ish
        // closure over `dag`. The split-borrow trick: move the
        // `ir.dag` reference into the closure via a local binding
        // that the borrow checker sees as disjoint.
        let dag: &GrammarDag = ir
            .dag
            .as_ref()
            .expect("generate_dispatch_tables requires ir.dag");
        // Re-borrow rules to sidestep the whole-`ir` conflict.
        let rules = &mut ir.rules;
        rules.par_iter_mut().for_each(|rule| {
            let follow = follow_sets.get(&rule.id);
            annotate_node(
                &mut rule.body,
                follow,
                dag,
                &rule_metas,
                &strings,
                &eligibility,
            );
            if let Some(ref mut recover) = rule.meta.directives.recover {
                annotate_node(recover, follow, dag, &rule_metas, &strings, &eligibility);
            }
        });
    } else {
        let dag: &GrammarDag = ir
            .dag
            .as_ref()
            .expect("generate_dispatch_tables requires ir.dag");
        let rules = &mut ir.rules;
        for rule in rules.iter_mut() {
            let follow = follow_sets.get(&rule.id);
            annotate_node(
                &mut rule.body,
                follow,
                dag,
                &rule_metas,
                &strings,
                &eligibility,
            );
            if let Some(ref mut recover) = rule.meta.directives.recover {
                annotate_node(recover, follow, dag, &rule_metas, &strings, &eligibility);
            }
        }
    }
}
