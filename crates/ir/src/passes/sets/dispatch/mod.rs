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
//! enough rules to justify the overhead. The threshold is deliberately
//! conservative: rayon's per-iteration latch wait dominates the work for
//! grammars under ~100 rules. The post-V profile attributed ~25% of
//! `compile_bbnf` to `LockLatch::wait_and_reset` from this pass on a
//! ~50-rule grammar — far inside the old `>= 16` threshold.

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

/// Minimum rule count before the tree-walk phase parallelizes via rayon.
///
/// Below this, the wait-and-reset cost on the rayon `LockLatch` exceeds
/// the work the dispatch annotator does per rule. CSS L4 (~265 rules)
/// crosses the threshold and benefits; BBNF (~50 rules) stays serial.
/// Tranche W phase 3a will move this constant onto `CostConfig`.
const RAYON_RULE_THRESHOLD: usize = 128;

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
    // Per-rule (first_set, nullable) snapshot. Each entry is 17 bytes; this
    // is a small per-pass allocation that lets the parallel closure read
    // metadata without re-borrowing `ir.rules` mutably.
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
        precompute_dispatch_eligibility(ir, dag, &rule_metas)
    };

    // Split-borrow `ir` so we can mutably iterate `ir.rules` while still
    // reading `ir.dag` / `ir.follow_sets` / `ir.strings` from the closure.
    // Destructuring through `&mut *ir` gives Rust's borrow checker the
    // disjoint-field analysis it needs to allow this.
    let GrammarIR {
        rules,
        dag,
        follow_sets,
        strings,
        ..
    } = &mut *ir;
    let dag: &GrammarDag = dag
        .as_ref()
        .expect("generate_dispatch_tables requires ir.dag");
    let follow_sets = &*follow_sets;
    let strings = &*strings;

    let parallel = rules.len() >= RAYON_RULE_THRESHOLD;

    if parallel {
        rules.par_iter_mut().for_each(|rule| {
            let follow = follow_sets.get(&rule.id);
            annotate_node(
                &mut rule.body,
                follow,
                dag,
                &rule_metas,
                strings,
                &eligibility,
            );
            if let Some(ref mut recover) = rule.meta.directives.recover {
                annotate_node(recover, follow, dag, &rule_metas, strings, &eligibility);
            }
        });
    } else {
        for rule in rules.iter_mut() {
            let follow = follow_sets.get(&rule.id);
            annotate_node(
                &mut rule.body,
                follow,
                dag,
                &rule_metas,
                strings,
                &eligibility,
            );
            if let Some(ref mut recover) = rule.meta.directives.recover {
                annotate_node(recover, follow, dag, &rule_metas, strings, &eligibility);
            }
        }
    }
}
