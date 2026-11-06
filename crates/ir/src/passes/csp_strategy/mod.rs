//! Tranche W phase 3b — recognizer-tier strategy CSP.
//!
//! Replaces V.6's `csp_recognizers.rs`. The previous file's name was a
//! lie: it was a deterministic priority cascade with **zero references
//! to `csp_solver::`**. This module fixes that by building a real
//! [`csp_solver::Csp`], adding [`csp_solver::constraint::ImplicationConstraint`]
//! constraints across related decision variables, and solving via
//! [`csp_solver::OptimizationMode::MinimizeCost`] — the **first
//! production use of the cost-optimization API in the bbnf workspace**.
//!
//! # Architecture
//!
//! Each grammar `NodeId` may map to one or more decision variables:
//!
//! - `Alt(node)` → `StrategyDomain` over feasible [`AltMode`] values
//! - `Wrap(node)` → `StrategyDomain` over feasible [`WrapMode`] values
//! - `Engine(node)` → `StrategyDomain` over feasible [`RegexEngine`] values
//!
//! The domains are populated from the upstream
//! `passes::recognizers::mine_recognizers` facts and the
//! `passes::regex_info::compute_regex_info` engine bitsets. Each value
//! carries a cost derived from `CostConfig.strategy_*`. Higher-cost
//! options stay in the domain — the optimizer prunes them via
//! branch-and-bound.
//!
//! # Constraints
//!
//! - **Domain restriction (implicit)** — feasibility comes from
//!   `NodeFacts.recognizer.shape` (which determines which `AltMode` /
//!   `WrapMode` variants are legal) and from
//!   `RegexInfo.feasible_engines`. Variables with a single legal value
//!   become singletons up front.
//! - **`ImplicationConstraint`** — "if `AltMode = ByteDispatch` then
//!   the children's `RegexEngine` must be a one-pass-eligible
//!   variant." Encodes the parent-child compatibility commitment from
//!   the V.10 plan §3b. The constraint substrate already exists in
//!   csp-solver (`csp_solver::constraint::ImplicationConstraint`) —
//!   we wire it directly.
//!
//! # Solve
//!
//! Per-component solve: build one CSP for each connected call-graph
//! component, finalize, then minimize the immutable domain costs. Components
//! bound the search without splitting cross-rule hard constraints.
//!
//! # Tranche AF.3 — per-component CSP solves
//!
//! Pre-AF.3, this module iterated `ir.rules` and solved each rule
//! body as its own isolated `csp_solver::Csp`. The per-rule loop was
//! sufficient under the pre-AF.3 constraint topology — every
//! constraint was intra-rule — but a per-rule solve is intrinsically
//! incapable of carrying cross-rule constraints: a constraint that
//! binds variables in two distinct rule bodies must be inside a
//! single `Csp` instance, or the two rules' decisions drift.
//!
//! AF.3 upgrades the solver to a **per-component** loop. The
//! [`components::partition_by_call_graph`] helper builds the
//! connected-components partition of the rule call graph (two rules
//! share a component iff one transitively references the other via
//! `IrNode::Ref`), and the solver walks components — not rules —
//! building one `Csp` per component and installing every rule in
//! that component's variables into the shared substrate. The active
//! `EnginePropagation` family then binds variables inside the component
//! boundary; shape, layout, dispatch, and token implications remain
//! component-local structural constraints.
//!
//! A component with one rule is still a component; a component with
//! fifty rules is still a component. With zero installed constraints,
//! the solver selects the exact factorized optimum only after proving
//! complete domain and site coverage.
//!
//! The public entry point is [`solve_grammar_components`], replacing
//! the pre-AF.3 `solve_strategy_and_materialization` name; the
//! migration window has closed and the old name is gone.
//!
//! # Cross-rule constraint installation
//!
//! The active constraint owners are `engine`, `shape`, `layout`, `dispatch`,
//! and `token`. Each installs one named family after variables are built and
//! before finalization; the solver owns lifecycle, counting, and reporting.
//!
//! # Failure contract
//!
//! The Y.-1 `SolveConfig::node_budget` guards every component solve.
//! Missing DAG coverage, cancellation, an exhausted budget,
//! unsatisfiability, or an invalid assignment returns
//! [`OptimizationUnavailable`] with every completed component report and the
//! failing report.
//! Component outputs remain private until every component succeeds, so a
//! later failure cannot publish earlier decisions.
//!
//! # Historical context
//!
//! The Tranche X.6 "global CSP batching" experiment tried a single
//! grammar-wide `Csp::new()` and blew up `compile_css_l4` from 9 ms
//! to 94 ms because branch-and-bound explored the full rule cross-
//! product whenever any single constraint fired. Y.5 landed the
//! `UnionFind` substrate as a deferred fix, but left the
//! per-rule loop intact because the constraint topology did not yet
//! need it. AF.3 wires the substrate to the solver: the unit of
//! work is now the component, and the constraint surface is
//! explicitly cross-rule.

pub mod components;
pub mod constraints;

mod domain;
mod report;
mod site;
mod solve;

pub use domain::{
    AltMode, RecognizerDecision, RecognizerDecisionMap, RegexEngine, StrategyDomain, StrategyValue,
    WrapMode,
};
pub use report::{
    ComponentReport, ConstraintCounts, DecisionCounts, DecisionFamily, GrammarSolveOutput,
    ObjectiveProof, OptimizationFailureLocus, OptimizationUnavailable,
    OptimizationUnavailableReason, SolveCompletionStatus,
};
pub use solve::{GrammarSolveControl, solve_grammar_components};

use std::collections::HashMap;

use crate::passes::inspect::visit_children_alt;
use crate::{GrammarIR, IrNode};

// ── Tranche X.8d — per-StringId regex engine projection ───────────────────

/// Project the per-NodeId regex engine decisions from a
/// [`RecognizerDecisionMap`] into a per-`StringId` lookup, grouping
/// all occurrences of the same pattern under one authoritative
/// decision.
///
/// The CSP assigns one `RegexEngine` per canonical `Regex(sid)` `NodeId`,
/// which makes cost minimization tractable; the backend's
/// `scanner_plan::plan_regex_scanner` consumes the result by pattern
/// string. When the same `StringId` has multiple decisions (e.g., the
/// same regex appears in two Seq positions that reach different
/// constraints), this function picks the strongest engine — the one
/// with the lowest tier-priced cost in the strategy-domain builder.
///
/// Returns an empty map when `ir.dag` is absent or the decision map
/// is empty.
pub fn extract_regex_engine_decisions(
    ir: &GrammarIR,
    decisions: &RecognizerDecisionMap,
) -> HashMap<crate::StringId, RegexEngine> {
    let mut out: HashMap<crate::StringId, RegexEngine> = HashMap::new();
    let Some(dag) = ir.dag.as_ref() else {
        return out;
    };

    for rule in &ir.rules {
        project_regex_decisions(&rule.body, ir, dag, decisions, &mut out);
    }

    out
}

fn project_regex_decisions(
    node: &IrNode,
    ir: &GrammarIR,
    dag: &crate::dag::GrammarDag,
    decisions: &RecognizerDecisionMap,
    out: &mut HashMap<crate::StringId, RegexEngine>,
) {
    if let IrNode::Regex(sid) = node {
        if let Some(nid) = dag.node_for(node) {
            if let Some(engine) = decisions.get(&nid).and_then(|d| d.regex_engine.as_ref()) {
                // Prefer the lowest-tier (fastest) engine when the
                // same pattern has multiple per-NodeId decisions.
                match out.get(sid) {
                    Some(existing) if engine_tier(existing) <= engine_tier(engine) => {}
                    _ => {
                        out.insert(*sid, engine.clone());
                    }
                }
            }
        }
    }

    visit_children_alt(node, |child| {
        project_regex_decisions(child, ir, dag, decisions, out)
    });
}

/// Lower tier = cheaper, preferred.
fn engine_tier(e: &RegexEngine) -> u8 {
    match e {
        RegexEngine::FamilyHelper => 0,
        RegexEngine::Memchr1 => 1,
        RegexEngine::Memchr2 => 2,
        RegexEngine::Memchr3 => 3,
        RegexEngine::NibbleLut => 4,
        RegexEngine::OnePass => 5,
        RegexEngine::SmallDfa => 6,
        RegexEngine::Dfa => 7,
    }
}

#[cfg(test)]
#[path = "../../../tests/passes/csp_strategy/mod.rs"]
mod tests;
