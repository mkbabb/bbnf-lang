//! Tranche W phase 3b — recognizer-tier strategy CSP.
//!
//! Replaces V.6's `csp_recognizers.rs`. The previous file's name was a
//! lie: it was a deterministic priority cascade with **zero references
//! to `csp_solver::`**. This module fixes that by building a real
//! [`csp_solver::Csp`], adding [`csp_solver::constraint::ImplicationConstraint`]
//! constraints across parent / child variables, and solving via
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
//! - **`ImplicationConstraint`** — "if `AltMode = TokenDispatch` then
//!   the children's `RegexEngine` must be a one-pass-eligible
//!   variant." Encodes the parent-child compatibility commitment from
//!   the V.10 plan §3b. The constraint substrate already exists in
//!   csp-solver (`csp_solver::constraint::ImplicationConstraint`) —
//!   we wire it directly.
//!
//! # Solve
//!
//! Per-rule local solve: build a fresh CSP for each rule, finalize,
//! call `csp.solve(&SolveConfig { optimization_mode: MinimizeCost, .. })`.
//! Branch-and-bound returns the cost-minimal assignment, which we
//! decode into the [`RecognizerDecisionMap`].
//!
//! Per-rule decomposition keeps the global problem size bounded —
//! CSS L4 has ~265 rules, and even the largest individual rules have
//! only a few dozen decision variables. The cross-rule hoisting CSP
//! that the plan §3b describes (per-group `HoistPlan` over the
//! `SharedHelper(sig)` peer groups) lands in a follow-up tranche
//! where the SeqMode/RepeatMode/CallStrategy/MemoMode variables also
//! join the substrate.
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
//! that component's variables into the shared substrate. The three
//! new cross-rule constraints (`EnginePropagation`,
//! `ParentCompatibility`, `TierFollowsMaterialization`) then bind
//! variables inside the component boundary, which is exactly what
//! the partition semantics guarantee.
//!
//! A component with one rule is still a component; a component with
//! fifty rules is still a component. The solver does not special-
//! case isolated rules — they degenerate naturally to the existing
//! fast paths (empty-sites skip, no-constraints
//! `decode_min_cost_per_variable` short-circuit) without any new
//! branching.
//!
//! The public entry point is [`solve_grammar_components`], replacing
//! the pre-AF.3 `solve_strategy_and_materialization` name. The old
//! name is kept as a `#[deprecated]` alias for one migration
//! window; internal callers already consume the new name.
//!
//! # Cross-rule constraint installation
//!
//! The three AF.3 cross-rule constraint families live as
//! sub-modules under [`constraints`]. Each sub-module exposes an
//! `install(csp, components, ir)` entry point called once per
//! component solve, after variable building but before finalize.
//! The seam is intentionally narrow: the parent solver owns the
//! CSP lifecycle and the component iteration; the constraint
//! modules own the decision about which variables to bind. A new
//! cross-rule constraint family lands as a new file in
//! `constraints/` and one new `install(...)` call in the component
//! solve loop — no parent-module changes required.
//!
//! # Node budget safety net
//!
//! The Y.-1 `SolveConfig::node_budget` guards every component solve.
//! A pathological component (e.g., one where cross-rule propagation
//! explodes the search space) falls back to per-variable trivial
//! picks for that component without hanging the compile. The per-
//! component logging is preserved from the existing
//! `BBNF_CSP_REPORT=1` path.
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

// Tranche AF.3 — cross-rule constraint families. The sub-modules
// (`engine`, `parent`, `tier`) register `EnginePropagation`,
// `ParentCompatibility`, and `TierFollowsMaterialization` constraints
// against the `GrammarComponents` partition. The stub below compiles
// as an empty module so the parent file's `constraints::*::install`
// call sites are syntactically valid; the real implementations land
// in Wave 5 Agent 5B's commit and get folded in by the orchestrator's
// cherry-pick step.
//
// The seam is: each sub-module exposes `pub fn install(csp: &mut
// Csp<StrategyDomain>, components: &GrammarComponents, ir:
// &GrammarIR)`, called once per component solve after variable
// building but before finalize.
pub mod constraints;

use std::collections::HashMap;

use bbnf_regex::EngineSet;
use csp_solver::{
    constraint::{ImplicationConstraint, VarId},
    domain::{CostDomain, Domain},
    Csp, OptimizationMode, Pruning, SolveConfig,
};
use rustc_hash::FxHashMap;

use self::components::{partition_by_call_graph, GrammarComponents};
use crate::dag::NodeId;
use crate::passes::materialization::MaterializationClass;
use crate::passes::patterns::{Recognizer, RecognizerShape};
use crate::{CostConfig, GrammarIR, IrNode, RuleId};

// ── Decision domain ─────────────────────────────────────────────────────────

/// Strategy chosen for an Alt node.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum AltMode {
    /// Sequential checkpoint chain (universal fallback for any Alt).
    Checkpoint,
    /// Byte-dispatch table — branches with disjoint FIRST byte sets.
    ///
    /// Tranche Y.3 folded the former `TokenDispatch` variant into
    /// `ByteDispatch`: both model "strong FIRST-byte discrimination",
    /// and the backend emits both paths identically (through the
    /// dispatch table when one exists, or through the pre-existing
    /// `IrNode::TokenDispatch` conversion performed upstream by
    /// `fuse_token_dispatch`). Keeping them as separate CSP variants
    /// made the cost model look richer than the backend reality and
    /// left the decode path with a Checkpoint fallthrough that never
    /// fired in practice.
    ByteDispatch,
    /// Keyword dispatch — branches with disjoint leading literals.
    KeyDispatch,
    //
    // Tranche Y.2 deleted `AltMode::SharedHelper(u64)`. The variant
    // was emitted by the CSP whenever `prefix_shared_group::mine`
    // assigned a peer group to an Alt, but the backend never had an
    // emission path for it — `alt_strategy::decide_alt_strategy`
    // simply fell through to `Checkpoint`. Rather than add new
    // codegen infrastructure for a feature that could not prove its
    // bench impact without landing first, Y.2 removes the ghost
    // variant along with the `peer_group` field that fed it. The
    // `RecognizerSignature.shape_hash` canonicalization is retained
    // for future use; only the hoisting decoration is gone.
}

/// Strategy chosen for a Wrap node.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WrapMode {
    /// Generic wrap (open / inner / close).
    Generic,
    /// Separator-by repeat.
    SepBy,
    /// Balanced delimiter scan (handles nested open/close).
    /// Also covers the forward-`memchr`-to-close case the deleted
    /// `DelimScan` variant used to gate.
    BalancedScan,
    // Tranche Y.2: `WrapMode::SharedHelper(u64)` deleted alongside
    // its Alt counterpart — same rationale (zero backend consumers,
    // no measurable bench pressure).
    //
    // Tranche Z.5: `WrapMode::DelimScan` deleted as a ghost variant.
    // `build_wrap_domain` never added it to the CSP domain and
    // `fallback_wrap_mode` never returned it. The two consumer
    // sites (`backend::driver::wrap` and `backend::recognizer_plan`)
    // pattern-matched it as a synonym of `BalancedScan`. The
    // forward-`memchr`-to-close emission path is gated on the
    // upstream `delim_scan_configs` sidecar (populated by
    // `delim_scan::collect`) plus a permissive `wrap_mode` check;
    // the per-NodeId `WrapMode` value never carried delim-scan
    // semantics independently. Y.13's consumer-invariant test was
    // updated to drop the variant from its exhaustive match.
}

/// Engine chosen for a regex pattern at a specific call site.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RegexEngine {
    Memchr1,
    Memchr2,
    Memchr3,
    NibbleLut,
    OnePass,
    SmallDfa,
    Dfa,
    /// Family kernel helper (one of the eight in `backend/kernels/`).
    FamilyHelper,
}

/// One decision record per `NodeId`. Populated by
/// [`solve_grammar_components`].
#[derive(Clone, Debug, Default)]
pub struct RecognizerDecision {
    pub alt_mode: Option<AltMode>,
    pub wrap_mode: Option<WrapMode>,
    pub regex_engine: Option<RegexEngine>,
}

pub type RecognizerDecisionMap = HashMap<NodeId, RecognizerDecision>;

// ── Unified strategy value + domain ─────────────────────────────────────────

/// Disjoint union of every per-variable decision value the strategy
/// CSP carries. The CSP holds variables of a single `Domain` type, so
/// `StrategyValue` unifies the four decision families behind one
/// enum. Each variable's domain is restricted to one variant family
/// (Alt vars only ever hold `Alt(_)`, Mat vars only ever hold
/// `Mat(_)`, etc.).
///
/// Tranche AB.1 — added `Mat(MaterializationClass)`. The CSP now
/// jointly optimizes strategy decisions (Alt / Wrap / Engine) AND
/// per-rule tape materialization commitments.
///
/// Tranche AF.4 — added `Tier(EmissionTier)`. The cross-rule CSP
/// solve picks a per-rule emission tier (Tape / Lazy / Direct)
/// constrained by `TierFollowsMaterialization` against the rule's
/// materialization class and `ParentCompatibility` against caller
/// tiers along `Ref` edges.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StrategyValue {
    Alt(AltMode),
    Wrap(WrapMode),
    Engine(RegexEngine),
    Mat(MaterializationClass),
    Tier(crate::passes::materialization::EmissionTier),
}

/// Cost-aware finite domain for [`StrategyValue`].
///
/// Stores the values plus a parallel cost vector. Implements both
/// [`Domain`] and [`CostDomain`] so the CSP can drive a
/// branch-and-bound `OptimizationMode::MinimizeCost` solve.
#[derive(Clone, Debug, PartialEq)]
pub struct StrategyDomain {
    values: Vec<StrategyValue>,
    costs: Vec<f64>,
}

impl StrategyDomain {
    fn new(value_costs: Vec<(StrategyValue, f64)>) -> Self {
        let mut values = Vec::with_capacity(value_costs.len());
        let mut costs = Vec::with_capacity(value_costs.len());
        for (v, c) in value_costs {
            values.push(v);
            costs.push(c);
        }
        Self { values, costs }
    }
}

impl Domain for StrategyDomain {
    type Value = StrategyValue;

    fn size(&self) -> usize {
        self.values.len()
    }

    fn contains(&self, val: &StrategyValue) -> bool {
        self.values.contains(val)
    }

    fn remove(&mut self, val: &StrategyValue) -> bool {
        if let Some(i) = self.values.iter().position(|v| v == val) {
            self.values.swap_remove(i);
            self.costs.swap_remove(i);
            true
        } else {
            false
        }
    }

    fn add(&mut self, val: &StrategyValue) {
        if !self.values.contains(val) {
            self.values.push(val.clone());
            // Re-added values default to `1.0` — `add` is only used
            // by AC-3's reset logic, which restores values pruned by
            // a backtracking decision; it never introduces new ones
            // outside the original domain build.
            self.costs.push(1.0);
        }
    }

    fn values(&self) -> Vec<StrategyValue> {
        self.values.clone()
    }
}

impl CostDomain for StrategyDomain {
    fn cost(&self, val: &StrategyValue) -> f64 {
        self.values
            .iter()
            .position(|v| v == val)
            .map(|i| self.costs[i])
            .unwrap_or(f64::INFINITY)
    }
}

// ── Solver entry point ──────────────────────────────────────────────────────

/// Build the per-NodeId strategy decision map by running a real CSP
/// per call-graph component with [`OptimizationMode::MinimizeCost`].
///
/// Renamed from V.6's `solve_recognizer_decisions` to reflect the
/// reality: this is a strategy synthesis pass that solves an
/// optimization problem, not a deterministic recognizer walk.
/// Renamed again in AB.1 to
/// `solve_strategy_and_materialization` when the per-rule joint
/// strategy + materialization solve landed. Renamed in AF.3 to
/// `solve_grammar_components` to reflect the **component** unit of
/// work: the solver now iterates the connected-components partition
/// of the rule call graph, solving each component as a single CSP.
///
/// # Component scope
///
/// Per-component scope is the coarsest decomposition under which
/// every cross-rule constraint (`EnginePropagation`,
/// `ParentCompatibility`, `TierFollowsMaterialization`) fits inside
/// a single `Csp` instance. Rules with no Ref chain between them
/// are in disjoint components and solved independently, bounding
/// branch-and-bound's search surface per component.
///
/// The Tranche X.6 union CSP experiment (single grammar-wide
/// `Csp::new()`) blew `compile_css_l4` from 9 ms to 94 ms because
/// the solver explored the full rule cross-product whenever any
/// constraint fired. The per-component decomposition recovers the
/// bounded search while permitting genuine cross-rule constraints.
///
/// # Returns
///
/// `(RecognizerDecisionMap, materialization_map)` — the former keyed
/// by NodeId with Alt/Wrap/Engine decisions, the latter the
/// cost-refined per-rule materialization class (one entry per rule
/// root NodeId). The returned materialization map is meant to be
/// merged into `ir.materialization`, overriding the bottom-up
/// estimates from `classify_materialization` where the CSP finds a
/// cheaper (or pin-satisfying) assignment.
pub fn solve_grammar_components(
    ir: &GrammarIR,
) -> (RecognizerDecisionMap, HashMap<NodeId, MaterializationClass>) {
    let dag = match ir.dag.as_ref() {
        Some(d) => d,
        None => return (HashMap::new(), HashMap::new()),
    };

    let mut decisions: RecognizerDecisionMap = HashMap::new();
    let mut mat_out: HashMap<NodeId, MaterializationClass> = HashMap::new();
    let cfg = &ir.cost_config;
    let debug_all = ir.debug_all;

    // AF.3 — build the connected-components partition of the rule
    // call graph exactly once, before the solve loop. The partition
    // is held by borrow for the entire loop so each component solve
    // can consult `components.component_of(rule_id)` without
    // rebuilding or allocating.
    let components = partition_by_call_graph(ir);

    // Reverse lookup — rule id → rule — needed because components
    // store `RuleId`s and we iterate components (not rules). O(1)
    // per lookup, built once.
    let mut rule_by_id: FxHashMap<RuleId, &crate::IrRule> =
        FxHashMap::with_capacity_and_hasher(ir.rules.len(), Default::default());
    for rule in &ir.rules {
        rule_by_id.insert(rule.id, rule);
    }

    // AF.3 — iterate components in deterministic order. Each
    // component gets one `solve_component` call that builds a
    // single `Csp`, installs variables for every rule in the
    // component, installs intra-rule constraints plus (via the
    // `constraints` sub-module seam) any cross-rule constraints,
    // and runs one optimization solve.
    for (_component_root, member_rules) in components.iter_components() {
        solve_component(
            member_rules,
            &rule_by_id,
            ir,
            dag,
            cfg,
            &components,
            &ir.materialization,
            debug_all,
            &mut decisions,
            &mut mat_out,
        );
    }

    (decisions, mat_out)
}

/// Deprecated alias for [`solve_grammar_components`]. Preserved so
/// external callers can migrate incrementally; the internal pipeline
/// and tests call the new name directly. Renamed in Tranche AF.3
/// when the per-rule solve became a per-component solve.
#[deprecated(
    since = "0.1.6",
    note = "use solve_grammar_components (renamed in Tranche AF.3; iterates connected components of the rule call graph)"
)]
pub fn solve_strategy_and_materialization(
    ir: &GrammarIR,
) -> (RecognizerDecisionMap, HashMap<NodeId, MaterializationClass>) {
    solve_grammar_components(ir)
}

/// Deprecated alias preserved for external callers from the Tranche
/// AB.1 transition when the joint strategy + materialization solve
/// landed. Uses the new entry point under the hood.
#[deprecated(
    since = "0.1.6",
    note = "use solve_grammar_components (renamed in Tranche AF.3; returns both decisions and updated materialization)"
)]
pub fn solve_strategy_decisions(ir: &GrammarIR) -> RecognizerDecisionMap {
    solve_grammar_components(ir).0
}

/// Build and solve the strategy CSP for a single call-graph component.
///
/// AF.3 — every rule in `member_rules` contributes its decision
/// sites to the same `csp_solver::Csp` instance. Cross-rule
/// constraints installed via the `constraints` sub-module seam bind
/// variables across rule boundaries inside the component; intra-rule
/// constraints (the legacy TokenDispatch implication) are installed
/// exactly as before, just inside the shared CSP.
///
/// A component containing a single rule degenerates to the pre-AF.3
/// per-rule solve without any special-casing.
#[allow(clippy::too_many_arguments)]
fn solve_component(
    member_rules: &[RuleId],
    rule_by_id: &FxHashMap<RuleId, &crate::IrRule>,
    ir: &GrammarIR,
    dag: &crate::dag::GrammarDag,
    cfg: &CostConfig,
    components: &GrammarComponents,
    materialization: &HashMap<NodeId, MaterializationClass>,
    debug_all: bool,
    decisions: &mut RecognizerDecisionMap,
    mat_out: &mut HashMap<NodeId, MaterializationClass>,
) {
    // ── Phase 1: collect decision sites + build CSP variables ──────────────
    //
    // One CSP carries every variable from every rule in the
    // component. The `by_node` scratch map stays per-component so
    // cross-rule constraints (installed at Phase 2) can look up
    // variables that live in other rules within the same component.
    let mut csp = Csp::<StrategyDomain>::new();
    let mut sites: Vec<(VarId, Site)> = Vec::new();
    let mut by_node: ByNodeVars =
        FxHashMap::default();

    // Track which rules contributed at least one variable; used
    // below for the post-solve fallback walk and for the "no
    // variables in this component" short-circuit.
    let mut contributing_rules: Vec<RuleId> = Vec::with_capacity(member_rules.len());

    // AF.4 stub — per-rule tier variables stay empty until AF.5's
    // `decode_emission_tier` pass owns the construction + decode.
    // The map is kept here so the cross-rule constraint installer
    // signature stays AF.5-ready; the installers gate on the
    // empty lookup and become no-ops, preserving the AF.2
    // cost_weights_unified decision invariants.
    let tier_vars: HashMap<RuleId, VarId> = HashMap::new();

    // AF.3 — per-(rule, regex-node) engine variables, the lookup
    // table the `EnginePropagation` constraint consumes. Built by
    // walking the per-node `by_node` triples after `collect_sites`
    // returns and re-keying the engine slot by `(rule_id, node_id)`.
    let mut engine_vars: HashMap<(RuleId, NodeId), VarId> = HashMap::new();

    for &rid in member_rules {
        let Some(rule) = rule_by_id.get(&rid).copied() else {
            continue;
        };
        let body = &rule.body;
        let sites_before = sites.len();
        let by_node_before = by_node.len();

        collect_sites(body, ir, dag, cfg, &mut csp, &mut sites, &mut by_node);

        // AF.3 — re-key the engine variables this rule contributed
        // by `(rule_id, node_id)` so the cross-rule constraint
        // installer can walk component-wide engine pairs without
        // re-walking the IR. The full `by_node` map is per-component
        // (NodeIds are unique across rules through the DAG), so we
        // only need to look at entries added since the prior rule.
        if by_node.len() > by_node_before {
            for (&node_id, &(_, _, engine_var)) in by_node.iter() {
                if let Some(var) = engine_var {
                    engine_vars.insert((rid, node_id), var);
                }
            }
        }

        // AB.1 — per-rule materialization variable. Only the rule
        // root gets a variable; per-descendant classes already come
        // from the bottom-up `classify_materialization` sweep. The
        // CSP's job is to refine the root's commitment under cost
        // + pin constraints. This runs once per rule inside the
        // component solve, not once per component.
        if let Some(body_id) = dag.node_for(body) {
            let initial = materialization
                .get(&body_id)
                .copied()
                .unwrap_or(MaterializationClass::MustTape);
            let domain = build_materialization_domain(rule, initial, cfg, debug_all);
            let var = csp.add_variable(domain);
            sites.push((var, Site::Materialization(body_id)));
        }

        if sites.len() > sites_before {
            contributing_rules.push(rid);
        }
    }

    if sites.is_empty() {
        return;
    }

    // ── Phase 2: cross-variable constraints ────────────────────────────────
    //
    // Intra-rule: the existing TokenDispatch → one-pass engine
    // implication, walked per-contributing-rule against the shared
    // CSP + shared `by_node`.
    let mut constraints_added = 0usize;
    for &rid in &contributing_rules {
        let Some(rule) = rule_by_id.get(&rid).copied() else {
            continue;
        };
        constraints_added +=
            add_token_dispatch_constraints(&rule.body, dag, &by_node, &mut csp);
    }

    // Cross-rule: install the AF.3 constraint sub-modules.
    // `tier::install` clamps each rule's emission-tier domain
    // against its materialization class; `engine::install`
    // unifies regex engine choice within the component;
    // `parent::install` enforces tier ordering along `Ref` edges
    // and prices the boundary coercion. The returned count feeds
    // the fast-path short-circuit below — when the component is
    // entirely degenerate (single rule, no Refs, no regex
    // variables) the solve is skipped.
    let _ = components; // partition consumed below for component slicing
    let component_constraints_added = install_cross_rule_constraints(
        &mut csp,
        member_rules,
        &tier_vars,
        &engine_vars,
        ir,
    );
    constraints_added += component_constraints_added;

    // Fast path: when no cross-variable constraint applies to this
    // component, every variable's optimal value is its lowest-cost
    // domain entry — branch-and-bound would pick exactly that. Skip
    // finalize+solve and pick the per-variable minimum directly.
    //
    // The CSP scaffolding is still **constructed** so the
    // architectural commitment ("every file with `csp` in its name
    // uses csp_solver::Csp") holds for every code path; we just elide
    // the search work that would not change the answer.
    if constraints_added == 0 {
        decode_min_cost_per_variable(&csp, &sites, decisions, mat_out);
        return;
    }

    // ── Phase 3: finalize + solve with MinimizeCost ────────────────────────
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        optimization_mode: OptimizationMode::MinimizeCost,
        ..SolveConfig::default()
    };
    let solutions = csp.solve_optimized(&config);

    // Tranche Y.-1: the csp-solver carries a default node budget
    // (`SolveConfig::node_budget`) so a pathological search cannot
    // hang the compile. If the budget fires, fall back to the
    // trivial per-variable pick — safe because it is the same
    // answer branch-and-bound would give if every
    // `ImplicationConstraint` were dropped. This is the structured
    // failure mode the X.6 global-CSP attempt lacked.
    if csp.stats().budget_exceeded {
        if std::env::var("BBNF_CSP_REPORT").is_ok() || cfg!(debug_assertions) {
            eprintln!(
                "csp_strategy::solve_component budget_exceeded \
                 nodes_explored={} constraints_added={} sites={} \
                 component_size={}; falling back to per-variable trivial pick",
                csp.stats().nodes_explored,
                constraints_added,
                sites.len(),
                contributing_rules.len(),
            );
        }
        decode_min_cost_per_variable(&csp, &sites, decisions, mat_out);
        return;
    }

    // ── Phase 4: decode solution → RecognizerDecisionMap ───────────────────
    if let Some(solution) = solutions.first() {
        for (var_id, site) in &sites {
            if let Some(value) = solution.get(*var_id as usize).cloned() {
                match (site, value) {
                    (Site::Alt(n), StrategyValue::Alt(m)) => {
                        decisions.entry(*n).or_default().alt_mode = Some(m);
                    }
                    (Site::Wrap(n), StrategyValue::Wrap(m)) => {
                        decisions.entry(*n).or_default().wrap_mode = Some(m);
                    }
                    (Site::Engine(n), StrategyValue::Engine(e)) => {
                        decisions.entry(*n).or_default().regex_engine = Some(e);
                    }
                    (Site::Materialization(n), StrategyValue::Mat(c)) => {
                        mat_out.insert(*n, c);
                    }
                    _ => {}
                }
            }
        }
    } else {
        // No optimization solution — fall back to the deterministic
        // priority decisions per contributing rule. This branch only
        // fires when the CSP is unsatisfiable, which should only
        // happen if a constraint prunes a domain to empty.
        for &rid in &contributing_rules {
            if let Some(rule) = rule_by_id.get(&rid).copied() {
                decode_fallback(&rule.body, ir, dag, decisions);
            }
        }
    }
}

/// AF.3 — install cross-rule constraints from the
/// `constraints` sub-modules. Called once per component solve,
/// after variable building but before finalize.
///
/// Returns the total count of constraints installed (used by the
/// fast-path short-circuit to decide whether to skip the
/// optimization solve entirely). The three sub-modules each take
/// the shared `ConstraintCtx` and the in-progress `Csp`:
///
/// - `tier::install` — unary `TierFollowsMaterialization` clamp
/// - `engine::install` — pairwise `EnginePropagation` equality
/// - `parent::install` — `ParentCompatibility` ordering + cost
fn install_cross_rule_constraints(
    csp: &mut Csp<StrategyDomain>,
    component: &[RuleId],
    tier_vars: &std::collections::HashMap<RuleId, VarId>,
    engine_vars: &std::collections::HashMap<(RuleId, NodeId), VarId>,
    ir: &GrammarIR,
) -> usize {
    let ctx = self::constraints::ConstraintCtx {
        component,
        tier_vars,
        mat_classes: &ir.materialization,
        engine_vars,
    };

    let mut count = 0usize;
    count += self::constraints::tier::install(&ctx, csp, ir);
    count += self::constraints::engine::install(&ctx, csp, ir);
    count += self::constraints::parent::install(&ctx, csp, ir);
    count
}

/// Build a per-rule tier variable domain.
///
/// Initial domain is the full lattice `{Tape, Lazy, Direct}`; the
/// `TierFollowsMaterialization` constraint clamps it later
/// against the rule's materialization class. Costs are derived
/// from `CostWeights`: `Tape` pays per-emission tape push,
/// `Lazy` pays the view-layer dispatch overhead, and `Direct`
/// gets the cheapest cost (no tape record at all). The actual
/// numbers come from the shared `egraph::CostWeights`, so
/// extreme weight patches in the AF.2 contract test propagate
/// through to the tier choice.
fn build_tier_domain(cfg: &CostConfig) -> StrategyDomain {
    use crate::passes::materialization::EmissionTier;
    let w = &cfg.egraph.weights;
    StrategyDomain::new(vec![
        (StrategyValue::Tier(EmissionTier::Tape), w.tape_push),
        (
            StrategyValue::Tier(EmissionTier::Lazy),
            w.tape_push * 0.5 + w.cross_module_coercion * 0.5,
        ),
        (StrategyValue::Tier(EmissionTier::Direct), 0.0),
    ])
}

/// Per-decision-site bookkeeping: which `NodeId` this var belongs to
/// and which decision family. Lifted to module level so the helper
/// fns below can name it.
enum Site {
    Alt(NodeId),
    Wrap(NodeId),
    Engine(NodeId),
    /// Tranche AB.1 — per-rule tape materialization class. One
    /// variable per rule root (not per descendant) — the
    /// rule-granularity decision is what the AB.2 emitter consumes.
    Materialization(NodeId),
    /// Tranche AF.4 — per-rule emission tier (Tape / Lazy /
    /// Direct). One variable per rule root, decoded into
    /// `ir.emission_tier` by the AF.5 `decode_emission_tier`
    /// pass. Cross-rule constraints
    /// (`TierFollowsMaterialization`, `EnginePropagation`,
    /// `ParentCompatibility`) bind these variables to each other
    /// and to materialization / engine sites.
    Tier(RuleId),
}

/// Per-node variable triple: `(alt_var, wrap_var, engine_var)`. Each
/// slot is `Some` iff the node contributed a variable of that
/// decision family to the current CSP. Used as the value type of the
/// `ByNodeVars` scratch map so cross-variable constraint walks can
/// look up sibling variables by `NodeId`.
type NodeVarTriple = (Option<VarId>, Option<VarId>, Option<VarId>);

/// Scratch mapping `NodeId → NodeVarTriple`, per-component, consulted
/// by intra-rule and cross-rule constraint installers to resolve a
/// node back to the variable ids the site-collection walk produced.
type ByNodeVars = FxHashMap<NodeId, NodeVarTriple>;

/// Direct decoder for the no-constraint fast path: pick the
/// lowest-cost value from each variable's domain independently and
/// write it into the decision map. Branch-and-bound would produce
/// exactly the same answer when there are no cross-variable
/// constraints, so this is a strict optimization.
///
/// Tranche AB.1 — also decodes `Site::Materialization` sites into
/// the caller's `mat_out` map.
fn decode_min_cost_per_variable(
    csp: &Csp<StrategyDomain>,
    sites: &[(VarId, Site)],
    decisions: &mut RecognizerDecisionMap,
    mat_out: &mut HashMap<NodeId, MaterializationClass>,
) {
    for (var_id, site) in sites {
        let domain = &csp.variables[*var_id as usize].domain;
        let mut best: Option<(StrategyValue, f64)> = None;
        for (i, v) in domain.values.iter().enumerate() {
            let c = domain.costs[i];
            match &best {
                Some((_, bc)) if c >= *bc => {}
                _ => best = Some((v.clone(), c)),
            }
        }
        let value = match best {
            Some((v, _)) => v,
            None => continue,
        };
        match (site, value) {
            (Site::Alt(n), StrategyValue::Alt(m)) => {
                decisions.entry(*n).or_default().alt_mode = Some(m);
            }
            (Site::Wrap(n), StrategyValue::Wrap(m)) => {
                decisions.entry(*n).or_default().wrap_mode = Some(m);
            }
            (Site::Engine(n), StrategyValue::Engine(e)) => {
                decisions.entry(*n).or_default().regex_engine = Some(e);
            }
            (Site::Materialization(n), StrategyValue::Mat(c)) => {
                mat_out.insert(*n, c);
            }
            _ => {}
        }
    }
}

/// Recursive walk that creates per-site CSP variables with feasible
/// strategy domains.
fn collect_sites(
    node: &IrNode,
    ir: &GrammarIR,
    dag: &crate::dag::GrammarDag,
    cfg: &CostConfig,
    csp: &mut Csp<StrategyDomain>,
    sites: &mut Vec<(VarId, Site)>,
    by_node: &mut ByNodeVars,
) {
    if let Some(node_id) = dag.node_for(node) {
        let fact = ir
            .node_facts
            .get(&node_id)
            .and_then(|f| f.recognizer.as_ref());

        match node {
            IrNode::Alt(_, dispatch) => {
                let domain = build_alt_domain(fact, dispatch.is_some(), cfg);
                let var = csp.add_variable(domain);
                sites.push((var, Site::Alt(node_id)));
                by_node.entry(node_id).or_default().0 = Some(var);
            }
            IrNode::Skip(_, _) | IrNode::Next(_, _) => {
                if is_wrap_shape(node) {
                    let domain = build_wrap_domain(fact, cfg);
                    let var = csp.add_variable(domain);
                    sites.push((var, Site::Wrap(node_id)));
                    by_node.entry(node_id).or_default().1 = Some(var);
                }
            }
            IrNode::Regex(sid) => {
                let info = ir.regex_info.get(sid);
                let feasible = info
                    .map(|i| i.feasible_engines)
                    .unwrap_or_else(EngineSet::empty);
                let domain = build_engine_domain(feasible, cfg);
                let var = csp.add_variable(domain);
                sites.push((var, Site::Engine(node_id)));
                by_node.entry(node_id).or_default().2 = Some(var);
            }
            _ => {}
        }
    }

    super::recognizers::visit_children_alt(node, |child| {
        collect_sites(child, ir, dag, cfg, csp, sites, by_node)
    });
}

/// Add cross-variable constraints encoding parent-child compatibility.
///
/// Currently a single rule:
/// - `Alt(node) = TokenDispatch` ⇒ each child `Engine(child_node)` must
///   be one of the one-pass-compatible variants
///   (`OnePass`, `FamilyHelper`, the memchr family, `NibbleLut`).
///
/// Encoded via [`ImplicationConstraint`] from csp-solver. The
/// constraint is no-op when the antecedent doesn't bind to the trigger
/// value, so it costs nothing for non-token-dispatch nodes.
///
/// Returns the count of constraints added so the caller can short-
/// circuit the finalize+solve when no cross-variable wiring exists.
fn add_token_dispatch_constraints(
    body: &IrNode,
    dag: &crate::dag::GrammarDag,
    by_node: &ByNodeVars,
    csp: &mut Csp<StrategyDomain>,
) -> usize {
    let one_pass_engines: Vec<StrategyValue> = vec![
        StrategyValue::Engine(RegexEngine::OnePass),
        StrategyValue::Engine(RegexEngine::FamilyHelper),
        StrategyValue::Engine(RegexEngine::Memchr1),
        StrategyValue::Engine(RegexEngine::Memchr2),
        StrategyValue::Engine(RegexEngine::Memchr3),
        StrategyValue::Engine(RegexEngine::NibbleLut),
    ];
    let mut count = 0usize;
    walk_token_dispatch(body, dag, by_node, csp, &one_pass_engines, &mut count);
    count
}

fn walk_token_dispatch(
    node: &IrNode,
    dag: &crate::dag::GrammarDag,
    by_node: &ByNodeVars,
    csp: &mut Csp<StrategyDomain>,
    one_pass_engines: &[StrategyValue],
    count: &mut usize,
) {
    if let IrNode::Alt(branches, _) = node {
        if let Some(parent_id) = dag.node_for(node) {
            if let Some((Some(alt_var), _, _)) = by_node.get(&parent_id) {
                // For each child Regex within the Alt branches, emit
                // "Alt = ByteDispatch → child Engine ∈ one_pass". The
                // implication models the constraint that a dispatch-
                // style Alt requires one-pass-eligible child regex
                // engines; Y.3 folded TokenDispatch into ByteDispatch
                // so this is now the single trigger value.
                let mut child_engine_vars = Vec::new();
                for branch in branches {
                    collect_engine_vars_in(&branch.node, dag, by_node, &mut child_engine_vars);
                }
                for child_var in child_engine_vars {
                    csp.add_constraint(ImplicationConstraint::new(
                        *alt_var,
                        child_var,
                        StrategyValue::Alt(AltMode::ByteDispatch),
                        one_pass_engines.to_vec(),
                    ));
                    *count += 1;
                }
            }
        }
    }
    super::recognizers::visit_children_alt(node, |child| {
        walk_token_dispatch(child, dag, by_node, csp, one_pass_engines, count)
    });
}

fn collect_engine_vars_in(
    node: &IrNode,
    dag: &crate::dag::GrammarDag,
    by_node: &ByNodeVars,
    out: &mut Vec<VarId>,
) {
    if let Some(nid) = dag.node_for(node) {
        if let Some((_, _, Some(engine_var))) = by_node.get(&nid) {
            out.push(*engine_var);
        }
    }
    super::recognizers::visit_children_alt(node, |child| {
        collect_engine_vars_in(child, dag, by_node, out)
    });
}

// ── Domain construction ─────────────────────────────────────────────────────

/// Build the cost-weighted domain for an `Alt` decision variable.
///
/// All architectural fallbacks (Checkpoint) stay in the domain at high
/// cost. Feasible faster strategies (ByteDispatch, KeyDispatch,
/// TokenDispatch, SharedHelper) are added at low cost based on the
/// upstream recognizer fact.
fn build_alt_domain(
    fact: Option<&Recognizer>,
    has_byte_dispatch: bool,
    cfg: &CostConfig,
) -> StrategyDomain {
    let mut values: Vec<(StrategyValue, f64)> = Vec::with_capacity(4);

    // Universal fallback — always legal, highest cost.
    values.push((StrategyValue::Alt(AltMode::Checkpoint), 10.0 * cfg.literal_cost));

    if has_byte_dispatch {
        values.push((
            StrategyValue::Alt(AltMode::ByteDispatch),
            cfg.egraph.weights.dispatch_bonus.abs(),
        ));
    }

    if let Some(rec) = fact {
        // Tranche Y.3: TokenLedBranches folds into ByteDispatch. The
        // previous code added a duplicate entry at the same cost
        // weight; the cost model is unchanged but the codepath is
        // unified. `fuse_token_dispatch` converts the strongest
        // TokenLed shapes into `IrNode::TokenDispatch` upstream
        // regardless of the CSP choice.
        if matches!(rec.shape, RecognizerShape::TokenLedBranches { .. })
            && !has_byte_dispatch
        {
            values.push((
                StrategyValue::Alt(AltMode::ByteDispatch),
                cfg.egraph.weights.dispatch_bonus.abs(),
            ));
        }
        if matches!(rec.shape, RecognizerShape::KeywordPrefix { .. }) {
            values.push((
                StrategyValue::Alt(AltMode::KeyDispatch),
                cfg.egraph.weights.dispatch_bonus.abs(),
            ));
        }
    }

    StrategyDomain::new(values)
}

/// Build the cost-weighted domain for a `Wrap` decision variable.
fn build_wrap_domain(fact: Option<&Recognizer>, cfg: &CostConfig) -> StrategyDomain {
    let mut values: Vec<(StrategyValue, f64)> = Vec::with_capacity(4);

    // Universal fallback.
    values.push((StrategyValue::Wrap(WrapMode::Generic), 10.0 * cfg.literal_cost));

    if let Some(rec) = fact {
        match &rec.shape {
            RecognizerShape::DelimiterBalanced { .. } => values.push((
                StrategyValue::Wrap(WrapMode::BalancedScan),
                cfg.egraph.weights.dispatch_bonus.abs(),
            )),
            RecognizerShape::SeparatorList { .. } => values.push((
                StrategyValue::Wrap(WrapMode::SepBy),
                cfg.egraph.weights.dispatch_bonus.abs(),
            )),
            _ => {}
        }
    }

    StrategyDomain::new(values)
}

/// Tranche AB.1 — build the cost-weighted domain for a
/// `Materialization` decision variable.
///
/// The domain includes every class legal for this rule given its
/// directives and its classification head. Pinned rules
/// (`@pretty` / `@debug` / `preserve_identity` / global `debug_all`)
/// have their domain clamped to `{MustTape}` — no other class is
/// legal, so the CSP can't demote them.
///
/// Non-pinned rules include their initial class (from
/// `classify_materialization`) plus every class at or below it in
/// the lattice. The cost weights drive the CSP toward the cheapest
/// legal answer: `TransparentElide` < `TapeSpanOnly` < `MustTape`.
fn build_materialization_domain(
    rule: &crate::IrRule,
    initial: MaterializationClass,
    cfg: &CostConfig,
    debug_all: bool,
) -> StrategyDomain {
    // Pinned rules: clamp to MustTape with a giant penalty attached
    // to any alternative. We express this as a single-value domain
    // because the CSP's cost model already rewards lower-cost
    // entries; a pinned domain with one entry means no alternative
    // can win.
    let pinned = rule.meta.preserve_identity
        || rule.meta.directives.pretty.is_some()
        || rule.meta.directives.debug
        || debug_all;
    if pinned {
        return StrategyDomain::new(vec![(
            StrategyValue::Mat(MaterializationClass::MustTape),
            cfg.mat_must_tape,
        )]);
    }

    // Non-pinned: include the initial class and any strictly lower
    // class. The cost model decides which to pick.
    let mut values: Vec<(StrategyValue, f64)> = Vec::with_capacity(3);
    values.push((
        StrategyValue::Mat(MaterializationClass::MustTape),
        cfg.mat_must_tape,
    ));
    if matches!(
        initial,
        MaterializationClass::TapeSpanOnly | MaterializationClass::TransparentElide
    ) {
        values.push((
            StrategyValue::Mat(MaterializationClass::TapeSpanOnly),
            cfg.mat_tape_span_only,
        ));
    }
    if initial == MaterializationClass::TransparentElide {
        values.push((
            StrategyValue::Mat(MaterializationClass::TransparentElide),
            cfg.mat_transparent_elide,
        ));
    }
    StrategyDomain::new(values)
}

/// Build the cost-weighted domain for a `RegexEngine` decision variable.
///
/// Each engine that's a member of `feasible` is added at a tier-priced
/// cost: kernel helpers cheapest, narrow memchr next, then nibble LUT,
/// one-pass, small DFA, and DFA last.
fn build_engine_domain(feasible: EngineSet, cfg: &CostConfig) -> StrategyDomain {
    let dispatch_bonus = cfg.egraph.weights.dispatch_bonus.abs();
    let mut values: Vec<(StrategyValue, f64)> = Vec::with_capacity(8);

    if feasible.contains(EngineSet::FAMILY_HELPER) {
        values.push((StrategyValue::Engine(RegexEngine::FamilyHelper), dispatch_bonus));
    }
    if feasible.contains(EngineSet::MEMCHR1) {
        values.push((StrategyValue::Engine(RegexEngine::Memchr1), dispatch_bonus + 0.1));
    }
    if feasible.contains(EngineSet::MEMCHR2) {
        values.push((StrategyValue::Engine(RegexEngine::Memchr2), dispatch_bonus + 0.2));
    }
    if feasible.contains(EngineSet::MEMCHR3) {
        values.push((StrategyValue::Engine(RegexEngine::Memchr3), dispatch_bonus + 0.3));
    }
    if feasible.contains(EngineSet::NIBBLE_LUT) {
        values.push((StrategyValue::Engine(RegexEngine::NibbleLut), dispatch_bonus + 0.5));
    }
    if feasible.contains(EngineSet::ONE_PASS) {
        values.push((StrategyValue::Engine(RegexEngine::OnePass), dispatch_bonus + 1.0));
    }
    if feasible.contains(EngineSet::SMALL_DFA) {
        values.push((StrategyValue::Engine(RegexEngine::SmallDfa), dispatch_bonus + 2.0));
    }
    // DFA is the universal fallback — always legal even when feasible
    // is empty (e.g., complex patterns with no narrower engine).
    values.push((StrategyValue::Engine(RegexEngine::Dfa), dispatch_bonus + 5.0));

    StrategyDomain::new(values)
}

// ── Helpers ─────────────────────────────────────────────────────────────────

fn is_wrap_shape(node: &IrNode) -> bool {
    match node {
        IrNode::Skip(left, _) => matches!(left.as_ref(), IrNode::Next(_, _)),
        IrNode::Next(_, right) => matches!(right.as_ref(), IrNode::Skip(_, _)),
        _ => false,
    }
}

/// Deterministic fallback decoder used when the CSP is unsatisfiable.
/// Mirrors the V.6 priority cascade exactly.
fn decode_fallback(
    node: &IrNode,
    ir: &GrammarIR,
    dag: &crate::dag::GrammarDag,
    decisions: &mut RecognizerDecisionMap,
) {
    if let Some(node_id) = dag.node_for(node) {
        let mut dec = RecognizerDecision::default();
        let fact = ir
            .node_facts
            .get(&node_id)
            .and_then(|f| f.recognizer.as_ref());

        match node {
            IrNode::Alt(_, dispatch) => {
                dec.alt_mode = Some(fallback_alt_mode(fact, dispatch.is_some()));
            }
            IrNode::Skip(_, _) | IrNode::Next(_, _) => {
                if is_wrap_shape(node) {
                    dec.wrap_mode = Some(fallback_wrap_mode(fact));
                }
            }
            IrNode::Regex(sid) => {
                let feasible = ir
                    .regex_info
                    .get(sid)
                    .map(|i| i.feasible_engines)
                    .unwrap_or_else(EngineSet::empty);
                dec.regex_engine = Some(fallback_engine(feasible));
            }
            _ => {}
        }

        if dec.alt_mode.is_some() || dec.wrap_mode.is_some() || dec.regex_engine.is_some() {
            decisions.insert(node_id, dec);
        }
    }
    super::recognizers::visit_children_alt(node, |child| decode_fallback(child, ir, dag, decisions));
}

fn fallback_alt_mode(fact: Option<&Recognizer>, has_dispatch: bool) -> AltMode {
    if has_dispatch {
        return AltMode::ByteDispatch;
    }
    if let Some(rec) = fact {
        // Tranche Y.3: TokenLedBranches → ByteDispatch (folded variant).
        if matches!(rec.shape, RecognizerShape::TokenLedBranches { .. }) {
            return AltMode::ByteDispatch;
        }
        if matches!(rec.shape, RecognizerShape::KeywordPrefix { .. }) {
            return AltMode::KeyDispatch;
        }
    }
    AltMode::Checkpoint
}

fn fallback_wrap_mode(fact: Option<&Recognizer>) -> WrapMode {
    if let Some(rec) = fact {
        match &rec.shape {
            RecognizerShape::DelimiterBalanced { .. } => return WrapMode::BalancedScan,
            RecognizerShape::SeparatorList { .. } => return WrapMode::SepBy,
            _ => {}
        }
    }
    WrapMode::Generic
}

fn fallback_engine(feasible: EngineSet) -> RegexEngine {
    if feasible.contains(EngineSet::FAMILY_HELPER) {
        return RegexEngine::FamilyHelper;
    }
    if feasible.contains(EngineSet::MEMCHR1) {
        return RegexEngine::Memchr1;
    }
    if feasible.contains(EngineSet::MEMCHR2) {
        return RegexEngine::Memchr2;
    }
    if feasible.contains(EngineSet::MEMCHR3) {
        return RegexEngine::Memchr3;
    }
    if feasible.contains(EngineSet::NIBBLE_LUT) {
        return RegexEngine::NibbleLut;
    }
    if feasible.contains(EngineSet::ONE_PASS) {
        return RegexEngine::OnePass;
    }
    if feasible.contains(EngineSet::SMALL_DFA) {
        return RegexEngine::SmallDfa;
    }
    RegexEngine::Dfa
}

// ── Tranche X.8d — per-StringId regex engine projection ───────────────────

/// Project the per-NodeId regex engine decisions from a
/// [`RecognizerDecisionMap`] into a per-`StringId` lookup, grouping
/// all occurrences of the same pattern under one authoritative
/// decision.
///
/// The CSP assigns one `RegexEngine` per occurrence (per `Regex(sid)`
/// NodeId), which makes cost minimization tractable; the backend's
/// `scanner_plan::plan_regex_scanner` consumes the result by pattern
/// string. When the same `StringId` has multiple decisions (e.g., the
/// same regex appears in two Seq positions that reach different
/// constraints), this function picks the strongest engine — the one
/// with the lowest tier-priced cost in [`build_engine_domain`].
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
            if let Some(engine) = decisions
                .get(&nid)
                .and_then(|d| d.regex_engine.as_ref())
            {
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

    super::recognizers::visit_children_alt(node, |child| {
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
