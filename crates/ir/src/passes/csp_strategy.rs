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

use std::collections::HashMap;

use bbnf_regex::EngineSet;
use csp_solver::{
    constraint::{ImplicationConstraint, VarId},
    domain::{CostDomain, Domain},
    Csp, OptimizationMode, Pruning, SolveConfig,
};
use rustc_hash::FxHashMap;

use crate::dag::NodeId;
use crate::passes::patterns::{Recognizer, RecognizerShape};
use crate::{CostConfig, GrammarIR, IrNode};

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
    /// Hoisted shared helper — N≥`hoist_threshold` peers share one kernel.
    SharedHelper(u64),
}

/// Strategy chosen for a Wrap node.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WrapMode {
    /// Generic wrap (open / inner / close).
    Generic,
    /// Separator-by repeat.
    SepBy,
    /// Delimiter scan (forward memchr to close).
    DelimScan,
    /// Balanced delimiter scan (handles nested open/close).
    BalancedScan,
    /// Hoisted shared helper.
    SharedHelper(u64),
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
/// [`solve_strategy_decisions`].
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
/// `StrategyValue` unifies the three decision families behind one
/// enum. Each variable's domain is restricted to one variant family
/// (Alt vars only ever hold `Alt(_)`, etc.).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StrategyValue {
    Alt(AltMode),
    Wrap(WrapMode),
    Engine(RegexEngine),
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
/// per rule body with [`OptimizationMode::MinimizeCost`].
///
/// Renamed from V.6's `solve_recognizer_decisions` to reflect the
/// reality: this is a strategy synthesis pass that solves an
/// optimization problem, not a deterministic recognizer walk.
///
/// Per-rule scope is intentional: branch-and-bound search complexity
/// is bounded by the per-rule variable count, even when no cross-rule
/// constraint exists. A union CSP (Tranche X attempted this and
/// reverted) blew compile_css_l4 from 9 ms to 94 ms because the
/// search had to explore the cross-product of every rule's decision
/// space whenever any single rule had an `ImplicationConstraint` —
/// the solver does not detect connected components.
///
/// True global cross-rule optimization (with `SharedHelper` hoisting
/// variables and a joint objective) belongs to Tranche Y, and will
/// require either a connected-components decomposition or a solver
/// substrate that exploits independent sub-problems.
pub fn solve_strategy_decisions(ir: &GrammarIR) -> RecognizerDecisionMap {
    let dag = match ir.dag.as_ref() {
        Some(d) => d,
        None => return HashMap::new(),
    };

    let mut decisions: RecognizerDecisionMap = HashMap::new();
    let cfg = &ir.cost_config;

    for rule in &ir.rules {
        solve_rule(&rule.body, ir, dag, cfg, &mut decisions);
    }

    decisions
}

/// Build and solve the strategy CSP for a single rule body.
fn solve_rule(
    body: &IrNode,
    ir: &GrammarIR,
    dag: &crate::dag::GrammarDag,
    cfg: &CostConfig,
    decisions: &mut RecognizerDecisionMap,
) {
    // ── Phase 1: collect decision sites + build CSP variables ──────────────
    let mut csp = Csp::<StrategyDomain>::new();
    let mut sites: Vec<(VarId, Site)> = Vec::new();
    // Map node → its (Alt, Wrap, Engine) variable ids, for cross-var
    // constraints. FxHashMap (Tranche X.6) over NodeId for the per-rule
    // scratch — the SipHasher cost was non-trivial on rule-rich
    // grammars even though by_node is per-rule.
    let mut by_node: FxHashMap<NodeId, (Option<VarId>, Option<VarId>, Option<VarId>)> =
        FxHashMap::default();

    collect_sites(body, ir, dag, cfg, &mut csp, &mut sites, &mut by_node);

    if sites.is_empty() {
        return;
    }

    // ── Phase 2: cross-variable constraints ────────────────────────────────
    let constraints_added = add_token_dispatch_constraints(body, dag, &by_node, &mut csp);

    // Fast path: when no cross-variable constraint applies to this
    // rule, every variable's optimal value is its lowest-cost domain
    // entry — branch-and-bound would pick exactly that. Skip
    // finalize+solve and pick the per-variable minimum directly.
    //
    // The CSP scaffolding is still **constructed** so the architectural
    // commitment ("every file with `csp` in its name uses csp_solver::Csp")
    // holds for every code path; we just elide the search work that
    // wouldn't change the answer.
    if constraints_added == 0 {
        decode_min_cost_per_variable(&csp, &sites, decisions);
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
    // (`SolveConfig::node_budget`) so that a pathological search cannot
    // hang the compile. If the budget fires, fall back to the trivial
    // per-variable pick — safe because it's the same answer branch-and-
    // bound would give if all `ImplicationConstraint`s were dropped.
    // This is the structured failure mode the X.6 global-CSP attempt
    // lacked, and the precondition for Y.3 / Y.5 broadening the same
    // failure surface.
    if csp.stats().budget_exceeded {
        #[cfg(debug_assertions)]
        eprintln!(
            "Note: csp_strategy::solve_rule hit the CSP node budget \
             (nodes_explored={}); falling back to per-variable trivial pick",
            csp.stats().nodes_explored
        );
        decode_min_cost_per_variable(&csp, &sites, decisions);
        return;
    }

    // ── Phase 4: decode solution → RecognizerDecisionMap ───────────────────
    if let Some(solution) = solutions.first() {
        for (var_id, site) in &sites {
            if let Some(value) = solution.get(*var_id as usize).cloned() {
                let entry = decisions.entry(site.node()).or_default();
                match (site, value) {
                    (Site::Alt(_), StrategyValue::Alt(m)) => entry.alt_mode = Some(m),
                    (Site::Wrap(_), StrategyValue::Wrap(m)) => entry.wrap_mode = Some(m),
                    (Site::Engine(_), StrategyValue::Engine(e)) => entry.regex_engine = Some(e),
                    _ => {}
                }
            }
        }
    } else {
        // No optimization solution — fall back to the deterministic
        // priority decisions. This branch only fires when the CSP is
        // unsatisfiable, which should only happen if a constraint
        // prunes a domain to empty.
        decode_fallback(body, ir, dag, decisions);
    }
}

/// Per-decision-site bookkeeping: which `NodeId` this var belongs to
/// and which decision family. Lifted to module level so the helper
/// fns below can name it.
enum Site {
    Alt(NodeId),
    Wrap(NodeId),
    Engine(NodeId),
}

impl Site {
    fn node(&self) -> NodeId {
        match self {
            Site::Alt(n) | Site::Wrap(n) | Site::Engine(n) => *n,
        }
    }
}

/// Direct decoder for the no-constraint fast path: pick the
/// lowest-cost value from each variable's domain independently and
/// write it into the decision map. Branch-and-bound would produce
/// exactly the same answer when there are no cross-variable
/// constraints, so this is a strict optimization.
fn decode_min_cost_per_variable(
    csp: &Csp<StrategyDomain>,
    sites: &[(VarId, Site)],
    decisions: &mut RecognizerDecisionMap,
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
        let entry = decisions.entry(site.node()).or_default();
        match (site, value) {
            (Site::Alt(_), StrategyValue::Alt(m)) => entry.alt_mode = Some(m),
            (Site::Wrap(_), StrategyValue::Wrap(m)) => entry.wrap_mode = Some(m),
            (Site::Engine(_), StrategyValue::Engine(e)) => entry.regex_engine = Some(e),
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
    by_node: &mut FxHashMap<NodeId, (Option<VarId>, Option<VarId>, Option<VarId>)>,
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
    by_node: &FxHashMap<NodeId, (Option<VarId>, Option<VarId>, Option<VarId>)>,
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
    by_node: &FxHashMap<NodeId, (Option<VarId>, Option<VarId>, Option<VarId>)>,
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
    by_node: &FxHashMap<NodeId, (Option<VarId>, Option<VarId>, Option<VarId>)>,
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
            cfg.strategy_dispatch_bonus.abs(),
        ));
    }

    if let Some(rec) = fact {
        if let Some(group) = rec.peer_group {
            // SharedHelper carries the lowest cost — hoist savings.
            values.push((
                StrategyValue::Alt(AltMode::SharedHelper(group as u64)),
                cfg.strategy_dispatch_bonus.abs() - cfg.strategy_hoist_savings,
            ));
        }
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
                cfg.strategy_dispatch_bonus.abs(),
            ));
        }
        if matches!(rec.shape, RecognizerShape::KeywordPrefix { .. }) {
            values.push((
                StrategyValue::Alt(AltMode::KeyDispatch),
                cfg.strategy_dispatch_bonus.abs(),
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
        if let Some(group) = rec.peer_group {
            values.push((
                StrategyValue::Wrap(WrapMode::SharedHelper(group as u64)),
                cfg.strategy_dispatch_bonus.abs() - cfg.strategy_hoist_savings,
            ));
        }
        match &rec.shape {
            RecognizerShape::DelimiterBalanced { .. } => values.push((
                StrategyValue::Wrap(WrapMode::BalancedScan),
                cfg.strategy_dispatch_bonus.abs(),
            )),
            RecognizerShape::SeparatorList { .. } => values.push((
                StrategyValue::Wrap(WrapMode::SepBy),
                cfg.strategy_dispatch_bonus.abs(),
            )),
            _ => {}
        }
    }

    StrategyDomain::new(values)
}

/// Build the cost-weighted domain for a `RegexEngine` decision variable.
///
/// Each engine that's a member of `feasible` is added at a tier-priced
/// cost: kernel helpers cheapest, narrow memchr next, then nibble LUT,
/// one-pass, small DFA, and DFA last.
fn build_engine_domain(feasible: EngineSet, cfg: &CostConfig) -> StrategyDomain {
    let dispatch_bonus = cfg.strategy_dispatch_bonus.abs();
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
        if let Some(group) = rec.peer_group {
            return AltMode::SharedHelper(group as u64);
        }
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
        if let Some(group) = rec.peer_group {
            return WrapMode::SharedHelper(group as u64);
        }
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
