use std::collections::HashMap;

use csp_solver::{
    CancelToken, Csp, OptimizationMode, Pruning, SolveConfig, constraint::VarId, domain::Domain,
};
use rustc_hash::FxHashMap;

use super::components::partition_by_call_graph;
use super::domain::{
    RecognizerDecisionMap, StrategyDomain, StrategyValue, build_materialization_domain,
};
use super::report::{
    ComponentReport, ConstraintCounts, DecisionCounts, DecisionFamily, GrammarSolveOutput,
    ObjectiveProof, OptimizationFailureLocus, OptimizationUnavailable,
    OptimizationUnavailableReason, SolveCompletionStatus,
};
use super::site::{
    ByNodeVars, Site, SiteRegistry, collect_sites, dag_covers, decode_complete_solution,
    decode_exact_factorized_optimum, index_rule_vars,
};
use crate::dag::NodeId;
use crate::passes::materialization::MaterializationClass;
use crate::{CostConfig, GrammarIR, RuleId};

/// Caller-controlled limits for the grammar-wide optimization transaction.
///
/// Search strategy and objective remain fixed by the optimizer; callers may
/// only bound node exploration or cooperatively cancel the transaction.
#[derive(Clone, Debug)]
pub struct GrammarSolveControl {
    pub node_budget: Option<u64>,
    pub cancel: Option<CancelToken>,
}

impl Default for GrammarSolveControl {
    fn default() -> Self {
        Self {
            node_budget: Some(1_000_000),
            cancel: None,
        }
    }
}

/// Private all-or-nothing transaction spanning every call-graph component.
/// Component results stay here until [`finish_grammar_transaction`] returns
/// the complete grammar output.
pub(super) struct GrammarTransaction<'a> {
    ir: &'a GrammarIR,
    rule_by_id: FxHashMap<RuleId, &'a crate::IrRule>,
    config: SolveConfig,
    output: GrammarSolveOutput,
}

impl<'a> GrammarTransaction<'a> {
    pub(super) fn new(
        ir: &'a GrammarIR,
        control: &GrammarSolveControl,
        component_capacity: usize,
    ) -> Result<Self, OptimizationUnavailable> {
        let config = SolveConfig {
            pruning: Pruning::ForwardChecking,
            optimization_mode: OptimizationMode::MinimizeCost,
            node_budget: control.node_budget,
            cancel: control.cancel.clone(),
            ..SolveConfig::default()
        };
        if ir.dag.is_none() {
            let (reason, locus) = if cancellation_requested(&config) {
                (
                    OptimizationUnavailableReason::Cancelled,
                    OptimizationFailureLocus::Search,
                )
            } else {
                (
                    OptimizationUnavailableReason::MissingDag,
                    OptimizationFailureLocus::GrammarDag,
                )
            };
            return Err(OptimizationUnavailable {
                reason,
                locus,
                reports: Vec::new(),
            });
        }

        let mut rule_by_id =
            FxHashMap::with_capacity_and_hasher(ir.rules.len(), Default::default());
        for rule in &ir.rules {
            rule_by_id.insert(rule.id, rule);
        }
        Ok(Self {
            ir,
            rule_by_id,
            config,
            output: GrammarSolveOutput {
                decisions: RecognizerDecisionMap::new(),
                materialization: HashMap::new(),
                reports: Vec::with_capacity(component_capacity),
            },
        })
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
/// Per-component scope is the coarsest decomposition under which the active
/// cross-rule `EnginePropagation` constraints fit inside a single `Csp`
/// instance. Rules with no Ref chain between them
/// are in disjoint components and solved independently, bounding
/// branch-and-bound's search surface per component.
///
/// The Tranche X.6 union CSP experiment (single grammar-wide
/// `Csp::new()`) blew `compile_css_l4` from 9 ms to 94 ms because
/// the solver explored the full rule cross-product whenever any
/// constraint fired. The per-component decomposition recovers the
/// bounded search while permitting genuine cross-rule constraints.
///
/// Returns decisions and materialization refinements only when every
/// component proves a complete optimum. No component output is returned on
/// [`OptimizationUnavailable`].
pub fn solve_grammar_components(
    ir: &GrammarIR,
    control: &GrammarSolveControl,
) -> Result<GrammarSolveOutput, OptimizationUnavailable> {
    // AF.3 — build the connected-components partition of the rule
    // call graph exactly once, before the solve loop. The partition
    // is held by borrow for the entire loop so each component solve
    // can consult `components.component_of(rule_id)` without
    // rebuilding or allocating.
    let mut transaction = GrammarTransaction::new(ir, control, ir.rules.len())?;
    let components = partition_by_call_graph(ir);

    // AF.3 — iterate components in deterministic order. Each
    // component gets one `solve_component` call that builds a
    // single `Csp`, installs variables for every rule in the
    // component, installs intra-rule constraints plus (via the
    // `constraints` sub-module seam) any cross-rule constraints,
    // and runs one optimization solve.
    for (component_root, member_rules) in components.iter_components() {
        let component = enter_component(&mut transaction, component_root, member_rules)?;
        commit_component(&mut transaction, component)?;
    }
    finish_grammar_transaction(transaction)
}

fn cancellation_requested(config: &SolveConfig) -> bool {
    config
        .cancel
        .as_ref()
        .is_some_and(CancelToken::is_cancelled)
}

fn cancelled_boundary_report(
    component_root: RuleId,
    member_rules: &[RuleId],
    node_budget: Option<u64>,
) -> ComponentReport {
    let mut members = member_rules.to_vec();
    members.sort_unstable();
    ComponentReport {
        root: component_root,
        members,
        variables: DecisionCounts::default(),
        sites: DecisionCounts::default(),
        domain_values: DecisionCounts::default(),
        constraints: ConstraintCounts::default(),
        semantic_sites: 0,
        covered_sites: 0,
        status: SolveCompletionStatus::Cancelled,
        nodes_explored: 0,
        node_budget,
        cancelled: true,
        budget_exceeded: false,
        objective: None,
        failure: Some(OptimizationFailureLocus::Search),
    }
}

fn mark_report_cancelled(report: &mut ComponentReport) {
    report.status = SolveCompletionStatus::Cancelled;
    report.cancelled = true;
    report.objective = None;
    report.failure = Some(OptimizationFailureLocus::Search);
}

pub(super) struct ComponentOutput {
    pub(super) decisions: RecognizerDecisionMap,
    pub(super) materialization: HashMap<NodeId, MaterializationClass>,
    pub(super) report: ComponentReport,
}

struct ComponentFailure {
    reason: OptimizationUnavailableReason,
    locus: OptimizationFailureLocus,
    report: ComponentReport,
}

fn take_failure(
    transaction: &mut GrammarTransaction<'_>,
    reason: OptimizationUnavailableReason,
    locus: OptimizationFailureLocus,
) -> OptimizationUnavailable {
    OptimizationUnavailable {
        reason,
        locus,
        reports: std::mem::take(&mut transaction.output.reports),
    }
}

/// Enter one component and prove its complete local result. Nothing is merged
/// into the grammar output until [`commit_component`].
pub(super) fn enter_component(
    transaction: &mut GrammarTransaction<'_>,
    component_root: RuleId,
    member_rules: &[RuleId],
) -> Result<ComponentOutput, OptimizationUnavailable> {
    if cancellation_requested(&transaction.config) {
        transaction.output.reports.push(cancelled_boundary_report(
            component_root,
            member_rules,
            transaction.config.node_budget,
        ));
        return Err(take_failure(
            transaction,
            OptimizationUnavailableReason::Cancelled,
            OptimizationFailureLocus::Search,
        ));
    }

    let ir = transaction.ir;
    let dag = ir
        .dag
        .as_ref()
        .expect("GrammarTransaction::new requires a grammar DAG");
    match solve_component(
        component_root,
        member_rules,
        &transaction.rule_by_id,
        ir,
        dag,
        &ir.cost_config,
        &ir.materialization,
        ir.debug_all,
        &transaction.config,
    ) {
        Ok(component) => Ok(component),
        Err(failure) => {
            transaction.output.reports.push(failure.report);
            Err(take_failure(transaction, failure.reason, failure.locus))
        }
    }
}

/// Commit one proven component at the transaction boundary. Cancellation is
/// re-read here so no decoded result can cross the boundary after its token
/// becomes observable.
pub(super) fn commit_component(
    transaction: &mut GrammarTransaction<'_>,
    component: ComponentOutput,
) -> Result<(), OptimizationUnavailable> {
    let ComponentOutput {
        decisions,
        materialization,
        mut report,
    } = component;
    if cancellation_requested(&transaction.config) {
        mark_report_cancelled(&mut report);
        transaction.output.reports.push(report);
        return Err(take_failure(
            transaction,
            OptimizationUnavailableReason::Cancelled,
            OptimizationFailureLocus::Search,
        ));
    }
    if let Err((reason, locus)) =
        merge_component(&mut transaction.output, decisions, materialization)
    {
        report.status = SolveCompletionStatus::InvalidAssignment;
        report.objective = None;
        report.failure = Some(locus);
        transaction.output.reports.push(report);
        return Err(take_failure(transaction, reason, locus));
    }
    transaction.output.reports.push(report);
    Ok(())
}

/// Finish the all-or-nothing grammar transaction. A cancellation observed
/// after the last component invalidates that last proof and withholds all
/// accumulated decisions.
pub(super) fn finish_grammar_transaction(
    mut transaction: GrammarTransaction<'_>,
) -> Result<GrammarSolveOutput, OptimizationUnavailable> {
    if cancellation_requested(&transaction.config) {
        if let Some(report) = transaction.output.reports.last_mut() {
            mark_report_cancelled(report);
        }
        return Err(take_failure(
            &mut transaction,
            OptimizationUnavailableReason::Cancelled,
            OptimizationFailureLocus::Search,
        ));
    }
    Ok(transaction.output)
}

fn merge_component(
    output: &mut GrammarSolveOutput,
    decisions: RecognizerDecisionMap,
    materialization: HashMap<NodeId, MaterializationClass>,
) -> Result<(), (OptimizationUnavailableReason, OptimizationFailureLocus)> {
    let mut decisions: Vec<_> = decisions.into_iter().collect();
    decisions.sort_unstable_by_key(|(node, _)| *node);
    for (node, decision) in decisions {
        let target = output.decisions.entry(node).or_default();
        merge_decision_slot(&mut target.alt_mode, decision.alt_mode).map_err(|()| {
            (
                OptimizationUnavailableReason::InvalidAssignment,
                OptimizationFailureLocus::Merge {
                    node,
                    family: DecisionFamily::Alt,
                },
            )
        })?;
        merge_decision_slot(&mut target.wrap_mode, decision.wrap_mode).map_err(|()| {
            (
                OptimizationUnavailableReason::InvalidAssignment,
                OptimizationFailureLocus::Merge {
                    node,
                    family: DecisionFamily::Wrap,
                },
            )
        })?;
        merge_decision_slot(&mut target.regex_engine, decision.regex_engine).map_err(|()| {
            (
                OptimizationUnavailableReason::InvalidAssignment,
                OptimizationFailureLocus::Merge {
                    node,
                    family: DecisionFamily::Engine,
                },
            )
        })?;
    }
    let mut materialization: Vec<_> = materialization.into_iter().collect();
    materialization.sort_unstable_by_key(|(node, _)| *node);
    for (node, class) in materialization {
        match output.materialization.entry(node) {
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(class);
            }
            std::collections::hash_map::Entry::Occupied(entry) if *entry.get() == class => {}
            std::collections::hash_map::Entry::Occupied(_) => {
                return Err((
                    OptimizationUnavailableReason::InvalidAssignment,
                    OptimizationFailureLocus::Merge {
                        node,
                        family: DecisionFamily::Materialization,
                    },
                ));
            }
        }
    }
    Ok(())
}

fn merge_decision_slot<T: PartialEq>(target: &mut Option<T>, value: Option<T>) -> Result<(), ()> {
    let Some(value) = value else {
        return Ok(());
    };
    match target {
        Some(existing) if existing == &value => Ok(()),
        Some(_) => Err(()),
        None => {
            *target = Some(value);
            Ok(())
        }
    }
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
    component_root: RuleId,
    member_rules: &[RuleId],
    rule_by_id: &FxHashMap<RuleId, &crate::IrRule>,
    ir: &GrammarIR,
    dag: &crate::dag::GrammarDag,
    cfg: &CostConfig,
    materialization: &HashMap<NodeId, MaterializationClass>,
    debug_all: bool,
    config: &SolveConfig,
) -> Result<ComponentOutput, ComponentFailure> {
    let mut members = member_rules.to_vec();
    members.sort_unstable();
    let mut report = ComponentReport {
        root: component_root,
        members,
        variables: DecisionCounts::default(),
        sites: DecisionCounts::default(),
        domain_values: DecisionCounts::default(),
        constraints: ConstraintCounts::default(),
        semantic_sites: 0,
        covered_sites: 0,
        status: SolveCompletionStatus::InvalidAssignment,
        nodes_explored: 0,
        node_budget: config.node_budget,
        cancelled: false,
        budget_exceeded: false,
        objective: None,
        failure: None,
    };

    // Collect and conjoin every occurrence before constructing variables.
    // This makes the variable's original domain authoritative: Csp::solve()
    // resets to it before search, so post-construction narrowing would be lost.
    let mut registry = SiteRegistry::default();

    for &rid in member_rules {
        let Some(rule) = rule_by_id.get(&rid).copied() else {
            report_counts(&mut report, &registry);
            return Err(component_failure(
                report,
                OptimizationUnavailableReason::InvalidAssignment,
                OptimizationFailureLocus::Rule(rid),
            ));
        };
        let body = &rule.body;
        if !dag_covers(body, dag) {
            report_counts(&mut report, &registry);
            return Err(component_failure(
                report,
                OptimizationUnavailableReason::InvalidAssignment,
                OptimizationFailureLocus::Rule(rid),
            ));
        }
        if let Err(failure) = collect_sites(body, ir, dag, cfg, &mut registry) {
            report_counts(&mut report, &registry);
            return Err(component_failure(report, failure.reason, failure.locus));
        }

        let body_id = dag
            .node_for(body)
            .expect("dag_covers proved the component root node");
        let initial = materialization
            .get(&body_id)
            .copied()
            .unwrap_or(MaterializationClass::MustTape);
        let domain = build_materialization_domain(rule, initial, cfg, debug_all);
        if let Err(failure) = registry.register(Site::Materialization(body_id), domain) {
            report_counts(&mut report, &registry);
            return Err(component_failure(report, failure.reason, failure.locus));
        }
    }

    report_counts(&mut report, &registry);

    // Construct one variable and one decode site for each semantic key.
    let mut csp = Csp::<StrategyDomain>::new();
    let mut sites: Vec<(VarId, Site)> = Vec::with_capacity(registry.entries.len());
    let mut by_node: ByNodeVars = FxHashMap::default();
    for (site, domain) in registry.entries {
        if !domain.valid() {
            return Err(component_failure(
                report,
                OptimizationUnavailableReason::InvalidAssignment,
                OptimizationFailureLocus::Site {
                    node: site.node(),
                    family: site.family(),
                },
            ));
        }
        if domain.is_empty() {
            return Err(component_failure(
                report,
                OptimizationUnavailableReason::Unsatisfiable,
                OptimizationFailureLocus::Site {
                    node: site.node(),
                    family: site.family(),
                },
            ));
        }
        let var = csp.add_variable(domain);
        sites.push((var, site));
        match site {
            Site::Alt(node) => by_node.entry(node).or_default().0 = Some(var),
            Site::Wrap(node) => by_node.entry(node).or_default().1 = Some(var),
            Site::Engine(node) => by_node.entry(node).or_default().2 = Some(var),
            Site::Materialization(_) => {}
        }
    }

    let mut engine_vars: HashMap<(RuleId, NodeId), VarId> = HashMap::new();
    let mut alt_vars: HashMap<NodeId, VarId> = HashMap::new();
    let mut wrap_vars: HashMap<NodeId, VarId> = HashMap::new();
    for &rid in member_rules {
        let rule = rule_by_id[&rid];
        index_rule_vars(
            &rule.body,
            rid,
            dag,
            &by_node,
            &mut engine_vars,
            &mut alt_vars,
            &mut wrap_vars,
        );
    }

    // ── Phase 2: cross-variable constraints ────────────────────────────────
    let mut constraint_counts = ConstraintCounts::default();
    for &rid in member_rules {
        let Some(rule) = rule_by_id.get(&rid).copied() else {
            continue;
        };
        constraint_counts.token_dispatch +=
            super::constraints::token::install(&rule.body, dag, &by_node, &mut csp);
    }

    let installed = install_cross_rule_constraints(
        &mut csp,
        member_rules,
        &engine_vars,
        &alt_vars,
        &wrap_vars,
        ir,
    );
    constraint_counts.engine = installed.engine;
    constraint_counts.shape = installed.shape;
    constraint_counts.layout = installed.layout;
    constraint_counts.dispatch = installed.dispatch;
    report.constraints = constraint_counts;

    if constraint_counts.total() == 0 {
        let decoded = decode_exact_factorized_optimum(&csp, &sites).map_err(|failure| {
            report.covered_sites = failure.covered;
            component_failure(
                report.clone(),
                OptimizationUnavailableReason::InvalidAssignment,
                failure.locus,
            )
        })?;
        report.covered_sites = decoded.covered;
        report.status = SolveCompletionStatus::FactorizedExact;
        report.objective = Some(ObjectiveProof::new(decoded.objective));
        return Ok(ComponentOutput {
            decisions: decoded.assignments.decisions,
            materialization: decoded.assignments.materialization,
            report,
        });
    }

    let solution = match solve_exact_constrained(&mut csp, config) {
        Ok(solution) => solution,
        Err(failure) => {
            report_solver_stats(&mut report, &csp);
            return Err(component_failure(report, failure.reason, failure.locus));
        }
    };
    report_solver_stats(&mut report, &csp);
    let decoded = decode_complete_solution(&solution, &csp, &sites).map_err(|failure| {
        report.covered_sites = failure.covered;
        component_failure(
            report.clone(),
            OptimizationUnavailableReason::InvalidAssignment,
            failure.locus,
        )
    })?;
    report.covered_sites = decoded.covered;
    report.status = SolveCompletionStatus::Optimal;
    report.objective = Some(ObjectiveProof::new(decoded.objective));
    Ok(ComponentOutput {
        decisions: decoded.assignments.decisions,
        materialization: decoded.assignments.materialization,
        report,
    })
}

fn report_counts(report: &mut ComponentReport, registry: &SiteRegistry) {
    let counts = registry.counts();
    report.variables = counts;
    report.sites = counts;
    report.domain_values = registry.domain_counts();
    report.semantic_sites = counts.total();
}

fn report_solver_stats(report: &mut ComponentReport, csp: &Csp<StrategyDomain>) {
    let stats = csp.stats();
    report.nodes_explored = stats.nodes_explored;
    report.cancelled = stats.cancelled;
    report.budget_exceeded = stats.budget_exceeded;
}

fn component_failure(
    mut report: ComponentReport,
    reason: OptimizationUnavailableReason,
    locus: OptimizationFailureLocus,
) -> ComponentFailure {
    if reason == OptimizationUnavailableReason::Cancelled {
        report.cancelled = true;
        report.objective = None;
    }
    report.status = match reason {
        OptimizationUnavailableReason::Cancelled => SolveCompletionStatus::Cancelled,
        OptimizationUnavailableReason::BudgetExceeded => SolveCompletionStatus::BudgetExceeded,
        OptimizationUnavailableReason::Unsatisfiable => SolveCompletionStatus::Unsatisfiable,
        OptimizationUnavailableReason::MissingDag
        | OptimizationUnavailableReason::InvalidAssignment => {
            SolveCompletionStatus::InvalidAssignment
        }
    };
    report.failure = Some(locus);
    ComponentFailure {
        reason,
        locus,
        report,
    }
}

pub(super) fn solve_exact_constrained(
    csp: &mut Csp<StrategyDomain>,
    config: &SolveConfig,
) -> Result<Vec<StrategyValue>, SolveFailure> {
    for (index, variable) in csp.variables.iter().enumerate() {
        if !variable.domain.valid() || variable.domain.is_empty() {
            return Err(SolveFailure {
                reason: OptimizationUnavailableReason::InvalidAssignment,
                locus: OptimizationFailureLocus::Variable(index as VarId),
            });
        }
    }
    csp.finalize();
    let solutions = csp.solve_optimized(config);
    if cancellation_requested(config) {
        return Err(SolveFailure {
            reason: OptimizationUnavailableReason::Cancelled,
            locus: OptimizationFailureLocus::Search,
        });
    }
    if csp.stats().budget_exceeded {
        return Err(SolveFailure {
            reason: OptimizationUnavailableReason::BudgetExceeded,
            locus: OptimizationFailureLocus::Search,
        });
    }
    solutions.into_iter().next().ok_or(SolveFailure {
        reason: OptimizationUnavailableReason::Unsatisfiable,
        locus: OptimizationFailureLocus::Search,
    })
}

#[derive(Clone, Copy, Debug)]
pub(super) struct SolveFailure {
    pub(super) reason: OptimizationUnavailableReason,
    pub(super) locus: OptimizationFailureLocus,
}

/// Install cross-rule and per-site structural constraints from the
/// `constraints` sub-modules. Called once per component solve,
/// after variable building but before finalize.
///
/// Returns the total count of constraints installed (used by the
/// fast-path short-circuit to decide whether to skip the
/// optimization solve entirely). The four installers each take the
/// shared `ConstraintCtx` and the in-progress `Csp`:
///
/// - `engine::install` — cross-rule pairwise `EnginePropagation`
///   equality across compiled regex engines (AF.3).
/// - `shape::install` — per-site Alt/Wrap pin from
///   `RecognizerShape` (AZ-III.W3b.1).
/// - `layout::install` — per-site Wrap pin from
///   `delim_scan_configs` and `SeparatorList` shape (AZ-III.W3b.2).
/// - `dispatch::install` — per-site Alt pin from
///   `key_dispatch_configs` / `keyword_branches` (AZ-III.W3b.3).
fn install_cross_rule_constraints(
    csp: &mut Csp<StrategyDomain>,
    component: &[RuleId],
    engine_vars: &std::collections::HashMap<(RuleId, NodeId), VarId>,
    alt_vars: &std::collections::HashMap<NodeId, VarId>,
    wrap_vars: &std::collections::HashMap<NodeId, VarId>,
    ir: &GrammarIR,
) -> ConstraintCounts {
    let ctx = super::constraints::ConstraintCtx {
        component,
        mat_classes: &ir.materialization,
        engine_vars,
        alt_vars,
        wrap_vars,
    };

    ConstraintCounts {
        token_dispatch: 0,
        engine: super::constraints::engine::install(&ctx, csp, ir),
        shape: super::constraints::shape::install(&ctx, csp, ir),
        layout: super::constraints::layout::install(&ctx, csp, ir),
        dispatch: super::constraints::dispatch::install(&ctx, csp, ir),
    }
}
