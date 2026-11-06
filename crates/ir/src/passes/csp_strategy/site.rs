use std::collections::HashMap;

use bbnf_regex::EngineSet;
use csp_solver::{
    Csp,
    constraint::VarId,
    domain::{CostDomain, Domain},
};
use rustc_hash::FxHashMap;

use super::domain::{
    RecognizerDecisionMap, StrategyDomain, StrategyValue, build_alt_domain, build_engine_domain,
    build_wrap_domain,
};
use super::report::{
    DecisionCounts, DecisionFamily, OptimizationFailureLocus, OptimizationUnavailableReason,
};
use crate::dag::NodeId;
use crate::passes::inspect::visit_children_alt;
use crate::passes::materialization::MaterializationClass;
use crate::{CostConfig, GrammarIR, IrNode, RuleId};

pub(super) fn dag_covers(node: &IrNode, dag: &crate::dag::GrammarDag) -> bool {
    if dag.node_for(node).is_none() {
        return false;
    }
    let mut covered = true;
    node.for_each_child(&mut |child| covered &= dag_covers(child, dag));
    covered
}

#[allow(clippy::too_many_arguments)]
pub(super) fn index_rule_vars(
    node: &IrNode,
    rule: RuleId,
    dag: &crate::dag::GrammarDag,
    by_node: &ByNodeVars,
    engine_vars: &mut HashMap<(RuleId, NodeId), VarId>,
    alt_vars: &mut HashMap<NodeId, VarId>,
    wrap_vars: &mut HashMap<NodeId, VarId>,
) {
    if let Some(node_id) = dag.node_for(node)
        && let Some(&(alt, wrap, engine)) = by_node.get(&node_id)
    {
        if let Some(var) = alt {
            alt_vars.insert(node_id, var);
        }
        if let Some(var) = wrap {
            wrap_vars.insert(node_id, var);
        }
        if let Some(var) = engine {
            engine_vars.insert((rule, node_id), var);
        }
    }
    visit_children_alt(node, |child| {
        index_rule_vars(child, rule, dag, by_node, engine_vars, alt_vars, wrap_vars)
    });
}

/// Per-decision-site bookkeeping: which `NodeId` this var belongs to
/// and which decision family. Lifted to module level so the helper
/// fns below can name it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) enum Site {
    Alt(NodeId),
    Wrap(NodeId),
    Engine(NodeId),
    /// Tranche AB.1 — tape materialization class. One variable per canonical
    /// root `NodeId` (not per occurrence or descendant); shared roots join
    /// their legality before the AB.2 emitter consumes the decision.
    Materialization(NodeId),
}

impl Site {
    pub(super) fn node(self) -> NodeId {
        match self {
            Self::Alt(node)
            | Self::Wrap(node)
            | Self::Engine(node)
            | Self::Materialization(node) => node,
        }
    }

    pub(super) fn family(self) -> DecisionFamily {
        match self {
            Self::Alt(_) => DecisionFamily::Alt,
            Self::Wrap(_) => DecisionFamily::Wrap,
            Self::Engine(_) => DecisionFamily::Engine,
            Self::Materialization(_) => DecisionFamily::Materialization,
        }
    }
}

#[derive(Default)]
pub(super) struct SiteRegistry {
    pub(super) entries: Vec<(Site, StrategyDomain)>,
    index: FxHashMap<Site, usize>,
}

impl SiteRegistry {
    pub(super) fn register(
        &mut self,
        site: Site,
        domain: StrategyDomain,
    ) -> Result<(), SiteRegistrationFailure> {
        if !domain.valid() {
            return Err(SiteRegistrationFailure {
                reason: OptimizationUnavailableReason::InvalidAssignment,
                locus: OptimizationFailureLocus::Site {
                    node: site.node(),
                    family: site.family(),
                },
            });
        }
        if let Some(&index) = self.index.get(&site) {
            if self.entries[index].1.intersect(&domain).is_err() {
                return Err(SiteRegistrationFailure {
                    reason: OptimizationUnavailableReason::InvalidAssignment,
                    locus: OptimizationFailureLocus::Site {
                        node: site.node(),
                        family: site.family(),
                    },
                });
            }
            if self.entries[index].1.is_empty() {
                return Err(SiteRegistrationFailure {
                    reason: OptimizationUnavailableReason::Unsatisfiable,
                    locus: OptimizationFailureLocus::Site {
                        node: site.node(),
                        family: site.family(),
                    },
                });
            }
            return Ok(());
        }
        if domain.is_empty() {
            return Err(SiteRegistrationFailure {
                reason: OptimizationUnavailableReason::Unsatisfiable,
                locus: OptimizationFailureLocus::Site {
                    node: site.node(),
                    family: site.family(),
                },
            });
        }
        self.index.insert(site, self.entries.len());
        self.entries.push((site, domain));
        Ok(())
    }

    pub(super) fn counts(&self) -> DecisionCounts {
        let mut counts = DecisionCounts::default();
        for (site, _) in &self.entries {
            counts.add(site.family());
        }
        counts
    }

    pub(super) fn domain_counts(&self) -> DecisionCounts {
        let mut counts = DecisionCounts::default();
        for (site, domain) in &self.entries {
            for _ in 0..domain.size() {
                counts.add(site.family());
            }
        }
        counts
    }
}

#[derive(Debug)]
pub(super) struct SiteRegistrationFailure {
    pub(super) reason: OptimizationUnavailableReason,
    pub(super) locus: OptimizationFailureLocus,
}

/// Per-node variable triple: `(alt_var, wrap_var, engine_var)`. Each
/// slot is `Some` iff the node contributed a variable of that
/// decision family to the current CSP. Used as the value type of the
/// `ByNodeVars` scratch map so cross-variable constraint walks can
/// look up sibling variables by `NodeId`.
pub(super) type NodeVarTriple = (Option<VarId>, Option<VarId>, Option<VarId>);

/// Scratch mapping `NodeId → NodeVarTriple`, per-component, consulted
/// by intra-rule and cross-rule constraint installers to resolve a
/// node back to the variable ids the site-collection walk produced.
pub(super) type ByNodeVars = FxHashMap<NodeId, NodeVarTriple>;

#[derive(Debug)]
pub(super) struct ComponentAssignments {
    pub(super) decisions: RecognizerDecisionMap,
    pub(super) materialization: HashMap<NodeId, MaterializationClass>,
}

#[derive(Debug)]
pub(super) struct DecodedComponent {
    pub(super) assignments: ComponentAssignments,
    pub(super) objective: f64,
    pub(super) covered: usize,
}

#[derive(Clone, Copy, Debug)]
pub(super) struct DecodeFailure {
    pub(super) locus: OptimizationFailureLocus,
    pub(super) covered: usize,
}

#[derive(Clone, Copy)]
enum AssignmentSource<'a> {
    Factorized,
    Solution(&'a [StrategyValue]),
}

/// Exact optimum for a proved factorized component. This function is called
/// only when the installed-constraint count is zero; it additionally proves
/// that every variable has a non-empty domain and every variable/site is
/// covered before returning any output.
pub(super) fn decode_exact_factorized_optimum(
    csp: &Csp<StrategyDomain>,
    sites: &[(VarId, Site)],
) -> Result<DecodedComponent, DecodeFailure> {
    decode_assignments(csp, sites, AssignmentSource::Factorized)
}

pub(super) fn decode_complete_solution(
    solution: &[StrategyValue],
    csp: &Csp<StrategyDomain>,
    sites: &[(VarId, Site)],
) -> Result<DecodedComponent, DecodeFailure> {
    decode_assignments(csp, sites, AssignmentSource::Solution(solution))
}

fn decode_assignments(
    csp: &Csp<StrategyDomain>,
    sites: &[(VarId, Site)],
    source: AssignmentSource<'_>,
) -> Result<DecodedComponent, DecodeFailure> {
    if sites.len() != csp.variables.len() {
        return Err(DecodeFailure {
            locus: OptimizationFailureLocus::Search,
            covered: 0,
        });
    }
    if let AssignmentSource::Solution(solution) = source
        && solution.len() != csp.variables.len()
    {
        return Err(DecodeFailure {
            locus: OptimizationFailureLocus::Search,
            covered: 0,
        });
    }
    let mut assignments = ComponentAssignments {
        decisions: RecognizerDecisionMap::new(),
        materialization: HashMap::new(),
    };
    let mut covered = vec![false; csp.variables.len()];
    let mut covered_count = 0usize;
    let mut objective = 0.0;
    for (var_id, site) in sites {
        let index = *var_id as usize;
        let Some(variable) = csp.variables.get(index) else {
            return Err(DecodeFailure {
                locus: OptimizationFailureLocus::Variable(*var_id),
                covered: covered_count,
            });
        };
        if !variable.domain.valid() || variable.domain.is_empty() {
            return Err(DecodeFailure {
                locus: OptimizationFailureLocus::Variable(*var_id),
                covered: covered_count,
            });
        }
        let value = match source {
            AssignmentSource::Factorized => {
                let mut values = variable.domain.active_costs();
                let Some((mut best_value, mut best_cost)) = values.next() else {
                    return Err(DecodeFailure {
                        locus: OptimizationFailureLocus::Variable(*var_id),
                        covered: covered_count,
                    });
                };
                for (candidate, cost) in values {
                    if cost < best_cost {
                        best_value = candidate;
                        best_cost = cost;
                    }
                }
                best_value
            }
            AssignmentSource::Solution(solution) => {
                let Some(value) = solution.get(index) else {
                    return Err(DecodeFailure {
                        locus: OptimizationFailureLocus::Variable(*var_id),
                        covered: covered_count,
                    });
                };
                value
            }
        };
        if covered[index] {
            return Err(DecodeFailure {
                locus: OptimizationFailureLocus::Variable(*var_id),
                covered: covered_count,
            });
        }
        if !variable.domain.contains(value) {
            return Err(DecodeFailure {
                locus: OptimizationFailureLocus::Variable(*var_id),
                covered: covered_count,
            });
        }
        assign_value(&mut assignments, *site, value).map_err(|locus| DecodeFailure {
            locus,
            covered: covered_count,
        })?;
        let next_objective = objective + variable.domain.cost(value);
        if !next_objective.is_finite() {
            return Err(DecodeFailure {
                locus: OptimizationFailureLocus::Variable(*var_id),
                covered: covered_count,
            });
        }
        objective = next_objective;
        covered[index] = true;
        covered_count += 1;
    }
    if covered.iter().any(|covered| !covered) {
        let missing = covered
            .iter()
            .position(|covered| !covered)
            .expect("at least one uncovered variable");
        return Err(DecodeFailure {
            locus: OptimizationFailureLocus::Variable(missing as VarId),
            covered: covered_count,
        });
    }
    Ok(DecodedComponent {
        assignments,
        objective,
        covered: covered_count,
    })
}

fn assign_value(
    output: &mut ComponentAssignments,
    site: Site,
    value: &StrategyValue,
) -> Result<(), OptimizationFailureLocus> {
    match (site, value) {
        (Site::Alt(node), StrategyValue::Alt(mode)) => {
            set_once(
                &mut output.decisions.entry(node).or_default().alt_mode,
                mode.clone(),
                node,
                DecisionFamily::Alt,
            )?;
        }
        (Site::Wrap(node), StrategyValue::Wrap(mode)) => {
            set_once(
                &mut output.decisions.entry(node).or_default().wrap_mode,
                mode.clone(),
                node,
                DecisionFamily::Wrap,
            )?;
        }
        (Site::Engine(node), StrategyValue::Engine(engine)) => {
            set_once(
                &mut output.decisions.entry(node).or_default().regex_engine,
                engine.clone(),
                node,
                DecisionFamily::Engine,
            )?;
        }
        (Site::Materialization(node), StrategyValue::Mat(class)) => {
            if output.materialization.insert(node, *class).is_some() {
                return Err(OptimizationFailureLocus::Site {
                    node,
                    family: DecisionFamily::Materialization,
                });
            }
        }
        _ => {
            return Err(OptimizationFailureLocus::Site {
                node: site.node(),
                family: site.family(),
            });
        }
    }
    Ok(())
}

fn set_once<T>(
    slot: &mut Option<T>,
    value: T,
    node: NodeId,
    family: DecisionFamily,
) -> Result<(), OptimizationFailureLocus> {
    if slot.is_some() {
        return Err(OptimizationFailureLocus::Site { node, family });
    }
    *slot = Some(value);
    Ok(())
}

/// Recursive walk that joins every occurrence into one semantic site.
pub(super) fn collect_sites(
    node: &IrNode,
    ir: &GrammarIR,
    dag: &crate::dag::GrammarDag,
    cfg: &CostConfig,
    registry: &mut SiteRegistry,
) -> Result<(), SiteRegistrationFailure> {
    if let Some(node_id) = dag.node_for(node) {
        let fact = ir
            .node_facts
            .get(&node_id)
            .and_then(|f| f.recognizer.as_ref());

        match node {
            IrNode::Alt(branches, dispatch) => {
                // AZ-IV.W4.2 — domain must contain `KeyDispatch` whenever
                // the upstream structural detector populated a sidecar
                // payload (`key_dispatch_configs`) or the keyword-stats
                // miner (`keyword_branches`) committed to this Alt. The
                // `dispatch` constraint installer pins `AltMode = KeyDispatch`
                // on those nodes; without the variant in the domain the
                // pin is unsatisfiable and optimization becomes typed
                // unavailable, so the legal value must be represented here.
                let has_key_dispatch = ir.key_dispatch_configs.contains_key(&node_id)
                    || ir.keyword_branches.contains_key(&node_id);
                let domain = build_alt_domain(
                    fact,
                    dispatch.is_some(),
                    has_key_dispatch,
                    branches.len(),
                    cfg,
                );
                registry.register(Site::Alt(node_id), domain)?;
            }
            IrNode::Skip(_, _) | IrNode::Next(_, _) => {
                if is_wrap_shape(node) {
                    let domain = build_wrap_domain(fact, cfg);
                    registry.register(Site::Wrap(node_id), domain)?;
                }
            }
            IrNode::Regex(sid) => {
                let info = ir.regex_info.get(sid);
                let feasible = info
                    .map(|i| i.feasible_engines)
                    .unwrap_or_else(EngineSet::empty);
                let domain = build_engine_domain(feasible, cfg);
                registry.register(Site::Engine(node_id), domain)?;
            }
            _ => {}
        }
    }

    let mut result = Ok(());
    visit_children_alt(node, |child| {
        if result.is_ok() {
            result = collect_sites(child, ir, dag, cfg, registry);
        }
    });
    result
}

// ── Helpers ─────────────────────────────────────────────────────────────────

fn is_wrap_shape(node: &IrNode) -> bool {
    match node {
        IrNode::Skip(left, _) => matches!(left.as_ref(), IrNode::Next(_, _)),
        IrNode::Next(_, right) => matches!(right.as_ref(), IrNode::Skip(_, _)),
        _ => false,
    }
}
