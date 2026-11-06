use std::collections::HashMap;

use csp_solver::constraint::VarId;

use super::domain::RecognizerDecisionMap;
use crate::RuleId;
use crate::dag::NodeId;
use crate::passes::materialization::MaterializationClass;

/// All-or-nothing optimizer output.
#[derive(Clone, Debug)]
pub struct GrammarSolveOutput {
    pub decisions: RecognizerDecisionMap,
    pub materialization: HashMap<NodeId, MaterializationClass>,
    pub reports: Vec<ComponentReport>,
}

/// Typed cause for an unavailable grammar-wide optimization.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OptimizationUnavailableReason {
    MissingDag,
    Cancelled,
    BudgetExceeded,
    Unsatisfiable,
    InvalidAssignment,
}

/// Decision family counted and validated by the component proof.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum DecisionFamily {
    Alt,
    Wrap,
    Engine,
    Materialization,
}

/// Compact counts for the bounded decision language.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct DecisionCounts {
    pub alt: usize,
    pub wrap: usize,
    pub engine: usize,
    pub materialization: usize,
}

impl DecisionCounts {
    pub(super) fn add(&mut self, family: DecisionFamily) {
        match family {
            DecisionFamily::Alt => self.alt += 1,
            DecisionFamily::Wrap => self.wrap += 1,
            DecisionFamily::Engine => self.engine += 1,
            DecisionFamily::Materialization => self.materialization += 1,
        }
    }

    pub fn total(self) -> usize {
        self.alt + self.wrap + self.engine + self.materialization
    }
}

/// Exact installed hard-constraint census.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct ConstraintCounts {
    pub token_dispatch: usize,
    pub engine: usize,
    pub shape: usize,
    pub layout: usize,
    pub dispatch: usize,
}

impl ConstraintCounts {
    pub fn total(self) -> usize {
        self.token_dispatch + self.engine + self.shape + self.layout + self.dispatch
    }
}

/// Whether a component proved a complete result or stopped explicitly.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SolveCompletionStatus {
    FactorizedExact,
    Optimal,
    Cancelled,
    BudgetExceeded,
    Unsatisfiable,
    InvalidAssignment,
}

/// Exact failure location; no prose parsing is needed to classify a stop.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OptimizationFailureLocus {
    GrammarDag,
    Rule(RuleId),
    Site {
        node: NodeId,
        family: DecisionFamily,
    },
    Variable(VarId),
    Search,
    Merge {
        node: NodeId,
        family: DecisionFamily,
    },
}

/// Bit-exact additive objective. `value()` recovers the reported `f64`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ObjectiveProof {
    pub bits: u64,
}

impl ObjectiveProof {
    pub(super) fn new(value: f64) -> Self {
        Self {
            bits: value.to_bits(),
        }
    }

    pub fn value(self) -> f64 {
        f64::from_bits(self.bits)
    }
}

/// Deterministic proof facts for one attempted call-graph component.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ComponentReport {
    pub root: RuleId,
    pub members: Vec<RuleId>,
    pub variables: DecisionCounts,
    pub sites: DecisionCounts,
    pub domain_values: DecisionCounts,
    pub constraints: ConstraintCounts,
    pub semantic_sites: usize,
    pub covered_sites: usize,
    pub status: SolveCompletionStatus,
    pub nodes_explored: u64,
    pub node_budget: Option<u64>,
    pub cancelled: bool,
    pub budget_exceeded: bool,
    pub objective: Option<ObjectiveProof>,
    pub failure: Option<OptimizationFailureLocus>,
}

/// Typed unavailable result. Reports contain every completed component and,
/// when a component was reached, the failing component.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OptimizationUnavailable {
    pub reason: OptimizationUnavailableReason,
    pub locus: OptimizationFailureLocus,
    pub reports: Vec<ComponentReport>,
}

impl std::fmt::Display for OptimizationUnavailable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "grammar optimization unavailable: {:?} at {:?} ({} component reports)",
            self.reason,
            self.locus,
            self.reports.len(),
        )
    }
}

impl std::error::Error for OptimizationUnavailable {}
