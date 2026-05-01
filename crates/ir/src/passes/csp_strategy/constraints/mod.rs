//! Cross-rule and structural CSP constraint installers.
//!
//! Each sub-module owns one constraint family with a named
//! production consumer. Installers register hard pins or
//! couplings against the shared component `Csp<StrategyDomain>`
//! built by [`crate::passes::csp_strategy::solve_grammar_components`].
//!
//! - [`engine`] — `EnginePropagation` pairwise equality across
//!   compiled regex engines within a component. Consumer:
//!   [`crate::passes::extract_regex_engine_decisions`] (pulled
//!   from `ir.regex_engine_decisions` by
//!   `crates/core/src/generate/regex/cost_model.rs`).
//! - [`shape`] — pins Alt and Wrap decision variables to the
//!   strategy implied by an admitted shape-dictionary template.
//!   Consumer: backend driver Alt/Wrap dispatch reading
//!   `ir.recognizer_decisions`.
//! - [`layout`] — pins Wrap decision variables to the
//!   `BalancedScan`/`SepBy` choice implied by the upstream
//!   `delim_scan_configs` / recognizer shape facts. Consumer:
//!   `backend::driver::wrap::compile_wrap`.
//! - [`dispatch`] — pins Alt decision variables to the
//!   `KeyDispatch` / `ByteDispatch` choice implied by the
//!   upstream `key_dispatch_configs` / `keyword_branches` /
//!   precomputed dispatch table. Consumer:
//!   `backend::strategy::alt_strategy::decide_alt_strategy`.
//! - [`shape_dict`] — grammar-wide shape-template admission
//!   selection (`solve_shape_dict_selection`). Consumer:
//!   `ir.shape_dict_selection` read by codegen at
//!   `crates/core/src/pipeline/compile.rs:861`.
//!
//! Each per-site installer adds at most one constraint per
//! eligible NodeId, deriving the pin value from facts that the
//! upstream miner already populated. Tests under
//! `crates/ir/tests/lattices/csp_authority.rs` cover the
//! "constraint installed → consumer reads CSP fact; constraint
//! absent → CSP returns its untargeted cost-min and the consumer
//! sees a different decision" disconnect-pairing.

pub mod dispatch;
pub mod engine;
pub mod layout;
pub mod shape;
pub mod shape_dict;

use std::collections::HashMap;

use csp_solver::constraint::VarId;

use crate::dag::NodeId;
use crate::passes::materialization::MaterializationClass;
use crate::{GrammarIR, RuleId};

/// Per-component bookkeeping passed to every constraint installer.
///
/// Built by the parent dispatcher
/// (`csp_strategy::solve_grammar_components`) before it runs the
/// installers. Carries the per-site variable maps so each installer
/// can resolve a `NodeId` back to the CSP `VarId` for the relevant
/// decision family without re-walking the IR.
pub struct ConstraintCtx<'a> {
    /// The component currently being wired, as a sorted list of
    /// `RuleId`s.
    pub component: &'a [RuleId],

    /// Per-rule materialization class. Populated upstream by
    /// `classify_materialization` into `ir.materialization` and
    /// read by constraint installers. Keyed by rule body `NodeId`.
    pub mat_classes: &'a HashMap<NodeId, MaterializationClass>,

    /// Per-(rule, regex-node) regex engine variable id. A single
    /// rule may own multiple regex variables (one per
    /// `IrNode::Regex` site in its body). The [`engine`]
    /// installer walks cross-rule pairs of these.
    pub engine_vars: &'a HashMap<(RuleId, NodeId), VarId>,

    /// Per-NodeId Alt decision variable id. Populated by the
    /// site-collection walk in
    /// `crate::passes::csp_strategy::solve_component`. Consumed by
    /// the [`shape`] and [`dispatch`] installers to pin Alt mode
    /// when upstream facts authoritatively determine it.
    pub alt_vars: &'a HashMap<NodeId, VarId>,

    /// Per-NodeId Wrap decision variable id. Populated by the
    /// site-collection walk. Consumed by the [`shape`] and
    /// [`layout`] installers to pin Wrap mode when upstream
    /// `delim_scan_configs` / recognizer shape facts determine it.
    pub wrap_vars: &'a HashMap<NodeId, VarId>,
}

impl<'a> ConstraintCtx<'a> {
    /// Resolve the materialization class for a rule by walking
    /// through its body `NodeId`. Returns `MustTape` (the safe
    /// top of the lattice) if the rule's body is absent from
    /// the DAG or unclassified — conservative by construction.
    pub fn rule_materialization(&self, ir: &GrammarIR, rule: RuleId) -> MaterializationClass {
        let Some(dag) = ir.dag.as_ref() else {
            return MaterializationClass::MustTape;
        };
        let Some(r) = ir.rules.get(rule as usize) else {
            return MaterializationClass::MustTape;
        };
        let Some(body_id) = dag.node_for(&r.body) else {
            return MaterializationClass::MustTape;
        };
        self.mat_classes
            .get(&body_id)
            .copied()
            .unwrap_or(MaterializationClass::MustTape)
    }
}
