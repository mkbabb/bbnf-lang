//! Tranche AF.3 — cross-rule CSP constraints.
//!
//! Constraints layer on top of the existing per-rule CSP solve to
//! make it component-scoped:
//!
//! - [`engine`] — `EnginePropagation` pins **compiled** regex
//!   engine choice per-component. When one rule in a component
//!   commits to a compiled engine (`Memchr*`, `NibbleLut`,
//!   `OnePass`, `SmallDfa`, `Dfa`), every other rule that
//!   carries a regex variable is pinned to the same compiled
//!   engine so the startup cost (DFA table construction,
//!   aho-corasick builder, nibble LUT) amortizes over every
//!   site. `FamilyHelper` is exempt: it is a named SIMD
//!   function call with zero startup cost and no shared
//!   infrastructure with compiled engines.
//! - [`shape_dict`] — Tranche AV.5.3 grammar-wide shape-template
//!   admission. Selects up to
//!   [`shape_dict::MAX_SHAPE_DICT_ENTRIES`] candidates from the
//!   `ShapeDictMiner` pool by greedy maximisation of `freq ×
//!   savings - static_entry_cost`. Runs as a separate pass via
//!   [`shape_dict::solve_shape_dict_selection`]; the per-component
//!   [`shape_dict::install`] hook is reserved for future
//!   cross-rule shape-dict interactions.
//!
//! # Integration
//!
//! Each sub-module exposes an `install(ctx, csp, ir)` free
//! function that registers its constraint with the shared
//! component CSP. The parent dispatcher in
//! `csp_strategy::mod::solve_grammar_components` builds the
//! [`ConstraintCtx`] once per component and calls the installer
//! after the per-rule variables have been constructed.
//!
//! # Layering
//!
//! These constraints are strictly cross-rule — they add edges
//! between variables owned by *different* rules within the same
//! component. The pre-existing intra-rule constraints
//! (`ImplicationConstraint` wiring an Alt parent to its child
//! regex engines, for instance) remain in
//! `csp_strategy::mod::add_token_dispatch_constraints` and are
//! unaffected.

pub mod engine;
pub mod shape_dict;

use std::collections::HashMap;

use csp_solver::constraint::VarId;

use crate::dag::NodeId;
use crate::passes::materialization::MaterializationClass;
use crate::{GrammarIR, RuleId};

/// Per-component bookkeeping passed to every cross-rule
/// constraint installer.
///
/// Built by the parent dispatcher
/// (`csp_strategy::mod::solve_grammar_components`) before it
/// runs the installers.
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
