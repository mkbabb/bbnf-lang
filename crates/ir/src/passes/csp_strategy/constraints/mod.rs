//! Tranche AF.3 — cross-rule CSP constraints.
//!
//! Three constraints layer on top of the existing per-rule CSP
//! solve to make it component-scoped:
//!
//! - [`engine`] — `EnginePropagation` pins regex engine choice
//!   per-component. When one rule in a component commits to a
//!   given [`RegexEngine`], every other rule in that component
//!   that carries a regex variable is pinned to the same engine
//!   so the startup cost (DFA table construction, aho-corasick
//!   builder, nibble LUT) amortizes over every site.
//!
//! - [`parent`] — `ParentCompatibility` enforces caller/callee
//!   tier ordering along every `Ref(target)` edge in the rule
//!   call graph, and charges `cross_module_coercion` at every
//!   boundary crossing. A parent tier must dominate its child
//!   (per `EmissionTier::rank()` — `Tape > Lazy > Direct`); a
//!   `Direct` parent calling a `Tape` child is illegal without
//!   an upgrade pass and is pruned by the constraint's `check`.
//!
//! - [`tier`] — `TierFollowsMaterialization` bounds each rule's
//!   `EmissionTier` domain above by the materialization class of
//!   the rule's body. `MustTape` → `{Tape}` only; `TapeSpanOnly`
//!   → `{Tape, Lazy}` (Tier B requires `FixedShape` which
//!   `TapeSpanOnly` does not guarantee); `TransparentElide` →
//!   `{Tape, Lazy, Direct}` (all three legal).
//!
//! # Integration
//!
//! Each sub-module exposes an `install(ctx, csp, ir)` free
//! function that registers its constraint with the shared
//! component CSP. The parent dispatcher in
//! `csp_strategy::mod::solve_grammar_components` (AF.3 Wave 5A)
//! builds the [`ConstraintCtx`] once per component and calls
//! all three installers in sequence after the per-rule
//! variables have been constructed.
//!
//! # Layering
//!
//! These constraints are strictly cross-rule — they add edges
//! between variables owned by *different* rules within the same
//! component. The pre-existing intra-rule constraints
//! (`ImplicationConstraint` wiring an Alt parent to its child
//! regex engines, for instance) remain in
//! `csp_strategy::mod::add_token_dispatch_constraints` and are
//! unaffected. A component with zero `Ref` edges reduces to the
//! degenerate case where every rule body is its own trivial
//! component, and the cross-rule constraints below are no-ops.
//!
//! # Cost weight sources
//!
//! The three constraints draw exclusively from
//! [`egraph::CostWeights`] (via `ir.cost_config.egraph.weights`):
//!
//! - `cross_module_coercion` — priced by [`parent`] at every
//!   Tape → Lazy / Lazy → Direct / Tape → Direct boundary.
//! - `prettify_emission` — not read here directly; the
//!   prettify pin is already baked into the materialization
//!   class via `classify_materialization`, which [`tier`]
//!   consumes transitively.
//! - `dispatch_branch` / `dispatch_table` — not read here;
//!   they drive the Alt-mode domain which is pre-existing CSP
//!   infrastructure.
//! - `tape_push` — not read here; it drives the
//!   materialization class selection upstream, which [`tier`]
//!   reads via `ir.materialization`.

pub mod engine;
pub mod parent;
pub mod tier;

use std::collections::HashMap;

use csp_solver::constraint::VarId;

use crate::dag::NodeId;
use crate::passes::materialization::{EmissionTier, MaterializationClass};
use crate::{GrammarIR, RuleId};

/// Per-component bookkeeping passed to every cross-rule
/// constraint installer.
///
/// Built by the parent dispatcher
/// (`csp_strategy::mod::solve_grammar_components`) before it
/// runs the three installers. Holds the component's rule
/// membership, the reverse rule → component index, and the
/// lookup tables mapping each rule to its per-rule CSP
/// variables (tier / materialization / engine). The installers
/// read from this context exclusively — they never mutate CSP
/// variables directly, and they never construct new ones.
///
/// `engine_vars` is keyed by `(RuleId, NodeId)` because a single
/// rule body may carry multiple `IrNode::Regex` sites, each with
/// its own per-NodeId engine variable. The installers for
/// [`engine`] walk every pair within a component; the installers
/// for [`parent`] and [`tier`] look up per-rule variables only.
pub struct ConstraintCtx<'a> {
    /// The component currently being wired, as a sorted list of
    /// `RuleId`s. Each entry is a rule belonging to this
    /// component per the Y.5 `UnionFind` decomposition.
    pub component: &'a [RuleId],

    /// Per-rule tier variable id (the `EmissionTier` CSP var
    /// owned by the AF.3 solve). A `None` entry means the rule
    /// has no tier variable yet — the installer treats it as
    /// absent and skips any edge touching it.
    pub tier_vars: &'a HashMap<RuleId, VarId>,

    /// Per-rule materialization class. Populated upstream by
    /// `classify_materialization` into `ir.materialization` and
    /// read by [`tier`] to bound the rule's tier domain above.
    /// Keyed by rule body `NodeId` — use
    /// [`Self::rule_materialization`] to project through the
    /// rule root.
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

    /// The legal [`EmissionTier`] set for a rule, given its
    /// materialization class. Used by [`tier`] to clamp the
    /// per-rule tier domain at variable-construction time; used
    /// by [`parent`] to walk the feasible parent tiers when
    /// scoring a call-site edge.
    ///
    /// - `MustTape` → `[Tape]`
    /// - `TapeSpanOnly` → `[Tape, Lazy]`
    /// - `TransparentElide` → `[Tape, Lazy, Direct]`
    pub fn legal_tiers(class: MaterializationClass) -> &'static [EmissionTier] {
        match class {
            MaterializationClass::MustTape => &[EmissionTier::Tape],
            MaterializationClass::TapeSpanOnly => &[EmissionTier::Tape, EmissionTier::Lazy],
            MaterializationClass::TransparentElide => {
                &[EmissionTier::Tape, EmissionTier::Lazy, EmissionTier::Direct]
            }
        }
    }
}
