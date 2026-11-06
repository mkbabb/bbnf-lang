use std::collections::HashMap;

use bbnf_regex::EngineSet;
use csp_solver::domain::{CostDomain, Domain};

use crate::CostConfig;
use crate::dag::NodeId;
use crate::passes::materialization::MaterializationClass;
use crate::passes::patterns::{Recognizer, RecognizerShape};

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
    // `build_wrap_domain` never added it to the CSP domain. The two consumer
    // sites (`backend::driver::wrap` and `backend::recognizer_plan`)
    // pattern-matched it as a synonym of `BalancedScan`. The
    // forward-`memchr`-to-close emission path is gated on the
    // upstream `delim_scan_configs` sidecar (populated by
    // `delim_scan::collect`) plus a permissive `wrap_mode` check;
    // the per-NodeId `WrapMode` value never carried delim-scan
    // semantics independently. Y.13's consumer-invariant test was
    // updated to drop the variant from its exhaustive match.
}

/// Engine chosen for one canonical regex `NodeId` site.
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
/// [`super::solve_grammar_components`].
#[derive(Clone, Debug, Default, PartialEq, Eq)]
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
/// jointly optimizes strategy decisions (Alt / Wrap / Engine) and
/// tape materialization commitments keyed by canonical root `NodeId`.
///
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StrategyValue {
    Alt(AltMode),
    Wrap(WrapMode),
    Engine(RegexEngine),
    Mat(MaterializationClass),
}

/// Cost-aware finite domain for [`StrategyValue`].
///
/// Canonical values, costs, and order never change. Search mutates only the
/// membership bits, so prune/restore cannot rewrite the objective or tie order.
#[derive(Clone, Debug, PartialEq)]
pub struct StrategyDomain {
    pub(super) entries: Vec<(StrategyValue, f64)>,
    active: Vec<bool>,
}

impl StrategyDomain {
    pub(super) fn new(value_costs: Vec<(StrategyValue, f64)>) -> Self {
        let active = vec![true; value_costs.len()];
        Self {
            entries: value_costs,
            active,
        }
    }

    fn canonical_index(&self, value: &StrategyValue) -> Option<usize> {
        self.entries
            .iter()
            .position(|(candidate, _)| candidate == value)
    }

    pub(super) fn valid(&self) -> bool {
        self.entries.len() == self.active.len()
            && self.entries.iter().all(|(_, cost)| cost.is_finite())
            && self.entries.iter().enumerate().all(|(index, (value, _))| {
                !self.entries[..index]
                    .iter()
                    .any(|(prior, _)| prior == value)
            })
    }

    /// Conjoin legality from another occurrence of the same semantic site.
    /// Costs for a shared value must be bit-identical; values legal in only one
    /// occurrence become inactive.
    pub(super) fn intersect(&mut self, other: &Self) -> Result<(), DomainIntersectionError> {
        if !self.valid() || !other.valid() {
            return Err(DomainIntersectionError::InvalidDomain);
        }
        for (value, cost) in &self.entries {
            if let Some(other_index) = other.canonical_index(value)
                && other.entries[other_index].1.to_bits() != cost.to_bits()
            {
                return Err(DomainIntersectionError::CostMismatch);
            }
        }
        for (index, (value, _)) in self.entries.iter().enumerate() {
            self.active[index] &= other
                .canonical_index(value)
                .is_some_and(|other_index| other.active[other_index]);
        }
        Ok(())
    }

    pub(super) fn active_costs(&self) -> impl Iterator<Item = (&StrategyValue, f64)> {
        self.entries
            .iter()
            .zip(&self.active)
            .filter_map(|((value, cost), active)| active.then_some((value, *cost)))
    }
}

pub(super) enum DomainIntersectionError {
    InvalidDomain,
    CostMismatch,
}

impl Domain for StrategyDomain {
    type Value = StrategyValue;

    fn size(&self) -> usize {
        self.active.iter().filter(|active| **active).count()
    }

    fn contains(&self, val: &StrategyValue) -> bool {
        self.canonical_index(val)
            .is_some_and(|index| self.active[index])
    }

    fn remove(&mut self, val: &StrategyValue) -> bool {
        let Some(index) = self.canonical_index(val) else {
            return false;
        };
        std::mem::replace(&mut self.active[index], false)
    }

    fn add(&mut self, val: &StrategyValue) {
        if let Some(index) = self.canonical_index(val) {
            self.active[index] = true;
        }
    }

    fn values(&self) -> Vec<StrategyValue> {
        self.active_costs()
            .map(|(value, _)| value.clone())
            .collect()
    }
}

impl CostDomain for StrategyDomain {
    fn cost(&self, val: &StrategyValue) -> f64 {
        self.canonical_index(val)
            .map(|index| self.entries[index].1)
            .unwrap_or(f64::INFINITY)
    }
}

// ── Domain construction ─────────────────────────────────────────────────────

/// Build the cost-weighted domain for an `Alt` decision variable.
///
/// All architectural fallbacks (Checkpoint) stay in the domain at high
/// cost. Feasible faster strategies (ByteDispatch, KeyDispatch) are
/// added at a cost computed from the AF.2 per-arm formula:
///
/// ```text
///     dispatch_cost = arm_count * dispatch_branch + dispatch_table
/// ```
///
/// This replaces the pre-AG.5 flat `dispatch_bonus.abs()` placeholder
/// with the two-component formula the cost_weights_unified contract
/// tests depend on. Each dimension (`dispatch_branch`,
/// `dispatch_table`) is independently perturbable, and the per-arm
/// scaling makes the cost model correctly sensitive to alternation
/// width — a 100-arm Alt pays proportionally more than a 3-arm Alt.
pub(super) fn build_alt_domain(
    fact: Option<&Recognizer>,
    has_byte_dispatch: bool,
    has_key_dispatch: bool,
    arm_count: usize,
    cfg: &CostConfig,
) -> StrategyDomain {
    let w = &cfg.egraph.weights;

    // AG.5 — dispatch cost via the per-arm formula. Defaults:
    // `dispatch_branch = 0.0`, `dispatch_table = 0.0`, so under
    // default weights the dispatch cost is 0.0 — strictly cheaper
    // than the Checkpoint fallback (10.0), preserving the existing
    // decision. When either knob is pushed to an extreme in the
    // AF.2 contract tests, the formula dominates the checkpoint
    // cost and flips the decision.
    let dispatch_cost = (arm_count as f64) * w.dispatch_branch + w.dispatch_table;

    // AZ-IV.W4.2 — when the upstream structural detectors
    // authoritatively populated a key-dispatch sidecar payload
    // (`key_dispatch_configs` / `keyword_branches`) the strategy
    // is structurally fixed; the CSP has no remaining choice. Build
    // the domain as a singleton so the variable becomes a unit-domain
    // pin without invoking branch-and-bound search. This matches the
    // production reality (the `dispatch::install` constraint pins the
    // value regardless) and keeps CSS L4's largest component from
    // exhausting the 1M-node budget proving the pinned answer.
    if has_key_dispatch {
        return StrategyDomain::new(vec![(
            StrategyValue::Alt(AltMode::KeyDispatch),
            dispatch_cost,
        )]);
    }

    let mut values: Vec<(StrategyValue, f64)> = Vec::with_capacity(4);

    // Universal fallback — always legal, highest cost.
    values.push((
        StrategyValue::Alt(AltMode::Checkpoint),
        10.0 * cfg.literal_cost,
    ));

    if has_byte_dispatch {
        values.push((StrategyValue::Alt(AltMode::ByteDispatch), dispatch_cost));
    }

    if let Some(rec) = fact {
        // Tranche Y.3: TokenLedBranches folds into ByteDispatch. The
        // previous code added a duplicate entry at the same cost
        // weight; the cost model is unchanged but the codepath is
        // unified. `fuse_token_dispatch` converts the strongest
        // TokenLed shapes into `IrNode::TokenDispatch` upstream
        // regardless of the CSP choice.
        if matches!(rec.shape, RecognizerShape::TokenLedBranches { .. }) && !has_byte_dispatch {
            values.push((StrategyValue::Alt(AltMode::ByteDispatch), dispatch_cost));
        }
        if matches!(rec.shape, RecognizerShape::KeywordPrefix { .. }) {
            values.push((StrategyValue::Alt(AltMode::KeyDispatch), dispatch_cost));
        }
    }

    StrategyDomain::new(values)
}

/// Build the cost-weighted domain for a `Wrap` decision variable.
pub(super) fn build_wrap_domain(fact: Option<&Recognizer>, cfg: &CostConfig) -> StrategyDomain {
    let mut values: Vec<(StrategyValue, f64)> = Vec::with_capacity(4);

    // Universal fallback.
    values.push((
        StrategyValue::Wrap(WrapMode::Generic),
        10.0 * cfg.literal_cost,
    ));

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
pub(super) fn build_materialization_domain(
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
pub(super) fn build_engine_domain(feasible: EngineSet, cfg: &CostConfig) -> StrategyDomain {
    let dispatch_bonus = cfg.egraph.weights.dispatch_bonus.abs();
    let mut values: Vec<(StrategyValue, f64)> = Vec::with_capacity(8);

    if feasible.contains(EngineSet::FAMILY_HELPER) {
        values.push((
            StrategyValue::Engine(RegexEngine::FamilyHelper),
            dispatch_bonus,
        ));
    }
    if feasible.contains(EngineSet::MEMCHR1) {
        values.push((
            StrategyValue::Engine(RegexEngine::Memchr1),
            dispatch_bonus + 0.1,
        ));
    }
    if feasible.contains(EngineSet::MEMCHR2) {
        values.push((
            StrategyValue::Engine(RegexEngine::Memchr2),
            dispatch_bonus + 0.2,
        ));
    }
    if feasible.contains(EngineSet::MEMCHR3) {
        values.push((
            StrategyValue::Engine(RegexEngine::Memchr3),
            dispatch_bonus + 0.3,
        ));
    }
    if feasible.contains(EngineSet::NIBBLE_LUT) {
        values.push((
            StrategyValue::Engine(RegexEngine::NibbleLut),
            dispatch_bonus + 0.5,
        ));
    }
    if feasible.contains(EngineSet::ONE_PASS) {
        values.push((
            StrategyValue::Engine(RegexEngine::OnePass),
            dispatch_bonus + 1.0,
        ));
    }
    if feasible.contains(EngineSet::SMALL_DFA) {
        values.push((
            StrategyValue::Engine(RegexEngine::SmallDfa),
            dispatch_bonus + 2.0,
        ));
    }
    // DFA is the universal fallback — always legal even when feasible
    // is empty (e.g., complex patterns with no narrower engine).
    values.push((
        StrategyValue::Engine(RegexEngine::Dfa),
        dispatch_bonus + 5.0,
    ));

    StrategyDomain::new(values)
}
