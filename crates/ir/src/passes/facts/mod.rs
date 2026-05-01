//! `FactAuthority` — durable read surface over the grammar fact substrate.
//!
//! Tranche AZ-III.W3a.1 lands a single read API that consolidates the
//! fact streams produced by the four substrate-bearing pass families
//! (egraph analysis, recognizer mining, node mining, projection) into
//! one named lookup surface. Each accessor below names a concrete
//! production consumer that breaks when the fact is absent, and a
//! disconnect test that proves the consumption.
//!
//! # Why a read surface, not a re-store
//!
//! Pre-W3a.1 every fact lived as a public field on [`crate::GrammarIR`]
//! and every consumer reached in via `ir.<field>.<get>`. That worked,
//! but it left the substrate-with-consumer invariant (AZ-III invariant
//! 3) verifiable only by `rg`-grepping for direct field reads. A
//! refactor that quietly stopped using the field would silently land
//! "substrate without consumer". W3a.1 makes the consumer wiring an
//! API contract: production passes go through this surface and the
//! disconnect tests in `crates/ir/tests/passes/fact_authority.rs`
//! confirm the wiring at every release.
//!
//! # Authority surfaces
//!
//! Each `pub fn` here owns one fact stream. The doc comment names:
//!
//! 1. the **producer site** — the function that fills the underlying
//!    storage on `GrammarIR`;
//! 2. the **consumer site** — the named production caller (or callers)
//!    that reads through this accessor;
//! 3. the **disconnect test** — the test in the IR test suite that
//!    asserts the consumer fails when the storage is cleared.
//!
//! See `docs/benchmarks/AZ-III/W3a-fact-authority.txt` for the full
//! authority manifest.
//!
//! # Construction
//!
//! [`FactAuthority::new`] borrows the IR immutably; the surface holds
//! no state of its own. Callers that need to mutate facts (the
//! producer side) do not go through this surface — they write to the
//! `GrammarIR` field directly. The authority is read-only by design;
//! mutation is producer-local.

use crate::dag::NodeId;
use crate::egraph::EClassFacts;
use crate::passes::MaterializationClass;
use crate::passes::context::ContextFacts;
use crate::passes::patterns::NodeFacts;
use crate::passes::recognizers::shape_dispatch::{ShapeAssignments, ShapeTag};
use crate::{GrammarIR, RuleId};

/// Read-only authority over the fact substrate of a [`GrammarIR`].
///
/// Holds a borrow of the IR; production passes that consume facts pass
/// the IR through `FactAuthority::new(&ir)` and then route every
/// `<fact>.get(<key>)` call through one of the methods below. Each
/// method documents:
///
/// - **Producer**: the pass that fills the underlying storage.
/// - **Consumer**: the named production caller bound to this method.
/// - **Disconnect test**: the test in `crates/ir/tests/passes/` that
///   exercises the consumer with the storage cleared.
///
/// The surface is intentionally small — it does not expose mutation
/// or recomputation. Producers write to `GrammarIR` directly; the
/// authority guarantees the read paths consume what was written.
#[derive(Copy, Clone, Debug)]
pub struct FactAuthority<'a> {
    ir: &'a GrammarIR,
}

impl<'a> FactAuthority<'a> {
    /// Build a read-only authority over the given IR.
    #[inline]
    pub fn new(ir: &'a GrammarIR) -> Self {
        Self { ir }
    }

    /// Underlying IR; exposed for consumers that need to read multiple
    /// fact streams in one call site.
    #[inline]
    pub fn ir(&self) -> &'a GrammarIR {
        self.ir
    }

    // ── Recognizer fact (NodeFacts) ────────────────────────────────────

    /// Per-node structural facts (operator chain / sep_by /
    /// all_span_collapse / recognizer record).
    ///
    /// **Producer**: [`crate::passes::mine_recognizers`] writes
    /// `ir.node_facts` after the unified recognizer-mining walk.
    ///
    /// **Consumer**: [`crate::passes::csp_strategy::solve_grammar_components`]
    /// reads the recognizer shape per Alt / Wrap node to size the
    /// strategy domain. With the substrate cleared, every Alt/Wrap
    /// site falls back to the maximum-domain (no recognizer ⇒ no
    /// `WrapMode::Wrap*` admission), changing the solver output.
    ///
    /// **Disconnect test**: `crates/ir/tests/passes/fact_authority.rs::
    /// node_facts_drives_csp_strategy_domain` clears `ir.node_facts`
    /// and asserts the strategy domain widens.
    #[inline]
    pub fn node_fact(&self, node_id: NodeId) -> Option<&'a NodeFacts> {
        self.ir.node_facts.get(&node_id)
    }

    /// Whether any rule in the IR carries a Tranche-X.10 family
    /// recognizer shape.
    ///
    /// **Producer**: [`crate::passes::mine_recognizers`] sets
    /// `ir.has_family_recognizers` at the tail of the unified walk.
    ///
    /// **Consumer**: `crates/core/src/backend/driver/node.rs::
    /// compile_node` short-circuits the per-node
    /// `try_emit_family_kernel` probe when this is `false`. With the
    /// flag cleared, the probe re-fires on every node — measurably
    /// slower on the JSON / CSS L4 grammars that match zero families.
    #[inline]
    pub fn has_family_recognizers(&self) -> bool {
        self.ir.has_family_recognizers
    }

    // ── Context fact (ContextFacts) ────────────────────────────────────

    /// Per-node propagated context facts (discrimination strength,
    /// scan safety, recovery / token-dispatch role).
    ///
    /// **Producer**:
    /// [`crate::passes::recognizers::ContextFactsMiner`] writes
    /// `ir.context_facts` during the unified
    /// `mine_recognizers` walk (Tranche AF.1 fold).
    ///
    /// **Consumer**: this authority's
    /// [`Self::alt_dispatch_admissible`] gates the AltDispatch shape
    /// detector on the per-Alt discrimination bit. Pre-W3a.1 the
    /// detector reached straight into `ir.context_facts`; W3a.1
    /// routes the read through the authority so the disconnect test
    /// can break it.
    ///
    /// **Disconnect test**: `crates/ir/tests/passes/fact_authority.rs::
    /// context_facts_drive_alt_dispatch` clears `ir.context_facts`
    /// and asserts the dispatch admission flips.
    #[inline]
    pub fn context_fact(&self, node_id: NodeId) -> Option<&'a ContextFacts> {
        self.ir.context_facts.get(&node_id)
    }

    /// Whether the Alt at `node_id` has a `Strong` enough discrimination
    /// signal to admit a dispatch-table lowering.
    ///
    /// **Producer**: see [`Self::context_fact`].
    ///
    /// **Consumer**:
    /// [`crate::passes::recognizers::shape_dispatch`] keyword and
    /// alt_dispatch detectors call this to decide whether an Alt
    /// node is shape-classifiable. Pre-W3a.1 the detector inlined
    /// the read; routing it through the authority makes the consumer
    /// break testably when context_facts is cleared.
    #[inline]
    pub fn alt_dispatch_admissible(&self, node_id: NodeId) -> bool {
        use crate::passes::context::DiscriminationStrength;
        matches!(
            self.context_fact(node_id).map(|f| f.discrimination),
            Some(DiscriminationStrength::Strong)
        )
    }

    // ── Materialization (per-node lattice class) ──────────────────────

    /// Per-node materialization class (TransparentElide / TapeSpanOnly /
    /// MustTape).
    ///
    /// **Producer**: [`crate::passes::classify_materialization`] writes
    /// `ir.materialization` after `project_types`. Refined by the CSP
    /// joint solve.
    ///
    /// **Consumer**:
    /// [`crate::passes::compute_payload_layouts`] gates the per-rule
    /// payload-layout planner on the rule body's materialization
    /// class — without `MustTape` or `TapeSpanOnly`, no layout is
    /// emitted. With the substrate cleared, the layout planner
    /// produces no entries.
    ///
    /// **Disconnect test**: `crates/ir/tests/passes/fact_authority.rs::
    /// materialization_drives_payload_layout` clears
    /// `ir.materialization` and asserts the layout planner output
    /// shrinks.
    #[inline]
    pub fn materialization(&self, node_id: NodeId) -> Option<MaterializationClass> {
        self.ir.materialization.get(&node_id).copied()
    }

    // ── Projection / Payload Layout ───────────────────────────────────

    /// Whether the rule has a registered packed payload layout.
    ///
    /// **Producer**: [`crate::passes::compute_payload_layouts`] writes
    /// `ir.payload_layouts` from per-rule projected types after
    /// `project_types`.
    ///
    /// **Consumer**: `crates/ir/src/passes/recognizers/dta.rs::
    /// emit_rule_dta` consults this to decide whether the rule's
    /// DTA prelude pre-allocates a payload buffer. Without the
    /// substrate the DTA emitter falls back to compound-payload
    /// pathways; the rule still parses but the runtime push-fingerprint
    /// regresses (one extra heap path per rule entry).
    ///
    /// **Disconnect test**: `crates/ir/tests/passes/fact_authority.rs::
    /// payload_layouts_drive_push_fingerprint` clears
    /// `ir.payload_layouts` and asserts the recomputed
    /// `push_fingerprint` reports zero packed-payload sites.
    #[inline]
    pub fn rule_payload_layout(&self, rule: RuleId) -> Option<&'a crate::passes::PayloadLayout> {
        self.ir.payload_layouts.get(&rule)
    }

    // ── Egraph (EClassFacts) ──────────────────────────────────────────

    /// Per-node lattice facts as walked by
    /// [`crate::passes::materialization::compute_eclass_facts`].
    ///
    /// **Producer**:
    /// [`crate::passes::recognizers::mine_recognizers`] populates
    /// `ir.eclass_facts` before its unified walk so downstream
    /// classifier callers can re-read the lattice without recomputing.
    ///
    /// **Consumer**:
    /// [`crate::passes::materialization::classify_materialization_with_facts`]
    /// reads each lattice value through the AF.1
    /// `gate_transparent_elide` step; an empty fact map demotes every
    /// `TransparentElide` candidate to `TapeSpanOnly`.
    ///
    /// **Disconnect test**: `crates/ir/tests/passes/fact_authority.rs::
    /// eclass_facts_gate_transparent_elide` clears
    /// `ir.eclass_facts` and asserts the materialization output
    /// changes.
    #[inline]
    pub fn eclass_facts(&self, node_id: NodeId) -> Option<&'a EClassFacts> {
        self.ir.eclass_facts.get(&node_id)
    }

    // ── Shape dispatch (rule-level classification) ────────────────────

    /// Per-rule shape classification produced by the unified
    /// shape-dispatch detector.
    ///
    /// **Producer**:
    /// [`crate::passes::recognizers::shape_dispatch::shape_dispatch`]
    /// runs at the tail of `mine_recognizers`.
    ///
    /// **Consumer**: `crates/core/src/backend/rust/emitter/shapes/`
    /// modules dispatch codegen by the assigned tag; rules absent
    /// from the assignment fall back to the AX cold-path replay
    /// path. With the assignment cleared the walker fallback runs
    /// for every rule, defeating the per-shape kernel codegen.
    ///
    /// **Disconnect test**: `crates/ir/tests/passes/fact_authority.rs::
    /// shape_assignments_drive_classifier` asserts the assignment
    /// reports the same tags it produced before clear.
    #[inline]
    pub fn shape_assignments(&self) -> &'a ShapeAssignments {
        &self.ir.shape_assignments
    }

    /// Convenience: the shape tag for a single rule.
    #[inline]
    pub fn rule_shape(&self, rule: RuleId) -> ShapeTag {
        self.ir.shape_assignments.get(rule)
    }
}
