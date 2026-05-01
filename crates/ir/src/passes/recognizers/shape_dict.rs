//! `ShapeDictMiner` — Tranche AV.5.2 mining pass for fixed-shape
//! compound subtrees; AW-IV.W3.1 recalibrated to the recurring-shape
//! signature model.
//!
//! Walks every IR node in the unified `mine_recognizers` walk and
//! emits a [`ShapeTemplate`] for each compound subtree whose
//! per-child-discriminant signature recurs enough across the grammar
//! to earn a runtime `ShapeRef` dictionary slot.
//!
//! # AW-IV.W3.1 — recalibration (pre-walker IR, discriminant-only)
//!
//! Pre-W3.1 the eligibility gated on propagated `EClassFacts`
//! (`closure_free`, `is_fixed_shape`, `all_descendants_elidable`). The
//! lattice's `closure_free` bit propagates `false` up through every
//! `Map { inner, .. }` child — so every CSS / JSON rule body carrying
//! a downstream typed projection (`-> f64`, `-> String`, `-> <named>`)
//! was demoted, even when the Seq's direct children were structural
//! (Literal + Ref + OptionalWhitespace). Net yield: CSS L4 ≤ 28
//! candidates (only the rare all-structural Seq bodies), JSON 0, Sheets
//! 0. The runtime `GrammarProfile::shape_dict` slot emitted as
//! `&[]` for every non-BBNF grammar despite the mining substrate
//! existing.
//!
//! W3.1 recalibrates to the **recurring-shape signature** model: a
//! shape is a hashable canonicalisation of a compound's child
//! discriminant sequence (Literal / LeafHole / Whitespace / Epsilon),
//! *ignoring* the specific `StringId` or referenced rule. If the same
//! signature recurs at three or more grammar sites, the compound is a
//! ShapeRef candidate. Safety is provided by the recurrence heuristic
//! (shared discriminant shape implies the collapsed runtime skeleton
//! re-expands uniformly) rather than the strict lattice bits.
//!
//! # Output shape
//!
//! Each match writes a `(NodeId, ShapeTemplate)` pair into
//! [`MineOutputs::shape_dict_templates`]. The template carries:
//!
//! - `skeleton` — the per-direct-child kind list (the bytes the
//!   tape cursor reads as `ShapeEntry::child_kinds` at view time).
//! - `leaf_holes` — the [`TypeDesc`] of each non-constant leaf
//!   position in source order. Constant positions (literals,
//!   epsilons, structural separators) are absent from this list.
//! - `shape_hash` — canonical 64-bit discriminant-only hash over the
//!   skeleton, stable across compile sessions. W3.1 drops the per-
//!   `StringId` payload from the hash so two Seq bodies differing
//!   only in their literal text hash equal — the recurring-shape
//!   recognition depends on this canonicalisation.
//!
//! Downstream the CSP pass (`constraints/shape_dict.rs`) selects a
//! subset of templates to admit to the per-grammar dictionary under
//! the 256-entry budget; the emitter then bakes the chosen templates
//! into `GrammarProfile::shape_dict` and the DTA emits ShapeRef on
//! `shape_hash` match.

use std::hash::{Hash, Hasher};

use rustc_hash::FxHasher;

use crate::dag::NodeId;
use crate::types::TypeDesc;
use crate::{IrNode, StringId};

use super::{MineOutputs, RecognizerMineCtx, RecognizerMiner};

/// A compile-time shape-template candidate.
///
/// Emitted by [`ShapeDictMiner`] for every fixed-shape compound
/// subtree the e-graph facts admit. The CSP shape-dict constraint
/// later selects a budget-bounded subset; the emitter bakes the
/// chosen templates as `ShapeEntry` entries in
/// `GrammarProfile::shape_dict`.
#[derive(Clone, Debug)]
pub struct ShapeTemplate {
    /// Canonical 64-bit hash over the template skeleton. Stable
    /// across compile sessions (FxHasher over [`TemplatePiece`]
    /// discriminants + ids).
    pub shape_hash: u64,
    /// Per-position skeleton describing the structural shape.
    pub skeleton: Vec<TemplatePiece>,
    /// Per-leaf-hole [`TypeDesc`], in source order. The list length
    /// equals the number of non-constant leaf positions in
    /// `skeleton`.
    pub leaf_holes: Vec<TypeDesc>,
    /// Emission cost estimate (bytes saved per dictionary hit). Set
    /// when the CSP cost model evaluates the candidate; zero on
    /// miner output.
    pub savings_bytes: u32,
}

/// One position in a [`ShapeTemplate`] skeleton.
///
/// The skeleton is a flat pre-order list of every direct-child
/// position the collapsed compound would have emitted. Each piece
/// names what the tape cursor would have read at that position.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TemplatePiece {
    /// A constant literal — the source bytes are known statically.
    Literal(StringId),
    /// A non-constant leaf hole — the source span varies per
    /// instance and is read from the packed payload blob.
    LeafHole,
    /// A structural separator (whitespace, optional whitespace).
    /// The tape cursor inherits the parent span.
    Whitespace,
    /// Epsilon — zero-width position.
    Epsilon,
}

/// Per-grammar map of `(NodeId, ShapeTemplate)` candidates emitted
/// by [`ShapeDictMiner`].
pub type ShapeDictMap = Vec<(NodeId, ShapeTemplate)>;

// ── Miner implementation ─────────────────────────────────────────

/// The mining pass. Folds into the single-walk recognizer
/// orchestrator alongside the other ten miners; runs at every node.
pub struct ShapeDictMiner;

impl RecognizerMiner for ShapeDictMiner {
    fn inspect(
        &self,
        _node: &IrNode,
        node_id: NodeId,
        ctx: &RecognizerMineCtx,
        outputs: &mut MineOutputs,
    ) {
        // AW-IV.W3.1 — emit templates keyed by rule-body roots only.
        //
        // The downstream projection at
        // [`crate::passes::profile::GrammarIR::profile`] resolves each
        // admitted template to an owning `rule_id` by matching the
        // template's NodeId against `dag.node_for(&rule.body)`.
        // Interior-subtree NodeIds never match a rule body root, so
        // pre-W3.1 interior templates were silently dropped on their
        // way to the runtime `GRAMMAR_PROFILE.shape_dict`.
        //
        // W3.1 anchors every emission at a rule body root — either the
        // rule body IS the mineable compound, or a transparent wrapper
        // (`Map { inner, .. }` / `OptionalWhitespace(inner)`) peels
        // down to one. The shape signature is computed from the peeled
        // structural root; the template's NodeId is the rule body's
        // own DAG id so the projection filter keeps every emission.
        // That closes the wire contract from mining through selection
        // to the per-grammar `GRAMMAR_PROFILE.shape_dict` literal.
        let Some(dag) = ctx.ir.dag.as_ref() else {
            return;
        };
        let rule_body_for_node = ctx.ir.rules.iter().find_map(|rule| {
            if dag.node_for(&rule.body) == Some(node_id) {
                Some(&rule.body)
            } else {
                None
            }
        });
        let Some(rule_body) = rule_body_for_node else {
            return;
        };

        // Peel transparent wrappers — `Map { inner, .. }` and
        // `OptionalWhitespace(inner)` preserve semantic shape but
        // hide the structural compound underneath. The skeleton is
        // derived from the structural root; the NodeId emitted is
        // still the rule body's original id (so the profile filter
        // keeps it).
        let structural_root = peel_structural_wrappers(rule_body);

        // Eligibility gate — only Seq / Skip / Next compounds qualify
        // as ShapeRef candidates.
        if !is_eligible_compound(structural_root, node_id, ctx) {
            return;
        }

        // Build the skeleton from the direct children.
        let skeleton = match build_skeleton(structural_root, ctx) {
            Some(skel) => skel,
            None => return,
        };

        // Reject trivial templates that wouldn't beat their dict
        // entry overhead — single-piece skeletons or all-constant
        // skeletons gain nothing from collapse.
        if skeleton.is_empty() || skeleton_is_trivial(&skeleton) {
            return;
        }

        // Collect leaf-hole types in source order.
        let leaf_holes = collect_leaf_holes(structural_root, ctx);

        // Compile the canonical shape hash.
        let shape_hash = hash_skeleton(&skeleton, &leaf_holes);

        outputs.shape_dict_templates.push((
            node_id,
            ShapeTemplate {
                shape_hash,
                skeleton,
                leaf_holes,
                savings_bytes: 0,
            },
        ));
    }
}

// ── Transparent wrapper peeling ───────────────────────────────────

/// Peel transparent wrappers to reach the structural compound.
///
/// `Map { inner, .. }` / `OptionalWhitespace(inner)` carry no
/// per-position structural contribution — the `->` map expression
/// lives in the leaf-hole's `TypeDesc` lane, the `?w` wrapper
/// contributes a single whitespace skip bracketing the inner shape.
/// Peeling reveals the Seq / Skip / Next the walker will actually
/// emit compound records for.
fn peel_structural_wrappers(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            peel_structural_wrappers(inner)
        }
        other => other,
    }
}

// ── Eligibility ───────────────────────────────────────────────────

/// True iff `node` is a compound whose **local** structural shape
/// admits ShapeRef collapse.
///
/// # AW-IV.W3.1 — recurring-signature model
///
/// Pre-W3.1 the eligibility gated on the propagated
/// [`crate::egraph::EClassFacts::closure_free`] bit. That bit
/// `&&`-propagates from every `Map { inner, .. }` child — and every
/// typed-projection `-> <native>` rule body is a Map — so the gate
/// rejected virtually every CSS / JSON / Sheets Seq body despite
/// those being the canonical ShapeRef targets.
///
/// W3.1 drops the `closure_free` gate: recurring-signature recognition
/// proves safety by construction. If the same child-discriminant
/// sequence appears at ≥ 3 grammar sites, the collapsed runtime
/// skeleton re-expands uniformly, and the per-child Map typed
/// projections survive as independent per-rule `push_leaf_with_*`
/// sites inside the ShapeRef's synthetic child iteration.
/// [`build_skeleton`] + [`emit_position`] still reject shapes whose
/// per-position kinds can't be expressed in the skeleton alphabet
/// (Alt-rooted compounds, variable-bound Repeats); those shapes never
/// hash in the first place.
fn is_eligible_compound(node: &IrNode, _node_id: NodeId, _ctx: &RecognizerMineCtx) -> bool {
    // Only compound shapes at the top level are dictionary
    // candidates. Pure leaves already collapse to a single tape
    // record; Alt / Repeat (variable-bound) / Map / TokenDispatch
    // carry per-instance state that cannot be skeletonised.
    match node {
        IrNode::Seq(children) if !children.is_empty() => true,
        IrNode::Skip(_, _) | IrNode::Next(_, _) => true,
        _ => false,
    }
}

// ── Skeleton construction ─────────────────────────────────────────

/// Walk the direct children of a compound node and emit one
/// [`TemplatePiece`] per child position. Returns `None` when the
/// compound contains a child shape the template cannot represent
/// (e.g. nested Alt / variable Repeat).
fn build_skeleton(node: &IrNode, ctx: &RecognizerMineCtx) -> Option<Vec<TemplatePiece>> {
    let mut skeleton = Vec::new();
    walk_compound_children(node, ctx, &mut skeleton)?;
    Some(skeleton)
}

/// Pre-order child walk. Flattens nested Skip / Next chains so the
/// resulting skeleton is a flat per-position list.
///
/// # AW-IV.W3.1 — transparent wrappers
///
/// `Map { inner, .. }` and `OptionalWhitespace(inner)` are transparent
/// at the skeleton level when they wrap a compound (their own
/// structural contribution is elided by the enclosing walk); when the
/// inner is a non-compound leaf we fall through to the position
/// classifier so the wrapper contributes exactly one position.
fn walk_compound_children(
    node: &IrNode,
    ctx: &RecognizerMineCtx,
    skeleton: &mut Vec<TemplatePiece>,
) -> Option<()> {
    match node {
        IrNode::Seq(children) => {
            for c in children {
                emit_position(c, ctx, skeleton)?;
            }
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) => {
            emit_position(a, ctx, skeleton)?;
            emit_position(b, ctx, skeleton)?;
        }
        // AW-IV.W3.1 — Map / OptionalWhitespace are transparent at the
        // structural level: `?w` emits a WsTrim state but no tape
        // record; `Map` carries the `->` typed projection but no
        // structural position. Recurse into inner so the walk reaches
        // the compound's real children. When the inner is itself a
        // leaf the compound is reduced to one position via
        // [`emit_position`].
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => match inner.as_ref() {
            IrNode::Seq(_)
            | IrNode::Skip(_, _)
            | IrNode::Next(_, _)
            | IrNode::Map { .. }
            | IrNode::OptionalWhitespace(_) => walk_compound_children(inner, ctx, skeleton)?,
            _ => emit_position(inner, ctx, skeleton)?,
        },
        _ => return None,
    }
    Some(())
}

/// Classify a single position as one of the four piece kinds.
///
/// # AW-IV.W3.1 — relaxed position classification
///
/// Pre-W3.1 the `Alt` / `Repeat` / `Negate` / `Minus` / `TokenDispatch`
/// arms returned `None` and demoted the entire candidate. That
/// rejected the canonical CSS declaration shape (`<name> , ":" ?w ,
/// (value ?w)* , importantSuffix , ";"?`) along with most Sheets
/// expression bodies — both of which carry a `Repeat` position that
/// the ShapeRef's synthetic child iteration can re-derive from the
/// outer `(span_lo, span_hi)` pair.
///
/// W3.1 folds those compound positions into [`TemplatePiece::LeafHole`]:
/// the ShapeRef packed-payload blob carries the sub-span per variable
/// position, and the view-layer `ShapeRefChildIter` re-invokes the
/// enclosed rule at the recorded offset. The runtime semantics are
/// preserved; what changes is the discriminant recognition is
/// discriminant-based, not structurally-total.
fn emit_position(
    node: &IrNode,
    ctx: &RecognizerMineCtx,
    skeleton: &mut Vec<TemplatePiece>,
) -> Option<()> {
    match node {
        IrNode::Literal(sid) => {
            skeleton.push(TemplatePiece::Literal(*sid));
        }
        IrNode::Epsilon => {
            skeleton.push(TemplatePiece::Epsilon);
        }
        IrNode::Regex(_) => {
            // Regex match → leaf hole (the matched span varies).
            skeleton.push(TemplatePiece::LeafHole);
        }
        IrNode::Ref(_) => {
            // Reference to another rule → leaf hole; the rule's
            // tape offset is packed into the payload blob.
            skeleton.push(TemplatePiece::LeafHole);
        }
        IrNode::OptionalWhitespace(inner) => {
            // AW-IV.W3.1 — `?w(x)` is transparent for shape purposes:
            // the `?w` wrapper contributes a WsTrim state to the
            // walker but no tape record, so the shape is derived from
            // the inner. Delegate to `emit_position(inner)` directly.
            emit_position(inner, ctx, skeleton)?;
        }
        // Nested compounds flatten in pre-order. Deep recursion is
        // bounded by the IR's e-class depth.
        IrNode::Seq(_) | IrNode::Skip(_, _) | IrNode::Next(_, _) => {
            walk_compound_children(node, ctx, skeleton)?;
        }
        IrNode::Map { inner, .. } => {
            // Map wraps a single inner expression; emit the inner
            // position (the map's typed projection lives in the
            // leaf-hole's TypeDesc).
            emit_position(inner, ctx, skeleton)?;
        }
        // AW-IV.W3.1 — Alt / Repeat / Negate / Minus / TokenDispatch
        // collapse to a single LeafHole. The ShapeRef's synthetic
        // child iteration re-derives the sub-span from the packed
        // payload's `(lo, hi)` pair; the per-position variability the
        // pre-W3.1 miner rejected is now representable.
        IrNode::Alt(_, _)
        | IrNode::Repeat { .. }
        | IrNode::Negate(_)
        | IrNode::Minus(_, _)
        | IrNode::TokenDispatch { .. } => {
            skeleton.push(TemplatePiece::LeafHole);
        }
    }
    Some(())
}

// ── Leaf-hole TypeDesc collection ─────────────────────────────────

/// Collect the [`TypeDesc`] of every non-constant leaf position in
/// `node` in source order. Constant pieces (literals, whitespace,
/// epsilon) contribute nothing.
fn collect_leaf_holes(node: &IrNode, ctx: &RecognizerMineCtx) -> Vec<TypeDesc> {
    let mut holes = Vec::new();
    collect_holes_recursive(node, ctx, &mut holes);
    holes
}

fn collect_holes_recursive(node: &IrNode, ctx: &RecognizerMineCtx, holes: &mut Vec<TypeDesc>) {
    // AW-IV.W3.1 — mirrors [`walk_compound_children`] /
    // [`emit_position`] in lockstep so the skeleton's per-position
    // LeafHole count equals `holes.len()` for every template. Pre-W3.1
    // the two walks diverged on `?w(compound)` which left `leaf_holes`
    // longer than the skeleton's hole count and tripped the parity
    // test at `crates/core/tests/shape_dict_css.rs::
    // css_l4_admitted_templates_contain_leaf_holes`.
    match node {
        IrNode::Regex(_) => holes.push(TypeDesc::Span),
        IrNode::Ref(rule) => {
            // Look up the referenced rule's typed projection from
            // the IR's `types` table. Default to Span when absent.
            let ty = ctx
                .ir
                .types
                .iter()
                .find_map(|(rid, t)| if rid == rule { Some(t.clone()) } else { None })
                .unwrap_or(TypeDesc::Span);
            holes.push(ty);
        }
        IrNode::Seq(children) => {
            for c in children {
                collect_holes_recursive(c, ctx, holes);
            }
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) => {
            collect_holes_recursive(a, ctx, holes);
            collect_holes_recursive(b, ctx, holes);
        }
        // Transparent wrappers — `Map` and `OptionalWhitespace`
        // contribute nothing structural; delegate to the inner.
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            collect_holes_recursive(inner, ctx, holes);
        }
        // AW-IV.W3.1 — Alt / Repeat / Negate / Minus / TokenDispatch
        // contribute one opaque Span hole each (parity with
        // [`emit_position`]'s single-LeafHole emission).
        IrNode::Alt(_, _)
        | IrNode::Repeat { .. }
        | IrNode::Negate(_)
        | IrNode::Minus(_, _)
        | IrNode::TokenDispatch { .. } => holes.push(TypeDesc::Span),
        // Constants carry no per-instance variability.
        IrNode::Literal(_) | IrNode::Epsilon => {}
    }
}

// ── Triviality check ─────────────────────────────────────────────

/// True iff a skeleton is trivial enough that ShapeRef collapse
/// would not amortize the dictionary entry's static cost.
///
/// A skeleton is trivial when it has fewer than 2 pieces (single
/// leaf hole or single literal) or when it contains no leaf holes
/// (an all-constant skeleton already collapses to a single Span via
/// the existing all_span_collapse path).
fn skeleton_is_trivial(skeleton: &[TemplatePiece]) -> bool {
    if skeleton.len() < 2 {
        return true;
    }
    !skeleton
        .iter()
        .any(|p| matches!(p, TemplatePiece::LeafHole))
}

// ── Canonical hashing ─────────────────────────────────────────────

/// Compute the canonical 64-bit hash over a skeleton's per-position
/// discriminants.
///
/// # AW-IV.W3.1 — discriminant-only canonicalisation
///
/// Pre-W3.1 the hash mixed the per-`Literal` [`StringId`]: two Seq
/// bodies differing only in their literal text (CSS `"color" , ":" ,
/// ...` vs `"font" , ":" , ...`) hashed distinctly even though the
/// downstream tape skeleton is identical — three structural records
/// regardless of which property the literal spells. That defeated
/// recurring-shape recognition; every declaration hashed to its own
/// slot and the 32-entry dictionary budget saturated on CSS's ~90
/// per-property bodies without any savings.
///
/// W3.1 drops `StringId` from the hash. A literal byte is encoded
/// solely by its discriminant tag; Ref / Regex / Alt / Repeat leaves
/// collapse to `TemplatePiece::LeafHole` (already discriminant-tag
/// `1u8`). Two compound subtrees with the same child-discriminant
/// sequence hash equal — this is the condition for recurring-shape
/// admission. The walker's runtime lookup (`SHAPE_DICT.lookup(
/// shape_hash)` at `emit_seq_arm`) reads the same canonical hash,
/// closing the wire contract from mining → selection → emit → runtime.
///
/// The [`TypeDesc`] leaf-holes list is hashed by discriminant only —
/// preserved from the pre-W3.1 shape so typed-hole variants (Span vs
/// F64 vs String) still distinguish templates whose structural shape
/// is identical but whose payload lanes differ.
fn hash_skeleton(skeleton: &[TemplatePiece], leaf_holes: &[TypeDesc]) -> u64 {
    let mut hasher = FxHasher::default();
    skeleton.len().hash(&mut hasher);
    for piece in skeleton {
        match piece {
            TemplatePiece::Literal(_) => {
                // AW-IV.W3.1 — discriminant-only hash. The StringId
                // is intentionally not mixed so Seq([Literal, Ref,
                // Literal]) at every CSS declaration rule hashes
                // equal regardless of the literal's spelling.
                0u8.hash(&mut hasher);
            }
            TemplatePiece::LeafHole => {
                1u8.hash(&mut hasher);
            }
            TemplatePiece::Whitespace => {
                2u8.hash(&mut hasher);
            }
            TemplatePiece::Epsilon => {
                3u8.hash(&mut hasher);
            }
        }
    }
    leaf_holes.len().hash(&mut hasher);
    for ty in leaf_holes {
        std::mem::discriminant(ty).hash(&mut hasher);
    }
    hasher.finish()
}

// ── Re-export for cross-module hashing ───────────────────────────

/// Public entry point for the discriminant-only skeleton hash.
///
/// Consumed by the walker emitter's [`crate::backend::rust::emitter::
/// dta_walker::lower_state::emit_seq_arm`] (AW-IV.W3.1) to compute
/// the shape hash of a Seq state's child discriminants at codegen
/// time. The hash must match the miner's output for the runtime
/// `SHAPE_DICT.lookup(shape_hash)` wire contract to close.
///
/// Lives in the IR crate alongside the miner so miner and emitter
/// share one canonical hashing routine. The emitter projects
/// per-`DtaState` children to [`TemplatePiece`] values and calls
/// through.
pub fn hash_skeleton_public(skeleton: &[TemplatePiece], leaf_holes: &[TypeDesc]) -> u64 {
    hash_skeleton(skeleton, leaf_holes)
}
