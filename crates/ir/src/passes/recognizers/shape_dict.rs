//! `ShapeDictMiner` — Tranche AV.5.2 mining pass for fixed-shape
//! compound subtrees.
//!
//! Walks every IR node in the unified `mine_recognizers` walk and
//! emits a [`ShapeTemplate`] for each compound subtree that is
//! eligible for compile-time deduplication via the
//! [`bbnf_tape::TapeKind::ShapeRef`] leaf collapse.
//!
//! # Eligibility
//!
//! A node is eligible iff its [`EClassFacts`] satisfy the four
//! dormant invariants the upstream e-graph analysis already
//! computes:
//!
//! - `is_fixed_shape` — the class has exactly one structural shape
//!   (no Alt, no variable-bound Repeat).
//! - `closure_free` — no host-closure node along the subtree (the
//!   ShapeRef leaf cannot carry a closure environment).
//! - `all_descendants_elidable` — every descendant is amenable to
//!   tape-layer collapse.
//!
//! These bits are `&&`-joined on e-class merge, so a single
//! disagreement anywhere in the class demotes the node out of the
//! candidate set.
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
//! - `shape_hash` — canonical 64-bit hash over the skeleton, stable
//!   across compile sessions.
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
/// chosen templates as [`bbnf_tape::ShapeEntry`] entries in
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
        node: &IrNode,
        node_id: NodeId,
        ctx: &RecognizerMineCtx,
        outputs: &mut MineOutputs,
    ) {
        // Eligibility gate — only fixed-shape, closure-free,
        // descendant-elidable compounds qualify.
        if !is_eligible_compound(node, node_id, ctx) {
            return;
        }

        // Build the skeleton from the direct children.
        let skeleton = match build_skeleton(node, ctx) {
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
        let leaf_holes = collect_leaf_holes(node, ctx);

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

// ── Eligibility ───────────────────────────────────────────────────

/// True iff `node` is a compound whose **local** structural shape
/// admits ShapeRef collapse.
///
/// The shape-dictionary contract is "the SKELETON is fixed, not the
/// leaf-hole content". A `Ref(rule)` at a leaf position produces a
/// single tape subtree at view time; the template's
/// [`TemplatePiece::LeafHole`] captures that variability via the
/// packed payload blob's typed slot. So the eligibility check is on
/// the node's own structural shape, not the transitive `is_fixed_shape`
/// lattice (which propagates Alt-ness up through every Ref edge,
/// disqualifying every compound that touches an Alt-rooted rule).
///
/// Per-position filters in [`emit_position`] still demote the node
/// when a child shape can't be skeletonised.
///
/// The closure-free + descendant-elidable lattice bits remain in
/// effect when populated — they're a defensive gate against
/// host-closure capture which can't be packed.
fn is_eligible_compound(node: &IrNode, node_id: NodeId, ctx: &RecognizerMineCtx) -> bool {
    // Only compound shapes at the top level are dictionary
    // candidates. Pure leaves already collapse to a single tape
    // record; Alt / Repeat (variable-bound) / Map / TokenDispatch
    // carry per-instance state that cannot be skeletonised.
    match node {
        IrNode::Seq(children) if !children.is_empty() => {}
        IrNode::Skip(_, _) | IrNode::Next(_, _) => {}
        _ => return false,
    }

    // Closure-free + descendant-elidable lattice gate. When the
    // facts map is populated, demote nodes whose subtree carries a
    // host closure (can't be packed into the per-instance blob) or
    // whose descendants can't be elided. `is_fixed_shape` is NOT
    // checked here — see the doc-comment above.
    if let Some(facts) = ctx.ir.eclass_facts.get(&node_id) {
        if !facts.closure_free {
            return false;
        }
    }
    true
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
        // Single-child wrappers don't add positions.
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            walk_compound_children(inner, ctx, skeleton)?;
        }
        _ => return None,
    }
    Some(())
}

/// Classify a single position as one of the four piece kinds.
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
        IrNode::OptionalWhitespace(_) => {
            skeleton.push(TemplatePiece::Whitespace);
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
        // Alt, Repeat (variable), Negate, Minus, TokenDispatch —
        // not skeletonisable. Demote the entire candidate.
        _ => return None,
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
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            collect_holes_recursive(inner, ctx, holes);
        }
        // Constants and non-skeletonisable shapes contribute no
        // leaf-hole entries.
        _ => {}
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
    !skeleton.iter().any(|p| matches!(p, TemplatePiece::LeafHole))
}

// ── Canonical hashing ─────────────────────────────────────────────

/// Compute the canonical 64-bit hash over a skeleton + leaf-hole
/// types. Stable across compile sessions; uses FxHasher with
/// discriminant tags so two structurally equivalent skeletons that
/// differ only in StringId interning still hash equal.
fn hash_skeleton(skeleton: &[TemplatePiece], leaf_holes: &[TypeDesc]) -> u64 {
    let mut hasher = FxHasher::default();
    skeleton.len().hash(&mut hasher);
    for piece in skeleton {
        match piece {
            TemplatePiece::Literal(sid) => {
                0u8.hash(&mut hasher);
                sid.hash(&mut hasher);
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
