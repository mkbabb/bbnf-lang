//! `classify_materialization` — the bottom-up classification pass.
//!
//! Tranche AB.0. Walks every rule's IR tree and assigns each
//! [`NodeId`] a [`MaterializationClass`] via four sub-passes:
//!
//! 1. **E-graph fact pre-seed** — walk every rule body bottom-up to
//!    compute per-[`NodeId`] [`EClassFacts`] (`is_fixed_shape`,
//!    `elision_safe`, `closure_free`, `all_descendants_elidable`)
//!    using the same monotone lattice rules as
//!    [`crate::egraph::analysis::GrammarAnalysis::make`]. A fixed-
//!    point iteration propagates facts across `Ref(target)`
//!    boundaries so transparent aliases pick up the target rule's
//!    body facts. When the e-graph hasn't run (structural mode,
//!    isolated unit tests), the fact map remains empty and the
//!    classifier falls back to pre-AF.1 structural behavior.
//! 2. **Bottom-up initial classification** — post-order over each
//!    rule body; every node gets the most aggressive class its
//!    structural shape allows (without yet considering pins). Every
//!    `TransparentElide` return is gated on the e-graph facts
//!    (all four bits must agree) — a node the e-graph cannot prove
//!    is fixed-shape, elision-safe, closure-free, and
//!    all-descendants-elidable demotes to `TapeSpanOnly`.
//! 3. **Consumer-pin fix-up** — walks every rule with a directive
//!    that requires `MustTape` (`@pretty`, `@debug`,
//!    `preserve_identity`) and transitively pins its subtree. See
//!    [`super::pin_sweep::apply_consumer_pins`].
//! 4. **Debug assertion sweep** — in debug builds, verifies every
//!    node has a class and the class respects the lattice
//!    invariants. See [`Self::assert_invariants`].
//!
//! The pass reads `ir.dag` to resolve [`NodeId`]s and writes the
//! per-node classification into `ir.materialization`. It does not
//! mutate rule bodies.
//!
//! # Tranche AF.1 — e-graph fact gate
//!
//! Before Tranche AF.1 the classifier consulted only structural
//! shape (leaf/compound kind, nullability) when deciding
//! `TransparentElide`. AF.1 activates the dormant
//! [`EClassFacts`] lattice (landed in Tranches AA.2/AB.0 as the
//! grammar-tier e-graph monotone analysis) as a pre-seed: a node is
//! a `TransparentElide` candidate only when the facts lattice
//! agrees with the structural decision via
//! [`facts_allow_transparent_elide`]. This wires the four dormant
//! bits (`is_fixed_shape`, `elision_safe`, `closure_free`,
//! `all_descendants_elidable`) into the live pipeline so the
//! compiler stops computing work it never read.
//!
//! [`NodeId`]: crate::dag::NodeId
//! [`MaterializationClass`]: super::lattice::MaterializationClass
//! [`EClassFacts`]: crate::egraph::analysis::EClassFacts

use std::collections::HashMap;

use bbnf_regex::sets::charset::CharSet128;

use crate::dag::NodeId;
use crate::egraph::analysis::{EClassFacts, WidthBound};
use crate::{FnDescriptor, GrammarIR, IrNode, RuleId};

use super::lattice::{MaterializationClass, mat_join};
use super::pin_sweep::apply_consumer_pins;

/// Per-`NodeId` snapshot of the e-graph monotone fact lattice,
/// consumed by the classifier to gate `TransparentElide` returns.
/// Empty when the pre-seed walk was skipped (fallback to structural
/// behavior).
pub type EClassFactsMap = HashMap<NodeId, EClassFacts>;

/// Run the full classification pipeline on `ir`.
///
/// Populates `ir.materialization` with a per-`NodeId` class for every
/// sub-expression reachable from any rule body. Panics in debug
/// builds if invariants fail; releases runs the classification
/// without the debug sweep.
///
/// Requires `ir.dag` to be populated — panics otherwise. Runs after
/// `project_types` in the production pipeline.
///
/// Tranche AF.1 — the classifier computes
/// [`EClassFacts`](crate::egraph::analysis::EClassFacts) bottom-up
/// as a pre-seed and gates every `TransparentElide` return on the
/// four dormant bits (`is_fixed_shape`, `elision_safe`,
/// `closure_free`, `all_descendants_elidable`). See
/// [`classify_materialization_with_facts`] for the test-only
/// variant that accepts an externally-supplied fact map (and an
/// empty one to exercise the fallback path).
pub fn classify_materialization(ir: &mut GrammarIR) {
    assert!(
        ir.dag.is_some(),
        "classify_materialization requires ir.dag to be built; call \
         bbnf_ir::dag::ensure_dag(&mut ir) in tests or run \
         crates/core/src/pipeline/compile.rs::build_durable_dag"
    );

    // Pass 0 — compute per-NodeId e-graph facts via a bottom-up walk
    // over every rule's body with a fixed-point over Ref resolution.
    // The same monotone lattice rules as
    // `GrammarAnalysis::make`, adapted to walk the owning `IrNode`
    // tree directly via `dag.node_for` so we don't need the e-graph
    // substrate at classification time.
    let facts = compute_eclass_facts(ir);

    classify_materialization_with_facts(ir, &facts);
}

/// Run the classification pipeline against an externally-supplied
/// fact map.
///
/// Production code calls [`classify_materialization`], which
/// computes facts from `ir.dag`. This variant exists for tests that
/// want to exercise the fallback path (empty map → pre-AF.1
/// structural behavior) or inject hand-crafted facts that disagree
/// with the structural shape, letting the AF.1 gate demote an
/// otherwise `TransparentElide` candidate.
pub fn classify_materialization_with_facts(ir: &mut GrammarIR, facts: &EClassFactsMap) {
    assert!(
        ir.dag.is_some(),
        "classify_materialization_with_facts requires ir.dag to be built",
    );

    let mut map: HashMap<NodeId, MaterializationClass> = HashMap::new();

    // Pass 1 — bottom-up initial classification. Walk every rule's
    // body and assign each node its most aggressive legal class.
    // Rule ids snapshotted to avoid holding a borrow of `ir.rules`
    // across the mutable access to `ir.materialization`.
    let rule_ids: Vec<RuleId> = ir.rules.iter().map(|r| r.id).collect();
    for rule_id in rule_ids.iter().copied() {
        let rule = &ir.rules[rule_id as usize];
        let body_ptr: *const IrNode = &rule.body as *const IrNode;
        // SAFETY: `body_ptr` is valid for the duration of this loop
        // — we only take it so we can call `classify_node` without
        // borrowing `ir.rules[rule_id]` while we also need to read
        // `ir.fns` and `ir.dag` through `ir`. The dereference
        // inside `classify_node` only reads the IR tree, which is
        // unmutated during Pass 1.
        let body = unsafe { &*body_ptr };
        classify_node(ir, body, facts, &mut map);
    }

    // Pass 2 — consumer-pin fix-up. `@pretty`, `@debug`, and
    // `preserve_identity` rules have their entire subtree pinned to
    // `MustTape`.
    apply_consumer_pins(ir, &mut map);

    // Pass 3 — debug-only invariant sweep.
    #[cfg(debug_assertions)]
    assert_invariants(ir, &map);

    ir.materialization = map;
}

/// Bottom-up classification of a single `IrNode` subtree.
///
/// Returns the class assigned to `node`. Populates `map` for `node`
/// AND every descendant. Uses `ir.dag.node_for` to resolve
/// [`NodeId`] for each tree position; a hash-consed node visited
/// twice (from different call sites) will store the same class
/// because the shape-based computation is deterministic.
///
/// Tranche AF.1 — every `TransparentElide` return is routed through
/// [`gate_transparent_elide`], which demotes the result to
/// `TapeSpanOnly` when the pre-computed e-graph facts for this
/// `NodeId` disagree. An empty `facts` map restores pre-AF.1
/// structural behavior unconditionally.
///
/// [`NodeId`]: crate::dag::NodeId
fn classify_node(
    ir: &GrammarIR,
    node: &IrNode,
    facts: &EClassFactsMap,
    map: &mut HashMap<NodeId, MaterializationClass>,
) -> MaterializationClass {
    // Resolve this tree position's stable `NodeId` once up front —
    // the AF.1 gate and the `map.insert` epilogue both need it.
    let node_id = ir.dag.as_ref().and_then(|dag| dag.node_for(node));

    // Classify children first (post-order) so the parent can consult
    // their results via `mat_join`.
    let class = match node {
        // ── Leaves ──────────────────────────────────────────────────
        IrNode::Literal(_) | IrNode::Regex(_) => MaterializationClass::TapeSpanOnly,

        IrNode::Epsilon => {
            // Epsilon carries no information — the view layer can
            // reconstruct it from the parent's span. Eligible for
            // elision when nothing else pins the subtree.
            gate_transparent_elide(MaterializationClass::TransparentElide, node_id, facts)
        }

        IrNode::Ref(target) => {
            // A rule reference inherits the target rule's head class
            // via transitive inlining at the call site. At
            // classification time we assign the ref node the
            // target's eligibility, demoted to `MustTape` if the
            // target is pinned. The consumer-pin sweep handles the
            // pinning direction after all rule heads are known.
            let hint = ref_class_hint(ir, *target);
            gate_transparent_elide(hint, node_id, facts)
        }

        // ── Compound: Seq ───────────────────────────────────────────
        IrNode::Seq(children) => {
            // A Seq that carries no observable structure (only
            // punctuation / elidable children) can itself become
            // `TapeSpanOnly` — a contiguous span over the match.
            // Anything else stays `MustTape` to preserve the child
            // offsets the view layer needs.
            let mut child_join = MaterializationClass::bottom();
            let mut all_elidable = true;
            for child in children {
                let c = classify_node(ir, child, facts, map);
                child_join = mat_join(child_join, c);
                if c == MaterializationClass::MustTape {
                    all_elidable = false;
                }
            }
            // Seq itself is never a structural wrapper — we can
            // collapse it only if every child is elidable and the
            // sequence contributes no typed output beyond its span.
            if all_elidable && child_join != MaterializationClass::MustTape {
                MaterializationClass::TapeSpanOnly
            } else {
                MaterializationClass::MustTape
            }
        }

        // ── Compound: Alt ───────────────────────────────────────────
        IrNode::Alt(branches, _dispatch) => {
            // Alt discriminates between branches — it needs a
            // variant index in the record's flags byte. Both
            // push_leaf and push_compound store variant_idx, so
            // when ALL branches are leaf-like (TapeSpanOnly or
            // TransparentElide), the Alt itself can be
            // TapeSpanOnly: one record, no children run. When any
            // branch is MustTape (compound children), the Alt
            // must be MustTape so the parent can walk its
            // subtree.
            let mut all_leaf_like = true;
            for b in branches {
                let c = classify_node(ir, &b.node, facts, map);
                if c == MaterializationClass::MustTape {
                    all_leaf_like = false;
                }
            }
            if all_leaf_like {
                MaterializationClass::TapeSpanOnly
            } else {
                MaterializationClass::MustTape
            }
        }

        // ── Compound: Repeat ────────────────────────────────────────
        IrNode::Repeat { inner, lo, hi } => {
            let inner_class = classify_node(ir, inner, facts, map);
            // Zero-repetition fixed-count (lo == hi == 0) is
            // equivalent to Epsilon — fully elidable. Otherwise a
            // Repeat emits a compound over its iterations so the
            // view layer can expose an iterator.
            if *lo == 0 && *hi == 0 {
                gate_transparent_elide(MaterializationClass::TransparentElide, node_id, facts)
            } else if inner_class == MaterializationClass::TransparentElide && *lo == *hi {
                // Fixed count of an elidable inner — same as N
                // copies of the inner, still elidable.
                gate_transparent_elide(MaterializationClass::TransparentElide, node_id, facts)
            } else {
                MaterializationClass::MustTape
            }
        }

        // ── Binary ──────────────────────────────────────────────────
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            // Skip/Next/Minus carry two children; their own output
            // is one span covering both. If neither child itself
            // demands `MustTape`, the wrapper can collapse to
            // `TapeSpanOnly`.
            let ca = classify_node(ir, a, facts, map);
            let cb = classify_node(ir, b, facts, map);
            let j = mat_join(ca, cb);
            if j == MaterializationClass::MustTape {
                MaterializationClass::MustTape
            } else {
                MaterializationClass::TapeSpanOnly
            }
        }

        // ── Lookahead ───────────────────────────────────────────────
        IrNode::Negate(inner) => {
            // Negate is zero-width — consumes no input. The inner
            // subtree is still classified (its NodeIds matter for
            // downstream passes) but the Negate itself produces no
            // record.
            classify_node(ir, inner, facts, map);
            gate_transparent_elide(MaterializationClass::TransparentElide, node_id, facts)
        }

        // ── Whitespace wrapper ──────────────────────────────────────
        IrNode::OptionalWhitespace(inner) => {
            let inner_class = classify_node(ir, inner, facts, map);
            // Whitespace wraps the inner without adding typed
            // output. Pass the inner's class through. If the inner
            // was TransparentElide, we still route through the
            // gate at this outer position so the whitespace-wrapped
            // NodeId's own facts can demote it independently.
            if inner_class == MaterializationClass::TransparentElide {
                gate_transparent_elide(inner_class, node_id, facts)
            } else {
                inner_class
            }
        }

        // ── Map ─────────────────────────────────────────────────────
        IrNode::Map { inner, fn_id } => {
            let inner_class = classify_node(ir, inner, facts, map);
            // A map with a closure-typed descriptor forces `MustTape`
            // — the closure environment has to be held live by the
            // caller. Non-closure descriptors (EnumWrap, BoxWrap,
            // NumberConvert, HexConvert, Constant) are transparent
            // to materialization at classification time; the CSP
            // solver may still demote them in AB.1.
            let fn_is_closure = ir
                .fns
                .get(*fn_id as usize)
                .map(fn_descriptor_is_closure)
                .unwrap_or(true);
            if fn_is_closure {
                MaterializationClass::MustTape
            } else if inner_class == MaterializationClass::TransparentElide {
                // Preserve the inner class — a Map over an
                // elision-safe leaf stays elision-safe, subject to
                // the AF.1 facts gate at the Map's own NodeId.
                gate_transparent_elide(inner_class, node_id, facts)
            } else {
                inner_class
            }
        }

        // ── Lexer fusion ────────────────────────────────────────────
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            // TokenDispatch is a multi-arm discriminator — emit a
            // compound to record the chosen arm. Classify children
            // for downstream CSP refinement.
            classify_node(ir, token, facts, map);
            for arm in arms {
                classify_node(ir, &arm.continuation, facts, map);
            }
            classify_node(ir, fallback, facts, map);
            MaterializationClass::MustTape
        }
    };

    // Record this tree position's class. A shared node visited
    // again is deterministic (same structural shape → same class),
    // so overwriting is safe.
    if let Some(id) = node_id {
        map.insert(id, class);
    }
    class
}

/// AF.1 gate: clamp a candidate class through the e-graph monotone
/// fact lattice.
///
/// The classifier calls this at every decision point that would
/// otherwise return `TransparentElide`. The gate inspects the
/// pre-computed facts for `node_id` and:
///
/// - **Agrees** (`is_fixed_shape && elision_safe && closure_free &&
///   all_descendants_elidable`) → `TransparentElide` stands.
/// - **Disagrees** (any bit false) → demote to `TapeSpanOnly`. One
///   level up the lattice, the minimum widening that preserves
///   soundness without over-pinning to `MustTape`.
/// - **No entry** (`facts` lacks `node_id`, or `node_id` is `None`)
///   → fallback to pre-AF.1 structural behavior, returning the
///   candidate unchanged. This path is load-bearing for structural
///   mode (no e-graph saturation) and for unit tests that construct
///   minimal `GrammarIR` instances without populating facts.
///
/// For any `candidate` other than `TransparentElide`, the gate is a
/// no-op — callers can invoke it unconditionally.
#[inline]
fn gate_transparent_elide(
    candidate: MaterializationClass,
    node_id: Option<NodeId>,
    facts: &EClassFactsMap,
) -> MaterializationClass {
    if candidate != MaterializationClass::TransparentElide {
        return candidate;
    }
    let Some(id) = node_id else {
        return candidate;
    };
    let Some(fact) = facts.get(&id) else {
        return candidate;
    };
    if facts_allow_transparent_elide(fact) {
        candidate
    } else {
        MaterializationClass::TapeSpanOnly
    }
}

/// True iff all four dormant e-graph bits agree that this class is
/// eligible for the scalar payload tier (TypeDesc-driven value
/// materialization).
///
/// The four bits — `is_fixed_shape`, `elision_safe`, `closure_free`,
/// and `all_descendants_elidable` — are the monotone lattice joins
/// defined in `crate::egraph::analysis::facts`. Each is `&&`-joined
/// on e-class merge; a `true` value means every sibling node in the
/// class agrees, and a single disagreement across the lattice demotes
/// it to `false`. When all four agree the subtree is structurally
/// canonical, carries no closure environment, and every descendant
/// can be safely elided — the exact set of conditions the
/// tape-first emitter needs before it can skip record emission.
#[inline]
pub(crate) fn facts_allow_transparent_elide(facts: &EClassFacts) -> bool {
    facts.is_fixed_shape
        && facts.elision_safe
        && facts.closure_free
        && facts.all_descendants_elidable
}

/// Hint for a `Ref(target)` node's initial class — consulted during
/// Pass 1 before pins are applied.
///
/// Transparent aliases and zero-body rules are `TransparentElide`
/// candidates. Anything else defaults to `MustTape`; Pass 2 will
/// demote compatible refs after the target's head class is known.
fn ref_class_hint(ir: &GrammarIR, target: RuleId) -> MaterializationClass {
    let Some(rule) = ir.rules.get(target as usize) else {
        return MaterializationClass::MustTape;
    };
    if rule.meta.preserve_identity {
        return MaterializationClass::MustTape;
    }
    if rule.meta.directives.pretty.is_some() || rule.meta.directives.debug {
        return MaterializationClass::MustTape;
    }
    if rule.meta.is_transparent || rule.meta.is_alias.is_some() {
        return MaterializationClass::TransparentElide;
    }
    MaterializationClass::MustTape
}

/// Whether an `FnDescriptor` forces `MustTape` at the call site.
///
/// The compiler-produced variants (`EnumWrap`, `BoxWrap`,
/// `SpanCapture`, `NumberConvert`, `HexConvert`) are transparent to
/// tape-level materialization — the view layer can apply them at
/// access time from the underlying span. Only the user-facing
/// `Expr` variant may carry an opaque host closure (or a MapExpr
/// that consults the parse result in a way the view layer cannot
/// replay cheaply), so it conservatively pins the node to
/// `MustTape`. The AB.1 CSP pass refines this when it can prove
/// the `Expr` tree is closure-free.
#[inline]
fn fn_descriptor_is_closure(desc: &FnDescriptor) -> bool {
    matches!(desc, FnDescriptor::Expr { .. })
}

/// Debug-only invariant sweep.
///
/// Verifies every rule body has a class and that `TransparentElide`
/// rules' children are all elidable (otherwise inlining would
/// produce an unclassified record).
#[cfg(debug_assertions)]
fn assert_invariants(ir: &GrammarIR, map: &HashMap<NodeId, MaterializationClass>) {
    let dag = ir.dag.as_ref().expect("assert_invariants requires dag");
    for rule in &ir.rules {
        let Some(body_id) = dag.node_for(&rule.body) else {
            continue;
        };
        assert!(
            map.contains_key(&body_id),
            "rule '{}' body has no materialization class",
            ir.get_string(rule.name),
        );
    }
}

// ═══════════════════════════════════════════════════════════════════
// AF.1 — per-`NodeId` e-graph fact pre-seed
// ═══════════════════════════════════════════════════════════════════
//
// Mirrors `crate::egraph::analysis::GrammarAnalysis::make` — the
// same monotone lattice rules, adapted to walk the post-optimization
// `IrNode` tree directly via `ir.dag.node_for` instead of going
// through the e-graph substrate. The e-graph runs earlier in the
// pipeline (`egraph_build_saturate_writeback`, line ~497 of
// `crates/core/src/pipeline/compile.rs`) and discards its analysis
// data on drop; re-computing the facts here over the final IR
// produces identical per-`NodeId` lattice values because bottom-up
// monotone facts of an `IrNode` tree are a pure function of its
// shape — the e-graph's merge propagation can only widen facts,
// never contradict the bottom-up values, and a tree has no merges
// to propagate.
//
// Fixed-point iteration propagates facts across `Ref(target)`
// boundaries: a rule body containing `Ref(r)` reads the previous
// iteration's `rule_body_facts[r]` for the ref's own facts. The
// lattice is monotone (all `&&` / `||` / widening joins), so the
// iteration converges in at most `|rules|` iterations in the
// worst case, and usually one or two in practice.

/// Compute per-`NodeId` [`EClassFacts`] for every sub-expression
/// reachable from any rule body.
///
/// Returns an empty map when `ir.dag` is absent. The map is keyed
/// by the `NodeId`s produced by [`crate::dag::GrammarDag::node_for`]
/// — one entry per distinct hash-consed position.
///
/// Consumed by [`classify_materialization`] to gate `TransparentElide`
/// returns. Other callers (AF.2, AF.3) may later lift this helper
/// to a `GrammarIR` sidecar if more than one pass needs the facts.
pub fn compute_eclass_facts(ir: &GrammarIR) -> EClassFactsMap {
    let mut node_facts: EClassFactsMap = HashMap::new();
    if ir.dag.is_none() {
        return node_facts;
    }

    // Per-rule "head" facts — the facts attached to a rule's body
    // subtree after the fixed point converges. Populated from
    // `unknown_ref()` initially and grown monotonically each
    // iteration. `Ref(target)` consults this map during the body
    // walk so its facts resolve to the target rule's head facts.
    let rule_count = ir.rules.len();
    let mut rule_head_facts: Vec<EClassFacts> = vec![EClassFacts::unknown_ref(); rule_count];

    // Fixed-point loop. An iteration either widens at least one
    // rule's head facts (in which case another iteration is
    // required because dependent rules may now see widened inputs)
    // or converges. Capped at `rule_count + 2` to guard against
    // non-monotone bugs in future extensions — every monotone
    // lattice loop over a finite domain converges in linear time.
    let max_iter = rule_count.saturating_add(2);
    for _ in 0..max_iter {
        let mut changed = false;
        for rule in &ir.rules {
            let body_facts =
                compute_facts_for_node(ir, &rule.body, &rule_head_facts, &mut node_facts);
            let idx = rule.id as usize;
            if idx < rule_head_facts.len() {
                // Monotone join. A rule's head facts only grow,
                // never shrink — `EClassFacts::merge` widens the
                // existing value by the new value.
                if rule_head_facts[idx].merge(&body_facts) {
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }

    // Final walk — after the fixed point converges, re-run the body
    // walk once more so the `node_facts` map holds the converged
    // per-NodeId values for every tree position. Earlier iterations
    // may have written intermediate values while `rule_head_facts`
    // was still growing.
    for rule in &ir.rules {
        compute_facts_for_node(ir, &rule.body, &rule_head_facts, &mut node_facts);
    }

    node_facts
}

/// Recursive bottom-up fact computation for a single `IrNode`.
///
/// Writes the computed facts for every `NodeId` encountered into
/// `node_facts`. Uses `rule_head_facts` to resolve `Ref(target)` to
/// the target rule's head facts — the value from the previous
/// fixed-point iteration, or `unknown_ref()` if the caller is on
/// the first iteration.
fn compute_facts_for_node(
    ir: &GrammarIR,
    node: &IrNode,
    rule_head_facts: &[EClassFacts],
    node_facts: &mut EClassFactsMap,
) -> EClassFacts {
    let facts = match node {
        IrNode::Literal(sid) => EClassFacts::literal(*sid),
        IrNode::Regex(sid) => EClassFacts::regex(*sid),
        IrNode::Epsilon => EClassFacts::epsilon(),

        IrNode::Ref(target) => {
            // Resolve via the per-rule head facts table; if the
            // target hasn't been seen yet, fall back to unknown.
            rule_head_facts
                .get(*target as usize)
                .cloned()
                .unwrap_or_else(EClassFacts::unknown_ref)
        }

        IrNode::Seq(children) => {
            // Mirrors `GrammarAnalysis::make`'s Seq case. FIRST is
            // accumulated until a non-nullable child is hit; width
            // is the sum; `elision_safe = false` (Seq itself is not
            // a structural wrapper); `closure_free`, `fixed_shape`,
            // and `all_descendants_elidable` are all `&&`-joined
            // over children.
            let mut first = CharSet128::new();
            let mut nullable = true;
            let mut min_width: u32 = 0;
            let mut max_width: Option<u32> = Some(0);
            let mut all_elidable = true;
            let mut closure_free = true;
            let mut fixed_shape = true;
            for child in children {
                let c = compute_facts_for_node(ir, child, rule_head_facts, node_facts);
                if nullable {
                    first.union(&c.first_set);
                }
                if !c.nullable {
                    nullable = false;
                }
                min_width = min_width.saturating_add(c.width.min);
                max_width = match (max_width, c.width.max) {
                    (Some(a), Some(b)) => Some(a.saturating_add(b)),
                    _ => None,
                };
                all_elidable &= c.all_descendants_elidable;
                closure_free &= c.closure_free;
                fixed_shape &= c.is_fixed_shape;
            }
            EClassFacts {
                first_set: first,
                nullable,
                width: WidthBound {
                    min: min_width,
                    max: max_width,
                },
                literal_sid: None,
                regex_sid: None,
                elision_safe: false,
                closure_free,
                is_fixed_shape: fixed_shape,
                all_descendants_elidable: all_elidable,
            }
        }

        IrNode::Alt(branches, _) => {
            let mut first = CharSet128::new();
            let mut nullable = false;
            let mut min_width: Option<u32> = None;
            let mut max_width: Option<u32> = Some(0);
            let mut all_elidable = true;
            let mut closure_free = true;
            for b in branches {
                let c = compute_facts_for_node(ir, &b.node, rule_head_facts, node_facts);
                first.union(&c.first_set);
                nullable |= c.nullable;
                min_width = Some(match min_width {
                    None => c.width.min,
                    Some(cur) => cur.min(c.width.min),
                });
                max_width = match (max_width, c.width.max) {
                    (Some(a), Some(b)) => Some(a.max(b)),
                    _ => None,
                };
                all_elidable &= c.all_descendants_elidable;
                closure_free &= c.closure_free;
            }
            // Alt is never fixed-shape (picks one of several
            // branches) and is never itself elision-safe.
            EClassFacts {
                first_set: first,
                nullable,
                width: WidthBound {
                    min: min_width.unwrap_or(0),
                    max: max_width,
                },
                literal_sid: None,
                regex_sid: None,
                elision_safe: false,
                closure_free,
                is_fixed_shape: false,
                all_descendants_elidable: all_elidable,
            }
        }

        IrNode::Repeat { inner, lo, hi } => {
            let c = compute_facts_for_node(ir, inner, rule_head_facts, node_facts);
            let nullable = *lo == 0 || c.nullable;
            let min_width = c.width.min.saturating_mul(*lo);
            let max_width = if *hi == u32::MAX {
                None
            } else {
                c.width.max.map(|m| m.saturating_mul(*hi))
            };
            let fixed_shape = *lo == *hi && c.is_fixed_shape;
            EClassFacts {
                first_set: c.first_set,
                nullable,
                width: WidthBound {
                    min: min_width,
                    max: max_width,
                },
                literal_sid: None,
                regex_sid: None,
                elision_safe: false,
                closure_free: c.closure_free,
                is_fixed_shape: fixed_shape,
                all_descendants_elidable: c.all_descendants_elidable,
            }
        }

        IrNode::Skip(a, b) | IrNode::Next(a, b) => {
            let left = compute_facts_for_node(ir, a, rule_head_facts, node_facts);
            let right = compute_facts_for_node(ir, b, rule_head_facts, node_facts);
            let min_width = left.width.min.saturating_add(right.width.min);
            let max_width = match (left.width.max, right.width.max) {
                (Some(l), Some(r)) => Some(l.saturating_add(r)),
                _ => None,
            };
            let nullable = left.nullable && right.nullable;
            let mut first_set = left.first_set;
            if left.nullable {
                first_set.union(&right.first_set);
            }
            EClassFacts {
                first_set,
                nullable,
                width: WidthBound {
                    min: min_width,
                    max: max_width,
                },
                literal_sid: None,
                regex_sid: None,
                elision_safe: false,
                closure_free: left.closure_free && right.closure_free,
                is_fixed_shape: left.is_fixed_shape && right.is_fixed_shape,
                all_descendants_elidable: left.all_descendants_elidable
                    && right.all_descendants_elidable,
            }
        }

        IrNode::Minus(a, b) => {
            // Minus: left without matches of right. Walk both so
            // their NodeIds populate `node_facts`, but approximate
            // with the left operand's facts (matches
            // `GrammarAnalysis::make`).
            let _ = compute_facts_for_node(ir, b, rule_head_facts, node_facts);
            compute_facts_for_node(ir, a, rule_head_facts, node_facts)
        }

        IrNode::Negate(inner) => {
            // Zero-width — consumes no input. Empty first_set,
            // nullable. Elision-safe by construction (produces no
            // output).
            let _ = compute_facts_for_node(ir, inner, rule_head_facts, node_facts);
            EClassFacts {
                first_set: CharSet128::new(),
                nullable: true,
                width: WidthBound {
                    min: 0,
                    max: Some(0),
                },
                literal_sid: None,
                regex_sid: None,
                elision_safe: true,
                closure_free: true,
                is_fixed_shape: true,
                all_descendants_elidable: true,
            }
        }

        IrNode::OptionalWhitespace(inner) => {
            let c = compute_facts_for_node(ir, inner, rule_head_facts, node_facts);
            EClassFacts {
                first_set: c.first_set,
                nullable: true,
                width: WidthBound {
                    min: 0,
                    max: c.width.max,
                },
                literal_sid: None,
                regex_sid: None,
                elision_safe: true,
                closure_free: c.closure_free,
                is_fixed_shape: c.is_fixed_shape,
                all_descendants_elidable: c.all_descendants_elidable,
            }
        }

        IrNode::Map { inner, .. } => {
            // Map transforms output. `elision_safe = false`,
            // `closure_free = false` — mirrors the e-graph; the
            // classifier refines closure-free via the FnDescriptor
            // table separately.
            let c = compute_facts_for_node(ir, inner, rule_head_facts, node_facts);
            EClassFacts {
                first_set: c.first_set,
                nullable: c.nullable,
                width: c.width,
                literal_sid: None,
                regex_sid: None,
                elision_safe: false,
                closure_free: false,
                is_fixed_shape: c.is_fixed_shape,
                all_descendants_elidable: false,
            }
        }

        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            // Facts from the token leaf, widened-conservative for
            // the outer shape (not fixed, not elision-safe, not
            // all-descendants-elidable). Walk arms + fallback so
            // their NodeIds populate `node_facts`.
            let token_facts = compute_facts_for_node(ir, token, rule_head_facts, node_facts);
            for arm in arms {
                compute_facts_for_node(ir, &arm.continuation, rule_head_facts, node_facts);
            }
            let _ = compute_facts_for_node(ir, fallback, rule_head_facts, node_facts);
            EClassFacts {
                first_set: token_facts.first_set,
                nullable: token_facts.nullable,
                width: token_facts.width,
                literal_sid: None,
                regex_sid: None,
                elision_safe: false,
                closure_free: token_facts.closure_free,
                is_fixed_shape: false,
                all_descendants_elidable: false,
            }
        }
    };

    if let Some(id) = ir.dag.as_ref().and_then(|dag| dag.node_for(node)) {
        node_facts.insert(id, facts.clone());
    }
    facts
}
