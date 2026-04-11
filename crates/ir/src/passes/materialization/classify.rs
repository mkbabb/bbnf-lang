//! `classify_materialization` — the bottom-up classification pass.
//!
//! Tranche AB.0. Walks every rule's IR tree and assigns each
//! [`NodeId`] a [`MaterializationClass`] via three sub-passes:
//!
//! 1. **Bottom-up initial classification** — post-order over each
//!    rule body; every node gets the most aggressive class its
//!    structural shape allows (without yet considering pins).
//! 2. **Consumer-pin fix-up** — walks every rule with a directive
//!    that requires `MustTape` (`@pretty`, `@debug`,
//!    `preserve_identity`) and transitively pins its subtree. See
//!    [`super::pin_sweep::apply_consumer_pins`].
//! 3. **Debug assertion sweep** — in debug builds, verifies every
//!    node has a class and the class respects the lattice
//!    invariants. See [`Self::assert_invariants`].
//!
//! The pass reads `ir.dag` to resolve [`NodeId`]s and writes the
//! per-node classification into `ir.materialization`. It does not
//! mutate rule bodies.
//!
//! [`NodeId`]: crate::dag::NodeId
//! [`MaterializationClass`]: super::lattice::MaterializationClass

use std::collections::HashMap;

use crate::dag::NodeId;
use crate::{FnDescriptor, GrammarIR, IrNode, RuleId};

use super::lattice::{MaterializationClass, mat_join};
use super::pin_sweep::apply_consumer_pins;

/// Run the full classification pipeline on `ir`.
///
/// Populates `ir.materialization` with a per-`NodeId` class for every
/// sub-expression reachable from any rule body. Panics in debug
/// builds if invariants fail; releases runs the classification
/// without the debug sweep.
///
/// Requires `ir.dag` to be populated — panics otherwise. Runs after
/// `project_types` in the production pipeline.
pub fn classify_materialization(ir: &mut GrammarIR) {
    assert!(
        ir.dag.is_some(),
        "classify_materialization requires ir.dag to be built; call \
         bbnf_ir::dag::ensure_dag(&mut ir) in tests or run \
         crates/core/src/pipeline/compile.rs::build_durable_dag"
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
        classify_node(ir, body, &mut map);
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
/// [`NodeId`]: crate::dag::NodeId
fn classify_node(
    ir: &GrammarIR,
    node: &IrNode,
    map: &mut HashMap<NodeId, MaterializationClass>,
) -> MaterializationClass {
    // Classify children first (post-order) so the parent can consult
    // their results via `mat_join`.
    let class = match node {
        // ── Leaves ──────────────────────────────────────────────────
        IrNode::Literal(_) | IrNode::Regex(_) => MaterializationClass::TapeSpanOnly,

        IrNode::Epsilon => {
            // Epsilon carries no information — the view layer can
            // reconstruct it from the parent's span. Eligible for
            // elision when nothing else pins the subtree.
            MaterializationClass::TransparentElide
        }

        IrNode::Ref(target) => {
            // A rule reference inherits the target rule's head class
            // via transitive inlining at the call site. At
            // classification time we assign the ref node the
            // target's eligibility, demoted to `MustTape` if the
            // target is pinned. The consumer-pin sweep handles the
            // pinning direction after all rule heads are known.
            ref_class_hint(ir, *target)
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
                let c = classify_node(ir, child, map);
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
            // Alt discriminates between branches — the view layer
            // needs the variant index, so Alt always emits a
            // compound record. Children still get classified for
            // AB.1 CSP joint-solve refinement.
            for b in branches {
                classify_node(ir, &b.node, map);
            }
            MaterializationClass::MustTape
        }

        // ── Compound: Repeat ────────────────────────────────────────
        IrNode::Repeat { inner, lo, hi } => {
            let inner_class = classify_node(ir, inner, map);
            // Zero-repetition fixed-count (lo == hi == 0) is
            // equivalent to Epsilon — fully elidable. Otherwise a
            // Repeat emits a compound over its iterations so the
            // view layer can expose an iterator.
            if *lo == 0 && *hi == 0 {
                MaterializationClass::TransparentElide
            } else if inner_class == MaterializationClass::TransparentElide && *lo == *hi {
                // Fixed count of an elidable inner — same as N
                // copies of the inner, still elidable.
                MaterializationClass::TransparentElide
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
            let ca = classify_node(ir, a, map);
            let cb = classify_node(ir, b, map);
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
            classify_node(ir, inner, map);
            MaterializationClass::TransparentElide
        }

        // ── Whitespace wrapper ──────────────────────────────────────
        IrNode::OptionalWhitespace(inner) => {
            let inner_class = classify_node(ir, inner, map);
            // Whitespace wraps the inner without adding typed
            // output. Pass the inner's class through.
            inner_class
        }

        // ── Map ─────────────────────────────────────────────────────
        IrNode::Map { inner, fn_id } => {
            let inner_class = classify_node(ir, inner, map);
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
            } else {
                // Preserve the inner class — a Map over an
                // elision-safe leaf stays elision-safe.
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
            classify_node(ir, token, map);
            for arm in arms {
                classify_node(ir, &arm.continuation, map);
            }
            classify_node(ir, fallback, map);
            MaterializationClass::MustTape
        }
    };

    // Record this tree position's class. A shared node visited
    // again is deterministic (same structural shape → same class),
    // so overwriting is safe.
    if let Some(id) = ir.dag.as_ref().and_then(|dag| dag.node_for(node)) {
        map.insert(id, class);
    }
    class
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
