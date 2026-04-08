//! E-graph → `IrNode` extraction and write-back.
//!
//! After saturation, the e-graph contains every equivalent form of
//! each rule's body. This module walks each rule's root e-class via
//! `Extractor` (cost-model-guided), rebuilds a concrete `IrNode`
//! tree from the chosen best e-nodes, and writes the results back
//! into a `GrammarIR`.
//!
//! **Rule boundaries are preserved**: when walking rule X's body
//! and the walk descends into a class whose canonical id is another
//! rule Y's root, the walker emits `Ref(Y)` instead of inlining Y's
//! full body. This matches the destructive pipeline's behavior
//! (rules reference each other via `Ref` and walks don't descend
//! across rule boundaries) and is essential for preserving mutual
//! recursion (JSON: `value ↔ object ↔ array ↔ value`).
//!
//! An additional per-walk `visiting` set guards against
//! non-rule-rooted cycles that can arise if an `InlineEligibleRef`-
//! style rule unions a non-root class with its own expansion.

use std::collections::HashMap;

use egraph::{EGraph, Extractor, Id};

use super::analysis::GrammarAnalysis;
use super::cost::GrammarCostModel;
use super::node::GrammarENode;
use crate::{AltBranch, FnId, GrammarIR, IrNode, RuleId, StringId};

/// Rebuild each rule's body from the saturated e-graph and write the
/// results back into `ir.rules`. Per-rule root ids come from the
/// `build_and_saturate` step; the cost model guides best-node
/// selection.
pub fn write_back_optimized(
    egraph: &EGraph<GrammarENode, GrammarAnalysis>,
    ir: &mut GrammarIR,
    rule_body_ids: &HashMap<RuleId, Id>,
    cost: &GrammarCostModel,
) {
    let extractor = Extractor::new(egraph, cost);
    // Reverse map: canonical e-class Id → owning RuleId. Used by
    // the reconstruction walk to re-emit cross-rule references as
    // `Ref(RuleId)` instead of inlining the target rule's body.
    let root_to_rule: HashMap<Id, RuleId> = rule_body_ids
        .iter()
        .map(|(&rid, &id)| (egraph.find_ref(id), rid))
        .collect();

    // Snapshot original rule ids (we mutate ir.rules in the loop).
    let rule_ids: Vec<RuleId> = ir.rules.iter().map(|r| r.id).collect();
    for rule_id in rule_ids {
        let Some(&root_id) = rule_body_ids.get(&rule_id) else {
            continue;
        };
        // Top-level extraction. The top level cannot delegate to
        // `rebuild` (which always short-circuits at rule boundaries
        // via Ref); instead, `materialize_best_at_root` picks the
        // best non-self-referential node from the class and walks
        // its children. "Non-self-referential" matters because the
        // e-graph can unify a rule's body class with a Ref-to-self
        // form (e.g. `pretty_hint`'s body is `Ref(identifier)` and
        // CanonicalizeAlias adds `Ref(pretty_hint)` to the same
        // class via another rule's rewrite). Extracting the naive
        // best would produce `rule_X = Ref(rule_X)`, a self-cycle.
        let mut visiting = HashMap::new();
        let canonical = egraph.find_ref(root_id);
        visiting.insert(canonical, ());
        if let Some(body) = materialize_best_at_root(
            egraph,
            &extractor,
            canonical,
            rule_id,
            &root_to_rule,
            &mut visiting,
        ) {
            if let Some(rule) = ir.rules.iter_mut().find(|r| r.id == rule_id) {
                rule.body = body;
            }
        }
    }
}

/// Top-level-only variant of [`materialize_best`]: picks the
/// cost-cheapest node in `canonical` that is **not** `Ref(rule_id)`,
/// then descends via `rebuild` for each child. Without this filter,
/// the extractor can pick `Ref(rule_id)` as the root form whenever
/// the e-graph has unified the rule's body class with a self-Ref
/// alternative (a benign consequence of hash-consing combined with
/// CanonicalizeAlias), producing a self-cycle in the extracted IR.
fn materialize_best_at_root(
    egraph: &EGraph<GrammarENode, GrammarAnalysis>,
    extractor: &Extractor<
        '_,
        GrammarENode,
        GrammarAnalysis,
        GrammarCostModel,
    >,
    canonical: Id,
    rule_id: RuleId,
    root_to_rule: &HashMap<Id, RuleId>,
    visiting: &mut HashMap<Id, ()>,
) -> Option<IrNode> {
    // Pick a node from the class that is NOT `Ref(rule_id)`. We
    // prefer the extractor's best if it passes the filter, otherwise
    // scan the class for the first suitable node. (The class is
    // guaranteed non-empty by construction.)
    let best_filtered: Option<GrammarENode> = match extractor.best_node(canonical) {
        Some(node) if !matches!(node, GrammarENode::Ref(r) if *r == rule_id) => {
            Some(node.clone())
        }
        _ => egraph
            .class(canonical)
            .iter()
            .find(|n| !matches!(n, GrammarENode::Ref(r) if *r == rule_id))
            .cloned(),
    };
    let best = best_filtered?;
    Some(materialize_node(egraph, extractor, best, root_to_rule, visiting))
}

/// Rebuild a single `IrNode` by recursively extracting the best form
/// for `root` and its transitive children. Without a `root_to_rule`
/// mapping, cross-rule references are not preserved — use
/// [`write_back_optimized`] for full IR reconstruction.
pub fn extract_ir_node(
    egraph: &EGraph<GrammarENode, GrammarAnalysis>,
    cost: &GrammarCostModel,
    root: Id,
) -> Option<IrNode> {
    let extractor = Extractor::new(egraph, cost);
    let empty = HashMap::new();
    let mut visiting = HashMap::new();
    let canonical = egraph.find_ref(root);
    visiting.insert(canonical, ());
    materialize_best(egraph, &extractor, canonical, &empty, &mut visiting)
}

/// Child-descent entry point — called for every non-top-level
/// walk. Unconditionally emits `Ref(rule_id)` when `id`'s canonical
/// is any rule's root, preserving the grammar's inter-rule structure
/// (including self-recursion). Only non-root classes descend into
/// their best node via [`materialize_best`].
fn rebuild(
    egraph: &EGraph<GrammarENode, GrammarAnalysis>,
    extractor: &Extractor<
        '_,
        GrammarENode,
        GrammarAnalysis,
        GrammarCostModel,
    >,
    id: Id,
    root_to_rule: &HashMap<Id, RuleId>,
    current_rule: Option<RuleId>,
    visiting: &mut HashMap<Id, ()>,
) -> Option<IrNode> {
    let _ = current_rule; // no longer consulted; Refs are always emitted at rule boundaries
    let canonical = egraph.find_ref(id);

    // Rule boundary: emit `Ref(rule_id)` and stop descending. The
    // referenced rule's body is reconstructed independently in its
    // own top-level `write_back_optimized` iteration. This preserves
    // both cross-rule references AND self-recursion (e.g.,
    // `expr = expr "+" term | term`).
    if let Some(&target_rule) = root_to_rule.get(&canonical) {
        return Some(IrNode::Ref(target_rule));
    }

    if visiting.contains_key(&canonical) {
        // Non-rule-rooted cycle — emit Epsilon so extraction
        // terminates.
        return Some(IrNode::Epsilon);
    }
    visiting.insert(canonical, ());
    let result = materialize_best(egraph, extractor, canonical, root_to_rule, visiting);
    visiting.remove(&canonical);
    return result;
}

/// Shared body of the reconstruction: given a canonical e-class that
/// already passed any rule-boundary / cycle checks, pick the best
/// e-node for it via the extractor and convert it into an `IrNode`,
/// descending into children via [`rebuild`].
fn materialize_best(
    egraph: &EGraph<GrammarENode, GrammarAnalysis>,
    extractor: &Extractor<
        '_,
        GrammarENode,
        GrammarAnalysis,
        GrammarCostModel,
    >,
    canonical: Id,
    root_to_rule: &HashMap<Id, RuleId>,
    visiting: &mut HashMap<Id, ()>,
) -> Option<IrNode> {
    let best = extractor.best_node(canonical)?.clone();
    Some(materialize_node(egraph, extractor, best, root_to_rule, visiting))
}

/// Convert a single `GrammarENode` (already selected via the
/// extractor) into an `IrNode`, descending into children via
/// [`rebuild`]. This is the pure node-to-IrNode translation — no
/// class-lookup or visiting bookkeeping (the caller owns those).
fn materialize_node(
    egraph: &EGraph<GrammarENode, GrammarAnalysis>,
    extractor: &Extractor<
        '_,
        GrammarENode,
        GrammarAnalysis,
        GrammarCostModel,
    >,
    best: GrammarENode,
    root_to_rule: &HashMap<Id, RuleId>,
    visiting: &mut HashMap<Id, ()>,
) -> IrNode {
    let current_rule: Option<RuleId> = None;

    let result = match best {
        GrammarENode::Literal(sid) => IrNode::Literal(sid as StringId),
        GrammarENode::Regex(sid) => IrNode::Regex(sid as StringId),
        GrammarENode::Epsilon => IrNode::Epsilon,
        GrammarENode::Ref(rid) => IrNode::Ref(rid),
        GrammarENode::Seq(children) => {
            let rebuilt: Vec<IrNode> = children
                .iter()
                .filter_map(|&cid| rebuild(egraph, extractor, cid, root_to_rule, current_rule, visiting))
                .collect();
            if rebuilt.is_empty() {
                IrNode::Epsilon
            } else if rebuilt.len() == 1 {
                rebuilt.into_iter().next().unwrap()
            } else {
                IrNode::Seq(rebuilt)
            }
        }
        GrammarENode::Alt(children, dispatch) => {
            let branches: Vec<AltBranch> = children
                .iter()
                .filter_map(|&cid| {
                    rebuild(egraph, extractor, cid, root_to_rule, current_rule, visiting).map(|node| AltBranch {
                        node,
                        // `first_set` is re-computed by the post-switch
                        // `compute_follow_sets` pass; leaving None
                        // here matches the build-time default.
                        first_set: None,
                    })
                })
                .collect();
            if branches.is_empty() {
                IrNode::Epsilon
            } else if branches.len() == 1 && dispatch.is_none() {
                branches.into_iter().next().unwrap().node
            } else {
                IrNode::Alt(branches, dispatch)
            }
        }
        GrammarENode::Repeat { inner, lo, hi } => {
            let inner = rebuild(egraph, extractor, inner, root_to_rule, current_rule, visiting)
                .unwrap_or(IrNode::Epsilon);
            IrNode::Repeat {
                inner: Box::new(inner),
                lo,
                hi,
            }
        }
        GrammarENode::Skip([a, b]) => {
            let a = rebuild(egraph, extractor, a, root_to_rule, current_rule, visiting).unwrap_or(IrNode::Epsilon);
            let b = rebuild(egraph, extractor, b, root_to_rule, current_rule, visiting).unwrap_or(IrNode::Epsilon);
            IrNode::Skip(Box::new(a), Box::new(b))
        }
        GrammarENode::Next([a, b]) => {
            let a = rebuild(egraph, extractor, a, root_to_rule, current_rule, visiting).unwrap_or(IrNode::Epsilon);
            let b = rebuild(egraph, extractor, b, root_to_rule, current_rule, visiting).unwrap_or(IrNode::Epsilon);
            IrNode::Next(Box::new(a), Box::new(b))
        }
        GrammarENode::Minus([a, b]) => {
            let a = rebuild(egraph, extractor, a, root_to_rule, current_rule, visiting).unwrap_or(IrNode::Epsilon);
            let b = rebuild(egraph, extractor, b, root_to_rule, current_rule, visiting).unwrap_or(IrNode::Epsilon);
            IrNode::Minus(Box::new(a), Box::new(b))
        }
        GrammarENode::Negate(inner) => {
            let inner = rebuild(egraph, extractor, inner, root_to_rule, current_rule, visiting)
                .unwrap_or(IrNode::Epsilon);
            IrNode::Negate(Box::new(inner))
        }
        GrammarENode::OptionalWhitespace(inner) => {
            let inner = rebuild(egraph, extractor, inner, root_to_rule, current_rule, visiting)
                .unwrap_or(IrNode::Epsilon);
            IrNode::OptionalWhitespace(Box::new(inner))
        }
        GrammarENode::Map { inner, fn_id } => {
            let inner = rebuild(egraph, extractor, inner, root_to_rule, current_rule, visiting)
                .unwrap_or(IrNode::Epsilon);
            IrNode::Map {
                inner: Box::new(inner),
                fn_id: fn_id as FnId,
            }
        }
        GrammarENode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            let token = rebuild(egraph, extractor, token, root_to_rule, current_rule, visiting)
                .unwrap_or(IrNode::Epsilon);
            let fallback = rebuild(egraph, extractor, fallback, root_to_rule, current_rule, visiting)
                .unwrap_or(IrNode::Epsilon);
            // TokenDispatch arms carry Id continuations via the opaque
            // `arms` metadata — unchanged across the e-graph walk.
            // Rebuild each arm's continuation from its stored Id.
            let arms: Vec<crate::TokenDispatchArm> = arms
                .iter()
                .map(|arm| crate::TokenDispatchArm {
                    patterns: arm.patterns.clone(),
                    guard_byte: arm.guard_byte,
                    continuation: rebuild(
                        egraph,
                        extractor,
                        arm.continuation,
                        root_to_rule,
                        current_rule,
                        visiting,
                    )
                    .unwrap_or(IrNode::Epsilon),
                    map_fn: arm.map_fn,
                })
                .collect();
            IrNode::TokenDispatch {
                token: Box::new(token),
                arms,
                fallback: Box::new(fallback),
            }
        }
    };

    result
}
