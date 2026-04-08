//! E-graph → `IrNode` extraction and write-back.
//!
//! After saturation, the e-graph contains every equivalent form of
//! each rule's body. This module walks each rule's root e-class via
//! `Extractor` (cost-model-guided), rebuilds a concrete `IrNode` tree
//! from the chosen best e-nodes, and writes the results back into a
//! `GrammarIR`.
//!
//! The reconstruction handles cycles defensively: e-classes visited
//! during a recursive walk are cached to prevent infinite recursion
//! via self-referential class structure (which can happen if the
//! e-graph collapses a `Ref(x)` class with its expansion via
//! `InlineEligibleRef`). On cycle detection the walk emits a
//! fallback `Ref` node.

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
    // Snapshot original rule ids (we mutate ir.rules in the loop).
    let rule_ids: Vec<RuleId> = ir.rules.iter().map(|r| r.id).collect();
    for rule_id in rule_ids {
        let Some(&root_id) = rule_body_ids.get(&rule_id) else {
            continue;
        };
        let mut visiting = HashMap::new();
        if let Some(body) = rebuild(egraph, &extractor, root_id, &mut visiting) {
            if let Some(rule) = ir.rules.iter_mut().find(|r| r.id == rule_id) {
                rule.body = body;
            }
        }
    }
}

/// Rebuild a single `IrNode` by recursively extracting the best form
/// for `root` and its transitive children. `visiting` guards against
/// cycles.
pub fn extract_ir_node(
    egraph: &EGraph<GrammarENode, GrammarAnalysis>,
    cost: &GrammarCostModel,
    root: Id,
) -> Option<IrNode> {
    let extractor = Extractor::new(egraph, cost);
    let mut visiting = HashMap::new();
    rebuild(egraph, &extractor, root, &mut visiting)
}

/// Recursive best-node → IrNode reconstruction. Uses `visiting` to
/// short-circuit cycles (cache the placeholder Ref the first time we
/// see a class, so recursive calls back to it don't loop).
fn rebuild(
    egraph: &EGraph<GrammarENode, GrammarAnalysis>,
    extractor: &Extractor<
        '_,
        GrammarENode,
        GrammarAnalysis,
        GrammarCostModel,
    >,
    id: Id,
    visiting: &mut HashMap<Id, ()>,
) -> Option<IrNode> {
    let canonical = egraph.find_ref(id);
    if visiting.contains_key(&canonical) {
        // Cycle — emit a placeholder. The only sensible cycle target
        // at the IR level is a `Ref` (which can self-reference via the
        // rule graph). If the best form isn't a Ref, drop to Epsilon
        // so extraction still terminates.
        for node in egraph.class(canonical).iter() {
            if let GrammarENode::Ref(rid) = node {
                return Some(IrNode::Ref(*rid));
            }
        }
        return Some(IrNode::Epsilon);
    }
    visiting.insert(canonical, ());

    let best = extractor.best_node(canonical)?.clone();
    let result = match best {
        GrammarENode::Literal(sid) => IrNode::Literal(sid as StringId),
        GrammarENode::Regex(sid) => IrNode::Regex(sid as StringId),
        GrammarENode::Epsilon => IrNode::Epsilon,
        GrammarENode::Ref(rid) => IrNode::Ref(rid),
        GrammarENode::Seq(children) => {
            let rebuilt: Vec<IrNode> = children
                .iter()
                .filter_map(|&cid| rebuild(egraph, extractor, cid, visiting))
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
                    rebuild(egraph, extractor, cid, visiting).map(|node| AltBranch {
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
            let inner = rebuild(egraph, extractor, inner, visiting)
                .unwrap_or(IrNode::Epsilon);
            IrNode::Repeat {
                inner: Box::new(inner),
                lo,
                hi,
            }
        }
        GrammarENode::Skip([a, b]) => {
            let a = rebuild(egraph, extractor, a, visiting).unwrap_or(IrNode::Epsilon);
            let b = rebuild(egraph, extractor, b, visiting).unwrap_or(IrNode::Epsilon);
            IrNode::Skip(Box::new(a), Box::new(b))
        }
        GrammarENode::Next([a, b]) => {
            let a = rebuild(egraph, extractor, a, visiting).unwrap_or(IrNode::Epsilon);
            let b = rebuild(egraph, extractor, b, visiting).unwrap_or(IrNode::Epsilon);
            IrNode::Next(Box::new(a), Box::new(b))
        }
        GrammarENode::Minus([a, b]) => {
            let a = rebuild(egraph, extractor, a, visiting).unwrap_or(IrNode::Epsilon);
            let b = rebuild(egraph, extractor, b, visiting).unwrap_or(IrNode::Epsilon);
            IrNode::Minus(Box::new(a), Box::new(b))
        }
        GrammarENode::Negate(inner) => {
            let inner = rebuild(egraph, extractor, inner, visiting)
                .unwrap_or(IrNode::Epsilon);
            IrNode::Negate(Box::new(inner))
        }
        GrammarENode::OptionalWhitespace(inner) => {
            let inner = rebuild(egraph, extractor, inner, visiting)
                .unwrap_or(IrNode::Epsilon);
            IrNode::OptionalWhitespace(Box::new(inner))
        }
        GrammarENode::Map { inner, fn_id } => {
            let inner = rebuild(egraph, extractor, inner, visiting)
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
            let token = rebuild(egraph, extractor, token, visiting)
                .unwrap_or(IrNode::Epsilon);
            let fallback = rebuild(egraph, extractor, fallback, visiting)
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

    visiting.remove(&canonical);
    Some(result)
}
