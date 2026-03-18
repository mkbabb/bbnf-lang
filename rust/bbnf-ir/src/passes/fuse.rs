//! Pass: Single-use rule fusion.
//!
//! Inlines rules that are referenced exactly once across the entire grammar.
//! Unlike `inline_acyclic` (which has a size threshold), this pass inlines
//! regardless of body size since single-use rules have no sharing benefit.
//!
//! The primary benefit is improving dispatch table coverage: when a single-use
//! rule is inlined into an Alt branch, the larger alternation exposes more
//! FIRST set information for O(1) dispatch.
//!
//! Example:
//!   statement = assignment | ifStmt;
//!   assignment = name >> "=" >> expr;
//!   ifStmt = "if" >> expr >> "then" >> statement;
//!
//! After fusion (both are single-use):
//!   statement = (name >> "=" >> expr) | ("if" >> expr >> "then" >> statement);
//!
//! Now dispatch sees 'name' FIRST chars vs '"if"' → O(1) byte dispatch.

use crate::{GrammarIR, IrNode, RuleId};

/// Fuse single-use rules into their sole call site.
///
/// A rule is fusable when:
/// 1. It is not cyclic
/// 2. It is not the grammar entry point
/// 3. It is referenced exactly once across all rule bodies
///
/// After fusion, run `prune_unreachable` to clean up dead rules.
pub fn fuse_single_use(ir: &mut GrammarIR) {
    // Count references for each rule.
    let mut ref_counts = vec![0u32; ir.rules.len()];
    for rule in &ir.rules {
        count_refs(&rule.body, &mut ref_counts);
        // Also count refs in recover expressions.
        if let Some(ref recover) = rule.meta.recover {
            count_refs(recover, &mut ref_counts);
        }
    }

    // Identify fusable rules: single-use, acyclic, not entry.
    let fusable: Vec<(RuleId, IrNode)> = ir
        .rules
        .iter()
        .filter(|r| {
            r.id != ir.entry
                && !r.meta.is_cyclic
                && r.meta.scc_id.is_none()
                && ref_counts.get(r.id as usize).copied().unwrap_or(0) == 1
        })
        .map(|r| (r.id, r.body.clone()))
        .collect();

    if fusable.is_empty() {
        return;
    }

    // Build a lookup: rule_id → body (for fusable rules only).
    let max_id = fusable.iter().map(|(id, _)| *id).max().unwrap_or(0) as usize;
    let mut bodies: Vec<Option<IrNode>> = vec![None; max_id + 1];
    for (id, body) in &fusable {
        bodies[*id as usize] = Some(body.clone());
    }

    // Rewrite all rule bodies, inlining single-use refs.
    for rule in &mut ir.rules {
        rule.body = inline_single_use(
            std::mem::replace(&mut rule.body, IrNode::Epsilon),
            &bodies,
        );
    }
}

/// Count `Ref(id)` occurrences in an IR tree.
fn count_refs(node: &IrNode, counts: &mut [u32]) {
    match node {
        IrNode::Ref(id) => {
            if let Some(count) = counts.get_mut(*id as usize) {
                *count += 1;
            }
        }
        IrNode::Seq(children) => {
            for c in children {
                count_refs(c, counts);
            }
        }
        IrNode::Alt(branches, _) => {
            for b in branches {
                count_refs(&b.node, counts);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => count_refs(inner, counts),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            count_refs(a, counts);
            count_refs(b, counts);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
    }
}

/// Recursively replace `Ref(id)` with the inlined body where applicable.
fn inline_single_use(node: IrNode, bodies: &[Option<IrNode>]) -> IrNode {
    match node {
        IrNode::Ref(id) => {
            if let Some(Some(body)) = bodies.get(id as usize) {
                body.clone()
            } else {
                IrNode::Ref(id)
            }
        }
        IrNode::Seq(children) => {
            IrNode::Seq(children.into_iter().map(|c| inline_single_use(c, bodies)).collect())
        }
        IrNode::Alt(branches, _dispatch) => {
            // Clear dispatch — branch structure may have changed.
            let branches = branches
                .into_iter()
                .map(|mut b| {
                    // Don't inline bare Refs in Alt branches — codegen needs rule
                    // identity for proper enum variant wrapping. Refs inside Map
                    // wrappers or deeper sub-expressions are fine to inline.
                    if !matches!(&b.node, IrNode::Ref(_)) {
                        b.node = inline_single_use(b.node, bodies);
                    }
                    b
                })
                .collect();
            IrNode::Alt(branches, None)
        }
        IrNode::Repeat { inner, lo, hi } => IrNode::Repeat {
            inner: Box::new(inline_single_use(*inner, bodies)),
            lo,
            hi,
        },
        IrNode::Skip(a, b) => IrNode::Skip(
            Box::new(inline_single_use(*a, bodies)),
            Box::new(inline_single_use(*b, bodies)),
        ),
        IrNode::Next(a, b) => IrNode::Next(
            Box::new(inline_single_use(*a, bodies)),
            Box::new(inline_single_use(*b, bodies)),
        ),
        IrNode::Minus(a, b) => IrNode::Minus(
            Box::new(inline_single_use(*a, bodies)),
            Box::new(inline_single_use(*b, bodies)),
        ),
        IrNode::Negate(inner) => IrNode::Negate(Box::new(inline_single_use(*inner, bodies))),
        IrNode::OptionalWhitespace(inner) => {
            IrNode::OptionalWhitespace(Box::new(inline_single_use(*inner, bodies)))
        }
        IrNode::Map { inner, fn_id } => IrNode::Map {
            inner: Box::new(inline_single_use(*inner, bodies)),
            fn_id,
        },
        other => other,
    }
}
