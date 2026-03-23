//! Pass: Force-inline rules marked with `@inline`.
//!
//! Inlines `@inline` rules at ALL call sites, regardless of ref count or body size.
//! After inlining, run `prune_unreachable` to garbage-collect the dead rule.
//!
//! Guard: panics if `@inline` is applied to a directly self-recursive rule
//! (would cause infinite expansion).

use crate::{GrammarIR, IrNode};

/// Inline all rules marked with `force_inline` at their call sites.
pub fn force_inline(ir: &mut GrammarIR) {
    // Collect force-inline rule bodies.
    let inlineable: Vec<(u32, IrNode)> = ir
        .rules
        .iter()
        .filter(|r| r.meta.force_inline)
        .map(|r| {
            // Guard: no direct self-recursion.
            assert!(
                !body_has_self_ref(&r.body, r.id),
                "@inline rule '{}' is directly self-recursive — this would cause infinite expansion",
                ir.get_string(r.name)
            );
            (r.id, r.body.clone())
        })
        .collect();

    if inlineable.is_empty() {
        return;
    }

    // Build lookup table.
    let max_id = inlineable.iter().map(|(id, _)| *id).max().unwrap_or(0) as usize;
    let mut bodies: Vec<Option<IrNode>> = vec![None; max_id + 1];
    for (id, body) in &inlineable {
        bodies[*id as usize] = Some(body.clone());
    }

    // Rewrite all rule bodies.
    for rule in &mut ir.rules {
        rule.body = do_inline(
            std::mem::replace(&mut rule.body, IrNode::Epsilon),
            &bodies,
        );
    }
}

fn body_has_self_ref(node: &IrNode, rule_id: u32) -> bool {
    match node {
        IrNode::Ref(id) => *id == rule_id,
        IrNode::Seq(children) => children.iter().any(|c| body_has_self_ref(c, rule_id)),
        IrNode::Alt(branches, _) => branches.iter().any(|b| body_has_self_ref(&b.node, rule_id)),
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => body_has_self_ref(inner, rule_id),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            body_has_self_ref(a, rule_id) || body_has_self_ref(b, rule_id)
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => false,
    }
}

fn do_inline(node: IrNode, bodies: &[Option<IrNode>]) -> IrNode {
    match node {
        IrNode::Ref(id) => {
            if let Some(Some(body)) = bodies.get(id as usize) {
                // Recursively inline in case the inlined body itself references
                // other force-inline rules.
                do_inline(body.clone(), bodies)
            } else {
                IrNode::Ref(id)
            }
        }
        IrNode::Seq(children) => {
            IrNode::Seq(children.into_iter().map(|c| do_inline(c, bodies)).collect())
        }
        IrNode::Alt(branches, _dispatch) => {
            let branches = branches
                .into_iter()
                .map(|mut b| {
                    b.node = do_inline(b.node, bodies);
                    b
                })
                .collect();
            IrNode::Alt(branches, None)
        }
        IrNode::Repeat { inner, lo, hi } => IrNode::Repeat {
            inner: Box::new(do_inline(*inner, bodies)),
            lo,
            hi,
        },
        IrNode::Skip(a, b) => {
            IrNode::Skip(Box::new(do_inline(*a, bodies)), Box::new(do_inline(*b, bodies)))
        }
        IrNode::Next(a, b) => {
            IrNode::Next(Box::new(do_inline(*a, bodies)), Box::new(do_inline(*b, bodies)))
        }
        IrNode::Minus(a, b) => {
            IrNode::Minus(Box::new(do_inline(*a, bodies)), Box::new(do_inline(*b, bodies)))
        }
        IrNode::Negate(inner) => IrNode::Negate(Box::new(do_inline(*inner, bodies))),
        IrNode::OptionalWhitespace(inner) => {
            IrNode::OptionalWhitespace(Box::new(do_inline(*inner, bodies)))
        }
        IrNode::Map { inner, fn_id } => IrNode::Map {
            inner: Box::new(do_inline(*inner, bodies)),
            fn_id,
        },
        other => other,
    }
}
