//! IR optimization passes: epsilon elimination and literal merging.
//!
//! These are lightweight tree-rewrite passes that reduce node count and match operations.
//! Run early in the pipeline (before dispatch table generation).

use std::collections::HashMap;

use rayon::prelude::*;

use crate::{GrammarIR, IrNode};

// ── Epsilon Elimination ─────────────────────────────────────────────────────

/// Remove `Epsilon` nodes from sequences and flatten trivial wrappers.
///
/// Rewrites:
/// - `Seq([..., Epsilon, ...])` → `Seq([...])` (remove epsilon elements)
/// - `Seq([single])` → `single` (unwrap singleton sequences)
/// - `Alt([single], _)` → `single.node` (unwrap singleton alternations)
pub fn eliminate_epsilon(ir: &mut GrammarIR) {
    if ir.rules.len() >= 16 {
        ir.rules.par_iter_mut().for_each(|rule| {
            rule.body = elim_epsilon(std::mem::replace(&mut rule.body, IrNode::Epsilon));
        });
    } else {
        for rule in &mut ir.rules {
            rule.body = elim_epsilon(std::mem::replace(&mut rule.body, IrNode::Epsilon));
        }
    }
}

fn elim_epsilon(node: IrNode) -> IrNode {
    match node {
        IrNode::Seq(children) => {
            let cleaned: Vec<IrNode> = children
                .into_iter()
                .map(elim_epsilon)
                .filter(|n| !matches!(n, IrNode::Epsilon))
                .collect();
            match cleaned.len() {
                0 => IrNode::Epsilon,
                1 => cleaned.into_iter().next()
                    .expect("cleaned Seq verified to have exactly one element"),
                _ => IrNode::Seq(cleaned),
            }
        }
        IrNode::Alt(branches, dispatch) => {
            let cleaned: Vec<_> = branches
                .into_iter()
                .map(|mut b| {
                    b.node = elim_epsilon(b.node);
                    b
                })
                .collect();
            if cleaned.len() == 1 && dispatch.is_none() {
                cleaned.into_iter().next()
                    .expect("cleaned Alt verified to have exactly one branch").node
            } else {
                IrNode::Alt(cleaned, dispatch)
            }
        }
        IrNode::Repeat { inner, lo, hi } => {
            let inner = elim_epsilon(*inner);
            // Repeat(Epsilon, 0, _) → Epsilon (optional nothing = nothing)
            if matches!(&inner, IrNode::Epsilon) && lo == 0 {
                return IrNode::Epsilon;
            }
            IrNode::Repeat {
                inner: Box::new(inner),
                lo,
                hi,
            }
        }
        IrNode::Skip(a, b) => {
            let a = elim_epsilon(*a);
            let b = elim_epsilon(*b);
            // Skip(Epsilon, x) → x
            if matches!(&a, IrNode::Epsilon) {
                return b;
            }
            IrNode::Skip(Box::new(a), Box::new(b))
        }
        IrNode::Next(a, b) => {
            let a = elim_epsilon(*a);
            let b = elim_epsilon(*b);
            // Next(x, Epsilon) → x
            if matches!(&b, IrNode::Epsilon) {
                return a;
            }
            IrNode::Next(Box::new(a), Box::new(b))
        }
        IrNode::Minus(a, b) => IrNode::Minus(
            Box::new(elim_epsilon(*a)),
            Box::new(elim_epsilon(*b)),
        ),
        IrNode::Negate(inner) => IrNode::Negate(Box::new(elim_epsilon(*inner))),
        IrNode::OptionalWhitespace(inner) => {
            let inner = elim_epsilon(*inner);
            // OptionalWhitespace(OptionalWhitespace(x)) → OptionalWhitespace(x)
            if matches!(&inner, IrNode::OptionalWhitespace(_)) {
                return inner;
            }
            IrNode::OptionalWhitespace(Box::new(inner))
        }
        IrNode::Map { inner, fn_id } => IrNode::Map {
            inner: Box::new(elim_epsilon(*inner)),
            fn_id,
        },
        // Leaves are unchanged.
        other => other,
    }
}

// ── Literal Merging ─────────────────────────────────────────────────────────

/// Merge adjacent `Literal` nodes in sequences.
///
/// Rewrites `Seq([Lit("a"), Lit("b"), ...])` → `Seq([Lit("ab"), ...])`.
/// Reduces the number of match operations in the interpreter.
pub fn merge_literals(ir: &mut GrammarIR) {
    // Build dedup index for the existing string table.
    let mut string_dedup: HashMap<String, u32> = ir
        .strings
        .iter()
        .enumerate()
        .map(|(i, s)| (s.clone(), i as u32))
        .collect();

    for rule in &mut ir.rules {
        rule.body = merge_lits(
            std::mem::replace(&mut rule.body, IrNode::Epsilon),
            &mut ir.strings,
            &mut string_dedup,
        );
    }
}

/// Intern a string, reusing an existing entry if present.
fn intern_string(s: String, strings: &mut Vec<String>, dedup: &mut HashMap<String, u32>) -> u32 {
    if let Some(&existing) = dedup.get(&s) {
        return existing;
    }
    let sid = strings.len() as u32;
    dedup.insert(s.clone(), sid);
    strings.push(s);
    sid
}

fn merge_lits(node: IrNode, strings: &mut Vec<String>, dedup: &mut HashMap<String, u32>) -> IrNode {
    match node {
        IrNode::Seq(children) => {
            let mut merged: Vec<IrNode> = Vec::with_capacity(children.len());
            let mut pending_lit: Option<String> = None;

            for child in children {
                let child = merge_lits(child, strings, dedup);
                if let IrNode::Literal(sid) = &child {
                    let s = strings[*sid as usize].clone();
                    if let Some(ref mut acc) = pending_lit {
                        acc.push_str(&s);
                        continue;
                    } else {
                        pending_lit = Some(s);
                        continue;
                    }
                }
                // Flush pending literal.
                if let Some(acc) = pending_lit.take() {
                    let sid = intern_string(acc, strings, dedup);
                    merged.push(IrNode::Literal(sid));
                }
                merged.push(child);
            }
            // Flush trailing pending.
            if let Some(acc) = pending_lit.take() {
                let sid = intern_string(acc, strings, dedup);
                merged.push(IrNode::Literal(sid));
            }

            match merged.len() {
                0 => IrNode::Epsilon,
                1 => merged.into_iter().next()
                    .expect("merged Seq verified to have exactly one element"),
                _ => IrNode::Seq(merged),
            }
        }
        IrNode::Alt(branches, dispatch) => {
            let cleaned: Vec<_> = branches
                .into_iter()
                .map(|mut b| {
                    b.node = merge_lits(b.node, strings, dedup);
                    b
                })
                .collect();
            IrNode::Alt(cleaned, dispatch)
        }
        IrNode::Repeat { inner, lo, hi } => IrNode::Repeat {
            inner: Box::new(merge_lits(*inner, strings, dedup)),
            lo,
            hi,
        },
        IrNode::Skip(a, b) => IrNode::Skip(
            Box::new(merge_lits(*a, strings, dedup)),
            Box::new(merge_lits(*b, strings, dedup)),
        ),
        IrNode::Next(a, b) => IrNode::Next(
            Box::new(merge_lits(*a, strings, dedup)),
            Box::new(merge_lits(*b, strings, dedup)),
        ),
        IrNode::Minus(a, b) => IrNode::Minus(
            Box::new(merge_lits(*a, strings, dedup)),
            Box::new(merge_lits(*b, strings, dedup)),
        ),
        IrNode::Negate(inner) => IrNode::Negate(Box::new(merge_lits(*inner, strings, dedup))),
        IrNode::OptionalWhitespace(inner) => {
            IrNode::OptionalWhitespace(Box::new(merge_lits(*inner, strings, dedup)))
        }
        IrNode::Map { inner, fn_id } => IrNode::Map {
            inner: Box::new(merge_lits(*inner, strings, dedup)),
            fn_id,
        },
        other => other,
    }
}

// ── Tests ───────────────────────────────────────────────────────────────────
