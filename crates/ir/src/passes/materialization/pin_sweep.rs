//! Consumer-pin fix-up — Pass 2 of `classify_materialization`.
//!
//! Tranche AB.0. After the bottom-up Pass 1 assigns each node its
//! most aggressive legal class, Pass 2 walks every rule with a
//! consumer pin (`@pretty`, `@debug`, `preserve_identity`, closure-
//! typed `Map` in the body, or the grammar's entry rule) and
//! transitively widens its subtree to `MustTape`. A pin is a load-
//! bearing guarantee: the prettify emitter walks parser state, the
//! debug emitter dumps rule boundaries, and `parse()` dispatches
//! through the entry rule function — none can work if the parser
//! elides the records.
//!
//! The entry rule is pinned unconditionally because the generated
//! `parse()` helper calls `Self::__<entry>(state, &mut builder)` by
//! name; if the entry rule's body materialized as `TransparentElide`
//! the emitter would skip its function and the `parse()` helper
//! would reference a symbol that does not exist.

use std::collections::HashMap;

use crate::dag::NodeId;
use crate::{GrammarIR, IrNode, RuleId};

use super::lattice::MaterializationClass;

/// Walk every pinned rule and widen its subtree to `MustTape`.
///
/// A rule is pinned when ANY of:
///
/// - `rule.id == ir.entry` — the grammar's entry rule (load-bearing
///   for the `parse()` helper's entry dispatch)
/// - `rule.meta.directives.pretty.is_some()` — `@pretty` rule
/// - `rule.meta.directives.debug` — `@debug` rule
/// - `ir.debug_all` — global `@debug * ;` directive
/// - `rule.meta.preserve_identity` — structural mode / identity
///   preservation
///
/// A pinned rule transitively pins every descendant in its body,
/// AND every rule it references. The walk is a BFS from the pinned
/// rule roots with cycle-safe rule visitation.
pub fn apply_consumer_pins(ir: &GrammarIR, map: &mut HashMap<NodeId, MaterializationClass>) {
    // Identify the initial pinned rule set. The entry rule is
    // always pinned so that the emitter produces a `__<entry>`
    // function the generated `parse()` helper can dispatch into.
    let debug_all = ir.debug_all;
    let mut pinned_rules: Vec<RuleId> = ir
        .rules
        .iter()
        .filter(|r| r.id == ir.entry || is_rule_pinned(r, debug_all))
        .map(|r| r.id)
        .collect();

    if pinned_rules.is_empty() {
        return;
    }

    // BFS closure over rule references, expanding the pinned set.
    let mut pinned_set: std::collections::HashSet<RuleId> = pinned_rules.iter().copied().collect();
    while let Some(rule_id) = pinned_rules.pop() {
        let Some(rule) = ir.rules.get(rule_id as usize) else {
            continue;
        };
        // Pin every NodeId in this rule's body to MustTape, and
        // collect any transitively-referenced rules.
        pin_subtree(ir, &rule.body, map, &mut pinned_rules, &mut pinned_set);
    }
}

/// True iff a rule must be pinned to `MustTape` based on its meta /
/// directives alone. Cross-rule transitive pinning is applied by
/// the BFS in [`apply_consumer_pins`].
#[inline]
fn is_rule_pinned(rule: &crate::IrRule, debug_all: bool) -> bool {
    rule.meta.preserve_identity
        || rule.meta.directives.pretty.is_some()
        || rule.meta.directives.debug
        || debug_all
}

/// Walk a rule-body subtree, pin every NodeId to `MustTape`, and
/// enqueue any referenced rules for further pinning.
fn pin_subtree(
    ir: &GrammarIR,
    node: &IrNode,
    map: &mut HashMap<NodeId, MaterializationClass>,
    pinned_rules: &mut Vec<RuleId>,
    pinned_set: &mut std::collections::HashSet<RuleId>,
) {
    // Pin this tree position.
    if let Some(id) = ir.dag.as_ref().and_then(|dag| dag.node_for(node)) {
        map.insert(id, MaterializationClass::MustTape);
    }

    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}

        IrNode::Ref(target) => {
            if pinned_set.insert(*target) {
                pinned_rules.push(*target);
            }
        }

        IrNode::Seq(children) => {
            for child in children {
                pin_subtree(ir, child, map, pinned_rules, pinned_set);
            }
        }

        IrNode::Alt(branches, _) => {
            for b in branches {
                pin_subtree(ir, &b.node, map, pinned_rules, pinned_set);
            }
        }

        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => {
            pin_subtree(ir, inner, map, pinned_rules, pinned_set);
        }

        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            pin_subtree(ir, a, map, pinned_rules, pinned_set);
            pin_subtree(ir, b, map, pinned_rules, pinned_set);
        }

        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            pin_subtree(ir, token, map, pinned_rules, pinned_set);
            for arm in arms {
                pin_subtree(ir, &arm.continuation, map, pinned_rules, pinned_set);
            }
            pin_subtree(ir, fallback, map, pinned_rules, pinned_set);
        }
    }
}
