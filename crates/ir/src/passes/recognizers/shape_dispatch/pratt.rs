//! Pratt-shape detector — operator-chain head with precedence resolution.
//!
//! # Predicate
//!
//! A rule is Pratt-shaped when its body matches the canonical
//! operator-chain rung shape — `Seq(operand, Repeat(Seq(op, operand)))`
//! — such that the chain detector in
//! [`crate::passes::recognizers::dta::collect_precedence_chain`] would
//! admit it as part of a multi-rung operator tower (≥ 2 rungs
//! collapsing into one [`ShuntingYard`](
//! crate::passes::recognizers::dta::DtaState::ShuntingYard) state).
//!
//! The detector does not invoke the DTA lift (which happens
//! downstream of the recognizer miners) — it reuses the structural
//! predicate [`node_facts.operator_chain`](
//! crate::passes::patterns::NodeFacts::operator_chain), which the
//! [`crate::passes::recognizers::node_facts::recognize_tree`] walk
//! populates during Phase 2 of
//! [`crate::passes::recognizers::mine_recognizers`] for any Seq that
//! unwraps to `Seq(operand, Repeat(Seq(op, rhs)))`.
//!
//! The rule's body's Seq [`crate::dag::NodeId`] carries the
//! `operator_chain: true` fact when the body matches; the detector
//! simply checks that flag.
//!
//! # Canonical sources
//!
//! - Sheets operator tower (6 rungs): `comparison_expr → concat_expr →
//!   add_expr → mul_expr → exp_expr → unary_expr → postfix_expr` per
//!   `grammar/google-sheets/google-sheets.bbnf:100-118`.
//! - CSS math: `mathProduct = mathValue , (("*"|"/") >> mathValue) *`
//!   + `mathExpr = mathProduct , (("+"|"-") >> mathProduct) *` per
//!   `grammar/css/l4/values.bbnf:49-50`.
//! - BBNF value expression tower: `value_or → value_and → value_cmp →
//!   value_add → value_mul → value_unary` per
//!   `grammar/bbnf/expressions.bbnf`.
//!
//! # Projection
//!
//! Reads [`GrammarIR::node_facts`] via the DAG's per-rule-body
//! [`crate::dag::NodeId`]. The `operator_chain` bit is the authoritative
//! Pratt-admissibility signal: it fires exactly when the rule's body
//! is a canonical operator-chain rung Seq.
//!
//! Note that the operator-chain tower admits rungs only in
//! pairs — `collect_precedence_chain` requires ≥ 2 rungs. A top-most
//! chain head that transitively references a second rung forms the
//! tower; individual rungs all satisfy the same per-body operator-
//! chain predicate. The detector therefore admits EVERY rung of a
//! mined chain — the emitter composes them into one `ShuntingYard`
//! state at the backend.

use crate::passes::inspect::unwrap_map_ow;
use crate::types::{GrammarIR, RuleId};

/// Detect Pratt-shape: the rule's body is an operator-chain rung (or
/// the outer head of a chain tower).
pub fn detect_pratt(rule_id: RuleId, ir: &GrammarIR) -> bool {
    let rule = &ir.rules[rule_id as usize];
    // The operator-chain flag rides on the rule's PatternAnnotations,
    // which the `mine_recognizers` Phase 1 pass populates for every
    // rule whose top-level Seq matches `Seq(operand, Repeat(Seq(op,
    // rhs)))`. Reading the rule-level annotation (not the node-facts)
    // ensures we hit the *outer* chain-rung body even after the body
    // is Map / OptionalWhitespace wrapped.
    //
    // Falls back to the DAG's per-NodeId facts when the annotation
    // isn't present — some rules with `?w`-wrapped bodies surface the
    // operator_chain bit on the inner Seq NodeId rather than on the
    // rule-level annotation.
    if let Some(ann) = ir.pattern_annotations.get(&rule.id) {
        if ann.is_operator_chain {
            return true;
        }
    }
    let Some(dag) = ir.dag.as_ref() else {
        return false;
    };
    let body = unwrap_map_ow(&rule.body);
    let Some(node_id) = dag.node_for(body) else {
        return false;
    };
    ir.node_facts
        .get(&node_id)
        .is_some_and(|facts| facts.operator_chain)
}
