//! Pratt-shape detector — operator-chain head with precedence resolution.
//!
//! # Predicate
//!
//! A rule is Pratt-shaped when its body matches the canonical
//! operator-chain rung shape — `Seq(operand, Repeat(Seq(op, operand)))`
//! — AND the operator position carries at least one extractable
//! literal operator token. The emitted `parse_pratt_*` body dispatches
//! on per-byte LUT entries; a rule whose operator alphabet is
//! regex-only (e.g. CSS `complexSelector`'s `combinator = /\s*>\s*/ |
//! /\s*\+\s*/ | /\s*~\s*/ | /\s+/`) produces an empty LUT + empty
//! `PRECEDENCE_ENTRIES_*` slice and its emitted body exits the inner
//! loop on the first byte, silently dropping every operator
//! occurrence. Classifying such rules as Pratt is a structural
//! mismatch — the detector must reject them so they fall through to
//! Flat (or another non-Pratt shape) whose emitter handles the
//! iteration wrapper without a byte-dispatch assumption.
//!
//! # Two-stage admission
//!
//! 1. **Structural** — the rule's body matches the operator-chain
//!    rung shape. Sourced from the DAG's per-NodeId facts
//!    (`operator_chain`) populated by
//!    [`crate::passes::recognizers::node_facts::recognize_tree`].
//!    `Map` / `OptionalWhitespace` wrappers are peeled via
//!    [`crate::passes::inspect::unwrap_map_ow`] before the NodeId
//!    lookup so wrapped operator-chain bodies surface the same fact.
//!
//! 2. **Operator-literal mineable** — [`match_operator_chain_rule`]
//!    must succeed AND produce at least one operator entry. This is
//!    the semantic gate: the structural shape is necessary but not
//!    sufficient; a rung whose operator is a Regex (no `Ref` to an
//!    Alt-of-literal, no inlined Alt-of-literal, no Literal) cannot
//!    populate `PRECEDENCE_ENTRIES_<rule>` and must not route through
//!    the Pratt emitter.
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
//! # Rejection cases (AX.W0a.2.p)
//!
//! - CSS `complexSelector = compoundSelector, (combinator,
//!   compoundSelector)*` where `combinator` is regex-only.
//! - CSS `selectorList = complexSelector, (/\s*,\s*/ >>
//!   complexSelector)*` — inline regex op.
//! - CSS `relativeSelectorList = relativeSelector, (/\s*,\s*/ >>
//!   relativeSelector)*` — inline regex op.
//! - CSS `keyframeSel = keyframeStop, (/\s*,\s*/ >> keyframeStop)*` —
//!   inline regex op.
//!
//! All four had `operator_chain: true` + empty
//! `PRECEDENCE_ENTRIES_*` pre-W0a.2.p, producing shape functions that
//! parsed only the leading operand and silently ignored every
//! subsequent operator. Narrowing drops them to Flat so the iteration
//! wrapper ships.
//!
//! # Projection
//!
//! Reads [`GrammarIR::node_facts`] via the DAG's per-rule-body
//! [`crate::dag::NodeId`]. The `operator_chain` bit is the
//! Pratt-admissibility *prerequisite*; the second-stage admission via
//! [`match_operator_chain_rule`] decides whether the rule's
//! operator alphabet is literal-mineable.
//!
//! Note that the operator-chain tower admits rungs only in
//! pairs — `collect_precedence_chain` requires ≥ 2 rungs. A top-most
//! chain head that transitively references a second rung forms the
//! tower; individual rungs all satisfy the same per-body operator-
//! chain predicate. The detector admits EVERY literal-mineable rung of
//! a mined chain — the emitter composes them into one `ShuntingYard`
//! state at the backend.

use crate::passes::inspect::unwrap_map_ow;
use crate::passes::recognizers::grammar_facts::match_operator_chain_rule;
use crate::types::{GrammarIR, RuleId};

/// Detect Pratt-shape: the rule's body is an operator-chain rung (or
/// the outer head of a chain tower) AND the operator position carries
/// at least one extractable literal operator token.
pub fn detect_pratt(rule_id: RuleId, ir: &GrammarIR) -> bool {
    let rule = &ir.rules[rule_id as usize];
    // Stage 1 — structural. The operator-chain flag rides on the
    // per-NodeId `node_facts` map populated by
    // [`crate::passes::recognizers::node_facts::recognize_tree`]. Peel
    // Map / OptionalWhitespace wrappers via `unwrap_map_ow` so a
    // rule whose body is `Map(Seq(operand, Repeat(...))) ` or
    // `OptionalWhitespace(Seq(...))` surfaces the same per-Seq fact
    // the bare-Seq case does. The DAG must be present — every
    // production grammar populates `ir.dag` before recognizer mining;
    // its absence is a pipeline-ordering bug, not a soft-fall-through.
    let structural = ir
        .dag
        .as_ref()
        .and_then(|dag| dag.node_for(unwrap_map_ow(&rule.body)))
        .and_then(|node_id| ir.node_facts.get(&node_id))
        .is_some_and(|facts| facts.operator_chain);
    if !structural {
        return false;
    }
    // Stage 2 — literal-mineable. A rule's operator position must
    // admit at least one `PrecedenceEntry`. `match_operator_chain_rule`
    // returns `Some((_, entries, _))` only when the operator resolves
    // to an Alt-of-literal (`Ref` or inlined), a single Literal, or a
    // prefix-factored Seq whose suffix is an Alt-of-literal. A
    // regex-only operator (CSS `combinator` = pure Alt of regexes, or
    // inline `/\s*,\s*/`) yields `None` — those rules fall through to
    // Flat / Scalar so their iteration wrapper emits a byte-walking
    // loop rather than the Pratt LUT dispatch that their empty LUT
    // would silently short-circuit.
    match match_operator_chain_rule(ir, rule) {
        Some((_, entries, _)) => !entries.is_empty(),
        None => false,
    }
}
