//! Scalar-shape detector — single typed leaf not admitted by the
//! primary shape detectors.
//!
//! # Predicate
//!
//! A rule is Scalar-shaped when its body is a single leaf after
//! stripping trivial wrappers:
//!
//! 1. **Literal body** — canonical case. Typically a single-byte
//!    structural separator the surrounding shape consumes inline
//!    (JSON's `comma = "," ?w` / `colon = ":" ?w`).
//!
//! 2. **Ref-to-classified body** — AX.W0a.2.b. Transparent-alias
//!    rules whose body is a single `Ref(target)` where `target`
//!    itself carries a classified shape tag (the non-structural
//!    pipeline's rule-unification sometimes yields such alias forms:
//!    BBNF `string_lit` unifies with `import_path`, BbnfBootstrap's
//!    `lhs = identifier` stays separate, etc.). The emitter
//!    delegates to the target's shape fn.
//!
//! # Projection
//!
//! Pure structural inspection of the rule body + a classification
//! read (for case 2) via [`GrammarIR::shape_assignments`].

use crate::passes::inspect::unwrap_map_ow;
use crate::types::{GrammarIR, IrNode, RuleId};

/// Detect Scalar-shape: a single Literal or Ref-to-classified leaf.
///
/// # Precedence
///
/// This detector runs AFTER the primary detectors (Object / Array /
/// String / Number / Keyword / HRegex / Pratt / Unordered / ArgList /
/// Wrap / AltDispatch / Flat) per the dispatch order in
/// [`super::classify_rule`]; its job is to catch single-leaf rules
/// earlier detectors did not admit.
pub fn detect_scalar(rule_id: RuleId, ir: &GrammarIR) -> bool {
    let rule = &ir.rules[rule_id as usize];
    let body = unwrap_map_ow(&rule.body);
    match body {
        IrNode::Literal(_) => true,
        IrNode::Ref(rid) => {
            // Transparent-alias rule — the body is just a Ref to
            // another rule. Admit when that target has a classified
            // shape tag; the Scalar emitter delegates via
            // `emit_ref_call_tape` at the emission site.
            ir.shape_assignments.get(*rid).is_classified()
        }
        _ => false,
    }
}
