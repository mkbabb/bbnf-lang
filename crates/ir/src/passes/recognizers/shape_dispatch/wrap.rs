//! Wrap-shape detector — transparent `Alt(Ref, Ref, …)` dispatcher.
//!
//! # Predicate
//!
//! A rule is Wrap-shaped when its body is an `Alt` whose every branch
//! is a plain `Ref` (no inline content). Such an Alt is a pure
//! structural dispatcher — it emits no compound record itself; the
//! chosen branch's `Ref` resolves to the real shape.
//!
//! # Canonical sources
//!
//! - JSON `value = object | array | string | number | bool | null` —
//!   a 6-way Alt of Refs to the primary shape rules per
//!   `grammar/json/json.bbnf`.
//! - CSS `color = colorMix | colorFn | hex | colorFunction |
//!   namedColor` per `grammar/css/l4/color.bbnf:321`.
//! - CSS `atRule` family — Alt of Refs to per-atRule rules.
//! - Sheets `range_end = cell_ref | /\$?[A-Za-z]{1,3}/ | /\$?\d+/`
//!   per `grammar/google-sheets/google-sheets.bbnf:72`. Note the
//!   3-Alt mixes Ref and Regex; the detector admits it because each
//!   branch is a single leaf (Ref OR Regex; no Seq / Alt inline).
//! - BBNF `rhs = closure | alternation` per
//!   `grammar/bbnf/bbnf.bbnf:52`.
//!
//! # Exclusion guards
//!
//! Wrap excludes:
//!
//! - Literal-led Alts (those are Keyword).
//! - Single-branch Alts (fold into the single branch).
//! - Alts whose branches contain inline Literals / Seqs / Repeats
//!   (those are decision-point shapes the emitter cannot collapse
//!   to a byte-dispatch tail-call).
//!
//! # Projection
//!
//! Pure structural inspection of the rule body. No new mining.

use crate::passes::inspect::unwrap_map_ow;
use crate::types::{GrammarIR, IrNode, RuleId};

/// Detect Wrap-shape: an Alt-of-Ref-or-Regex-only dispatcher.
pub fn detect_wrap(rule_id: RuleId, ir: &GrammarIR) -> bool {
    let rule = &ir.rules[rule_id as usize];
    let body = unwrap_map_ow(&rule.body);
    let IrNode::Alt(branches, _) = body else {
        return false;
    };
    if branches.len() < 2 {
        return false;
    }
    // Every branch must be a plain Ref OR a plain Regex (Sheets's
    // `range_end` mixes them). No Literal, no Seq, no Repeat, no
    // Alt. A Literal-led Alt is Keyword's territory; an Alt
    // containing a Seq is decision-point + the emitter can't
    // collapse to a tail-call.
    //
    // Accept `Ref` OR `Regex` branches only.
    //
    // We do NOT admit an Alt of all-Literal branches — that's
    // Keyword's precedence (runs earlier). Literal branches reach
    // this detector only when Keyword rejected them (e.g. a literal
    // too long / too many branches), in which case Wrap isn't the
    // right shape either.
    //
    // Note: `Ref + Regex` mix IS admitted (Sheets `range_end`).
    let _ = ir;
    branches.iter().all(|b| {
        let inner = unwrap_map_ow(&b.node);
        matches!(inner, IrNode::Ref(_) | IrNode::Regex(_))
    })
}
