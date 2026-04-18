//! AW-V.W3.1 — Shape-dispatch classifier.
//!
//! Assigns each grammar rule a [`ShapeTag`] that downstream per-shape
//! emitter modules (landed W3.2 at
//! `crates/core/src/backend/rust/emitter/shapes/`) consume to route
//! codegen. Every detector is a pure projection from existing
//! recognizer-miner outputs (DisjointFirstMiner, KeywordStatsMiner,
//! DelimScanMiner, PatternAlphabetMiner, list-rule miner, operator-
//! chain miner, QuotedStringMiner, etc.) — no new mining, no grammar-
//! name branches. Per-grammar OUTPUT differs because per-grammar IR
//! differs; per-grammar MECHANISM does not.
//!
//! # Shape taxonomy (AW-V.md §"Shape taxonomy — 11 categories")
//!
//! W3 implements: **Object**, **Array**, **String**, **Number**,
//! **Keyword**, **Scalar**. W4 extends with **Pratt**, **Unordered**,
//! **ArgList**, **Flat**, **Wrap**, **HRegex**. Rules that match no
//! shape detector stay [`ShapeTag::None`] and route through
//! `__dta_walker_inline::run` per the AX cold-path replay contract.
//!
//! The W4 detectors are defined here with stub bodies returning
//! `false` so the wire contract is in place for W3.2's emitter lift;
//! full bodies land W4 alongside the corresponding emitter modules.
//!
//! # Outputs
//!
//! [`shape_dispatch`] returns a [`ShapeAssignments`] mapping every
//! classified rule to its [`ShapeTag`]. Rules absent from the map are
//! implicitly `ShapeTag::None` — the emitter routes them to the
//! walker fallback.
//!
//! # Execution
//!
//! The classifier runs from
//! [`crate::passes::recognizers::mine_recognizers`] after every
//! miner has populated its sidecar on `GrammarIR`, so every detector
//! reads committed data. The per-rule classification dispatches
//! through the sub-modules:
//!
//! - [`object::detect_object`]
//! - [`array::detect_array`]
//! - [`string::detect_string`]
//! - [`number::detect_number`]
//! - [`keyword::detect_keyword`]
//! - [`scalar::detect_scalar`]
//!
//! Precedence is by specificity: the more specific shape wins. The
//! dispatch order (Object → Array → String → Number → Keyword →
//! Scalar) matches the AW-V.md §"Shape taxonomy" table order.

pub mod array;
pub mod keyword;
pub mod number;
pub mod object;
pub mod scalar;
pub mod string;

use std::collections::HashMap;

use crate::types::{GrammarIR, RuleId};

/// Per-rule shape assignment.
///
/// Downstream per-shape emitter modules consume this mapping to
/// decide which inline parse function shape to emit per rule.
///
/// # Layout
///
/// A `HashMap<RuleId, ShapeTag>` storing only classified rules.
/// Rules absent from the map fall back to [`ShapeTag::None`] via
/// [`get`](Self::get) — the emitter routes them to
/// `__dta_walker_inline::run` per the AX replay contract.
#[derive(Debug, Clone, Default)]
pub struct ShapeAssignments {
    /// Per-rule shape tag. Rules absent default to [`ShapeTag::None`].
    pub per_rule: HashMap<RuleId, ShapeTag>,
}

impl ShapeAssignments {
    /// Look up the shape tag for a rule; returns [`ShapeTag::None`]
    /// when the rule was not classified.
    #[inline]
    pub fn get(&self, rule: RuleId) -> ShapeTag {
        self.per_rule
            .get(&rule)
            .copied()
            .unwrap_or(ShapeTag::None)
    }

    /// Record `tag` for `rule`. Overwrites any previous assignment.
    #[inline]
    pub fn assign(&mut self, rule: RuleId, tag: ShapeTag) {
        self.per_rule.insert(rule, tag);
    }

    /// Number of rules classified to a non-`None` shape.
    #[inline]
    pub fn classified_count(&self) -> usize {
        self.per_rule
            .values()
            .filter(|t| !matches!(t, ShapeTag::None))
            .count()
    }

    /// Number of rules classified to `tag`.
    pub fn count_of(&self, tag: ShapeTag) -> usize {
        self.per_rule.values().filter(|&&t| t == tag).count()
    }
}

/// Shape categories the per-shape emitter modules consume.
///
/// W3 actively classifies: [`Object`](Self::Object),
/// [`Array`](Self::Array), [`String`](Self::String),
/// [`Number`](Self::Number), [`Keyword`](Self::Keyword),
/// [`Scalar`](Self::Scalar).
///
/// W4 extends with [`Pratt`](Self::Pratt),
/// [`Unordered`](Self::Unordered), [`ArgList`](Self::ArgList),
/// [`Flat`](Self::Flat), [`Wrap`](Self::Wrap),
/// [`HRegex`](Self::HRegex). Their detector signatures are defined
/// now; full bodies land W4.
///
/// [`None`](Self::None) routes the rule through
/// `__dta_walker_inline::run` (the AX cold-path replay surface).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ShapeTag {
    /// JSON `object`, CSS declaration block — key → value compound
    /// with a closed key alphabet.
    Object,
    /// JSON `array`, CSS selector list, comma-separated values —
    /// homogeneous repeated compound.
    Array,
    /// JSON `string`, CSS `<string>`, BBNF string literal — quoted-
    /// string regex leaf.
    String,
    /// JSON `number`, CSS number, Sheets number — numeric regex leaf.
    Number,
    /// JSON `true`/`false`/`null`, BBNF directive prefix, CSS `@`-rule
    /// head — literal or short Alt-of-literal.
    Keyword,
    /// Single typed leaf (not Object / Array / String / Number /
    /// Keyword) — e.g. a `-> u8` or `-> i64` Map over a single literal
    /// that isn't caught by `Keyword`.
    Scalar,

    /// W4 — operator-chain head (Sheets arithmetic tower, CSS
    /// `calc()` math body).
    Pratt,
    /// W4 — disjoint-FIRST Alt under a `Repeat {lo: 1, ..}` (CSS
    /// `compoundSelector`).
    Unordered,
    /// W4 — `name(arg, arg, …)` positional call (CSS `calc` /
    /// `rgb(…)` / `url(…)`, Sheets `func_call`).
    ArgList,
    /// W4 — typed `Seq(head, (literal|ref|regex)+)` with literal or
    /// keyword head (CSS `*Decl` family, BBNF directive bodies).
    Flat,
    /// W4 — transparent `Alt(Ref, Ref, …)` dispatcher (CSS `color` /
    /// `atRule`, Sheets `range_end`).
    Wrap,
    /// W4 — regex leaf with host decode (CSS `hex`, Sheets `cell_ref`,
    /// BBNF `identifier`).
    HRegex,

    /// Rule did not match any detector — falls back to the walker.
    None,
}

impl ShapeTag {
    /// Returns true when this tag represents a W3-actively-classified
    /// shape (Object / Array / String / Number / Keyword / Scalar).
    /// W4 shapes and `None` return false.
    #[inline]
    pub fn is_w3_classified(self) -> bool {
        matches!(
            self,
            ShapeTag::Object
                | ShapeTag::Array
                | ShapeTag::String
                | ShapeTag::Number
                | ShapeTag::Keyword
                | ShapeTag::Scalar
        )
    }
}

/// Run shape classification over every rule in `ir`.
///
/// Dispatch order (most specific first): Object → Array → String →
/// Number → Keyword → Scalar. W4 detectors (Pratt / Unordered /
/// ArgList / Flat / Wrap / HRegex) are stubs returning `false` until
/// W4 ships; W3 ignores them and lets unmatched rules fall to
/// [`ShapeTag::None`].
///
/// Every detector is a pure projection from existing recognizer-
/// miner outputs committed on `ir` by
/// [`crate::passes::recognizers::mine_recognizers`]; this function
/// must run AFTER that pass completes.
pub fn shape_dispatch(ir: &GrammarIR) -> ShapeAssignments {
    let mut assignments = ShapeAssignments::default();
    for rule in &ir.rules {
        let tag = classify_rule(rule.id, ir);
        if !matches!(tag, ShapeTag::None) {
            assignments.assign(rule.id, tag);
        }
    }
    assignments
}

/// Classify a single rule. First matching detector (by the precedence
/// order Object → Array → String → Number → Keyword → Scalar) wins;
/// rules that match no detector return [`ShapeTag::None`] to route
/// through the walker.
fn classify_rule(rule_id: RuleId, ir: &GrammarIR) -> ShapeTag {
    // Precedence: most specific shape first. Object / Array are
    // compound-shaped and exclude the leaf detectors; String / Number
    // match only on regex leaves; Keyword matches literal-led Alt or
    // single literal; Scalar catches a single typed leaf not picked
    // by the above.
    if object::detect_object(rule_id, ir) {
        return ShapeTag::Object;
    }
    if array::detect_array(rule_id, ir) {
        return ShapeTag::Array;
    }
    if string::detect_string(rule_id, ir) {
        return ShapeTag::String;
    }
    if number::detect_number(rule_id, ir) {
        return ShapeTag::Number;
    }
    if keyword::detect_keyword(rule_id, ir) {
        return ShapeTag::Keyword;
    }
    if scalar::detect_scalar(rule_id, ir) {
        return ShapeTag::Scalar;
    }
    ShapeTag::None
}
