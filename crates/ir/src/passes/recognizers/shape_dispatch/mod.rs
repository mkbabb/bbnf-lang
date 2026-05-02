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
//! Active shapes: **Object**, **Array**, **String**, **Number**,
//! **Keyword**, **Scalar**, **Pratt**, **Unordered**, **ArgList**,
//! **Flat**, **Wrap**, **HRegex**. Rules that match no shape detector
//! stay [`ShapeTag::None`] (transparent / pruned set; the dispatcher
//! does not enter them).
//!
//! # Outputs
//!
//! [`shape_dispatch`] returns a [`ShapeAssignments`] mapping every
//! classified rule to its [`ShapeTag`]. Rules absent from the map are
//! implicitly `ShapeTag::None`.
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
//! - [`pratt::detect_pratt`]
//! - [`unordered::detect_unordered`]
//! - [`arglist::detect_arglist`]
//! - [`flat::detect_flat`]
//! - [`wrap::detect_wrap`]
//! - [`hregex::detect_hregex`]
//!
//! Precedence is by specificity: the more specific shape wins. The
//! dispatch order (Object → Array → String → Number → Keyword →
//! HRegex → Pratt → Unordered → ArgList → Flat → Wrap → Scalar)
//! enforces that compound-shaped detectors match before leaf-like
//! detectors, and that String / Number / Keyword (specific regex
//! classes + literal-Alt bodies) claim their regex / literal leaves
//! ahead of HRegex / Scalar (the fallthrough leaf families).

pub mod alt_dispatch;
pub mod arglist;
pub mod array;
pub mod flat;
pub mod hregex;
pub mod keyword;
pub mod number;
pub mod object;
pub mod pratt;
pub mod scalar;
pub mod string;
pub mod unordered;
pub mod wrap;

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
/// [`get`](Self::get) — the dispatcher does not enter them.
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
        self.per_rule.get(&rule).copied().unwrap_or(ShapeTag::None)
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
/// Active classifications: [`Object`](Self::Object),
/// [`Array`](Self::Array), [`String`](Self::String),
/// [`Number`](Self::Number), [`Keyword`](Self::Keyword),
/// [`Scalar`](Self::Scalar), [`Pratt`](Self::Pratt),
/// [`Unordered`](Self::Unordered), [`ArgList`](Self::ArgList),
/// [`Flat`](Self::Flat), [`Wrap`](Self::Wrap),
/// [`HRegex`](Self::HRegex).
///
/// [`None`](Self::None) marks transparent / pruned rules the
/// dispatcher does not enter.
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

    /// AX.W0a.2.b — generalized `Alt` byte-dispatcher over classified
    /// leaves (Ref-to-classified / Literal / Regex / leaf-only Seq).
    /// Strict superset of Wrap; Wrap wins on the narrow
    /// `Alt(Ref | Regex)` canonical cases (JSON `value`, CSS
    /// `color`). AltDispatch captures the broader pattern covering
    /// CSS `value`, BBNF `type_name`, CSS `keyframeStop`, EBNF
    /// `letter`, etc.
    AltDispatch,

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

    /// Returns true when this tag represents a W4-actively-classified
    /// shape (Pratt / Unordered / ArgList / Flat / Wrap / HRegex /
    /// AltDispatch). W3 shapes and `None` return false.
    #[inline]
    pub fn is_w4_classified(self) -> bool {
        matches!(
            self,
            ShapeTag::Pratt
                | ShapeTag::Unordered
                | ShapeTag::ArgList
                | ShapeTag::Flat
                | ShapeTag::Wrap
                | ShapeTag::HRegex
                | ShapeTag::AltDispatch
        )
    }

    /// Returns true when this tag represents ANY actively-classified
    /// shape (W3 + W4). `None` is the only excluded tag.
    #[inline]
    pub fn is_classified(self) -> bool {
        self.is_w3_classified() || self.is_w4_classified()
    }
}

/// Run shape classification over every rule in `ir`.
///
/// Dispatch order: Object → Array → String → Number → Keyword →
/// HRegex → Pratt → Unordered → ArgList → Flat → Wrap → Scalar. Every
/// detector is a pure projection from existing recognizer-miner
/// outputs committed on `ir` by
/// [`crate::passes::recognizers::mine_recognizers`]; this function
/// must run AFTER that pass completes.
pub fn shape_dispatch(ir: &mut GrammarIR) -> ShapeAssignments {
    // AX.W0a.2.b — fixed-point classification. The AltDispatch
    // detector depends on its branch targets already carrying a
    // classified shape tag; when the grammar declares rules
    // top-down (value defined before calcFunction, etc.), a single-
    // pass walk reads stale `ir.shape_assignments`. Iterate until no
    // new rule changes classification (Changed-bool convergence per
    // the LLVM-style fixed-point discipline).
    //
    // Each iteration commits the running classifications back to
    // `ir.shape_assignments` so downstream detectors observe the
    // latest state. The monotone admission invariant (a rule once
    // classified never regresses in the same grammar, because
    // predicates are positive-monotone over the assignments state)
    // guarantees convergence in ≤ N passes.
    let mut iterations = 0;
    loop {
        iterations += 1;
        if iterations > ir.rules.len() + 2 {
            // Defensive cap — convergence must happen in ≤ N passes
            // for any finite grammar. Break to avoid runaway.
            break ir.shape_assignments.clone();
        }
        let mut next = ShapeAssignments::default();
        for rule in &ir.rules {
            let tag = classify_rule(rule.id, ir);
            if !matches!(tag, ShapeTag::None) {
                next.assign(rule.id, tag);
            }
        }
        let stable = next.per_rule == ir.shape_assignments.per_rule;
        ir.shape_assignments = next;
        if stable {
            break ir.shape_assignments.clone();
        }
    }
}

/// Classify a single rule. First matching detector (by the precedence
/// order Object → Array → String → Number → Keyword → HRegex → Pratt
/// → Unordered → ArgList → Flat → Wrap → Scalar) wins; rules that
/// match no detector return [`ShapeTag::None`] to route through the
/// walker.
///
/// # Ordering rationale
///
/// - **Compound-closed shapes first** (Object / Array) — they alone
///   claim their Wrap+Repeat-of-pair / Wrap+Repeat-of-value bodies.
/// - **Typed leaves** (String / Number) — regex bodies classified
///   via `regex_info`; they must run before HRegex (the regex-leaf
///   fallthrough) so a JSON `string` / `number` doesn't land on
///   HRegex by accident.
/// - **Keyword** — literal-led Alt bodies and single literals; must
///   run before Wrap (a pure Alt-of-Ref dispatcher) and ArgList (a
///   literal-led Seq).
/// - **HRegex** — regex leaves the String / Number classifiers
///   rejected (Identifier / HexDigits / PrefixThenClass /
///   CharClassQuantified / Unknown). Runs after String / Number.
/// - **Pratt** — operator-chain Seq bodies. Must run before Flat
///   because Pratt-shape is a typed Seq with specific recursion
///   structure; Flat would claim it otherwise.
/// - **Unordered** — Repeat-over-disjoint-FIRST-Alt. Runs before
///   Flat because a Flat-Seq might contain a Repeat-over-Alt at its
///   tail, but the head-literal requirement of Flat excludes
///   Repeat-rooted bodies.
/// - **ArgList** — `name(args)` function-call Seq. Runs before Flat
///   because its specific parenthesised-args shape is a subset of
///   generic Flat Seqs.
/// - **Flat** — literal-led typed Seq (after compound / Pratt /
///   Unordered / ArgList claim theirs). Runs before Wrap.
/// - **Wrap** — transparent Alt-of-Ref dispatcher. Runs last among
///   compound shapes so Keyword (literal-led Alt) claims its Alts
///   first.
/// - **Scalar** — single-Literal body fallthrough. Runs last so the
///   Keyword single-literal path wins on named keywords.
fn classify_rule(rule_id: RuleId, ir: &GrammarIR) -> ShapeTag {
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
    if hregex::detect_hregex(rule_id, ir) {
        return ShapeTag::HRegex;
    }
    if pratt::detect_pratt(rule_id, ir) {
        return ShapeTag::Pratt;
    }
    if unordered::detect_unordered(rule_id, ir) {
        return ShapeTag::Unordered;
    }
    if arglist::detect_arglist(rule_id, ir) {
        return ShapeTag::ArgList;
    }
    if wrap::detect_wrap(rule_id, ir) {
        return ShapeTag::Wrap;
    }
    // AX.W0a.2.b — AltDispatch generalizes Wrap to mixed
    // Ref / Literal / Regex / leaf-Seq branches. Runs AFTER Wrap so
    // the narrow `Alt(Ref | Regex)` canonical cases (JSON `value`,
    // CSS `color`) keep their specialized emitter.
    if alt_dispatch::detect_alt_dispatch(rule_id, ir) {
        return ShapeTag::AltDispatch;
    }
    if flat::detect_flat(rule_id, ir) {
        return ShapeTag::Flat;
    }
    if scalar::detect_scalar(rule_id, ir) {
        return ShapeTag::Scalar;
    }
    ShapeTag::None
}
