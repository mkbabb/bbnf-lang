//! `compute_push_fingerprint` — derives the per-grammar push-site
//! fingerprint used for capacity-closure tuning and downstream cost-
//! model decisions.
//!
//! Each emitted rule function in a tape-first backend resolves to
//! exactly one of three push shapes via its [`MaterializationClass`]
//! plus payload layout:
//!
//! - **`push_compound`** — `MaterializationClass::MustTape`. Every
//!   tape emission for the rule writes a compound header; the rule
//!   owns its children.
//! - **`push_leaf`** — `MaterializationClass::TapeSpanOnly` without
//!   a payload. A single span record, no children, no aggregate
//!   buffer.
//! - **`push_leaf_with_*`** — `TapeSpanOnly` with a scalar payload
//!   type or an aggregate `payload_layouts` entry. Emits a leaf
//!   record plus a scalar / aggregate payload slot.
//!
//! `TransparentElide` rules emit no function and contribute zero
//! push sites — their bodies are inlined at every call site, where
//! the caller's own classification decides the push shape.
//!
//! `preserve_identity` rules force `MustTape` regardless of class
//! lookup, mirroring the emitter's
//! `materialization_for_rule` precedence.
//!
//! The fingerprint counts the rule-level epilogue site for every
//! emitted rule. Per-Repeat compounds and per-Alt-branch surgery
//! sites are not counted: they are conditional on per-call dispatch
//! (only one Alt branch fires per parse-tree node, only Repeat-
//! actively-iterated bodies push) and would otherwise inflate the
//! denominator with sites that fire less than once per byte. The
//! per-rule view is what calibrates the capacity divisor.
//!
//! ## Data flow
//!
//! Runs after `classify_materialization` + `compute_payload_layouts`
//! so both `ir.materialization` and `ir.payload_layouts` are
//! populated. Stored on `GrammarIR.push_fingerprint` as an
//! `Option<PushFingerprint>`; callers treat `None` as "fall back to
//! the default divisor" (the pass never produces `None` today but
//! the optional shape matches the structural_alphabet precedent and
//! leaves room for empty-grammar edge cases).

use serde::{Deserialize, Serialize};

use crate::passes::materialization::MaterializationClass;
use crate::{GrammarIR, IrRule};

/// Static push-site counts over every emitted rule function in the
/// grammar.
///
/// Produced by [`compute_push_fingerprint`]. Consumed at codegen time
/// by the Rust emitter's `parse()` entry point to pick a grammar-
/// specific `Tape<R>::with_capacity` divisor (`AU.6.2`). The same
/// numbers are used as seed inputs for any future cost-model decision
/// that keys on grammar density.
#[derive(Serialize, Deserialize, Clone, Copy, Debug, Default)]
pub struct PushFingerprint {
    /// Count of rules whose emitted function calls
    /// `Tape<R>::push_compound(...)` at its epilogue. Every
    /// `MaterializationClass::MustTape` rule contributes one push
    /// site; `TransparentElide` rules contribute zero.
    pub compound_pushes: u32,

    /// Count of rules whose emitted function calls
    /// `Tape<R>::push_leaf(...)` without a payload. Rules with a
    /// scalar or aggregate payload layout do NOT count here — they
    /// count as `leaf_with_pushes`.
    pub leaf_pushes: u32,

    /// Count of rules whose emitted function calls a
    /// `Tape<R>::push_leaf_with_*` variant (scalar, Span, or
    /// aggregate). The rule is `TapeSpanOnly` AND has either an
    /// aggregate `payload_layout` entry or carries a scalar payload
    /// type through its body.
    pub leaf_with_pushes: u32,
}

impl PushFingerprint {
    /// Total push-site count (compound + leaf + leaf_with).
    #[inline]
    pub fn total(&self) -> u32 {
        self.compound_pushes + self.leaf_pushes + self.leaf_with_pushes
    }

    /// Compound-push ratio, scaled to 0..=100 (integer percent).
    /// Returns `0` when the grammar has zero emitted push sites — a
    /// degenerate shape (every rule `TransparentElide`) that cannot
    /// reach a real parse.
    #[inline]
    pub fn compound_ratio_pct(&self) -> u32 {
        let total = self.total();
        if total == 0 {
            return 0;
        }
        (self.compound_pushes * 100) / total
    }

    /// Derive the `(numer, denom)` capacity ratio for
    /// `Tape<R>::with_capacity(input.len() * numer / denom + 2)`.
    ///
    /// The ratio is chosen to avoid the `RawVec::grow_one` /
    /// `_mi_heap_realloc_zero` path on first-parse: over-allocating
    /// is cheap (wasted bytes in the final tape), while under-
    /// allocating triggers a realloc that is 10–22% of `parse_simple`
    /// Sheets samples per the AU Wave-2 profile.
    ///
    /// The four tiers cover the AU codegen-fingerprint table.
    /// Decision order: JSON-like wins on either low compound ratio
    /// OR small grammar size, CSS-like wins on large grammar size,
    /// Sheets-like wins on near-100% compound ratio, BBNF-like is
    /// the residual:
    ///
    /// | grammar family | ratio_pct | total | numer / denom | density |
    /// |----------------|----------:|------:|---------------|--------:|
    /// | JSON-like      | ≤ 70 OR total < 25 | any | 1 / 2 | 0.5 |
    /// | CSS-like       | > 70      | ≥ 150 | 1 / 1         | 1.0     |
    /// | Sheets-like    | ≥ 95      | 25–149 | 1 / 1        | 1.0     |
    /// | BBNF-like      | 70–95     | 25–149 | 5 / 8        | 0.625   |
    ///
    /// The total-rule threshold of 25 distinguishes JSON's small
    /// grammar (10 push sites, all `MustTape`) from Sheets's
    /// medium grammar (37 push sites, all `MustTape`). Both are
    /// 100% compound by per-rule classification but produce
    /// different per-byte densities at runtime — JSON's input is
    /// dominated by leaf scalars consumed inside Alt-surgery
    /// branches, Sheets's input creates a fresh compound per
    /// formula token.
    ///
    /// The 150-rule CSS-like override is preserved as a separate
    /// dispatch arm so future cost-model tuning can pick a
    /// different ratio for deeply-nested grammars (the original
    /// 3/2 over-allocation matched CSS bootstrap's raw record
    /// count but traded cache locality for a small regression).
    /// Today both CSS-like and Sheets-like resolve to (1, 1).
    ///
    /// The thresholds are tunable; future cost-model work can
    /// replace the bucketing with a continuous fit once a wider
    /// per-grammar profile corpus is in hand.
    pub fn capacity_ratio(&self) -> (u32, u32) {
        let total = self.total();
        // Degenerate shape: fall back to the historic conservative
        // JSON-calibrated divisor. `input.len() / 2 + 2`.
        if total == 0 {
            return (1, 2);
        }
        let ratio_pct = self.compound_ratio_pct();
        if ratio_pct <= 70 || total < 25 {
            // JSON-like: scalar-heavy AND/OR small grammar. ~0.5
            // records per input byte.
            (1, 2)
        } else if total >= 150 {
            // CSS-like: deep grammar, 150+ rule push sites. Every
            // declaration / value / selector produces many tape
            // records per input byte. ~1.0 records/byte for the
            // typed selectors / declarations / value-unit chain.
            // The 3/2 over-allocation that matched CSS bootstrap's
            // raw record count traded cache locality for a
            // non-trivial regression — staying at 1.0 keeps the
            // initial reserve generous without flooding L1.
            (1, 1)
        } else if ratio_pct >= 95 {
            // Sheets-like: effectively every rule is `MustTape`.
            // A formula averages ≈ 1 tape record per input byte.
            (1, 1)
        } else {
            // BBNF-like: high compound ratio, moderate depth.
            // ~0.625 records/byte on `json.bbnf`-shape inputs.
            (5, 8)
        }
    }
}

/// Walk every rule in the grammar, classify its emitted push site,
/// and store the aggregated fingerprint on `ir.push_fingerprint`.
///
/// Must run after `classify_materialization` and
/// `compute_payload_layouts` — consults both side tables. Safe to
/// run before or after any body-mutating pass that preserves the
/// rule-to-materialization mapping.
pub fn compute_push_fingerprint(ir: &mut GrammarIR) {
    let mut compound_pushes: u32 = 0;
    let mut leaf_pushes: u32 = 0;
    let mut leaf_with_pushes: u32 = 0;

    for rule in &ir.rules {
        match classify_push(ir, rule) {
            PushShape::None => {}
            PushShape::Compound => compound_pushes += 1,
            PushShape::Leaf => leaf_pushes += 1,
            PushShape::LeafWith => leaf_with_pushes += 1,
        }
    }

    ir.push_fingerprint = Some(PushFingerprint {
        compound_pushes,
        leaf_pushes,
        leaf_with_pushes,
    });
}

/// The four push shapes a rule can emit. `None` covers
/// `TransparentElide` rules whose body is inlined at every call site
/// — their push shape is the caller's responsibility, not their own.
enum PushShape {
    None,
    Compound,
    Leaf,
    LeafWith,
}

/// Decide which push shape a rule's emitted function will use.
///
/// Mirrors the dispatch in
/// `crates/core/src/backend/rust/emitter/grammar.rs` — every
/// `preserve_identity` rule forces `MustTape`; otherwise we read
/// `ir.materialization` via the rule's body `NodeId`. A rule with a
/// `payload_layouts` entry OR carrying a scalar payload type
/// classifies as `LeafWith` regardless of the raw `TapeSpanOnly`
/// class.
fn classify_push(ir: &GrammarIR, rule: &IrRule) -> PushShape {
    // `preserve_identity` rules always push a compound — mirror of
    // `materialization_for_rule` in the Rust emitter.
    if rule.meta.preserve_identity {
        return PushShape::Compound;
    }
    let class = ir
        .dag
        .as_ref()
        .and_then(|dag| dag.node_for(&rule.body))
        .and_then(|nid| ir.materialization.get(&nid).copied())
        .unwrap_or(MaterializationClass::MustTape);
    match class {
        MaterializationClass::TransparentElide => PushShape::None,
        MaterializationClass::MustTape => PushShape::Compound,
        MaterializationClass::TapeSpanOnly => {
            if ir.payload_layouts.contains_key(&rule.id) {
                return PushShape::LeafWith;
            }
            if rule_has_scalar_payload(ir, rule) {
                return PushShape::LeafWith;
            }
            PushShape::Leaf
        }
    }
}

/// True iff the rule's projected `TypeDesc` is a scalar that maps to
/// a `push_leaf_with_<T>` call. Matches the emitter's gate in
/// `emit_tape_span_only_scalar_epilogue`.
fn rule_has_scalar_payload(ir: &GrammarIR, rule: &IrRule) -> bool {
    ir.types
        .iter()
        .find_map(|(rid, td)| (*rid == rule.id).then_some(td))
        .is_some_and(|td| td.is_scalar_payload())
}

// Tests live in tests/passes/push_fingerprint.rs (crate-level).
