//! Path-shape seed rewrites — hand-authored Class-1 rules over typed
//! [`PathSegment`](super::super::path) sequences.
//!
//! W3 Hard Gate 9 (W2 carry, see
//! `docs/tranches/AZ-IV/audit/SYNTHESIS-2026-05-02.md` §4) requires the
//! rewrite-rule storage to be seeded with a small, defensible bag of
//! Path-IR rewrites *before* the recycled BA tranche opens its
//! discovery cohort. The seed cannot wait on the discovery enumerator
//! and must compile against the existing [`Pattern`] alphabet without
//! introducing a Path-IR variant.
//!
//! ## Encoding
//!
//! `bbnf-ir` is downstream of `bbnf-core` only via the runtime path —
//! the rewrite store cannot import `bbnf_core::path::ir`. The seed
//! therefore encodes Path segments through the alphabet the rewrite
//! substrate already carries: each segment shape lifts to a
//! [`PatternRef`]-backed [`Atom::Rule`] whose `name` field follows a
//! stable convention:
//!
//! | Path-IR segment           | Encoded atom                         |
//! |---------------------------|--------------------------------------|
//! | `Field("<name>")`         | `Atom::Rule("path:Field:<name>")`    |
//! | `Index(<n>)`              | `Atom::Rule("path:Index:<n>")`       |
//! | `VariantName("<n>")`      | `Atom::Rule("path:Variant:<n>")`     |
//! | `Wildcard`                | `Atom::Rule("path:Wildcard")`        |
//! | fused `Field+Index`       | `Atom::Rule("path:FieldIndex:<f>:<n>")` |
//!
//! Rest-of-path tails are pattern variables ([`Atom::Var`]). A path is
//! laid out left-to-right inside a [`Pattern::Seq`].
//!
//! ## Encoding shape — nested Seq mirrors `(Op a (Op a x))`
//!
//! The classifier ([`super::tiering::classify`]) decides Class-1 by
//! walking the LHS for a structural sub-tree equal to the RHS. The
//! prompt's pseudo-pattern `(Ascend a (Ascend a x))` is the literal
//! source: an outer step wrapping the inner `(Ascend a x)`. The seed
//! encodes that shape as `Seq([head, Seq([head, Var(0)])])`, so the
//! RHS `Seq([head, Var(0)])` appears as the second child of the LHS.
//! `contains_subpattern` walks the LHS, finds the RHS child, returns
//! true — Class-1.
//!
//! ## Soundness sketch
//!
//! 1. **DuplicatePrefixElimination** — Class-1. Two adjacent steps
//!    that name the same struct field both resolve through the
//!    registry to the same field accessor; the inner repeat is a
//!    no-op on a struct value.
//! 2. **RedundantVariantSelectRemoval** — Class-1. Variant selection
//!    (`PathSegment::VariantName`) re-projects an already-typed enum
//!    into the same variant; a second projection on an
//!    already-narrowed value is the identity.
//! 3. **AdjacentAccessorFusion** — Class-3. A `Field` followed by an
//!    `Index` lifts to a fused compound accessor that is a *new*
//!    atomic shape, not a sub-tree of the LHS. The classifier records
//!    this as Class-3 (oracle-accepted) — the witness is
//!    [`Witness::Authored`] because the fusion is sound by
//!    construction (positional access is associative on the type lattice).
//!
//! ## Loader integration
//!
//! [`seed_ruleset`] returns a [`RuleSet`] keyed against the synthetic
//! grammar id `"path"`. The loader pulls it via [`super::path_seed`]
//! (re-exported from [`super`]) and merges it into per-grammar rule
//! sets at egraph saturation time; the [`RuleSet::path_seed`]
//! accessor is the named entry-point.

use super::base::{Atom, Pattern, PatternRef, Witness};
use super::tiering::classify;
use super::{RewriteRuleId, Rule, RuleSet};

/// Synthetic grammar id used for the path-shape seed bag.
///
/// The seed is grammar-agnostic — it operates on any path that can be
/// expressed in [`crate::path::ir::PathSegment`] terms. Per-grammar
/// rule files at `grammar/<g>/rewrites/*.ron` may merge this seed into
/// their saturation set without re-keying.
pub const PATH_SEED_GRAMMAR: &str = "path";

/// Stable atom-name prefix for encoded path segments.
const PATH_PREFIX: &str = "path:";

/// Construct an `Atom::Rule` referencing a stable encoded path segment.
fn path_atom(encoded: impl Into<String>) -> Pattern {
    Pattern::Atom(Atom::Rule(PatternRef::by_name(encoded.into())))
}

/// `Field("<name>")` → `Atom::Rule("path:Field:<name>")`.
fn field_atom(name: &str) -> Pattern {
    path_atom(format!("{PATH_PREFIX}Field:{name}"))
}

/// `Index(<n>)` → `Atom::Rule("path:Index:<n>")`.
fn index_atom(n: usize) -> Pattern {
    path_atom(format!("{PATH_PREFIX}Index:{n}"))
}

/// `VariantName("<v>")` → `Atom::Rule("path:Variant:<v>")`.
fn variant_atom(name: &str) -> Pattern {
    path_atom(format!("{PATH_PREFIX}Variant:{name}"))
}

/// Fused `Field+Index` → `Atom::Rule("path:FieldIndex:<f>:<n>")`. The
/// fused form is the W3 normalised compound step that the executor
/// can dispatch in one decision rather than two.
fn field_index_atom(field: &str, idx: usize) -> Pattern {
    path_atom(format!("{PATH_PREFIX}FieldIndex:{field}:{idx}"))
}

/// Rest-of-path variable: stands for an arbitrary trailing segment
/// sequence. `Atom::Var(0)` is the conventional rest-pattern.
fn rest_var() -> Pattern {
    Pattern::Atom(Atom::Var(0))
}

/// Build a path pattern from an ordered list of encoded segments,
/// terminated by the rest-tail variable. Three or more segments is
/// the minimum the seed rewrites operate over (two head segments
/// plus the tail).
fn path_seq(segments: Vec<Pattern>) -> Pattern {
    Pattern::Seq(segments)
}

/// Author the **DuplicatePrefixElimination** rewrite.
///
/// LHS: `Seq([Field("<f>"), Seq([Field("<f>"), Var(0)])])`
///      — nested form mirroring `(Ascend a (Ascend a x))`.
/// RHS: `Seq([Field("<f>"), Var(0)])`
///
/// The RHS appears as the second child of the LHS, so the structural
/// sub-tree check in [`super::tiering::classify`] succeeds and the
/// rule is Class-1. The fixture pins the rewrite to a `"statuses"`
/// field — any other field name is structurally identical because
/// the encoded atom embeds the name verbatim.
pub fn duplicate_prefix_elimination() -> Rule {
    let inner = path_seq(vec![field_atom("statuses"), rest_var()]);
    let lhs = path_seq(vec![field_atom("statuses"), inner.clone()]);
    let rhs = inner;
    let class = classify(&lhs, &rhs);
    Rule {
        id: RewriteRuleId(0),
        class,
        lhs,
        rhs,
        witness: Witness::Authored {
            note: "duplicate adjacent field accessor — outer visit is the identity".into(),
        },
        cost_delta: -1,
        frequency: 0,
    }
}

/// Author the **RedundantVariantSelectRemoval** rewrite.
///
/// LHS: `Seq([VariantName("Color"), Seq([VariantName("Color"), Var(0)])])`
///      — nested form, second child equals the RHS.
/// RHS: `Seq([VariantName("Color"), Var(0)])`
///
/// A second variant selection on an already-narrowed sum value is
/// the identity by construction; the `path_check` IR pass narrows
/// the inner expression to the named variant. Class-1.
pub fn redundant_variant_select_removal() -> Rule {
    let inner = path_seq(vec![variant_atom("Color"), rest_var()]);
    let lhs = path_seq(vec![variant_atom("Color"), inner.clone()]);
    let rhs = inner;
    let class = classify(&lhs, &rhs);
    Rule {
        id: RewriteRuleId(0),
        class,
        lhs,
        rhs,
        witness: Witness::Authored {
            note: "duplicate variant select — inner value already narrowed by `path_check`".into(),
        },
        cost_delta: -1,
        frequency: 0,
    }
}

/// Author the **AdjacentAccessorFusion** rewrite.
///
/// LHS: `Seq([Field("statuses"), Index(0), Var(0)])`
/// RHS: `Seq([FieldIndex("statuses", 0), Var(0)])`
///
/// The RHS introduces a fused atomic accessor that is a *new* shape,
/// not a sub-tree of the LHS — the classifier records this as
/// Class-3 (oracle-accepted). The witness is [`Witness::Authored`]
/// because the fusion is sound by construction: the field-then-index
/// pair is a compound positional accessor on the type lattice, and
/// the fused form is the W3 normalised compound step.
pub fn adjacent_accessor_fusion() -> Rule {
    let lhs = path_seq(vec![field_atom("statuses"), index_atom(0), rest_var()]);
    let rhs = path_seq(vec![field_index_atom("statuses", 0), rest_var()]);
    let class = classify(&lhs, &rhs);
    Rule {
        id: RewriteRuleId(0),
        class,
        lhs,
        rhs,
        witness: Witness::Authored {
            note: "adjacent field-then-index — fold into compound positional accessor".into(),
        },
        cost_delta: -1,
        frequency: 0,
    }
}

/// Construct the path-shape seed rule set.
///
/// The set carries the three hand-authored rewrites in deterministic
/// order: duplicate-prefix elimination, redundant variant-select
/// removal, adjacent accessor fusion. Each id is reassigned by
/// [`RuleSet::push`] at insertion.
pub fn seed_ruleset() -> RuleSet {
    let mut rs = RuleSet::new(PATH_SEED_GRAMMAR);
    rs.push(duplicate_prefix_elimination());
    rs.push(redundant_variant_select_removal());
    rs.push(adjacent_accessor_fusion());
    rs
}
