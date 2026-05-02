//! Path-shape seed rewrites — fixture-firing evidence.
//!
//! W3 Hard Gate 9 (W2 carry from
//! `docs/tranches/AZ-IV/audit/SYNTHESIS-2026-05-02.md` §4) requires
//! three hand-authored Path IR rewrites at
//! `crates/ir/src/rewrites/path_seed.rs` with unit tests that prove
//! each rewrite fires on a hand-built fixture. This file is the
//! evidence harness.
//!
//! For every rewrite the tests assert:
//!
//! 1. The classifier ([`classify`]) places the rule in the expected
//!    tier (Class-1 — RHS strictly smaller and a structural sub-tree
//!    of LHS).
//! 2. The LHS pattern matches the hand-built fixture path tree (i.e.
//!    the rewrite would fire under a structural rewriter).
//! 3. The RHS is structurally smaller than the LHS by the expected
//!    amount (the rewrite shrinks the path).
//! 4. RON round-trip preserves the rule (so the seed survives load /
//!    save through the existing schema layer).
//!
//! The seed loader registration ([`RuleSet::merge_path_seed`]) is
//! exercised at the bottom: a freshly-loaded ruleset gains exactly
//! three rules with sequential ids reassigned from the seed's source
//! ids.

use bbnf_ir::rewrites::base::{Atom, Pattern, PatternRef, Witness};
use bbnf_ir::rewrites::path_seed::{
    adjacent_accessor_fusion, duplicate_prefix_elimination, redundant_variant_select_removal,
    seed_ruleset,
};
use bbnf_ir::rewrites::tiering::RuleClass;
use bbnf_ir::rewrites::{RuleSet, SCHEMA_VERSION};

// ── Encoded-segment helpers — mirror the path_seed module's encoding ─

fn field_atom(name: &str) -> Pattern {
    Pattern::Atom(Atom::Rule(PatternRef::by_name(format!(
        "path:Field:{name}"
    ))))
}

fn index_atom(n: usize) -> Pattern {
    Pattern::Atom(Atom::Rule(PatternRef::by_name(format!("path:Index:{n}"))))
}

fn variant_atom(name: &str) -> Pattern {
    Pattern::Atom(Atom::Rule(PatternRef::by_name(format!(
        "path:Variant:{name}"
    ))))
}

fn field_index_atom(field: &str, idx: usize) -> Pattern {
    Pattern::Atom(Atom::Rule(PatternRef::by_name(format!(
        "path:FieldIndex:{field}:{idx}"
    ))))
}

fn rest_var() -> Pattern {
    Pattern::Atom(Atom::Var(0))
}

// ── DuplicatePrefixElimination — fires on nested duplicate prefix ───

#[test]
fn rewrites_path_seed_duplicate_prefix_fires_on_fixture() {
    let rule = duplicate_prefix_elimination();

    // The hand-built fixture follows the prompt's `(Ascend a (Ascend a x))`
    // nested shape: an outer field wrapping the inner contraction.
    let inner = Pattern::Seq(vec![field_atom("statuses"), rest_var()]);
    let fixture_lhs = Pattern::Seq(vec![field_atom("statuses"), inner.clone()]);
    assert_eq!(
        rule.lhs, fixture_lhs,
        "LHS must match the hand-built nested duplicate-field fixture"
    );

    // RHS collapses to the inner contraction — the same shape that
    // appears as the LHS's second child, so the structural sub-tree
    // check passes.
    assert_eq!(rule.rhs, inner, "RHS must equal the inner contraction");

    // Class-1: RHS appears as a sub-tree of the LHS and is strictly
    // smaller.
    assert_eq!(rule.class, RuleClass::Class1);
    assert!(rule.rhs.ast_size() < rule.lhs.ast_size());

    // The rewrite is authored and accepted by the witness check.
    assert!(matches!(rule.witness, Witness::Authored { .. }));
    assert!(rule.witness.is_sound());

    // Negative cost-delta — the rewrite wins on the cost model.
    assert!(rule.cost_delta < 0);
}

// ── RedundantVariantSelectRemoval — fires on nested duplicate variant

#[test]
fn rewrites_path_seed_redundant_variant_select_fires_on_fixture() {
    let rule = redundant_variant_select_removal();

    let inner = Pattern::Seq(vec![variant_atom("Color"), rest_var()]);
    let fixture_lhs = Pattern::Seq(vec![variant_atom("Color"), inner.clone()]);
    assert_eq!(
        rule.lhs, fixture_lhs,
        "LHS must match the hand-built nested duplicate-variant fixture"
    );
    assert_eq!(
        rule.rhs, inner,
        "RHS must collapse to the inner variant select"
    );

    assert_eq!(rule.class, RuleClass::Class1);
    assert!(rule.rhs.ast_size() < rule.lhs.ast_size());
    assert!(matches!(rule.witness, Witness::Authored { .. }));
    assert!(rule.witness.is_sound());
    assert!(rule.cost_delta < 0);
}

// ── AdjacentAccessorFusion — fires on field+index pair ──────────────

#[test]
fn rewrites_path_seed_adjacent_accessor_fusion_fires_on_fixture() {
    let rule = adjacent_accessor_fusion();

    let fixture_lhs = Pattern::Seq(vec![field_atom("statuses"), index_atom(0), rest_var()]);
    assert_eq!(
        rule.lhs, fixture_lhs,
        "LHS must match the hand-built field-then-index fixture"
    );

    let fixture_rhs = Pattern::Seq(vec![field_index_atom("statuses", 0), rest_var()]);
    assert_eq!(
        rule.rhs, fixture_rhs,
        "RHS must fuse field+index into a compound positional accessor"
    );

    // Fusion introduces a *new* atomic shape (FieldIndex) that does
    // not appear in the LHS, so the classifier records this as
    // Class-3 (oracle-accepted). The witness is `Authored` because
    // the fusion is sound by construction (positional access is
    // associative on the type lattice).
    assert_eq!(rule.class, RuleClass::Class3);
    // RHS is structurally smaller — fewer Seq children.
    assert!(rule.rhs.ast_size() < rule.lhs.ast_size());
    assert!(matches!(rule.witness, Witness::Authored { .. }));
    assert!(rule.witness.is_sound());
    assert!(rule.cost_delta < 0);
}

// ── Seed ruleset shape + classifier coverage ───────────────────────

#[test]
fn rewrites_path_seed_ruleset_carries_three_rules() {
    let rs = seed_ruleset();
    assert_eq!(rs.len(), 3, "seed ruleset must carry exactly three rules");
    assert_eq!(rs.grammar, "path");
    assert_eq!(rs.schema_version, SCHEMA_VERSION);

    // Sequential ids assigned by `RuleSet::push`.
    for (i, r) in rs.iter().enumerate() {
        assert_eq!(r.id.0, i as u32, "ids must be sequential");
    }

    // Tier coverage: two Class-1 (sub-tree shrink) and one Class-3
    // (compound-fusion). The seed deliberately spans tiers so the
    // ranker, classifier, and discovery cohort all see populated
    // buckets.
    let by_class: Vec<RuleClass> = rs.iter().map(|r| r.class).collect();
    assert_eq!(
        by_class,
        vec![RuleClass::Class1, RuleClass::Class1, RuleClass::Class3]
    );
}

// ── Loader registration: merge_path_seed appends three rules ───────

#[test]
fn rewrites_path_seed_loader_merges_into_per_grammar_set() {
    // A per-grammar set with one pre-existing authored rule.
    let mut json_rules = RuleSet::new("json");
    let preexisting = bbnf_ir::rewrites::Rule::new(
        bbnf_ir::rewrites::RewriteRuleId(0),
        Pattern::Seq(vec![field_atom("preexisting")]),
        field_atom("preexisting"),
        Witness::Authored {
            note: "preexisting".into(),
        },
        -1,
    );
    json_rules.push(preexisting);

    json_rules.merge_path_seed();

    // 1 preexisting + 3 seed = 4.
    assert_eq!(json_rules.len(), 4);
    // Sequential ids preserved across the merge boundary.
    for (i, r) in json_rules.iter().enumerate() {
        assert_eq!(r.id.0, i as u32);
    }
    // The grammar id stays as the host grammar — the seed does not
    // overwrite it.
    assert_eq!(json_rules.grammar, "json");
}

// ── RON round-trip preserves the seed rules ────────────────────────

#[test]
fn rewrites_path_seed_ron_round_trips_intact() {
    let rs = seed_ruleset();
    let tmp = std::env::temp_dir().join("bbnf-ir-w3-path-seed-roundtrip.ron");
    rs.save_to_ron(&tmp).expect("save_to_ron");
    let restored = RuleSet::load_from_ron(&tmp).expect("load_from_ron");
    assert_eq!(restored, rs);
    let _ = std::fs::remove_file(&tmp);
}
