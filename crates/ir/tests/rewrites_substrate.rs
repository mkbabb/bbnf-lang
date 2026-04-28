//! BB.scaffold.B substrate-level tests.
//!
//! Verifies the five hard gates the BB.scaffold.B job statement
//! enumerates:
//!
//! - Gate 4: RON round-trip on a fixture rule.
//! - Gate 5: ranker known-input → known-ranking on a 5-rule fixture.
//!
//! Plus tier-classifier coverage of all three tiers, ranker
//! axis-disable behaviour, base-type primitives (`ast_size`,
//! `Witness::is_sound`), and schema-version rejection to keep the
//! surface honest under future cost-config integration.

use bbnf_ir::rewrites::base::{Alphabet, Atom, Pattern, Witness};
use bbnf_ir::rewrites::rank::{RankConfig, rank, select_top_k};
use bbnf_ir::rewrites::schema::{RuleFile, RuleSerialized};
use bbnf_ir::rewrites::tiering::{RuleClass, classify};
use bbnf_ir::rewrites::{RewriteRuleId, Rule, RuleSet};

fn lit(c: u8) -> Pattern {
    Pattern::Atom(Atom::Term(Alphabet::Byte(c)))
}

fn rule(id: u32, freq: u32, cost: i64, lhs: Pattern, rhs: Pattern) -> Rule {
    let class = classify(&lhs, &rhs);
    Rule {
        id: RewriteRuleId(id),
        class,
        lhs,
        rhs,
        witness: Witness::Authored { note: "fixture".into() },
        cost_delta: cost,
        frequency: freq,
    }
}

// ── Gate 4: RON round-trip on a fixture rule ───────────────────────

#[test]
fn ron_round_trip_fixture_rule() {
    let mut rs = RuleSet::new("json");
    rs.push(rule(
        0,
        50,
        -3,
        Pattern::Seq(vec![lit(b'{'), lit(b'}')]),
        lit(b'{'),
    ));
    rs.push(rule(
        0,
        20,
        -1,
        Pattern::Repeat {
            inner: Box::new(lit(b'a')),
            lo: 0,
            hi: 0,
        },
        Pattern::Atom(Atom::Epsilon),
    ));

    let tmp = std::env::temp_dir().join("bb-scaffold-b-rt.ron");
    rs.save_to_ron(&tmp).expect("save_to_ron");
    let restored = RuleSet::load_from_ron(&tmp).expect("load_from_ron");
    assert_eq!(restored, rs);
    let _ = std::fs::remove_file(&tmp);
}

#[test]
fn ron_load_rejects_wrong_schema_version() {
    use bbnf_ir::rewrites::schema::{RuleFile, SchemaError};
    let bad = RuleFile {
        schema_version: 9999,
        grammar: "json".into(),
        rules: Vec::new(),
    };
    let err = RuleSet::from_file(bad).unwrap_err();
    matches!(err, SchemaError::VersionMismatch { .. });
}

// ── Gate 5: ranker known-input → known-ranking on a 5-rule fixture.

#[test]
fn ranker_five_rule_fixture_ordering() {
    // Five fixture rules with known static-axis values:
    //
    //  id |  freq | cost  | atoms        | expected static rank
    //  ---+-------+-------+--------------+---------------------
    //   0 |  100  |  -10  | {b'a'}       | very high (max freq + max cost-win)
    //   1 |   80  |   -5  | {b'b'}       | high
    //   2 |   50  |   -1  | {b'c'}       | medium
    //   3 |   10  |    0  | {b'd'}       | low
    //   4 |    1  |   +5  | {b'e'}       | very low (positive cost = ignored)
    let mut rules = vec![
        rule(0, 100, -10, lit(b'a'), lit(b'a')),
        rule(1, 80, -5, lit(b'b'), lit(b'b')),
        rule(2, 50, -1, lit(b'c'), lit(b'c')),
        rule(3, 10, 0, lit(b'd'), lit(b'd')),
        rule(4, 1, 5, lit(b'e'), lit(b'e')),
    ];

    rank(&mut rules, &RankConfig::default());

    // Rule 0 must land first.
    assert_eq!(rules[0].id, RewriteRuleId(0));
    // Rule 4 must land last (positive cost ⇒ static cost-axis is 0;
    // freq is at floor; novelty is uniform).
    assert_eq!(rules[4].id, RewriteRuleId(4));
}

#[test]
fn select_top_k_does_not_mutate() {
    let rules = vec![
        rule(0, 100, -10, lit(b'a'), lit(b'a')),
        rule(1, 50, -5, lit(b'b'), lit(b'b')),
        rule(2, 10, -1, lit(b'c'), lit(b'c')),
    ];
    let snapshot: Vec<RewriteRuleId> = rules.iter().map(|r| r.id).collect();
    let top1 = select_top_k(&rules, 1);
    assert_eq!(top1.len(), 1);
    assert_eq!(top1[0].id, RewriteRuleId(0));
    // Unchanged.
    let after: Vec<RewriteRuleId> = rules.iter().map(|r| r.id).collect();
    assert_eq!(snapshot, after);
}

// ── Tier classifier — coverage of all three tiers ───────────────────

#[test]
fn tier_class1_strict_shrink() {
    // Seq([a]) ⇒ a is Class-1.
    let r = rule(0, 0, 0, Pattern::Seq(vec![lit(b'a')]), lit(b'a'));
    assert_eq!(r.class, RuleClass::Class1);
}

#[test]
fn tier_class2_constant_fold() {
    // Repeat{x, 0, 0} ⇒ epsilon — the LHS folds (Repeat with hi=0
    // is vacuous) but RHS is not a sub-tree of LHS, so this is
    // strictly Class-2 (not Class-1).
    let r = rule(
        0,
        0,
        0,
        Pattern::Repeat {
            inner: Box::new(lit(b'a')),
            lo: 0,
            hi: 0,
        },
        Pattern::Atom(Atom::Epsilon),
    );
    assert_eq!(r.class, RuleClass::Class2);
}

#[test]
fn tier_class3_shape_change() {
    // a >> b ⇒ a << b is Class-3.
    let r = rule(
        0,
        0,
        0,
        Pattern::Next(Box::new(lit(b'a')), Box::new(lit(b'b'))),
        Pattern::Skip(Box::new(lit(b'a')), Box::new(lit(b'b'))),
    );
    assert_eq!(r.class, RuleClass::Class3);
}

#[test]
fn ruleset_by_class_partition() {
    // Build a RuleSet that exercises all three tiers. Verify
    // `by_class` returns the right populations.
    let mut rs = RuleSet::new("json");
    rs.push(rule(0, 0, 0, Pattern::Seq(vec![lit(b'x')]), lit(b'x')));
    rs.push(rule(
        0,
        0,
        0,
        Pattern::Repeat {
            inner: Box::new(lit(b'y')),
            lo: 0,
            hi: 0,
        },
        Pattern::Atom(Atom::Epsilon),
    ));
    rs.push(rule(
        0,
        0,
        0,
        Pattern::Next(Box::new(lit(b'a')), Box::new(lit(b'b'))),
        Pattern::Skip(Box::new(lit(b'a')), Box::new(lit(b'b'))),
    ));

    assert_eq!(rs.by_class(RuleClass::Class1).count(), 1);
    assert_eq!(rs.by_class(RuleClass::Class2).count(), 1);
    assert_eq!(rs.by_class(RuleClass::Class3).count(), 1);
}

#[test]
fn empty_ruleset_round_trips() {
    let rs = RuleSet::new("css_l4");
    let tmp = std::env::temp_dir().join("bb-scaffold-b-empty.ron");
    rs.save_to_ron(&tmp).unwrap();
    let restored = RuleSet::load_from_ron(&tmp).unwrap();
    assert_eq!(rs, restored);
    let _ = std::fs::remove_file(&tmp);
}

// ── Base-type primitives ───────────────────────────────────────────

#[test]
fn ast_size_counts_every_node() {
    let p = Pattern::Seq(vec![
        Pattern::Atom(Atom::Term(Alphabet::Byte(b'a'))),
        Pattern::Repeat {
            inner: Box::new(Pattern::Atom(Atom::Var(0))),
            lo: 0,
            hi: 1,
        },
    ]);
    // root Seq + atom + repeat + inner atom = 4
    assert_eq!(p.ast_size(), 4);
}

#[test]
fn atoms_collects_all_leaves() {
    let p = Pattern::Seq(vec![
        Pattern::Atom(Atom::Term(Alphabet::Byte(b'a'))),
        Pattern::Atom(Atom::Var(0)),
    ]);
    assert_eq!(p.atoms().len(), 2);
}

#[test]
fn witness_soundness_authored_is_unconditional() {
    assert!(Witness::Authored { note: "x".into() }.is_sound());
}

#[test]
fn witness_soundness_egraph_requires_seed() {
    assert!(!Witness::EgraphEquiv { seed_count: 0 }.is_sound());
    assert!(Witness::EgraphEquiv { seed_count: 1 }.is_sound());
}

#[test]
fn witness_soundness_oracle_requires_zero_counterexamples() {
    assert!(!Witness::VmOracle { sample_count: 100, counterexamples: 1 }.is_sound());
    assert!(Witness::VmOracle { sample_count: 100, counterexamples: 0 }.is_sound());
    // sample_count == 0 is also unsound (no evidence at all).
    assert!(!Witness::VmOracle { sample_count: 0, counterexamples: 0 }.is_sound());
}

// ── Tier classifier — extended cases ────────────────────────────────

#[test]
fn class1_rejects_equal_size() {
    // a ⇒ a — sub-tree match, but RHS not strictly smaller.
    assert_ne!(classify(&lit(b'a'), &lit(b'a')), RuleClass::Class1);
}

#[test]
fn class1_nested_shrink() {
    // Seq([Repeat[a]]) ⇒ Repeat[a]
    let inner = Pattern::Repeat {
        inner: Box::new(lit(b'a')),
        lo: 0,
        hi: 1,
    };
    let lhs = Pattern::Seq(vec![inner.clone()]);
    let rhs = inner;
    assert_eq!(classify(&lhs, &rhs), RuleClass::Class1);
}

// ── Ranker — extended cases ─────────────────────────────────────────

#[test]
fn ranker_diversifies_via_novelty() {
    // Three rules: two share a literal, one is distinct. With
    // novelty cranked, the distinct one should land position 1
    // because its novelty bonus offsets a slightly worse static
    // score.
    let mut rules = vec![
        rule(0, 100, -10, lit(b'a'), lit(b'a')),
        rule(1, 90, -10, lit(b'a'), lit(b'a')),
        rule(2, 95, -10, lit(b'z'), lit(b'z')),
    ];
    let cfg = RankConfig {
        w_novelty: 5.0,
        ..RankConfig::default()
    };
    rank(&mut rules, &cfg);
    assert_eq!(rules[0].id, RewriteRuleId(0));
    assert_eq!(rules[1].id, RewriteRuleId(2));
}

#[test]
fn select_top_k_zero_returns_empty() {
    let rules = vec![rule(0, 1, 0, lit(b'a'), lit(b'a'))];
    assert!(select_top_k(&rules, 0).is_empty());
}

// ── Schema layer — class is reconstructed at load ─────────────────

#[test]
fn rule_serialized_class_recomputed_from_lhs_rhs() {
    let r = Rule {
        id: RewriteRuleId(7),
        class: RuleClass::Class3, // intentionally wrong; classifier overrides
        lhs: Pattern::Seq(vec![lit(b'a')]),
        rhs: lit(b'a'),
        witness: Witness::Authored { note: "shrink".into() },
        cost_delta: -1,
        frequency: 42,
    };
    let s = RuleSerialized::from_rule(&r);
    let back = s.into_rule(RewriteRuleId(7));
    // Classifier sees Seq([a]) ⇒ a as Class-1.
    assert_eq!(back.class, RuleClass::Class1);
    // Everything else round-trips.
    assert_eq!(back.lhs, r.lhs);
    assert_eq!(back.rhs, r.rhs);
    assert_eq!(back.witness, r.witness);
    assert_eq!(back.cost_delta, r.cost_delta);
    assert_eq!(back.frequency, r.frequency);
}

#[test]
fn empty_rulefile_serializes_to_ron() {
    let f = RuleFile::empty("json");
    let s = ron::ser::to_string_pretty(&f, ron::ser::PrettyConfig::default()).unwrap();
    let back: RuleFile = ron::from_str(&s).unwrap();
    assert_eq!(f, back);
}

// ── RuleSet — push and filter ──────────────────────────────────────

#[test]
fn ruleset_push_assigns_sequential_ids() {
    let mut rs = RuleSet::new("test");
    let r1 = Rule::new(
        RewriteRuleId(999),
        lit(b'a'),
        lit(b'a'),
        Witness::Authored { note: "id-test".into() },
        0,
    );
    let id = rs.push(r1);
    assert_eq!(id, RewriteRuleId(0));
    assert_eq!(rs.rules[0].id, RewriteRuleId(0));
    let r2 = Rule::new(
        RewriteRuleId(999),
        lit(b'b'),
        lit(b'b'),
        Witness::Authored { note: "id-test-2".into() },
        0,
    );
    let id2 = rs.push(r2);
    assert_eq!(id2, RewriteRuleId(1));
}
