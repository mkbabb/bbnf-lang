//! Per-rule unit tests for the AY.W2.3 universal rewrites (G1-G3).
//!
//! - [`alt_of_single_collapses_to_inner`] — G1 `Alt([x]) ≡ x`.
//! - [`alt_of_single_abstains_on_multi_branch`] — G1 negative case.
//! - [`repeat_of_single_collapses_to_inner`] — G2 `Repeat{1,1} ≡ inner`.
//! - [`repeat_of_single_abstains_on_bounds`] — G2 negative case.
//! - [`wrap_of_epsilon_scalar_collapses_to_leaf`] — G3 PRIMARY LEVER.
//! - [`wrap_of_epsilon_with_ref_scalar_collapses`] — G3 via Ref +
//!   TypeDesc side-table.
//! - [`wrap_of_epsilon_non_scalar_abstains`] — G3 composite-type
//!   negative case.
//! - [`wrap_of_epsilon_three_branch_abstains`] — G3 shape guard.

use rustc_hash::FxHashMap;

use bbnf_ir::egraph::node::GrammarENode;
use bbnf_ir::egraph::rules::{AltOfSingle, RepeatOfSingle, WrapOfEpsilonScalar};
use bbnf_ir::{RuleId, TypeDesc};
use egraph::{EGraph, NoAnalysis, Rewrite};

// ── G1 ───────────────────────────────────────────────────────────────────────

#[test]
fn alt_of_single_collapses_to_inner() {
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();
    let x = eg.add(GrammarENode::Literal(0));
    let alt = eg.add(GrammarENode::Alt(Box::new([x]), None));
    eg.rebuild();

    let rule = AltOfSingle;
    let matches = rule.search(&eg);
    assert_eq!(matches.len(), 1, "G1 expected to match single-branch Alt");
    for (class_id, m) in matches {
        rule.apply(&mut eg, class_id, m);
    }
    eg.rebuild();

    assert_eq!(
        eg.find_ref(alt),
        eg.find_ref(x),
        "G1: Alt([x]) class should unify with x after rewrite"
    );
}

#[test]
fn alt_of_single_abstains_on_multi_branch() {
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();
    let x = eg.add(GrammarENode::Literal(0));
    let y = eg.add(GrammarENode::Literal(1));
    let _alt = eg.add(GrammarENode::Alt(Box::new([x, y]), None));
    eg.rebuild();

    let matches = AltOfSingle.search(&eg);
    assert!(
        matches.is_empty(),
        "G1 should ignore multi-branch Alt, got {} matches",
        matches.len()
    );
}

// ── G2 ───────────────────────────────────────────────────────────────────────

#[test]
fn repeat_of_single_collapses_to_inner() {
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();
    let inner = eg.add(GrammarENode::Literal(0));
    let rep = eg.add(GrammarENode::Repeat {
        inner,
        lo: 1,
        hi: 1,
    });
    eg.rebuild();

    let rule = RepeatOfSingle;
    let matches = rule.search(&eg);
    assert_eq!(matches.len(), 1, "G2 expected to match Repeat {{lo:1,hi:1}}");
    for (class_id, m) in matches {
        rule.apply(&mut eg, class_id, m);
    }
    eg.rebuild();

    assert_eq!(
        eg.find_ref(rep),
        eg.find_ref(inner),
        "G2: Repeat {{1,1}}(x) class should unify with x after rewrite"
    );
}

#[test]
fn repeat_of_single_abstains_on_bounds() {
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();
    let inner = eg.add(GrammarENode::Literal(0));
    let _rep_opt = eg.add(GrammarENode::Repeat {
        inner,
        lo: 0,
        hi: 1,
    });
    let _rep_star = eg.add(GrammarENode::Repeat {
        inner,
        lo: 0,
        hi: u32::MAX,
    });
    let _rep_plus = eg.add(GrammarENode::Repeat {
        inner,
        lo: 1,
        hi: u32::MAX,
    });
    eg.rebuild();

    let matches = RepeatOfSingle.search(&eg);
    assert!(
        matches.is_empty(),
        "G2 should ignore Repeat with bounds other than (1,1), got {} matches",
        matches.len()
    );
}

// ── G3 ───────────────────────────────────────────────────────────────────────

#[test]
fn wrap_of_epsilon_scalar_collapses_to_leaf() {
    // Alt([Literal("a"), Epsilon]) → Literal("a").
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();
    let leaf = eg.add(GrammarENode::Literal(0));
    let eps = eg.add(GrammarENode::Epsilon);
    let alt = eg.add(GrammarENode::Alt(Box::new([leaf, eps]), None));
    eg.rebuild();

    // Literal branches are scalar by construction — empty TypeDesc
    // table suffices.
    let rule = WrapOfEpsilonScalar::new(FxHashMap::default());
    let matches = rule.search(&eg);
    assert_eq!(
        matches.len(),
        1,
        "G3 expected to match Alt([leaf, ε]) with scalar leaf"
    );
    for (class_id, m) in matches {
        rule.apply(&mut eg, class_id, m);
    }
    eg.rebuild();

    assert_eq!(
        eg.find_ref(alt),
        eg.find_ref(leaf),
        "G3: Alt([leaf, ε]) class should unify with leaf"
    );
}

#[test]
fn wrap_of_epsilon_with_ref_scalar_collapses() {
    // Alt([Ref(rule_id), Epsilon]) with rule_id → TypeDesc::F64.
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();
    let rid: RuleId = 42;
    let leaf = eg.add(GrammarENode::Ref(rid));
    let eps = eg.add(GrammarENode::Epsilon);
    let alt = eg.add(GrammarENode::Alt(Box::new([leaf, eps]), None));
    eg.rebuild();

    let mut rule_types = FxHashMap::default();
    rule_types.insert(rid, TypeDesc::F64);
    let rule = WrapOfEpsilonScalar::new(rule_types);
    let matches = rule.search(&eg);
    assert_eq!(
        matches.len(),
        1,
        "G3 expected to match Alt([Ref, ε]) with scalar-typed target"
    );
    for (class_id, m) in matches {
        rule.apply(&mut eg, class_id, m);
    }
    eg.rebuild();
    assert_eq!(
        eg.find_ref(alt),
        eg.find_ref(leaf),
        "G3: Alt([scalar Ref, ε]) class should unify with the Ref branch"
    );
}

#[test]
fn wrap_of_epsilon_non_scalar_abstains() {
    // Alt([Ref(rule_id), Epsilon]) with rule_id → TypeDesc::Vec(Span)
    // (composite, not scalar). G3 must abstain.
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();
    let rid: RuleId = 42;
    let _leaf = eg.add(GrammarENode::Ref(rid));
    let _eps = eg.add(GrammarENode::Epsilon);
    let _alt = eg.add(GrammarENode::Alt(Box::new([_leaf, _eps]), None));
    eg.rebuild();

    let mut rule_types = FxHashMap::default();
    rule_types.insert(rid, TypeDesc::Vec(Box::new(TypeDesc::Span)));
    let rule = WrapOfEpsilonScalar::new(rule_types);
    let matches = rule.search(&eg);
    assert!(
        matches.is_empty(),
        "G3 should abstain on non-scalar projection, got {} matches",
        matches.len()
    );
}

#[test]
fn wrap_of_epsilon_three_branch_abstains() {
    // Alt with 3 branches — G3 only fires on exactly 2.
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();
    let a = eg.add(GrammarENode::Literal(0));
    let b = eg.add(GrammarENode::Literal(1));
    let eps = eg.add(GrammarENode::Epsilon);
    let _alt = eg.add(GrammarENode::Alt(Box::new([a, b, eps]), None));
    eg.rebuild();

    let matches = WrapOfEpsilonScalar::new(FxHashMap::default()).search(&eg);
    assert!(
        matches.is_empty(),
        "G3 should abstain on 3-branch Alt, got {} matches",
        matches.len()
    );
}
