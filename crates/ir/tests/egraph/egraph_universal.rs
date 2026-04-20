//! Per-rule unit tests for the AY.W2.3 universal rewrites (G1-G2).
//!
//! - [`alt_of_single_collapses_to_inner`] — G1 `Alt([x]) ≡ x`.
//! - [`alt_of_single_abstains_on_multi_branch`] — G1 negative case.
//! - [`repeat_of_single_collapses_to_inner`] — G2 `Repeat{1,1} ≡ inner`.
//! - [`repeat_of_single_abstains_on_bounds`] — G2 negative case.

use bbnf_ir::egraph::node::GrammarENode;
use bbnf_ir::egraph::rules::{AltOfSingle, RepeatOfSingle};
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
