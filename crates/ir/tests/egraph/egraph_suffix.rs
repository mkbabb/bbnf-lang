//! Tests for the Y.11 `CommonSuffixFactor` rule.
//!
//! Verifies that an Alt whose branches are all Seqs with a shared
//! trailing sub-expression is rewritten to the factored form, and
//! that mixed Alts (where at least one branch isn't a Seq with a
//! matching tail) are left alone.

use bbnf_ir::egraph::node::GrammarENode;
use egraph::{EGraph, NoAnalysis, Rewrite};

use bbnf_ir::egraph::rules::CommonSuffixFactor;

fn canonical_form(egraph: &EGraph<GrammarENode, NoAnalysis>, id: egraph::Id) -> String {
    // Simple debug-format of the canonical e-class.
    let class = egraph.class(egraph.find_ref(id));
    format!("{:?}", class.iter().collect::<Vec<_>>())
}

#[test]
fn factors_common_trailing_literal() {
    // Build: Alt([Seq([Lit(0), Lit(99)]), Seq([Lit(1), Lit(99)])])
    // Expected after saturation: Seq([Alt([Lit(0), Lit(1)]), Lit(99)])
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();

    let a = eg.add(GrammarENode::Literal(0));
    let b = eg.add(GrammarENode::Literal(1));
    let tail = eg.add(GrammarENode::Literal(99));
    let seq1 = eg.add(GrammarENode::Seq(Box::new([a, tail])));
    let seq2 = eg.add(GrammarENode::Seq(Box::new([b, tail])));
    let alt = eg.add(GrammarENode::Alt(Box::new([seq1, seq2]), None));

    eg.rebuild();

    // Fire the rule once.
    let rule = CommonSuffixFactor;
    let matches = rule.search(&eg);
    assert!(
        !matches.is_empty(),
        "expected CommonSuffixFactor to match the uniform-suffix Alt"
    );

    for (class_id, m) in matches {
        rule.apply(&mut eg, class_id, m);
    }
    eg.rebuild();

    // The Alt class should now contain both the original form AND
    // a factored Seq([Alt([...]), Lit(99)]). Walk the class for a
    // Seq node whose last child is Lit(99).
    let class = eg.class(eg.find_ref(alt));
    let has_factored_form = class.iter().any(|n| match n {
        GrammarENode::Seq(children) if children.len() == 2 => {
            // last child must be the shared tail class
            eg.find_ref(children[1]) == eg.find_ref(tail)
        }
        _ => false,
    });
    assert!(
        has_factored_form,
        "factored form not present in Alt class; canonical = {}",
        canonical_form(&eg, alt)
    );
}

#[test]
fn leaves_mixed_alt_alone() {
    // Build: Alt([Seq([Lit(0), Lit(99)]), Lit(1)])
    // Expected: no match (second branch is not a Seq).
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();

    let a = eg.add(GrammarENode::Literal(0));
    let b = eg.add(GrammarENode::Literal(1));
    let tail = eg.add(GrammarENode::Literal(99));
    let seq1 = eg.add(GrammarENode::Seq(Box::new([a, tail])));
    let _alt = eg.add(GrammarENode::Alt(Box::new([seq1, b]), None));

    eg.rebuild();

    let rule = CommonSuffixFactor;
    let matches = rule.search(&eg);
    assert!(
        matches.is_empty(),
        "CommonSuffixFactor should skip mixed Alts (non-Seq branch) — got {} matches",
        matches.len()
    );
}

#[test]
fn leaves_divergent_tails_alone() {
    // Build: Alt([Seq([Lit(0), Lit(98)]), Seq([Lit(1), Lit(99)])])
    // Both branches are Seqs, but their tails differ — no match.
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();

    let a = eg.add(GrammarENode::Literal(0));
    let b = eg.add(GrammarENode::Literal(1));
    let t1 = eg.add(GrammarENode::Literal(98));
    let t2 = eg.add(GrammarENode::Literal(99));
    let seq1 = eg.add(GrammarENode::Seq(Box::new([a, t1])));
    let seq2 = eg.add(GrammarENode::Seq(Box::new([b, t2])));
    let _alt = eg.add(GrammarENode::Alt(Box::new([seq1, seq2]), None));

    eg.rebuild();

    let rule = CommonSuffixFactor;
    let matches = rule.search(&eg);
    assert!(
        matches.is_empty(),
        "CommonSuffixFactor should skip Alts with divergent tails — got {} matches",
        matches.len()
    );
}
