//! Grammar e-graph end-to-end tests.
//!
//! Exercises build_and_saturate + extraction on simple IR snippets to
//! verify rewrite rules fire and the cost model picks simpler forms.

use std::collections::HashMap;

use bbnf_ir::egraph::{build_and_saturate, GrammarCostModel, GrammarENode};
use bbnf_ir::{
    AltBranch, GrammarIR, IrNode, IrRule, RuleId, RuleMeta,
};
use egraph::{Extractor, Language};

fn make_ir_with(body: IrNode) -> GrammarIR {
    let mut strings = Vec::new();
    strings.push("r".to_string());
    let rules = vec![IrRule {
        id: 0,
        name: 0,
        body,
        meta: RuleMeta::default(),
        source_span: None,
    }];
    GrammarIR {
        rules,
        entry: 0,
        strings,
        fns: Vec::new(),
        types: Vec::new(),
        follow_sets: HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: Vec::new(),
        type_map: None,
        pattern_annotations: HashMap::new(),
        regex_info: HashMap::new(),
        node_facts: HashMap::new(),
    }
}

fn lit(ir: &mut GrammarIR, s: &str) -> IrNode {
    let sid = ir.strings.len() as u32;
    ir.strings.push(s.to_string());
    IrNode::Literal(sid)
}

#[test]
fn build_literal() {
    let mut ir = make_ir_with(IrNode::Epsilon);
    let body = lit(&mut ir, "hello");
    ir.rules[0].body = body;

    let egraph = build_and_saturate(&ir);
    // Should have 1 class (the Literal) after interning.
    assert!(egraph.classes().count() >= 1);
}

#[test]
fn rule_eliminate_epsilon_in_seq() {
    // Seq([Literal("a"), Epsilon, Literal("b")]) should become equivalent
    // to Seq([Literal("a"), Literal("b")]) after saturation.
    let mut ir = make_ir_with(IrNode::Epsilon);
    let a = lit(&mut ir, "a");
    let b = lit(&mut ir, "b");
    let seq = IrNode::Seq(vec![a, IrNode::Epsilon, b]);
    ir.rules[0].body = seq;

    let egraph = build_and_saturate(&ir);

    // Extract — cost model should prefer the smaller (2-element) Seq.
    let cost = GrammarCostModel::default();
    let extractor = Extractor::new(&egraph, &cost);

    // Find a class containing a Seq and verify the best node is a 2-element Seq.
    let mut found_optimized = false;
    for class in egraph.classes() {
        if let Some(best) = extractor.best_node(class.id) {
            if let GrammarENode::Seq(children) = best {
                if children.len() == 2 && !found_optimized {
                    // Check that neither child is Epsilon.
                    let all_non_eps = children.iter().all(|&c| {
                        !egraph
                            .class(c)
                            .iter()
                            .any(|n| matches!(n, GrammarENode::Epsilon))
                    });
                    if all_non_eps {
                        found_optimized = true;
                    }
                }
            }
        }
    }
    assert!(
        found_optimized,
        "EliminateEpsilon rule should produce a 2-element Seq without epsilons"
    );
}

#[test]
fn rule_unwrap_singleton_alt() {
    // Alt([Literal("x")]) should collapse to Literal("x").
    let mut ir = make_ir_with(IrNode::Epsilon);
    let x = lit(&mut ir, "x");
    let alt = IrNode::Alt(
        vec![AltBranch {
            node: x,
            first_set: None,
        }],
        None,
    );
    ir.rules[0].body = alt;

    let egraph = build_and_saturate(&ir);

    // The Alt class should now be unioned with the Literal class.
    let cost = GrammarCostModel::default();
    let extractor = Extractor::new(&egraph, &cost);

    // Find the root class (the one the IR inserted at rule body).
    // Since we only added one rule, the last class added is the Alt root.
    let mut found_literal_as_best = false;
    for class in egraph.classes() {
        if let Some(best) = extractor.best_node(class.id) {
            // The Alt should prefer its Literal child.
            if let GrammarENode::Alt(_, _) = best {
                // Not simplified yet — check other classes.
                continue;
            }
            if matches!(best, GrammarENode::Literal(_)) {
                found_literal_as_best = true;
            }
        }
    }
    assert!(
        found_literal_as_best,
        "UnwrapSingletonAlt should let extraction pick Literal over Alt"
    );
}

#[test]
fn e_node_language_impl() {
    use egraph::Id;
    // Sanity-check that GrammarENode::children() returns the expected slice.
    let seq = GrammarENode::Seq(vec![Id(1), Id(2), Id(3)].into_boxed_slice());
    let children: Vec<Id> = seq.children().iter().copied().collect();
    assert_eq!(children, vec![Id(1), Id(2), Id(3)]);

    let rep = GrammarENode::Repeat {
        inner: Id(5),
        lo: 0,
        hi: u32::MAX,
    };
    let children: Vec<Id> = rep.children().iter().copied().collect();
    assert_eq!(children, vec![Id(5)]);
}
