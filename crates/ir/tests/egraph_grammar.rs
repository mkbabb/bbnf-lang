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

    // Binary (two-child) variants now use [Id; 2] and report both children.
    let skip = GrammarENode::Skip([Id(7), Id(9)]);
    let children: Vec<Id> = skip.children().iter().copied().collect();
    assert_eq!(children, vec![Id(7), Id(9)]);
}

#[test]
fn rule_canonicalize_alias() {
    // Build an IR with an alias chain: rule[1] → rule[0] (alias).
    // A Ref(1) in rule[0]'s body should be rewritten to Ref(0).
    let mut ir = make_ir_with(IrNode::Epsilon);
    ir.strings.push("alias".to_string());
    // Add a second rule marked as alias of rule 0.
    let mut alias_meta = RuleMeta::default();
    alias_meta.is_alias = Some(0 as RuleId);
    ir.rules.push(IrRule {
        id: 1,
        name: 1,
        body: IrNode::Epsilon,
        meta: alias_meta,
        source_span: None,
    });
    // Rule 0 references the alias.
    ir.rules[0].body = IrNode::Ref(1);

    let egraph = build_and_saturate(&ir);

    // The class containing Ref(1) should also contain Ref(0) after saturation.
    let mut found_canonical = false;
    for class in egraph.classes() {
        let has_alias = class.iter().any(|n| matches!(n, GrammarENode::Ref(1)));
        let has_canon = class.iter().any(|n| matches!(n, GrammarENode::Ref(0)));
        if has_alias && has_canon {
            found_canonical = true;
            break;
        }
    }
    assert!(
        found_canonical,
        "CanonicalizeAlias should union Ref(alias_id) with Ref(canonical_id)"
    );
}

#[test]
fn rule_canonicalize_alias_chain() {
    // Three-rule chain: rule[2] → rule[1] → rule[0]. A Ref(2) should
    // resolve all the way to Ref(0).
    let mut ir = make_ir_with(IrNode::Epsilon);
    ir.strings.push("a1".to_string());
    ir.strings.push("a2".to_string());

    let mut m1 = RuleMeta::default();
    m1.is_alias = Some(0);
    ir.rules.push(IrRule {
        id: 1,
        name: 1,
        body: IrNode::Epsilon,
        meta: m1,
        source_span: None,
    });

    let mut m2 = RuleMeta::default();
    m2.is_alias = Some(1);
    ir.rules.push(IrRule {
        id: 2,
        name: 2,
        body: IrNode::Epsilon,
        meta: m2,
        source_span: None,
    });

    ir.rules[0].body = IrNode::Ref(2);

    let egraph = build_and_saturate(&ir);

    let mut found_terminal = false;
    for class in egraph.classes() {
        let has_mid = class.iter().any(|n| matches!(n, GrammarENode::Ref(2)));
        let has_canon = class.iter().any(|n| matches!(n, GrammarENode::Ref(0)));
        if has_mid && has_canon {
            found_terminal = true;
            break;
        }
    }
    assert!(
        found_terminal,
        "CanonicalizeAlias should resolve chains to the terminal target"
    );
}
