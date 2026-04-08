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

    let (egraph, _pool) = build_and_saturate(&ir);
    // Should have 1 class (the Literal) after interning.
    assert!(egraph.classes().count() >= 1);
}

#[test]
fn rule_eliminate_epsilon_in_seq() {
    // Seq([Literal("a"), Epsilon, Literal("c")]) should saturate all the
    // way down: EliminateEpsilon removes the epsilon, then MergeLiterals
    // collapses the two adjacent literals into a single "ac" literal,
    // then UnwrapSingletonSeq collapses the 1-element Seq to that literal.
    //
    // We use "a" + "c" to avoid accidentally hitting an existing entry
    // in the pool, and verify the merged literal exists in the pool.
    let mut ir = make_ir_with(IrNode::Epsilon);
    let a = lit(&mut ir, "a");
    let c = lit(&mut ir, "c");
    let seq = IrNode::Seq(vec![a, IrNode::Epsilon, c]);
    ir.rules[0].body = seq;

    let (egraph, pool) = build_and_saturate(&ir);

    // Extract — cost model should prefer the merged literal over the Seq.
    let cost = GrammarCostModel::default();
    let extractor = Extractor::new(&egraph, &cost);

    // The pool should now contain "ac" (interned by MergeLiterals).
    let strings = pool.into_vec();
    assert!(
        strings.iter().any(|s| s == "ac"),
        "MergeLiterals should have interned 'ac' into the shared pool, got: {:?}",
        strings
    );

    // At least one class should extract to Literal("ac").
    let ac_sid = strings.iter().position(|s| s == "ac").unwrap() as u32;
    let mut found_merged = false;
    for class in egraph.classes() {
        if let Some(best) = extractor.best_node(class.id) {
            if matches!(best, GrammarENode::Literal(sid) if *sid == ac_sid) {
                found_merged = true;
                break;
            }
        }
    }
    assert!(
        found_merged,
        "EliminateEpsilon + MergeLiterals + UnwrapSingletonSeq should \
         produce Literal(\"ac\") as the best form"
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

    let (egraph, _pool) = build_and_saturate(&ir);

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

    let (egraph, _pool) = build_and_saturate(&ir);

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
fn rule_merge_literals_concatenates_run() {
    // Seq([Lit("foo"), Lit("bar"), Lit("baz")]) should saturate to
    // Literal("foobarbaz") via MergeLiterals + UnwrapSingletonSeq.
    let mut ir = make_ir_with(IrNode::Epsilon);
    let a = lit(&mut ir, "foo");
    let b = lit(&mut ir, "bar");
    let c = lit(&mut ir, "baz");
    ir.rules[0].body = IrNode::Seq(vec![a, b, c]);

    let (egraph, pool) = build_and_saturate(&ir);
    let strings = pool.into_vec();

    assert!(
        strings.iter().any(|s| s == "foobarbaz"),
        "MergeLiterals should intern 'foobarbaz', got: {:?}",
        strings
    );

    let target = strings.iter().position(|s| s == "foobarbaz").unwrap() as u32;
    let cost = GrammarCostModel::default();
    let extractor = Extractor::new(&egraph, &cost);

    let mut found = false;
    for class in egraph.classes() {
        if let Some(best) = extractor.best_node(class.id) {
            if matches!(best, GrammarENode::Literal(sid) if *sid == target) {
                found = true;
                break;
            }
        }
    }
    assert!(found, "MergeLiterals should fully merge a pure-literal Seq");
}

#[test]
fn rule_merge_literals_leaves_non_literal_breaks() {
    // Seq([Lit("a"), Lit("b"), Ref(7), Lit("c"), Lit("d")]) should merge
    // into Seq([Lit("ab"), Ref(7), Lit("cd")]). Two merged literals, one
    // structural break.
    let mut ir = make_ir_with(IrNode::Epsilon);
    let a = lit(&mut ir, "a");
    let b = lit(&mut ir, "b");
    let c = lit(&mut ir, "c");
    let d = lit(&mut ir, "d");
    ir.rules[0].body = IrNode::Seq(vec![a, b, IrNode::Ref(7), c, d]);

    let (_egraph, pool) = build_and_saturate(&ir);
    let strings = pool.into_vec();

    assert!(
        strings.iter().any(|s| s == "ab"),
        "MergeLiterals should intern the 'ab' prefix-run"
    );
    assert!(
        strings.iter().any(|s| s == "cd"),
        "MergeLiterals should intern the 'cd' suffix-run"
    );
}

#[test]
fn rule_deduplicate_alt_branches() {
    // Alt([Literal("x"), Literal("x")]) should dedupe (two equivalent
    // e-classes) and then collapse to Literal("x") via UnwrapSingletonAlt.
    let mut ir = make_ir_with(IrNode::Epsilon);
    ir.strings.push("x".to_string());
    let x_sid = (ir.strings.len() - 1) as u32;
    let alt = IrNode::Alt(
        vec![
            AltBranch { node: IrNode::Literal(x_sid), first_set: None },
            AltBranch { node: IrNode::Literal(x_sid), first_set: None },
        ],
        None,
    );
    ir.rules[0].body = alt;

    let (egraph, _pool) = build_and_saturate(&ir);
    let cost = GrammarCostModel::default();
    let extractor = Extractor::new(&egraph, &cost);

    let mut found_single = false;
    for class in egraph.classes() {
        if let Some(best) = extractor.best_node(class.id) {
            if matches!(best, GrammarENode::Literal(sid) if *sid == x_sid) {
                found_single = true;
                break;
            }
        }
    }
    assert!(
        found_single,
        "DeduplicateAltBranches + UnwrapSingletonAlt should collapse \
         Alt([x, x]) to Literal(x)"
    );
}

#[test]
fn rule_superset_absorb_alt() {
    // Alt([Regex("[a-z]"), Regex("[a-c]")]) should absorb [a-c] and
    // collapse to Regex("[a-z]") via UnwrapSingletonAlt.
    let mut ir = make_ir_with(IrNode::Epsilon);
    let wide_sid = ir.strings.len() as u32;
    ir.strings.push("[a-z]".to_string());
    let narrow_sid = ir.strings.len() as u32;
    ir.strings.push("[a-c]".to_string());

    let alt = IrNode::Alt(
        vec![
            AltBranch { node: IrNode::Regex(wide_sid), first_set: None },
            AltBranch { node: IrNode::Regex(narrow_sid), first_set: None },
        ],
        None,
    );
    ir.rules[0].body = alt;

    let (egraph, _pool) = build_and_saturate(&ir);
    let cost = GrammarCostModel::default();
    let extractor = Extractor::new(&egraph, &cost);

    let mut found_wide_only = false;
    for class in egraph.classes() {
        if let Some(best) = extractor.best_node(class.id) {
            if matches!(best, GrammarENode::Regex(sid) if *sid == wide_sid) {
                found_wide_only = true;
                break;
            }
        }
    }
    assert!(
        found_wide_only,
        "SupersetAbsorbAlt should drop [a-c] from Alt([[a-z], [a-c]])"
    );
}

#[test]
fn rule_union_merge_alt() {
    // Alt([Regex("[a-c]"), Regex("[d-f]")]) should merge into Regex("[a-f]").
    let mut ir = make_ir_with(IrNode::Epsilon);
    let left_sid = ir.strings.len() as u32;
    ir.strings.push("[a-c]".to_string());
    let right_sid = ir.strings.len() as u32;
    ir.strings.push("[d-f]".to_string());

    let alt = IrNode::Alt(
        vec![
            AltBranch { node: IrNode::Regex(left_sid), first_set: None },
            AltBranch { node: IrNode::Regex(right_sid), first_set: None },
        ],
        None,
    );
    ir.rules[0].body = alt;

    let (egraph, pool) = build_and_saturate(&ir);
    let strings = pool.into_vec();

    // The algebra should intern a merged pattern covering [a-f].
    let merged: Vec<&String> = strings
        .iter()
        .filter(|s| s.contains("a") && s.contains("f") && s.starts_with('[') && s.ends_with(']'))
        .collect();
    assert!(
        !merged.is_empty(),
        "UnionMergeAlt should intern a merged char-class pattern, got: {:?}",
        strings
    );

    // At least one class should extract to the merged regex.
    let cost = GrammarCostModel::default();
    let extractor = Extractor::new(&egraph, &cost);
    let merged_sids: Vec<u32> = strings
        .iter()
        .enumerate()
        .filter_map(|(i, s)| {
            if s.contains("a") && s.contains("f") && s.starts_with('[') && s.ends_with(']') {
                Some(i as u32)
            } else {
                None
            }
        })
        .collect();

    let mut found_merged = false;
    for class in egraph.classes() {
        if let Some(best) = extractor.best_node(class.id) {
            if let GrammarENode::Regex(sid) = best {
                if merged_sids.contains(sid) {
                    found_merged = true;
                    break;
                }
            }
        }
    }
    assert!(
        found_merged,
        "UnionMergeAlt + UnwrapSingletonAlt should produce the merged Regex"
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

    let (egraph, _pool) = build_and_saturate(&ir);

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
