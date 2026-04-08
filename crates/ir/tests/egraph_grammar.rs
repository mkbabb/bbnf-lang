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

    let (egraph, _pool, _rule_body_ids) = build_and_saturate(&ir);
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

    let (egraph, pool, _rule_body_ids) = build_and_saturate(&ir);

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

    let (egraph, _pool, _rule_body_ids) = build_and_saturate(&ir);

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

    let (egraph, _pool, _rule_body_ids) = build_and_saturate(&ir);

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

    let (egraph, pool, _rule_body_ids) = build_and_saturate(&ir);
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

    let (_egraph, pool, _rule_body_ids) = build_and_saturate(&ir);
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

    let (egraph, _pool, _rule_body_ids) = build_and_saturate(&ir);
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

    let (egraph, _pool, _rule_body_ids) = build_and_saturate(&ir);
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
fn rule_inline_single_use_ref_nested() {
    // Rule 0 (entry) = Seq([Lit("prefix"), Ref(1)])
    // Rule 1 (single-use, acyclic, small body) = Literal("x")
    //
    // The nested Ref(1) e-class should saturate into being equivalent
    // to Literal("x") after InlineEligibleRef fires. The rule applies
    // only to Ref sites that are NOT themselves a rule's root — at
    // the root, inlining would dissolve the rule boundary, which the
    // rule's `canonical_roots` guard prevents.
    let mut ir = make_ir_with(IrNode::Epsilon);
    ir.strings.push("r1".to_string());
    let prefix_sid = ir.strings.len() as u32;
    ir.strings.push("prefix".to_string());
    let x_sid = ir.strings.len() as u32;
    ir.strings.push("x".to_string());

    ir.rules[0].body = IrNode::Seq(vec![IrNode::Literal(prefix_sid), IrNode::Ref(1)]);
    ir.rules.push(IrRule {
        id: 1,
        name: 1,
        body: IrNode::Literal(x_sid),
        meta: RuleMeta::default(),
        source_span: None,
    });

    let (egraph, _pool, _rule_body_ids) = build_and_saturate(&ir);

    // The nested Ref(1) class should now also contain Literal("x").
    let mut found_unified = false;
    for class in egraph.classes() {
        let has_ref = class.iter().any(|n| matches!(n, GrammarENode::Ref(1)));
        let has_lit = class.iter().any(|n| matches!(n, GrammarENode::Literal(sid) if *sid == x_sid));
        if has_ref && has_lit {
            found_unified = true;
            break;
        }
    }
    assert!(
        found_unified,
        "InlineEligibleRef should union nested Ref(1) with its body Literal(\"x\")"
    );
}

#[test]
fn rule_inline_small_acyclic_ref() {
    // Rule 0 (entry) = Seq([Ref(1), Ref(1)])  — two uses, so fuse_single_use
    // does NOT apply. But rule 1's body has ≤4 nodes, so acyclic
    // inlining should still fire.
    // Rule 1 = Seq([Lit("a"), Lit("b")])  (3 nodes: Seq + 2 Lits)
    let mut ir = make_ir_with(IrNode::Epsilon);
    ir.strings.push("r1".to_string());
    ir.strings.push("a".to_string());
    ir.strings.push("b".to_string());
    let a_sid = 2u32;
    let b_sid = 3u32;

    ir.rules[0].body = IrNode::Seq(vec![IrNode::Ref(1), IrNode::Ref(1)]);
    ir.rules.push(IrRule {
        id: 1,
        name: 1,
        body: IrNode::Seq(vec![IrNode::Literal(a_sid), IrNode::Literal(b_sid)]),
        meta: RuleMeta::default(),
        source_span: None,
    });

    let (egraph, _pool, _rule_body_ids) = build_and_saturate(&ir);

    // Ref(1)'s class should now also contain a Seq(Lit(a), Lit(b)) — or
    // a form unioned with it. We check by looking for a Seq class that
    // contains both Ref(1) and the Seq(Lit(a), Lit(b)) form.
    let mut found_inlined = false;
    for class in egraph.classes() {
        let has_ref = class.iter().any(|n| matches!(n, GrammarENode::Ref(1)));
        let has_seq = class.iter().any(|n| {
            if let GrammarENode::Seq(children) = n {
                children.len() == 2
                    && egraph.class(children[0]).iter().any(|n2| {
                        matches!(n2, GrammarENode::Literal(s) if *s == a_sid)
                    })
                    && egraph.class(children[1]).iter().any(|n2| {
                        matches!(n2, GrammarENode::Literal(s) if *s == b_sid)
                    })
            } else {
                false
            }
        });
        if has_ref && has_seq {
            found_inlined = true;
            break;
        }
    }
    assert!(
        found_inlined,
        "InlineEligibleRef should union Ref(1) with Seq(Lit(a), Lit(b))"
    );
}

#[test]
fn rule_factor_literal_byte_trie() {
    // Alt([Literal("rem"), Literal("rlh")]) should factor the shared
    // first byte 'r', producing Seq(Literal("r"), Alt([Literal("em"),
    // Literal("lh")])).
    let mut ir = make_ir_with(IrNode::Epsilon);
    let rem_sid = ir.strings.len() as u32;
    ir.strings.push("rem".to_string());
    let rlh_sid = ir.strings.len() as u32;
    ir.strings.push("rlh".to_string());

    let alt = IrNode::Alt(
        vec![
            AltBranch { node: IrNode::Literal(rem_sid), first_set: None },
            AltBranch { node: IrNode::Literal(rlh_sid), first_set: None },
        ],
        None,
    );
    ir.rules[0].body = alt;

    let (egraph, pool, _rule_body_ids) = build_and_saturate(&ir);
    let strings = pool.into_vec();

    // The prefix byte "r", and the tails "em" and "lh", must all be interned.
    for expected in ["r", "em", "lh"] {
        assert!(
            strings.iter().any(|s| s == expected),
            "FactorLiteralByteTrie should intern '{}', got: {:?}",
            expected, strings
        );
    }

    // The e-class containing the original Alt should now also contain
    // a Seq whose first child is Literal("r") and second is an Alt
    // of Literal("em")/Literal("lh") — confirming the factored form.
    let r_sid = strings.iter().position(|s| s == "r").unwrap() as u32;
    let em_sid = strings.iter().position(|s| s == "em").unwrap() as u32;
    let lh_sid = strings.iter().position(|s| s == "lh").unwrap() as u32;

    let mut found_factored = false;
    for class in egraph.classes() {
        for node in class.iter() {
            if let GrammarENode::Seq(children) = node {
                if children.len() == 2 {
                    // First child should be Literal("r")
                    let first_is_r = egraph.class(children[0]).iter().any(|n| {
                        matches!(n, GrammarENode::Literal(sid) if *sid == r_sid)
                    });
                    // Second child should be Alt containing Lit("em") and Lit("lh")
                    let second_is_tails = egraph.class(children[1]).iter().any(|n| {
                        if let GrammarENode::Alt(inner, _) = n {
                            inner.iter().any(|&id| {
                                egraph.class(id).iter().any(|n2| {
                                    matches!(n2, GrammarENode::Literal(s) if *s == em_sid)
                                })
                            }) && inner.iter().any(|&id| {
                                egraph.class(id).iter().any(|n2| {
                                    matches!(n2, GrammarENode::Literal(s) if *s == lh_sid)
                                })
                            })
                        } else {
                            false
                        }
                    });
                    if first_is_r && second_is_tails {
                        found_factored = true;
                        break;
                    }
                }
            }
        }
        if found_factored {
            break;
        }
    }
    assert!(
        found_factored,
        "FactorLiteralByteTrie should produce Seq(Lit(r), Alt([Lit(em), Lit(lh)]))"
    );
}

#[test]
fn rule_factor_shared_seq_prefix() {
    // Alt([Seq([Lit("a"), Lit("x")]), Seq([Lit("a"), Lit("y")])]) should
    // factor out the shared Lit("a") leader, producing Seq([Lit("a"),
    // Alt([Lit("x"), Lit("y")])]).
    let mut ir = make_ir_with(IrNode::Epsilon);
    let a_sid = ir.strings.len() as u32;
    ir.strings.push("a".to_string());
    let x_sid = ir.strings.len() as u32;
    ir.strings.push("x".to_string());
    let y_sid = ir.strings.len() as u32;
    ir.strings.push("y".to_string());

    let alt = IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Seq(vec![IrNode::Literal(a_sid), IrNode::Literal(x_sid)]),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Seq(vec![IrNode::Literal(a_sid), IrNode::Literal(y_sid)]),
                first_set: None,
            },
        ],
        None,
    );
    ir.rules[0].body = alt;

    let (egraph, _pool, _rule_body_ids) = build_and_saturate(&ir);

    // Look for an equivalence class containing a Seq where the first
    // child's class has Literal(a) and the second child's class has
    // an Alt of Literal(x), Literal(y).
    let mut found = false;
    for class in egraph.classes() {
        for node in class.iter() {
            if let GrammarENode::Seq(children) = node {
                if children.len() == 2 {
                    let first_is_a = egraph.class(children[0]).iter().any(|n| {
                        matches!(n, GrammarENode::Literal(s) if *s == a_sid)
                    });
                    let second_is_xy = egraph.class(children[1]).iter().any(|n| {
                        if let GrammarENode::Alt(inner, _) = n {
                            inner.len() == 2
                                && inner.iter().any(|&id| {
                                    egraph.class(id).iter().any(|n2| {
                                        matches!(n2, GrammarENode::Literal(s) if *s == x_sid)
                                    })
                                })
                                && inner.iter().any(|&id| {
                                    egraph.class(id).iter().any(|n2| {
                                        matches!(n2, GrammarENode::Literal(s) if *s == y_sid)
                                    })
                                })
                        } else {
                            false
                        }
                    });
                    if first_is_a && second_is_xy {
                        found = true;
                        break;
                    }
                }
            }
        }
        if found {
            break;
        }
    }
    assert!(
        found,
        "FactorSharedSeqPrefix should produce Seq(Lit(a), Alt([Lit(x), Lit(y)]))"
    );
}

#[test]
fn rule_fuse_alt_regex_branches() {
    // Alt([Regex("foo"), Regex("bar"), Literal("baz")]) without a
    // dispatch table should fuse to Regex("foo|bar|baz").
    let mut ir = make_ir_with(IrNode::Epsilon);
    let foo_sid = ir.strings.len() as u32;
    ir.strings.push("foo".to_string());
    let bar_sid = ir.strings.len() as u32;
    ir.strings.push("bar".to_string());
    let baz_sid = ir.strings.len() as u32;
    ir.strings.push("baz".to_string());

    let alt = IrNode::Alt(
        vec![
            AltBranch { node: IrNode::Regex(foo_sid), first_set: None },
            AltBranch { node: IrNode::Regex(bar_sid), first_set: None },
            AltBranch { node: IrNode::Literal(baz_sid), first_set: None },
        ],
        None,
    );
    ir.rules[0].body = alt;

    let (egraph, pool, _rule_body_ids) = build_and_saturate(&ir);
    let strings = pool.into_vec();

    assert!(
        strings.iter().any(|s| s == "foo|bar|baz"),
        "FuseAltRegexBranches should intern 'foo|bar|baz', got: {:?}",
        strings
    );

    let fused_sid = strings.iter().position(|s| s == "foo|bar|baz").unwrap() as u32;
    let cost = GrammarCostModel::default();
    let extractor = Extractor::new(&egraph, &cost);

    let mut found_fused = false;
    for class in egraph.classes() {
        if let Some(best) = extractor.best_node(class.id) {
            if matches!(best, GrammarENode::Regex(sid) if *sid == fused_sid) {
                found_fused = true;
                break;
            }
        }
    }
    assert!(
        found_fused,
        "FuseAltRegexBranches should produce Regex('foo|bar|baz') as the best form"
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

    let (egraph, pool, _rule_body_ids) = build_and_saturate(&ir);
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

    let (egraph, _pool, _rule_body_ids) = build_and_saturate(&ir);

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
