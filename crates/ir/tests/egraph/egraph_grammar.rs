//! Grammar e-graph rule unit tests — one per retained rewrite rule.
//!
//! Exercises `build_and_saturate` on minimal IR fixtures to verify
//! each rule fires and cost-guided extraction picks the simplified
//! form. Retained rules: `DeduplicateAltBranches`, `SupersetAbsorbAlt`,
//! `UnionMergeAlt`, `FuseAltRegexBranches` (see
//! `crates/ir/src/egraph/rules/mod.rs` for the retention rationale).

use std::collections::HashMap;

use bbnf_ir::egraph::{GrammarCostModel, GrammarENode, build_and_saturate};
use bbnf_ir::{AltBranch, GrammarIR, IrNode, IrRule, RuleMeta};
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
        recognizer_decisions: HashMap::new(),
        delim_scan_configs: std::collections::HashMap::new(),
        key_dispatch_configs: std::collections::HashMap::new(),
        context_facts: std::collections::HashMap::new(),
        has_family_recognizers: false,
        regex_engine_decisions: std::collections::HashMap::new(),
        dag: None, cost_config: bbnf_ir::CostConfig::default(), type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: std::collections::HashMap::new(),
        payload_layouts: std::collections::HashMap::new(),
        string_index: Default::default(),
        structural_alphabet: None,
        push_fingerprint: None,
            dedup_eligible_rules: Vec::new(),

            shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(),
        eclass_facts: std::collections::HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: std::collections::HashMap::new(),
        disjoint_first_tables: std::collections::HashMap::new(),
        pattern_alphabets: std::collections::HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        struct_registry: bbnf_ir::StructRegistry::default(),
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
    assert!(egraph.classes().count() >= 1);
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

    let skip = GrammarENode::Skip([Id(7), Id(9)]);
    let children: Vec<Id> = skip.children().iter().copied().collect();
    assert_eq!(children, vec![Id(7), Id(9)]);
}

#[test]
fn rule_deduplicate_alt_branches() {
    // Alt([Literal("x"), Literal("x")]) — DeduplicateAltBranches sees
    // two children with the same canonical e-class and unions the Alt
    // class directly with the sole surviving child via the
    // single-survivor fast path in `apply`. Extraction prefers the
    // Literal over an Alt via the cost model.
    let mut ir = make_ir_with(IrNode::Epsilon);
    ir.strings.push("x".to_string());
    let x_sid = (ir.strings.len() - 1) as u32;
    let alt = IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Literal(x_sid),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Literal(x_sid),
                first_set: None,
            },
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
        "DeduplicateAltBranches should collapse Alt([x, x]) to Literal(x)"
    );
}

#[test]
fn rule_superset_absorb_alt() {
    // Alt([Regex("[a-z]"), Regex("[a-c]")]) — SupersetAbsorbAlt drops
    // the subsumed [a-c] branch; the single-survivor fast path unions
    // the Alt class directly with the surviving Regex child.
    let mut ir = make_ir_with(IrNode::Epsilon);
    let wide_sid = ir.strings.len() as u32;
    ir.strings.push("[a-z]".to_string());
    let narrow_sid = ir.strings.len() as u32;
    ir.strings.push("[a-c]".to_string());

    let alt = IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Regex(wide_sid),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Regex(narrow_sid),
                first_set: None,
            },
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
fn rule_union_merge_alt() {
    // Alt([Regex("[a-c]"), Regex("[d-f]")]) should merge into Regex("[a-f]").
    let mut ir = make_ir_with(IrNode::Epsilon);
    let left_sid = ir.strings.len() as u32;
    ir.strings.push("[a-c]".to_string());
    let right_sid = ir.strings.len() as u32;
    ir.strings.push("[d-f]".to_string());

    let alt = IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Regex(left_sid),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Regex(right_sid),
                first_set: None,
            },
        ],
        None,
    );
    ir.rules[0].body = alt;

    let (egraph, pool, _rule_body_ids) = build_and_saturate(&ir);
    let strings = pool.into_vec();

    let merged: Vec<&String> = strings
        .iter()
        .filter(|s| s.contains('a') && s.contains('f') && s.starts_with('[') && s.ends_with(']'))
        .collect();
    assert!(
        !merged.is_empty(),
        "UnionMergeAlt should intern a merged char-class pattern, got: {:?}",
        strings
    );

    let cost = GrammarCostModel::default();
    let extractor = Extractor::new(&egraph, &cost);
    let merged_sids: Vec<u32> = strings
        .iter()
        .enumerate()
        .filter_map(|(i, s)| {
            if s.contains('a') && s.contains('f') && s.starts_with('[') && s.ends_with(']') {
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
        "UnionMergeAlt should produce the merged Regex as the best form"
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
            AltBranch {
                node: IrNode::Regex(foo_sid),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Regex(bar_sid),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Literal(baz_sid),
                first_set: None,
            },
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
