//! AW-IV.W4.3.c — grammar-level pattern hoisting coverage.
//!
//! Verifies that recurring Seq/Alt sub-patterns are hoisted into
//! fresh synthesised rules at the declared occurrence threshold.

use std::collections::HashMap;

use bbnf_ir::passes::transform::hoist_recurring_patterns;
use bbnf_ir::{AltBranch, GrammarIR, IrNode, IrRule, RuleMeta};

fn rule(id: u32, name: u32, body: IrNode) -> IrRule {
    IrRule {
        id,
        name,
        body,
        meta: RuleMeta::default(),
        source_span: None,
    }
}

fn empty_ir(rules: Vec<IrRule>, strings: Vec<String>) -> GrammarIR {
    GrammarIR {
        rules,
        entry: 0,
        strings,
        fns: vec![],
        types: vec![],
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
        delim_scan_configs: HashMap::new(),
        key_dispatch_configs: HashMap::new(),
        context_facts: HashMap::new(),
        has_family_recognizers: false,
        regex_engine_decisions: HashMap::new(),
        dag: None,
        cost_config: bbnf_ir::CostConfig::default(),
        type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: HashMap::new(),
        string_index: HashMap::new(),
        payload_layouts: HashMap::new(),
        structural_alphabet: None,
        push_fingerprint: None,
        dedup_eligible_rules: Vec::new(),

        shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(
        ),
        eclass_facts: HashMap::new(),
        keyword_branches: HashMap::new(),
        disjoint_first_tables: HashMap::new(),
        pattern_alphabets: HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        struct_registry: bbnf_ir::StructRegistry::default(),
        type_obligations: Vec::new(),
        inline_trace: bbnf_ir::passes::inline_trace::InlineTrace::default(),
        path_check_resolver: bbnf_ir::passes::path_check::PathCheckResolver::default(),
    }
}

/// Build a `Seq` of three children that's recurring enough to trigger
/// hoisting when present three times.
fn triple_seq() -> IrNode {
    IrNode::Seq(vec![
        IrNode::Literal(0),
        IrNode::Literal(1),
        IrNode::Literal(2),
    ])
}

#[test]
fn three_occurrences_trigger_hoist() {
    // Three rules, each containing a copy of the same Seq nested under
    // an outer wrapper. The hoist should lift the Seq into a new rule.
    let outer = |inner: IrNode| IrNode::Seq(vec![IrNode::Literal(3), inner]);
    let mut ir = empty_ir(
        vec![
            rule(0, 0, outer(triple_seq())),
            rule(1, 1, outer(triple_seq())),
            rule(2, 2, outer(triple_seq())),
        ],
        vec!["a".into(), "b".into(), "c".into(), "w".into()],
    );
    let hoisted = hoist_recurring_patterns(&mut ir);
    assert!(hoisted > 0, "expected at least one hoist, got {}", hoisted);
    // New synthesised rule(s) appended at the end.
    assert!(ir.rules.len() > 3, "new rules appended");
    // Every original rule body now contains a Ref to the synthesised
    // rule instead of the inline Seq.
    for i in 0..3 {
        match &ir.rules[i].body {
            IrNode::Seq(children) => {
                let inner = &children[1];
                assert!(
                    matches!(inner, IrNode::Ref(_)),
                    "rule {} inner should now be Ref, got {:?}",
                    i,
                    inner
                );
            }
            other => panic!("rule {} body should be Seq, got {:?}", i, other),
        }
    }
}

#[test]
fn two_occurrences_do_not_hoist() {
    // Only two copies — below MIN_OCCURRENCES=3 so no hoist fires.
    let outer = |inner: IrNode| IrNode::Seq(vec![IrNode::Literal(3), inner]);
    let mut ir = empty_ir(
        vec![
            rule(0, 0, outer(triple_seq())),
            rule(1, 1, outer(triple_seq())),
        ],
        vec!["a".into(), "b".into(), "c".into(), "w".into()],
    );
    let hoisted = hoist_recurring_patterns(&mut ir);
    assert_eq!(hoisted, 0, "below threshold — no hoist expected");
    assert_eq!(ir.rules.len(), 2, "no new rules synthesised");
}

#[test]
fn rule_bodies_are_not_self_hoisted() {
    // Three rules with the SAME body — if the pass allowed root-level
    // hoisting, each rule body would rewrite to a Ref pointing at the
    // new rule (infinite indirection). The `is_root` guard prevents
    // this.
    let mut ir = empty_ir(
        vec![
            rule(0, 0, triple_seq()),
            rule(1, 1, triple_seq()),
            rule(2, 2, triple_seq()),
        ],
        vec!["a".into(), "b".into(), "c".into()],
    );
    let _ = hoist_recurring_patterns(&mut ir);
    // Rule bodies unchanged — no self-hoisting.
    for i in 0..3 {
        assert!(
            matches!(&ir.rules[i].body, IrNode::Seq(_)),
            "rule {} body should remain Seq (not Ref)",
            i
        );
    }
}

#[test]
fn non_eligible_nodes_are_not_hoisted() {
    // A recurring single-literal — below MIN_PATTERN_NODES, so the
    // miner rejects even at ≥3 occurrences. The outer rule body
    // wrapper is root-level (never hoisted) and its inner literal is
    // too small to qualify, so the pass emits zero hoists.
    let outer = |inner: IrNode| IrNode::Seq(vec![inner, IrNode::Literal(9)]);
    let mut ir = empty_ir(
        vec![
            rule(0, 0, outer(IrNode::Literal(0))),
            rule(1, 1, outer(IrNode::Literal(0))),
            rule(2, 2, outer(IrNode::Literal(0))),
        ],
        vec!["x".into(), "y".into()],
    );
    let hoisted = hoist_recurring_patterns(&mut ir);
    // The outer Seq at each rule is the rule body root — the is_root
    // guard excludes it. The inner Literal(0) is below MIN_PATTERN_NODES.
    // Zero hoists is the correct outcome.
    assert_eq!(
        hoisted, 0,
        "root bodies skipped, inner literal below threshold — no hoist"
    );
}

#[test]
fn alt_pattern_hoists() {
    // An Alt with the same literal branches recurring three times.
    let alt_body = || {
        IrNode::Alt(
            vec![
                AltBranch {
                    node: IrNode::Literal(0),
                    first_set: None,
                },
                AltBranch {
                    node: IrNode::Literal(1),
                    first_set: None,
                },
            ],
            None,
        )
    };
    let outer = |inner: IrNode| IrNode::Seq(vec![IrNode::Literal(2), inner]);
    let mut ir = empty_ir(
        vec![
            rule(0, 0, outer(alt_body())),
            rule(1, 1, outer(alt_body())),
            rule(2, 2, outer(alt_body())),
        ],
        vec!["a".into(), "b".into(), "c".into()],
    );
    let hoisted = hoist_recurring_patterns(&mut ir);
    assert_eq!(hoisted, 1, "one Alt pattern hoisted");
    assert_eq!(ir.rules.len(), 4, "one new rule synthesised");
}
