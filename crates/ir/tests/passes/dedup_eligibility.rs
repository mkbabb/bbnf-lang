//! AW-IV.W4.3 — Dedup-eligibility miner coverage.
//!
//! Verifies the admission contract: fixed-width skeleton, total record
//! count ≤ MAX_DEDUP_ROWS, and a repeated-payload signature. Rules that
//! match all three are admitted; rules that fail any one are rejected.

use std::collections::HashMap;

use bbnf_ir::passes::recognizers::dedup_eligibility::{
    is_dedup_eligible_body, mine_dedup_eligible_rules, MAX_DEDUP_ROWS,
};
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

        shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(),
        eclass_facts: HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: HashMap::new(),
        disjoint_first_tables: HashMap::new(),
        pattern_alphabets: HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
    }
}

#[test]
fn literal_only_alt_is_admitted() {
    // Shape: `null | true | false` — every branch is a Literal.
    let ir = empty_ir(
        vec![rule(
            0,
            0,
            IrNode::Alt(
                vec![
                    AltBranch { node: IrNode::Literal(0), first_set: None },
                    AltBranch { node: IrNode::Literal(1), first_set: None },
                    AltBranch { node: IrNode::Literal(2), first_set: None },
                ],
                None,
            ),
        )],
        vec!["null".into(), "true".into(), "false".into()],
    );
    assert!(is_dedup_eligible_body(&ir.rules[0].body, &ir));
}

#[test]
fn single_literal_is_admitted() {
    let ir = empty_ir(
        vec![rule(0, 0, IrNode::Literal(0))],
        vec!["!important".into()],
    );
    assert!(is_dedup_eligible_body(&ir.rules[0].body, &ir));
}

#[test]
fn mixed_alt_with_non_literal_branch_is_rejected() {
    // Shape: `Literal | Regex` — the Regex branch is not literal-led.
    let ir = empty_ir(
        vec![rule(
            0,
            0,
            IrNode::Alt(
                vec![
                    AltBranch { node: IrNode::Literal(0), first_set: None },
                    AltBranch { node: IrNode::Regex(1), first_set: None },
                ],
                None,
            ),
        )],
        vec!["kw".into(), "[a-z]+".into()],
    );
    // Alt branches differ in row count (Literal = 1, Regex = 1) but
    // the repeated-payload check requires literal-only branches, so
    // this rejects.
    assert!(!is_dedup_eligible_body(&ir.rules[0].body, &ir));
}

#[test]
fn variable_width_repeat_is_rejected() {
    let ir = empty_ir(
        vec![rule(
            0,
            0,
            IrNode::Repeat {
                inner: Box::new(IrNode::Literal(0)),
                lo: 0,
                hi: u32::MAX,
            },
        )],
        vec!["x".into()],
    );
    assert!(!is_dedup_eligible_body(&ir.rules[0].body, &ir));
}

#[test]
fn oversized_rule_is_rejected() {
    // A Seq of more than MAX_DEDUP_ROWS children -> the total emitted
    // row count exceeds the dedup budget.
    let mut children = Vec::new();
    for _ in 0..(MAX_DEDUP_ROWS + 1) {
        children.push(IrNode::Literal(0));
    }
    let ir = empty_ir(
        vec![rule(0, 0, IrNode::Seq(children))],
        vec!["x".into()],
    );
    assert!(!is_dedup_eligible_body(&ir.rules[0].body, &ir));
}

#[test]
fn transparent_rule_is_skipped() {
    let mut r = rule(0, 0, IrNode::Literal(0));
    r.meta.is_transparent = true;
    let ir = empty_ir(vec![r], vec!["x".into()]);
    let admitted = mine_dedup_eligible_rules(&ir);
    // Transparent rules skip the admission check entirely.
    assert!(admitted.is_empty());
}

#[test]
fn mine_returns_sorted_deduplicated_vec() {
    let ir = empty_ir(
        vec![
            // Rule 2 — admitted.
            rule(2, 0, IrNode::Literal(0)),
            // Rule 0 — admitted.
            rule(0, 1, IrNode::Literal(1)),
            // Rule 1 — NOT admitted (variable-width).
            rule(
                1,
                2,
                IrNode::Repeat {
                    inner: Box::new(IrNode::Literal(0)),
                    lo: 0,
                    hi: u32::MAX,
                },
            ),
        ],
        vec!["a".into(), "b".into(), "c".into()],
    );
    let admitted = mine_dedup_eligible_rules(&ir);
    assert_eq!(admitted, vec![0, 2], "ascending order, rule 1 excluded");
}
