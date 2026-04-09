//! Context facts propagation tests.

use std::collections::HashMap;

use bbnf_ir::dag::GrammarDag;
use bbnf_ir::passes::context::{
    compute_context_facts, DiscriminationStrength, ScanSafety,
};
use bbnf_ir::{
    AltBranch, AltDispatch, GrammarIR, IrNode, IrRule, RuleId, RuleMeta,
};

fn base_ir() -> GrammarIR {
    GrammarIR {
        rules: Vec::new(),
        entry: 0,
        strings: Vec::new(),
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
        dag: None,    }
}

fn lit(ir: &mut GrammarIR, s: &str) -> IrNode {
    let sid = ir.strings.len() as u32;
    ir.strings.push(s.to_string());
    IrNode::Literal(sid)
}

fn push_rule(ir: &mut GrammarIR, body: IrNode) {
    let id = ir.rules.len() as RuleId;
    ir.rules.push(IrRule {
        id,
        name: 0,
        body,
        meta: RuleMeta::default(),
        source_span: None,
    });
}

#[test]
fn alt_with_dispatch_is_strong() {
    let mut ir = base_ir();
    let a = lit(&mut ir, "a");
    let b = lit(&mut ir, "b");
    let dispatch = AltDispatch {
        table: vec![255; 128],
        fallback_idx: None,
    };
    let alt = IrNode::Alt(
        vec![
            AltBranch { node: a, first_set: None },
            AltBranch { node: b, first_set: None },
        ],
        Some(dispatch),
    );
    push_rule(&mut ir, alt);

    let dag = GrammarDag::from_ir(&ir);
    let facts = compute_context_facts(&ir, &dag);

    let root = dag.rule_root(0).unwrap();
    let f = facts.get(&root).expect("root should have facts");
    assert_eq!(f.discrimination, DiscriminationStrength::Strong);
}

#[test]
fn wrap_pattern_is_scan_safe() {
    let mut ir = base_ir();
    let open = lit(&mut ir, "{");
    let body = lit(&mut ir, "x");
    let close = lit(&mut ir, "}");
    // Wrap: Skip(Next(open, body), close)
    let wrap = IrNode::Skip(
        Box::new(IrNode::Next(Box::new(open), Box::new(body))),
        Box::new(close),
    );
    push_rule(&mut ir, wrap);

    let dag = GrammarDag::from_ir(&ir);
    let facts = compute_context_facts(&ir, &dag);

    let root = dag.rule_root(0).unwrap();
    let f = facts.get(&root).expect("root should have facts");
    assert_eq!(f.scan_safety, ScanSafety::Safe);
}

#[test]
fn bare_literal_has_default_facts() {
    let mut ir = base_ir();
    let body = lit(&mut ir, "hello");
    push_rule(&mut ir, body);

    let dag = GrammarDag::from_ir(&ir);
    let facts = compute_context_facts(&ir, &dag);

    let root = dag.rule_root(0).unwrap();
    let f = facts.get(&root).cloned().unwrap_or_default();
    assert_eq!(f.discrimination, DiscriminationStrength::Weak);
    assert_eq!(f.scan_safety, ScanSafety::Unsafe);
    assert!(!f.in_recovery_context);
    assert!(!f.in_token_dispatch);
}
