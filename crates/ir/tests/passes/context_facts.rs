//! Context facts propagation tests.
//!
//! Post-AF.1 the standalone `compute_context_facts` pass is gone; the
//! facts are produced by `ContextFactsMiner` during the unified
//! `mine_recognizers` walk and cached on `ir.context_facts`. These
//! tests run the full miner pass and then read the sidecar.

use std::collections::HashMap;

use bbnf_ir::dag::GrammarDag;
use bbnf_ir::passes::context::{DiscriminationStrength, ScanSafety};
use bbnf_ir::passes::mine_recognizers;
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
        delim_scan_configs: std::collections::HashMap::new(),
        key_dispatch_configs: std::collections::HashMap::new(),
        context_facts: std::collections::HashMap::new(),
        has_family_recognizers: false,
        regex_engine_decisions: std::collections::HashMap::new(),
        dag: None,
        cost_config: bbnf_ir::CostConfig::default(),
        type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: std::collections::HashMap::new(),
        string_index: std::collections::HashMap::new(),
        payload_layouts: std::collections::HashMap::new(),
    }
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

/// Build the DAG, run the full miner pass, and return the root
/// `NodeId` so the caller can read `ir.context_facts[root]`.
fn mine(ir: &mut GrammarIR) -> bbnf_ir::dag::NodeId {
    ir.dag = Some(GrammarDag::from_ir(ir));
    let root = ir.dag.as_ref().unwrap().rule_root(0).unwrap();
    mine_recognizers(ir);
    root
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

    let root = mine(&mut ir);
    let f = ir
        .context_facts
        .get(&root)
        .expect("root should have facts");
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

    let root = mine(&mut ir);
    let f = ir
        .context_facts
        .get(&root)
        .expect("root should have facts");
    assert_eq!(f.scan_safety, ScanSafety::Safe);
}

#[test]
fn bare_literal_has_default_facts() {
    let mut ir = base_ir();
    let body = lit(&mut ir, "hello");
    push_rule(&mut ir, body);

    let root = mine(&mut ir);
    let f = ir.context_facts.get(&root).cloned().unwrap_or_default();
    assert_eq!(f.discrimination, DiscriminationStrength::Weak);
    assert_eq!(f.scan_safety, ScanSafety::Unsafe);
    assert!(!f.in_recovery_context);
    assert!(!f.in_token_dispatch);
}
