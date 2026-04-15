//! Tests for `SharedStrings` — the e-graph's shared, interior-mutable
//! string interner that replaces direct `ir.strings` mutation in
//! rewrite rules.

use std::collections::HashMap;

use bbnf_ir::egraph::SharedStrings;
use bbnf_ir::{GrammarIR, IrNode, IrRule, RuleMeta};

fn empty_ir(strings: Vec<&str>) -> GrammarIR {
    GrammarIR {
        entry: 0,
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Epsilon,
            meta: RuleMeta::default(),
            source_span: None,
        }],
        strings: strings.into_iter().map(String::from).collect(),
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: vec![],
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
        struct_registry: Default::default(),
        structural_alphabet: None,
    }
}

#[test]
fn intern_dedups_existing() {
    let ir = empty_ir(vec!["a", "b", "c"]);
    let pool = SharedStrings::from_ir(&ir);
    assert_eq!(pool.intern("a"), 0);
    assert_eq!(pool.intern("b"), 1);
    assert_eq!(pool.intern("c"), 2);
    assert_eq!(pool.len(), 3);
}

#[test]
fn intern_allocates_new() {
    let ir = empty_ir(vec!["a"]);
    let pool = SharedStrings::from_ir(&ir);
    assert_eq!(pool.intern("a"), 0);
    assert_eq!(pool.intern("ab"), 1);
    assert_eq!(pool.intern("ab"), 1); // redup
    assert_eq!(pool.len(), 2);
}

#[test]
fn sharing_across_clones() {
    let ir = empty_ir(vec!["x"]);
    let p1 = SharedStrings::from_ir(&ir);
    let p2 = p1.clone();

    let id = p1.intern("new");
    // p2 sees the same insertion.
    assert_eq!(p2.intern("new"), id);
    assert_eq!(p2.len(), 2);
}

#[test]
fn write_back_updates_ir() {
    let mut ir = empty_ir(vec!["a"]);
    let pool = SharedStrings::from_ir(&ir);
    pool.intern("b");
    pool.intern("c");
    pool.write_back(&mut ir);
    assert_eq!(
        ir.strings,
        vec!["a".to_string(), "b".to_string(), "c".to_string()]
    );
}

#[test]
fn into_vec_clones_when_shared() {
    let ir = empty_ir(vec!["a"]);
    let p1 = SharedStrings::from_ir(&ir);
    let _p2 = p1.clone(); // keeps the Rc alive

    p1.intern("b");
    // _p2 still has a reference — into_vec must clone rather than panic.
    let strings = _p2.into_vec();
    assert_eq!(strings, vec!["a".to_string(), "b".to_string()]);
}
