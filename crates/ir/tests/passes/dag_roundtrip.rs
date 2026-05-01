//! DAG round-trip tests.
//!
//! Building a DAG from an IR and extracting back should produce
//! a semantically equivalent tree (modulo structural sharing).

use std::collections::HashMap;

use bbnf_ir::dag::{DagNode, GrammarDag, NodeId};
use bbnf_ir::{AltBranch, GrammarIR, IrNode, IrRule, RuleId, RuleMeta};

fn make_ir(rules: Vec<(&str, IrNode)>) -> GrammarIR {
    let mut ir_rules = Vec::new();
    let mut strings = Vec::new();
    for (i, (name, body)) in rules.into_iter().enumerate() {
        let name_id = strings.len() as u32;
        strings.push(name.to_string());
        ir_rules.push(IrRule {
            id: i as RuleId,
            name: name_id,
            body,
            meta: RuleMeta::default(),
            source_span: None,
        });
    }
    let entry = ir_rules.last().map(|r| r.id).unwrap_or(0);
    GrammarIR {
        rules: ir_rules,
        entry,
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
        dag: None,
        cost_config: bbnf_ir::CostConfig::default(),
        type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: std::collections::HashMap::new(),
        string_index: std::collections::HashMap::new(),
        payload_layouts: std::collections::HashMap::new(),
        structural_alphabet: None,
        push_fingerprint: None,
        dedup_eligible_rules: Vec::new(),

        shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(
        ),
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
fn roundtrip_literal() {
    let mut ir = make_ir(vec![]);
    let body = lit(&mut ir, "hello");
    ir.rules.push(IrRule {
        id: 0,
        name: 0,
        body: body.clone(),
        meta: RuleMeta::default(),
        source_span: None,
    });

    let dag = GrammarDag::from_ir(&ir);
    let root = dag.rule_root(0).expect("rule 0 should have a root");
    let extracted = dag.extract(root);
    assert_eq!(extracted, body);
}

#[test]
fn roundtrip_seq() {
    let mut ir = make_ir(vec![]);
    let a = lit(&mut ir, "a");
    let b = lit(&mut ir, "b");
    let c = lit(&mut ir, "c");
    let body = IrNode::Seq(vec![a, b, c]);
    ir.rules.push(IrRule {
        id: 0,
        name: 0,
        body: body.clone(),
        meta: RuleMeta::default(),
        source_span: None,
    });

    let dag = GrammarDag::from_ir(&ir);
    let root = dag.rule_root(0).unwrap();
    assert_eq!(dag.extract(root), body);
}

#[test]
fn roundtrip_alt() {
    let mut ir = make_ir(vec![]);
    let a = lit(&mut ir, "a");
    let b = lit(&mut ir, "b");
    let body = IrNode::Alt(
        vec![
            AltBranch {
                node: a,
                first_set: None,
            },
            AltBranch {
                node: b,
                first_set: None,
            },
        ],
        None,
    );
    ir.rules.push(IrRule {
        id: 0,
        name: 0,
        body: body.clone(),
        meta: RuleMeta::default(),
        source_span: None,
    });

    let dag = GrammarDag::from_ir(&ir);
    let root = dag.rule_root(0).unwrap();
    // Extraction produces an Alt with equal branches (modulo first_set
    // which is cleared — DAG doesn't carry per-branch metadata).
    match dag.extract(root) {
        IrNode::Alt(branches, dispatch) => {
            assert_eq!(branches.len(), 2);
            assert!(dispatch.is_none());
        }
        other => panic!("expected Alt, got {:?}", other),
    }
}

#[test]
fn interning_dedupes_identical_literals() {
    let mut ir = make_ir(vec![]);
    // Two rules that both reference Literal("x") by the SAME string id.
    let sid = ir.strings.len() as u32;
    ir.strings.push("x".to_string());
    let x1 = IrNode::Literal(sid);
    let x2 = IrNode::Literal(sid);
    ir.rules.push(IrRule {
        id: 0,
        name: 0,
        body: x1,
        meta: RuleMeta::default(),
        source_span: None,
    });
    ir.rules.push(IrRule {
        id: 1,
        name: 0,
        body: x2,
        meta: RuleMeta::default(),
        source_span: None,
    });

    let dag = GrammarDag::from_ir(&ir);
    // Both rules should point to the same DAG node.
    let root0 = dag.rule_root(0).unwrap();
    let root1 = dag.rule_root(1).unwrap();
    assert_eq!(root0, root1);
    // And the DAG should have only one node (the shared literal).
    assert_eq!(dag.node_count(), 1);
}

#[test]
fn interning_shares_subexpressions() {
    let mut ir = make_ir(vec![]);
    let sid = ir.strings.len() as u32;
    ir.strings.push("x".to_string());
    // Rule 0: Seq(Literal("x"), Literal("x"))
    let seq0 = IrNode::Seq(vec![IrNode::Literal(sid), IrNode::Literal(sid)]);
    // Rule 1: Literal("x")
    let lit1 = IrNode::Literal(sid);
    ir.rules.push(IrRule {
        id: 0,
        name: 0,
        body: seq0,
        meta: RuleMeta::default(),
        source_span: None,
    });
    ir.rules.push(IrRule {
        id: 1,
        name: 0,
        body: lit1,
        meta: RuleMeta::default(),
        source_span: None,
    });

    let dag = GrammarDag::from_ir(&ir);
    // Literal("x") should be shared between the two Seq children and
    // rule 1's body — exactly 2 DAG nodes (Literal + Seq).
    assert_eq!(dag.node_count(), 2);

    // Rule 1 should point at the shared literal.
    let root1 = dag.rule_root(1).unwrap();
    assert!(matches!(dag.node(root1), DagNode::Literal(_)));
}

#[test]
fn child_iter_visits_all_children() {
    use bbnf_ir::StringId;
    let a = DagNode::Literal(0 as StringId);
    let b = DagNode::Literal(1 as StringId);
    let _ = (a, b);
    let seq = DagNode::Seq(vec![NodeId::new(0), NodeId::new(1), NodeId::new(2)]);
    let children: Vec<NodeId> = seq.children().collect();
    assert_eq!(
        children,
        vec![NodeId::new(0), NodeId::new(1), NodeId::new(2)]
    );
}
