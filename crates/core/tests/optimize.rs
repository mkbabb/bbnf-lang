//! Integration tests for left-recursion elimination (IR-level passes).
//!
//! These tests exercise the LR elimination through the full pipeline by
//! constructing a GrammarIR and running the passes directly.

use std::collections::HashMap;

use bbnf_ir::passes::{eliminate_direct_lr, eliminate_indirect_lr};
use bbnf_ir::{AltBranch, GrammarIR, IrNode, IrRule, RuleId, RuleMeta};

/// Build a minimal GrammarIR for testing.
fn make_ir(rules: Vec<(&str, IrNode, Option<u32>, bool)>) -> GrammarIR {
    let mut strings = Vec::new();
    let mut string_dedup: HashMap<String, u32> = HashMap::new();
    let mut ir_rules = Vec::new();

    for (i, (name, body, scc_id, is_cyclic)) in rules.into_iter().enumerate() {
        let name_id = intern(name, &mut strings, &mut string_dedup);
        ir_rules.push(IrRule {
            id: i as RuleId,
            name: name_id,
            body,
            meta: RuleMeta {
                scc_id,
                is_cyclic,
                ..Default::default()
            },
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
        payload_layouts: std::collections::HashMap::new(),
        string_index: Default::default(),
        structural_alphabet: None,
        push_fingerprint: None,
        eclass_facts: std::collections::HashMap::new(),
        keyword_branches: std::collections::HashMap::new(),
        disjoint_first_tables: std::collections::HashMap::new(),
        pattern_alphabets: std::collections::HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        struct_registry: bbnf_ir::StructRegistry::default(),
        type_obligations: Vec::new(),
        inline_trace: bbnf_ir::passes::inline_trace::InlineTrace::default(),
        path_check_resolver: bbnf_ir::passes::path_check::PathCheckResolver::default(),
        dedup_eligible_rules: Vec::new(),

        shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(
        ),
    }
}

fn intern(s: &str, strings: &mut Vec<String>, dedup: &mut HashMap<String, u32>) -> u32 {
    if let Some(&existing) = dedup.get(s) {
        return existing;
    }
    let sid = strings.len() as u32;
    dedup.insert(s.to_string(), sid);
    strings.push(s.to_string());
    sid
}

fn lit(s: &str, ir: &mut GrammarIR) -> IrNode {
    let sid = ir.strings.len() as u32;
    ir.strings.push(s.to_string());
    IrNode::Literal(sid)
}

fn alt(branches: Vec<IrNode>) -> IrNode {
    IrNode::Alt(
        branches
            .into_iter()
            .map(|node| AltBranch {
                node,
                first_set: None,
            })
            .collect(),
        None,
    )
}

fn seq(children: Vec<IrNode>) -> IrNode {
    IrNode::Seq(children)
}

#[test]
fn test_no_left_recursion() {
    let mut ir = make_ir(vec![("A", IrNode::Epsilon, None, false)]);
    let x = lit("x", &mut ir);
    ir.rules[0].body = x;

    eliminate_direct_lr(&mut ir);
    assert_eq!(ir.rules.len(), 1, "No tail rule should be created");
}

#[test]
fn test_direct_left_recursion() {
    // A = A "x" | "y"
    let mut ir = make_ir(vec![("A", IrNode::Epsilon, None, false)]);
    let x = lit("x", &mut ir);
    let y = lit("y", &mut ir);

    ir.rules[0].body = alt(vec![seq(vec![IrNode::Ref(0), x]), y]);

    eliminate_direct_lr(&mut ir);

    // Should have 2 rules: A and A_tail.
    assert_eq!(ir.rules.len(), 2);
    assert_eq!(
        ir.strings[ir.rules[1].name as usize], "A_tail",
        "Second rule should be A_tail"
    );
}

#[test]
fn test_indirect_then_direct() {
    // A = B "x"          (id=0, scc=0)
    // B = A "y" | "z"    (id=1, scc=0)
    let mut ir = make_ir(vec![
        ("A", IrNode::Epsilon, Some(0), true),
        ("B", IrNode::Epsilon, Some(0), true),
    ]);

    let x = lit("x", &mut ir);
    let y = lit("y", &mut ir);
    let z = lit("z", &mut ir);

    ir.rules[0].body = seq(vec![IrNode::Ref(1), x]);
    ir.rules[1].body = alt(vec![seq(vec![IrNode::Ref(0), y]), z]);

    // Run indirect first (Paull's), then direct.
    eliminate_indirect_lr(&mut ir);
    eliminate_direct_lr(&mut ir);

    // B should now be directly left-recursive (B starts with B after substitution),
    // and direct LR elimination should have created a B_tail rule.
    let has_tail = ir
        .rules
        .iter()
        .any(|r| ir.strings[r.name as usize] == "B_tail");
    assert!(
        has_tail,
        "Expected B_tail rule after indirect + direct LR elimination"
    );
}
