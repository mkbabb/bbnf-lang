//! Roundtrip test: build_and_saturate + write_back_optimized
//! on a mutually-recursive grammar should preserve rule boundaries
//! and not produce any self-referential or corrupted rule bodies.

use std::collections::HashMap;

use bbnf_ir::egraph::{build_and_saturate, write_back_optimized, GrammarCostModel};
use bbnf_ir::{AltBranch, GrammarIR, IrNode, IrRule, RuleMeta};

fn empty_ir() -> GrammarIR {
    GrammarIR {
        entry: 0,
        rules: vec![],
        strings: vec![],
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
    }
}

fn intern(ir: &mut GrammarIR, s: &str) -> u32 {
    let sid = ir.strings.len() as u32;
    ir.strings.push(s.to_string());
    sid
}

fn push_rule(ir: &mut GrammarIR, name: &str, body: IrNode) -> u32 {
    let id = ir.rules.len() as u32;
    let name_sid = intern(ir, name);
    ir.rules.push(IrRule {
        id,
        name: name_sid,
        body,
        meta: RuleMeta::default(),
        source_span: None,
    });
    id
}

fn roundtrip(mut ir: GrammarIR) -> GrammarIR {
    let (egraph, pool, rule_body_ids) = build_and_saturate(&ir);
    let cost = GrammarCostModel::default();
    write_back_optimized(&egraph, &mut ir, &rule_body_ids, &cost);
    pool.write_back(&mut ir);
    ir
}

#[test]
fn roundtrip_json_like_grammar() {
    // Build a JSON-like grammar and verify roundtrip preserves
    // structure without any self-referential rule bodies.
    let mut ir = empty_ir();

    // rule 0: null = "null"
    let null_lit = intern(&mut ir, "null");
    push_rule(&mut ir, "null", IrNode::Literal(null_lit));

    // rule 1: bool = "true" | "false"
    let true_lit = intern(&mut ir, "true");
    let false_lit = intern(&mut ir, "false");
    push_rule(
        &mut ir,
        "bool",
        IrNode::Alt(
            vec![
                AltBranch { node: IrNode::Literal(true_lit), first_set: None },
                AltBranch { node: IrNode::Literal(false_lit), first_set: None },
            ],
            None,
        ),
    );

    // rule 2: number = /[0-9]+/
    let number_pat = intern(&mut ir, "[0-9]+");
    push_rule(&mut ir, "number", IrNode::Regex(number_pat));

    // rule 3: string = /"[^"]*"/
    let string_pat = intern(&mut ir, "\"[^\"]*\"");
    push_rule(&mut ir, "string", IrNode::Regex(string_pat));

    // rule 4: array = "[" value "]"  (simplified, no repetition)
    let lb = intern(&mut ir, "[");
    let rb = intern(&mut ir, "]");
    push_rule(
        &mut ir,
        "array",
        IrNode::Seq(vec![IrNode::Literal(lb), IrNode::Ref(6), IrNode::Literal(rb)]),
    );

    // rule 5: object = "{" string ":" value "}"
    let lc = intern(&mut ir, "{");
    let rc = intern(&mut ir, "}");
    let colon = intern(&mut ir, ":");
    push_rule(
        &mut ir,
        "object",
        IrNode::Seq(vec![
            IrNode::Literal(lc),
            IrNode::Ref(3), // string
            IrNode::Literal(colon),
            IrNode::Ref(6), // value
            IrNode::Literal(rc),
        ]),
    );

    // rule 6: value = null | bool | number | string | array | object
    push_rule(
        &mut ir,
        "value",
        IrNode::Alt(
            vec![
                AltBranch { node: IrNode::Ref(0), first_set: None },
                AltBranch { node: IrNode::Ref(1), first_set: None },
                AltBranch { node: IrNode::Ref(2), first_set: None },
                AltBranch { node: IrNode::Ref(3), first_set: None },
                AltBranch { node: IrNode::Ref(4), first_set: None },
                AltBranch { node: IrNode::Ref(5), first_set: None },
            ],
            None,
        ),
    );
    ir.entry = 6;

    let before_rule_count = ir.rules.len();
    let after = roundtrip(ir);

    // Invariants.
    assert_eq!(after.rules.len(), before_rule_count, "rule count changed");

    for rule in &after.rules {
        // No self-referential root body: `rule_x = Ref(x)`.
        if let IrNode::Ref(target) = &rule.body {
            assert_ne!(
                *target, rule.id,
                "rule {} collapsed to Ref(self): body={:?}",
                rule.id, rule.body
            );
        }
    }

    // value rule (id 6) should still be an Alt.
    let value_body = &after
        .rules
        .iter()
        .find(|r| r.id == 6)
        .expect("value rule")
        .body;
    assert!(
        matches!(value_body, IrNode::Alt(..)),
        "value body expected Alt, got: {:?}",
        value_body
    );

    // object rule (id 5) should still be a Seq.
    let object_body = &after
        .rules
        .iter()
        .find(|r| r.id == 5)
        .expect("object rule")
        .body;
    assert!(
        matches!(object_body, IrNode::Seq(..)),
        "object body expected Seq, got: {:?}",
        object_body
    );

    // array rule (id 4) should still be a Seq.
    let array_body = &after
        .rules
        .iter()
        .find(|r| r.id == 4)
        .expect("array rule")
        .body;
    assert!(
        matches!(array_body, IrNode::Seq(..)),
        "array body expected Seq, got: {:?}",
        array_body
    );
}
