use rustc_hash::FxHashMap;

use bbnf_ir::passes::lr::{
    eliminate_direct_lr, eliminate_indirect_lr, intern_string, starts_with_ref, strip_leading_ref,
    substitute_leading_ref,
};
use bbnf_ir::{AltBranch, GrammarIR, IrNode, IrRule, RuleId, RuleMeta};

/// Build a minimal GrammarIR for testing.
fn make_ir(rules: Vec<(&str, IrNode, Option<u32>, bool)>) -> GrammarIR {
    let mut strings = Vec::new();
    let mut string_dedup: FxHashMap<String, u32> = FxHashMap::default();
    let mut ir_rules = Vec::new();

    for (i, (name, body, scc_id, is_cyclic)) in rules.into_iter().enumerate() {
        let name_id = intern_string(name.to_string(), &mut strings, &mut string_dedup);
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
        follow_sets: std::collections::HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: Vec::new(),
        type_map: None,
        pattern_annotations: std::collections::HashMap::new(),
        regex_info: std::collections::HashMap::new(),
        node_facts: std::collections::HashMap::new(),
        recognizer_decisions: std::collections::HashMap::new(),
        delim_scan_configs: std::collections::HashMap::new(),
        key_dispatch_configs: std::collections::HashMap::new(),
        context_facts: std::collections::HashMap::new(),
        has_family_recognizers: false,
        regex_engine_decisions: std::collections::HashMap::new(),
        dag: None, cost_config: bbnf_ir::CostConfig::default(),
    }
}

fn lit(s: &str, ir: &mut GrammarIR) -> IrNode {
    let sid = ir.strings.len() as u32;
    ir.strings.push(s.to_string());
    IrNode::Literal(sid)
}

fn ref_node(id: RuleId) -> IrNode {
    IrNode::Ref(id)
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

// ── Direct LR Tests ────────────────────────────────────────────────

#[test]
fn direct_lr_eliminated() {
    // A = A "+" "x" | "x"
    // Rule A has id=0.
    let mut ir = make_ir(vec![
        ("A", IrNode::Epsilon, None, false), // placeholder
    ]);

    let plus = lit("+", &mut ir);
    let x1 = lit("x", &mut ir);
    let x2 = lit("x", &mut ir);

    ir.rules[0].body = alt(vec![seq(vec![ref_node(0), plus, x1]), x2]);

    eliminate_direct_lr(&mut ir);

    // Should produce A and A_tail rules.
    assert_eq!(ir.rules.len(), 2, "Expected 2 rules (A + A_tail)");
    assert_eq!(
        ir.strings[ir.rules[1].name as usize], "A_tail",
        "Second rule should be A_tail"
    );

    // A_tail should reference itself recursively and have an epsilon branch.
    let tail_body = &ir.rules[1].body;
    if let IrNode::Alt(branches, _) = tail_body {
        assert_eq!(
            branches.len(),
            2,
            "A_tail should have 2 branches (alpha + epsilon)"
        );
        // Last branch should be Epsilon.
        assert!(
            matches!(&branches.last().unwrap().node, IrNode::Epsilon),
            "Last branch of A_tail should be Epsilon"
        );
    } else {
        panic!("Expected A_tail body to be Alt, got {:?}", tail_body);
    }
}

#[test]
fn no_lr_unchanged() {
    let mut ir = make_ir(vec![("A", IrNode::Epsilon, None, false)]);

    let x = lit("x", &mut ir);
    let y = lit("y", &mut ir);
    ir.rules[0].body = alt(vec![x, y]);

    eliminate_direct_lr(&mut ir);

    // No tail rule should be created.
    assert_eq!(ir.rules.len(), 1, "Expected 1 rule (no LR to eliminate)");
}

#[test]
fn non_alt_body_unchanged() {
    let mut ir = make_ir(vec![("A", IrNode::Epsilon, None, false)]);

    let x = lit("x", &mut ir);
    ir.rules[0].body = x;

    eliminate_direct_lr(&mut ir);
    assert_eq!(ir.rules.len(), 1);
}

// ── Indirect LR Tests ──────────────────────────────────────────────

#[test]
fn indirect_lr_substituted() {
    // A = B "x"          (id=0, scc=0)
    // B = A "y" | "z"    (id=1, scc=0)
    // SCC: {A, B} — A starts with B, B starts with A (indirect cycle).
    let mut ir = make_ir(vec![
        ("A", IrNode::Epsilon, Some(0), true),
        ("B", IrNode::Epsilon, Some(0), true),
    ]);

    let x1 = lit("x", &mut ir);
    let y1 = lit("y", &mut ir);
    let z1 = lit("z", &mut ir);

    ir.rules[0].body = seq(vec![ref_node(1), x1]); // A = B "x"
    ir.rules[1].body = alt(vec![
        seq(vec![ref_node(0), y1]), // A "y"
        z1,                         // "z"
    ]);

    eliminate_indirect_lr(&mut ir);

    // B should now have A's body substituted for leading Ref(A).
    // B was: A "y" | "z"  ->  (B "x") "y" | "z"
    let b_body = &ir.rules[1].body;
    if let IrNode::Alt(branches, _) = b_body {
        assert_eq!(branches.len(), 2, "Expected 2 branches after substitution");
        let first = &branches[0].node;
        // First branch should be Seq containing A's body (B "x") followed by "y".
        if let IrNode::Seq(elems) = first {
            // The substitution of A = (B "x") into (A "y") gives Seq([B, "x", "y"])
            // or Seq([Seq([B, "x"]), "y"]).
            // Our implementation creates Seq([Seq([B, "x"]), "y"]).
            assert!(
                elems.len() >= 2,
                "Expected at least 2 elements in substituted seq"
            );
            // The first element should contain a reference to B (rule 1).
            let has_b_ref = contains_ref(&elems[0], 1);
            assert!(
                has_b_ref,
                "Expected substituted branch to reference B (rule 1)"
            );
        } else {
            panic!("Expected Seq after substitution, got {:?}", first);
        }
    } else {
        panic!("Expected Alt after substitution, got {:?}", b_body);
    }
}

#[test]
fn indirect_lr_no_multi_sccs_is_noop() {
    // Single-member SCCs should not trigger indirect LR elimination.
    let mut ir = make_ir(vec![("A", IrNode::Epsilon, Some(0), true)]);

    let x = lit("x", &mut ir);
    ir.rules[0].body = alt(vec![seq(vec![ref_node(0), x])]);

    let body_before = ir.rules[0].body.clone();
    eliminate_indirect_lr(&mut ir);
    assert_eq!(
        ir.rules[0].body, body_before,
        "Single-member SCC should be unchanged"
    );
}

/// Check if an IrNode tree contains a Ref to the given rule id.
fn contains_ref(node: &IrNode, target: RuleId) -> bool {
    match node {
        IrNode::Ref(id) => *id == target,
        IrNode::Seq(children) => children.iter().any(|c| contains_ref(c, target)),
        IrNode::Alt(branches, _) => branches.iter().any(|b| contains_ref(&b.node, target)),
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => contains_ref(inner, target),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            contains_ref(a, target) || contains_ref(b, target)
        }
        _ => false,
    }
}

// ── Helper Tests ───────────────────────────────────────────────────

#[test]
fn starts_with_ref_checks() {
    assert!(starts_with_ref(&IrNode::Ref(0), 0));
    assert!(!starts_with_ref(&IrNode::Ref(1), 0));
    assert!(starts_with_ref(
        &IrNode::Seq(vec![IrNode::Ref(0), IrNode::Epsilon]),
        0
    ));
    assert!(!starts_with_ref(
        &IrNode::Seq(vec![IrNode::Epsilon, IrNode::Ref(0)]),
        0
    ));
}

#[test]
fn strip_leading_ref_checks() {
    // Ref(0) -> Epsilon
    assert_eq!(strip_leading_ref(&IrNode::Ref(0), 0), Some(IrNode::Epsilon));

    // Ref(1) with target 0 -> None
    assert_eq!(strip_leading_ref(&IrNode::Ref(1), 0), None);

    // Seq([Ref(0), Lit]) -> Lit
    let lit = IrNode::Literal(42);
    let node = IrNode::Seq(vec![IrNode::Ref(0), lit.clone()]);
    assert_eq!(strip_leading_ref(&node, 0), Some(lit));

    // Seq([Ref(0), Lit, Lit]) -> Seq([Lit, Lit])
    let node = IrNode::Seq(vec![IrNode::Ref(0), IrNode::Literal(1), IrNode::Literal(2)]);
    assert_eq!(
        strip_leading_ref(&node, 0),
        Some(IrNode::Seq(vec![IrNode::Literal(1), IrNode::Literal(2)]))
    );
}

#[test]
fn substitute_single_ref() {
    let node = IrNode::Ref(0);
    let replacement = IrNode::Literal(42);
    assert_eq!(
        substitute_leading_ref(&node, 0, &replacement),
        Some(IrNode::Literal(42))
    );
}

#[test]
fn substitute_seq_leading() {
    let node = IrNode::Seq(vec![IrNode::Ref(0), IrNode::Literal(1)]);
    let replacement = IrNode::Literal(42);
    let result = substitute_leading_ref(&node, 0, &replacement);
    assert_eq!(
        result,
        Some(IrNode::Seq(vec![IrNode::Literal(42), IrNode::Literal(1)]))
    );
}

#[test]
fn substitute_with_alt_replacement() {
    // Seq([Ref(0), Lit(1)]) with replacement Alt([Lit(2), Lit(3)])
    // -> Alt([Seq([Lit(2), Lit(1)]), Seq([Lit(3), Lit(1)])])
    let node = IrNode::Seq(vec![IrNode::Ref(0), IrNode::Literal(1)]);
    let replacement = IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Literal(2),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Literal(3),
                first_set: None,
            },
        ],
        None,
    );
    let result = substitute_leading_ref(&node, 0, &replacement);
    if let Some(IrNode::Alt(branches, _)) = result {
        assert_eq!(branches.len(), 2);
    } else {
        panic!("Expected Alt result, got {:?}", result);
    }
}

#[test]
fn substitute_no_match() {
    let node = IrNode::Seq(vec![IrNode::Ref(1), IrNode::Literal(1)]);
    assert!(substitute_leading_ref(&node, 0, &IrNode::Literal(42)).is_none());
}
