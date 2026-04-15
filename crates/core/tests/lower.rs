use std::collections::HashMap;

use bbnf::graph::{calculate_ast_deps, tarjan_scc};
use bbnf::lower::{DirectiveSet, lower_to_ir};

use bbnf_ir::{GrammarIR, IrNode, MemoStrategy};

fn lower_grammar(source: &str) -> GrammarIR {
    let ast = bbnf::grammar::parse(source)
        .expect("failed to parse grammar")
        .rules;

    let deps = calculate_ast_deps(&ast);
    let scc_result = tarjan_scc(&deps);

    let directives = DirectiveSet::empty();

    let mut ir = lower_to_ir(&ast, &scc_result, &directives, &[]);

    // Compute FIRST sets at IR level.
    bbnf_ir::passes::compute_first_sets(&mut ir);

    // SCC pass populates RuleMeta.{is_cyclic, scc_id, memo} at IR level.
    // Must run before `compute_transparent`, which gates transparency on
    // `is_cyclic` — the AST-level `tarjan_scc` above seeds `lower_to_ir`'s
    // node-id memo hints, but the IR pass is authoritative for RuleMeta.
    bbnf_ir::passes::compute_scc(&mut ir);

    // Run metadata IR passes (alias + transparent + span eligibility detection).
    bbnf_ir::passes::compute_aliases(&mut ir);
    bbnf_ir::passes::compute_transparent(&mut ir);
    bbnf_ir::passes::refine_span_eligibility(&mut ir);

    ir
}

#[test]
fn lower_simple_literal() {
    let ir = lower_grammar(r#"hello = "world" ;"#);
    assert_eq!(ir.rules.len(), 1);
    assert_eq!(ir.get_string(ir.rules[0].name), "hello");
    assert!(matches!(ir.rules[0].body, IrNode::Literal(_)));
}

#[test]
fn lower_alternation() {
    let ir = lower_grammar(r#"value = "true" | "false" | "null" ;"#);
    assert_eq!(ir.rules.len(), 1);
    match &ir.rules[0].body {
        IrNode::Alt(branches, _) => assert_eq!(branches.len(), 3),
        other => panic!("expected Alt, got {:?}", other),
    }
}

#[test]
fn lower_concatenation() {
    let ir = lower_grammar(r#"pair = "key" , ":" , "value" ;"#);
    assert_eq!(ir.rules.len(), 1);
    match &ir.rules[0].body {
        IrNode::Seq(children) => assert_eq!(children.len(), 3),
        other => panic!("expected Seq, got {:?}", other),
    }
}

#[test]
fn lower_repetition() {
    let ir = lower_grammar(r#"items = "x" * ;"#);
    assert_eq!(ir.rules.len(), 1);
    match &ir.rules[0].body {
        IrNode::Repeat { lo: 0, hi, .. } => assert_eq!(*hi, u32::MAX),
        other => panic!("expected Repeat(0, MAX), got {:?}", other),
    }
}

#[test]
fn lower_nonterminal_ref() {
    let ir = lower_grammar(
        r#"
        start = item ;
        item = "x" ;
    "#,
    );
    assert_eq!(ir.rules.len(), 2);
    // start should reference item via Ref(1)
    match &ir.rules[0].body {
        IrNode::Ref(id) => assert_eq!(*id, 1),
        other => panic!("expected Ref, got {:?}", other),
    }
}

#[test]
fn lower_skip_next() {
    let ir = lower_grammar(r#"a = "x" << "y" ;"#);
    assert_eq!(ir.rules.len(), 1);
    assert!(matches!(ir.rules[0].body, IrNode::Skip(_, _)));

    let ir = lower_grammar(r#"a = "x" >> "y" ;"#);
    assert_eq!(ir.rules.len(), 1);
    assert!(matches!(ir.rules[0].body, IrNode::Next(_, _)));
}

#[test]
fn lower_optional() {
    let ir = lower_grammar(r#"a = "x" ? ;"#);
    assert_eq!(ir.rules.len(), 1);
    match &ir.rules[0].body {
        IrNode::Repeat { lo: 0, hi: 1, .. } => {}
        other => panic!("expected Repeat(0, 1), got {:?}", other),
    }
}

#[test]
fn lower_regex() {
    let ir = lower_grammar(r#"num = /[0-9]+/ ;"#);
    assert_eq!(ir.rules.len(), 1);
    match &ir.rules[0].body {
        IrNode::Regex(id) => {
            assert_eq!(ir.get_string(*id), "[0-9]+");
        }
        other => panic!("expected Regex, got {:?}", other),
    }
}

#[test]
fn lower_cyclic_rule_gets_memo() {
    let ir = lower_grammar(
        r#"
        expr = expr , "+" , term | term ;
        term = "x" ;
    "#,
    );
    // expr is cyclic -> should get Full memoization.
    let expr_rule = ir.find_rule("expr").unwrap();
    assert_eq!(expr_rule.meta.memo, MemoStrategy::Full);
    assert!(expr_rule.meta.is_cyclic);

    // term is acyclic -> should get None memoization.
    let term_rule = ir.find_rule("term").unwrap();
    assert_eq!(term_rule.meta.memo, MemoStrategy::None);
    assert!(!term_rule.meta.is_cyclic);
}

#[test]
fn lower_span_eligible() {
    let ir = lower_grammar(r#"ident = /[a-zA-Z_][a-zA-Z0-9_]*/ ;"#);
    let rule = &ir.rules[0];
    assert!(rule.meta.span_eligible);
}

#[test]
fn msgpack_roundtrip() {
    let ir = lower_grammar(
        r#"
        value = "true" | "false" | /[0-9]+/ ;
    "#,
    );

    let bytes = ir.to_msgpack().expect("serialize failed");
    let roundtripped = GrammarIR::from_msgpack(&bytes).expect("deserialize failed");

    assert_eq!(ir.rules.len(), roundtripped.rules.len());
    assert_eq!(ir.strings, roundtripped.strings);
    assert_eq!(ir.rules[0].body, roundtripped.rules[0].body);
}

#[test]
fn json_roundtrip() {
    let ir = lower_grammar(r#"hello = "world" ;"#);

    let json = ir.to_json().expect("json serialize failed");
    let roundtripped = GrammarIR::from_json(&json).expect("json deserialize failed");

    assert_eq!(ir.rules.len(), roundtripped.rules.len());
    assert_eq!(ir.strings, roundtripped.strings);
}

#[test]
fn lower_json_grammar() {
    let source = r#"
        null = "null" ;
        bool = "true" | "false" ;
        number = /-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/ ;
        comma = "," ?w ;
        colon = ":" ?w ;
        string = /"(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*"/ ;
        array = "[" >> (( value << comma ? ) *)?w << "]" ;
        pair = string, colon >> value ;
        object = "{" >> (( pair << comma ? ) *)?w << "}" ;
        value = object | array | string | number | bool | null ;
    "#;
    let ir = lower_grammar(source);

    // All 10 rules: null, bool, number, comma, colon, string, array, pair, object, value.
    assert_eq!(ir.rules.len(), 10);

    // Check rule names are all present.
    let rule_names: Vec<&str> = ir.rules.iter().map(|r| ir.get_string(r.name)).collect();
    for expected in &[
        "null", "bool", "number", "comma", "colon", "string", "array", "pair", "object", "value",
    ] {
        assert!(rule_names.contains(expected), "missing rule: {}", expected);
    }

    // value is cyclic (references array/object which reference value).
    let value_rule = ir.find_rule("value").unwrap();
    assert!(value_rule.meta.is_cyclic);
    assert_eq!(value_rule.meta.memo, MemoStrategy::Full);

    // value should be a transparent alternation of nonterminals.
    assert!(value_rule.meta.is_transparent);

    // null, bool, number, string should be span-eligible.
    assert!(ir.find_rule("null").unwrap().meta.span_eligible);
    assert!(ir.find_rule("number").unwrap().meta.span_eligible);
    assert!(ir.find_rule("string").unwrap().meta.span_eligible);

    // MessagePack roundtrip.
    let bytes = ir.to_msgpack().unwrap();
    let rt = GrammarIR::from_msgpack(&bytes).unwrap();
    assert_eq!(ir.rules.len(), rt.rules.len());

    // Check that all rule bodies survived the roundtrip.
    for (orig, rt_rule) in ir.rules.iter().zip(rt.rules.iter()) {
        assert_eq!(orig.body, rt_rule.body);
    }
}

#[test]
fn lower_with_pretty_hints() {
    let ir = {
        let source = r#"items = "x" * ;"#;
        let ast = bbnf::grammar::parse(source)
            .expect("failed to parse grammar")
            .rules;

        let deps = calculate_ast_deps(&ast);
        let scc_result = tarjan_scc(&deps);

        let mut pretties = HashMap::new();
        pretties.insert(
            "items".to_string(),
            vec![
                "group".to_string(),
                "indent".to_string(),
                r#"sep(", ")"#.to_string(),
            ],
        );

        let directives = DirectiveSet {
            recovers: None,
            pretties: Some(&pretties),
            ws_pattern: None,
            token_rules: None,
            debug_rules: None,
            debug_all: false,
            host_fns: None,
        };

        lower_to_ir(&ast, &scc_result, &directives, &[])
    };

    let rule = &ir.rules[0];
    let hints = rule.meta.directives.pretty.as_ref().unwrap();
    assert!(hints.group);
    assert!(hints.indent);
    assert_eq!(hints.sep.as_deref(), Some(", "));
    assert!(!hints.block);
}
