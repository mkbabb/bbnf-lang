//! Verify CSP type solver produces correct type inference.

use bbnf_ir::passes::types::generate::generate_constraints;
use bbnf_ir::*;

fn make_ir(rules: Vec<IrRule>, fns: Vec<FnDescriptor>) -> GrammarIR {
    let entry = rules.last().map(|r| r.id).unwrap_or(0);
    GrammarIR {
        rules,
        entry,
        strings: vec![
            "rule0".into(), "rule1".into(), "rule2".into(),
            "hello".into(), "world".into(), "[a-z]+".into(),
            "[0-9]+".into(), "f64".into(), "a".into(), "b".into(),
            "x".into(),
        ],
        fns,
        types: vec![],
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
        dag: None, cost_config: bbnf_ir::CostConfig::default(), type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: std::collections::HashMap::new(),
        string_index: std::collections::HashMap::new(),
        payload_layouts: std::collections::HashMap::new(),
        follow_sets: std::collections::HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: vec![],
        struct_registry: Default::default(),
    }
}

fn rule(id: RuleId, body: IrNode) -> IrRule {
    IrRule {
        id,
        name: id as StringId, // use id as string index
        body,
        meta: RuleMeta::default(),
        source_span: None,
    }
}

fn solve_rule_type(ir: &GrammarIR, rule_id: RuleId) -> TypeDesc {
    // Tests build a raw GrammarIR without a DAG; `generate_constraints`
    // (like `project_types`) requires one, so we build it here.
    let mut ir_owned = ir.clone();
    bbnf_ir::dag::ensure_dag(&mut ir_owned);
    let mut system = generate_constraints(&ir_owned);
    let _ = system.csp.propagate();
    let var = system.rule_vars[&rule_id];
    system.csp.variables[var as usize]
        .domain
        .solved
        .clone()
        .expect("Rule type should be solved")
}

#[test]
fn csp_seq_of_spans_compresses() {
    // value = "hello" , /[a-z]+/ → Seq(Span, Span) → Span (compressed)
    let ir = make_ir(
        vec![rule(0, IrNode::Seq(vec![IrNode::Literal(3), IrNode::Regex(5)]))],
        vec![],
    );
    assert_eq!(solve_rule_type(&ir, 0), TypeDesc::Span);
}

#[test]
fn csp_alt_homogeneous_spans() {
    // value = "a" | "b" → Alt(Span, Span) → Span
    let ir = make_ir(
        vec![rule(
            0,
            IrNode::Alt(
                vec![
                    AltBranch { node: IrNode::Literal(8), first_set: None },
                    AltBranch { node: IrNode::Literal(9), first_set: None },
                ],
                None,
            ),
        )],
        vec![],
    );
    assert_eq!(solve_rule_type(&ir, 0), TypeDesc::Span);
}

#[test]
fn csp_optional_span_collapses() {
    // opt = "x"? → Optional(Span) → Span
    let ir = make_ir(
        vec![rule(
            0,
            IrNode::Repeat {
                inner: Box::new(IrNode::Literal(10)),
                lo: 0,
                hi: 1,
            },
        )],
        vec![],
    );
    assert_eq!(solve_rule_type(&ir, 0), TypeDesc::Span);
}

#[test]
fn csp_repeat_boxedenum() {
    // other = "a" "b" "[0-9]+"
    // items = other+ → Vec(BoxedEnum)
    let ir = make_ir(
        vec![
            rule(
                0,
                IrNode::Seq(vec![
                    IrNode::Literal(8),
                    IrNode::Regex(6),
                    IrNode::Literal(9),
                ]),
            ),
            rule(
                1,
                IrNode::Repeat {
                    inner: Box::new(IrNode::Ref(0)),
                    lo: 1,
                    hi: u32::MAX,
                },
            ),
        ],
        vec![],
    );
    // In Vec context, Ref produces Enum (Vec provides heap indirection,
    // so Box is unnecessary). The old CSP test expected BoxedEnum which was
    // the incorrect pre-vec-context behavior.
    assert_eq!(
        solve_rule_type(&ir, 1),
        TypeDesc::Vec(Box::new(TypeDesc::Enum))
    );
}

#[test]
fn csp_map_overrides_to_named() {
    // number = /[0-9]+/ -> f64
    let ir = make_ir(
        vec![rule(
            0,
            IrNode::Map {
                inner: Box::new(IrNode::Regex(6)),
                fn_id: 0,
            },
        )],
        vec![FnDescriptor::Expr {
            expr: MapExpr::Input,
            return_type: Some(TypeDesc::Named(7)), // "f64"
        }],
    );
    assert_eq!(solve_rule_type(&ir, 0), TypeDesc::Named(7));
}

#[test]
fn csp_skip_keeps_left() {
    // value = "a" << "b" → Span (keeps left)
    let ir = make_ir(
        vec![rule(
            0,
            IrNode::Skip(
                Box::new(IrNode::Literal(8)),
                Box::new(IrNode::Literal(9)),
            ),
        )],
        vec![],
    );
    assert_eq!(solve_rule_type(&ir, 0), TypeDesc::Span);
}

#[test]
fn csp_next_keeps_right() {
    // value = "a" >> "b" → Span (keeps right)
    let ir = make_ir(
        vec![rule(
            0,
            IrNode::Next(
                Box::new(IrNode::Literal(8)),
                Box::new(IrNode::Literal(9)),
            ),
        )],
        vec![],
    );
    assert_eq!(solve_rule_type(&ir, 0), TypeDesc::Span);
}

#[test]
fn csp_number_convert() {
    // number = /[0-9]+/ -> f64 (specialized to NumberConvert)
    let ir = make_ir(
        vec![rule(
            0,
            IrNode::Map {
                inner: Box::new(IrNode::Regex(6)),
                fn_id: 0,
            },
        )],
        vec![FnDescriptor::NumberConvert],
    );
    assert_eq!(solve_rule_type(&ir, 0), TypeDesc::F64);
}
