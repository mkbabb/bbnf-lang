//! Tests for repeat/sep_by codegen (extracted from codegen/repeat.rs).

use std::collections::HashMap;

use bbnf_ir::{GrammarIR, IrNode, IrRule, RuleMeta, TypeDesc};

use bbnf::generate::codegen::generate_monolithic;
use bbnf::generate::codegen::ir_types::{IrCodegenCtx, ParserAttributes};

#[test]
fn sep_by_ws_until_uses_scratch_for_slab_mode() {
    let mut ir = GrammarIR {
        rules: vec![
            IrRule {
                id: 0,
                name: 0,
                // Body = Repeat(Ref(1)) → projects to Vec(Enum).
                body: IrNode::Repeat {
                    inner: Box::new(IrNode::Ref(1)),
                    lo: 0,
                    hi: u32::MAX,
                },
                meta: RuleMeta::default(),
                source_span: None,
            },
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Literal(2),
                meta: RuleMeta::default(),
                source_span: None,
            },
        ],
        entry: 0,
        strings: vec!["items".into(), "comma".into(), ",".into()],
        fns: vec![],
        types: vec![
            (0, TypeDesc::Vec(Box::new(TypeDesc::Enum))),
            (1, TypeDesc::Span),
        ],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: Vec::new(),
        type_map: None,
    };

    bbnf_ir::passes::project_types(&mut ir);

    let ident = quote::format_ident!("TestParser");
    let attrs = ParserAttributes::default();
    let ctx = IrCodegenCtx::new(&ir, &ident, &attrs, attrs.prettify);

    // Verify that scratch types were collected and alloc ctx generates correctly.
    assert!(
        !ctx.scratch_types.is_empty(),
        "scratch types should be collected"
    );
    let (struct_def, helper_fn) = ctx.generate_alloc_ctx();
    let struct_str = struct_def.to_string();
    let helper_str = helper_fn.to_string();
    assert!(
        struct_str.contains("__s0"),
        "slab ctx should have scratch field: {}",
        struct_str
    );
    assert!(
        helper_str.contains("__TestParserEnum"),
        "helper should reference ctx type: {}",
        helper_str
    );
}

#[test]
fn operator_chain_rules_use_specialized_chain_emission() {
    let mut ir = GrammarIR {
        rules: vec![
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Seq(vec![
                    IrNode::OptionalWhitespace(Box::new(IrNode::Ref(1))),
                    IrNode::Repeat {
                        inner: Box::new(IrNode::Seq(vec![
                            IrNode::OptionalWhitespace(Box::new(IrNode::Ref(2))),
                            IrNode::OptionalWhitespace(Box::new(IrNode::Ref(1))),
                        ])),
                        lo: 0,
                        hi: u32::MAX,
                    },
                ]),
                meta: RuleMeta::default(),
                source_span: None,
            },
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Literal(3),
                meta: RuleMeta::default(),
                source_span: None,
            },
            IrRule {
                id: 2,
                name: 2,
                body: IrNode::Literal(4),
                meta: RuleMeta::default(),
                source_span: None,
            },
        ],
        entry: 0,
        strings: vec![
            "expr".into(),
            "term".into(),
            "op".into(),
            "x".into(),
            "+".into(),
        ],
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: Vec::new(),
        type_map: None,
    };

    bbnf_ir::passes::compute_sp_method_rules(&mut ir);
    bbnf_ir::passes::project_types(&mut ir);

    let ident = quote::format_ident!("ChainParser");
    let attrs = ParserAttributes::default();
    let mut ctx = IrCodegenCtx::new(&ir, &ident, &attrs, attrs.prettify);
    ctx.operator_chain_rules.insert(0);

    let generated = generate_monolithic(&ir, &ctx).to_string();
    assert!(
        generated.contains("chain_depth"),
        "expected specialized chain scratch loop, got: {generated}"
    );
    assert!(
        generated.contains("chain_prev"),
        "expected specialized chain loop checkpoint, got: {generated}"
    );
}

#[test]
fn operator_chain_rules_allow_span_valued_link_ops() {
    let mut ir = GrammarIR {
        rules: vec![
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Seq(vec![
                    IrNode::Ref(1),
                    IrNode::Repeat {
                        inner: Box::new(IrNode::Seq(vec![IrNode::Literal(3), IrNode::Ref(1)])),
                        lo: 0,
                        hi: u32::MAX,
                    },
                ]),
                meta: RuleMeta::default(),
                source_span: None,
            },
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Literal(2),
                meta: RuleMeta::default(),
                source_span: None,
            },
        ],
        entry: 0,
        strings: vec!["expr".into(), "term".into(), "x".into(), "+".into()],
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: Vec::new(),
        type_map: None,
    };

    bbnf_ir::passes::compute_sp_method_rules(&mut ir);
    bbnf_ir::passes::project_types(&mut ir);

    let ident = quote::format_ident!("SpanLinkChainParser");
    let attrs = ParserAttributes::default();
    let mut ctx = IrCodegenCtx::new(&ir, &ident, &attrs, attrs.prettify);
    ctx.operator_chain_rules.insert(0);

    let generated = generate_monolithic(&ir, &ctx).to_string();
    assert!(
        generated.contains("chain_depth"),
        "expected specialized chain scratch loop for span-valued operator, got: {generated}"
    );
    assert!(
        generated.contains("__chain_op3") && generated.contains(":: parse_that :: Span :: new"),
        "expected span-valued link op capture in specialized chain, got: {generated}"
    );
}
