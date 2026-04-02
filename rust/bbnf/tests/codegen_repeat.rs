//! Tests for repeat/sep_by codegen (extracted from codegen/repeat.rs).

use std::collections::HashMap;

use bbnf_ir::{GrammarIR, IrNode, IrRule, RuleMeta, TypeDesc};

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
    let ctx = IrCodegenCtx::new(&ir, &ident, &attrs);

    // Verify that scratch types were collected and alloc ctx generates correctly.
    assert!(!ctx.scratch_types.is_empty(), "scratch types should be collected");
    let (struct_def, helper_fn) = ctx.generate_alloc_ctx();
    let struct_str = struct_def.to_string();
    let helper_str = helper_fn.to_string();
    assert!(struct_str.contains("__s0"), "slab ctx should have scratch field: {}", struct_str);
    assert!(helper_str.contains("__TestParserEnum"), "helper should reference ctx type: {}", helper_str);
}
