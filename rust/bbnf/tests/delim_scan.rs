use std::collections::HashMap;

use bbnf::backend::rust::analysis::inline::CallMode;
use bbnf::backend::rust::delim_scan::{emit_scan, DelimScanConfig};
use bbnf::backend::rust::ir_types::{IrCodegenCtx, ParserAttributes};
use bbnf::backend::rust::MonoCtx;
use bbnf_ir::{GrammarIR, IrNode, IrRule, RuleMeta, TypeDesc};

#[test]
fn emit_alloc_uses_local_close_lookahead_capacity() {
    let ir = GrammarIR {
        rules: vec![
            IrRule {
                id: 0,
                name: 0,
                // Body infers to Vec(Enum) because Repeat(Ref(1)) → Vec(Enum).
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
        strings: vec!["items".into(), "item".into(), "x".into()],
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

    let mut ir = ir;
    bbnf_ir::passes::project_types(&mut ir);

    let ident = quote::format_ident!("TestParser");
    let attrs = ParserAttributes::default();
    let ctx = IrCodegenCtx::new(&ir, &ident, &attrs, attrs.prettify);
    let config = DelimScanConfig {
        open_byte: b'[',
        close_byte: b']',
        pivot_byte: b':',
        trail_byte: None,
        block_fn: None,
        pivot_fn: None,
        content_rule: Some(0),
    };
    let mut mctx = MonoCtx::new(vec![CallMode::DirectCall, CallMode::DirectCall]);

    let tokens = emit_scan(&config, &ctx, &mut mctx).to_string();
    // Slab mode uses scratch-based collection.
    assert!(
        tokens.contains("__s0"),
        "should use scratch push: {}",
        tokens
    );
    assert!(
        tokens.contains("__c0"),
        "should use scratch collect: {}",
        tokens
    );
}
