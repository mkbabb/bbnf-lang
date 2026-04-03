//! Delimiter-driven flat scanner for Wrap(Repeat(Alt)) patterns.
//!
//! When a `Repeat` is inside a `Wrap` (e.g., `"{" >> items * << "}"`), and the
//! Repeat's body is an `Alt` whose branches can be distinguished by which
//! delimiter byte appears first in a forward `memchr` scan, this module emits
//! a flat scanner loop instead of the standard recursive-descent per-element loop.
//!
//! All delimiter bytes are extracted from the grammar's own `Literal` nodes —
//! no grammar-specific knowledge is hard-coded.
//!
//! The emitted scanner uses 2–3 `memchr` calls per item instead of ~20
//! recursive-descent operations, eliminating IIFE closures, checkpoint/restore,
//! and per-element Option wrapping.
//!
//! Sub-modules:
//! - `detect`: Pattern detection — `try_detect()` and structural helpers
//! - `emit`: Code emission — `emit_scan()`, `emit_scan_loop()`, `try_emit_alloc_wrap()`

mod detect;
mod emit;

use bbnf_ir::RuleId;

// ── Re-exports (public API for sibling codegen modules) ─────────────────────

pub(super) use emit::try_emit_alloc_wrap;

// ── Configuration ───────────────────────────────────────────────────────────

/// Grammar-agnostic delimiter-scan configuration.
/// All bytes extracted from the IR's Literal nodes.
pub(super) struct DelimScanConfig {
    /// Opening delimiter byte.
    pub open_byte: u8,
    /// Closing delimiter byte.
    pub close_byte: u8,
    /// Pivot byte that distinguishes branches.
    pub pivot_byte: u8,
    /// Optional trailing delimiter for the pivot branch.
    pub trail_byte: Option<u8>,
    /// RuleId of the block/fallback branch (the cyclic Ref in the Alt).
    pub block_fn: Option<RuleId>,
    /// RuleId of the pivot branch (the rule whose body contains the pivot Literal).
    pub pivot_fn: Option<RuleId>,
    /// RuleId of the content rule containing the Repeat(Alt) — used for Vec variant name.
    pub content_rule: Option<RuleId>,
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use bbnf_ir::{GrammarIR, IrNode, IrRule, RuleMeta, TypeDesc};

    use super::*;
    use crate::backend::rust::ir_types::{IrCodegenCtx, ParserAttributes};

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
        let mut mctx = super::super::MonoCtx::new(vec![
            crate::backend::rust::analysis::inline::CallMode::DirectCall,
            crate::backend::rust::analysis::inline::CallMode::DirectCall,
        ]);

        let tokens = emit::emit_scan(&config, &ctx, &mut mctx).to_string();
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
}
