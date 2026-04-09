//! Leaf-op emission for the WASM backend: literals, regex, epsilon, and
//! sequence composition.
//!
//! Each method is `pub(super)` so the trait impl in `mod.rs` can delegate
//! to it via `self.emit_xxx_impl(...)`.

use bbnf_ir::{GrammarIR, TypeDesc};

use super::{WasmEmitCtx, WasmEmitter};
use crate::backend::{FlattenStrategy, SeqChildGroup};

impl WasmEmitter {
    pub(super) fn emit_literal_match_impl(
        &mut self,
        value: &str,
        guaranteed_byte: Option<u8>,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        let unescaped = value.to_string();
        let bytes = unescaped.as_bytes();

        if let Some(_byte) = guaranteed_byte {
            return "(i32.add (local.get $off) (i32.const 1))".to_string();
        }

        if bytes.len() == 1 {
            let byte = bytes[0];
            format!(
                "(if (result i32) (i32.and \
                   (i32.lt_u (local.get $off) (local.get $len)) \
                   (i32.eq (i32.load8_u (local.get $off)) (i32.const {byte}))) \
                 (then (i32.add (local.get $off) (i32.const 1))) \
                 (else (i32.const -1)))"
            )
        } else {
            let len = bytes.len();
            let checks: Vec<String> = bytes
                .iter()
                .enumerate()
                .map(|(i, &b)| {
                    format!(
                        "(i32.eq (i32.load8_u (i32.add (local.get $off) (i32.const {i}))) (i32.const {b}))"
                    )
                })
                .collect();

            let mut condition = format!(
                "(i32.le_u (i32.add (local.get $off) (i32.const {len})) (local.get $len))"
            );
            for check in checks {
                condition = format!("(i32.and {condition} {check})");
            }

            format!(
                "(if (result i32) {condition} \
                 (then (i32.add (local.get $off) (i32.const {len}))) \
                 (else (i32.const -1)))"
            )
        }
    }

    pub(super) fn emit_regex_match_impl(
        &mut self,
        _pattern: &str,
        regex_id: usize,
        _ir: &GrammarIR,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        format!("(call $__match_regex (i32.const {regex_id}) (local.get $off) (local.get $len))")
    }

    pub(super) fn emit_epsilon_impl(&mut self, _ctx: &mut WasmEmitCtx) -> String {
        "(local.get $off)".to_string()
    }

    pub(super) fn emit_seq_all_span_impl(
        &mut self,
        child_outputs: Vec<String>,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        if child_outputs.is_empty() {
            return "(local.get $off)".to_string();
        }
        let result = ctx.fresh("seq_ok");
        let mut body = format!("(local.set {result} (i32.const 1)) ");
        for child in &child_outputs {
            let v = ctx.fresh("seq");
            body.push_str(&format!(
                "(if (local.get {result}) (then \
                   (local.set {v} {child}) \
                   (if (i32.eq (local.get {v}) (i32.const -1)) \
                     (then (local.set {result} (i32.const 0))) \
                     (else (local.set $off (local.get {v})))) \
                 )) "
            ));
        }
        body.push_str(&format!(
            "(if (result i32) (local.get {result}) \
               (then (local.get $off)) \
               (else (i32.const -1)))"
        ));
        body
    }

    pub(super) fn emit_seq_grouped_impl(
        &mut self,
        groups: Vec<SeqChildGroup<String>>,
        _result_type: &TypeDesc,
        _flatten: Option<FlattenStrategy>,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let mut all_outputs = Vec::new();
        for group in groups {
            match group {
                SeqChildGroup::Single { output, .. } => all_outputs.push(output),
                SeqChildGroup::SpanCompressed { outputs } => all_outputs.extend(outputs),
            }
        }
        self.emit_seq_all_span_impl(all_outputs, ctx)
    }
}
