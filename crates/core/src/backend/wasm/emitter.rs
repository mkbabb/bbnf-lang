//! Emitter trait implementation for the WASM backend.

use bbnf_ir::{AltDispatch, FnDescriptor, GrammarIR, IrRule, MapExpr, RuleId, TypeDesc};

use crate::backend::driver::analysis::BackendAnalysis;
use crate::backend::patterns::key_dispatch::KeyDispatchConfig;
use crate::backend::{
    ValuePlacement, AltBranchInfo, DelimScanConfig, Emitter, FlattenStrategy, KeyDispatchBranch,
    SepByConfig, SeqChildGroup, TokenDispatchArmCompiled,
};

pub use super::code::{WasmEmitCtx, WasmEmitter};
pub use super::helpers::unescape_literal;

// ─── Emitter Trait Impl ─────────────────────────────────────────────────────

impl Emitter for WasmEmitter {
    type Output = String;
    type Ctx = WasmEmitCtx;

    // ── Leaves ──────────────────────────────────────────────────────────

    fn emit_literal_match(
        &mut self,
        value: &str,
        guaranteed_byte: Option<u8>,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        let unescaped = unescape_literal(value);
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

    fn emit_regex_match(
        &mut self,
        _pattern: &str,
        regex_id: usize,
        _ir: &GrammarIR,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        format!("(call $__match_regex (i32.const {regex_id}) (local.get $off) (local.get $len))")
    }

    fn emit_epsilon(&mut self, _ctx: &mut WasmEmitCtx) -> String {
        "(local.get $off)".to_string()
    }

    // ── Sequences ───────────────────────────────────────────────────────

    fn emit_seq_all_span(
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

    fn emit_seq_grouped(
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
        self.emit_seq_all_span(all_outputs, ctx)
    }

    // ── Alternations (delegated to alt.rs) ──────────────────────────────

    fn emit_alt_dispatch(
        &mut self,
        table: &AltDispatch,
        branches: Vec<(AltBranchInfo, String)>,
        fallback: Option<(AltBranchInfo, String)>,
        alloc: ValuePlacement,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.alt_dispatch(table, branches, fallback, alloc, ctx)
    }

    fn emit_alt_checkpoint(
        &mut self,
        branches: Vec<(AltBranchInfo, String)>,
        alloc: ValuePlacement,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.alt_checkpoint(branches, alloc, ctx)
    }

    fn emit_alt_all_literal(
        &mut self,
        literals: Vec<(String, String)>,
        alloc: ValuePlacement,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.alt_all_literal(literals, alloc, ctx)
    }

    fn emit_key_dispatch(
        &mut self,
        config: &KeyDispatchConfig,
        branches: Vec<KeyDispatchBranch<String>>,
        fallback: Option<(AltBranchInfo, String)>,
        alloc: ValuePlacement,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.key_dispatch(config, branches, fallback, alloc, ctx)
    }

    // ── Repetition (delegated to repeat.rs) ─────────────────────────────

    fn emit_repeat_many(
        &mut self,
        body: String,
        lo: u32,
        hi: u32,
        elem_type: &TypeDesc,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.repeat_many(body, lo, hi, elem_type, ctx)
    }

    fn emit_repeat_optional(
        &mut self,
        body: String,
        inner_type: &TypeDesc,
        alloc: ValuePlacement,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.repeat_optional(body, inner_type, alloc, ctx)
    }

    fn emit_sep_by(
        &mut self,
        element: String,
        separator: String,
        config: &SepByConfig,
        elem_type: &TypeDesc,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.sep_by(element, separator, config, elem_type, ctx)
    }

    // ── References ──────────────────────────────────────────────────────

    fn emit_call(
        &mut self,
        _rule_id: RuleId,
        rule_name: &str,
        _alloc: ValuePlacement,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        format!("(call $__{rule_name} (local.get $off) (local.get $len))")
    }

    fn emit_inline_wrap(
        &mut self,
        body: String,
        _variant_name: Option<&str>,
        _alloc: ValuePlacement,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        body
    }

    // ── Operator chains ──────────────────────────────────────────────────

    fn emit_operator_chain(
        &mut self,
        head: String,
        op: String,
        rhs: String,
        _head_type: &TypeDesc,
        _link_elem_type: &TypeDesc,
        _ir: &GrammarIR,
        ctx: &mut WasmEmitCtx,
    ) -> Option<String> {
        let head_var = ctx.fresh("oc_head");
        let cp = ctx.fresh("oc_cp");
        let op_var = ctx.fresh("oc_op");
        let rhs_var = ctx.fresh("oc_rhs");
        let exit = ctx.fresh_label("oc_exit");
        let lp = ctx.fresh_label("oc_loop");
        Some(format!(
            "(local.set {head_var} {head}) \
             (if (result i32) (i32.eq (local.get {head_var}) (i32.const -1)) (then (i32.const -1)) (else \
             (local.set $off (local.get {head_var})) \
             (block {exit} (loop {lp} \
               (local.set {cp} (local.get $off)) \
               (local.set {op_var} {op}) \
               (br_if {exit} (i32.eq (local.get {op_var}) (i32.const -1))) \
               (local.set $off (local.get {op_var})) \
               (local.set {rhs_var} {rhs}) \
               (if (i32.eq (local.get {rhs_var}) (i32.const -1)) \
                 (then (local.set $off (local.get {cp})) (br {exit}))) \
               (local.set $off (local.get {rhs_var})) \
               (br {lp}) \
             )) \
             (local.get $off) ))"
        ))
    }

    // ── Binary operators ────────────────────────────────────────────────

    fn emit_skip(
        &mut self,
        kept: String,
        discarded: String,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let kept_var = ctx.fresh("skip_kept");
        let disc_var = ctx.fresh("skip_disc");
        format!(
            "(local.set {kept_var} {kept}) \
             (if (result i32) (i32.ne (local.get {kept_var}) (i32.const -1)) \
               (then \
                 (local.set $off (local.get {kept_var})) \
                 (local.set {disc_var} {discarded}) \
                 (if (result i32) (i32.ne (local.get {disc_var}) (i32.const -1)) \
                   (then (local.get {disc_var})) \
                   (else (i32.const -1)))) \
               (else (i32.const -1)))"
        )
    }

    fn emit_next(
        &mut self,
        discarded: String,
        kept: String,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let disc_var = ctx.fresh("next_disc");
        format!(
            "(local.set {disc_var} {discarded}) \
             (if (result i32) (i32.ne (local.get {disc_var}) (i32.const -1)) \
               (then (local.set $off (local.get {disc_var})) {kept}) \
               (else (i32.const -1)))"
        )
    }

    fn emit_minus(
        &mut self,
        lhs: String,
        rhs: String,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let save = ctx.fresh("minus_save");
        let excluded = ctx.fresh("minus_excl");
        format!(
            "(local.set {save} (local.get $off)) \
             (local.set {excluded} {rhs}) \
             (local.set $off (local.get {save})) \
             (if (result i32) (i32.ne (local.get {excluded}) (i32.const -1)) \
               (then (i32.const -1)) \
               (else {lhs}))"
        )
    }

    fn emit_negate(
        &mut self,
        inner: String,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let save = ctx.fresh("neg_save");
        let result = ctx.fresh("neg_result");
        format!(
            "(local.set {save} (local.get $off)) \
             (local.set {result} {inner}) \
             (local.set $off (local.get {save})) \
             (if (result i32) (i32.ne (local.get {result}) (i32.const -1)) \
               (then (i32.const -1)) \
               (else (local.get $off)))"
        )
    }

    // ── Value manipulation ──────────────────────────────────────────────

    fn emit_enum_wrap(
        &mut self,
        inner: String,
        _variant_name: &str,
        _alloc: ValuePlacement,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        inner
    }

    fn emit_number_convert(&mut self, _ctx: &mut WasmEmitCtx) -> String {
        "(call $__number_convert (local.get $off) (local.get $len))".to_string()
    }

    fn emit_constant(
        &mut self,
        discard_inner: String,
        _value: &str,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let result = ctx.fresh("const_r");
        format!(
            "(local.set {result} {discard_inner}) \
             (if (result i32) (i32.ne (local.get {result}) (i32.const -1)) \
               (then (local.get {result})) \
               (else (i32.const -1)))"
        )
    }

    fn emit_map_expr(
        &mut self,
        inner: String,
        expr: &MapExpr,
        _return_type: Option<&TypeDesc>,
        _alloc: ValuePlacement,
        _ir: &GrammarIR,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        // WASM map expressions: simple cases only.
        let result = ctx.fresh("map_r");
        match expr {
            MapExpr::IntLit(n) => {
                format!(
                    "(local.set {result} {inner}) \
                     (if (result i32) (i32.ne (local.get {result}) (i32.const -1)) \
                       (then (i32.const {n})) \
                       (else (i32.const -1)))"
                )
            }
            MapExpr::BoolLit(b) => {
                let val = if *b { 1 } else { 0 };
                format!(
                    "(local.set {result} {inner}) \
                     (if (result i32) (i32.ne (local.get {result}) (i32.const -1)) \
                       (then (i32.const {val})) \
                       (else (i32.const -1)))"
                )
            }
            _ => {
                // General case: pass through (WASM doesn't evaluate complex expressions).
                format!(
                    "(local.set {result} {inner}) \
                     (local.get {result})"
                )
            }
        }
    }

    fn emit_span_capture(
        &mut self,
        inner: String,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let result = ctx.fresh("span_r");
        format!(
            "(local.set {result} {inner}) \
             (local.get {result})"
        )
    }

    fn emit_hex_convert(
        &mut self,
        inner: String,
        _fn_path: &str,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let result = ctx.fresh("hex_r");
        format!(
            "(local.set {result} {inner}) \
             (if (result i32) (i32.ne (local.get {result}) (i32.const -1)) \
               (then (call $__hex_convert (local.get {result}))) \
               (else (i32.const -1)))"
        )
    }

    fn emit_fused_map(
        &mut self,
        _inner: String,
        _inner_fd: &FnDescriptor,
        _outer_fd: &FnDescriptor,
        _alloc: ValuePlacement,
        _ir: &GrammarIR,
        _ctx: &mut WasmEmitCtx,
    ) -> Option<String> {
        None
    }

    // ── Whitespace (delegated to ws.rs) ─────────────────────────────────

    fn emit_ws_trim(
        &mut self,
        ws_pattern: Option<&str>,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.ws_trim(ws_pattern, ctx)
    }

    fn emit_with_ws_trim(
        &mut self,
        inner: String,
        ws_pattern: Option<&str>,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.with_ws_trim(inner, ws_pattern, ctx)
    }

    // ── Token dispatch (delegated to dispatch.rs) ───────────────────────

    fn emit_token_dispatch(
        &mut self,
        token: String,
        arms: Vec<TokenDispatchArmCompiled<String>>,
        fallback: String,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.token_dispatch(token, arms, fallback, ctx)
    }

    // ── Delimiter scan (delegated to dispatch.rs) ───────────────────────

    fn emit_delim_scan(
        &mut self,
        config: &DelimScanConfig,
        ctx: &mut WasmEmitCtx,
    ) -> Option<String> {
        self.delim_scan(config, ctx)
    }

    // ── Rule-level emission ─────────────────────────────────────────────

    fn emit_rule_function(
        &mut self,
        rule: &IrRule,
        body: String,
        _sync_body: Option<String>,
        ir: &GrammarIR,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        let name = ir.get_string(rule.name);
        let locals = ctx.drain_locals();
        format!(
            "  (func $__{name} (param $off i32) (param $len i32) (result i32)\n    \
             {locals}\n    \
             {body}\n  )\n"
        )
    }

    fn emit_type_definitions(
        &mut self,
        _ir: &GrammarIR,
        _analysis: &BackendAnalysis,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        "  ;; Host imports: our DFA regex engine + number scanner\n  \
         (import \"host\" \"match_regex\" (func $__match_regex (param i32 i32 i32) (result i32)))\n  \
         (import \"host\" \"number_convert\" (func $__number_convert (param i32 i32) (result i32)))\n  \
         (memory (export \"memory\") 1)\n"
            .to_string()
    }

    fn emit_grammar(
        &mut self,
        type_defs: String,
        rule_functions: Vec<String>,
        ir: &GrammarIR,
        _ctx: &mut WasmEmitCtx,
    ) -> String {
        let entry_name = ir.get_string(ir.rules[ir.entry as usize].name);
        let module_name = &self.module_name;

        let mut output = format!(
            ";; Generated by BBNF — do not edit.\n(module ${module_name}\n"
        );
        output.push_str(&type_defs);
        output.push('\n');
        for func in &rule_functions {
            output.push_str(func);
        }
        output.push_str(&format!(
            "\n  (export \"parse\" (func $__{entry_name}))\n"
        ));
        output.push_str(")\n");
        output
    }
}
