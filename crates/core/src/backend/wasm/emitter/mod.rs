//! Emitter trait impl for the WASM backend. The single `impl Emitter for
//! WasmEmitter` block lives here and delegates to `xxx_impl` helpers in
//! siblings [`leaves`], [`binary`], [`value`], [`grammar`]. Alt / repeat /
//! dispatch / ws emission live next to this directory in
//! `backend/wasm/{alt,repeat,dispatch,ws}.rs`.
mod binary;
mod grammar;
mod leaves;
mod value;
use bbnf_ir::{AltDispatch, FnDescriptor, GrammarIR, IrRule, MapExpr, RuleId, TypeDesc};
use crate::backend::driver::analysis::BackendAnalysis;
use crate::backend::{
    AltBranchInfo, DelimScanConfig, Emitter, FlattenStrategy, KeyDispatchBranch, KeyDispatchConfig,
    SepByConfig, SeqChildGroup, TokenDispatchArmCompiled, ValuePlacement,
};
pub use super::code::{WasmEmitCtx, WasmEmitter};
pub use super::helpers::unescape_literal;

impl Emitter for WasmEmitter {
    type Output = String;
    type Ctx = WasmEmitCtx;

    fn emit_literal_match(
        &mut self,
        value: &str,
        guaranteed_byte: Option<u8>,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.emit_literal_match_impl(value, guaranteed_byte, ctx)
    }

    fn emit_regex_match(
        &mut self,
        pattern: &str,
        regex_id: usize,
        ir: &GrammarIR,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.emit_regex_match_impl(pattern, regex_id, ir, ctx)
    }

    fn emit_epsilon(&mut self, ctx: &mut WasmEmitCtx) -> String {
        self.emit_epsilon_impl(ctx)
    }

    fn emit_seq_all_span(&mut self, child_outputs: Vec<String>, ctx: &mut WasmEmitCtx) -> String {
        self.emit_seq_all_span_impl(child_outputs, ctx)
    }

    fn emit_seq_grouped(
        &mut self,
        groups: Vec<SeqChildGroup<String>>,
        result_type: &TypeDesc,
        flatten: Option<FlattenStrategy>,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.emit_seq_grouped_impl(groups, result_type, flatten, ctx)
    }

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

    fn emit_call(
        &mut self,
        rule_id: RuleId,
        rule_name: &str,
        alloc: ValuePlacement,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.emit_call_impl(rule_id, rule_name, alloc, ctx)
    }

    fn emit_inline_wrap(
        &mut self,
        body: String,
        variant_name: Option<&str>,
        alloc: ValuePlacement,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.emit_inline_wrap_impl(body, variant_name, alloc, ctx)
    }

    fn emit_operator_chain(
        &mut self,
        head: String,
        op: String,
        rhs: String,
        head_type: &TypeDesc,
        link_elem_type: &TypeDesc,
        ir: &GrammarIR,
        ctx: &mut WasmEmitCtx,
    ) -> Option<String> {
        self.emit_operator_chain_impl(head, op, rhs, head_type, link_elem_type, ir, ctx)
    }

    fn emit_skip(&mut self, kept: String, discarded: String, ctx: &mut WasmEmitCtx) -> String {
        self.emit_skip_impl(kept, discarded, ctx)
    }

    fn emit_next(&mut self, discarded: String, kept: String, ctx: &mut WasmEmitCtx) -> String {
        self.emit_next_impl(discarded, kept, ctx)
    }

    fn emit_minus(&mut self, lhs: String, rhs: String, ctx: &mut WasmEmitCtx) -> String {
        self.emit_minus_impl(lhs, rhs, ctx)
    }

    fn emit_negate(&mut self, inner: String, ctx: &mut WasmEmitCtx) -> String {
        self.emit_negate_impl(inner, ctx)
    }

    fn emit_enum_wrap(
        &mut self,
        inner: String,
        variant_name: &str,
        alloc: ValuePlacement,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.emit_enum_wrap_impl(inner, variant_name, alloc, ctx)
    }

    fn emit_number_convert(&mut self, ctx: &mut WasmEmitCtx) -> String {
        self.emit_number_convert_impl(ctx)
    }

    fn emit_constant(
        &mut self,
        discard_inner: String,
        value: &str,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.emit_constant_impl(discard_inner, value, ctx)
    }

    fn emit_map_expr(
        &mut self,
        inner: String,
        expr: &MapExpr,
        return_type: Option<&TypeDesc>,
        alloc: ValuePlacement,
        ir: &GrammarIR,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.emit_map_expr_impl(inner, expr, return_type, alloc, ir, ctx)
    }

    fn emit_span_capture(&mut self, inner: String, ctx: &mut WasmEmitCtx) -> String {
        self.emit_span_capture_impl(inner, ctx)
    }

    fn emit_hex_convert(&mut self, inner: String, fn_path: &str, ctx: &mut WasmEmitCtx) -> String {
        self.emit_hex_convert_impl(inner, fn_path, ctx)
    }

    fn emit_fused_map(
        &mut self,
        inner: String,
        inner_fd: &FnDescriptor,
        outer_fd: &FnDescriptor,
        alloc: ValuePlacement,
        ir: &GrammarIR,
        ctx: &mut WasmEmitCtx,
    ) -> Option<String> {
        self.emit_fused_map_impl(inner, inner_fd, outer_fd, alloc, ir, ctx)
    }

    fn emit_ws_trim(&mut self, ws_pattern: Option<&str>, ctx: &mut WasmEmitCtx) -> String {
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

    fn emit_token_dispatch(
        &mut self,
        token: String,
        arms: Vec<TokenDispatchArmCompiled<String>>,
        fallback: String,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.token_dispatch(token, arms, fallback, ctx)
    }

    fn emit_delim_scan(
        &mut self,
        config: &DelimScanConfig,
        ctx: &mut WasmEmitCtx,
    ) -> Option<String> {
        self.delim_scan(config, ctx)
    }

    fn emit_rule_function(
        &mut self,
        rule: &IrRule,
        body: String,
        sync_body: Option<String>,
        ir: &GrammarIR,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.emit_rule_function_impl(rule, body, sync_body, ir, ctx)
    }

    fn emit_type_definitions(
        &mut self,
        ir: &GrammarIR,
        analysis: &BackendAnalysis,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.emit_type_definitions_impl(ir, analysis, ctx)
    }

    fn emit_grammar(
        &mut self,
        type_defs: String,
        rule_functions: Vec<String>,
        ir: &GrammarIR,
        ctx: &mut WasmEmitCtx,
    ) -> String {
        self.emit_grammar_impl(type_defs, rule_functions, ir, ctx)
    }
}
