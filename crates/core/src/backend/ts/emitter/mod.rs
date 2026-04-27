//! TypeScript Emitter: implements [`Emitter`] to produce self-contained TS source.
//!
//! Uses `TsCode { stmts, expr }` to separate setup statements from result expressions,
//! eliminating IIFE closures. Generated code is flat sequential statements.
//!
//! `mod.rs` owns the single `impl Emitter for TsEmitter` block (Rust
//! requires it to be one block) and delegates every method to a
//! `xxx_impl` helper defined in a sibling file or to a helper method
//! defined in a sibling module of the enclosing `ts/` directory:
//!
//! | Source              | Methods |
//! |---|---|
//! | [`leaves`]          | literal / regex / epsilon + sequence forms |
//! | [`binary`]          | call / inline_wrap / operator_chain / skip / next / minus / negate |
//! | [`value`]           | enum_wrap / number_convert / constant / map_expr / span_capture / hex_convert / fused_map |
//! | [`grammar`]         | rule_function / type_definitions / emit_grammar |
//! | `super::alt`        | alternation dispatch / checkpoint / all-literal / key dispatch |
//! | `super::repeat`     | many / optional / sep-by |
//! | `super::dispatch`   | token dispatch / delimiter scan |
//! | `super::ws`         | whitespace trim |

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

pub use super::code::{TsCode, TsEmitCtx, TsEmitter};
pub use super::projection::{
    compile_map_expr_to_js, translate_rust_constant_to_js, ts_escape, type_desc_to_ts,
    unescape_literal, ws_skip_stmts,
};

// ─── Emitter Impl ───────────────────────────────────────────────────────────

impl Emitter for TsEmitter {
    type Output = TsCode;
    type Ctx = TsEmitCtx;

    // ── Leaves ──────────────────────────────────────────────────────────

    fn emit_literal_match(
        &mut self,
        value: &str,
        guaranteed_byte: Option<u8>,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.emit_literal_match_impl(value, guaranteed_byte, ctx)
    }

    fn emit_regex_match(
        &mut self,
        pattern: &str,
        regex_id: usize,
        ir: &GrammarIR,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.emit_regex_match_impl(pattern, regex_id, ir, ctx)
    }

    fn emit_epsilon(&mut self, ctx: &mut TsEmitCtx) -> TsCode {
        self.emit_epsilon_impl(ctx)
    }

    // ── Sequences ───────────────────────────────────────────────────────

    fn emit_seq_all_span(
        &mut self,
        child_outputs: Vec<TsCode>,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.emit_seq_all_span_impl(child_outputs, ctx)
    }

    fn emit_seq_grouped(
        &mut self,
        groups: Vec<SeqChildGroup<TsCode>>,
        result_type: &TypeDesc,
        flatten: Option<FlattenStrategy>,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.emit_seq_grouped_impl(groups, result_type, flatten, ctx)
    }

    // ── Alternations (delegated to super::alt) ──────────────────────────

    fn emit_alt_dispatch(
        &mut self,
        table: &AltDispatch,
        branches: Vec<(AltBranchInfo, TsCode)>,
        fallback: Option<(AltBranchInfo, TsCode)>,
        alloc: ValuePlacement,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.alt_dispatch(table, branches, fallback, alloc, ctx)
    }

    fn emit_alt_checkpoint(
        &mut self,
        branches: Vec<(AltBranchInfo, TsCode)>,
        alloc: ValuePlacement,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.alt_checkpoint(branches, alloc, ctx)
    }

    fn emit_alt_all_literal(
        &mut self,
        literals: Vec<(String, TsCode)>,
        alloc: ValuePlacement,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.alt_all_literal(literals, alloc, ctx)
    }

    fn emit_key_dispatch(
        &mut self,
        config: &KeyDispatchConfig,
        branches: Vec<KeyDispatchBranch<TsCode>>,
        fallback: Option<(AltBranchInfo, TsCode)>,
        alloc: ValuePlacement,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.key_dispatch(config, branches, fallback, alloc, ctx)
    }

    // ── Repetition (delegated to super::repeat) ─────────────────────────

    fn emit_repeat_many(
        &mut self,
        body: TsCode,
        lo: u32,
        hi: u32,
        elem_type: &TypeDesc,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.repeat_many(body, lo, hi, elem_type, ctx)
    }

    fn emit_repeat_optional(
        &mut self,
        body: TsCode,
        inner_type: &TypeDesc,
        alloc: ValuePlacement,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.repeat_optional(body, inner_type, alloc, ctx)
    }

    fn emit_sep_by(
        &mut self,
        element: TsCode,
        separator: TsCode,
        config: &SepByConfig,
        elem_type: &TypeDesc,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.sep_by(element, separator, config, elem_type, ctx)
    }

    // ── References ──────────────────────────────────────────────────────

    fn emit_call(
        &mut self,
        rule_id: RuleId,
        rule_name: &str,
        alloc: ValuePlacement,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.emit_call_impl(rule_id, rule_name, alloc, ctx)
    }

    fn emit_inline_wrap(
        &mut self,
        body: TsCode,
        variant_name: Option<&str>,
        alloc: ValuePlacement,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.emit_inline_wrap_impl(body, variant_name, alloc, ctx)
    }

    // ── Operator chains ─────────────────────────────────────────────────

    fn emit_operator_chain(
        &mut self,
        head: TsCode,
        op: TsCode,
        rhs: TsCode,
        head_type: &TypeDesc,
        link_elem_type: &TypeDesc,
        ir: &GrammarIR,
        ctx: &mut TsEmitCtx,
    ) -> Option<TsCode> {
        self.emit_operator_chain_impl(head, op, rhs, head_type, link_elem_type, ir, ctx)
    }

    // ── Binary operators ────────────────────────────────────────────────

    fn emit_skip(
        &mut self,
        kept: TsCode,
        discarded: TsCode,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.emit_skip_impl(kept, discarded, ctx)
    }

    fn emit_next(
        &mut self,
        discarded: TsCode,
        kept: TsCode,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.emit_next_impl(discarded, kept, ctx)
    }

    fn emit_minus(
        &mut self,
        lhs: TsCode,
        rhs: TsCode,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.emit_minus_impl(lhs, rhs, ctx)
    }

    fn emit_negate(
        &mut self,
        inner: TsCode,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.emit_negate_impl(inner, ctx)
    }

    // ── Value manipulation ──────────────────────────────────────────────

    fn emit_enum_wrap(
        &mut self,
        inner: TsCode,
        variant_name: &str,
        alloc: ValuePlacement,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.emit_enum_wrap_impl(inner, variant_name, alloc, ctx)
    }

    fn emit_number_convert(&mut self, _allow_leading_dot: bool, ctx: &mut TsEmitCtx) -> TsCode {
        // TS emitter's number path calls `Number(span)` which already
        // accepts both `.5` and strict digits; the policy is enforced
        // upstream by the regex scan, not the conversion call.
        self.emit_number_convert_impl(ctx)
    }

    fn emit_constant(
        &mut self,
        discard_inner: TsCode,
        value: &str,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.emit_constant_impl(discard_inner, value, ctx)
    }

    fn emit_map_expr(
        &mut self,
        inner: TsCode,
        expr: &MapExpr,
        return_type: Option<&TypeDesc>,
        alloc: ValuePlacement,
        ir: &GrammarIR,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.emit_map_expr_impl(inner, expr, return_type, alloc, ir, ctx)
    }

    fn emit_span_capture(
        &mut self,
        inner: TsCode,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.emit_span_capture_impl(inner, ctx)
    }

    fn emit_hex_convert(
        &mut self,
        inner: TsCode,
        fn_path: &str,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.emit_hex_convert_impl(inner, fn_path, ctx)
    }

    fn emit_fused_map(
        &mut self,
        inner: TsCode,
        inner_fd: &FnDescriptor,
        outer_fd: &FnDescriptor,
        alloc: ValuePlacement,
        ir: &GrammarIR,
        ctx: &mut TsEmitCtx,
    ) -> Option<TsCode> {
        self.emit_fused_map_impl(inner, inner_fd, outer_fd, alloc, ir, ctx)
    }

    // ── Whitespace (delegated to super::ws) ─────────────────────────────

    fn emit_ws_trim(
        &mut self,
        ws_pattern: Option<&str>,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.ws_trim(ws_pattern, ctx)
    }

    fn emit_with_ws_trim(
        &mut self,
        inner: TsCode,
        ws_pattern: Option<&str>,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.with_ws_trim(inner, ws_pattern, ctx)
    }

    // ── Token dispatch (delegated to super::dispatch) ───────────────────

    fn emit_token_dispatch(
        &mut self,
        token: TsCode,
        arms: Vec<TokenDispatchArmCompiled<TsCode>>,
        fallback: TsCode,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.token_dispatch(token, arms, fallback, ctx)
    }

    // ── Delimiter scan (delegated to super::dispatch) ───────────────────

    fn emit_delim_scan(
        &mut self,
        config: &DelimScanConfig,
        ctx: &mut TsEmitCtx,
    ) -> Option<TsCode> {
        self.delim_scan(config, ctx)
    }

    // ── Rule-level ──────────────────────────────────────────────────────

    fn emit_rule_function(
        &mut self,
        rule: &IrRule,
        body: TsCode,
        sync_body: Option<TsCode>,
        ir: &GrammarIR,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.emit_rule_function_impl(rule, body, sync_body, ir, ctx)
    }

    fn emit_type_definitions(
        &mut self,
        ir: &GrammarIR,
        analysis: &BackendAnalysis,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.emit_type_definitions_impl(ir, analysis, ctx)
    }

    fn emit_grammar(
        &mut self,
        type_defs: TsCode,
        rule_functions: Vec<TsCode>,
        ir: &GrammarIR,
        ctx: &mut TsEmitCtx,
    ) -> TsCode {
        self.emit_grammar_impl(type_defs, rule_functions, ir, ctx)
    }
}
