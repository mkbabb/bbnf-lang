//! The [`Emitter`] trait — backend-specific code emission.

use bbnf_ir::{AltDispatch, FnDescriptor, GrammarIR, IrRule, RuleId, TypeDesc};

use super::analysis::BackendAnalysis;
use super::key_dispatch::KeyDispatchConfig;
use super::prettify::{PrettyPolicy, PrettyRulePlan};
use super::types::*;

/// Backend-specific code emission.
///
/// The compilation driver walks `GrammarIR`, makes target-agnostic decisions
/// (dispatch strategy, span compression, inlining, etc.), and calls these methods
/// with pre-resolved data. Each backend implements this trait to produce target code.
///
/// ## Naming convention
/// - **`compile_*`** = shared driver methods (make decisions, call emitter)
/// - **`emit_*`** = emitter trait methods (produce target syntax)
pub trait Emitter {
    /// Opaque code fragment produced by emission methods.
    type Output;
    /// Backend-specific mutable context.
    type Ctx;

    // ── Leaves ──────────────────────────────────────────────────────────

    fn emit_literal_match(
        &mut self,
        value: &str,
        guaranteed_byte: Option<u8>,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_regex_match(
        &mut self,
        pattern: &str,
        regex_id: usize,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_epsilon(&mut self, ctx: &mut Self::Ctx) -> Self::Output;

    // ── Sequences ───────────────────────────────────────────────────────

    fn emit_seq_all_span(
        &mut self,
        child_outputs: Vec<Self::Output>,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_seq_grouped(
        &mut self,
        groups: Vec<SeqChildGroup<Self::Output>>,
        result_type: &TypeDesc,
        flatten: Option<FlattenStrategy>,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    // ── Alternations ────────────────────────────────────────────────────

    fn emit_alt_dispatch(
        &mut self,
        table: &AltDispatch,
        branches: Vec<(AltBranchInfo, Self::Output)>,
        fallback: Option<(AltBranchInfo, Self::Output)>,
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_alt_checkpoint(
        &mut self,
        branches: Vec<(AltBranchInfo, Self::Output)>,
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_alt_all_literal(
        &mut self,
        literals: Vec<(String, Self::Output)>,
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_key_dispatch(
        &mut self,
        config: &KeyDispatchConfig,
        branches: Vec<KeyDispatchBranch<Self::Output>>,
        fallback: Option<(AltBranchInfo, Self::Output)>,
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    // ── Repetition ──────────────────────────────────────────────────────

    fn emit_repeat_many(
        &mut self,
        body: Self::Output,
        lo: u32,
        hi: u32,
        elem_type: &TypeDesc,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_repeat_optional(
        &mut self,
        body: Self::Output,
        inner_type: &TypeDesc,
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_sep_by(
        &mut self,
        element: Self::Output,
        separator: Self::Output,
        config: &SepByConfig,
        elem_type: &TypeDesc,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    // ── References ──────────────────────────────────────────────────────

    fn emit_call(
        &mut self,
        rule_id: RuleId,
        rule_name: &str,
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_inline_wrap(
        &mut self,
        body: Self::Output,
        variant_name: Option<&str>,
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    // ── Operator chains ─────────────────────────────────────────────────

    /// Emit a specialized operator chain: `head (op rhs)*`.
    /// The driver pre-compiles head, op, rhs via `compile_node`.
    /// Returns `None` if the emitter declines — driver falls back to normal Seq.
    fn emit_operator_chain(
        &mut self,
        head: Self::Output,
        op: Self::Output,
        rhs: Self::Output,
        head_type: &TypeDesc,
        link_elem_type: &TypeDesc,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> Option<Self::Output> {
        let _ = (head, op, rhs, head_type, link_elem_type, ir, ctx);
        None
    }

    // ── Binary operators ────────────────────────────────────────────────

    fn emit_skip(
        &mut self,
        kept: Self::Output,
        discarded: Self::Output,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_next(
        &mut self,
        discarded: Self::Output,
        kept: Self::Output,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_minus(
        &mut self,
        lhs: Self::Output,
        rhs: Self::Output,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_negate(
        &mut self,
        inner: Self::Output,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    // ── Value manipulation ──────────────────────────────────────────────

    fn emit_enum_wrap(
        &mut self,
        inner: Self::Output,
        variant_name: &str,
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_number_convert(&mut self, ctx: &mut Self::Ctx) -> Self::Output;

    fn emit_constant(
        &mut self,
        discard_inner: Self::Output,
        value: &str,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Emit a structured value expression (MapExpr).
    /// Walks the expression tree and generates native code for the target backend.
    fn emit_map_expr(
        &mut self,
        inner: Self::Output,
        expr: &bbnf_ir::MapExpr,
        return_type: Option<&bbnf_ir::TypeDesc>,
        alloc: ValuePlacement,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Emit a span capture: parse inner for validation, return Span.
    fn emit_span_capture(
        &mut self,
        inner: Self::Output,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Emit a hex conversion: inline char-class scan + user function call.
    fn emit_hex_convert(
        &mut self,
        inner: Self::Output,
        fn_path: &str,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    /// Try to fuse two chained Map operations into one emission.
    /// Returns `None` if fusion is not supported for this pair — driver falls back
    /// to compiling the full inner Map node separately.
    fn emit_fused_map(
        &mut self,
        _inner: Self::Output,
        _inner_fd: &FnDescriptor,
        _outer_fd: &FnDescriptor,
        _alloc: ValuePlacement,
        _ir: &GrammarIR,
        _ctx: &mut Self::Ctx,
    ) -> Option<Self::Output> {
        None
    }

    fn emit_ws_trim(
        &mut self,
        ws_pattern: Option<&str>,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_with_ws_trim(
        &mut self,
        inner: Self::Output,
        ws_pattern: Option<&str>,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    // ── Token dispatch ──────────────────────────────────────────────────

    fn emit_token_dispatch(
        &mut self,
        token: Self::Output,
        arms: Vec<TokenDispatchArmCompiled<Self::Output>>,
        fallback: Self::Output,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    // ── Delimiter scan ──────────────────────────────────────────────────

    /// Returns `None` if not implemented — driver falls back to generic wrap.
    fn emit_delim_scan(
        &mut self,
        _config: &DelimScanConfig,
        _ctx: &mut Self::Ctx,
    ) -> Option<Self::Output> {
        None
    }

    // ── Rule-level emission ─────────────────────────────────────────────

    /// Emit a fused number rule body (JSON number → f64 specialization).
    /// Returns `Some` if the backend handles fused number rules, `None` to
    /// fall through to normal `compile_node`.
    fn emit_fused_number_rule(
        &mut self,
        _rule: &IrRule,
        _ir: &GrammarIR,
        _ctx: &mut Self::Ctx,
    ) -> Option<Self::Output> {
        None
    }

    /// Emit an operator chain rule body (precedence climbing specialization).
    /// Returns `Some` if the backend handles operator chains, `None` to
    /// fall through to normal `compile_node`.
    fn emit_operator_chain_rule(
        &mut self,
        _rule: &IrRule,
        _ir: &GrammarIR,
        _ctx: &mut Self::Ctx,
    ) -> Option<Self::Output> {
        None
    }

    fn emit_rule_function(
        &mut self,
        rule: &IrRule,
        body: Self::Output,
        sync_body: Option<Self::Output>,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_type_definitions(
        &mut self,
        ir: &GrammarIR,
        analysis: &BackendAnalysis,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_grammar(
        &mut self,
        type_defs: Self::Output,
        rule_functions: Vec<Self::Output>,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    // ── Prettify emission ──────────────────────────────────────────────

    fn emit_prettify_literal(&mut self, _value: &str, _ctx: &mut Self::Ctx) -> Self::Output {
        unimplemented!("prettify not supported")
    }
    fn emit_prettify_regex(&mut self, _pattern: &str, _regex_id: usize, _ir: &GrammarIR, _ctx: &mut Self::Ctx) -> Self::Output {
        unimplemented!("prettify not supported")
    }
    fn emit_prettify_ref(&mut self, _rule_id: RuleId, _rule_name: &str, _plan: &PrettyRulePlan, _ir: &GrammarIR, _ctx: &mut Self::Ctx) -> Self::Output {
        unimplemented!("prettify not supported")
    }
    fn emit_prettify_seq(&mut self, _children: Vec<Self::Output>, _ctx: &mut Self::Ctx) -> Self::Output {
        unimplemented!("prettify not supported")
    }
    fn emit_prettify_alt_dispatch(&mut self, _table: &AltDispatch, _branches: Vec<Self::Output>, _fallback: Option<Self::Output>, _ctx: &mut Self::Ctx) -> Self::Output {
        unimplemented!("prettify not supported")
    }
    fn emit_prettify_alt_sequential(&mut self, _branches: Vec<(Self::Output, bool)>, _ctx: &mut Self::Ctx) -> Self::Output {
        unimplemented!("prettify not supported")
    }
    fn emit_prettify_repeat(&mut self, _body: Self::Output, _lo: u32, _hi: u32, _policy: &PrettyPolicy, _ctx: &mut Self::Ctx) -> Self::Output {
        unimplemented!("prettify not supported")
    }
    fn emit_prettify_skip(&mut self, _left: Self::Output, _right: Self::Output, _ctx: &mut Self::Ctx) -> Self::Output {
        unimplemented!("prettify not supported")
    }
    fn emit_prettify_next(&mut self, _left: Self::Output, _right: Self::Output, _ctx: &mut Self::Ctx) -> Self::Output {
        unimplemented!("prettify not supported")
    }
    fn emit_prettify_optional_ws(&mut self, _inner: Self::Output, _is_atomic: bool, _ctx: &mut Self::Ctx) -> Self::Output {
        unimplemented!("prettify not supported")
    }
    fn emit_prettify_attempt(&mut self, _expr: Self::Output, _rollback_builder: bool, _use_light: bool, _ctx: &mut Self::Ctx) -> Self::Output {
        unimplemented!("prettify not supported")
    }
    fn emit_prettify_rule_function(&mut self, _rule: &IrRule, _body: Self::Output, _policy: &PrettyPolicy, _ir: &GrammarIR, _ctx: &mut Self::Ctx) -> Self::Output {
        unimplemented!("prettify not supported")
    }
    fn emit_prettify_grammar(&mut self, _rule_functions: Vec<Self::Output>, _ir: &GrammarIR, _ctx: &mut Self::Ctx) -> Self::Output {
        unimplemented!("prettify not supported")
    }
}
