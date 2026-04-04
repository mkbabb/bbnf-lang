//! The [`Emitter`] trait — backend-specific code emission.

use bbnf_ir::{AltDispatch, GrammarIR, IrRule, RuleId, TypeDesc};

use super::analysis::BackendAnalysis;
use super::key_dispatch::KeyDispatchConfig;
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
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_alt_checkpoint(
        &mut self,
        branches: Vec<(AltBranchInfo, Self::Output)>,
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_alt_all_literal(
        &mut self,
        literals: Vec<(String, Self::Output)>,
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_key_dispatch(
        &mut self,
        config: &KeyDispatchConfig,
        branches: Vec<KeyDispatchBranch<Self::Output>>,
        fallback: Option<(AltBranchInfo, Self::Output)>,
        alloc: AllocStrategy,
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
        alloc: AllocStrategy,
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
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_inline_wrap(
        &mut self,
        body: Self::Output,
        variant_name: Option<&str>,
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    // ── Operator chains ─────────────────────────────────────────────────

    fn emit_operator_chain(
        &mut self,
        head: Self::Output,
        op: Self::Output,
        rhs: Self::Output,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

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
        alloc: AllocStrategy,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

    fn emit_number_convert(&mut self, ctx: &mut Self::Ctx) -> Self::Output;

    fn emit_constant(
        &mut self,
        discard_inner: Self::Output,
        value: &str,
        ctx: &mut Self::Ctx,
    ) -> Self::Output;

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

    /// Override the rule body compilation. If this returns `Some(output)`,
    /// the driver skips `compile_node` for this rule's body and uses the
    /// override directly. Used by the Rust backend for fused-number rules
    /// and operator-chain hot paths.
    fn emit_rule_body_override(
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
}
