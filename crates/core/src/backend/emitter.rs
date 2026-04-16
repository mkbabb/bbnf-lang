//! The [`Emitter`] trait — backend-specific code emission.

use bbnf_ir::{AltDispatch, FnDescriptor, GrammarIR, IrRule, RuleId, TypeDesc};

use super::driver::analysis::BackendAnalysis;
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
///
/// ## Default impls (AW-I.W4γ)
/// The per-node `emit_*` methods covering Leaves / Seq / Alt / Repeat / Ref /
/// Binary / Value-manipulation / Ws-trim / Token-dispatch carry default impls
/// returning `Self::Output::default()` for backends whose parse-emission path
/// is dead. The Rust backend routes `parse()` through `dta_run` wholesale and
/// discards per-rule bodies at `emit_rule_function_impl`; it overrides none of
/// these defaults and the driver's per-rule traversal produces empty tokens
/// that are discarded downstream. TS + WASM continue to override every method.
///
/// The `where Self::Output: Default` bound is the minimal surface that admits
/// the no-op default — every live backend's `Output` (TokenStream, TsCode,
/// String) satisfies `Default` trivially.
pub trait Emitter {
    /// Opaque code fragment produced by emission methods.
    ///
    /// Must implement `Default` so the parse-side emit methods can
    /// carry a `Self::Output::default()` body. The Rust backend
    /// overrides only the `emit_rule_function` / `emit_type_definitions`
    /// / `emit_grammar` / `emit_prettify_*` / hook methods — all other
    /// methods return the default (empty TokenStream) which the Rust
    /// `emit_rule_function_impl` discards. TS + WASM override every
    /// method and never observe the defaults.
    type Output: Default;
    /// Backend-specific mutable context.
    type Ctx;

    // ── Leaves ──────────────────────────────────────────────────────────

    fn emit_literal_match(
        &mut self,
        _value: &str,
        _guaranteed_byte: Option<u8>,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    fn emit_regex_match(
        &mut self,
        _pattern: &str,
        _regex_id: usize,
        _ir: &GrammarIR,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    fn emit_epsilon(&mut self, _ctx: &mut Self::Ctx) -> Self::Output {
        Self::Output::default()
    }

    // ── Sequences ───────────────────────────────────────────────────────

    fn emit_seq_all_span(
        &mut self,
        _child_outputs: Vec<Self::Output>,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    fn emit_seq_grouped(
        &mut self,
        _groups: Vec<SeqChildGroup<Self::Output>>,
        _result_type: &TypeDesc,
        _flatten: Option<FlattenStrategy>,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    // ── Alternations ────────────────────────────────────────────────────

    fn emit_alt_dispatch(
        &mut self,
        _table: &AltDispatch,
        _branches: Vec<(AltBranchInfo, Self::Output)>,
        _fallback: Option<(AltBranchInfo, Self::Output)>,
        _alloc: ValuePlacement,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    fn emit_alt_checkpoint(
        &mut self,
        _branches: Vec<(AltBranchInfo, Self::Output)>,
        _alloc: ValuePlacement,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    fn emit_alt_all_literal(
        &mut self,
        _literals: Vec<(String, Self::Output)>,
        _alloc: ValuePlacement,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    fn emit_key_dispatch(
        &mut self,
        _config: &KeyDispatchConfig,
        _branches: Vec<KeyDispatchBranch<Self::Output>>,
        _fallback: Option<(AltBranchInfo, Self::Output)>,
        _alloc: ValuePlacement,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    // ── Repetition ──────────────────────────────────────────────────────

    fn emit_repeat_many(
        &mut self,
        _body: Self::Output,
        _lo: u32,
        _hi: u32,
        _elem_type: &TypeDesc,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    fn emit_repeat_optional(
        &mut self,
        _body: Self::Output,
        _inner_type: &TypeDesc,
        _alloc: ValuePlacement,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    fn emit_sep_by(
        &mut self,
        _element: Self::Output,
        _separator: Self::Output,
        _config: &SepByConfig,
        _elem_type: &TypeDesc,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    // ── References ──────────────────────────────────────────────────────

    fn emit_call(
        &mut self,
        _rule_id: RuleId,
        _rule_name: &str,
        _alloc: ValuePlacement,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    fn emit_inline_wrap(
        &mut self,
        _body: Self::Output,
        _variant_name: Option<&str>,
        _alloc: ValuePlacement,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

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
        _kept: Self::Output,
        _discarded: Self::Output,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    fn emit_next(
        &mut self,
        _discarded: Self::Output,
        _kept: Self::Output,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    fn emit_minus(
        &mut self,
        _lhs: Self::Output,
        _rhs: Self::Output,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    fn emit_negate(
        &mut self,
        _inner: Self::Output,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    // ── Value manipulation ──────────────────────────────────────────────

    fn emit_enum_wrap(
        &mut self,
        _inner: Self::Output,
        _variant_name: &str,
        _alloc: ValuePlacement,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    fn emit_number_convert(&mut self, _allow_leading_dot: bool, _ctx: &mut Self::Ctx) -> Self::Output {
        Self::Output::default()
    }

    fn emit_constant(
        &mut self,
        _discard_inner: Self::Output,
        _value: &str,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    /// Emit a structured value expression (MapExpr).
    /// Walks the expression tree and generates native code for the target backend.
    fn emit_map_expr(
        &mut self,
        _inner: Self::Output,
        _expr: &bbnf_ir::MapExpr,
        _return_type: Option<&bbnf_ir::TypeDesc>,
        _alloc: ValuePlacement,
        _ir: &GrammarIR,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    /// Emit a span capture: parse inner for validation, return Span.
    fn emit_span_capture(
        &mut self,
        _inner: Self::Output,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    /// Emit a hex conversion: inline char-class scan + user function call.
    fn emit_hex_convert(
        &mut self,
        _inner: Self::Output,
        _fn_path: &str,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

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
        _ws_pattern: Option<&str>,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    fn emit_with_ws_trim(
        &mut self,
        _inner: Self::Output,
        _ws_pattern: Option<&str>,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    // ── Token dispatch ──────────────────────────────────────────────────

    fn emit_token_dispatch(
        &mut self,
        _token: Self::Output,
        _arms: Vec<TokenDispatchArmCompiled<Self::Output>>,
        _fallback: Self::Output,
        _ctx: &mut Self::Ctx,
    ) -> Self::Output {
        Self::Output::default()
    }

    // ── Delimiter scan ──────────────────────────────────────────────────

    /// Returns `None` if not implemented — driver falls back to generic wrap.
    fn emit_delim_scan(
        &mut self,
        _config: &DelimScanConfig,
        _ctx: &mut Self::Ctx,
    ) -> Option<Self::Output> {
        None
    }

    /// Tranche X.10 — CSS recognizer family kernel emission hook.
    ///
    /// The driver consults `ir.node_facts[node_id].recognizer.shape`
    /// at Seq/Next nodes and, when a `FunctionHead`, `HashPrefix`,
    /// `UnitTail`, or `PunctWsRegion` shape applies, defers to this
    /// method. Returning `Some(out)` short-circuits the normal
    /// child-emission path; returning `None` lets the driver proceed
    /// with its usual compilation strategy (backends that don't
    /// implement kernel routing transparently fall through).
    fn emit_recognizer_family_kernel(
        &mut self,
        _shape: &bbnf_ir::passes::patterns::RecognizerShape,
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

    /// Called before the driver compiles a rule's body. Backends can
    /// use this to set up per-rule context (e.g., the Rust backend
    /// sets `branch_idx_ident` for Alt-bodied rules so the Alt
    /// emitter threads a branch discriminator through each arm).
    fn pre_compile_rule_body(
        &mut self,
        _rule: &IrRule,
        _ir: &GrammarIR,
        _ctx: &mut Self::Ctx,
    ) {}

    /// Called before the driver compiles Alt branch bodies. Backends
    /// can use this to save per-Alt context that inner (nested) Alts
    /// might otherwise consume. The Rust backend saves
    /// `tape_surgery` and `branch_idx_ident` so nested Alts don't
    /// clobber the outer Alt's state.
    fn pre_compile_alt_branches(
        &mut self,
        _ctx: &mut Self::Ctx,
    ) {}

    /// Called after the driver compiles Alt branch bodies (before the
    /// `emit_alt_*` call). Backends restore context saved by
    /// `pre_compile_alt_branches`.
    fn post_compile_alt_branches(
        &mut self,
        _ctx: &mut Self::Ctx,
    ) {}

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
