//! Emitter trait implementation for the Rust backend.
//!
//! `mod.rs` owns the single `impl Emitter for RustEmitter` block — Rust
//! requires the trait impl to be one block — and delegates every method
//! to a `xxx_impl` helper in a sibling file grouped by emit-kind:
//! `leaves`, `seq`, `alt`, `dispatch`, `repeat`, `binary`,
//! `operator_chain`, `map_value`, `grammar`, `prettify` (sub-dir), `ws`.

mod alt;
mod binary;
mod dispatch;
mod grammar;
mod leaves;
mod map_value;
mod operator_chain;
mod prettify;
mod repeat;
mod seq;
mod ws;

use bbnf_ir::{FnDescriptor, GrammarIR, IrRule, MapExpr, RuleId, TypeDesc};
use proc_macro2::TokenStream;

use crate::backend::driver::analysis::BackendAnalysis;
use crate::backend::patterns::key_dispatch::KeyDispatchConfig;
use crate::backend::prettify::{PrettyPolicy, PrettyRulePlan};
use crate::backend::{
    AltBranchInfo, DelimScanConfig, Emitter, FlattenStrategy, KeyDispatchBranch, SepByConfig,
    SeqChildGroup, TokenDispatchArmCompiled, ValuePlacement,
};

pub use super::emitter_types::{RustEmitCtx, RustEmitter};

impl Emitter for RustEmitter {
    type Output = TokenStream;
    type Ctx = RustEmitCtx;

    fn emit_literal_match(
        &mut self,
        value: &str,
        guaranteed_byte: Option<u8>,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_literal_match_impl(value, guaranteed_byte, ctx)
    }

    fn emit_regex_match(
        &mut self,
        pattern: &str,
        regex_id: usize,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_regex_match_impl(pattern, regex_id, ir, ctx)
    }

    fn emit_epsilon(&mut self, ctx: &mut Self::Ctx) -> TokenStream {
        self.emit_epsilon_impl(ctx)
    }

    fn emit_seq_all_span(
        &mut self,
        child_outputs: Vec<TokenStream>,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_seq_all_span_impl(child_outputs, ctx)
    }

    fn emit_seq_grouped(
        &mut self,
        groups: Vec<SeqChildGroup<TokenStream>>,
        result_type: &TypeDesc,
        flatten: Option<FlattenStrategy>,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_seq_grouped_impl(groups, result_type, flatten, ctx)
    }

    fn emit_alt_dispatch(
        &mut self,
        table: &bbnf_ir::AltDispatch,
        branches: Vec<(AltBranchInfo, TokenStream)>,
        fallback: Option<(AltBranchInfo, TokenStream)>,
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_alt_dispatch_impl(table, branches, fallback, alloc, ctx)
    }

    fn emit_alt_checkpoint(
        &mut self,
        branches: Vec<(AltBranchInfo, TokenStream)>,
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_alt_checkpoint_impl(branches, alloc, ctx)
    }

    fn emit_alt_all_literal(
        &mut self,
        literals: Vec<(String, TokenStream)>,
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_alt_all_literal_impl(literals, alloc, ctx)
    }

    fn emit_repeat_many(
        &mut self,
        body: TokenStream,
        lo: u32,
        hi: u32,
        elem_type: &TypeDesc,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_repeat_many_impl(body, lo, hi, elem_type, ctx)
    }

    fn emit_repeat_optional(
        &mut self,
        body: TokenStream,
        inner_type: &TypeDesc,
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_repeat_optional_impl(body, inner_type, alloc, ctx)
    }

    fn emit_sep_by(
        &mut self,
        element: TokenStream,
        separator: TokenStream,
        config: &SepByConfig,
        elem_type: &TypeDesc,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_sep_by_impl(element, separator, config, elem_type, ctx)
    }

    fn emit_call(
        &mut self,
        _rule_id: RuleId,
        rule_name: &str,
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_call_impl(rule_name, alloc, ctx)
    }

    fn emit_inline_wrap(
        &mut self,
        body: TokenStream,
        variant_name: Option<&str>,
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_inline_wrap_impl(body, variant_name, alloc, ctx)
    }

    fn emit_operator_chain(
        &mut self,
        head: TokenStream,
        op: TokenStream,
        rhs: TokenStream,
        head_type: &TypeDesc,
        link_elem_type: &TypeDesc,
        _ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> Option<TokenStream> {
        self.emit_operator_chain_impl(head, op, rhs, head_type, link_elem_type, ctx)
    }

    fn emit_skip(
        &mut self,
        kept: TokenStream,
        discarded: TokenStream,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_skip_impl(kept, discarded, ctx)
    }

    fn emit_next(
        &mut self,
        discarded: TokenStream,
        kept: TokenStream,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_next_impl(discarded, kept, ctx)
    }

    fn emit_minus(
        &mut self,
        lhs: TokenStream,
        rhs: TokenStream,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_minus_impl(lhs, rhs, ctx)
    }

    fn emit_negate(&mut self, inner: TokenStream, ctx: &mut Self::Ctx) -> TokenStream {
        self.emit_negate_impl(inner, ctx)
    }

    fn emit_enum_wrap(
        &mut self,
        inner: TokenStream,
        variant_name: &str,
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_enum_wrap_impl(inner, variant_name, alloc, ctx)
    }

    fn emit_number_convert(&mut self, ctx: &mut Self::Ctx) -> TokenStream {
        self.emit_number_convert_impl(ctx)
    }

    fn emit_constant(
        &mut self,
        discard_inner: TokenStream,
        value: &str,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_constant_impl(discard_inner, value, ctx)
    }

    fn emit_map_expr(
        &mut self,
        inner: TokenStream,
        expr: &MapExpr,
        return_type: Option<&TypeDesc>,
        _alloc: ValuePlacement,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_map_expr_impl(inner, expr, return_type, ir, ctx)
    }

    fn emit_span_capture(&mut self, inner: TokenStream, ctx: &mut Self::Ctx) -> TokenStream {
        self.emit_span_capture_impl(inner, ctx)
    }

    fn emit_hex_convert(
        &mut self,
        inner: TokenStream,
        fn_path: &str,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_hex_convert_impl(inner, fn_path, ctx)
    }

    fn emit_fused_map(
        &mut self,
        inner: TokenStream,
        inner_fd: &FnDescriptor,
        outer_fd: &FnDescriptor,
        alloc: ValuePlacement,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> Option<TokenStream> {
        self.emit_fused_map_impl(inner, inner_fd, outer_fd, alloc, ir, ctx)
    }

    fn emit_ws_trim(&mut self, ws_pattern: Option<&str>, ctx: &mut Self::Ctx) -> TokenStream {
        self.emit_ws_trim_impl(ws_pattern, ctx)
    }

    fn emit_with_ws_trim(
        &mut self,
        inner: TokenStream,
        ws_pattern: Option<&str>,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_with_ws_trim_impl(inner, ws_pattern, ctx)
    }

    fn emit_key_dispatch(
        &mut self,
        config: &KeyDispatchConfig,
        branches: Vec<KeyDispatchBranch<TokenStream>>,
        fallback: Option<(AltBranchInfo, TokenStream)>,
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_key_dispatch_impl(config, branches, fallback, alloc, ctx)
    }

    fn emit_token_dispatch(
        &mut self,
        token: TokenStream,
        arms: Vec<TokenDispatchArmCompiled<TokenStream>>,
        fallback: TokenStream,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_token_dispatch_impl(token, arms, fallback, ctx)
    }

    fn emit_delim_scan(
        &mut self,
        config: &DelimScanConfig,
        ctx: &mut Self::Ctx,
    ) -> Option<TokenStream> {
        self.emit_delim_scan_impl(config, ctx)
    }

    fn emit_recognizer_family_kernel(
        &mut self,
        shape: &bbnf_ir::passes::patterns::RecognizerShape,
        _ctx: &mut Self::Ctx,
    ) -> Option<TokenStream> {
        use bbnf_ir::passes::patterns::RecognizerShape;
        match shape {
            RecognizerShape::FunctionHead { name, paren_byte } => Some(
                crate::backend::kernels::function_head::emit_call(name.as_slice(), *paren_byte),
            ),
            RecognizerShape::HashPrefix { .. } => {
                Some(crate::backend::kernels::hash_prefix::emit_call())
            }
            RecognizerShape::UnitTail { unit } => Some(
                crate::backend::kernels::unit_tail::emit_call_span(unit.as_slice()),
            ),
            RecognizerShape::PunctWsRegion { puncts } => Some(
                crate::backend::kernels::punct_ws_region::emit_call(puncts.as_slice()),
            ),
            _ => None,
        }
    }

    fn emit_fused_number_rule(
        &mut self,
        rule: &IrRule,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> Option<TokenStream> {
        self.emit_fused_number_rule_impl(rule, ir, ctx)
    }

    fn emit_operator_chain_rule(
        &mut self,
        _rule: &IrRule,
        _ir: &GrammarIR,
        _ctx: &mut Self::Ctx,
    ) -> Option<TokenStream> {
        // Operator chains are handled at the Seq level via emit_operator_chain.
        // The driver compiles head/op/rhs with type-aware alloc and Span projection
        // before calling emit_operator_chain.
        None
    }

    fn emit_rule_function(
        &mut self,
        rule: &IrRule,
        body: TokenStream,
        sync_body: Option<TokenStream>,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_rule_function_impl(rule, body, sync_body, ir, ctx)
    }

    fn emit_type_definitions(
        &mut self,
        ir: &GrammarIR,
        analysis: &BackendAnalysis,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_type_definitions_impl(ir, analysis, ctx)
    }

    fn emit_grammar(
        &mut self,
        type_defs: TokenStream,
        rule_functions: Vec<TokenStream>,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_grammar_impl(type_defs, rule_functions, ir, ctx)
    }

    fn emit_prettify_literal(&mut self, value: &str, ctx: &mut Self::Ctx) -> TokenStream {
        self.emit_prettify_literal_impl(value, ctx)
    }
    fn emit_prettify_regex(
        &mut self,
        pattern: &str,
        regex_id: usize,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_prettify_regex_impl(pattern, regex_id, ir, ctx)
    }
    fn emit_prettify_ref(
        &mut self,
        rule_id: RuleId,
        rule_name: &str,
        plan: &PrettyRulePlan,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_prettify_ref_impl(rule_id, rule_name, plan, ir, ctx)
    }
    fn emit_prettify_seq(
        &mut self,
        children: Vec<TokenStream>,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_prettify_seq_impl(children, ctx)
    }
    fn emit_prettify_alt_dispatch(
        &mut self,
        table: &bbnf_ir::AltDispatch,
        branches: Vec<TokenStream>,
        fallback: Option<TokenStream>,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_prettify_alt_dispatch_impl(table, branches, fallback, ctx)
    }
    fn emit_prettify_alt_sequential(
        &mut self,
        branches: Vec<(TokenStream, bool)>,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_prettify_alt_sequential_impl(branches, ctx)
    }
    fn emit_prettify_repeat(
        &mut self,
        body: TokenStream,
        lo: u32,
        hi: u32,
        policy: &PrettyPolicy,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_prettify_repeat_impl(body, lo, hi, policy, ctx)
    }
    fn emit_prettify_skip(
        &mut self,
        left: TokenStream,
        right: TokenStream,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_prettify_skip_impl(left, right, ctx)
    }
    fn emit_prettify_next(
        &mut self,
        left: TokenStream,
        right: TokenStream,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_prettify_next_impl(left, right, ctx)
    }
    fn emit_prettify_optional_ws(
        &mut self,
        inner: TokenStream,
        is_atomic: bool,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_prettify_optional_ws_impl(inner, is_atomic, ctx)
    }
    fn emit_prettify_attempt(
        &mut self,
        expr: TokenStream,
        rollback_builder: bool,
        use_light: bool,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_prettify_attempt_impl(expr, rollback_builder, use_light, ctx)
    }
    fn emit_prettify_rule_function(
        &mut self,
        rule: &IrRule,
        body: TokenStream,
        policy: &PrettyPolicy,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_prettify_rule_function_impl(rule, body, policy, ir, ctx)
    }
    fn emit_prettify_grammar(
        &mut self,
        rule_functions: Vec<TokenStream>,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_prettify_grammar_impl(rule_functions, ir, ctx)
    }
}
