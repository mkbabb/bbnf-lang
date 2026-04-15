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
pub(crate) mod profile;
mod repeat;
mod seq;
pub(crate) mod string_decode;
pub(crate) mod tape_prelude;
mod ws;

use bbnf_ir::{AltBranch, FnDescriptor, GrammarIR, IrNode, IrRule, MapExpr, RuleId, TypeDesc};
use proc_macro2::TokenStream;

use crate::backend::driver::analysis::BackendAnalysis;
use crate::backend::prettify::{PrettyPolicy, PrettyRulePlan};
use crate::backend::{
    AltBranchInfo, DelimScanConfig, Emitter, FlattenStrategy, KeyDispatchBranch, KeyDispatchConfig,
    SepByConfig, SeqChildGroup, TokenDispatchArmCompiled, ValuePlacement,
};

pub use super::emitter_types::{RustEmitCtx, RustEmitter};

/// AT.1.1: Resolve the projected payload type for a node, including
/// inlined Map/Constant/FnDescriptor nodes that appear after rule
/// fusing.
///
/// This is the general replacement for the AQ.6-era `Ref(rid)` → type
/// lookup. It walks:
/// - `Ref(rid)` → look up in `ir.types`
/// - `Map { fn_id, .. }` → derive from `FnDescriptor` variant
/// - `Alt(branches)` → recurse; return the type iff all branches agree
/// - `OptionalWhitespace(inner)` → recurse (ws is transparent)
/// - `Seq([Literal, tail])` → AU.2.5 factored-prefix shape:
///   `factor_common_prefixes` rewrites `"px" | "pc" | "pt"` into
///   `Seq(Literal("p"), Alt("x" | "c" | "t"))`; the leading Literal
///   is structural, so the branch's payload type is the tail's type.
///
/// Returns `None` for compound/non-scalar nodes.
fn resolve_branch_type(node: &IrNode, ir: &GrammarIR) -> Option<TypeDesc> {
    match node {
        IrNode::Ref(rid) => ir
            .types
            .iter()
            .find_map(|(r, t)| if *r == *rid { Some(t.clone()) } else { None }),
        IrNode::Map { fn_id, .. } => {
            let fn_desc = &ir.fns[*fn_id as usize];
            match fn_desc {
                FnDescriptor::NumberConvert { .. } => Some(TypeDesc::F64),
                FnDescriptor::HexConvert { .. } => Some(TypeDesc::U32),
                FnDescriptor::Expr { return_type, .. } => return_type.clone(),
                FnDescriptor::EnumWrap { .. }
                | FnDescriptor::BoxWrap
                | FnDescriptor::SpanCapture => None,
            }
        }
        IrNode::Alt(branches, _) => {
            // For nested Alts (e.g., inlined `bool` rule), return the
            // type only if ALL branches agree on a single type.
            let mut result: Option<TypeDesc> = None;
            for b in branches {
                match resolve_branch_type(&b.node, ir) {
                    Some(td) => match &result {
                        None => result = Some(td),
                        Some(existing) if *existing == td => {}
                        Some(_) => return None, // heterogeneous nested Alt
                    },
                    None => return None, // non-scalar branch in nested Alt
                }
            }
            result
        }
        IrNode::Seq(children) if children.len() == 2 => {
            if matches!(children[0], IrNode::Literal(_)) {
                resolve_branch_type(&children[1], ir)
            } else {
                None
            }
        }
        IrNode::OptionalWhitespace(inner) => resolve_branch_type(inner, ir),
        _ => None,
    }
}

/// AT.1.1: Collect all distinct scalar payload types across Alt
/// branches. Each branch is resolved via `resolve_branch_type`;
/// types that satisfy `needs_payload_slot()` are deduplicated and
/// returned in branch order.
fn collect_alt_payload_types(branches: &[AltBranch], ir: &GrammarIR) -> Vec<TypeDesc> {
    let mut types = Vec::new();
    for b in branches {
        if let Some(td) = resolve_branch_type(&b.node, ir) {
            if td.needs_payload_slot() && !types.contains(&td) {
                types.push(td);
            }
        }
    }
    types
}

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
        rule_id: RuleId,
        rule_name: &str,
        alloc: ValuePlacement,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        self.emit_call_impl(rule_id, rule_name, alloc, ctx)
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

    fn emit_number_convert(&mut self, allow_leading_dot: bool, ctx: &mut Self::Ctx) -> TokenStream {
        self.emit_number_convert_impl(allow_leading_dot, ctx)
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
        ctx: &mut Self::Ctx,
    ) -> Option<TokenStream> {
        use bbnf_ir::passes::patterns::RecognizerShape;
        match shape {
            // Tranche Y.4: FunctionHead / HashPrefix / UnitTail were
            // deleted (zero production matches + FunctionHead's
            // short-circuit kernel couldn't emit function bodies).
            // Only PunctWsRegion survives from the X.10 / X.11b slate.
            //
            // AP.ws: thread the grammar's @ws pattern so the kernel
            // emits comment-aware whitespace when applicable.
            RecognizerShape::PunctWsRegion { puncts } => {
                let ir = ctx.ir_ctx().ir;
                let ws_pat = ir.ws_pattern.map(|sid| ir.get_string(sid));
                Some(crate::backend::kernels::punct_ws_region::emit_call(
                    puncts.as_slice(),
                    ws_pat,
                ))
            }
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

    fn pre_compile_alt_branches(
        &mut self,
        ctx: &mut Self::Ctx,
    ) {
        ctx.save_alt_context();
    }

    fn post_compile_alt_branches(
        &mut self,
        ctx: &mut Self::Ctx,
    ) {
        ctx.restore_alt_context();
    }

    fn pre_compile_rule_body(
        &mut self,
        rule: &bbnf_ir::IrRule,
        ir: &bbnf_ir::GrammarIR,
        ctx: &mut Self::Ctx,
    ) {
        let is_alt = matches!(&rule.body, bbnf_ir::IrNode::Alt(_, _));
        let is_visible = !rule.meta.is_transparent;

        // AK.1: For Alt-bodied rules, signal the Alt emitter to set
        // __branch_idx per arm. The rule epilogue will use it as the
        // variant discriminator instead of the rule's global ID.
        if is_alt && is_visible {
            ctx.branch_idx_ident = Some(quote::format_ident!("__branch_idx"));
        } else {
            ctx.branch_idx_ident = None;
        }

        // AM.3: For Alt-bodied MustTape rules, enable per-branch tape
        // surgery so each branch arm emits its own push_leaf or
        // mark_children + push_compound.
        if is_alt && is_visible {
            use bbnf_ir::passes::MaterializationClass;
            let class = RustEmitter::materialization_for_rule_pub(ir, rule);
            if class == MaterializationClass::MustTape {
                ctx.tape_surgery = Some(crate::backend::rust::emitter_types::TapeSurgeryCtx {
                    tape_kind: quote::quote! { ::bbnf::runtime::tape::TapeKind::Rule },
                });
            } else {
                ctx.tape_surgery = None;
            }
        } else {
            ctx.tape_surgery = None;
        }

        // AT.1: source payload types from projected types + inlined
        // node resolution. The CSP `project_types` pass computes each
        // rule's `TypeDesc`; for Alt-bodied rules,
        // `resolve_branch_type` walks inlined Map/Constant/FnDescriptor
        // nodes to surface per-branch scalar types that the fusing
        // passes hid from the rule-level type table.
        ctx.payload_types.clear();
        ctx.payload_layout = None;
        ctx.aggregate_field_cursor = 0;
        if is_visible {
            // AQ.6.B: prefer the aggregate layout when one is planned.
            if let Some(layout) = ir.payload_layouts.get(&rule.id) {
                ctx.payload_layout = Some(layout.clone());
            } else {
                // Direct rule type lookup.
                let direct = ir
                    .types
                    .iter()
                    .find_map(|(rid, td)| if *rid == rule.id { Some(td.clone()) } else { None })
                    .filter(|td| td.needs_payload_slot());
                if let Some(td) = direct {
                    ctx.payload_types = vec![td];
                } else if is_alt {
                    // AT.1.1: collect ALL scalar types across Alt
                    // branches, including inlined Map/Constant nodes.
                    if let IrNode::Alt(branches, _) = &rule.body {
                        ctx.payload_types = collect_alt_payload_types(branches, ir);
                    }
                }
            }
        }
    }

    fn emit_rule_function(
        &mut self,
        rule: &IrRule,
        body: TokenStream,
        sync_body: Option<TokenStream>,
        ir: &GrammarIR,
        ctx: &mut Self::Ctx,
    ) -> TokenStream {
        let result = self.emit_rule_function_impl(rule, body, sync_body, ir, ctx);
        // Clear per-rule context after the rule is emitted.
        ctx.branch_idx_ident = None;
        ctx.tape_surgery = None;
        ctx.payload_layout = None;
        ctx.aggregate_field_cursor = 0;
        result
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
