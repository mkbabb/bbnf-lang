//! Shared compilation driver.
//!
//! Walks `GrammarIR`, makes target-agnostic structural decisions (dispatch strategy,
//! span compression, inlining, sep_by detection, etc.), and delegates target-specific
//! code emission to the [`Emitter`] trait.
//!
//! ## Naming convention
//! - **`compile_*`** functions in this module make shared decisions
//! - **`emit_*`** methods on [`Emitter`] produce target syntax

use bbnf_ir::{FnDescriptor, GrammarIR, IrNode, RuleId, TypeDesc};

use super::analysis::BackendAnalysis;
use super::decisions;
use super::{
    ValuePlacement, AltBranchInfo, CallStrategy, Emitter, FlattenStrategy, KeyDispatchBranch,
    SepByConfig, SeqChildGroup, TokenDispatchArmCompiled,
};

// ─── Driver State ───────────────────────────────────────────────────────────

/// Shared mutable state for the compilation driver.
///
/// Tracks target-agnostic traversal state that flows through the recursive walk.
/// This is the shared subset of what was formerly `MonoCtx`.
pub struct DriverState {
    /// Per-rule call strategy (inline vs direct call).
    /// Indexed by `RuleId`.
    pub call_strategies: Vec<CallStrategy>,

    /// When set, the byte at `state.offset` is guaranteed to equal this value
    /// (from a preceding dispatch-table match). The next single-byte literal
    /// check that matches can skip the bounds check.
    /// Consumed (set to `None`) after use.
    pub dispatch_guaranteed_byte: Option<u8>,

    /// Name of the rule currently being compiled.
    pub current_rule_name: Option<String>,

    /// ID of the rule currently being compiled.
    pub current_rule_id: Option<RuleId>,

    /// Regex patterns encountered during compilation, in order of first encounter.
    /// Each pattern gets a stable `regex_id` (index). Backends use these IDs for
    /// hoisting (TS: module-level const, WASM: host function index).
    pub regex_patterns: Vec<String>,

    /// Regex ID of the `@ws` whitespace pattern (if custom `@ws` is set).
    /// Emitters use this to reference the ws regex by ID rather than sentinel.
    pub ws_regex_id: Option<usize>,

}

impl DriverState {
    pub fn new(call_strategies: Vec<CallStrategy>) -> Self {
        Self {
            call_strategies,
            dispatch_guaranteed_byte: None,
            current_rule_name: None,
            current_rule_id: None,
            regex_patterns: Vec::new(),
            ws_regex_id: None,
        }
    }

    /// Register a regex pattern and return its stable ID.
    /// If the pattern was already seen, returns the existing ID.
    pub fn register_regex(&mut self, pattern: &str) -> usize {
        if let Some(idx) = self.regex_patterns.iter().position(|p| p == pattern) {
            idx
        } else {
            let idx = self.regex_patterns.len();
            self.regex_patterns.push(pattern.to_string());
            idx
        }
    }

    /// Look up the call strategy for a rule.
    pub fn call_strategy(&self, rule_id: RuleId) -> CallStrategy {
        self.call_strategies
            .get(rule_id as usize)
            .copied()
            .unwrap_or(CallStrategy::DirectCall)
    }
}

// ─── Grammar-Level Compilation ──────────────────────────────────────────────

/// Compile an entire grammar to backend output.
///
/// This is the top-level entry point. It:
/// 1. Emits type definitions (enum, discriminated union, etc.)
/// 2. Compiles each rule body via [`compile_node`]
/// 3. Wraps each body in a rule function definition
/// 4. Assembles everything via `emitter.emit_grammar()`
pub fn compile_grammar<E: Emitter>(
    ir: &GrammarIR,
    analysis: &BackendAnalysis,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    // 0. Register ws pattern as a regex if custom @ws is set.
    if let Some(ws_sid) = ir.ws_pattern {
        let ws_pat = ir.get_string(ws_sid);
        let id = dstate.register_regex(ws_pat);
        dstate.ws_regex_id = Some(id);
    }

    // 1. Type definitions.
    let type_defs = emitter.emit_type_definitions(ir, analysis, ctx);

    // 2. Compile each rule.
    // Skip rules that are always inlined — they don't need standalone functions.
    // Exception: transparent rules are never inlined (compile_ref falls back
    // to emit_call), so they always need standalone functions.
    let mut rule_functions = Vec::with_capacity(ir.rules.len());
    for rule in &ir.rules {
        let strategy = dstate.call_strategy(rule.id);
        let is_entry = rule.id == ir.entry;
        if !is_entry
            && !rule.meta.is_transparent
            && (strategy == CallStrategy::InlineBody || strategy == CallStrategy::InlineFusion)
        {
            continue;
        }

        dstate.current_rule_name = Some(ir.get_string(rule.name).to_string());
        dstate.current_rule_id = Some(rule.id);

        // Transparent rules compile with Inline (public method wraps).
        // Non-transparent BoxedEnum rules compile with Alloc (variant expects &Enum,
        // so body must produce &Enum via alloc propagation to Refs).
        // Non-transparent concrete-type rules compile with Inline (variant accepts
        // the raw type, no boxing needed).
        let rule_type = ir.types.iter().find(|(id, _)| *id == rule.id).map(|(_, td)| td);
        let body_alloc = if rule.meta.is_transparent {
            ValuePlacement::Inline
        } else if matches!(rule_type, Some(TypeDesc::BoxedEnum)) || rule_type.is_none() {
            ValuePlacement::Alloc
        } else {
            ValuePlacement::Inline
        };

        // Tier 2: backend-specific rule body specializations.
        let body = if analysis.fused_number_rules.contains(&rule.id) {
            emitter
                .emit_fused_number_rule(rule, ir, ctx)
                .unwrap_or_else(|| compile_node(&rule.body, body_alloc, ir, dstate, emitter, ctx))
        } else if analysis.operator_chain_rules.contains(&rule.id) {
            emitter
                .emit_operator_chain_rule(rule, ir, ctx)
                .unwrap_or_else(|| compile_node(&rule.body, body_alloc, ir, dstate, emitter, ctx))
        } else {
            compile_node(&rule.body, body_alloc, ir, dstate, emitter, ctx)
        };

        // Compile recovery sync expression if @recover is present.
        let sync_body = rule.meta.directives.recover.as_ref().map(|sync_node| {
            compile_node(sync_node, ValuePlacement::Inline, ir, dstate, emitter, ctx)
        });

        // Wrap in a rule function definition.
        let rule_fn = emitter.emit_rule_function(rule, body, sync_body, ir, ctx);
        rule_functions.push(rule_fn);
    }

    // 3. Assemble.
    emitter.emit_grammar(type_defs, rule_functions, ir, ctx)
}

// ─── Node-Level Compilation ─────────────────────────────────────────────────

/// Compile a single IR node, making target-agnostic decisions and delegating
/// emission to the [`Emitter`].
///
/// `alloc` controls whether the result should be heap-allocated or returned inline.
/// This corresponds to the former `elide_box` parameter (inverted: `Inline` = `elide_box=true`).
pub fn compile_node<E: Emitter>(
    node: &IrNode,
    alloc: ValuePlacement,
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    match node {
        // ── Leaves ──────────────────────────────────────────────────────
        IrNode::Literal(sid) => {
            let raw = ir.get_string(*sid);
            // Decision: can we use the dispatch-guaranteed byte optimization?
            let guaranteed = check_guaranteed_byte(raw, dstate);
            emitter.emit_literal_match(raw, guaranteed, ctx)
        }

        IrNode::Regex(sid) => {
            let pattern = ir.get_string(*sid);
            let regex_id = dstate.register_regex(pattern);
            emitter.emit_regex_match(pattern, regex_id, ir, ctx)
        }

        IrNode::Epsilon => emitter.emit_epsilon(ctx),

        // ── Structural ─────────────────────────────────────────────────
        IrNode::Seq(children) => {
            // Decision: detect operator chain pattern Seq(head, Repeat(Seq(op, rhs))).
            if let Some((head, link, op, rhs)) = detect_operator_chain(children) {
                let type_map = ir.type_map.as_ref();

                // Compute types from TypeMap. Fall back to node_type if seq_result unavailable.
                let seq_result = type_map
                    .and_then(|tm| tm.seq_result_type(children.as_ptr() as usize).cloned());
                let head_type = seq_result
                    .as_ref()
                    .and_then(|t| match t {
                        TypeDesc::Tuple(elems) if elems.len() == 2 => Some(elems[0].clone()),
                        _ => None,
                    })
                    .or_else(|| {
                        type_map.and_then(|tm| tm.node_type(head).cloned())
                    })
                    .unwrap_or(TypeDesc::Span);

                let link_elem_type = type_map
                    .and_then(|tm| tm.vec_elem_type(link).cloned())
                    .unwrap_or(TypeDesc::Span);

                // Skip if head is Span — operator chain not beneficial for all-Span chains.
                if head_type != TypeDesc::Span {
                    // Compute per-element alloc and Span projection from types.
                    let link_elem_types = match &link_elem_type {
                        TypeDesc::Tuple(elems) if elems.len() == 2 => Some((&elems[0], &elems[1])),
                        _ => None,
                    };

                    // Alloc: BoxedEnum → Alloc, else Inline.
                    fn alloc_for(ty: &TypeDesc) -> ValuePlacement {
                        if matches!(ty, TypeDesc::BoxedEnum) { ValuePlacement::Alloc } else { ValuePlacement::Inline }
                    }

                    let head_alloc = alloc_for(&head_type);
                    let (op_alloc, rhs_alloc) = link_elem_types
                        .map(|(o, r)| (alloc_for(o), alloc_for(r)))
                        .unwrap_or((ValuePlacement::Inline, ValuePlacement::Inline));

                    // Compile each child. Span-projected children get span capture
                    // wrapping: parse the child for side effects, return Span.
                    let head_out = compile_chain_child(head, &head_type, head_alloc, ir, dstate, emitter, ctx);
                    let op_out = link_elem_types.map(|(ot, _)| compile_chain_child(op, ot, op_alloc, ir, dstate, emitter, ctx))
                        .unwrap_or_else(|| compile_node(op, ValuePlacement::Inline, ir, dstate, emitter, ctx));
                    let rhs_out = link_elem_types.map(|(_, rt)| compile_chain_child(rhs, rt, rhs_alloc, ir, dstate, emitter, ctx))
                        .unwrap_or_else(|| compile_node(rhs, ValuePlacement::Inline, ir, dstate, emitter, ctx));

                    if let Some(chain) = emitter.emit_operator_chain(
                        head_out, op_out, rhs_out, &head_type, &link_elem_type, ir, ctx,
                    ) {
                        return chain;
                    }
                }
                // Emitter declined — fall through to normal Seq.
            }
            compile_seq(children, alloc, ir, dstate, emitter, ctx)
        }

        IrNode::Alt(branches, dispatch) => {
            compile_alt(branches, dispatch.as_ref(), alloc, ir, dstate, emitter, ctx)
        }

        IrNode::Repeat { inner, lo, hi } => {
            compile_repeat(inner, *lo, *hi, alloc, ir, dstate, emitter, ctx)
        }

        IrNode::Ref(rule_id) => compile_ref(*rule_id, alloc, ir, dstate, emitter, ctx),

        // ── Binary operators ───────────────────────────────────────────
        IrNode::Skip(left, right) => {
            // Decision: detect Wrap pattern (Skip(Next(open, middle), close)).
            if let IrNode::Next(open, middle) = left.as_ref() {
                // This is a Wrap: open >> middle << close
                compile_wrap(open, middle, right, alloc, ir, dstate, emitter, ctx)
            } else {
                let kept = compile_node(left, alloc, ir, dstate, emitter, ctx);
                let discarded = compile_node(right, ValuePlacement::Inline, ir, dstate, emitter, ctx);
                emitter.emit_skip(kept, discarded, ctx)
            }
        }

        IrNode::Next(left, right) => {
            // Decision: detect Wrap pattern (Next(open, Skip(middle, close))).
            if let IrNode::Skip(middle, close) = right.as_ref() {
                compile_wrap(left, middle, close, alloc, ir, dstate, emitter, ctx)
            } else {
                let discarded =
                    compile_node(left, ValuePlacement::Inline, ir, dstate, emitter, ctx);
                let kept = compile_node(right, alloc, ir, dstate, emitter, ctx);
                emitter.emit_next(discarded, kept, ctx)
            }
        }

        IrNode::Minus(left, right) => {
            // Checkpoint/restore: try right (excluded), if it matches, reject.
            let rhs = compile_node(right, ValuePlacement::Inline, ir, dstate, emitter, ctx);
            let lhs = compile_node(left, alloc, ir, dstate, emitter, ctx);
            emitter.emit_minus(lhs, rhs, ctx)
        }

        IrNode::Negate(inner) => {
            let inner_out = compile_node(inner, ValuePlacement::Inline, ir, dstate, emitter, ctx);
            emitter.emit_negate(inner_out, ctx)
        }

        // ── Host integration ───────────────────────────────────────────
        IrNode::Map { inner, fn_id } => {
            compile_map(inner, *fn_id, alloc, ir, dstate, emitter, ctx)
        }

        // ── Whitespace ─────────────────────────────────────────────────
        IrNode::OptionalWhitespace(inner) => {
            let ws_pattern = ir.ws_pattern.map(|sid| ir.get_string(sid));
            let inner_out = compile_node(inner, alloc, ir, dstate, emitter, ctx);
            emitter.emit_with_ws_trim(inner_out, ws_pattern, ctx)
        }

        // ── Lexer-parser fusion ────────────────────────────────────────
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            if arms.is_empty() {
                let token_out =
                    compile_node(token, ValuePlacement::Inline, ir, dstate, emitter, ctx);
                let fallback_out = compile_node(fallback, alloc, ir, dstate, emitter, ctx);
                return emitter.emit_next(token_out, fallback_out, ctx);
            }
            let token_out = compile_node(token, ValuePlacement::Inline, ir, dstate, emitter, ctx);
            let compiled_arms: Vec<TokenDispatchArmCompiled<E::Output>> = arms
                .iter()
                .map(|arm| {
                    let patterns = arm
                        .patterns
                        .iter()
                        .map(|&sid| super::unescape_literal(ir.get_string(sid)).into_bytes())
                        .collect();
                    let continuation =
                        compile_node(&arm.continuation, alloc, ir, dstate, emitter, ctx);
                    TokenDispatchArmCompiled {
                        patterns,
                        guard_byte: arm.guard_byte,
                        continuation,
                    }
                })
                .collect();
            let fallback_out = compile_node(fallback, alloc, ir, dstate, emitter, ctx);
            emitter.emit_token_dispatch(token_out, compiled_arms, fallback_out, ctx)
        }
    }
}

// ─── Structural Node Compilation ────────────────────────────────────────────

/// Compile a Seq node.
///
/// Decisions made here (shared across all backends):
/// - All-Span detection: if all children are Span-typed, compress to single Span
/// - Span grouping: consecutive Span children merge into compressed groups
/// - Vec flattening: `(T, Vec<T>)` or `(Vec<T>, T)` pairs flatten to `Vec<T>`
fn compile_seq<E: Emitter>(
    children: &[IrNode],
    alloc: ValuePlacement,
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    // Shared decision: resolve types, flatten, all-Span from TypeMap.
    let decision = decisions::decide_seq(children, ir);

    if decision.all_span {
        let outputs: Vec<_> = children
            .iter()
            .map(|c| compile_node(c, ValuePlacement::Inline, ir, dstate, emitter, ctx))
            .collect();
        emitter.emit_seq_all_span(outputs, ctx)
    } else {
        let mut groups = Vec::new();
        let mut span_run: Vec<E::Output> = Vec::new();

        for (child, ty) in children.iter().zip(decision.child_types.iter()) {
            if *ty == TypeDesc::Span {
                let out = compile_node(child, ValuePlacement::Inline, ir, dstate, emitter, ctx);
                span_run.push(out);
            } else {
                if !span_run.is_empty() {
                    groups.push(SeqChildGroup::SpanCompressed {
                        outputs: std::mem::take(&mut span_run),
                    });
                }
                let ca = decisions::child_alloc(ty, alloc);
                let out = compile_node(child, ca, ir, dstate, emitter, ctx);
                groups.push(SeqChildGroup::Single {
                    output: out,
                    ty: ty.clone(),
                });
            }
        }
        if !span_run.is_empty() {
            groups.push(SeqChildGroup::SpanCompressed { outputs: span_run });
        }

        emitter.emit_seq_grouped(groups, &decision.result_type, decision.flatten, ctx)
    }
}

/// Compile an Alt node.
///
/// Decisions made here (shared across all backends):
/// - All-literal fast path
/// - Dispatch table (O(1) byte lookup)
/// - Checkpoint chain fallback
fn compile_alt<E: Emitter>(
    branches: &[bbnf_ir::AltBranch],
    dispatch: Option<&bbnf_ir::AltDispatch>,
    alloc: ValuePlacement,
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    let type_map = ir.type_map.as_ref();

    // Classify branch types.
    // In Inline context (Vec elements, elide_box=true), map BoxedEnum → Enum
    // to match the TypeMap's Vec projection. Elements are collected unboxed
    // in scratch Vecs; the slab allocates the entire slice at collect time.
    let branch_infos: Vec<AltBranchInfo> = branches
        .iter()
        .map(|b| {
            let mut ty = type_map
                .and_then(|tm| tm.node_type(&b.node).cloned())
                .unwrap_or(TypeDesc::Span);
            if alloc == ValuePlacement::Inline && ty == TypeDesc::BoxedEnum {
                ty = TypeDesc::Enum;
            }
            AltBranchInfo {
                ty,
                coercion_variant: None,
            }
        })
        .collect();

    // Decision: check for all-literal fast path.
    // Also detects Map(Literal, Expr{constant}) — literal with constant value mapping.
    // Skip this fast path when alloc == Alloc: the raw constants need sub-variant
    // wrapping + slab allocation, which the all-literal path doesn't provide.
    let all_literal_like = alloc == ValuePlacement::Inline && branches.iter().all(|b| {
        matches!(b.node, IrNode::Literal(_))
            || matches!(&b.node, IrNode::Map { inner, fn_id } if {
                matches!(inner.as_ref(), IrNode::Literal(_))
                    && matches!(&ir.fns[*fn_id as usize], FnDescriptor::Expr { expr, .. } if expr.is_constant())
            })
    });

    if all_literal_like {
        let literals: Vec<_> = branches
            .iter()
            .map(|b| {
                let (lit_sid, node_to_compile) = match &b.node {
                    IrNode::Literal(sid) => (*sid, &b.node),
                    IrNode::Map { inner, .. } if matches!(inner.as_ref(), IrNode::Literal(_)) => {
                        let IrNode::Literal(sid) = inner.as_ref() else { unreachable!() };
                        (*sid, &b.node)
                    }
                    _ => unreachable!(),
                };
                let value = ir.get_string(lit_sid).to_string();
                let output = compile_node(node_to_compile, alloc, ir, dstate, emitter, ctx);
                (value, output)
            })
            .collect();
        return emitter.emit_alt_all_literal(literals, alloc, ctx);
    }

    // Decision: use dispatch table if available.
    if let Some(table) = dispatch {
        let mut branch_outputs = Vec::with_capacity(branches.len());
        let mut fallback = None;

        for (i, (branch, info)) in branches.iter().zip(branch_infos.into_iter()).enumerate() {
            // Set guaranteed byte for single-byte dispatch branches.
            let byte_patterns: Vec<u8> = table
                .table
                .iter()
                .enumerate()
                .filter(|&(_, &b)| b as usize == i)
                .map(|(bv, _)| bv as u8)
                .collect();
            if byte_patterns.len() == 1 {
                dstate.dispatch_guaranteed_byte = Some(byte_patterns[0]);
            }
            let output = compile_node(&branch.node, alloc, ir, dstate, emitter, ctx);
            dstate.dispatch_guaranteed_byte = None;
            if table.fallback_idx == Some(i as u8) {
                fallback = Some((info, output));
            } else {
                branch_outputs.push((info, output));
            }
        }

        return emitter.emit_alt_dispatch(table, branch_outputs, fallback, alloc, ctx);
    }

    // Decision: try key dispatch.
    if let Some((mut config, detected, fallback_idx)) =
        super::key_dispatch::try_detect(branches, ir)
    {
        // Register scanner regex.
        let pattern = super::key_dispatch::key_class_regex_pattern(&config.key_class);
        config.key_scanner_regex_id = Some(dstate.register_regex(pattern));

        let mut kd_branches = Vec::with_capacity(detected.len());
        for det in &detected {
            let branch = &branches[det.branch_idx];
            let info = AltBranchInfo {
                ty: ir
                    .type_map
                    .as_ref()
                    .and_then(|tm| tm.node_type(&branch.node).cloned())
                    .unwrap_or(TypeDesc::Span),
                coercion_variant: None,
            };
            let body = compile_node(&branch.node, alloc, ir, dstate, emitter, ctx);
            let key_bytes = det
                .key_literals
                .iter()
                .map(|k| k.as_bytes().to_vec())
                .collect();
            kd_branches.push(KeyDispatchBranch {
                key_bytes,
                body,
                info,
            });
        }
        let fallback = fallback_idx.map(|fi| {
            let branch = &branches[fi];
            let info = AltBranchInfo {
                ty: ir
                    .type_map
                    .as_ref()
                    .and_then(|tm| tm.node_type(&branch.node).cloned())
                    .unwrap_or(TypeDesc::Span),
                coercion_variant: None,
            };
            let body = compile_node(&branch.node, alloc, ir, dstate, emitter, ctx);
            (info, body)
        });
        return emitter.emit_key_dispatch(&config, kd_branches, fallback, alloc, ctx);
    }

    // Fallback: checkpoint chain.
    let branch_outputs: Vec<_> = branches
        .iter()
        .zip(branch_infos)
        .map(|(branch, info)| {
            let output = compile_node(&branch.node, alloc, ir, dstate, emitter, ctx);
            (info, output)
        })
        .collect();

    emitter.emit_alt_checkpoint(branch_outputs, alloc, ctx)
}

/// Compile a Repeat node.
///
/// Decisions made here:
/// - sep_by pattern detection
/// - Optional (0..1) vs Many (0+/1+)
fn compile_repeat<E: Emitter>(
    inner: &IrNode,
    lo: u32,
    hi: u32,
    alloc: ValuePlacement,
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    let type_map = ir.type_map.as_ref();

    // Decision: detect sep_by pattern.
    // Pattern: Repeat(Skip(element, Repeat(separator, 0, 1)), lo, MAX)
    if hi == u32::MAX {
        if let Some((element, separator)) = decisions::detect_sep_by(inner) {
            // Use vec_elem_type for sep_by (scratch Vec collection context).
            // Fallback maps BoxedEnum→Enum since scratch stores unboxed values.
            let elem_type = type_map
                .and_then(|tm| tm.vec_elem_type(element).cloned())
                .or_else(|| type_map.and_then(|tm| {
                    let ty = tm.node_type(element).cloned()?;
                    Some(if ty == TypeDesc::BoxedEnum { TypeDesc::Enum } else { ty })
                }))
                .unwrap_or(TypeDesc::Span);

            let elem_alloc = if elem_type == TypeDesc::BoxedEnum {
                ValuePlacement::Alloc
            } else {
                ValuePlacement::Inline
            };
            let element_out = compile_node(element, elem_alloc, ir, dstate, emitter, ctx);
            let sep_out =
                compile_node(separator, ValuePlacement::Inline, ir, dstate, emitter, ctx);

            let config = SepByConfig {
                ws: false,
                lo,
                terminator_bytes: None,
            };

            return emitter.emit_sep_by(element_out, sep_out, &config, &elem_type, ctx);
        }
    }

    // Decision: optional (0..1) vs many.
    if lo == 0 && hi == 1 {
        let inner_type = type_map
            .and_then(|tm| tm.node_type(inner).cloned())
            .unwrap_or(TypeDesc::Span);
        // BoxedEnum optionals need Alloc so the inner Ref produces &'a Enum.
        let inner_alloc = if matches!(inner_type, TypeDesc::BoxedEnum) {
            ValuePlacement::Alloc
        } else {
            ValuePlacement::Inline
        };
        let body = compile_node(inner, inner_alloc, ir, dstate, emitter, ctx);
        emitter.emit_repeat_optional(body, &inner_type, alloc, ctx)
    } else {
        // Use vec_elem_type for repeat-many: this is the element type for
        // scratch Vec collection, not the node's projected type. Falls back
        // to node_type if vec_elem_type not populated, mapping BoxedEnum→Enum
        // since scratch Vecs store unboxed values.
        let elem_type = type_map
            .and_then(|tm| tm.vec_elem_type(inner).cloned())
            .or_else(|| {
                type_map.and_then(|tm| {
                    let ty = tm.node_type(inner).cloned()?;
                    Some(if ty == TypeDesc::BoxedEnum { TypeDesc::Enum } else { ty })
                })
            })
            .unwrap_or_else(|| {
                match inner {
                    IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => TypeDesc::Span,
                    IrNode::Ref(_) => TypeDesc::Enum,
                    _ => TypeDesc::BoxedEnum,
                }
            });
        // Override: when parent forces Alloc (Vec(non-Span) expected), prevent
        // Span compression even if elem_type fell back to Span from TypeMap.
        let elem_type = if alloc == ValuePlacement::Alloc && elem_type == TypeDesc::Span {
            TypeDesc::Enum
        } else {
            elem_type
        };
        // Match elem_type: if BoxedEnum, inner must produce &Enum (Alloc).
        // If Enum, inner produces Enum (Inline). Monolithic uses elide_box=true
        // when elem_type is Enum, elide_box=false when BoxedEnum.
        let inner_alloc = if elem_type == TypeDesc::BoxedEnum {
            ValuePlacement::Alloc
        } else {
            ValuePlacement::Inline
        };
        let body = compile_node(inner, inner_alloc, ir, dstate, emitter, ctx);
        emitter.emit_repeat_many(body, lo, hi, &elem_type, ctx)
    }
}

/// Compile a Ref node.
///
/// Decision: inline body vs direct call (from inline analysis).
fn compile_ref<E: Emitter>(
    rule_id: RuleId,
    alloc: ValuePlacement,
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    let rule = ir.get_rule(rule_id);
    let rule_name = ir.get_string(rule.name);
    let strategy = dstate.call_strategy(rule_id);

    match strategy {
        CallStrategy::DirectCall => emitter.emit_call(rule_id, rule_name, alloc, ctx),
        CallStrategy::InlineBody | CallStrategy::InlineFusion => {
            // Don't inline transparent rules or when inlining is suppressed
            // (e.g., inside heterogeneous Alt branches where types must match node_type).
            if rule.meta.is_transparent {
                return emitter.emit_call(rule_id, rule_name, alloc, ctx);
            }
            // Inline non-transparent: body compiled with Alloc so Refs produce boxed.
            let inline_alloc = ValuePlacement::Alloc;
            let body = compile_node(&rule.body, inline_alloc, ir, dstate, emitter, ctx);
            let variant_name = if rule.meta.is_transparent {
                None
            } else {
                Some(rule_name)
            };
            emitter.emit_inline_wrap(body, variant_name, alloc, ctx)
        }
    }
}

/// Compile a Map node.
///
/// Decisions: classify FnDescriptor, detect strength reductions.
fn compile_map<E: Emitter>(
    inner: &IrNode,
    fn_id: bbnf_ir::FnId,
    alloc: ValuePlacement,
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    let fn_desc = &ir.fns[fn_id as usize];

    // Map fusion: if inner is also a Map, try to fuse both operations.
    if let IrNode::Map { inner: inner2, fn_id: fn_id2 } = inner {
        let inner_fd = &ir.fns[*fn_id2 as usize];
        let inner_out = compile_node(inner2, ValuePlacement::Inline, ir, dstate, emitter, ctx);
        if let Some(fused) = emitter.emit_fused_map(inner_out, inner_fd, fn_desc, alloc, ir, ctx) {
            return fused;
        }
        // Fusion not handled — fall through to single-map path with re-compiled inner.
    }

    match fn_desc {
        FnDescriptor::NumberConvert => {
            // Fused regex → f64: the emitter handles the regex + conversion.
            emitter.emit_number_convert(ctx)
        }

        FnDescriptor::EnumWrap { variant } => {
            let variant_name = ir.get_string(*variant);
            let inner_out = compile_node(inner, ValuePlacement::Inline, ir, dstate, emitter, ctx);
            emitter.emit_enum_wrap(inner_out, variant_name, alloc, ctx)
        }

        FnDescriptor::BoxWrap => {
            // Box allocation — delegate inner with alloc.
            compile_node(inner, alloc, ir, dstate, emitter, ctx)
        }

        FnDescriptor::SpanCapture => {
            let inner_out = compile_node(inner, ValuePlacement::Inline, ir, dstate, emitter, ctx);
            emitter.emit_span_capture(inner_out, ctx)
        }

        FnDescriptor::HexConvert { fn_path } => {
            let path_str = ir.get_string(*fn_path);
            let inner_out = compile_node(inner, ValuePlacement::Inline, ir, dstate, emitter, ctx);
            emitter.emit_hex_convert(inner_out, path_str, ctx)
        }

        FnDescriptor::Expr { expr, return_type } => {
            let inner_out = compile_node(inner, ValuePlacement::Inline, ir, dstate, emitter, ctx);
            emitter.emit_map_expr(inner_out, expr, return_type.as_ref(), alloc, ir, ctx)
        }
    }
}

/// Compile a Wrap pattern: `open >> middle << close`.
///
/// Detects delimited sep_by: `open >> OW(Repeat(sep_by)) << close` with terminator.
fn compile_wrap<E: Emitter>(
    open: &IrNode,
    middle: &IrNode,
    close: &IrNode,
    alloc: ValuePlacement,
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    let type_map = ir.type_map.as_ref();

    // Decision: detect delimited sep_by with terminator.
    // Pattern: open >> OW(Repeat(Skip(element, Optional(separator)))) << close
    // where close is a single-byte Literal.
    if let Some((inner_repeat, is_ow)) = unwrap_ow(middle) {
        if let IrNode::Repeat {
            inner,
            lo,
            hi: u32::MAX,
        } = inner_repeat
        {
            if let Some((element, separator)) = decisions::detect_sep_by(inner) {
                // Extract terminator byte(s) from close literal.
                let terminator_bytes = if let IrNode::Literal(sid) = close {
                    let raw = ir.get_string(*sid);
                    let unesc = super::unescape_literal(raw);
                    Some(unesc.into_bytes())
                } else {
                    None
                };

                let elem_type = type_map
                    .and_then(|tm| {
                        tm.vec_elem_type(element).cloned().or_else(|| {
                            let ty = tm.node_type(element).cloned()?;
                            Some(if ty == TypeDesc::BoxedEnum { TypeDesc::Enum } else { ty })
                        })
                    })
                    .unwrap_or(TypeDesc::Span);

                let elem_alloc = if elem_type == TypeDesc::BoxedEnum {
                    ValuePlacement::Alloc
                } else {
                    ValuePlacement::Inline
                };
                let open_out =
                    compile_node(open, ValuePlacement::Inline, ir, dstate, emitter, ctx);
                let element_out =
                    compile_node(element, elem_alloc, ir, dstate, emitter, ctx);
                let sep_out =
                    compile_node(separator, ValuePlacement::Inline, ir, dstate, emitter, ctx);
                let close_out =
                    compile_node(close, ValuePlacement::Inline, ir, dstate, emitter, ctx);

                let config = SepByConfig {
                    ws: is_ow,
                    lo: *lo,
                    terminator_bytes,
                };

                let ws_pattern = ir.ws_pattern.map(|sid| ir.get_string(sid));
                let sep_by_out = emitter.emit_sep_by(element_out, sep_out, &config, &elem_type, ctx);

                // Wrap: open >> ws_trim(sep_by) << close
                let middle_out = if is_ow {
                    emitter.emit_with_ws_trim(sep_by_out, ws_pattern, ctx)
                } else {
                    sep_by_out
                };
                let after_open = emitter.emit_next(open_out, middle_out, ctx);
                return emitter.emit_skip(after_open, close_out, ctx);
            }
        }
    }

    // Decision: try delimiter-scan optimization.
    // Skip when alloc=Alloc — delim scan always produces Span, but BoxedEnum
    // rules need the full typed result for variant wrapping.
    if alloc == ValuePlacement::Inline {
        if let Some(config) = super::delim_scan::try_detect(open, middle, close, ir) {
            if let Some(output) = emitter.emit_delim_scan(&config, ctx) {
                return output;
            }
        }
    }

    // Generic wrap: open >> middle << close.
    let open_out = compile_node(open, ValuePlacement::Inline, ir, dstate, emitter, ctx);
    let middle_out = compile_node(middle, alloc, ir, dstate, emitter, ctx);
    let close_out = compile_node(close, ValuePlacement::Inline, ir, dstate, emitter, ctx);
    let after_open = emitter.emit_next(open_out, middle_out, ctx);
    emitter.emit_skip(after_open, close_out, ctx)
}

// ─── Decision Helpers ───────────────────────────────────────────────────────

/// Check if a literal can use the dispatch-guaranteed-byte optimization.
///
/// If the literal is a single byte matching the guaranteed byte, consumes
/// the guarantee and returns `Some(byte)`.
fn check_guaranteed_byte(raw_literal: &str, dstate: &mut DriverState) -> Option<u8> {
    let unescaped = super::unescape_literal(raw_literal);
    let bytes = unescaped.as_bytes();
    if bytes.len() == 1 {
        if let Some(guaranteed) = dstate.dispatch_guaranteed_byte {
            if guaranteed == bytes[0] {
                dstate.dispatch_guaranteed_byte = None;
                return Some(guaranteed);
            }
        }
    }
    None
}


/// Unwrap an OptionalWhitespace wrapper. Returns `(inner, is_ow)`.
fn unwrap_ow(node: &IrNode) -> Option<(&IrNode, bool)> {
    match node {
        IrNode::OptionalWhitespace(inner) => Some((inner.as_ref(), true)),
        other => Some((other, false)),
    }
}

/// Compile an operator chain child with type-aware alloc and Span projection.
///
/// When the projected type is Span, the child is compiled for its side effects
/// (advancing state.offset) and wrapped in a Span capture. This matches the
/// monolithic `emit_projected_child` behavior.
fn compile_chain_child<E: Emitter>(
    child: &IrNode,
    projected_ty: &TypeDesc,
    alloc: ValuePlacement,
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    if *projected_ty == TypeDesc::Span {
        // Span projection: compile child and wrap in span capture.
        let inner = compile_node(child, ValuePlacement::Inline, ir, dstate, emitter, ctx);
        emitter.emit_span_capture(inner, ctx)
    } else {
        compile_node(child, alloc, ir, dstate, emitter, ctx)
    }
}

/// Detect operator chain pattern: `Seq([head, Repeat(Seq([op, rhs]), 0, MAX)])`.
///
/// Returns `(head, link_node, op, rhs)` if the pattern matches.
/// `link_node` is the Seq(op, rhs) inside Repeat — needed for type computation.
fn detect_operator_chain(children: &[IrNode]) -> Option<(&IrNode, &IrNode, &IrNode, &IrNode)> {
    if children.len() != 2 {
        return None;
    }
    if let IrNode::Repeat {
        inner,
        lo: 0,
        hi: u32::MAX,
    } = &children[1]
    {
        if let IrNode::Seq(link) = inner.as_ref() {
            if link.len() == 2 {
                return Some((&children[0], inner.as_ref(), &link[0], &link[1]));
            }
        }
    }
    None
}

