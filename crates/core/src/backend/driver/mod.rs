//! Shared compilation driver.
//!
//! Walks `GrammarIR`, makes target-agnostic structural decisions
//! (dispatch strategy, span compression, inlining, sep_by detection,
//! etc.), and delegates target-specific code emission to the
//! [`Emitter`] trait.
//!
//! # Module layout
//!
//! - [`mod@node`]      — per-node dispatcher (`compile_node`)
//! - [`mod@seq`]       — Seq + operator-chain emission
//! - [`mod@alt`]       — Alt strategy dispatch (all-literal, dispatch
//!   table, key dispatch, checkpoint)
//! - [`mod@repeat`]    — Repeat (sep_by / optional / many)
//! - [`mod@reference`] — Ref (inline vs call)
//! - [`mod@map`]       — Map (FnDescriptor classification + fusion)
//! - [`mod@wrap`]      — `open >> middle << close` (delim sep_by,
//!   delim scan, generic wrap)
//! - [`mod@analysis`]  — `BackendAnalysis`, `prepare_grammar`
//! - [`mod@prettify`]  — prettify-mode specialization
//!
//! # Naming convention
//!
//! - `compile_*` functions make shared decisions.
//! - `emit_*` methods on [`Emitter`] produce target syntax.

pub mod analysis;
pub mod prettify;

mod alt;
mod map;
mod node;
mod reference;
mod repeat;
mod seq;
mod wrap;

use bbnf_ir::dag::NodeId;
use bbnf_ir::passes::MaterializationClass;
use bbnf_ir::{GrammarIR, IrNode, RuleId, TypeDesc};

use self::analysis::BackendAnalysis;
use self::node::compile_node;
use super::{CallStrategy, Emitter, ValuePlacement};

// ─── Driver State ───────────────────────────────────────────────────────────

/// Shared mutable state for the compilation driver.
///
/// Tracks target-agnostic traversal state that flows through the
/// recursive walk.
pub struct DriverState {
    /// Per-rule call strategy (inline vs direct call). Indexed by
    /// `RuleId`.
    pub call_strategies: Vec<CallStrategy>,

    /// Pre-solved Alt strategies keyed by stable `NodeId`. Populated
    /// by `solve_alt_strategies` during `prepare_grammar` and read
    /// by `compile_alt` to skip inline detection passes that would
    /// re-derive the same decision.
    pub alt_strategies:
        std::collections::HashMap<NodeId, crate::backend::strategy::alt_strategy::AltStrategy>,

    /// Pre-solved delim-scan configurations keyed by the wrap
    /// node's `NodeId`. Tranche X.8b: cloned from
    /// `ir.delim_scan_configs` (populated upstream during
    /// `mine_recognizers`). `compile_wrap` reads this sidecar instead
    /// of re-walking the tree.
    pub delim_scan_configs: std::collections::HashMap<NodeId, bbnf_ir::DelimScanConfig>,

    /// Pre-solved key-dispatch configurations keyed by the Alt
    /// node's `NodeId`. Tranche X.8b: cloned from
    /// `ir.key_dispatch_configs` (populated upstream during
    /// `mine_recognizers`). `compile_alt` reads this sidecar instead
    /// of re-walking the tree.
    pub key_dispatch_configs: std::collections::HashMap<NodeId, bbnf_ir::KeyDispatchMatch>,

    /// When set, the byte at `state.offset` is guaranteed to equal
    /// this value (from a preceding dispatch-table match). The next
    /// single-byte literal check that matches can skip the bounds
    /// check. Consumed (set to `None`) after use.
    pub dispatch_guaranteed_byte: Option<u8>,

    /// Name of the rule currently being compiled.
    pub current_rule_name: Option<String>,

    /// ID of the rule currently being compiled.
    pub current_rule_id: Option<RuleId>,

    /// Regex patterns encountered during compilation, in order of
    /// first encounter. Each pattern gets a stable `regex_id`
    /// (index). Backends use these IDs for hoisting (TS: module-level
    /// const, WASM: host function index).
    pub regex_patterns: Vec<String>,

    /// Regex ID of the `@ws` whitespace pattern (if custom `@ws` is
    /// set). Emitters use this to reference the ws regex by ID
    /// rather than by sentinel.
    pub ws_regex_id: Option<usize>,

    /// Tranche AB.2 — per-`NodeId` tape materialization class.
    ///
    /// Populated by `install_pattern_caches` from `ir.materialization`
    /// (which in turn is populated by `classify_materialization` +
    /// the joint strategy CSP). Read by the per-kind emitters to
    /// decide whether a rule emits a compound tape record
    /// (`MustTape`), a single leaf span (`TapeSpanOnly`), or
    /// nothing at all (`TransparentElide`, inlined at call sites).
    pub materialization: std::collections::HashMap<NodeId, MaterializationClass>,
}

impl DriverState {
    pub fn new(call_strategies: Vec<CallStrategy>) -> Self {
        Self {
            call_strategies,
            alt_strategies: std::collections::HashMap::new(),
            delim_scan_configs: std::collections::HashMap::new(),
            key_dispatch_configs: std::collections::HashMap::new(),
            dispatch_guaranteed_byte: None,
            current_rule_name: None,
            current_rule_id: None,
            regex_patterns: Vec::new(),
            ws_regex_id: None,
            materialization: std::collections::HashMap::new(),
        }
    }

    /// Tranche AB.2 — look up a node's tape materialization class.
    ///
    /// Resolves via the durable DAG in `ir.dag`. Returns
    /// `MaterializationClass::MustTape` (the safe default) when the
    /// DAG is absent, the node wasn't present at DAG-build time, or
    /// no classification is recorded — all three are equivalent to
    /// "fall back to the full-record path, it's always legal".
    pub fn materialization_class(
        &self,
        ir: &GrammarIR,
        node: &IrNode,
    ) -> MaterializationClass {
        ir.dag
            .as_ref()
            .and_then(|dag| dag.node_for(node))
            .and_then(|id| self.materialization.get(&id).copied())
            .unwrap_or(MaterializationClass::MustTape)
    }

    /// Look up the solved Alt strategy for a node, resolved via the
    /// durable DAG in `ir.dag`. Returns `None` if the DAG is absent
    /// or the node was not present when the DAG was built.
    pub fn alt_strategy<'a>(
        &'a self,
        node: &IrNode,
        ir: &GrammarIR,
    ) -> Option<&'a crate::backend::strategy::alt_strategy::AltStrategy> {
        let id = ir.dag.as_ref()?.node_for(node)?;
        self.alt_strategies.get(&id)
    }

    /// Look up the pre-solved delim-scan configuration for a Wrap
    /// node (the outer `Skip`/`Next` node), resolved via the DAG.
    pub fn delim_scan_config<'a>(
        &'a self,
        wrap_node: &IrNode,
        ir: &GrammarIR,
    ) -> Option<&'a bbnf_ir::DelimScanConfig> {
        let id = ir.dag.as_ref()?.node_for(wrap_node)?;
        self.delim_scan_configs.get(&id)
    }

    /// Look up the pre-solved key-dispatch configuration for an Alt
    /// node, resolved via the DAG.
    pub fn key_dispatch_config<'a>(
        &'a self,
        alt_node: &IrNode,
        ir: &GrammarIR,
    ) -> Option<&'a bbnf_ir::KeyDispatchMatch> {
        let id = ir.dag.as_ref()?.node_for(alt_node)?;
        self.key_dispatch_configs.get(&id)
    }

    /// Tranche V.8: look up the V.6 recognizer decision for a node.
    ///
    /// Resolves via the durable DAG in `ir.dag`. Consumers (the
    /// per-kind drivers and the strategy solvers) read this in
    /// preference to recomputing decisions inline. Returns `None` if
    /// the DAG is absent, the node was not present when the DAG was
    /// built, or no decision was produced for the node (e.g.,
    /// architecturally simple nodes that need no decision).
    pub fn recognizer_decision<'a>(
        &self,
        node: &IrNode,
        ir: &'a GrammarIR,
    ) -> Option<&'a bbnf_ir::passes::csp_strategy::RecognizerDecision> {
        let id = ir.dag.as_ref()?.node_for(node)?;
        ir.recognizer_decisions.get(&id)
    }

    /// Register a regex pattern and return its stable ID. If the
    /// pattern was already seen, returns the existing ID.
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

// Tranche X phase 5: shared elem-type derivation for sep_by /
// repeat-many / wrap-sep_by call sites. Replaces the prior
// `vec_elem_type(n).cloned().or_else(|| node_type(n).cloned() ...)`
// chains with a single match that clones at most once and avoids the
// closure-capture overhead.
//
// Semantics preserved from the prior implementation:
// 1. Prefer `vec_elem_type` when set (the explicit Vec-context type).
// 2. Otherwise fall back to `node_type` with the BoxedEnum→Enum
//    conversion that the Vec context implies (Vec stores unboxed
//    values; the heap indirection comes from the Vec itself).
// 3. Default to `TypeDesc::Span` when both are absent.
//
// The post-W samply profile attributed 7.42% of `compile_bbnf` self
// time to `Option::or_else` for `TypeDesc` lookups across the call
// sites in repeat.rs / wrap.rs, plus 2.41% to `TypeDesc::clone`.
pub(super) fn derive_vec_elem_type(ir: &GrammarIR, node: &IrNode) -> TypeDesc {
    match ir.vec_elem_type(node) {
        Some(t) => t.clone(),
        None => match ir.node_type(node) {
            Some(t) if *t == TypeDesc::BoxedEnum => TypeDesc::Enum,
            Some(t) => t.clone(),
            None => TypeDesc::Span,
        },
    }
}

// ─── Grammar-Level Compilation ──────────────────────────────────────────────

/// Compile an entire grammar to backend output.
///
/// Top-level entry point. Emits type definitions, compiles each rule
/// body via `compile_node`, wraps each body in a rule function, and
/// assembles everything via `emitter.emit_grammar()`.
pub fn compile_grammar<E: Emitter>(
    ir: &GrammarIR,
    analysis: &BackendAnalysis,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    // Register `@ws` as a regex so emitters can look it up by ID.
    if let Some(ws_sid) = ir.ws_pattern {
        let ws_pat = ir.get_string(ws_sid);
        let id = dstate.register_regex(ws_pat);
        dstate.ws_regex_id = Some(id);
    }

    // 1. Type definitions.
    let type_defs = emitter.emit_type_definitions(ir, analysis, ctx);

    // 2. Per-rule bodies. Skip rules that are always inlined
    //    (exceptions: transparent rules are never inlined, and
    //    `preserve_identity` rules — set under structural mode —
    //    always need a standalone function regardless of the
    //    inline-planning verdict).
    let mut rule_functions = Vec::with_capacity(ir.rules.len());
    for rule in &ir.rules {
        let strategy = dstate.call_strategy(rule.id);
        let is_entry = rule.id == ir.entry;
        if !is_entry
            && !rule.meta.is_transparent
            && !rule.meta.preserve_identity
            && strategy == CallStrategy::InlineBody
        {
            continue;
        }

        dstate.current_rule_name = Some(ir.get_string(rule.name).to_string());
        dstate.current_rule_id = Some(rule.id);

        // Transparent rules compile with Inline (public method
        // wraps). Non-transparent BoxedEnum rules compile with Alloc
        // (variant expects `&Enum`, so body must produce `&Enum` via
        // alloc propagation to Refs). Non-transparent concrete-type
        // rules compile with Inline (variant accepts the raw type, no
        // boxing needed).
        let rule_type = ir
            .types
            .iter()
            .find(|(id, _)| *id == rule.id)
            .map(|(_, td)| td);
        let body_alloc = if rule.meta.is_transparent {
            ValuePlacement::Inline
        } else if matches!(rule_type, Some(TypeDesc::BoxedEnum)) || rule_type.is_none() {
            ValuePlacement::Alloc
        } else {
            ValuePlacement::Inline
        };

        // Rule-level specialization: fused number rules and
        // operator-chain rules get an emitter-owned fast path. When
        // the emitter declines, fall back to the generic walk.
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

        // `@recover` sync expression.
        let sync_body = rule.meta.directives.recover.as_ref().map(|sync_node| {
            compile_node(sync_node, ValuePlacement::Inline, ir, dstate, emitter, ctx)
        });

        let rule_fn = emitter.emit_rule_function(rule, body, sync_body, ir, ctx);
        rule_functions.push(rule_fn);
    }

    // 3. Assemble.
    emitter.emit_grammar(type_defs, rule_functions, ir, ctx)
}
