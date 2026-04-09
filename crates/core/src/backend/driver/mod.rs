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
    /// node's `NodeId`. Built by
    /// `patterns::cache::solve_delim_scan_configs`; `compile_wrap`
    /// reads this cache instead of re-walking the tree.
    pub delim_scan_configs:
        std::collections::HashMap<NodeId, crate::backend::types::DelimScanConfig>,

    /// Pre-solved key-dispatch configurations keyed by the Alt
    /// node's `NodeId`. Built by
    /// `patterns::cache::solve_key_dispatch_configs`; `compile_alt`
    /// reads this cache instead of re-walking the tree.
    pub key_dispatch_configs:
        std::collections::HashMap<NodeId, crate::backend::patterns::cache::KeyDispatchMatch>,

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
        }
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
    ) -> Option<&'a crate::backend::types::DelimScanConfig> {
        let id = ir.dag.as_ref()?.node_for(wrap_node)?;
        self.delim_scan_configs.get(&id)
    }

    /// Look up the pre-solved key-dispatch configuration for an Alt
    /// node, resolved via the DAG.
    pub fn key_dispatch_config<'a>(
        &'a self,
        alt_node: &IrNode,
        ir: &GrammarIR,
    ) -> Option<&'a crate::backend::patterns::cache::KeyDispatchMatch> {
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
    ) -> Option<&'a bbnf_ir::passes::csp_recognizers::RecognizerDecision> {
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
    //    (exception: transparent rules are never inlined, so they
    //    always need standalone functions).
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
