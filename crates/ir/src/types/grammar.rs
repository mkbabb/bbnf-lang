//! `GrammarIR` — the top-level IR container, its accessors, and serialization.
//!
//! `GrammarIR` is the canonical intermediary between the BBNF frontend and all
//! backends (Rust codegen, bytecode VM, TS interpreter, prettify). It owns
//! every rule, the string interning table, host functions, type info, and
//! every cached analysis result (FOLLOW sets, dispatch hints, type map,
//! pattern annotations, regex info, node facts, durable DAG).

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use bbnf_regex::sets::charset::CharSet128;

use crate::dag;
use crate::passes;

use super::{FnDescriptor, IrNode, IrRule, RuleId, StringId, TypeDesc, count_nodes};

/// The canonical Grammar IR — the single intermediary between the BBNF frontend
/// and all backends.
#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct GrammarIR {
    /// All rules in topological order.
    pub rules: Vec<IrRule>,

    /// The entry rule (first rule parsed).
    pub entry: RuleId,

    /// String interning table. All `StringId` values index into this vector.
    pub strings: Vec<String>,

    /// Host function table. All `FnId` values index into this vector.
    pub fns: Vec<FnDescriptor>,

    /// Type information for rules that have been inferred.
    pub types: Vec<(RuleId, TypeDesc)>,

    /// FOLLOW sets for all rules, keyed by `RuleId`.
    /// Populated by the `compute_follow_sets` pass; empty until that pass runs.
    #[serde(default)]
    pub follow_sets: HashMap<RuleId, CharSet128>,

    /// Custom whitespace pattern from `@ws /regex/ ;` directive.
    /// When set, `?w` (OptionalWhitespace) compiles to this regex instead of the
    /// default ASCII `\s*` trim. The StringId indexes `self.strings`.
    #[serde(default)]
    pub ws_pattern: Option<StringId>,

    /// When true, Seq nodes where all children are simple Span leaves collapse
    /// to a single Span, eliminating slab allocation. Enabled when prettify is
    /// disabled (no @pretty formatting constraints require individual Span identity).
    #[serde(default)]
    pub collapse_simple_spans: bool,

    /// When true, all rules are instrumented for debugging.
    /// Set by `@debug * ;` directive or `#[parser(debug)]` attribute.
    #[serde(default)]
    pub debug_all: bool,

    /// Debug labels from `DebugExpression` AST nodes.
    /// Preserved through lowering for display in debug adapters.
    #[serde(default)]
    pub debug_labels: Vec<(RuleId, StringId)>,

    /// Precomputed sub-expression types for codegen. Built by `project_types` pass.
    /// Keyed by `IrNode` raw pointer — valid only within the process that ran
    /// `project_types`. Not serializable (skipped for WASM boundary transfer).
    #[serde(skip)]
    pub type_map: Option<passes::TypeMap>,

    /// Structural pattern annotations per rule. Built by `recognize_patterns` pass.
    #[serde(default)]
    pub pattern_annotations: HashMap<RuleId, passes::patterns::PatternAnnotations>,

    /// Cached regex analysis per interned regex pattern. Built by `compute_regex_info` pass.
    /// Pointer-stable within a compile session. Not serializable.
    #[serde(skip)]
    pub regex_info: HashMap<StringId, bbnf_regex::RegexInfo>,

    /// Per-node structural facts. Built by `recognize_patterns` pass
    /// (tree walk). Keyed by stable `NodeId` from the durable
    /// `GrammarDag` substrate — requires `self.dag` to be populated
    /// before the pass runs.
    #[serde(skip)]
    pub node_facts: HashMap<dag::NodeId, passes::patterns::NodeFacts>,

    /// Per-NodeId strategy decisions produced by
    /// `passes::csp_strategy::solve_strategy_decisions` (Tranche W
    /// phase 3b — replaces V.6's `solve_recognizer_decisions` with a
    /// real `csp_solver::Csp` running in
    /// `OptimizationMode::MinimizeCost`). Consumed by
    /// `crates/core/src/backend/kernels/` and the per-kind drivers.
    /// Skipped during serialization — decisions are always recomputed
    /// at compile time, never persisted.
    #[serde(skip)]
    pub recognizer_decisions: passes::csp_strategy::RecognizerDecisionMap,

    /// Durable post-extraction canonical DAG over the optimized IR tree.
    ///
    /// Populated after the intra-rule e-graph optimization + post-extraction
    /// inline/fuse loop finalizes `rules[*].body`. Downstream passes that
    /// need stable sub-expression identity (e.g., `NodeFacts`, type
    /// projection, alt strategy solving) query this via
    /// [`dag::GrammarDag::node_for`] to obtain a `NodeId` instead of
    /// pointer-keyed `HashMap<usize, _>`.
    ///
    /// Any pass that rewrites rule bodies after this is populated must
    /// rebuild the DAG (or clear this field); the reverse pointer map
    /// inside it goes stale on mutation.
    #[serde(skip)]
    pub dag: Option<dag::GrammarDag>,

    /// Per-compile cost / scheduling configuration. Single source of
    /// truth for every cost-model and scheduler in the pipeline. Built
    /// at IR construction via [`crate::CostConfig::from_env`] (so
    /// `BBNF_COST_*` benchmarking knobs apply automatically), then
    /// read by every downstream consumer.
    #[serde(skip, default)]
    pub cost_config: crate::CostConfig,
}

impl GrammarIR {
    /// Look up an interned string by its `StringId`.
    pub fn get_string(&self, id: StringId) -> &str {
        &self.strings[id as usize]
    }

    /// Look up a rule by its `RuleId`.
    pub fn get_rule(&self, id: RuleId) -> &IrRule {
        &self.rules[id as usize]
    }

    /// Find a rule by name.
    pub fn find_rule(&self, name: &str) -> Option<&IrRule> {
        self.rules.iter().find(|r| self.get_string(r.name) == name)
    }

    /// Compute a structural fingerprint of the IR for fixed-point convergence detection.
    ///
    /// Uses rule count + total node count + string pool size as a coarse-grained
    /// change detector. Used as a `debug_assert!` backup for the Changed-flag loop.
    pub fn structural_fingerprint(&self) -> (usize, usize, usize) {
        let node_count: usize = self.rules.iter().map(|r| count_nodes(&r.body)).sum();
        (self.rules.len(), node_count, self.strings.len())
    }

    // ── NodeId-resolved TypeMap convenience accessors ───────────────────
    //
    // Chain `self.dag.node_for(node)` → `self.type_map.XXX(id)` so call
    // sites don't have to spell out both lookups. All return `None`
    // when either the DAG or the TypeMap is absent.

    /// Look up the node's type from the TypeMap.
    #[inline]
    pub fn node_type(&self, node: &IrNode) -> Option<&TypeDesc> {
        let id = self.dag.as_ref()?.node_for(node)?;
        self.type_map.as_ref()?.node_type(id)
    }

    /// Look up the structural (pre-collapse) type of a node.
    #[inline]
    pub fn structural_type(&self, node: &IrNode) -> Option<&TypeDesc> {
        let id = self.dag.as_ref()?.node_for(node)?;
        self.type_map.as_ref()?.structural_type(id)
    }

    /// Look up the Vec-element type of a node.
    #[inline]
    pub fn vec_elem_type(&self, node: &IrNode) -> Option<&TypeDesc> {
        let id = self.dag.as_ref()?.node_for(node)?;
        self.type_map.as_ref()?.vec_elem_type(id)
    }

    /// Look up the effective child types for a Seq node.
    #[inline]
    pub fn seq_child_types(&self, seq_node: &IrNode) -> Option<&[TypeDesc]> {
        let id = self.dag.as_ref()?.node_for(seq_node)?;
        self.type_map.as_ref()?.seq_child_types(id)
    }

    /// Look up the result type of a Seq (post-compression,
    /// post-flattening) by the Seq node.
    #[inline]
    pub fn seq_result_type(&self, seq_node: &IrNode) -> Option<&TypeDesc> {
        let id = self.dag.as_ref()?.node_for(seq_node)?;
        self.type_map.as_ref()?.seq_result_type(id)
    }

    /// Whether the Seq preserved individual Span identity
    /// (skipped compression). Defaults to `false` when the DAG or
    /// TypeMap is absent.
    #[inline]
    pub fn seq_preserve_spans(&self, seq_node: &IrNode) -> bool {
        self.dag
            .as_ref()
            .and_then(|dag| dag.node_for(seq_node))
            .zip(self.type_map.as_ref())
            .map(|(id, tm)| tm.seq_preserve_spans(id))
            .unwrap_or(false)
    }
}

// ── Serialization ──────────────────────────────────────────────────────────

impl GrammarIR {
    /// Serialize to MessagePack bytes (compact binary, suitable for WASM boundary).
    pub fn to_msgpack(&self) -> Result<Vec<u8>, rmp_serde::encode::Error> {
        rmp_serde::to_vec(self)
    }

    /// Deserialize from MessagePack bytes.
    pub fn from_msgpack(bytes: &[u8]) -> Result<Self, rmp_serde::decode::Error> {
        rmp_serde::from_slice(bytes)
    }

    /// Serialize to JSON string (for debugging).
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }

    /// Deserialize from JSON string.
    pub fn from_json(json: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(json)
    }
}
