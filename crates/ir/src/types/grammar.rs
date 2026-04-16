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
use crate::egraph;
use crate::passes;

use super::{
    DelimScanConfig, FnDescriptor, IrNode, IrRule, KeyDispatchMatch, RuleId, StringId, TypeDesc,
    TypeDescId, TypeDescInterner, count_nodes,
};

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

    /// Per-NodeId delimiter-scan configurations. Populated by
    /// `passes::recognizers::delim_scan::collect` during
    /// `mine_recognizers` (Tranche X.8a). Authoritative source for
    /// delim-scan emission — the backend reads this sidecar instead of
    /// re-walking the IR. Keyed by the Wrap root's stable `NodeId`.
    #[serde(skip)]
    pub delim_scan_configs: HashMap<dag::NodeId, DelimScanConfig>,

    /// Per-NodeId key-dispatch configurations. Populated by
    /// `passes::recognizers::key_dispatch::collect` during
    /// `mine_recognizers` (Tranche X.8a). Authoritative source for
    /// key-dispatch emission — the backend reads this sidecar instead
    /// of re-walking the IR. Keyed by the Alt node's stable `NodeId`.
    #[serde(skip)]
    pub key_dispatch_configs: HashMap<dag::NodeId, KeyDispatchMatch>,

    /// Per-NodeId propagated context facts. Populated by
    /// `passes::recognizers::ContextFactsMiner` as part of the unified
    /// `mine_recognizers` walk (Tranche X.8g, fused in AF.1) and
    /// consumed by downstream passes.
    #[serde(skip)]
    pub context_facts: passes::context::ContextFactsMap,

    /// `true` iff at least one `NodeFacts.recognizer` carries a
    /// Tranche-X.10 family shape (`FunctionHead` / `HashPrefix` /
    /// `UnitTail` / `PunctWsRegion`). Computed once at the end of
    /// `mine_recognizers` and consumed by
    /// `backend::driver::node::compile_node` to elide the per-node
    /// family-kernel probe on grammars with no matches.
    ///
    /// This flag exists because post-Tranche-X parse-time regressions
    /// (`json_canada −3.9%`, `css_tailwind −5.6%`) traced to the
    /// `try_emit_family_kernel` probe firing on every node of grammars
    /// (JSON, CSS L4) that match zero families. Gating the probe on
    /// this flag recovers the regression without deleting the
    /// families (which Y.4 re-evaluates via staged match-or-delete).
    #[serde(skip)]
    pub has_family_recognizers: bool,

    /// Per-`StringId` regex engine decision. Populated by
    /// `passes::csp_strategy::solve_strategy_decisions` after the
    /// strategy CSP picks an engine per regex site (Tranche X.8d).
    /// The `scanner_plan::plan_regex_scanner` primary path reads from
    /// this map; `classify_regex` survives only as the fall-through
    /// when a pattern has no authoritative decision.
    #[serde(skip)]
    pub regex_engine_decisions: HashMap<StringId, passes::csp_strategy::RegexEngine>,

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

    /// Tranche AA.1 — structural-type hash-cons.
    ///
    /// Every `TypeDesc` that appears in the type-projection CSP's
    /// lattice domain is interned here so `LatticeDomain::join` can
    /// compare `Option<TypeDescId>` by `Copy` instead of cloning deep
    /// `TypeDesc::Tuple(Vec<_>)` trees. The interner also provides
    /// reference-equality structural typing for downstream consumers
    /// (dispatch-share signatures in AA.5, TaggedUnion narrowing in
    /// AA.7, tape view codegen in AB.2). Not serialized across the
    /// WASM boundary: every compile rebuilds it from scratch so the
    /// wire format stays compact.
    #[serde(skip, default)]
    pub type_desc_interner: TypeDescInterner,

    /// Tranche AB.0 — per-`NodeId` materialization class.
    ///
    /// Populated by `classify_materialization` after `project_types`
    /// in `finalize_compile`. Refined in Tranche AB.1 by the CSP
    /// joint strategy + materialization solve. Consumed by the
    /// tape-first emitter (AB.2) to decide per-rule prelude/epilogue
    /// shape and per-ref inlining at call sites.
    ///
    /// Keyed by `dag::NodeId` — requires `self.dag` to be populated.
    /// Not serialized: every compile rebuilds it from scratch.
    #[serde(skip, default)]
    pub materialization: HashMap<dag::NodeId, passes::MaterializationClass>,

    /// Tranche AV.5.2 — per-`NodeId` [`EClassFacts`] sidecar.
    ///
    /// Populated by [`passes::materialization::compute_eclass_facts`]
    /// during `finalize_compile`, before the recognizer-mining walk.
    /// Cached on the IR so downstream miners (notably
    /// [`passes::recognizers::ShapeDictMiner`]) can read fixed-shape
    /// / closure-free / descendant-elidable bits without recomputing
    /// the bottom-up lattice. Keyed by `dag::NodeId` — requires
    /// `self.dag` to be populated.
    /// Not serialized: every compile rebuilds it from scratch.
    #[serde(skip, default)]
    pub eclass_facts: HashMap<dag::NodeId, egraph::EClassFacts>,

    /// Tranche AV.5.2 — per-NodeId shape-dictionary template
    /// candidates emitted by
    /// [`passes::recognizers::ShapeDictMiner`].
    ///
    /// Each entry pairs a node with its discovered
    /// [`passes::recognizers::shape_dict::ShapeTemplate`]. The CSP
    /// shape-dict constraint
    /// ([`passes::csp_strategy::constraints::shape_dict`]) selects a
    /// budget-bounded subset; the emitter bakes the chosen subset
    /// into `GrammarProfile::shape_dict`.
    /// Not serialized.
    #[serde(skip, default)]
    pub shape_dict_templates: passes::recognizers::shape_dict::ShapeDictMap,

    /// Tranche AQ.6.B — per-rule aggregate payload layouts.
    ///
    /// Populated by [`passes::compute_payload_layouts`] after
    /// `project_types` in `finalize_compile`. For each rule whose
    /// projected `TypeDesc` is a `Tuple` of scalars and whose
    /// total packed size fits in
    /// [`passes::MAX_PAYLOAD_BYTES`], the planner records field
    /// offsets respecting natural alignment.
    ///
    /// Consumed by the Rust backend's rule prelude / epilogue
    /// (writes scalars into a stack-allocated 16-byte buffer, then
    /// commits via `push_leaf_with` + `PayloadData::Aggregate`) and
    /// by the view layer (reads the bytes back via
    /// `Tape::payload_bytes`). Rules
    /// missing from this map fall back to the existing compound or
    /// scalar-payload pathways. Not serialized: every compile
    /// rebuilds it from scratch.
    #[serde(skip, default)]
    pub payload_layouts: HashMap<RuleId, passes::PayloadLayout>,

    /// Reverse string index: pattern string → `StringId`.
    ///
    /// Built once by [`GrammarIR::build_string_index`] after all
    /// string-mutating passes complete (typically at DAG-build time in
    /// `pipeline/compile.rs`). Provides O(1) lookup from `&str` to
    /// `StringId`, eliminating O(n) linear scans over `self.strings`.
    /// Not serialized: every compile rebuilds it from scratch.
    #[serde(skip, default)]
    pub string_index: HashMap<String, StringId>,

    /// Tranche AU.2.7 — grammar-parameterised structural alphabet.
    ///
    /// The byte set `S` of every byte that could terminate a
    /// scanner's inner loop (Alt dispatch starters, terminal
    /// literal starters, digraph first-bytes). Populated by
    /// [`passes::compute_structural_alphabet`] after
    /// `generate_dispatch_tables`. Consumed by the scanner-kernel
    /// emitters in `crates/core/src/generate/regex/emit/simd.rs`
    /// to emit grammar-specific nibble-LUTs instead of per-site
    /// ad-hoc sets. Digraph set `D` is the derived list of
    /// two-byte structural delimiters (`/*`, `*/`, `->`, etc.).
    /// Not serialized: every compile rebuilds it from scratch.
    #[serde(skip, default)]
    pub structural_alphabet: Option<passes::sets::StructuralAlphabet>,

    /// Tranche AU.6.2 — per-grammar push-site fingerprint.
    ///
    /// Static count of `(push_compound, push_leaf, push_leaf_with_*)`
    /// call sites across every emitted rule function. Populated by
    /// [`passes::compute_push_fingerprint`] after
    /// `classify_materialization` + `compute_payload_layouts`. The
    /// Rust emitter's `parse()` entry point reads the fingerprint at
    /// codegen time and picks a grammar-specific
    /// `TapeBuilder::with_capacity` divisor so `RawVec::grow_one` /
    /// `_mi_heap_realloc_zero` does not fire on the first parse.
    /// Not serialized: every compile rebuilds it from scratch.
    #[serde(skip, default)]
    pub push_fingerprint: Option<passes::sets::PushFingerprint>,
}

impl GrammarIR {
    /// Look up an interned string by its `StringId`.
    pub fn get_string(&self, id: StringId) -> &str {
        &self.strings[id as usize]
    }

    /// Reverse-lookup: find the `StringId` for an interned string.
    ///
    /// Returns `None` if the string is not in the pool. O(1) when
    /// [`build_string_index`](Self::build_string_index) has been called;
    /// falls back to O(n) linear scan otherwise (should not happen in
    /// production — the pipeline always builds the index).
    pub fn find_string_id(&self, s: &str) -> Option<StringId> {
        if !self.string_index.is_empty() {
            return self.string_index.get(s).copied();
        }
        // Fallback: linear scan (only hit if the index hasn't been built).
        self.strings
            .iter()
            .position(|existing| existing == s)
            .map(|i| i as StringId)
    }

    /// Build the reverse string index from `self.strings`.
    ///
    /// Call once after all string-mutating passes complete (typically at
    /// DAG-build time). Subsequent calls are idempotent — they rebuild
    /// the index from the current strings table.
    pub fn build_string_index(&mut self) {
        self.string_index = self
            .strings
            .iter()
            .enumerate()
            .map(|(i, s)| (s.clone(), i as StringId))
            .collect();
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

    // ── TypeDesc interner accessors (Tranche AA.1) ──────────────────────

    /// Intern a `TypeDesc`, returning its stable id. Idempotent.
    ///
    /// Callers that need reference-equality structural typing (dispatch
    /// signatures, TaggedUnion discriminants, tape view kinds) should
    /// use this rather than comparing `TypeDesc` values directly.
    pub fn intern_type(&mut self, ty: TypeDesc) -> TypeDescId {
        self.type_desc_interner.intern(ty)
    }

    /// Resolve a `TypeDescId` back to its canonical `TypeDesc`.
    /// Panics on out-of-range ids — callers should not fabricate ids.
    #[inline]
    pub fn resolve_type(&self, id: TypeDescId) -> &TypeDesc {
        self.type_desc_interner.resolve(id)
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
