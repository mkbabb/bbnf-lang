//! Shared backend preparation.
//!
//! This module owns the immutable preparation artifacts consumed by all
//! code-generating backends (Rust, TS, WASM). The generator itself should
//! remain read-only.

use std::collections::HashSet;

use bbnf_ir::dag::NodeId;
use bbnf_ir::{GrammarIR, RuleId, TypeDesc};

use crate::generate::regex;

/// Immutable type-analysis snapshot handed off to backend preparation.
#[derive(Clone, Debug, Default)]
pub struct TypeAnalysis {
    pub rule_types: Vec<(RuleId, TypeDesc)>,
    pub scratch_types: Vec<TypeDesc>,
}

impl TypeAnalysis {
    pub fn from_ir(ir: &GrammarIR) -> Self {
        Self {
            rule_types: ir.types.clone(),
            scratch_types: ir
                .type_map
                .as_ref()
                .map(|map| map.scratch_types().to_vec())
                .unwrap_or_default(),
        }
    }
}

/// Finalized AOT prep configuration.
#[derive(Clone, Debug)]
pub struct EffectiveBackendConfig {
    pub effective_prettify: bool,
    pub collapse_simple_spans: bool,
}

/// Full AOT analysis bundle handed to codegen.
#[derive(Clone, Debug, Default)]
pub struct BackendAnalysis {
    pub type_analysis: TypeAnalysis,
    pub sp_method_rules: HashSet<String>,
    pub fused_number_rules: HashSet<RuleId>,
    pub operator_chain_rules: HashSet<RuleId>,
}

/// Immutable AOT preparation snapshot produced by the shared pipeline.
#[derive(Clone, Debug, Default)]
pub struct BackendPreparation {
    pub effective_prettify: bool,
    pub analysis: BackendAnalysis,
    /// Solved Alt strategies keyed by stable `NodeId`. Built by
    /// `solve_alt_strategies`.
    pub alt_strategies:
        std::collections::HashMap<NodeId, crate::backend::strategy::alt_strategy::AltStrategy>,
    /// Pre-solved delim-scan configurations, keyed by the Wrap
    /// root node's `NodeId`. Tranche X.8b: cloned directly from
    /// `ir.delim_scan_configs` (populated upstream by
    /// `bbnf_ir::passes::recognizers::delim_scan::collect`).
    pub delim_scan_configs: std::collections::HashMap<NodeId, bbnf_ir::DelimScanConfig>,
    /// Pre-solved key-dispatch configurations, keyed by the Alt
    /// node's `NodeId`. Tranche X.8b: cloned directly from
    /// `ir.key_dispatch_configs` (populated upstream by
    /// `bbnf_ir::passes::recognizers::key_dispatch::collect`).
    pub key_dispatch_configs: std::collections::HashMap<NodeId, bbnf_ir::KeyDispatchMatch>,
}

/// Fully prepared grammar bundle consumed by codegen.
#[derive(Clone, Debug)]
pub struct PreparedGrammar {
    pub ir: GrammarIR,
    pub prep: BackendPreparation,
}

/// Prepare a fully-lowered IR for AOT code generation.
pub fn prepare_grammar(mut ir: GrammarIR, requested_prettify: bool) -> PreparedGrammar {
    let config = resolve_backend_config(&ir, requested_prettify);
    apply_ir_prep(&mut ir, &config);
    let analysis = analyze_grammar(&mut ir, &config);

    // Tranche X.8b: read authoritative decisions upstream from the IR.
    // `ir.delim_scan_configs` and `ir.key_dispatch_configs` were
    // populated during `mine_recognizers` (see
    // `bbnf_ir::passes::recognizers::{delim_scan,key_dispatch}`).
    let alt_strategies = crate::backend::strategy::alt_strategy::solve_alt_strategies(&ir);
    let delim_scan_configs = ir.delim_scan_configs.clone();
    let key_dispatch_configs = ir.key_dispatch_configs.clone();

    PreparedGrammar {
        ir,
        prep: BackendPreparation {
            effective_prettify: config.effective_prettify,
            analysis,
            alt_strategies,
            delim_scan_configs,
            key_dispatch_configs,
        },
    }
}

pub fn resolve_backend_config(ir: &GrammarIR, requested_prettify: bool) -> EffectiveBackendConfig {
    let has_pretty_directive = ir
        .rules
        .iter()
        .any(|rule| rule.meta.directives.pretty.is_some());
    let effective_prettify = requested_prettify || has_pretty_directive;

    EffectiveBackendConfig {
        effective_prettify,
        collapse_simple_spans: !effective_prettify,
    }
}

pub fn apply_ir_prep(ir: &mut GrammarIR, config: &EffectiveBackendConfig) {
    // When prettify is not enabled, clear @pretty metadata so that
    // preserve_spans is not applied — this allows span compression
    // in Seq codegen, which is critical for throughput.
    if !config.effective_prettify {
        for rule in &mut ir.rules {
            rule.meta.directives.pretty = None;
        }
    }

    // Enable simple Span collapse when prettify is disabled — allows Seqs of
    // simple Span children to collapse to a single Span, eliminating slab allocs.
    ir.collapse_simple_spans = config.collapse_simple_spans;
}

pub fn analyze_grammar(ir: &mut GrammarIR, config: &EffectiveBackendConfig) -> BackendAnalysis {
    // Compute sp_method_rules via iterative fixed-point BEFORE type inference,
    // so that project_types uses the correct has_sp_method flags for span-method override.
    bbnf_ir::passes::compute_sp_method_rules(ir);
    bbnf_ir::passes::project_types(ir);
    // AZ-IV.W2.2 — `path_check` IR pass runs after `project_types`
    // so the resolver consumes a populated `StructRegistry` and the
    // inline trace recorded by the structural normalizer loop. The
    // resolver binds every user-written source rule name to the
    // post-pipeline `RuleId` whose layout describes the rule's
    // body; W2.4's `path!` proc-macro reads the resolver to honour
    // the W2 invariant 8 ("Path resolution uses source rule names").
    {
        let trace = std::mem::take(&mut ir.inline_trace);
        let resolver = bbnf_ir::passes::run_path_check(ir, &trace);
        ir.path_check_resolver = resolver;
        ir.inline_trace = trace;
    }
    // Tranche AQ.6.B — plan aggregate payload layouts for any
    // tuple-of-scalars rule whose total packed size fits in
    // `MAX_PAYLOAD_BYTES`. Consumed by the Rust emitter and view
    // layer to materialize typed structs from a flat payload buffer.
    //
    // AW.0.5: the Rust backend supplies `RustNamedTypes` so the
    // layout pass admits `TypeDesc::Named` rules (e.g. CSS L4
    // `colorFunction` → `Color`) whose backend-resolved tuple shape
    // fits the `LARGE_PAYLOAD_MAX` cap. Per AU.4.2's stated path,
    // the resolver lives in the backend crate; IR owns only the
    // `NamedTypeResolver` trait.
    {
        let resolver = crate::backend::rust::view::named_types::RustNamedTypes::from_ir(ir);
        ir.payload_layouts = bbnf_ir::passes::compute_payload_layouts_with_resolver(ir, &resolver);
    }
    // Tranche AU.6.2 — count (push_compound, push_leaf,
    // push_leaf_with_*) sites across every emitted rule function so
    // the Rust emitter's `parse()` entry point can pick a grammar-
    // specific `Tape<R>::with_capacity` divisor. Must run after
    // `classify_materialization` (fills `ir.materialization`) and
    // `compute_payload_layouts` (fills `ir.payload_layouts`) — both
    // are already in place at this point in `analyze_grammar`.
    bbnf_ir::passes::compute_push_fingerprint(ir);

    // AW-IV.W4.3 — dedup-eligibility mining. Identifies rules whose
    // body shape qualifies for runtime bloom + GADT compound-record
    // dedup (fixed-width skeleton, ≤ MAX_DEDUP_ROWS records, likely
    // repeated payloads). The admitted set lands in
    // `ir.dedup_eligible_rules`; the IR projection folds it into
    // `GrammarProfile::dedup_eligible_rules` which the emitter reads
    // at codegen time and the walker consumes at parse time.
    ir.dedup_eligible_rules =
        bbnf_ir::passes::recognizers::dedup_eligibility::mine_dedup_eligible_rules(ir);

    let sp_method_rules = ir
        .rules
        .iter()
        .filter(|rule| rule.meta.has_sp_method)
        .map(|rule| ir.get_string(rule.name).to_string())
        .collect();

    let fused_number_rules = if config.effective_prettify {
        HashSet::new()
    } else {
        ir.rules
            .iter()
            .filter_map(|rule| match &rule.body {
                bbnf_ir::IrNode::Regex(sid)
                    if regex::is_fused_number_regex_cached(ir, ir.get_string(*sid)) =>
                {
                    Some(rule.id)
                }
                _ => None,
            })
            .collect()
    };

    let operator_chain_rules = ir
        .rules
        .iter()
        .filter(|rule| {
            ir.dag
                .as_ref()
                .and_then(|dag| dag.node_for(&rule.body))
                .and_then(|id| ir.node_facts.get(&id))
                .is_some_and(|f| f.operator_chain)
        })
        .map(|rule| rule.id)
        .collect();

    BackendAnalysis {
        type_analysis: TypeAnalysis::from_ir(ir),
        sp_method_rules,
        fused_number_rules,
        operator_chain_rules,
    }
}
