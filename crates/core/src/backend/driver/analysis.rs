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

    // Solve Alt strategies from NodeFacts + dispatch tables.
    let alt_strategies = crate::backend::strategy::alt_strategy::solve_alt_strategies(&ir);

    PreparedGrammar {
        ir,
        prep: BackendPreparation {
            effective_prettify: config.effective_prettify,
            analysis,
            alt_strategies,
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

