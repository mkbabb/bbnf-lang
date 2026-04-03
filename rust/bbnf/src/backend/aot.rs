//! AOT backend preparation.
//!
//! This module owns the immutable preparation artifacts consumed by the AOT
//! code generator. The generator itself should remain read-only.

use std::collections::HashSet;

use bbnf_ir::{GrammarIR, RuleId, TypeDesc};

use crate::generate::regex;

/// Immutable type-analysis snapshot handed off to AOT preparation.
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
pub struct EffectiveAotConfig {
    pub effective_prettify: bool,
    pub collapse_simple_spans: bool,
}

/// Full AOT analysis bundle handed to codegen.
#[derive(Clone, Debug, Default)]
pub struct AotAnalysis {
    pub type_analysis: TypeAnalysis,
    pub sp_method_rules: HashSet<String>,
    pub fused_number_rules: HashSet<RuleId>,
    pub operator_chain_rules: HashSet<RuleId>,
}

/// Immutable AOT preparation snapshot produced by the shared pipeline.
#[derive(Clone, Debug, Default)]
pub struct AotPreparation {
    pub effective_prettify: bool,
    pub analysis: AotAnalysis,
}

/// Fully prepared grammar bundle consumed by codegen.
#[derive(Clone, Debug)]
pub struct PreparedAotGrammar {
    pub ir: GrammarIR,
    pub prep: AotPreparation,
}

/// Prepare a fully-lowered IR for AOT code generation.
pub fn prepare_aot(mut ir: GrammarIR, requested_prettify: bool) -> PreparedAotGrammar {
    let config = resolve_aot_config(&ir, requested_prettify);
    apply_aot_ir_prep(&mut ir, &config);
    let analysis = analyze_aot(&mut ir, &config);

    PreparedAotGrammar {
        ir,
        prep: AotPreparation {
            effective_prettify: config.effective_prettify,
            analysis,
        },
    }
}

pub fn resolve_aot_config(ir: &GrammarIR, requested_prettify: bool) -> EffectiveAotConfig {
    let has_pretty_directive = ir
        .rules
        .iter()
        .any(|rule| rule.meta.directives.pretty.is_some());
    let effective_prettify = requested_prettify || has_pretty_directive;

    EffectiveAotConfig {
        effective_prettify,
        collapse_simple_spans: !effective_prettify,
    }
}

pub fn apply_aot_ir_prep(ir: &mut GrammarIR, config: &EffectiveAotConfig) {
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

pub fn analyze_aot(ir: &mut GrammarIR, config: &EffectiveAotConfig) -> AotAnalysis {
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
                    if regex::is_fused_number_regex(ir.get_string(*sid)) =>
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
        .filter(|rule| matches_operator_chain(&rule.body))
        .map(|rule| rule.id)
        .collect();

    AotAnalysis {
        type_analysis: TypeAnalysis::from_ir(ir),
        sp_method_rules,
        fused_number_rules,
        operator_chain_rules,
    }
}

fn matches_operator_chain(node: &bbnf_ir::IrNode) -> bool {
    let bbnf_ir::IrNode::Seq(children) = node else {
        return false;
    };
    if children.len() != 2 {
        return false;
    }

    let bbnf_ir::IrNode::Repeat { inner, lo, hi } = &children[1] else {
        return false;
    };
    if *lo != 0 || *hi != u32::MAX {
        return false;
    }

    let bbnf_ir::IrNode::Seq(link_children) = inner.as_ref() else {
        return false;
    };
    link_children.len() == 2
}
