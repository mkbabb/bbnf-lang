//! Regex analysis cache pass.
//!
//! Populates `GrammarIR::regex_info` by calling
//! `bbnf_regex::RegexInfo::analyze_with_cost` for each unique regex
//! pattern in the grammar. The HIR-tier extraction cost is built from
//! `ir.cost_config` so the per-compile knobs apply uniformly across
//! both tiers — see `crate::cost_config::CostConfig`. Run after all
//! regex merging and simplification passes to ensure patterns are in
//! their final form.

use std::collections::HashMap;

use bbnf_regex::egraph::SaturationCache;

use crate::{GrammarIR, IrNode, StringId};

/// Compute and cache `RegexInfo` for all regex patterns in the grammar.
///
/// Walks all rules, collects unique `Regex(StringId)` nodes, and calls
/// `bbnf_regex::RegexInfo::analyze_with_cost` for each, threading the
/// per-compile `CostConfig` through to the HIR-tier extractor. Results
/// are cached on `ir.regex_info` keyed by `StringId`.
pub fn compute_regex_info(ir: &mut GrammarIR) {
    let mut seen = HashMap::<StringId, ()>::new();
    let mut info_map = HashMap::new();

    // Collect all unique regex StringIds.
    for rule in &ir.rules {
        collect_regex_ids(&rule.body, &mut seen);
    }

    // Build the HIR-tier cost from the per-compile CostConfig: the
    // shared `weights` substrate flows from `ir.cost_config.egraph`,
    // and the HIR-specific knobs (`hir_*`) flow from the bbnf-ir
    // wrapper. Both halves come from the same struct, no
    // `Default::default` calls.
    let cost = bbnf_regex::egraph::RegexExtractionCost {
        weights: ir.cost_config.egraph.weights,
        literal_per_byte: ir.cost_config.hir_literal_per_byte,
        class_cost: ir.cost_config.hir_class_cost,
        repeat_cost: ir.cost_config.hir_repeat_cost,
        merged_bonus: ir.cost_config.hir_merged_bonus,
    };

    // Tranche X phase 3: per-compile saturation cache. JSON / CSS L4
    // grammars contain the same regex patterns in multiple positions
    // (e.g., the JSON string pattern as object key + object value +
    // array element). Without the cache each occurrence pays the full
    // build → saturate → extract → drop cycle. The cache stores the
    // canonicalized HIR keyed on the input HIR's structural hash so
    // that each unique pattern's saturation cost is paid at most once
    // per compile. Lifetime is exactly this pass — the cache drops at
    // function exit, no global state.
    let mut sat_cache = SaturationCache::with_capacity(seen.len());

    // Analyze each unique pattern with the explicit cost, threading
    // the per-compile cache through the HIR canonicalization step.
    for &sid in seen.keys() {
        let pattern = ir.get_string(sid);
        if let Some(info) =
            bbnf_regex::RegexInfo::analyze_with_cost_cached(pattern, &cost, &mut sat_cache)
        {
            info_map.insert(sid, info);
        }
    }

    ir.regex_info = info_map;
}

fn collect_regex_ids(node: &IrNode, seen: &mut HashMap<StringId, ()>) {
    match node {
        IrNode::Regex(sid) => {
            seen.entry(*sid).or_insert(());
        }
        IrNode::Seq(children) => {
            for child in children {
                collect_regex_ids(child, seen);
            }
        }
        IrNode::Alt(branches, _) => {
            for branch in branches {
                collect_regex_ids(&branch.node, seen);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner) => {
            collect_regex_ids(inner, seen);
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            collect_regex_ids(a, seen);
            collect_regex_ids(b, seen);
        }
        IrNode::Map { inner, .. } => {
            collect_regex_ids(inner, seen);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            collect_regex_ids(token, seen);
            for arm in arms {
                collect_regex_ids(&arm.continuation, seen);
            }
            collect_regex_ids(fallback, seen);
        }
        IrNode::Literal(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}
