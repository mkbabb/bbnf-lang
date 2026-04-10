//! Grammar e-graph — permanent secondary equivalence discoverer
//! running on the normalized IR.
//!
//! # Role in the four-layer optimizer
//!
//! The structural normalizer (`passes/transform/*`, `passes/regex/*`,
//! `passes/prefix.rs`) is the primary cross-rule optimizer. It iterates
//! the inline→merge→factor→inline cascade to fixed point — work that
//! one-pass equality saturation architecturally cannot express.
//!
//! The grammar e-graph runs **once** after the normalizer converges,
//! catching equivalences the normalizer's fixed pass order misses via
//! e-class canonical matching, and picking cost-cheapest canonical
//! forms via `GrammarCostModel` (the shared cost model, also consumed
//! by the bbnf-regex HIR e-graph in Tranche H and by CSP strategy
//! solvers).
//!
//! ```text
//!   normalized IR          EGraph<GrammarENode>          optimized IR
//!   ─────────────►         ──────────────────►           ─────────────►
//!       build                  saturate                 extract + write-back
//! ```
//!
//! Wraps the general-purpose `egraph` crate with a grammar-specific
//! e-node type (`GrammarENode`), rewrite rules (`rules/`), and the
//! cost model (`cost::GrammarCostModel`). The substrate analysis is
//! `egraph::NoAnalysis` — no per-class lattice data is consumed by
//! any rule, cost model, or extractor.

mod build_egraph;
mod cost;
mod interner;
pub mod node;
pub mod rules;
mod write_back;

pub use cost::GrammarCostModel;
pub use interner::SharedStrings;
pub use node::GrammarENode;
pub use rules::default_rules;
pub use write_back::{extract_ir_node, write_back_optimized};

use egraph::{EGraph, NoAnalysis};

use crate::GrammarIR;

/// Build an e-graph from the IR and saturate it with default rules.
/// Returns the populated e-graph, the shared string pool (which may
/// contain new entries produced by string-mutating rules like
/// `MergeLiterals`), and the per-rule body root ids captured during
/// insertion. Consumers use the returned `rule_body_ids` to feed
/// `write_back_optimized`.
pub fn build_and_saturate(
    ir: &GrammarIR,
) -> (
    EGraph<GrammarENode, NoAnalysis>,
    SharedStrings,
    rustc_hash::FxHashMap<crate::RuleId, egraph::Id>,
) {
    use rustc_hash::FxHashMap;
    use crate::RuleId;

    let pool = SharedStrings::from_ir(ir);

    // Pre-size the e-graph hash-cons table from the IR node count. Without
    // this, FxHashMap settles at the next power of two and the dropped
    // capacity becomes the dominant cost on small grammars (the BBNF
    // ~300-node case where the post-V profile measured drop_in_place at
    // 25.52% self time).
    let expected_nodes: usize = ir
        .rules
        .iter()
        .map(|r| crate::types::count_nodes(&r.body))
        .sum();
    let mut egraph: EGraph<GrammarENode, NoAnalysis> =
        EGraph::with_capacity(expected_nodes);

    let rule_root_ids = build_egraph::insert_ir(&mut egraph, ir);
    egraph.rebuild();

    // Map RuleId → root e-class Id using the Vec<Id> insert_ir
    // returned — the i-th entry is the root of ir.rules[i].body.
    let rule_body_ids: FxHashMap<RuleId, egraph::Id> = ir
        .rules
        .iter()
        .enumerate()
        .filter_map(|(i, r)| rule_root_ids.get(i).map(|&id| (r.id, id)))
        .collect();

    let rules = default_rules(ir, &pool, rule_body_ids.clone());
    let rule_refs: Vec<&dyn egraph::RewriteFn<GrammarENode, NoAnalysis>> =
        rules.iter().map(|r| r.as_ref()).collect();

    // Read scheduler caps from the per-compile cost config — single
    // source of truth for iter_limit and node_limit.
    let scheduler = egraph::CspScheduler::from_config(&ir.cost_config.egraph);
    use egraph::Scheduler;
    let _report = scheduler.run(&mut egraph, &rule_refs);
    if std::env::var("BBNF_EGRAPH_REPORT").is_ok() {
        eprintln!(
            "egraph saturation: rules={} iters={} applied={} initial_nodes={} final_nodes={} final_classes={} saturated={} iter_hit={} growth_hit={}",
            rule_refs.len(),
            _report.iterations,
            _report.total_applied,
            egraph.total_nodes(),
            _report.final_nodes,
            _report.final_classes,
            _report.saturated,
            _report.iter_limit_hit,
            _report.growth_limit_hit,
        );
        for (rule_name, work) in &_report.per_rule {
            eprintln!("  rule={} work={}", rule_name, work);
        }
    }
    // Drop the rules explicitly so they release their `SharedStrings`
    // clones, letting `into_vec()` unwrap without a fallback clone.
    drop(rule_refs);
    drop(rules);

    (egraph, pool, rule_body_ids)
}
