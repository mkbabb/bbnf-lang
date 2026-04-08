//! Grammar e-graph — equality saturation over IR nodes.
//!
//! Wraps the general-purpose `egraph` crate with a grammar-specific e-node
//! type (`GrammarENode`), analysis (`GrammarAnalysis`), rewrite rules, and
//! an extraction cost model (`GrammarCostModel`).
//!
//! # Pipeline integration
//!
//! ```text
//!      IrNode tree            EGraph<GrammarENode>        optimized IrNode tree
//!      ──────────────►        ──────────────────────►     ──────────────────────►
//!        build                    saturate                  extract + write-back
//! ```
//!
//! Consumers invoke `run_grammar_egraph(&mut ir)` as a drop-in that builds
//! the e-graph, runs equality saturation with default rules, extracts the
//! cheapest form per rule, and writes the result back into `ir.rules`.
//!
//! This coexists with the current destructive passes. The caller picks
//! which pipeline to run — the e-graph path is opt-in via
//! `bbnf_ir::egraph::run_grammar_egraph` while the existing
//! `pipeline::compile` loop remains the default.

mod analysis;
mod build_egraph;
mod cost;
mod interner;
mod node;
mod rules;

pub use analysis::GrammarAnalysis;
pub use cost::GrammarCostModel;
pub use interner::SharedStrings;
pub use node::GrammarENode;
pub use rules::{default_rules, normalize_rules};

use egraph::EGraph;

use crate::GrammarIR;

/// Run the grammar e-graph pipeline: build → saturate → extract → write-back.
///
/// Experimental — runs alongside the existing destructive passes. Opt-in
/// via callers that want non-destructive optimization with cost-model-guided
/// extraction.
pub fn run_grammar_egraph(ir: &mut GrammarIR) {
    let _ = build_and_saturate(ir);
}

/// Build an e-graph from the IR and saturate it with default rules.
/// Returns the populated e-graph together with the shared string pool
/// (which may contain new entries produced by string-mutating rules
/// like `MergeLiterals`).
pub fn build_and_saturate(
    ir: &GrammarIR,
) -> (EGraph<GrammarENode, GrammarAnalysis>, SharedStrings) {
    let pool = SharedStrings::from_ir(ir);
    let mut egraph: EGraph<GrammarENode, GrammarAnalysis> = EGraph::new();
    build_egraph::insert_ir(&mut egraph, ir);
    egraph.rebuild();

    let rules = default_rules(ir, &pool);
    let rule_refs: Vec<&dyn egraph::RewriteFn<GrammarENode, GrammarAnalysis>> =
        rules.iter().map(|r| r.as_ref()).collect();

    let scheduler = egraph::BackoffScheduler::default();
    use egraph::Scheduler;
    let _report = scheduler.run(&mut egraph, &rule_refs);
    // Drop the rules explicitly so they release their `SharedStrings`
    // clones, letting `into_vec()` unwrap without a fallback clone.
    drop(rule_refs);
    drop(rules);

    (egraph, pool)
}
