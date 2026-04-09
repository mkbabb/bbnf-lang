//! Tranche V — recognizer mining pass.
//!
//! Replaces the old `passes/patterns/recognize.rs` with a richer mining
//! pipeline that populates `NodeFacts` with both the legacy structural
//! flags (`operator_chain`, `sep_by`, `all_span_collapse`) AND the new
//! `Recognizer` record introduced in V.3.
//!
//! Eight miners run as one phase under the `mine_recognizers` entry
//! point. Mining order is load-bearing: later miners read earlier
//! miners' outputs.
//!
//! 1. `mine_node_facts`           — operator_chain, sep_by, all_span_collapse
//! 2. `mine_quoted_string`        — RegexClass::QuotedString → Recognizer
//! 3. `mine_balanced_wrap`        — Wrap(open, body, close) → DelimiterBalanced
//! 4. `mine_comment_aware_ws`     — RegexClass::WsBlockComment → Recognizer
//! 5. `mine_identifier_family`    — RegexClass::Identifier / PrefixThenClass
//! 6. `mine_separator_list`       — sep_by + element signature → SeparatorList
//! 7. `mine_token_led_branches`   — disjoint-FIRST Alt → TokenLedBranches
//! 8. `mine_prefix_shared_group`  — cross-rule signature dedup → peer_group
//!
//! All miners are single DAG walks, NodeId-keyed, O(N) per pass. The
//! cross-rule miner (`mine_prefix_shared_group`) is O(N) average-case
//! via hash-map sharing.
//!
//! `RegexInfo.feasible_engines` is populated upstream in
//! `bbnf_regex::RegexInfo::analyze_from_hir` (Tranche V.2). The
//! grammar-tier mining pass treats it as a precomputed input.

mod balanced_wrap;
mod comment_ws;
mod identifier;
mod node_facts;
mod prefix_shared_group;
mod quoted_string;
mod separator_list;
mod signature;
mod token_led_branches;

use crate::GrammarIR;
use crate::passes::context::compute_context_facts;
use crate::passes::patterns::{NodeFacts, NodeFactsMap, PatternAnnotations};
use std::collections::HashMap;

pub use signature::{compute_shape_hash, hash_recognizer_shape};

/// Tranche V — replaces `recognize_patterns`.
///
/// Walks every rule body once and populates the legacy
/// `ir.pattern_annotations` (per-rule) plus the per-node `ir.node_facts`
/// (NodeId-keyed). Subsequent miners read those facts and refine them
/// with `Recognizer` records.
pub fn mine_recognizers(ir: &mut GrammarIR) {
    let mut legacy_annotations = HashMap::new();
    let mut node_facts: NodeFactsMap = HashMap::new();

    // Phase 1: legacy per-rule annotations + per-node shape facts.
    for rule in &ir.rules {
        let mut ann = PatternAnnotations::default();
        node_facts::recognize_body(&rule.body, &mut ann, ir);
        if ann.alt_pattern.is_some() || ann.seq_pattern.is_some() || ann.is_operator_chain {
            legacy_annotations.insert(rule.id, ann);
        }
    }

    // Phase 2: per-node facts (require the durable DAG).
    if let Some(dag) = ir.dag.as_ref() {
        for rule in &ir.rules {
            node_facts::recognize_tree(&rule.body, &mut node_facts, ir, dag);
        }
    }

    // Commit phase 1+2 outputs so subsequent miners can read them.
    ir.pattern_annotations = legacy_annotations;
    ir.node_facts = node_facts;

    // Phase 3: recognizer-shape miners. Each miner walks the DAG once
    // and refines `node_facts.recognizer` for nodes it knows about.
    //
    // Context facts are computed once here and passed to the miners
    // that need them (token_led_branches reads `discrimination`).
    if let Some(dag) = ir.dag.clone() {
        let context_facts = compute_context_facts(ir, &dag);

        quoted_string::mine(ir);
        balanced_wrap::mine(ir);
        comment_ws::mine(ir);
        identifier::mine(ir);
        separator_list::mine(ir);
        token_led_branches::mine(ir, &context_facts);
        prefix_shared_group::mine(ir);
    }
}

/// Insert or update a recognizer record on the given node.
///
/// Used by every miner to write its output. If a previous miner already
/// installed a recognizer on the node, this overwrites it (the later
/// miner has the more refined shape).
pub(crate) fn install_recognizer(
    facts: &mut NodeFactsMap,
    node_id: crate::dag::NodeId,
    kind_for_default: crate::passes::patterns::NodeKind,
    recognizer: crate::passes::patterns::Recognizer,
) {
    facts
        .entry(node_id)
        .or_insert_with(|| NodeFacts {
            node_kind: kind_for_default,
            operator_chain: false,
            sep_by: false,
            all_span_collapse: false,
            recognizer: None,
        })
        .recognizer = Some(recognizer);
}

/// Walk the children of an `IrNode`, invoking `f` on each. Shared by
/// every recognizer-shape miner so they don't each re-implement the
/// recursion logic.
pub(crate) fn visit_children_alt(node: &crate::IrNode, mut f: impl FnMut(&crate::IrNode)) {
    use crate::IrNode;
    match node {
        IrNode::Seq(children) => {
            for c in children {
                f(c);
            }
        }
        IrNode::Alt(branches, _) => {
            for b in branches {
                f(&b.node);
            }
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            f(a);
            f(b);
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => {
            f(inner);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            f(token);
            for arm in arms {
                f(&arm.continuation);
            }
            f(fallback);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}
