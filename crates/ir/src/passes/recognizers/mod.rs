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
pub mod delim_scan;
mod identifier;
pub mod key_dispatch;
mod node_facts;
mod prefix_shared_group;
mod punct_ws_region;
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
    // (by reference) and returns a Vec of decisions. The orchestrator
    // merges them into `ir.node_facts` once at the end. Avoids per-
    // miner clones of `ir.regex_info`/`ir.rules`/`ir.dag` that would
    // dominate compile time on small grammars.
    //
    // The whole-DAG clone the previous version did was a workaround for
    // a borrow conflict that NLL handles cleanly: the `dag` borrow is
    // live only for the miner calls, and ends before the `&mut ir`
    // mutations below. Profile-confirmed cost on the post-V baseline:
    // 40-200 KB allocated per compile (the entire NodeId→DagNode map).
    let (additions, context_facts, delim_scan_configs, key_dispatch_configs) =
        if let Some(dag) = ir.dag.as_ref() {
            let context_facts = compute_context_facts(ir, dag);
            let mut additions: Vec<(crate::dag::NodeId, crate::passes::patterns::Recognizer)> =
                Vec::new();

            quoted_string::collect(ir, dag, &mut additions);
            balanced_wrap::collect(ir, dag, &mut additions);
            comment_ws::collect(ir, dag, &mut additions);
            identifier::collect(ir, dag, &mut additions);
            separator_list::collect(ir, dag, &mut additions);
            token_led_branches::collect(ir, dag, &context_facts, &mut additions);

            // Tranche Y.4: the three ghost-family miners that landed
            // in Tranche X.10 (function_head, hash_prefix, unit_tail)
            // produced zero matches on every production grammar —
            // CSS L4, JSON, BBNF, Sheets, EBNF — and the FunctionHead
            // kernel's emission semantics are structurally
            // incompatible with CSS functions that have bodies
            // anyway (the `try_emit_family_kernel` short-circuit
            // replaces the entire node's output, dropping the
            // function body). Y.4 deletes them outright. Only
            // `punct_ws_region` (56 matches on CSS L4) earned its
            // keep and stays.
            punct_ws_region::collect(ir, dag, &mut additions);

            // Tranche X.8a: upstream structural pattern detection.
            // Populates sidecar maps on `GrammarIR` that the backend
            // reads directly via `ir.delim_scan_configs` /
            // `ir.key_dispatch_configs`. This replaces the deleted
            // `backend/patterns/{delim_scan,key_dispatch,cache}.rs`.
            let delim_scan_configs = delim_scan::collect(ir);
            let key_dispatch_configs = key_dispatch::collect(ir);

            // `dag` and `context_facts` borrows end here (NLL).
            (
                additions,
                context_facts,
                delim_scan_configs,
                key_dispatch_configs,
            )
        } else {
            return;
        };

    for (node_id, rec) in additions {
        install_recognizer(
            &mut ir.node_facts,
            node_id,
            crate::passes::patterns::NodeKind::Leaf,
            rec,
        );
    }

    // Tranche X.8g: cache context_facts on `GrammarIR` so downstream
    // passes can read from it without recomputing.
    ir.context_facts = context_facts;
    // Tranche X.8a: commit the upstream-mined pattern configs.
    ir.delim_scan_configs = delim_scan_configs;
    ir.key_dispatch_configs = key_dispatch_configs;

    // Tranche Y.0 / Y.4: the family-recognizer flag gates the driver's
    // per-node `try_emit_family_kernel` probe. Post-Y.4 the only
    // surviving family shape is `PunctWsRegion`; grammars that match
    // it (CSS L4: 56 hits) pay the probe, those that don't (JSON,
    // BBNF, Sheets, EBNF) pay zero per-node lookup overhead.
    ir.has_family_recognizers = ir.node_facts.values().any(|facts| {
        facts
            .recognizer
            .as_ref()
            .is_some_and(|rec| {
                matches!(
                    rec.shape,
                    crate::passes::patterns::RecognizerShape::PunctWsRegion { .. }
                )
            })
    });

    prefix_shared_group::mine(ir);
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
