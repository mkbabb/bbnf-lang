//! Tranche V — recognizer mining pass (consolidated in Tranche Z.0,
//! extended with context-facts fusion in Tranche AF.1).
//!
//! Replaces the old `passes/patterns/recognize.rs` with a richer mining
//! pipeline that populates `NodeFacts` with both the legacy structural
//! flags (`operator_chain`, `sep_by`, `all_span_collapse`) and the
//! `Recognizer` records introduced in V.3. Since AF.1 the same walk
//! also produces `ContextFacts` (role-in-parent facts previously
//! produced by the standalone `compute_context_facts` pass).
//!
//! Ten miners run as one phase under the `mine_recognizers` entry
//! point. Mining order is load-bearing: later miners read earlier
//! miners' outputs and, on shape overlap, `install_recognizer`
//! overwrites the earlier record with the later one.
//!
//! 1. `ContextFactsMiner` — `ContextFacts` (discrimination, scan_safety, in_token_dispatch)
//! 2. `QuotedStringMiner` — RegexClass::QuotedString → Recognizer
//! 3. `BalancedWrapMiner` — Wrap(open, body, close) → DelimiterBalanced
//! 4. `CommentWsMiner` — RegexClass::WhitespaceWithBlockComment → Recognizer
//! 5. `IdentifierMiner` — RegexClass::Identifier / PrefixThenClass
//! 6. `SeparatorListMiner` — sep_by + element signature → SeparatorList
//! 7. `TokenLedBranchesMiner` — disjoint-FIRST Alt → TokenLedBranches (reads discrimination strength from `ContextFactsMiner`, same walk)
//! 8. `PunctWsRegionMiner` — ws-wrapped structural punctuation
//! 9. `DelimScanMiner` — Wrap(open, Repeat(Alt), close) → DelimScanConfig
//! 10. `KeyDispatchMiner` — Alt(Literal-led branches) → KeyDispatchMatch
//!
//! # Tranche Z.0 — single-walk consolidation
//!
//! Pre-Z, each miner implemented its own `pub(super) fn collect` that
//! walked every rule body. That meant **nine independent DAG
//! descents** per compile, each reading the same `GrammarDag` and
//! calling the same `visit_children_alt` recursor. The work differed
//! only in per-node match logic.
//!
//! Z.0 consolidates those nine walks into a single orchestrator walk
//! via the `RecognizerMiner` trait: each miner's per-node logic lives
//! in its `inspect` impl, and `walk_unified` descends the DAG once,
//! invoking every miner at every node.
//!
//! # Tranche AF.1 — context-facts fold
//!
//! Pre-AF the Z.0 walk was bracketed by a separate `compute_context_facts`
//! call that re-walked the entire DAG to produce a `ContextFactsMap`
//! consumed by `TokenLedBranchesMiner` (and cached on `ir.context_facts`
//! for downstream passes). AF.1 folds that producer into the miner
//! trait substrate as `ContextFactsMiner`, eliminating the second walk.
//! The miner writes into `MineOutputs::context_facts`, which is handed
//! to every downstream miner in the same `inspect` tick.
//!
//! # Mined sidecars
//!
//! Most miners emit into the shared `Vec<(NodeId, Recognizer)>`
//! consumed by `install_recognizer`. The two pattern-config miners
//! emit into `HashMap`-valued sidecars
//! (`delim_scan_configs` / `key_dispatch_configs`). The context-facts
//! miner emits into `context_facts`. All three streams ride the
//! unified walk via the `MineOutputs` struct.

mod balanced_wrap;
mod comment_ws;
mod context_facts_miner;
pub mod delim_scan;
mod identifier;
pub mod key_dispatch;
mod node_facts;
mod punct_ws_region;
mod quoted_string;
mod separator_list;
mod signature;
mod token_led_branches;

use std::collections::HashMap;

use crate::dag::{GrammarDag, NodeId};
use crate::passes::context::ContextFactsMap;
use crate::passes::inspect::visit_children_alt;
use crate::passes::patterns::{NodeFacts, NodeFactsMap, PatternAnnotations, Recognizer};
use crate::{DelimScanConfig, GrammarIR, IrNode, KeyDispatchMatch};

pub use signature::{compute_shape_hash, hash_recognizer_shape};

pub use balanced_wrap::BalancedWrapMiner;
pub use comment_ws::CommentWsMiner;
pub use context_facts_miner::ContextFactsMiner;
pub use delim_scan::DelimScanMiner;
pub use identifier::IdentifierMiner;
pub use key_dispatch::KeyDispatchMiner;
pub use punct_ws_region::PunctWsRegionMiner;
pub use quoted_string::QuotedStringMiner;
pub use separator_list::SeparatorListMiner;
pub use token_led_branches::TokenLedBranchesMiner;

// ── Recognizer miner substrate (Tranche Z.0 + AF.1) ────────────────────

/// Shared per-walk context passed to every `RecognizerMiner::inspect`.
///
/// Holds the borrowed grammar and its durable DAG. Miners that need
/// per-node role-in-parent information read from the `context_facts`
/// field of [`MineOutputs`] — populated in the same walk by
/// [`ContextFactsMiner`], which is invoked first at every node.
pub struct RecognizerMineCtx<'a> {
    pub ir: &'a GrammarIR,
    pub dag: &'a GrammarDag,
}

/// Aggregated outputs produced by one recognizer-mining walk.
///
/// * `recognizers` — `Vec<(NodeId, Recognizer)>` pushed by every miner
///   that produces recognizer records, installed via
///   [`install_recognizer`] after the walk completes.
/// * `delim_scan_configs` / `key_dispatch_configs` — per-node pattern
///   configs written by the two config miners.
/// * `context_facts` — per-node `ContextFacts` written by
///   [`ContextFactsMiner`] and read in-walk by downstream miners such
///   as [`TokenLedBranchesMiner`], then cached on `ir.context_facts`
///   after the walk completes.
#[derive(Default)]
pub struct MineOutputs {
    pub recognizers: Vec<(NodeId, Recognizer)>,
    pub delim_scan_configs: HashMap<NodeId, DelimScanConfig>,
    pub key_dispatch_configs: HashMap<NodeId, KeyDispatchMatch>,
    pub context_facts: ContextFactsMap,
}

/// A recognizer-mining pass. Called once per node during the unified
/// `mine_recognizers` walk.
///
/// ## Miner ordering (load-bearing)
///
/// Multiple miners may match the same node; `install_recognizer` at
/// commit-time overwrites with the later miner's record, so the call
/// order in [`mine_recognizers`] determines precedence. The sequence
/// (preserved from the pre-Z sequential version) is listed in the
/// module doc-comment above.
pub trait RecognizerMiner {
    /// Called once per DAG node with a stable `NodeId`. A miner that
    /// matches the node shape pushes its record into the appropriate
    /// `outputs` bucket. A miner that does not match is a no-op.
    fn inspect(
        &self,
        node: &IrNode,
        node_id: NodeId,
        ctx: &RecognizerMineCtx,
        outputs: &mut MineOutputs,
    );
}

/// Single-walk orchestrator: descends the IR tree once and invokes
/// every miner's `inspect` at every node with a stable `NodeId`.
///
/// Consolidates the nine per-miner walks the pre-Z version did into
/// one traversal. The `node_for` lookup happens once per node per
/// walk (previously once per miner per node — a 9x reduction in
/// hashmap accesses).
fn walk_unified(
    node: &IrNode,
    ctx: &RecognizerMineCtx,
    miners: &[&dyn RecognizerMiner],
    outputs: &mut MineOutputs,
) {
    if let Some(node_id) = ctx.dag.node_for(node) {
        for miner in miners {
            miner.inspect(node, node_id, ctx, outputs);
        }
    }
    visit_children_alt(node, |child| walk_unified(child, ctx, miners, outputs));
}

/// Tranche V — `mine_recognizers` entry point (Z.0 consolidation).
///
/// Walks every rule body once via the unified Z.0 orchestrator and
/// populates the legacy `ir.pattern_annotations` (per-rule), the
/// per-node `ir.node_facts` (NodeId-keyed), plus the sidecar config
/// maps `ir.delim_scan_configs` / `ir.key_dispatch_configs`.
pub fn mine_recognizers(ir: &mut GrammarIR) {
    let mut legacy_annotations = HashMap::new();
    let mut node_facts: NodeFactsMap = HashMap::new();

    // Phase 1: legacy per-rule annotations.
    for rule in &ir.rules {
        let mut ann = PatternAnnotations::default();
        node_facts::recognize_body(&rule.body, &mut ann, ir);
        if ann.alt_pattern.is_some() || ann.seq_pattern.is_some() || ann.is_operator_chain {
            legacy_annotations.insert(rule.id, ann);
        }
    }

    // Phase 2: per-node structural facts (operator_chain, sep_by,
    // all_span_collapse). Kept as a separate walk because it produces
    // `NodeFacts`, a different output type than the `Recognizer`
    // records the Phase 3 miners emit, and the miners read it.
    if let Some(dag) = ir.dag.as_ref() {
        for rule in &ir.rules {
            node_facts::recognize_tree(&rule.body, &mut node_facts, ir, dag);
        }
    }

    // Commit Phase 1+2 outputs so Phase 3 miners can read them.
    ir.pattern_annotations = legacy_annotations;
    ir.node_facts = node_facts;

    // Phase 3 (Tranche Z.0 + AF.1): unified single-walk recognizer mining.
    //
    // Pre-Z this ran nine independent tree descents — one per miner.
    // Z.0 collapsed them into one orchestrator walk dispatching every
    // miner at every node via the `RecognizerMiner` trait. AF.1 folds
    // the previously-separate `compute_context_facts` producer in as
    // `ContextFactsMiner`, which must run first so downstream miners
    // (`TokenLedBranchesMiner`) can read the discrimination strength
    // from `MineOutputs::context_facts` during the same `inspect`
    // tick. The per-miner match logic lives in each `impl`; the walk
    // traversal logic lives here, once.
    let outputs = if let Some(dag) = ir.dag.as_ref() {
        let ctx = RecognizerMineCtx { ir, dag };
        let punct_ws_region_miner = PunctWsRegionMiner::new();
        let miners: &[&dyn RecognizerMiner] = &[
            &ContextFactsMiner,
            &QuotedStringMiner,
            &BalancedWrapMiner,
            &CommentWsMiner,
            &IdentifierMiner,
            &SeparatorListMiner,
            &TokenLedBranchesMiner,
            &punct_ws_region_miner,
            &DelimScanMiner,
            &KeyDispatchMiner,
        ];
        let mut outputs = MineOutputs::default();
        for rule in &ir.rules {
            walk_unified(&rule.body, &ctx, miners, &mut outputs);
        }
        outputs
    } else {
        return;
    };

    for (node_id, rec) in outputs.recognizers {
        install_recognizer(
            &mut ir.node_facts,
            node_id,
            crate::passes::patterns::NodeKind::Leaf,
            rec,
        );
    }

    // Cache context_facts on `GrammarIR` so downstream passes can read
    // from it without recomputing (Tranche X.8g / AF.1 fused producer).
    ir.context_facts = outputs.context_facts;
    // Commit the upstream-mined pattern configs (Tranche X.8a).
    ir.delim_scan_configs = outputs.delim_scan_configs;
    ir.key_dispatch_configs = outputs.key_dispatch_configs;

    // Tranche Y.0 / Y.4: the family-recognizer flag gates the driver's
    // per-node `try_emit_family_kernel` probe. Post-Y.4 the only
    // surviving family shape is `PunctWsRegion`.
    ir.has_family_recognizers = ir.node_facts.values().any(|facts| {
        facts.recognizer.as_ref().is_some_and(|rec| {
            matches!(
                rec.shape,
                crate::passes::patterns::RecognizerShape::PunctWsRegion { .. }
            )
        })
    });
}

/// Insert or update a recognizer record on the given node.
///
/// Used by every miner (via the commit loop in `mine_recognizers`) to
/// write its output. If a previous miner already installed a
/// recognizer on the node, this overwrites it — the later miner has
/// the more refined shape.
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

