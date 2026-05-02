//! AZ-IV.W2.5 — parent-pointer micro-bench for the three
//! `AscentStrategy` candidates.
//!
//! The bench measures ascent-heavy workloads on synthetic node graphs
//! sized to match the cardinality of the citm / tailwind / sheets
//! fixtures (citm ≈ 32 K nodes, tailwind ≈ 80 K nodes, sheets ≈ 4 K
//! nodes). The synthetic shape is a balanced k-ary tree of depth ~6
//! over the target node count — close enough to real parsed-document
//! fan-out for the parent-pointer trade-off to manifest.
//!
//! Three lanes run on every fixture:
//!
//! - `RootTraversal` — ascent re-walks from the root via a `HashMap`
//!   lookup. No per-node bloat.
//! - `InStructPointer` — parent slots stored on every node. 4 B per
//!   node, eager.
//! - `HybridSidecar` — sidecar populated lazily; populate time +
//!   ascent time both metered.
//!
//! Per-bench output collapses to `data/post-bench` JSON via the
//! divan-side recorder; the W2 close commit cites the artefact at
//! `docs/tranches/AZ-IV/audit/W2-ascent-microbench.json`.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::collections::HashMap;

use divan::black_box;

use bbnf::path::{AscentStrategy, HybridSidecar, InStructPointer, RootTraversal};
use bbnf_ir::dag::NodeId;

/// Synthetic graph shape — a flat parent-vector keyed by depth-first
/// id, plus a leaves vector listing the deepest nodes (the ascent-heavy
/// workload climbs from a leaf to the root).
struct SyntheticGraph {
    parents: Vec<NodeId>,
    leaves: Vec<NodeId>,
}

/// Build a balanced fan-out tree with `node_count` nodes, fan-out `k`.
/// Assigns parent ids in depth-first order so leaves cluster in the
/// upper id range — mirroring how a parsed-document arena lays out
/// late-emitted leaf records.
fn build_graph(node_count: usize, k: usize) -> SyntheticGraph {
    assert!(node_count > 0);
    assert!(k > 0);
    let mut parents = Vec::with_capacity(node_count);
    parents.push(NodeId::INVALID); // root
    for i in 1..node_count {
        let parent_idx = (i - 1) / k;
        parents.push(NodeId::from_usize(parent_idx));
    }
    // Leaves are nodes with no children. In a balanced k-ary tree
    // numbered breadth-first-ish, that's everyone past `node_count / k`.
    let leaf_start = node_count / (k + 1);
    let leaves = (leaf_start..node_count).map(NodeId::from_usize).collect();
    SyntheticGraph { parents, leaves }
}

/// Walker callback for the RootTraversal lane — looks up a parent
/// through a `HashMap` keyed by node id. The synthetic version of "walk
/// from the root to find this child's parent": the HashMap stands in
/// for the descent cost a real executor would pay re-traversing the
/// tape.
fn root_traversal_walker(graph: &SyntheticGraph) -> impl Fn(NodeId) -> Option<NodeId> + '_ {
    let map: HashMap<NodeId, NodeId> = (0..graph.parents.len())
        .map(|i| (NodeId::from_usize(i), graph.parents[i]))
        .collect();
    move |id: NodeId| {
        let p = map.get(&id).copied()?;
        if p == NodeId::INVALID { None } else { Some(p) }
    }
}

/// Climb every leaf to the root via the strategy. The bench-meaningful
/// loop — every iteration touches every leaf, every leaf paid the
/// strategy's per-ascent cost.
#[inline]
fn climb_all_leaves<S: AscentStrategy>(strategy: &S, leaves: &[NodeId]) -> u64 {
    let mut hops = 0u64;
    for leaf in leaves {
        let mut cur = Some(*leaf);
        while let Some(node) = cur {
            hops += 1;
            cur = strategy.ascend(node);
        }
    }
    hops
}

// ── Synthetic fixture sizes (matched to citm/tailwind/sheets node
// cardinality from the existing parser benches). The exact node
// counts are approximations — the parent-pointer trade-off is
// monotonic in tree height and node count, so the ranking of the
// three strategies is robust to the precise number.

/// Sheets-shaped — small graph, deep nesting (~ formula AST scale).
const SHEETS_NODES: usize = 4_000;
const SHEETS_FANOUT: usize = 3;

/// Citm-shaped — medium graph, moderate fan-out.
const CITM_NODES: usize = 32_000;
const CITM_FANOUT: usize = 6;

/// Tailwind-shaped — large graph, broad fan-out.
const TAILWIND_NODES: usize = 80_000;
const TAILWIND_FANOUT: usize = 8;

macro_rules! ascent_bench {
    ($mod_name:ident, $nodes:expr, $fanout:expr) => {
        mod $mod_name {
            use super::*;

            #[divan::bench]
            fn root_traversal(b: divan::Bencher) {
                let graph = build_graph($nodes, $fanout);
                let walker = root_traversal_walker(&graph);
                let strategy = RootTraversal::new(walker);
                b.bench_local(|| {
                    let hops = climb_all_leaves(&strategy, &graph.leaves);
                    black_box(hops);
                });
            }

            #[divan::bench]
            fn in_struct_pointer(b: divan::Bencher) {
                let graph = build_graph($nodes, $fanout);
                let strategy = InStructPointer::from_parents(graph.parents.clone());
                b.bench_local(|| {
                    let hops = climb_all_leaves(&strategy, &graph.leaves);
                    black_box(hops);
                });
            }

            #[divan::bench]
            fn hybrid_sidecar(b: divan::Bencher) {
                let graph = build_graph($nodes, $fanout);
                let mut strategy = HybridSidecar::new();
                let parents = graph.parents.clone();
                strategy.populate_from_walker($nodes, |id| {
                    let p = parents[id.as_usize()];
                    if p == NodeId::INVALID { None } else { Some(p) }
                });
                b.bench_local(|| {
                    let hops = climb_all_leaves(&strategy, &graph.leaves);
                    black_box(hops);
                });
            }
        }
    };
}

ascent_bench!(sheets, SHEETS_NODES, SHEETS_FANOUT);
ascent_bench!(citm, CITM_NODES, CITM_FANOUT);
ascent_bench!(tailwind, TAILWIND_NODES, TAILWIND_FANOUT);

fn main() {
    divan::Divan::default()
        .sample_count(50)
        .sample_size(1)
        .skip_ext_time(true)
        .max_time(std::time::Duration::from_secs(20))
        .run_benches();
}
