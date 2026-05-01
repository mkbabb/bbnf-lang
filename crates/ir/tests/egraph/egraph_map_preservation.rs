//! AZ-IV.W0.4 — `Map { fn_id }` preservation through extraction.
//!
//! Pinned regression: when an e-class becomes equivalent to its inner
//! payload class — the named-color emitter pattern out of AZ-III's
//! W3c.1 closure — the cost-guided extractor used to drop the
//! `Map { fn_id }` wrapper and elect the cheaper non-Map form. The
//! semantic transformation that the host fn carried then vanished
//! from the optimized IR; runtime activation of the alt_dispatch
//! named_color row stalled because the wrapper never resurfaced.
//!
//! The fix lives at the cost layer: `Map { fn_id, inner }` is a
//! semantically distinct wrapper, never extractionally interchangeable
//! with `inner`. The cost model now treats Map as a hard preserve —
//! when its class also carries a non-Map equivalent, the wrapper still
//! wins on cost. The test below seeds exactly that contention.

use egraph::{EGraph, Extractor, NoAnalysis};

use bbnf_ir::egraph::GrammarCostModel;
use bbnf_ir::egraph::node::GrammarENode;

/// Forge the contention: a `Map { fn_id, inner }` is unioned with a
/// non-Map e-node so the resulting class carries both shapes. The
/// inner payload sits in its own untouched class so the cost model's
/// fixed-point iteration converges, mirroring AZ-III W3c.1's named-
/// color emitter site where post-saturation rewrites parked the
/// wrapped and unwrapped forms in the same e-class. Assert the
/// extractor elects the Map node — the wrapper carries the host fn
/// that downstream codegen consumes.
#[test]
fn map_wrapper_preserved_when_inner_equivalent_in_class() {
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();

    // Inner payload class — a Literal stands in for the wrapped
    // grammar fragment (e.g. a `ColorRef` rule body). Lives in its
    // own class so Map.inner does not self-reference its outer class.
    let inner_payload = eg.add(GrammarENode::Literal(0));

    // Map wraps the inner via fn_id 7 (the host fn that synthesizes
    // the typed payload — named_color, css_color, etc.).
    let map_id = eg.add(GrammarENode::Map {
        inner: inner_payload,
        fn_id: 7,
    });

    // A bare equivalent: a different leaf node lives in its own class
    // until the union below.
    let bare_form = eg.add(GrammarENode::Literal(1));

    // Force the contention. Production rewrites collapse the wrapped
    // form (`Map { f, …}`) and an equivalent bare form into the same
    // class — what survives extraction is the question this test
    // pins. The union merges Map's class with `bare_form`'s class
    // without disturbing `inner_payload`, so Map's `inner` field
    // still points outside the merged class and the cost model's
    // fixed point has no self-cycle.
    eg.union(map_id, bare_form);
    eg.rebuild();

    let canonical = eg.find_ref(map_id);

    // Sanity: both shapes share the unified class.
    let class_nodes: Vec<&GrammarENode> = eg.class(canonical).iter().collect();
    assert!(
        class_nodes
            .iter()
            .any(|n| matches!(n, GrammarENode::Map { fn_id: 7, .. })),
        "Map node missing from unified class: {:?}",
        class_nodes
    );
    assert!(
        class_nodes
            .iter()
            .any(|n| matches!(n, GrammarENode::Literal(1))),
        "bare-form Literal missing from unified class: {:?}",
        class_nodes
    );
    assert_ne!(
        eg.find_ref(inner_payload),
        canonical,
        "test invariant: inner payload must live outside the unified class"
    );

    // Extract under the production grammar cost model.
    let cost = GrammarCostModel::default();
    let extractor = Extractor::new(&eg, &cost);
    let best = extractor
        .best_node(canonical)
        .expect("extractor must elect a best node for the unified class");

    // Hard gate: the elected best must be the Map wrapper. A non-Map
    // election strips the host-fn semantic transformation — the exact
    // bug AZ-III W3c.1 routed forward.
    match best {
        GrammarENode::Map { fn_id, inner } => {
            assert_eq!(*fn_id, 7, "extracted Map carries the wrong fn_id");
            assert_eq!(
                eg.find_ref(*inner),
                eg.find_ref(inner_payload),
                "extracted Map's inner should canonicalize to the original payload class"
            );
        }
        other => panic!(
            "extraction stripped the Map wrapper; elected non-Map node {:?} \
             from a class containing Map {{ fn_id: 7, .. }}. \
             The host-fn semantic transformation has been silently dropped.",
            other
        ),
    }
}
