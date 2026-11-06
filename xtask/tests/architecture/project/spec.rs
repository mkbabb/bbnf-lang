use std::path::{Path, PathBuf};

use xtask::architecture::project::project;
use xtask::architecture::schema::{
    CanonicalText, GraphArtifact, NodeKind, Obligation, ObligationCode, ObligationSubject,
    PipelineStage, ProjectionName, SourceAnchor, exact_node,
};
use xtask::architecture::test_support::RunEpoch;

use super::{graph, identity, multigraph, multigraph_seed, rust_projection};

#[test]
fn f01_required_failure_denies_movement_in_every_view() {
    let mut graph = graph("resolver", false, false);
    graph.unknown(
        Obligation::required(
            ObligationCode::RustSemanticsUnproven,
            ObligationSubject::new("outside-containment").expect("subject"),
            vec![ProjectionName::RustModule],
            PipelineStage::Rust,
            SourceAnchor::Synthesized {
                authority: CanonicalText::new("f01").expect("authority"),
            },
            CanonicalText::new("required failure").expect("reason"),
        )
        .expect("obligation"),
    );
    graph.canonicalize().expect("graph");
    let projections = project(&graph).expect("projections");
    assert!(projections.projections.iter().all(|projection| {
        !projection.movement_authorized
            && !projection.quotient.movement_authorized
            && projection
                .clusters
                .clusters
                .iter()
                .all(|cluster| !cluster.movement_authorized)
    }));
}

#[test]
fn f02_structurally_green_graph_cannot_self_authorize_movement() {
    let projections = project(&graph("resolver", true, false)).expect("projections");
    assert!(
        projections
            .projections
            .iter()
            .all(|projection| projection.structurally_valid_for_selected_edges)
    );
    assert!(
        projections
            .projections
            .iter()
            .all(|projection| !projection.movement_authorized)
    );
}

#[test]
fn semantic_hash_is_sha256_and_binds_resolver() {
    let first = rust_projection(&graph("resolver-a", true, false));
    let second = rust_projection(&graph("resolver-b", true, false));
    assert_ne!(first.hash, second.hash);
    assert_eq!(first.hash.as_str().len(), 64);
}

#[test]
fn scc_identity_is_projection_qualified_and_basis_is_edge_typed() {
    let projection = rust_projection(&graph("resolver", true, true));
    let component = projection
        .components
        .iter()
        .find(|item| item.cyclic)
        .expect("cycle");
    assert_eq!(component.identity.projection, ProjectionName::RustModule);
    let basis = projection
        .component_bases
        .iter()
        .find(|basis| basis.component == component.identity)
        .expect("basis");
    assert!(!basis.cycles.is_empty());
    assert!(basis.cycles.iter().all(|witness| {
        witness
            .edges
            .iter()
            .all(|edge| edge.as_str().starts_with("edge:sha256:"))
    }));
}

#[test]
fn f15_k3_self_loops_parallel_edges_and_input_order_have_exact_gf2_rank() {
    let first = rust_projection(&multigraph(false));
    let second = rust_projection(&multigraph(true));
    let basis = first.component_bases.first().expect("basis");
    assert_eq!(basis.rank, 6);
    assert_eq!(basis.cycles.len(), 6);
    assert_eq!(first.component_bases, second.component_bases);
    assert_eq!(first.hash, second.hash);
}

pub(super) fn p3_one_thousand_ordering_scenario() {
    let expected = rust_projection(&multigraph(false)).hash;
    for seed in 0..1_000u64 {
        assert_eq!(
            rust_projection(&multigraph_seed(seed)).hash,
            expected,
            "seed {seed}"
        );
    }
}

#[test]
#[ignore = "full captured constellation gate; set BBNF_ARCHITECTURE_FULL_GENERATION"]
fn one_thousand_full_captured_fact_orderings_have_one_exact_graph_output() {
    let generation = std::env::var_os("BBNF_ARCHITECTURE_FULL_GENERATION")
        .map(PathBuf::from)
        .expect("BBNF_ARCHITECTURE_FULL_GENERATION must name one complete generation directory");
    p3_one_thousand_full_artifact_ordering_scenario(&generation);
}

pub(super) fn p3_one_thousand_full_artifact_ordering_scenario(generation: &Path) {
    use xtask::architecture::evidence::binding::SourceBinding;
    use xtask::architecture::evidence::write::{EvidencePayload, GenerationStatus, assemble};
    use xtask::architecture::inventory::Inventory;
    use xtask::architecture::manifest::ConstellationManifest;

    let read = |name: &str| std::fs::read(generation.join(name)).expect("generation member");
    let binding: SourceBinding = serde_json::from_slice(&read("binding.json")).expect("binding");
    let manifest: ConstellationManifest =
        serde_json::from_slice(&read("manifest.json")).expect("manifest");
    let inventory: Inventory = serde_json::from_slice(&read("inventory.json")).expect("inventory");
    let original: GraphArtifact =
        serde_json::from_slice(&read("graph.json")).expect("full captured graph");
    assert!(
        original.truth.len() > 1,
        "captured graph has no ordering domain"
    );
    assert!(
        !original.nodes().is_empty(),
        "captured graph has no real nodes"
    );
    assert!(
        !original.edges().is_empty(),
        "captured graph has no real edges"
    );
    let derive = |mut graph: GraphArtifact| {
        graph.canonicalize().expect("canonical graph");
        let projections = project(&graph).expect("full projections");
        let failures = graph.required_failures();
        assemble(EvidencePayload {
            binding: &binding,
            manifest: Some(&manifest),
            inventory: Some(&inventory),
            graph: Some(&graph),
            graph_validated: true,
            projections: Some(&projections),
            failures: Some(&failures),
            diagnostics: Vec::new(),
            stage: PipelineStage::Complete,
            status: GenerationStatus::Red,
        })
        .expect("complete generation assembly")
        .files
    };
    let expected = derive(original.clone());
    let mut distinct_input_observed = false;
    for seed in 0..1_000u64 {
        let mut candidate = original.clone();
        permute(&mut candidate.truth, seed);
        distinct_input_observed |= candidate.truth != original.truth;
        assert_eq!(derive(candidate), expected, "full derivation seed {seed}");
    }
    assert!(
        distinct_input_observed,
        "the proof never changed its captured input"
    );
}

fn permute<T>(items: &mut [T], seed: u64) {
    let mut state = seed ^ 0x9e37_79b9_7f4a_7c15;
    for index in (1..items.len()).rev() {
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        items.swap(index, (state as usize) % (index + 1));
    }
}

#[test]
fn f19_empty_edge_projections_retain_total_implicit_singletons() {
    let mut graph = GraphArtifact::new();
    for index in 0..100 {
        let key = format!("isolated-{index:03}");
        graph.exact(
            exact_node(identity(&key), NodeKind::VirtualArtifact, key, Vec::new()).expect("node"),
        );
    }
    graph.canonicalize().expect("graph");
    let projections = project(&graph).expect("projections");
    assert_eq!(projections.node_count, 100);
    assert!(projections.projections.iter().all(|projection| {
        projection.components.is_empty()
            && projection.clusters.clusters.is_empty()
            && projection.quotient.edges.is_empty()
            && projection.node_count == 100
    }));
    xtask::architecture::evidence::validate::validate_structural(&graph, &projections)
        .expect("empty-edge projections are structurally valid");
    assert!(serde_json::to_vec(&projections).expect("JSON").len() < 100_000);
}

#[test]
fn f18_projection_basis_cluster_and_quotient_failpoints_are_operation_local() {
    let graph = graph("resolver", true, true);
    for failpoint in [
        "projection-build",
        "cycle-basis",
        "dynamic-cluster",
        "quotient",
    ] {
        let epoch = RunEpoch::for_test(Some(failpoint), None).expect("projection epoch");
        let scope = epoch.scope().expect("projection epoch scope");
        let error = project(&graph).expect_err(failpoint);
        drop(scope);
        assert_eq!(
            error.to_string(),
            format!("INJECTED_FAILURE at real operation `{failpoint}`"),
            "{error:#}"
        );
        crate::assert_epoch(&epoch, 1, false, false);
    }

    for (requested, stage, context) in [
        ("cycle", PipelineStage::Cycle, "PIPELINE_STAGE_CYCLE"),
        ("cluster", PipelineStage::Cluster, "PIPELINE_STAGE_CLUSTER"),
    ] {
        let epoch = RunEpoch::for_test(None, Some(requested)).expect("projection stage epoch");
        let scope = epoch.scope().expect("projection stage epoch scope");
        let error = project(&graph).expect_err(requested);
        assert_eq!(
            error.chain().map(ToString::to_string).collect::<Vec<_>>(),
            [
                context.to_owned(),
                format!("INJECTED_FAILURE at typed stage {stage:?}"),
            ],
            "{error:#}"
        );
        project(&graph).expect("typed projection-stage injection is one-shot");
        drop(scope);
        crate::assert_epoch(&epoch, 1, false, false);
    }
}
