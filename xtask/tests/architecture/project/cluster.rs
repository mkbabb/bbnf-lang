use std::collections::BTreeSet;

use xtask::architecture::project::cluster::{ClusterSignal, cluster};
use xtask::architecture::project::cycle::component_bases;
use xtask::architecture::project::scc::{ProjectionBudget, decompose};
use xtask::architecture::schema::{
    Attribute, CanonicalText, Condition, EdgeKind, GraphArtifact, Identity, NodeKind,
    PackageFaceId, ProjectionName, ResolverId, SourceAnchor, exact_edge, exact_node,
};

use super::{articulation_graph, identity, memberships, signal_graph};

#[test]
fn cluster_rejects_incomplete_node_and_edge_domains_without_panicking() {
    let graph = signal_graph(None, false, false);
    let mut nodes = graph.node_map();
    let vertices = nodes.keys().cloned().collect::<BTreeSet<_>>();
    let edges = graph.edges().into_iter().cloned().collect::<Vec<_>>();
    let mut budget = ProjectionBudget::standard(ProjectionName::RustModule);
    let decomposition = decompose(ProjectionName::RustModule, &vertices, &edges, &mut budget)
        .expect("decomposition");
    nodes.remove(
        decomposition.components()[0]
            .members
            .first()
            .expect("member"),
    );
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        cluster(&decomposition, &nodes, &mut budget)
    }));
    let error = match result.expect("incomplete node domain must not panic") {
        Ok(_) => panic!("incomplete node domain must fail"),
        Err(error) => error,
    };
    assert!(
        error.to_string().contains("CLUSTER_DOMAIN_MISMATCH"),
        "{error:#}"
    );
}

#[test]
fn one_generation_budget_reaches_cluster_shard_bases() {
    let graph = signal_graph(None, false, false);
    let nodes = graph.node_map();
    let vertices = nodes.keys().cloned().collect::<BTreeSet<_>>();
    let edges = graph.edges().into_iter().cloned().collect::<Vec<_>>();
    let mut measured = ProjectionBudget::with_limit(ProjectionName::RustModule, usize::MAX);
    let decomposition = decompose(ProjectionName::RustModule, &vertices, &edges, &mut measured)
        .expect("decomposition");
    component_bases(&decomposition, &mut measured).expect("component bases");
    let before_cluster = measured.used();
    cluster(&decomposition, &nodes, &mut measured).expect("clusters");
    assert!(measured.used() > before_cluster);

    let mut bounded = ProjectionBudget::with_limit(ProjectionName::RustModule, before_cluster);
    let decomposition = decompose(ProjectionName::RustModule, &vertices, &edges, &mut bounded)
        .expect("decomposition at exact prefix");
    component_bases(&decomposition, &mut bounded).expect("basis at exact prefix");
    let error = match cluster(&decomposition, &nodes, &mut bounded) {
        Ok(_) => panic!("cluster must share the exhausted budget"),
        Err(error) => error,
    };
    assert!(error.to_string().contains("RESOURCE_BUDGET_EXCEEDED"));
}

#[test]
fn f16_hard_compatibility_fan_and_articulation_signals_drive_membership() {
    let baseline = memberships(&signal_graph(None, false, false));
    for attribute in [
        Attribute::Ownership {
            owner: CanonicalText::new("owner-b").expect("owner"),
        },
        Attribute::Feature {
            feature: CanonicalText::new("feature-b").expect("feature"),
        },
        Attribute::GeneratedLineage {
            lineage: CanonicalText::new("lineage-b").expect("lineage"),
        },
        Attribute::PackageFace {
            package_face: PackageFaceId::new("face-b").expect("face"),
        },
        Attribute::TestReachability { reachable: true },
        Attribute::FrontendFeature {
            feature: CanonicalText::new("frontend-b").expect("frontend"),
        },
    ] {
        assert_ne!(
            baseline,
            memberships(&signal_graph(Some(attribute), false, false))
        );
    }
    assert_ne!(baseline, memberships(&signal_graph(None, true, false)));
    let articulation = super::rust_projection(&signal_graph(None, false, true));
    assert!(
        articulation
            .clusters
            .clusters
            .iter()
            .any(|cluster| cluster.signals.contains(&ClusterSignal::Articulation))
    );
    assert_ne!(
        memberships(&articulation_graph(false)),
        memberships(&articulation_graph(true)),
        "one controlled bypass that removes articulation did not change the cut"
    );
}

#[test]
fn f16_compatible_but_disconnected_nodes_never_merge() {
    let mut graph = GraphArtifact::new();
    for key in ["a", "b", "c"] {
        let attributes = (key == "b")
            .then(|| Attribute::Feature {
                feature: CanonicalText::new("separating-feature").expect("feature"),
            })
            .into_iter()
            .collect();
        graph.exact(
            exact_node(identity(key), NodeKind::VirtualArtifact, key, attributes).expect("node"),
        );
    }
    for (from, to, resolver) in [
        ("a", "b", "ab"),
        ("b", "a", "ba"),
        ("b", "c", "bc"),
        ("c", "b", "cb"),
    ] {
        graph.exact(
            exact_edge(
                identity(from),
                identity(to),
                EdgeKind::RustUses,
                Condition::Always,
                SourceAnchor::Synthesized {
                    authority: CanonicalText::new("f16-connected").expect("authority"),
                },
                ResolverId::new(resolver).expect("resolver"),
                Vec::new(),
            )
            .expect("edge"),
        );
    }
    graph.canonicalize().expect("graph");
    assert!(
        memberships(&graph)
            .iter()
            .all(|cluster| !(cluster.contains(&identity("a")) && cluster.contains(&identity("c"))))
    );
}

#[test]
fn f16_production_package_face_identity_is_a_hard_signature() {
    let face = |name: &str| Identity::PackageFace {
        package_face: PackageFaceId::new(name).expect("face"),
    };
    let mut graph = GraphArtifact::new();
    for name in ["face-a", "face-b"] {
        graph.exact(
            exact_node(face(name), NodeKind::PackageFace, name, Vec::new()).expect("face node"),
        );
    }
    for (from, to, resolver) in [
        ("face-a", "face-b", "face-ab"),
        ("face-b", "face-a", "face-ba"),
    ] {
        graph.exact(
            exact_edge(
                face(from),
                face(to),
                EdgeKind::RustUses,
                Condition::Always,
                SourceAnchor::Synthesized {
                    authority: CanonicalText::new("f16-package-face").expect("authority"),
                },
                ResolverId::new(resolver).expect("resolver"),
                Vec::new(),
            )
            .expect("edge"),
        );
    }
    graph.canonicalize().expect("graph");
    assert!(
        memberships(&graph).iter().all(|cluster| cluster.len() == 1),
        "distinct production package-face identities merged"
    );
}
