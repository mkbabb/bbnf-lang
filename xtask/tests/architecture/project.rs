use xtask::architecture::project::project;
use xtask::architecture::schema::{
    Attribute, CanonicalText, Condition, EdgeKind, GraphArtifact, Identity, NodeKind,
    ProjectionName, ResolverId, SourceAnchor, exact_edge, exact_node,
};

#[path = "project/cluster.rs"]
mod cluster;
#[path = "project/cycle.rs"]
mod cycle;
#[path = "project/quotient.rs"]
mod quotient;
#[path = "project/scc.rs"]
mod scc;
#[path = "project/spec.rs"]
mod spec;

pub(super) fn p3_one_thousand_ordering_scenario() {
    spec::p3_one_thousand_ordering_scenario();
}

pub(crate) fn identity(key: &str) -> Identity {
    Identity::Virtual {
        namespace: CanonicalText::new("test").expect("namespace"),
        key: CanonicalText::new(key).expect("key"),
    }
}

pub(crate) fn graph(resolver: &str, cut: bool, cycle: bool) -> GraphArtifact {
    let mut graph = GraphArtifact::new();
    for key in ["a", "b"] {
        graph.exact(
            exact_node(identity(key), NodeKind::VirtualArtifact, key, Vec::new()).expect("node"),
        );
    }
    if cut || cycle {
        graph.exact(
            exact_edge(
                identity("a"),
                identity("b"),
                EdgeKind::RustUses,
                Condition::Always,
                SourceAnchor::Synthesized {
                    authority: CanonicalText::new("test").expect("authority"),
                },
                ResolverId::new(resolver).expect("resolver"),
                Vec::new(),
            )
            .expect("edge"),
        );
    }
    if cycle {
        graph.exact(
            exact_edge(
                identity("b"),
                identity("a"),
                EdgeKind::RustUses,
                Condition::Always,
                SourceAnchor::Synthesized {
                    authority: CanonicalText::new("test").expect("authority"),
                },
                ResolverId::new("reverse").expect("resolver"),
                Vec::new(),
            )
            .expect("edge"),
        );
    }
    graph.canonicalize().expect("graph");
    graph
}

pub(crate) fn rust_projection(graph: &GraphArtifact) -> xtask::architecture::project::Projection {
    project(graph)
        .expect("projection set")
        .projections
        .into_iter()
        .find(|item| item.spec.name == ProjectionName::RustModule)
        .expect("Rust projection")
}

pub(crate) fn multigraph(reverse: bool) -> GraphArtifact {
    let mut rows = vec![
        ("a", "b", "ab"),
        ("b", "a", "ba"),
        ("a", "c", "ac"),
        ("c", "a", "ca"),
        ("b", "c", "bc"),
        ("c", "b", "cb"),
        ("a", "a", "aa"),
        ("a", "b", "ab-parallel"),
    ];
    if reverse {
        rows.reverse();
    }
    multigraph_rows(rows)
}

pub(crate) fn multigraph_seed(mut seed: u64) -> GraphArtifact {
    let mut rows = vec![
        ("a", "b", "ab"),
        ("b", "a", "ba"),
        ("a", "c", "ac"),
        ("c", "a", "ca"),
        ("b", "c", "bc"),
        ("c", "b", "cb"),
        ("a", "a", "aa"),
        ("a", "b", "ab-parallel"),
    ];
    for index in (1..rows.len()).rev() {
        seed = seed.wrapping_mul(6_364_136_223_846_793_005).wrapping_add(1);
        rows.swap(index, seed as usize % (index + 1));
    }
    multigraph_rows(rows)
}

fn multigraph_rows(rows: Vec<(&str, &str, &str)>) -> GraphArtifact {
    let mut graph = GraphArtifact::new();
    for key in ["a", "b", "c"] {
        graph.exact(
            exact_node(identity(key), NodeKind::VirtualArtifact, key, Vec::new()).expect("node"),
        );
    }
    for (from, to, resolver) in rows {
        graph.exact(
            exact_edge(
                identity(from),
                identity(to),
                EdgeKind::RustUses,
                Condition::Always,
                SourceAnchor::Synthesized {
                    authority: CanonicalText::new("f15").expect("authority"),
                },
                ResolverId::new(resolver).expect("resolver"),
                Vec::new(),
            )
            .expect("edge"),
        );
    }
    graph.canonicalize().expect("graph");
    graph
}

pub(crate) fn signal_graph(
    mutation: Option<Attribute>,
    fan: bool,
    articulation: bool,
) -> GraphArtifact {
    let mut graph = GraphArtifact::new();
    for index in 0..25 {
        let key = format!("n{index:02}");
        let attributes = (index == 12)
            .then(|| mutation.clone())
            .flatten()
            .into_iter()
            .collect();
        graph.exact(
            exact_node(identity(&key), NodeKind::VirtualArtifact, key, attributes).expect("node"),
        );
    }
    let mut rows = Vec::new();
    if articulation {
        for range in [1..=12, 13..=24] {
            let mut previous = 0;
            for next in range {
                rows.push((previous, next, format!("art-{previous}-{next}")));
                previous = next;
            }
            rows.push((previous, 0, format!("art-{previous}-0")));
        }
    } else {
        for index in 0..25 {
            rows.push((index, (index + 1) % 25, format!("ring-{index}")));
        }
        if fan {
            for target in 0..25 {
                if target != 12 && target != 13 {
                    rows.push((12, target, format!("fan-{target}")));
                }
            }
        }
    }
    for (from, to, resolver) in rows {
        graph.exact(
            exact_edge(
                identity(&format!("n{from:02}")),
                identity(&format!("n{to:02}")),
                EdgeKind::RustUses,
                Condition::Always,
                SourceAnchor::Synthesized {
                    authority: CanonicalText::new("f16").expect("authority"),
                },
                ResolverId::new(resolver).expect("resolver"),
                Vec::new(),
            )
            .expect("edge"),
        );
    }
    graph.canonicalize().expect("graph");
    graph
}

pub(crate) fn memberships(graph: &GraphArtifact) -> Vec<Vec<Identity>> {
    let mut memberships = rust_projection(graph)
        .clusters
        .clusters
        .into_iter()
        .map(|cluster| cluster.nodes)
        .collect::<Vec<_>>();
    memberships.sort();
    memberships
}

pub(crate) fn articulation_graph(bypass: bool) -> GraphArtifact {
    let mut graph = GraphArtifact::new();
    for index in 0..25 {
        let key = format!("a{index:02}");
        graph.exact(
            exact_node(identity(&key), NodeKind::VirtualArtifact, key, Vec::new()).expect("node"),
        );
    }
    let mut rows = Vec::new();
    for range in [1..=12, 13..=24] {
        let mut previous = 0;
        for next in range {
            rows.push((previous, next, format!("forward-{previous}-{next}")));
            previous = next;
        }
        rows.push((previous, 0, format!("close-{previous}")));
    }
    if bypass {
        rows.push((12, 13, "controlled-bypass".into()));
    }
    for (from, to, resolver) in rows {
        graph.exact(
            exact_edge(
                identity(&format!("a{from:02}")),
                identity(&format!("a{to:02}")),
                EdgeKind::RustUses,
                Condition::Always,
                SourceAnchor::Synthesized {
                    authority: CanonicalText::new("f16-articulation").expect("authority"),
                },
                ResolverId::new(resolver).expect("resolver"),
                Vec::new(),
            )
            .expect("edge"),
        );
    }
    graph.canonicalize().expect("graph");
    graph
}
