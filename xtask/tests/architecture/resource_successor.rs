use std::collections::BTreeSet;
use std::process::Command;
use std::time::{Duration, Instant};

use xtask::architecture::project::cycle::{basis_for_subgraph, component_bases};
use xtask::architecture::project::scc::{ProjectionBudget, decompose, strongly_connected};
use xtask::architecture::schema::{
    CanonicalText, Condition, EdgeFact, EdgeKind, Fact, GraphArtifact, Identity, NodeKind,
    ProjectionName, ResolverId, SourceAnchor, exact_edge, exact_node,
};

fn identity(index: usize) -> Identity {
    Identity::Virtual {
        namespace: CanonicalText::new("resource-successor").expect("namespace"),
        key: CanonicalText::new(format!("n{index:05}")).expect("key"),
    }
}

fn edge(from: usize, to: usize, discriminator: usize) -> EdgeFact {
    match exact_edge(
        identity(from),
        identity(to),
        EdgeKind::RustUses,
        Condition::Always,
        SourceAnchor::Synthesized {
            authority: CanonicalText::new(format!("holdout-{discriminator:08}"))
                .expect("authority"),
        },
        ResolverId::new(format!("holdout-{discriminator:08}")).expect("resolver"),
        Vec::new(),
    )
    .expect("edge")
    {
        Fact::Edge { edge } => *edge,
        Fact::Node { .. } => unreachable!("edge constructor"),
    }
}

#[test]
fn test_only_resource_telemetry_is_absent_from_the_production_surface() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let module = std::fs::read_to_string(root.join("src/architecture/mod.rs")).expect("module");
    let manifest = std::fs::read_to_string(root.join("Cargo.toml")).expect("Cargo manifest");
    assert!(!module.contains("pub mod resource"));
    assert!(
        !manifest
            .lines()
            .any(|line| line.trim_start().starts_with("libc"))
    );
    assert!(
        !root.join("src/architecture/resource.rs").exists(),
        "test-only telemetry must not ship in architecture"
    );
}

#[test]
#[ignore = "external stack/resource holdout; run explicitly with /usr/bin/time -l"]
fn iterative_scc_survives_a_frozen_long_chain_in_an_isolated_process() {
    let started = Instant::now();
    let output = Command::new(std::env::current_exe().expect("test executable"))
        .args([
            "--ignored",
            "--exact",
            "resource_successor::long_chain_scc_child",
            "--nocapture",
        ])
        .output()
        .expect("long-chain child");
    assert!(
        output.status.success() && String::from_utf8_lossy(&output.stdout).contains("1 passed"),
        "long-chain SCC child failed: status={} stdout={} stderr={}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(started.elapsed() < Duration::from_secs(10));
}

#[test]
#[ignore = "isolated child for iterative_scc_survives_a_frozen_long_chain_in_an_isolated_process"]
fn long_chain_scc_child() {
    const NODES: usize = 10_000;
    let vertices = (0..NODES).map(identity).collect::<BTreeSet<_>>();
    let edges = (0..NODES - 1)
        .map(|index| edge(index, index + 1, index))
        .collect::<Vec<_>>();
    let mut budget = ProjectionBudget::standard(ProjectionName::RustModule);
    let components = strongly_connected(ProjectionName::RustModule, &vertices, &edges, &mut budget)
        .expect("SCCs");
    assert_eq!(components.len(), NODES);
    assert!(components.iter().all(|component| !component.cyclic));
}

#[test]
fn dense_scc_returns_a_typed_budget_failure_before_enumeration() {
    let started = Instant::now();
    const NODES: usize = 96;
    let members = (0..NODES).map(identity).collect::<BTreeSet<_>>();
    let mut edges = Vec::new();
    for from in 0..NODES {
        for to in 0..NODES {
            if from != to {
                edges.push(edge(from, to, edges.len()));
            }
        }
    }
    let mut budget = ProjectionBudget::standard(ProjectionName::RustModule);
    let edge_refs = edges.iter().collect::<Vec<_>>();
    let error = basis_for_subgraph(
        ProjectionName::RustModule,
        &members,
        &edge_refs,
        &mut budget,
    )
    .expect_err("dense holdout must be rejected before exponential search");
    assert!(error.to_string().contains("RESOURCE_BUDGET_EXCEEDED"));
    assert!(started.elapsed() < Duration::from_secs(10));
}

#[test]
fn escaped_scc_endpoint_is_rejected_before_traversal_without_panicking() {
    let members = BTreeSet::from([identity(0)]);
    let edges = vec![edge(0, 1, 0)];
    let mut budget = ProjectionBudget::standard(ProjectionName::RustModule);
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        strongly_connected(ProjectionName::RustModule, &members, &edges, &mut budget)
    }));
    let error = result
        .expect("escaped endpoint must not panic")
        .expect_err("escaped endpoint must fail closed");
    assert!(error.to_string().contains("SCC_DOMAIN_MISMATCH"));
}

#[test]
fn adversarial_branching_has_a_deterministic_checked_budget() {
    const NODES: usize = 4_100;
    let members = (0..NODES).map(identity).collect::<BTreeSet<_>>();
    let mut edges = Vec::new();
    for from in 0..NODES {
        edges.push(edge(from, (from + 1) % NODES, edges.len()));
    }
    for from in 0..4_000 {
        edges.push(edge(from, (from + 2) % NODES, edges.len()));
    }
    let edge_refs = edges.iter().collect::<Vec<_>>();
    let started = Instant::now();
    let mut first_budget = ProjectionBudget::standard(ProjectionName::RustModule);
    let first = basis_for_subgraph(
        ProjectionName::RustModule,
        &members,
        &edge_refs,
        &mut first_budget,
    )
    .expect_err("branching node budget")
    .to_string();
    let mut second_budget = ProjectionBudget::standard(ProjectionName::RustModule);
    let second = basis_for_subgraph(
        ProjectionName::RustModule,
        &members,
        &edge_refs,
        &mut second_budget,
    )
    .expect_err("repeat branching node budget")
    .to_string();
    assert_eq!(first, second);
    assert!(first.contains("RESOURCE_BUDGET_EXCEEDED"));
    assert!(started.elapsed() < Duration::from_secs(10));
}

#[test]
fn in_cap_cycle_search_exhausts_the_exact_deterministic_projection_budget() {
    const NODES: usize = 64;
    let members = (0..NODES).map(identity).collect::<BTreeSet<_>>();
    let edges = (0..NODES)
        .map(|from| edge(from, (from + 1) % NODES, from))
        .collect::<Vec<_>>();
    let mut measured = ProjectionBudget::with_limit(ProjectionName::RustModule, usize::MAX);
    let decomposition =
        decompose(ProjectionName::RustModule, &members, &edges, &mut measured).expect("SCCs");
    let expected = component_bases(&decomposition, &mut measured).expect("basis");
    let exact = measured.used();
    let run = |limit| {
        let mut budget = ProjectionBudget::with_limit(ProjectionName::RustModule, limit);
        let decomposition = decompose(ProjectionName::RustModule, &members, &edges, &mut budget)?;
        component_bases(&decomposition, &mut budget).map_err(anyhow::Error::new)
    };
    let first = run(exact - 1)
        .expect_err("one unit below the exact projection work")
        .to_string();
    let second = run(exact - 1)
        .expect_err("repeat exact work exhaustion")
        .to_string();
    assert_eq!(first, second);
    assert!(first.contains("RESOURCE_BUDGET_EXCEEDED"));
    assert_eq!(run(exact).expect("exact work boundary succeeds"), expected);
}

#[test]
fn parallel_edge_holdout_preserves_exact_gf2_rank() {
    let started = Instant::now();
    let mut graph = GraphArtifact::new();
    for index in 0..2 {
        graph.exact(
            exact_node(
                identity(index),
                NodeKind::VirtualArtifact,
                format!("node {index}"),
                Vec::new(),
            )
            .expect("node"),
        );
    }
    let mut edges = Vec::new();
    for index in 0..32 {
        edges.push(edge(0, 1, index));
        edges.push(edge(1, 0, 32 + index));
    }
    let members = graph
        .nodes()
        .into_iter()
        .map(|node| node.identity.clone())
        .collect::<BTreeSet<_>>();
    let mut budget = ProjectionBudget::standard(ProjectionName::RustModule);
    let edge_refs = edges.iter().collect::<Vec<_>>();
    let (rank, cycles) = basis_for_subgraph(
        ProjectionName::RustModule,
        &members,
        &edge_refs,
        &mut budget,
    )
    .expect("basis");
    assert_eq!(rank, edges.len() - members.len() + 1);
    assert_eq!(cycles.len(), rank);
    assert!(started.elapsed() < Duration::from_secs(10));
}
