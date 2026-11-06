use std::collections::BTreeSet;

use xtask::architecture::project::cluster::cluster;
use xtask::architecture::project::cycle::{ComponentBasis, component_bases};
use xtask::architecture::project::quotient::build;
use xtask::architecture::project::scc::{ProjectionBudget, decompose};
use xtask::architecture::project::{ProjectionSet, project};
use xtask::architecture::schema::ProjectionName;
use xtask::architecture::test_support::{
    QuotientDomainOperation, with_quotient_domain_observer_for_test,
};

use std::sync::atomic::{AtomicUsize, Ordering};

use super::{graph, rust_projection, signal_graph};

static DOMAIN_SIZING_WORK: AtomicUsize = AtomicUsize::new(0);
static DOMAIN_SCAN_WORK: AtomicUsize = AtomicUsize::new(0);
static DOMAIN_DECLARED_BYTES: AtomicUsize = AtomicUsize::new(0);
static DOMAIN_SCANS: AtomicUsize = AtomicUsize::new(0);
static COMPLETE_BASIS_WORK: AtomicUsize = AtomicUsize::new(0);
static COMPLETE_BASIS_VISITS: AtomicUsize = AtomicUsize::new(0);
static QUOTIENT_DERIVED: AtomicUsize = AtomicUsize::new(0);

fn observe_domain(operation: QuotientDomainOperation) {
    match operation {
        QuotientDomainOperation::SerializerSizingPrecharged { work } => {
            DOMAIN_SIZING_WORK.store(work, Ordering::Relaxed);
        }
        QuotientDomainOperation::DomainScanPrecharged { work, bytes } => {
            DOMAIN_SCAN_WORK.store(work, Ordering::Relaxed);
            DOMAIN_DECLARED_BYTES.store(bytes, Ordering::Relaxed);
        }
        QuotientDomainOperation::Scan => {
            DOMAIN_SCANS.fetch_add(1, Ordering::Relaxed);
        }
        QuotientDomainOperation::CompleteBasisPrecharged { work } => {
            COMPLETE_BASIS_WORK.store(work, Ordering::Relaxed);
        }
        QuotientDomainOperation::CompleteBasisVisit => {
            COMPLETE_BASIS_VISITS.fetch_add(1, Ordering::Relaxed);
        }
        QuotientDomainOperation::Derived => {
            QUOTIENT_DERIVED.fetch_add(1, Ordering::Relaxed);
        }
    }
}

fn reset_domain_observer() {
    DOMAIN_SIZING_WORK.store(0, Ordering::Relaxed);
    DOMAIN_SCAN_WORK.store(0, Ordering::Relaxed);
    DOMAIN_DECLARED_BYTES.store(0, Ordering::Relaxed);
    DOMAIN_SCANS.store(0, Ordering::Relaxed);
    COMPLETE_BASIS_WORK.store(0, Ordering::Relaxed);
    COMPLETE_BASIS_VISITS.store(0, Ordering::Relaxed);
    QUOTIENT_DERIVED.store(0, Ordering::Relaxed);
}

#[test]
fn shard_hash_binds_complete_typed_cut_record() {
    let with_cut = rust_projection(&graph("resolver", true, false));
    let without_cut = rust_projection(&graph("resolver", false, false));
    assert_ne!(with_cut.quotient.hash, without_cut.quotient.hash);
}

#[test]
fn quotient_requires_the_exact_complete_component_basis_domain() {
    let graph = signal_graph(None, false, false);
    let nodes = graph.node_map();
    let vertices = nodes.keys().cloned().collect::<BTreeSet<_>>();
    let edges = graph.edges().into_iter().cloned().collect::<Vec<_>>();
    let mut budget = ProjectionBudget::with_limit(ProjectionName::RustModule, usize::MAX);
    let decomposition = decompose(ProjectionName::RustModule, &vertices, &edges, &mut budget)
        .expect("decomposition");
    let bases = component_bases(&decomposition, &mut budget).expect("bases");
    let clusters = cluster(&decomposition, &nodes, &mut budget).expect("clusters");
    assert!(!bases.is_empty());

    let error = build(&clusters, &decomposition, &bases[1..], &mut budget)
        .expect_err("missing complete basis");
    assert!(
        error
            .to_string()
            .contains("complete component basis domain differs")
    );

    let mut duplicate = bases.clone();
    duplicate.push(bases[0].clone());
    let error = build(&clusters, &decomposition, &duplicate, &mut budget)
        .expect_err("duplicate complete basis");
    assert!(
        error
            .to_string()
            .contains("duplicate complete component basis")
    );
}

#[test]
fn quotient_projection_and_byte_budget_are_sealed_before_derived_indexes() {
    let graph = signal_graph(None, false, false);
    let nodes = graph.node_map();
    let vertices = nodes.keys().cloned().collect::<BTreeSet<_>>();
    let edges = graph.edges().into_iter().cloned().collect::<Vec<_>>();
    let run = |projection, byte_limit| {
        let mut budget = ProjectionBudget::with_limits(projection, usize::MAX, byte_limit);
        let decomposition = decompose(projection, &vertices, &edges, &mut budget)?;
        let bases = component_bases(&decomposition, &mut budget)?;
        let clusters = cluster(&decomposition, &nodes, &mut budget)?;
        let quotient = build(&clusters, &decomposition, &bases, &mut budget)?;
        Ok::<_, anyhow::Error>((quotient, budget.bytes_reserved()))
    };

    let (_, exact) = run(ProjectionName::RustModule, usize::MAX).expect("measured quotient");
    let error = run(ProjectionName::RustModule, exact - 1)
        .expect_err("one byte below the complete quotient reservation")
        .to_string();
    assert!(error.contains("RESOURCE_BUDGET_EXCEEDED"), "{error}");
    run(ProjectionName::RustModule, exact).expect("exact quotient byte boundary");

    let mut rust_budget =
        ProjectionBudget::with_limits(ProjectionName::RustModule, usize::MAX, usize::MAX);
    let rust = decompose(
        ProjectionName::RustModule,
        &vertices,
        &edges,
        &mut rust_budget,
    )
    .expect("Rust decomposition");
    let rust_clusters = cluster(&rust, &nodes, &mut rust_budget).expect("Rust clusters");
    let mut call_budget =
        ProjectionBudget::with_limits(ProjectionName::Call, usize::MAX, usize::MAX);
    let call = decompose(ProjectionName::Call, &vertices, &edges, &mut call_budget)
        .expect("Call decomposition");
    let call_bases = component_bases(&call, &mut call_budget).expect("Call bases");
    let error = build(&rust_clusters, &call, &call_bases, &mut call_budget)
        .expect_err("cluster projection cannot be supplied independently")
        .to_string();
    assert!(error.contains("CLUSTER_DOMAIN_MISMATCH"), "{error}");
}

#[test]
fn quotient_domain_scan_has_its_own_exact_precharge_and_admission() {
    let graph = signal_graph(None, false, false);
    let nodes = graph.node_map();
    let vertices = nodes.keys().cloned().collect::<BTreeSet<_>>();
    let edges = graph.edges().into_iter().cloned().collect::<Vec<_>>();
    let mut fixture_budget =
        ProjectionBudget::with_limits(ProjectionName::RustModule, usize::MAX, usize::MAX);
    let decomposition = decompose(
        ProjectionName::RustModule,
        &vertices,
        &edges,
        &mut fixture_budget,
    )
    .expect("decomposition");
    let bases = component_bases(&decomposition, &mut fixture_budget).expect("bases");
    let clusters = cluster(&decomposition, &nodes, &mut fixture_budget).expect("clusters");

    reset_domain_observer();
    let mut budget =
        ProjectionBudget::with_limits(ProjectionName::RustModule, usize::MAX, usize::MAX);
    with_quotient_domain_observer_for_test(observe_domain, || {
        build(&clusters, &decomposition, &bases, &mut budget)
    })
    .expect("correct cluster/decomposition domain");
    let sizing_work = DOMAIN_SIZING_WORK.load(Ordering::Relaxed);
    let scan_work = DOMAIN_SCAN_WORK.load(Ordering::Relaxed);
    let domain_work = sizing_work + scan_work;
    let domain_bytes = DOMAIN_DECLARED_BYTES.load(Ordering::Relaxed);
    assert!(
        sizing_work > 0 && scan_work > 0 && domain_bytes > 0,
        "separate serializer-sizing and semantic-domain budgets"
    );
    assert!(
        DOMAIN_SCANS.load(Ordering::Relaxed) > 0,
        "domain scan observed"
    );
    assert!(
        QUOTIENT_DERIVED.load(Ordering::Relaxed) > 0,
        "quotient derived work observed"
    );
    assert!(budget.used() >= domain_work);
    assert!(budget.bytes_reserved() >= domain_bytes);

    reset_domain_observer();
    let mut exact_work =
        ProjectionBudget::with_limits(ProjectionName::RustModule, domain_work, usize::MAX);
    let error = with_quotient_domain_observer_for_test(observe_domain, || {
        build(&clusters, &decomposition, &bases, &mut exact_work)
    })
    .expect_err("domain work boundary stops at later quotient charge")
    .to_string();
    assert!(error.contains("complete-basis-domain"), "{error}");
    assert!(DOMAIN_SCANS.load(Ordering::Relaxed) > 0);
    assert_eq!(COMPLETE_BASIS_VISITS.load(Ordering::Relaxed), 0);
    assert_eq!(QUOTIENT_DERIVED.load(Ordering::Relaxed), 0);

    reset_domain_observer();
    let mut below_work =
        ProjectionBudget::with_limits(ProjectionName::RustModule, domain_work - 1, usize::MAX);
    let error = with_quotient_domain_observer_for_test(observe_domain, || {
        build(&clusters, &decomposition, &bases, &mut below_work)
    })
    .expect_err("one work unit below domain scan")
    .to_string();
    assert!(error.contains("cluster-domain-validation"), "{error}");
    assert_eq!(DOMAIN_SCANS.load(Ordering::Relaxed), 0);
    assert_eq!(COMPLETE_BASIS_VISITS.load(Ordering::Relaxed), 0);
    assert_eq!(QUOTIENT_DERIVED.load(Ordering::Relaxed), 0);

    reset_domain_observer();
    let mut exact_bytes =
        ProjectionBudget::with_limits(ProjectionName::RustModule, usize::MAX, domain_bytes);
    let error = with_quotient_domain_observer_for_test(observe_domain, || {
        build(&clusters, &decomposition, &bases, &mut exact_bytes)
    })
    .expect_err("domain byte boundary stops at later quotient reservation")
    .to_string();
    assert!(error.contains("quotient"), "{error}");
    assert!(DOMAIN_SCANS.load(Ordering::Relaxed) > 0);
    assert!(COMPLETE_BASIS_VISITS.load(Ordering::Relaxed) > 0);
    assert_eq!(QUOTIENT_DERIVED.load(Ordering::Relaxed), 0);

    reset_domain_observer();
    let mut below_bytes =
        ProjectionBudget::with_limits(ProjectionName::RustModule, usize::MAX, domain_bytes - 1);
    let error = with_quotient_domain_observer_for_test(observe_domain, || {
        build(&clusters, &decomposition, &bases, &mut below_bytes)
    })
    .expect_err("one byte below domain scan")
    .to_string();
    assert!(error.contains("cluster domain"), "{error}");
    assert_eq!(DOMAIN_SCANS.load(Ordering::Relaxed), 0);
    assert_eq!(COMPLETE_BASIS_VISITS.load(Ordering::Relaxed), 0);
    assert_eq!(QUOTIENT_DERIVED.load(Ordering::Relaxed), 0);
}

#[test]
fn complete_basis_key_domain_is_precharged_before_any_visit_or_derived_work() {
    let graph = signal_graph(None, false, false);
    let nodes = graph.node_map();
    let vertices = nodes.keys().cloned().collect::<BTreeSet<_>>();
    let edges = graph.edges().into_iter().cloned().collect::<Vec<_>>();
    let mut fixture_budget =
        ProjectionBudget::with_limits(ProjectionName::RustModule, usize::MAX, usize::MAX);
    let decomposition = decompose(
        ProjectionName::RustModule,
        &vertices,
        &edges,
        &mut fixture_budget,
    )
    .expect("decomposition");
    let bases = component_bases(&decomposition, &mut fixture_budget).expect("bases");
    let clusters = cluster(&decomposition, &nodes, &mut fixture_budget).expect("clusters");

    reset_domain_observer();
    let mut measured =
        ProjectionBudget::with_limits(ProjectionName::RustModule, usize::MAX, usize::MAX);
    with_quotient_domain_observer_for_test(observe_domain, || {
        build(&clusters, &decomposition, &bases, &mut measured)
    })
    .expect("measure complete-basis domain");
    let prefix =
        DOMAIN_SIZING_WORK.load(Ordering::Relaxed) + DOMAIN_SCAN_WORK.load(Ordering::Relaxed);
    let basis_work = COMPLETE_BASIS_WORK.load(Ordering::Relaxed);
    assert!(basis_work > 0, "complete-basis key-domain budget");
    assert!(
        COMPLETE_BASIS_VISITS.load(Ordering::Relaxed) > 0,
        "complete-basis key-domain visits"
    );

    reset_domain_observer();
    let mut below = ProjectionBudget::with_limits(
        ProjectionName::RustModule,
        prefix + basis_work - 1,
        usize::MAX,
    );
    let error = with_quotient_domain_observer_for_test(observe_domain, || {
        build(&clusters, &decomposition, &bases, &mut below)
    })
    .expect_err("one work unit below complete-basis key-domain admission")
    .to_string();
    assert!(error.contains("complete-basis-domain"), "{error}");
    assert_eq!(COMPLETE_BASIS_VISITS.load(Ordering::Relaxed), 0);
    assert_eq!(QUOTIENT_DERIVED.load(Ordering::Relaxed), 0);

    reset_domain_observer();
    let mut exact =
        ProjectionBudget::with_limits(ProjectionName::RustModule, prefix + basis_work, usize::MAX);
    let error = with_quotient_domain_observer_for_test(observe_domain, || {
        build(&clusters, &decomposition, &bases, &mut exact)
    })
    .expect_err("exact complete-basis boundary stops before quotient derivation")
    .to_string();
    assert!(error.contains("quotient declared cost"), "{error}");
    assert!(COMPLETE_BASIS_VISITS.load(Ordering::Relaxed) > 0);
    assert_eq!(QUOTIENT_DERIVED.load(Ordering::Relaxed), 0);
}

#[test]
fn quotient_rejects_every_invalid_complete_cyclic_basis_before_using_its_rank() {
    let graph = signal_graph(None, false, false);
    let nodes = graph.node_map();
    let vertices = nodes.keys().cloned().collect::<BTreeSet<_>>();
    let edges = graph.edges().into_iter().cloned().collect::<Vec<_>>();
    let assert_rejected = |name: &str, mutate: fn(&mut ComponentBasis)| {
        let mut budget =
            ProjectionBudget::with_limits(ProjectionName::RustModule, usize::MAX, usize::MAX);
        let decomposition = decompose(ProjectionName::RustModule, &vertices, &edges, &mut budget)
            .expect("decomposition");
        let mut bases = component_bases(&decomposition, &mut budget).expect("complete bases");
        let clusters = cluster(&decomposition, &nodes, &mut budget).expect("clusters");
        let basis = bases.first_mut().expect("cyclic component basis");
        mutate(basis);
        let error = build(&clusters, &decomposition, &bases, &mut budget)
            .expect_err(name)
            .to_string();
        assert!(error.contains("CYCLE_BASIS_INCOMPLETE"), "{name}: {error}");
    };
    assert_rejected("rank-zero empty basis", |basis| {
        basis.rank = 0;
        basis.cycles.clear();
    });
    assert_rejected("wrong rank", |basis| basis.rank += 1);
    assert_rejected("dependent cycle", |basis| {
        basis.cycles.push(basis.cycles[0].clone());
    });
    assert_rejected("invalid cycle", |basis| {
        basis.cycles[0].edges.clear();
    });
}

#[test]
fn quotient_rejects_cluster_build_from_a_different_typed_edge_domain() {
    let first = graph("origin-a", true, false);
    let second = graph("origin-b", true, false);
    let nodes = first.node_map();
    let vertices = nodes.keys().cloned().collect::<BTreeSet<_>>();
    let first_edges = first.edges().into_iter().cloned().collect::<Vec<_>>();
    let second_edges = second.edges().into_iter().cloned().collect::<Vec<_>>();
    let mut first_budget =
        ProjectionBudget::with_limits(ProjectionName::RustModule, usize::MAX, usize::MAX);
    let first_decomposition = decompose(
        ProjectionName::RustModule,
        &vertices,
        &first_edges,
        &mut first_budget,
    )
    .expect("first decomposition");
    let first_bases =
        component_bases(&first_decomposition, &mut first_budget).expect("first bases");
    let clusters = cluster(&first_decomposition, &nodes, &mut first_budget).expect("clusters");
    build(
        &clusters,
        &first_decomposition,
        &first_bases,
        &mut first_budget,
    )
    .expect("originating typed-edge domain");

    let mut second_budget =
        ProjectionBudget::with_limits(ProjectionName::RustModule, usize::MAX, usize::MAX);
    let second_decomposition = decompose(
        ProjectionName::RustModule,
        &vertices,
        &second_edges,
        &mut second_budget,
    )
    .expect("second decomposition");
    let bases = component_bases(&second_decomposition, &mut second_budget).expect("bases");
    reset_domain_observer();
    let error = with_quotient_domain_observer_for_test(observe_domain, || {
        build(&clusters, &second_decomposition, &bases, &mut second_budget)
    })
    .expect_err("foreign typed-edge origin")
    .to_string();
    assert!(error.contains("CLUSTER_DOMAIN_MISMATCH"), "{error}");
    assert!(DOMAIN_SCANS.load(Ordering::Relaxed) > 0);
    assert_eq!(COMPLETE_BASIS_VISITS.load(Ordering::Relaxed), 0);
    assert_eq!(QUOTIENT_DERIVED.load(Ordering::Relaxed), 0);
}

#[test]
fn f15_quotient_complement_cycles_replay_against_original_edges() {
    let graph = signal_graph(None, false, false);
    let edge_map = graph.edge_map();
    let projection = rust_projection(&graph);
    for basis in &projection.quotient.bases {
        for cycle in &basis.cycles {
            let first = &edge_map[&cycle.edges[0]].record.from;
            let mut previous = first.clone();
            for edge in &cycle.edges {
                let record = &edge_map[edge].record;
                assert_eq!(record.from, previous);
                previous = record.to.clone();
            }
            assert_eq!(&previous, first);
        }
    }
    assert!(
        projection
            .quotient
            .bases
            .iter()
            .map(|basis| basis.rank)
            .sum::<usize>()
            > 0
    );
}

#[test]
fn f15_quotient_validator_rejects_missing_duplicate_and_internal_crossings() {
    let graph = graph("quotient-validator", true, false);
    let projections = project(&graph).expect("projections");
    let mutate =
        |mut projections: ProjectionSet,
         mutation: fn(&mut xtask::architecture::project::quotient::QuotientEdge)| {
            let quotient = projections
                .projections
                .iter_mut()
                .find(|projection| projection.spec.name == ProjectionName::RustModule)
                .expect("Rust projection")
                .quotient
                .edges
                .first_mut()
                .expect("crossing edge");
            mutation(quotient);
            let error =
                xtask::architecture::evidence::validate::validate_structural(&graph, &projections)
                    .expect_err("mutated quotient");
            assert!(error.to_string().contains("BOUNDARY_QUOTIENT_ABSENT"));
        };
    mutate(projections.clone(), |edge| edge.edges.clear());
    mutate(projections.clone(), |edge| {
        edge.edges.push(edge.edges[0].clone());
    });
    mutate(projections, |edge| edge.to = edge.from.clone());
}
