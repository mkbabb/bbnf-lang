use std::collections::BTreeSet;

use xtask::architecture::project::cycle::{
    CycleWitness, basis_for_subgraph, complement_basis, component_bases, directed_cycle_rank,
    validate_basis, validate_cycle_completion,
};
use xtask::architecture::project::scc::{ProjectionBudget, decompose};
use xtask::architecture::schema::{
    CanonicalText, Condition, EdgeFact, EdgeIdentity, EdgeKind, Fact, Identity, ProjectionName,
    ResolverId, SourceAnchor, exact_edge,
};
use xtask::architecture::test_support::{
    CycleStorageOperation, JsonOperation, ValidationOperation, ValidationPhase,
    cycle_basis_fresh_prefix_for_test, validate_cycle_domain_for_test,
    with_cycle_storage_observer_for_test, with_json_observer_for_test,
    with_validation_observer_for_test,
};

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Mutex, OnceLock};

fn cycle_events() -> &'static Mutex<Vec<ValidationOperation>> {
    static EVENTS: OnceLock<Mutex<Vec<ValidationOperation>>> = OnceLock::new();
    EVENTS.get_or_init(|| Mutex::new(Vec::new()))
}

static CYCLE_STORAGE_COUNTS: [AtomicU64; 27] = [const { AtomicU64::new(0) }; 27];
static WITNESS_HEAP: [AtomicU64; 3] = [const { AtomicU64::new(0) }; 3];
static COMPLEMENT_HEAP: [AtomicU64; 3] = [const { AtomicU64::new(0) }; 3];
static MEMBER_HEAP: [AtomicU64; 3] = [const { AtomicU64::new(0) }; 3];
static ROW_HEAP: [AtomicU64; 3] = [const { AtomicU64::new(0) }; 3];
static TYPED_ERROR_BASELINES: [AtomicU64; 5] = [const { AtomicU64::new(0) }; 5];
static TYPED_ERROR_DELTAS: [[AtomicU64; 2]; 5] = [const { [const { AtomicU64::new(0) }; 2] }; 5];

fn capture_heap(storage: &[AtomicU64; 3]) {
    let (active, _) = crate::take_heap_usage();
    let (_, peak) = crate::take_heap_usage();
    let baseline = storage[0].load(Ordering::Relaxed);
    storage[1].store(active.saturating_sub(baseline), Ordering::Relaxed);
    storage[2].store(peak.saturating_sub(baseline), Ordering::Relaxed);
}

fn observe_cycle_storage(operation: CycleStorageOperation) {
    fn capture_typed_error(index: usize) {
        let baseline = TYPED_ERROR_BASELINES[index].load(Ordering::Relaxed);
        let (active, peak) = crate::take_heap_usage();
        TYPED_ERROR_DELTAS[index][0].store(active.saturating_sub(baseline), Ordering::Relaxed);
        TYPED_ERROR_DELTAS[index][1].store(peak.saturating_sub(baseline), Ordering::Relaxed);
    }

    let index = match operation {
        CycleStorageOperation::WitnessPrecharged => {
            WITNESS_HEAP[0].store(crate::take_heap_usage().0, Ordering::Relaxed);
            0
        }
        CycleStorageOperation::WitnessVisit => 1,
        CycleStorageOperation::WitnessAllocation => 2,
        CycleStorageOperation::WitnessRetainedComplete => {
            capture_heap(&WITNESS_HEAP);
            3
        }
        CycleStorageOperation::ComplementPrecharged => {
            COMPLEMENT_HEAP[0].store(crate::take_heap_usage().0, Ordering::Relaxed);
            4
        }
        CycleStorageOperation::ComplementClone => 5,
        CycleStorageOperation::ComplementAllocation => 6,
        CycleStorageOperation::ComplementRetainedComplete => {
            capture_heap(&COMPLEMENT_HEAP);
            7
        }
        CycleStorageOperation::MemberPrecharged => {
            MEMBER_HEAP[0].store(crate::take_heap_usage().0, Ordering::Relaxed);
            8
        }
        CycleStorageOperation::MemberVisit => 9,
        CycleStorageOperation::MemberAllocation => 10,
        CycleStorageOperation::MemberRetainedComplete => {
            capture_heap(&MEMBER_HEAP);
            11
        }
        CycleStorageOperation::RowPrecharged => {
            ROW_HEAP[0].store(crate::take_heap_usage().0, Ordering::Relaxed);
            12
        }
        CycleStorageOperation::RowVisit => 13,
        CycleStorageOperation::RowAllocation => 14,
        CycleStorageOperation::RowRetainedComplete => {
            let active = crate::take_heap_usage().0;
            let baseline = ROW_HEAP[0].load(Ordering::Relaxed);
            ROW_HEAP[1].store(active.saturating_sub(baseline), Ordering::Relaxed);
            15
        }
        CycleStorageOperation::WitnessPrechargeAttempt => {
            TYPED_ERROR_BASELINES[0].store(crate::take_heap_usage().0, Ordering::Relaxed);
            16
        }
        CycleStorageOperation::ComplementPrechargeAttempt => {
            TYPED_ERROR_BASELINES[1].store(crate::take_heap_usage().0, Ordering::Relaxed);
            17
        }
        CycleStorageOperation::MemberPrechargeAttempt => {
            TYPED_ERROR_BASELINES[2].store(crate::take_heap_usage().0, Ordering::Relaxed);
            18
        }
        CycleStorageOperation::RowPrechargeAttempt => {
            TYPED_ERROR_BASELINES[3].store(crate::take_heap_usage().0, Ordering::Relaxed);
            19
        }
        CycleStorageOperation::WitnessFailure => {
            capture_typed_error(4);
            20
        }
        CycleStorageOperation::WitnessPrechargeFailure => {
            capture_typed_error(0);
            21
        }
        CycleStorageOperation::ComplementPrechargeFailure => {
            capture_typed_error(1);
            22
        }
        CycleStorageOperation::MemberPrechargeFailure => {
            capture_typed_error(2);
            23
        }
        CycleStorageOperation::RowPrechargeFailure => {
            capture_typed_error(3);
            24
        }
        CycleStorageOperation::WitnessFailureAttempt => {
            TYPED_ERROR_BASELINES[4].store(crate::take_heap_usage().0, Ordering::Relaxed);
            25
        }
        CycleStorageOperation::RowTransientPeak => {
            let active = crate::take_heap_usage().0;
            let baseline = ROW_HEAP[0].load(Ordering::Relaxed);
            ROW_HEAP[2].store(active.saturating_sub(baseline), Ordering::Relaxed);
            26
        }
    };
    CYCLE_STORAGE_COUNTS[index].fetch_add(1, Ordering::Relaxed);
}

fn reset_storage_events() {
    for count in &CYCLE_STORAGE_COUNTS {
        count.store(0, Ordering::Relaxed);
    }
    for delta in &TYPED_ERROR_DELTAS {
        for value in delta {
            value.store(u64::MAX, Ordering::Relaxed);
        }
    }
}

fn storage_counts() -> [u64; 27] {
    std::array::from_fn(|index| CYCLE_STORAGE_COUNTS[index].load(Ordering::Relaxed))
}

fn assert_typed_error_heap(index: usize) {
    assert_eq!(
        [
            TYPED_ERROR_DELTAS[index][0].load(Ordering::Relaxed),
            TYPED_ERROR_DELTAS[index][1].load(Ordering::Relaxed),
        ],
        [0, 0]
    );
}

fn observe_cycle_phase(operation: ValidationOperation) {
    if matches!(
        operation,
        ValidationOperation::PhasePrecharged(_)
            | ValidationOperation::PhaseVisit(_)
            | ValidationOperation::PhaseAllocation(_)
    ) {
        cycle_events().lock().expect("cycle events").push(operation);
    }
}

fn observe_member_hash_phase(operation: ValidationOperation) {
    observe_cycle_phase(operation);
    if operation == ValidationOperation::PhaseAllocation(ValidationPhase::CycleDomain) {
        CYCLE_JSON_SOURCE_BYTES.store(0, Ordering::Relaxed);
    }
}

fn take_cycle_events() -> Vec<ValidationOperation> {
    std::mem::take(&mut *cycle_events().lock().expect("cycle events"))
}

fn count_cycle_event(expected: ValidationOperation) -> usize {
    take_cycle_events()
        .into_iter()
        .filter(|event| *event == expected)
        .count()
}

fn events_matching(events: &[ValidationOperation], expected: ValidationOperation) -> usize {
    events.iter().filter(|event| **event == expected).count()
}

static BASIS_HEAP: [AtomicU64; 3] = [const { AtomicU64::new(0) }; 3];
static CYCLE_JSON_SOURCE_BYTES: AtomicU64 = AtomicU64::new(0);

fn observe_cycle_json(operation: JsonOperation) {
    if let JsonOperation::SourceBytesScanned(bytes) = operation {
        CYCLE_JSON_SOURCE_BYTES.fetch_add(bytes as u64, Ordering::Relaxed);
    }
}

fn observe_basis_heap(operation: ValidationOperation) {
    match operation {
        ValidationOperation::PhasePrecharged(ValidationPhase::CycleBasis) => {
            let (active, _) = crate::take_heap_usage();
            BASIS_HEAP[0].store(active, Ordering::Relaxed);
        }
        ValidationOperation::PhaseAllocation(ValidationPhase::CycleBasis) => {
            let (active, peak) = crate::take_heap_usage();
            let baseline = BASIS_HEAP[0].load(Ordering::Relaxed);
            BASIS_HEAP[1].store(active.saturating_sub(baseline), Ordering::Relaxed);
            BASIS_HEAP[2].store(peak.saturating_sub(baseline), Ordering::Relaxed);
        }
        _ => {}
    }
}

fn identity(index: usize) -> Identity {
    padded_identity(index, 0)
}

fn padded_identity(index: usize, padding: usize) -> Identity {
    Identity::Virtual {
        namespace: CanonicalText::new("kernel-test").expect("namespace"),
        key: CanonicalText::new(format!("{}n{index}", "p".repeat(padding))).expect("key"),
    }
}

fn edge(from: usize, to: usize, resolver: &str) -> EdgeFact {
    edge_between(identity(from), identity(to), resolver)
}

fn edge_between(from: Identity, to: Identity, resolver: &str) -> EdgeFact {
    match exact_edge(
        from,
        to,
        EdgeKind::RustUses,
        Condition::Always,
        SourceAnchor::Synthesized {
            authority: CanonicalText::new("kernel-test").expect("authority"),
        },
        ResolverId::new(resolver).expect("resolver"),
        Vec::new(),
    )
    .expect("edge")
    {
        Fact::Edge { edge } => *edge,
        Fact::Node { .. } => unreachable!("edge constructor"),
    }
}

fn triangle() -> (BTreeSet<Identity>, Vec<EdgeFact>) {
    let vertices = (0..3).map(identity).collect();
    let edges = vec![
        edge(0, 1, "01"),
        edge(1, 2, "12"),
        edge(2, 0, "20"),
        edge(0, 2, "02"),
        edge(2, 1, "21"),
        edge(1, 0, "10"),
    ];
    (vertices, edges)
}

fn padded_triangle(padding: usize) -> (BTreeSet<Identity>, Vec<EdgeFact>) {
    let identities = (0..3)
        .map(|index| padded_identity(index, padding))
        .collect::<Vec<_>>();
    let vertices = identities.iter().cloned().collect();
    let edges = [
        (0, 1, "01"),
        (1, 2, "12"),
        (2, 0, "20"),
        (0, 2, "02"),
        (2, 1, "21"),
        (1, 0, "10"),
    ]
    .into_iter()
    .map(|(from, to, resolver)| {
        edge_between(identities[from].clone(), identities[to].clone(), resolver)
    })
    .collect();
    (vertices, edges)
}

fn independent_cycle_domain_work(members: &BTreeSet<Identity>, edges: &[&EdgeFact]) -> usize {
    members.len()
        + edges.len() * 3
        + edges
            .iter()
            .map(|edge| super::scc::edge_record_work(edge, 2))
            .sum::<usize>()
        + super::scc::container_work(2)
        + super::scc::container_work(members.len())
        + members
            .iter()
            .map(|member| super::scc::virtual_identity_work(member, 1))
            .sum::<usize>()
        + super::scc::container_work(edges.len())
        + edges
            .iter()
            .map(|edge| super::scc::edge_fact_work(edge, 1))
            .sum::<usize>()
}

fn independent_cycle_member_hash_work(members: &BTreeSet<Identity>) -> usize {
    let members = members.iter().cloned().collect::<Vec<_>>();
    members.len() + super::scc::component_member_work(ProjectionName::RustModule, &members, 1)
}

struct ExpectedSearchFrame {
    _current: usize,
    _next_candidate: usize,
    _inserted_node: Option<usize>,
}

fn independent_basis_reservation(nodes: usize, edges: usize, target: usize) -> usize {
    let words = edges.div_ceil(64);
    let nodes_plus_one = nodes + 1;
    let witness_edges = target * nodes.min(edges);
    nodes * std::mem::size_of::<&Identity>()
        + nodes_plus_one * std::mem::size_of::<usize>()
        + edges * std::mem::size_of::<usize>()
        + nodes * std::mem::size_of::<usize>()
        + nodes * std::mem::size_of::<u8>()
        + nodes_plus_one * std::mem::size_of::<usize>()
        + nodes * std::mem::size_of::<ExpectedSearchFrame>()
        + target * std::mem::size_of::<CycleWitness>()
        + edges * std::mem::size_of::<Option<Box<[u64]>>>()
        + (target + 1) * words * std::mem::size_of::<u64>()
        + witness_edges * std::mem::size_of::<EdgeIdentity>()
        + witness_edges * (b"edge:sha256:".len() + 64)
}

fn independent_basis_observed(
    nodes: usize,
    edges: usize,
    target: usize,
    witness_edges: usize,
) -> (usize, usize) {
    let words = edges.div_ceil(64);
    let retained = nodes * std::mem::size_of::<&Identity>()
        + (nodes + 1) * std::mem::size_of::<usize>()
        + edges * std::mem::size_of::<usize>()
        + edges * std::mem::size_of::<Option<Box<[u64]>>>()
        + target * std::mem::size_of::<CycleWitness>()
        + nodes * std::mem::size_of::<u8>()
        + (nodes + 1) * std::mem::size_of::<usize>()
        + nodes * std::mem::size_of::<ExpectedSearchFrame>()
        + target * words * std::mem::size_of::<u64>()
        + witness_edges * std::mem::size_of::<EdgeIdentity>()
        + witness_edges * (b"edge:sha256:".len() + 64);
    let transient = words * std::mem::size_of::<u64>();
    (retained, retained + transient)
}

fn independent_cycle_domain_bytes(members: &BTreeSet<Identity>, edges: &[&EdgeFact]) -> usize {
    let fixed = members.len() * std::mem::size_of::<Identity>() * 2
        + edges.len()
            * (std::mem::size_of::<&EdgeFact>() + std::mem::size_of::<(EdgeIdentity, usize)>());
    let json = serde_json::to_vec(&(members, edges))
        .expect("cycle-domain bytes")
        .len();
    fixed + 2 * json
}

fn independent_linear_bytes(columns: usize, rows: usize, witness_edges: usize) -> usize {
    let words = columns.div_ceil(64);
    (rows + 1) * words * std::mem::size_of::<u64>()
        + columns * std::mem::size_of::<Option<Box<[u64]>>>()
        + witness_edges * std::mem::size_of::<EdgeIdentity>()
        + columns.div_ceil(8)
}

fn independent_scc_rank_bytes(members: &BTreeSet<Identity>, edges: &[&EdgeFact]) -> usize {
    let fixed =
        members.len() * std::mem::size_of::<Identity>() * 8 + std::mem::size_of_val(edges) * 6;
    let json = serde_json::to_vec(&(members, edges))
        .expect("rank-domain bytes")
        .len();
    fixed + 4 * json + 64
}

fn independent_member_hash_bytes(members: &BTreeSet<Identity>) -> usize {
    members.len() * std::mem::size_of::<&Identity>()
        + serde_json::to_vec(&(ProjectionName::RustModule, members))
            .expect("member-hash bytes")
            .len()
}

fn independent_validate_basis_bytes(
    members: &BTreeSet<Identity>,
    edges: &[&EdgeFact],
    basis: &xtask::architecture::project::cycle::ComponentBasis,
) -> usize {
    let witness_edges = basis
        .cycles
        .iter()
        .map(|witness| witness.edges.len())
        .sum::<usize>();
    independent_cycle_domain_bytes(members, edges)
        + independent_member_hash_bytes(members)
        + 64
        + independent_linear_bytes(edges.len(), basis.cycles.len(), witness_edges)
        + independent_scc_rank_bytes(members, edges)
}

#[test]
fn public_cycle_entrypoints_totalize_the_empty_graph_on_one_budget() {
    let members = BTreeSet::new();
    let edge_refs = Vec::new();
    let mut budget = ProjectionBudget::standard(ProjectionName::RustModule);
    assert_eq!(
        directed_cycle_rank(
            ProjectionName::RustModule,
            &members,
            &edge_refs,
            &mut budget,
        )
        .expect("empty rank"),
        0
    );
    assert_eq!(
        basis_for_subgraph(
            ProjectionName::RustModule,
            &members,
            &edge_refs,
            &mut budget,
        )
        .expect("empty basis"),
        (0, Vec::new())
    );
}

#[test]
fn directed_rank_matches_basis_and_does_not_expose_weak_undirected_rank() {
    let members = (0..3).map(identity).collect::<BTreeSet<_>>();
    let edges = [edge(0, 1, "01"), edge(0, 2, "02"), edge(1, 2, "12")];
    let edge_refs = edges.iter().collect::<Vec<_>>();
    let mut rank_budget = ProjectionBudget::standard(ProjectionName::RustModule);
    let rank = directed_cycle_rank(
        ProjectionName::RustModule,
        &members,
        &edge_refs,
        &mut rank_budget,
    )
    .expect("directed rank");
    let mut basis_budget = ProjectionBudget::standard(ProjectionName::RustModule);
    let (basis_rank, basis) = basis_for_subgraph(
        ProjectionName::RustModule,
        &members,
        &edge_refs,
        &mut basis_budget,
    )
    .expect("directed basis");
    assert_eq!((rank, basis_rank, basis.len()), (0, 0, 0));
}

#[test]
fn one_directed_rank_law_covers_disconnected_cyclic_and_acyclic_domains() {
    let members = (0..7).map(identity).collect::<BTreeSet<_>>();
    let edges = [
        edge(0, 1, "01"),
        edge(1, 0, "10"),
        edge(2, 3, "23"),
        edge(3, 4, "34"),
        edge(4, 2, "42"),
        edge(2, 4, "24"),
        edge(1, 2, "12-crossing"),
        edge(5, 6, "56-acyclic"),
    ];
    let edge_refs = edges.iter().collect::<Vec<_>>();
    let mut rank_budget = ProjectionBudget::standard(ProjectionName::RustModule);
    let rank = directed_cycle_rank(
        ProjectionName::RustModule,
        &members,
        &edge_refs,
        &mut rank_budget,
    )
    .expect("arbitrary-domain rank");
    let mut basis_budget = ProjectionBudget::standard(ProjectionName::RustModule);
    let (basis_rank, cycles) = basis_for_subgraph(
        ProjectionName::RustModule,
        &members,
        &edge_refs,
        &mut basis_budget,
    )
    .expect("arbitrary-domain basis");
    assert_eq!((rank, basis_rank, cycles.len()), (3, 3, 3));
}

#[test]
fn one_projection_budget_covers_scc_and_cycle_costs() {
    let (vertices, edges) = triangle();
    let mut budget = ProjectionBudget::with_limit(ProjectionName::RustModule, usize::MAX);
    let decomposition =
        decompose(ProjectionName::RustModule, &vertices, &edges, &mut budget).expect("SCCs");
    let scc_cost = budget.used();
    let bases = component_bases(&decomposition, &mut budget).expect("basis");
    assert!(!bases.is_empty());
    assert!(budget.used() > scc_cost, "cycle work was not charged");

    let mut bounded = ProjectionBudget::with_limit(ProjectionName::RustModule, scc_cost);
    let decomposition = decompose(ProjectionName::RustModule, &vertices, &edges, &mut bounded)
        .expect("SCC cost fits exactly");
    let error = component_bases(&decomposition, &mut bounded)
        .expect_err("cycle cost must share the exhausted projection budget");
    assert!(error.to_string().contains("RESOURCE_BUDGET_EXCEEDED"));
}

#[test]
fn quotient_complement_reuses_the_complete_component_basis() {
    let (vertices, edges) = triangle();
    let mut budget = ProjectionBudget::standard(ProjectionName::RustModule);
    let decomposition =
        decompose(ProjectionName::RustModule, &vertices, &edges, &mut budget).expect("SCCs");
    let complete = component_bases(&decomposition, &mut budget)
        .expect("basis")
        .pop()
        .expect("cyclic component");
    let internal = decomposition
        .internal_edges(&complete.component)
        .expect("bucket");
    let seed = complete.cycles[..1].to_vec();
    let (rank, complement) = complement_basis(&vertices, internal, &seed, &complete, &mut budget)
        .expect("complete from existing basis");
    assert_eq!(rank, complete.rank - 1);
    assert_eq!(complement.len(), rank);
}

#[test]
fn quotient_complement_uses_the_real_gf2_seed_rank() {
    let (vertices, edges) = triangle();
    let mut budget = ProjectionBudget::standard(ProjectionName::RustModule);
    let decomposition =
        decompose(ProjectionName::RustModule, &vertices, &edges, &mut budget).expect("SCCs");
    let complete = component_bases(&decomposition, &mut budget)
        .expect("basis")
        .pop()
        .expect("cyclic component");
    let internal = decomposition
        .internal_edges(&complete.component)
        .expect("bucket");
    let seed = vec![complete.cycles[0].clone(), complete.cycles[0].clone()];
    let (rank, complement) = complement_basis(&vertices, internal, &seed, &complete, &mut budget)
        .expect("dependent seed is ranked algebraically");
    assert_eq!(rank, complete.rank - 1);
    assert_eq!(complement.len(), rank);
}

#[test]
fn forged_component_digest_and_cyclicity_fail_closed_in_validation() {
    let graph = super::graph("kernel-validation", true, true);
    let projections = xtask::architecture::project::project(&graph).expect("projections");
    for mutation in ["digest", "cyclic", "members"] {
        let mut changed = projections.clone();
        let component = changed
            .projections
            .iter_mut()
            .find(|projection| projection.spec.name == ProjectionName::RustModule)
            .and_then(|projection| projection.components.first_mut())
            .expect("component");
        match mutation {
            "digest" => {
                component.identity.digest =
                    xtask::architecture::schema::Sha256Digest::new("0".repeat(64)).expect("digest")
            }
            "cyclic" => component.cyclic = false,
            "members" => {
                component.members.pop();
            }
            _ => unreachable!(),
        }
        let error = xtask::architecture::evidence::validate::validate_structural(&graph, &changed)
            .expect_err(mutation);
        assert!(error.to_string().contains("SCC_TOTALITY_MISMATCH"));
    }
}

#[test]
fn every_public_cycle_boundary_rejects_one_forged_edge_domain() {
    let (members, edges) = triangle();
    let mut setup = ProjectionBudget::standard(ProjectionName::RustModule);
    let decomposition =
        decompose(ProjectionName::RustModule, &members, &edges, &mut setup).expect("SCCs");
    let complete = component_bases(&decomposition, &mut setup)
        .expect("basis")
        .pop()
        .expect("cyclic component");
    let mut forged = edges.clone();
    forged[0].record.resolver = ResolverId::new("forged-after-identity").expect("resolver");
    let forged_refs = forged.iter().collect::<Vec<_>>();

    let assert_domain = |result: anyhow::Result<()>, owner: &str| {
        let error = result.expect_err(owner).to_string();
        assert!(
            error.contains("typed edge identity is invalid"),
            "{owner}: {error}"
        );
    };
    let mut budget = ProjectionBudget::standard(ProjectionName::RustModule);
    assert_domain(
        directed_cycle_rank(
            ProjectionName::RustModule,
            &members,
            &forged_refs,
            &mut budget,
        )
        .map(|_| ())
        .map_err(anyhow::Error::new),
        "directed rank",
    );
    let mut budget = ProjectionBudget::standard(ProjectionName::RustModule);
    assert_domain(
        basis_for_subgraph(
            ProjectionName::RustModule,
            &members,
            &forged_refs,
            &mut budget,
        )
        .map(|_| ())
        .map_err(anyhow::Error::new),
        "basis",
    );
    let mut budget = ProjectionBudget::standard(ProjectionName::RustModule);
    assert_domain(
        validate_basis(&members, &forged_refs, &complete, &mut budget).map_err(anyhow::Error::new),
        "basis validation",
    );
    let mut budget = ProjectionBudget::standard(ProjectionName::RustModule);
    assert_domain(
        complement_basis(&members, &forged_refs, &[], &complete, &mut budget)
            .map(|_| ())
            .map_err(anyhow::Error::new),
        "complement",
    );
    let mut budget = ProjectionBudget::standard(ProjectionName::RustModule);
    assert_domain(
        validate_cycle_completion(
            ProjectionName::RustModule,
            &members,
            &forged_refs,
            &[],
            &complete.cycles,
            complete.rank,
            &mut budget,
        )
        .map_err(anyhow::Error::new),
        "completion validation",
    );
}

#[test]
fn cycle_member_and_domain_materialization_have_separate_exact_precharges() {
    let (members, edges) = triangle();
    let mut setup = ProjectionBudget::standard(ProjectionName::RustModule);
    let decomposition =
        decompose(ProjectionName::RustModule, &members, &edges, &mut setup).expect("SCCs");

    let edge_refs = edges.iter().collect::<Vec<_>>();
    let domain_work = independent_cycle_domain_work(&members, &edge_refs);
    for (limit, phase) in [
        (3, ValidationPhase::CycleMember),
        (4 + domain_work - 1, ValidationPhase::CycleDomain),
    ] {
        take_cycle_events();
        let mut short = ProjectionBudget::with_limit(ProjectionName::RustModule, limit);
        with_validation_observer_for_test(observe_cycle_phase, || {
            component_bases(&decomposition, &mut short)
        })
        .expect_err("one below the test-owned cycle phase prefix");
        assert_eq!(
            count_cycle_event(ValidationOperation::PhasePrecharged(phase)),
            0,
            "target cycle phase began before its exact precharge"
        );
    }

    take_cycle_events();
    let mut exact_member = ProjectionBudget::with_limit(ProjectionName::RustModule, 4);
    with_validation_observer_for_test(observe_cycle_phase, || {
        component_bases(&decomposition, &mut exact_member)
    })
    .expect_err("the next cycle-domain phase exceeds the exact member prefix");
    let events = take_cycle_events();
    assert_eq!(
        events
            .iter()
            .filter(|event| {
                **event == ValidationOperation::PhaseVisit(ValidationPhase::CycleMember)
            })
            .count(),
        3
    );
    assert!(events.contains(&ValidationOperation::PhaseAllocation(
        ValidationPhase::CycleMember
    )));

    take_cycle_events();
    let mut exact_domain =
        ProjectionBudget::with_limit(ProjectionName::RustModule, 4 + domain_work);
    with_validation_observer_for_test(observe_cycle_phase, || {
        component_bases(&decomposition, &mut exact_domain)
    })
    .expect_err("the next directed-rank phase exceeds the exact domain prefix");
    let events = take_cycle_events();
    assert_eq!(
        events
            .iter()
            .filter(|event| {
                **event == ValidationOperation::PhaseVisit(ValidationPhase::CycleDomain)
            })
            .count(),
        7
    );
    assert!(events.contains(&ValidationOperation::PhaseAllocation(
        ValidationPhase::CycleDomain
    )));
}

#[test]
fn cycle_domain_and_member_hash_work_are_content_sensitive_before_source_scans() {
    let (short_members, short_edges) = padded_triangle(0);
    let (long_members, long_edges) = padded_triangle(4096);
    let short_refs = short_edges.iter().collect::<Vec<_>>();
    let long_refs = long_edges.iter().collect::<Vec<_>>();
    let short_domain = independent_cycle_domain_work(&short_members, &short_refs);
    let long_domain = independent_cycle_domain_work(&long_members, &long_refs);
    assert!(long_domain > short_domain);

    CYCLE_JSON_SOURCE_BYTES.store(0, Ordering::Relaxed);
    let mut domain_one_below =
        ProjectionBudget::with_limit(ProjectionName::RustModule, long_domain - 1);
    with_json_observer_for_test(observe_cycle_json, || {
        basis_for_subgraph(
            ProjectionName::RustModule,
            &long_members,
            &long_refs,
            &mut domain_one_below,
        )
    })
    .expect_err("one below long cycle-domain preflight");
    assert_eq!(CYCLE_JSON_SOURCE_BYTES.load(Ordering::Relaxed), 0);

    CYCLE_JSON_SOURCE_BYTES.store(0, Ordering::Relaxed);
    assert_eq!(
        with_json_observer_for_test(observe_cycle_json, || {
            validate_cycle_domain_for_test(
                ProjectionName::RustModule,
                &long_members,
                &long_refs,
                long_domain,
            )
        })
        .expect("exact independent cycle-domain boundary"),
        long_domain
    );
    assert!(CYCLE_JSON_SOURCE_BYTES.load(Ordering::Relaxed) > 0);

    let long_member = independent_cycle_member_hash_work(&long_members);
    assert!(long_member > independent_cycle_member_hash_work(&short_members));
    let mut setup = ProjectionBudget::standard(ProjectionName::RustModule);
    let decomposition = decompose(
        ProjectionName::RustModule,
        &long_members,
        &long_edges,
        &mut setup,
    )
    .expect("long-member SCC");
    let complete = component_bases(&decomposition, &mut setup)
        .expect("long-member basis")
        .pop()
        .expect("long-member cyclic component");
    let member_prefix = long_domain + long_member;

    take_cycle_events();
    CYCLE_JSON_SOURCE_BYTES.store(0, Ordering::Relaxed);
    let mut one_below = ProjectionBudget::with_limit(ProjectionName::RustModule, member_prefix - 1);
    with_validation_observer_for_test(observe_member_hash_phase, || {
        with_json_observer_for_test(observe_cycle_json, || {
            validate_basis(&long_members, &long_refs, &complete, &mut one_below)
        })
    })
    .expect_err("one below long cycle member-hash boundary");
    assert_eq!(CYCLE_JSON_SOURCE_BYTES.load(Ordering::Relaxed), 0);
    assert_eq!(
        count_cycle_event(ValidationOperation::PhasePrecharged(
            ValidationPhase::CycleMemberHash
        )),
        0
    );

    take_cycle_events();
    CYCLE_JSON_SOURCE_BYTES.store(0, Ordering::Relaxed);
    let mut exact = ProjectionBudget::with_limit(ProjectionName::RustModule, member_prefix);
    with_validation_observer_for_test(observe_member_hash_phase, || {
        with_json_observer_for_test(observe_cycle_json, || {
            validate_basis(&long_members, &long_refs, &complete, &mut exact)
        })
    })
    .expect_err("the next actual phase exceeds the exact cycle-member-hash boundary");
    let events = take_cycle_events();
    assert_eq!(
        events_matching(
            &events,
            ValidationOperation::PhasePrecharged(ValidationPhase::CycleMemberHash)
        ),
        1
    );
    assert_eq!(
        events_matching(
            &events,
            ValidationOperation::PhaseVisit(ValidationPhase::CycleMemberHash)
        ),
        long_members.len()
    );
    assert_eq!(
        events_matching(
            &events,
            ValidationOperation::PhaseAllocation(ValidationPhase::CycleMemberHash)
        ),
        1
    );
    assert!(CYCLE_JSON_SOURCE_BYTES.load(Ordering::Relaxed) > 0);

    let long_members = long_members.iter().cloned().collect::<Vec<_>>();
    let complete_member_hash =
        super::scc::component_member_work(ProjectionName::RustModule, &long_members, 2);
    let digest = super::scc::independent_component(ProjectionName::RustModule, &long_members);
    xtask::architecture::test_support::validate_component_members_for_test(
        &digest,
        ProjectionName::RustModule,
        &long_members,
        complete_member_hash,
    )
    .expect("independent complete component-member boundary");
}

#[test]
fn cycle_member_hash_and_linear_rows_stop_at_their_exact_phase_boundaries() {
    let (members, edges) = triangle();
    let edge_refs = edges.iter().collect::<Vec<_>>();
    let mut setup = ProjectionBudget::standard(ProjectionName::RustModule);
    let decomposition =
        decompose(ProjectionName::RustModule, &members, &edges, &mut setup).expect("SCCs");
    let mut complete = component_bases(&decomposition, &mut setup)
        .expect("basis")
        .pop()
        .expect("cyclic component");
    let member_vector = members.iter().cloned().collect::<Vec<_>>();
    complete.component =
        super::scc::independent_component(ProjectionName::RustModule, &member_vector);

    let member_prefix = independent_cycle_domain_work(&members, &edge_refs)
        + independent_cycle_member_hash_work(&members);
    let linear_sizing_prefix = member_prefix
        + super::scc::component_member_work(ProjectionName::RustModule, &member_vector, 2)
        + complete.cycles.len();
    let rows = complete.cycles.len();
    let words = edge_refs.len().div_ceil(64);
    let witness_edges = complete
        .cycles
        .iter()
        .map(|witness| witness.edges.len())
        .sum::<usize>();
    let linear_allocation_prefix = linear_sizing_prefix
        + rows * (rows + 1) * words
        + rows * words
        + witness_edges
        + edge_refs.len();

    for (limit, phase) in [
        (member_prefix - 1, ValidationPhase::CycleMemberHash),
        (linear_sizing_prefix - 1, ValidationPhase::CycleLinearSizing),
        (
            linear_allocation_prefix - 1,
            ValidationPhase::CycleLinearAllocation,
        ),
    ] {
        take_cycle_events();
        let mut short = ProjectionBudget::with_limit(ProjectionName::RustModule, limit);
        with_validation_observer_for_test(observe_cycle_phase, || {
            validate_basis(&members, &edge_refs, &complete, &mut short)
        })
        .expect_err("one below the test-owned cycle phase prefix");
        assert_eq!(
            count_cycle_event(ValidationOperation::PhasePrecharged(phase)),
            0,
            "target cycle phase began before its exact precharge"
        );
    }

    for (limit, phase, visits) in [
        (member_prefix, ValidationPhase::CycleMemberHash, 3),
        (linear_sizing_prefix, ValidationPhase::CycleLinearSizing, 4),
        (
            linear_allocation_prefix,
            ValidationPhase::CycleLinearAllocation,
            0,
        ),
    ] {
        take_cycle_events();
        let mut exact = ProjectionBudget::with_limit(ProjectionName::RustModule, limit);
        with_validation_observer_for_test(observe_cycle_phase, || {
            validate_basis(&members, &edge_refs, &complete, &mut exact)
        })
        .expect_err("the next cycle phase exceeds the exact target prefix");
        let events = take_cycle_events();
        assert_eq!(
            events_matching(&events, ValidationOperation::PhasePrecharged(phase)),
            1,
            "exact target phase admission drifted"
        );
        assert_eq!(
            events_matching(&events, ValidationOperation::PhaseVisit(phase)),
            visits,
            "exact target phase traversal drifted"
        );
        assert_eq!(
            events_matching(&events, ValidationOperation::PhaseAllocation(phase)),
            usize::from(phase != ValidationPhase::CycleLinearSizing),
            "exact target phase allocation drifted"
        );
    }
}

#[test]
fn rank_three_sparse_basis_uses_one_exact_packed_storage_reservation() {
    let members = (0..7).map(identity).collect::<BTreeSet<_>>();
    let edges = [
        edge(0, 1, "01"),
        edge(1, 0, "10"),
        edge(2, 3, "23"),
        edge(3, 4, "34"),
        edge(4, 2, "42"),
        edge(2, 4, "24"),
        edge(1, 2, "12-crossing"),
        edge(5, 6, "56-acyclic"),
    ];
    let edge_refs = edges.iter().collect::<Vec<_>>();
    let prefix =
        cycle_basis_fresh_prefix_for_test(ProjectionName::RustModule, &members, &edge_refs)
            .expect("fresh domain/rank phase snapshot");
    let reservation = independent_basis_reservation(members.len(), edges.len(), 3);
    with_validation_observer_for_test(observe_basis_heap, || {});
    let mut short = ProjectionBudget::with_limits(
        ProjectionName::RustModule,
        usize::MAX,
        prefix + reservation - 1,
    );
    BASIS_HEAP[1].store(0, Ordering::Relaxed);
    BASIS_HEAP[2].store(0, Ordering::Relaxed);
    reset_storage_events();
    let error = with_cycle_storage_observer_for_test(observe_cycle_storage, || {
        with_validation_observer_for_test(observe_basis_heap, || {
            basis_for_subgraph(ProjectionName::RustModule, &members, &edge_refs, &mut short)
        })
    })
    .expect_err("one byte below packed search storage");
    assert_typed_error_heap(3);
    let _ = error;
    assert_eq!(BASIS_HEAP[1].load(Ordering::Relaxed), 0);
    assert_eq!(BASIS_HEAP[2].load(Ordering::Relaxed), 0);
    assert_eq!(storage_counts()[12..16], [0; 4]);

    let mut exact =
        ProjectionBudget::with_limits(ProjectionName::RustModule, usize::MAX, prefix + reservation);
    reset_storage_events();
    let (rank, cycles) = with_cycle_storage_observer_for_test(observe_cycle_storage, || {
        with_validation_observer_for_test(observe_basis_heap, || {
            basis_for_subgraph(ProjectionName::RustModule, &members, &edge_refs, &mut exact)
        })
    })
    .expect("exact packed search storage");
    assert_eq!((rank, cycles.len()), (3, 3));
    assert_eq!(exact.bytes_reserved(), prefix + reservation);
    let witness_edges = cycles.iter().map(|cycle| cycle.edges.len()).sum();
    let (expected_retained, expected_peak) =
        independent_basis_observed(members.len(), edges.len(), 3, witness_edges);
    let retained = ROW_HEAP[1].load(Ordering::Relaxed);
    let peak = ROW_HEAP[2].load(Ordering::Relaxed);
    assert_eq!(retained as usize, expected_retained);
    assert_eq!(peak as usize, expected_peak);
    let counts = storage_counts();
    assert_eq!((counts[12], counts[14], counts[15]), (1, 1, 1));
    assert!(
        counts[13] as usize > rank,
        "actual sparse search never rejected a dependent candidate"
    );
}

#[test]
fn witness_validation_uses_one_reusable_indexed_bitset_with_borrowed_long_endpoints() {
    let (members, edges) = padded_triangle(4096);
    let refs = edges.iter().collect::<Vec<_>>();
    let mut setup = ProjectionBudget::standard(ProjectionName::RustModule);
    let decomposition = decompose(ProjectionName::RustModule, &members, &edges, &mut setup)
        .expect("long-endpoint SCC");
    let basis = component_bases(&decomposition, &mut setup)
        .expect("long-endpoint basis")
        .pop()
        .expect("cyclic basis");
    let exact_bytes = independent_validate_basis_bytes(&members, &refs, &basis);
    let words = edges.len().div_ceil(64);
    let expected_heap = edges.len() * std::mem::size_of::<Option<Box<[u64]>>>()
        + (basis.rank + 1) * words * std::mem::size_of::<u64>()
        + edges.len().div_ceil(8);

    reset_storage_events();
    let mut short =
        ProjectionBudget::with_limits(ProjectionName::RustModule, usize::MAX, exact_bytes - 1);
    let error = with_cycle_storage_observer_for_test(observe_cycle_storage, || {
        validate_basis(&members, &refs, &basis, &mut short)
    })
    .expect_err("one below complete actual witness-validation storage");
    assert_typed_error_heap(0);
    let _ = error;
    assert_eq!(storage_counts()[..4], [0; 4]);

    reset_storage_events();
    let mut exact =
        ProjectionBudget::with_limits(ProjectionName::RustModule, usize::MAX, exact_bytes);
    with_cycle_storage_observer_for_test(observe_cycle_storage, || {
        validate_basis(&members, &refs, &basis, &mut exact)
    })
    .expect("exact complete actual witness-validation storage");
    assert_eq!(
        WITNESS_HEAP[1].load(Ordering::Relaxed) as usize,
        expected_heap
    );
    assert_eq!(
        WITNESS_HEAP[2].load(Ordering::Relaxed) as usize,
        expected_heap
    );
    let counts = storage_counts();
    assert_eq!((counts[0], counts[2], counts[3]), (1, 1, 1));
    assert_eq!(counts[1] as usize, basis.cycles.len());

    let source = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src/architecture/project/cycle.rs"),
    )
    .expect("cycle source");
    let start = source
        .find("fn validate_witness(")
        .expect("witness validator");
    let end = source[start..]
        .find("fn witness_vector(")
        .map(|offset| start + offset)
        .expect("witness validator end");
    let validator = &source[start..end];
    assert!(!validator.contains("BTreeSet"));
    assert!(!validator.contains("record.from.clone()"));
    assert!(!validator.contains("record.to.clone()"));
}

#[test]
fn duplicate_witness_checking_scales_by_edge_bits_not_btree_rounding() {
    let count = 130;
    let members = (0..count).map(identity).collect::<BTreeSet<_>>();
    let edges = (0..count)
        .map(|index| edge(index, (index + 1) % count, &format!("ring-{index}")))
        .collect::<Vec<_>>();
    let refs = edges.iter().collect::<Vec<_>>();
    let duplicate = CycleWitness {
        edges: vec![edges[0].identity.clone(), edges[0].identity.clone()],
    };
    let mut setup = ProjectionBudget::standard(ProjectionName::RustModule);
    let decomposition =
        decompose(ProjectionName::RustModule, &members, &edges, &mut setup).expect("ring SCC");
    let complete = component_bases(&decomposition, &mut setup)
        .expect("ring basis")
        .pop()
        .expect("cyclic ring");
    let duplicate_basis = xtask::architecture::project::cycle::ComponentBasis {
        component: complete.component,
        rank: 1,
        cycles: vec![duplicate],
    };
    let exact_bytes = independent_validate_basis_bytes(&members, &refs, &duplicate_basis);
    reset_storage_events();
    let mut budget =
        ProjectionBudget::with_limits(ProjectionName::RustModule, usize::MAX, exact_bytes);
    let error = with_cycle_storage_observer_for_test(observe_cycle_storage, || {
        validate_basis(&members, &refs, &duplicate_basis, &mut budget)
    })
    .expect_err("duplicate typed edge in one witness");
    assert_typed_error_heap(4);
    assert!(error.to_string().contains("CYCLE_BASIS_INCOMPLETE"));
    assert_eq!(storage_counts()[..4], [1, 1, 1, 0]);
}

#[test]
fn actual_complement_output_has_exact_test_owned_storage() {
    let (members, edges) = padded_triangle(4096);
    let mut setup = ProjectionBudget::standard(ProjectionName::RustModule);
    let decomposition = decompose(ProjectionName::RustModule, &members, &edges, &mut setup)
        .expect("complement SCC");
    let basis = component_bases(&decomposition, &mut setup)
        .expect("complement basis")
        .pop()
        .expect("cyclic basis");
    let seed = &basis.cycles[..basis.cycles.len() - 1];
    let witness = basis.cycles.last().expect("complement witness");
    let complement_bytes = std::mem::size_of::<CycleWitness>()
        + witness.edges.len() * std::mem::size_of::<EdgeIdentity>()
        + witness
            .edges
            .iter()
            .map(|edge| edge.as_str().len())
            .sum::<usize>();
    let basis_witness_edges = basis
        .cycles
        .iter()
        .map(|cycle| cycle.edges.len())
        .sum::<usize>();
    let complement_linear_edges = seed
        .iter()
        .chain(&basis.cycles)
        .map(|cycle| cycle.edges.len())
        .sum::<usize>();
    let complement_prefix =
        independent_cycle_domain_bytes(&members, &edges.iter().collect::<Vec<_>>())
            + 64
            + independent_member_hash_bytes(&members)
            + 64
            + independent_linear_bytes(edges.len(), basis.cycles.len(), basis_witness_edges)
            + independent_scc_rank_bytes(&members, &edges.iter().collect::<Vec<_>>())
            + independent_linear_bytes(
                edges.len(),
                seed.len() + basis.cycles.len(),
                complement_linear_edges,
            );
    let refs = edges.iter().collect::<Vec<_>>();

    reset_storage_events();
    let mut short = ProjectionBudget::with_limits(
        ProjectionName::RustModule,
        usize::MAX,
        complement_prefix + complement_bytes - 1,
    );
    let error = with_cycle_storage_observer_for_test(observe_cycle_storage, || {
        complement_basis(&members, &refs, seed, &basis, &mut short)
    })
    .expect_err("one below complement output storage");
    assert_typed_error_heap(1);
    let _ = error;
    assert_eq!(storage_counts()[4..8], [0; 4]);

    reset_storage_events();
    let mut exact = ProjectionBudget::with_limits(
        ProjectionName::RustModule,
        usize::MAX,
        complement_prefix + complement_bytes,
    );
    with_cycle_storage_observer_for_test(observe_cycle_storage, || {
        complement_basis(&members, &refs, seed, &basis, &mut exact)
    })
    .expect_err("the next actual validation phase exceeds exact complement storage");
    assert_eq!(
        COMPLEMENT_HEAP[1].load(Ordering::Relaxed) as usize,
        complement_bytes
    );
    assert_eq!(
        COMPLEMENT_HEAP[2].load(Ordering::Relaxed) as usize,
        complement_bytes
    );
    assert_eq!(storage_counts()[4..8], [1, 1, 1, 1]);
}

#[test]
fn actual_component_member_view_has_exact_test_owned_storage() {
    let (members, edges) = padded_triangle(4096);
    let mut setup = ProjectionBudget::standard(ProjectionName::RustModule);
    let decomposition =
        decompose(ProjectionName::RustModule, &members, &edges, &mut setup).expect("member SCC");
    let member_bytes = members.len() * std::mem::size_of::<&Identity>();
    let basis_output_bytes = decomposition.components().len()
        * std::mem::size_of::<xtask::architecture::project::cycle::ComponentBasis>();
    reset_storage_events();
    let mut short = ProjectionBudget::with_limits(
        ProjectionName::RustModule,
        usize::MAX,
        basis_output_bytes + member_bytes - 1,
    );
    let error = with_cycle_storage_observer_for_test(observe_cycle_storage, || {
        component_bases(&decomposition, &mut short)
    })
    .expect_err("one below borrowed member view");
    assert_typed_error_heap(2);
    let _ = error;
    assert_eq!(storage_counts()[8..12], [0; 4]);

    reset_storage_events();
    let mut exact = ProjectionBudget::with_limits(
        ProjectionName::RustModule,
        usize::MAX,
        basis_output_bytes + member_bytes,
    );
    with_cycle_storage_observer_for_test(observe_cycle_storage, || {
        component_bases(&decomposition, &mut exact)
    })
    .expect_err("the next actual component phase exceeds exact member storage");
    assert_eq!(
        MEMBER_HEAP[1].load(Ordering::Relaxed) as usize,
        member_bytes
    );
    assert_eq!(
        MEMBER_HEAP[2].load(Ordering::Relaxed) as usize,
        member_bytes
    );
    assert_eq!(storage_counts()[8..12], [1, 3, 1, 1]);
}
