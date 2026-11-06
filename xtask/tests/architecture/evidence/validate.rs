use xtask::architecture::evidence::validate::{
    canonical_endpoint_search_arithmetic_for_test, identity_two_field_width_arithmetic_for_test,
    identity_width_construction_arithmetic_for_test,
};
use xtask::architecture::project::cluster::ClusterSignal;
use xtask::architecture::project::project;
use xtask::architecture::schema::{
    Attribute, CanonicalText, ContextId, DocumentationId, EdgeFact, Fact, GraphArtifact, Identity,
    NodeFact, NodeKind, Obligation, ObligationCode, ObligationSubject, PackageFaceId,
    PhysicalIdentity, PhysicalKind, PipelineStage, ProductId, ProjectionName, RepoPath,
    RepositoryId, Sha256Digest, SourceAnchor, Truth, WorkspaceId, WorktreeId, exact_node,
};
use xtask::architecture::test_support::{
    JsonOperation, ValidationOperation, ValidationPhase, unchecked_canonical_text_for_test,
    validate_canonical_limits_for_test, validate_canonical_prefix_for_test,
    with_json_observer_for_test, with_percent_decode_observer_for_test,
};

use serde::Serialize;
use serde::ser::{
    SerializeMap, SerializeSeq, SerializeStruct, SerializeStructVariant, SerializeTuple,
    SerializeTupleStruct, SerializeTupleVariant, Serializer,
};
use std::collections::BTreeSet;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};

static VALIDATION_AUDIT_COUNTS: [AtomicUsize; 8] = [const { AtomicUsize::new(0) }; 8];
static VALIDATION_AUDIT_BASELINE: AtomicU64 = AtomicU64::new(0);
static VALIDATION_AUDIT_FIRST_ACTIVE: AtomicU64 = AtomicU64::new(u64::MAX);
static VALIDATION_PHASE_COUNTS: [[AtomicUsize; 3]; 9] =
    [const { [const { AtomicUsize::new(0) }; 3] }; 9];
static CANONICAL_SOURCE_SCANS: AtomicUsize = AtomicUsize::new(0);
static CANONICAL_PREFLIGHT_TOUCHES: AtomicUsize = AtomicUsize::new(0);
static CANONICAL_PERCENT_DECODES: AtomicUsize = AtomicUsize::new(0);
static CANONICAL_PHASE_HEAP: [AtomicU64; 3] = [const { AtomicU64::new(0) }; 3];
static VALIDATION_PHASE_EVENT_CLOCK: AtomicUsize = AtomicUsize::new(0);
static IDENTITY_WIDTH_PRECHARGE_ORDER: AtomicUsize = AtomicUsize::new(usize::MAX);
static IDENTITY_WIDTH_FIRST_VISIT_ORDER: AtomicUsize = AtomicUsize::new(usize::MAX);

fn observe_canonical_json(operation: JsonOperation) {
    match operation {
        JsonOperation::PreflightBytesTouched(bytes) => {
            CANONICAL_PREFLIGHT_TOUCHES.fetch_add(bytes, Ordering::Relaxed);
        }
        JsonOperation::SourceBytesScanned(bytes) => {
            CANONICAL_SOURCE_SCANS.fetch_add(bytes, Ordering::Relaxed);
        }
        JsonOperation::FragmentWritten(_) => {}
    }
}

fn observe_canonical_percent(
    _operation: xtask::architecture::test_support::PercentDecodeOperation,
) {
    CANONICAL_PERCENT_DECODES.fetch_add(1, Ordering::Relaxed);
}

fn observe_validation(operation: ValidationOperation) {
    let event_order = VALIDATION_PHASE_EVENT_CLOCK.fetch_add(1, Ordering::Relaxed);
    match operation {
        ValidationOperation::PhasePrecharged(ValidationPhase::IdentityWidthConstruction) => {
            IDENTITY_WIDTH_PRECHARGE_ORDER.store(event_order, Ordering::Relaxed);
        }
        ValidationOperation::PhaseVisit(ValidationPhase::IdentityWidthConstruction) => {
            IDENTITY_WIDTH_FIRST_VISIT_ORDER
                .compare_exchange(
                    usize::MAX,
                    event_order,
                    Ordering::Relaxed,
                    Ordering::Relaxed,
                )
                .ok();
        }
        _ => {}
    }
    match operation {
        ValidationOperation::PhasePrecharged(ValidationPhase::CanonicalPrivacy) => {
            CANONICAL_PHASE_HEAP[0].store(crate::take_heap_usage().0, Ordering::Relaxed);
        }
        ValidationOperation::PhaseAllocation(ValidationPhase::CanonicalPrivacy) => {
            let (active, peak) = crate::take_heap_usage();
            let baseline = CANONICAL_PHASE_HEAP[0].load(Ordering::Relaxed);
            CANONICAL_PHASE_HEAP[1].store(active.saturating_sub(baseline), Ordering::Relaxed);
            CANONICAL_PHASE_HEAP[2].store(peak.saturating_sub(baseline), Ordering::Relaxed);
        }
        _ => {}
    }
    VALIDATION_AUDIT_FIRST_ACTIVE
        .compare_exchange(
            u64::MAX,
            crate::ACTIVE_HEAP_BYTES.load(Ordering::Relaxed),
            Ordering::Relaxed,
            Ordering::Relaxed,
        )
        .ok();
    let index = match operation {
        ValidationOperation::Setup => Some(0),
        ValidationOperation::Selected => Some(1),
        ValidationOperation::Member => Some(2),
        ValidationOperation::Owner => Some(3),
        ValidationOperation::Signature => Some(4),
        ValidationOperation::Incidence => Some(5),
        ValidationOperation::Connectivity => Some(6),
        ValidationOperation::Quotient => Some(7),
        ValidationOperation::PhasePrecharged(phase) => {
            VALIDATION_PHASE_COUNTS[phase as usize][0].fetch_add(1, Ordering::Relaxed);
            None
        }
        ValidationOperation::PhaseVisit(phase) => {
            assert!(
                VALIDATION_PHASE_COUNTS[phase as usize][0].load(Ordering::Relaxed) > 0,
                "phase visit occurred before any admission: {phase:?}"
            );
            VALIDATION_PHASE_COUNTS[phase as usize][1].fetch_add(1, Ordering::Relaxed);
            None
        }
        ValidationOperation::PhaseAllocation(phase) => {
            assert!(
                VALIDATION_PHASE_COUNTS[phase as usize][0].load(Ordering::Relaxed)
                    > VALIDATION_PHASE_COUNTS[phase as usize][2].load(Ordering::Relaxed),
                "phase allocation occurred without a matching admission: {phase:?}"
            );
            VALIDATION_PHASE_COUNTS[phase as usize][2].fetch_add(1, Ordering::Relaxed);
            None
        }
    };
    if let Some(index) = index {
        VALIDATION_AUDIT_COUNTS[index].fetch_add(1, Ordering::Relaxed);
    }
}

fn reset_validation_audit() {
    for count in &VALIDATION_AUDIT_COUNTS {
        count.store(0, Ordering::Relaxed);
    }
    for phase in &VALIDATION_PHASE_COUNTS {
        for count in phase {
            count.store(0, Ordering::Relaxed);
        }
    }
    VALIDATION_PHASE_EVENT_CLOCK.store(0, Ordering::Relaxed);
    IDENTITY_WIDTH_PRECHARGE_ORDER.store(usize::MAX, Ordering::Relaxed);
    IDENTITY_WIDTH_FIRST_VISIT_ORDER.store(usize::MAX, Ordering::Relaxed);
    VALIDATION_AUDIT_FIRST_ACTIVE.store(u64::MAX, Ordering::Relaxed);
}

fn validation_counts() -> [usize; 8] {
    std::array::from_fn(|index| VALIDATION_AUDIT_COUNTS[index].load(Ordering::Relaxed))
}

fn phase_counts(phase: ValidationPhase) -> [usize; 3] {
    std::array::from_fn(|index| {
        VALIDATION_PHASE_COUNTS[phase as usize][index].load(Ordering::Relaxed)
    })
}

fn all_phase_counts() -> [[usize; 3]; 9] {
    std::array::from_fn(|phase| {
        std::array::from_fn(|event| VALIDATION_PHASE_COUNTS[phase][event].load(Ordering::Relaxed))
    })
}

fn fixture_graph_preflight(graph: &GraphArtifact) -> usize {
    let mut serializer = FixtureJsonWork {
        work: 0,
        source_passes: 0,
        output_passes: 0,
        map_key: false,
        raw_string_bytes: 0,
        percent_string_bytes: 0,
        max_percent_string_bytes: 0,
        traversal_work: 0,
    };
    graph
        .serialize(&mut serializer)
        .expect("sealed fixture graph dry-preflight shape");
    serializer
        .work
        .checked_mul(2)
        .and_then(|work| work.checked_add(serializer.raw_string_bytes))
        .expect("fixture paired graph dry-preflight work")
}

#[derive(Clone, Copy)]
struct CanonicalPrefix {
    setup: usize,
    preflight: usize,
    comparison: usize,
    canonical: usize,
}

#[allow(dead_code)]
struct FixtureDecodeByte {
    byte: u8,
    previous: Option<usize>,
    next: Option<usize>,
    live: bool,
    generation: usize,
    queued: usize,
}

fn fixture_percent_decode_allocation_bound(input_bytes: usize) -> usize {
    let arena_nodes = input_bytes
        .checked_add(
            (input_bytes / 3)
                .checked_mul(2)
                .expect("fixture decoder expansion"),
        )
        .expect("fixture decoder arena nodes");
    arena_nodes
        .checked_mul(std::mem::size_of::<FixtureDecodeByte>())
        .and_then(|bytes| {
            input_bytes
                .checked_mul(std::mem::size_of::<usize>() * 2)
                .and_then(|worklists| bytes.checked_add(worklists))
        })
        .and_then(|bytes| {
            input_bytes
                .checked_mul(3)
                .and_then(|scratch| bytes.checked_add(scratch))
        })
        .expect("fixture decoder allocation")
}

fn fixture_percent_decode_work_bound(input_bytes: usize) -> usize {
    let multiply = |count: usize, coefficient: usize| {
        count
            .checked_mul(coefficient)
            .expect("fixture percent decoder term")
    };
    let successful_decodes = input_bytes / 2;
    let invalid_decodes = input_bytes / 3;
    let arena_nodes = input_bytes
        .checked_add(multiply(invalid_decodes, 2))
        .expect("fixture decoder arena nodes");
    let generated_run_visits = successful_decodes;
    let generated_runs = successful_decodes;
    let utf8_and_equality = multiply(generated_run_visits, 2);
    let normalized_writes = multiply(generated_run_visits, 3);
    let retirements = multiply(successful_decodes, 2)
        .checked_add(generated_run_visits)
        .expect("fixture decoder retirements");
    let neighborhood_visits = multiply(generated_run_visits, 3)
        .checked_add(multiply(generated_runs, 4))
        .expect("fixture decoder neighborhood");
    let enqueues = input_bytes
        .checked_add(neighborhood_visits)
        .expect("fixture decoder enqueues");
    let queue_actions = multiply(enqueues, 2);
    let terms = [
        multiply(input_bytes, 4),
        multiply(arena_nodes, 7),
        queue_actions,
        multiply(enqueues, 24),
        multiply(successful_decodes, 10),
        multiply(successful_decodes, 8),
        multiply(generated_run_visits, 6),
        multiply(utf8_and_equality, 4),
        multiply(normalized_writes, 12),
        multiply(retirements, 6),
        multiply(neighborhood_visits, 12),
        multiply(input_bytes, 4),
        successful_decodes,
    ];
    let total = terms
        .into_iter()
        .try_fold(0usize, usize::checked_add)
        .expect("fixture percent decoder ledger");
    assert!(
        total <= multiply(input_bytes, 256),
        "fixture decoder derivation exceeds its named ceiling"
    );
    total
}

fn fixture_canonical_prefix(graph: &GraphArtifact) -> CanonicalPrefix {
    let complete = complete_canonical_ledger(graph);
    CanonicalPrefix {
        setup: fixture_setup_work(graph),
        preflight: complete.preflight(),
        comparison: complete.comparison(),
        canonical: complete.canonical(),
    }
}

fn canonical_fixture(keys: &[&str], labels: &[String], resolvers: &[String]) -> GraphArtifact {
    let mut graph = GraphArtifact::new();
    for (key, label) in keys.iter().zip(labels) {
        graph.exact(
            exact_node(
                crate::project::identity(key),
                NodeKind::VirtualArtifact,
                label,
                Vec::new(),
            )
            .expect("fixture node"),
        );
    }
    for resolver in resolvers {
        graph.exact(
            xtask::architecture::schema::exact_edge(
                crate::project::identity(keys[0]),
                crate::project::identity(keys[1]),
                xtask::architecture::schema::EdgeKind::RustUses,
                xtask::architecture::schema::Condition::Always,
                SourceAnchor::Synthesized {
                    authority: CanonicalText::new("canonical-prefix").expect("authority"),
                },
                xtask::architecture::schema::ResolverId::new(resolver).expect("resolver"),
                Vec::new(),
            )
            .expect("fixture edge"),
        );
    }
    graph.canonicalize().expect("canonical fixture");
    graph
}

#[derive(Clone, Copy, Debug)]
struct CompleteCanonicalLedger {
    structural: usize,
    weighted_structural_admission: usize,
    raw: usize,
    json: usize,
    percent: usize,
    max_identity_width: usize,
    decoder: usize,
    identity_width_construction: usize,
    graph_writer: usize,
    edge_writers: usize,
    privacy: usize,
    fixed_semantic: usize,
    comparison_non_search: usize,
    comparison_search: usize,
}

impl CompleteCanonicalLedger {
    fn preflight(self) -> usize {
        self.weighted_structural_admission
            .checked_mul(2)
            .and_then(|work| work.checked_add(self.raw))
            .expect("complete fixture preflight")
    }

    fn canonical(self) -> usize {
        [
            self.graph_writer,
            self.edge_writers,
            self.privacy,
            self.fixed_semantic,
        ]
        .into_iter()
        .try_fold(0usize, usize::checked_add)
        .expect("complete fixture canonical ledger")
    }

    fn comparison(self) -> usize {
        self.identity_width_construction
            .checked_add(self.comparison_charge())
            .expect("complete fixture construction and comparison ledger")
    }

    fn comparison_charge(self) -> usize {
        self.comparison_non_search
            .checked_add(self.comparison_search)
            .expect("complete fixture comparison ledger")
    }

    fn emit(self, fixture: &str) {
        for (term, amount) in [
            ("S-structural", self.structural),
            (
                "P-weighted-structural-admission",
                self.weighted_structural_admission,
            ),
            ("R-raw", self.raw),
            ("J-json", self.json),
            ("P-percent", self.percent),
            ("W-max-total-identity", self.max_identity_width),
            ("D-decoder", self.decoder),
            (
                "G-identity-width-construction",
                self.identity_width_construction,
            ),
            ("C-graph-writer", self.graph_writer),
            ("C-two-edge-writers", self.edge_writers),
            ("C-privacy", self.privacy),
            ("C-fixed-semantic", self.fixed_semantic),
            ("comparison-non-search", self.comparison_non_search),
            ("comparison-search", self.comparison_search),
        ] {
            eprintln!("R13_CANONICAL_LEDGER fixture={fixture} term={term} amount={amount}");
        }
    }
}

fn fixture_setup_work(graph: &GraphArtifact) -> usize {
    graph
        .truth
        .len()
        .checked_mul(6)
        .and_then(|work| {
            graph
                .nodes()
                .len()
                .checked_mul(2)
                .and_then(|v| work.checked_add(v))
        })
        .and_then(|work| {
            graph
                .edges()
                .len()
                .checked_mul(3)
                .and_then(|v| work.checked_add(v))
        })
        .expect("complete fixture setup")
}

fn fixture_identity_total_dynamic_string_bytes(identity: &Identity) -> usize {
    match identity {
        Identity::Repository { repository } => repository.as_str().len(),
        Identity::Product { product } => product.as_str().len(),
        Identity::Workspace { workspace } => workspace.as_str().len(),
        Identity::PackageFace { package_face } => package_face.as_str().len(),
        Identity::Context { context } => context.as_str().len(),
        Identity::Worktree { worktree } => worktree.as_str().len(),
        Identity::Documentation { documentation } => documentation.as_str().len(),
        Identity::Physical { physical } => physical
            .repository
            .as_str()
            .len()
            .checked_add(physical.path.as_str().len())
            .expect("fixture Physical total width"),
        Identity::Virtual { namespace, key } => namespace
            .as_str()
            .len()
            .checked_add(key.as_str().len())
            .expect("fixture Virtual total width"),
    }
}

fn fixture_graph_identity_total_dynamic_string_bytes(graph: &GraphArtifact) -> usize {
    graph.truth.iter().fold(0usize, |maximum, truth| {
        let current = match truth {
            Truth::Exact {
                fact: Fact::Node { node },
            } => fixture_identity_total_dynamic_string_bytes(&node.identity),
            Truth::Exact {
                fact: Fact::Edge { edge },
            } => fixture_identity_total_dynamic_string_bytes(&edge.record.from)
                .max(fixture_identity_total_dynamic_string_bytes(&edge.record.to)),
            Truth::Unknown { .. } => 0,
        };
        maximum.max(current)
    })
}

fn complete_canonical_ledger(graph: &GraphArtifact) -> CompleteCanonicalLedger {
    let mut serializer = FixtureJsonWork {
        work: 0,
        source_passes: 0,
        output_passes: 0,
        map_key: false,
        raw_string_bytes: 0,
        percent_string_bytes: 0,
        max_percent_string_bytes: 0,
        traversal_work: 0,
    };
    graph
        .serialize(&mut serializer)
        .expect("complete fixture graph shape");
    let structural = serializer.traversal_work;
    let weighted_structural_admission = serializer.work;
    let raw = serializer.raw_string_bytes;
    let json = serde_json::to_vec(graph)
        .expect("complete fixture exact JSON")
        .len();
    let percent = serializer.percent_string_bytes;
    let max_identity_width = fixture_graph_identity_total_dynamic_string_bytes(graph);
    let decoder = fixture_percent_decode_work_bound(percent);
    let identity_width_construction = graph
        .truth
        .len()
        .checked_mul(4)
        .and_then(|work| {
            graph
                .nodes()
                .len()
                .checked_mul(9)
                .and_then(|term| work.checked_add(term))
        })
        .and_then(|work| {
            graph
                .edges()
                .len()
                .checked_mul(19)
                .and_then(|term| work.checked_add(term))
        })
        .expect("complete fixture Identity-width construction");
    let graph_writer = structural
        .checked_add(raw)
        .and_then(|work| work.checked_add(json))
        .expect("complete fixture graph writer");
    let edge_writers = structural
        .checked_mul(2)
        .and_then(|work| raw.checked_mul(2).and_then(|v| work.checked_add(v)))
        .and_then(|work| json.checked_mul(2).and_then(|v| work.checked_add(v)))
        .expect("complete fixture two edge writers");
    let privacy = structural
        .checked_add(raw.checked_mul(2).expect("complete fixture privacy raw"))
        .and_then(|work| work.checked_add(decoder))
        .expect("complete fixture privacy");
    let fixed_semantic = graph
        .truth
        .len()
        .checked_mul(16)
        .expect("complete fixture fixed semantics");
    let comparison_non_search = structural
        .checked_add(raw)
        .and_then(|volume| volume.checked_mul(6))
        .expect("complete fixture non-search comparisons");
    let nodes = graph.nodes().len();
    let depth = if nodes == 0 {
        0
    } else {
        nodes
            .checked_next_power_of_two()
            .expect("complete fixture node power")
            .ilog2() as usize
            + 1
    };
    let comparison_search = graph
        .edges()
        .len()
        .checked_mul(2)
        .and_then(|work| work.checked_mul(depth))
        .and_then(|work| {
            max_identity_width
                .checked_mul(2)
                .and_then(|bytes| bytes.checked_add(32))
                .and_then(|per_comparison| work.checked_mul(per_comparison))
        })
        .expect("complete fixture search comparisons");
    CompleteCanonicalLedger {
        structural,
        weighted_structural_admission,
        raw,
        json,
        percent,
        max_identity_width,
        decoder,
        identity_width_construction,
        graph_writer,
        edge_writers,
        privacy,
        fixed_semantic,
        comparison_non_search,
        comparison_search,
    }
}

fn long_prefix_many_edge_graph() -> GraphArtifact {
    const NODES: usize = 16;
    const EDGES: usize = 24;
    let prefix = "shared-identity-prefix-".repeat(96);
    let identities = (0..NODES)
        .map(|index| crate::project::identity(&format!("{prefix}{index:04}")))
        .collect::<Vec<_>>();
    let mut graph = GraphArtifact::new();
    for (index, identity) in identities.iter().cloned().enumerate() {
        graph.exact(
            exact_node(
                identity,
                NodeKind::VirtualArtifact,
                format!("{prefix}label-{index:04}"),
                vec![Attribute::Feature {
                    feature: CanonicalText::new(format!("{prefix}feature-{index:04}"))
                        .expect("long-prefix feature"),
                }],
            )
            .expect("long-prefix node"),
        );
    }
    for index in 0..EDGES {
        graph.exact(
            xtask::architecture::schema::exact_edge(
                identities[index % NODES].clone(),
                identities[(index * 7 + 1) % NODES].clone(),
                xtask::architecture::schema::EdgeKind::RustUses,
                xtask::architecture::schema::Condition::Always,
                SourceAnchor::Synthesized {
                    authority: CanonicalText::new("r13-comparison").expect("authority"),
                },
                xtask::architecture::schema::ResolverId::new(format!(
                    "{prefix}resolver-{index:04}"
                ))
                .expect("resolver"),
                vec![Attribute::Feature {
                    feature: CanonicalText::new(format!("{prefix}edge-{index:04}"))
                        .expect("edge feature"),
                }],
            )
            .expect("long-prefix edge"),
        );
    }
    graph.canonicalize().expect("long-prefix graph");
    graph
}

#[test]
fn complete_canonical_privacy_charge_covers_a_long_no_percent_edge_record() {
    let graph = canonical_fixture(
        &["source", "target"],
        &["source".into(), "target".into()],
        &["r".repeat(128 * 1024)],
    );
    let ledger = complete_canonical_ledger(&graph);
    ledger.emit("long-no-percent-edge");
    assert_eq!(
        ledger.percent, 0,
        "fixture must isolate the no-percent path"
    );
    assert_eq!(ledger.decoder, 0, "no-percent decoder work");
    let complete = fixture_setup_work(&graph)
        .checked_add(ledger.preflight())
        .and_then(|work| work.checked_add(ledger.comparison()))
        .and_then(|work| work.checked_add(ledger.canonical()))
        .expect("complete no-percent boundary");
    assert!(complete <= 8_000_000, "fixture exceeds the standard cap");

    reset_validation_audit();
    let error = validate_canonical_prefix_for_test(
        &graph,
        ProjectionName::RustModule,
        complete - 1,
        usize::MAX,
        observe_validation,
    )
    .expect_err("one below complete C must reject before canonical/privacy work");
    assert!(
        error
            .to_string()
            .contains("validation-canonical-privacy-complete"),
        "{error}"
    );
    assert_eq!(phase_counts(ValidationPhase::CanonicalPrivacy), [0, 0, 0]);
}

#[test]
fn canonical_validation_reuses_the_exact_dry_width_measurement() {
    let graph = canonical_fixture(
        &["source", "target"],
        &["source".into(), "target".into()],
        &["w".repeat(128 * 1024)],
    );
    let ledger = complete_canonical_ledger(&graph);
    let complete = fixture_setup_work(&graph)
        .checked_add(ledger.preflight())
        .and_then(|work| work.checked_add(ledger.comparison()))
        .and_then(|work| work.checked_add(ledger.canonical()))
        .expect("complete width-reuse boundary");
    CANONICAL_PREFLIGHT_TOUCHES.store(0, Ordering::Relaxed);
    with_json_observer_for_test(observe_canonical_json, || {
        validate_canonical_prefix_for_test(
            &graph,
            ProjectionName::RustModule,
            complete,
            usize::MAX,
            observe_validation,
        )
    })
    .expect("complete canonical width-reuse boundary");
    assert_eq!(
        CANONICAL_PREFLIGHT_TOUCHES.load(Ordering::Relaxed),
        ledger.raw,
        "only the admitted exact-dry traversal may inspect JSON width"
    );
}

#[test]
fn complete_comparison_charge_covers_nested_long_prefix_many_edge_graphs() {
    let graph = long_prefix_many_edge_graph();
    let ledger = complete_canonical_ledger(&graph);
    ledger.emit("long-prefix-many-edge");
    assert!(
        ledger.comparison_search > 0,
        "fixture must exercise endpoint search"
    );
    let comparison_prefix = fixture_setup_work(&graph)
        .checked_add(ledger.preflight())
        .expect("comparison prefix");
    let error = validate_canonical_prefix_for_test(
        &graph,
        ProjectionName::RustModule,
        comparison_prefix + ledger.comparison() - 1,
        usize::MAX,
        observe_validation,
    )
    .expect_err("one below generalized comparison charge");
    assert!(
        error
            .to_string()
            .contains("validation-canonical-comparison-complete"),
        "{error}"
    );
    assert_eq!(phase_counts(ValidationPhase::CanonicalPrivacy), [0, 0, 0]);
}

#[test]
fn identity_width_construction_is_precharged_before_first_visit() {
    let endpoint_from = Identity::Physical {
        physical: PhysicalIdentity {
            repository: RepositoryId::new("construction-physical-repository")
                .expect("Physical repository"),
            path: RepoPath::new("construction/physical/path").expect("Physical path"),
            kind: PhysicalKind::File,
        },
    };
    let endpoint_to = Identity::Virtual {
        namespace: CanonicalText::new("construction-virtual-namespace").expect("Virtual namespace"),
        key: CanonicalText::new("construction-virtual-key").expect("Virtual key"),
    };
    let mut graph = GraphArtifact::new();
    graph.exact(
        xtask::architecture::schema::exact_edge(
            endpoint_from.clone(),
            endpoint_to.clone(),
            xtask::architecture::schema::EdgeKind::RustUses,
            xtask::architecture::schema::Condition::Always,
            SourceAnchor::Synthesized {
                authority: CanonicalText::new("identity-width-construction").expect("authority"),
            },
            xtask::architecture::schema::ResolverId::new("identity-width-construction")
                .expect("resolver"),
            Vec::new(),
        )
        .expect("construction fixture edge"),
    );
    let truth = graph.truth.len();
    let nodes = graph.nodes().len();
    let edges = graph.edges().len();
    assert_eq!((truth, nodes, edges), (1, 0, 1));
    assert!(
        graph
            .nodes()
            .iter()
            .all(|node| { node.identity != endpoint_from && node.identity != endpoint_to })
    );

    let construction = truth
        .checked_mul(4)
        .and_then(|work| nodes.checked_mul(9).and_then(|term| work.checked_add(term)))
        .and_then(|work| {
            edges
                .checked_mul(19)
                .and_then(|term| work.checked_add(term))
        })
        .expect("independent identity-width construction envelope");
    assert_eq!(construction, 23);
    let construction_prefix = fixture_setup_work(&graph)
        .checked_add(fixture_graph_preflight(&graph))
        .expect("independent construction prefix");

    reset_validation_audit();
    let error = validate_canonical_prefix_for_test(
        &graph,
        ProjectionName::RustModule,
        construction_prefix + construction - 1,
        usize::MAX,
        observe_validation,
    )
    .expect_err("one below G must reject before the first Identity visit");
    assert!(
        error
            .to_string()
            .contains("validation-identity-width-construction"),
        "{error}"
    );
    assert_eq!(
        phase_counts(ValidationPhase::IdentityWidthConstruction),
        [0, 0, 0]
    );

    reset_validation_audit();
    let error = validate_canonical_prefix_for_test(
        &graph,
        ProjectionName::RustModule,
        construction_prefix + construction,
        usize::MAX,
        observe_validation,
    )
    .expect_err("the exact G boundary isolates the following comparison charge");
    assert!(
        error
            .to_string()
            .contains("validation-canonical-comparison-complete"),
        "{error}"
    );
    assert_eq!(
        phase_counts(ValidationPhase::IdentityWidthConstruction),
        [1, nodes + 2 * edges, 0]
    );
    assert!(
        IDENTITY_WIDTH_PRECHARGE_ORDER.load(Ordering::Relaxed)
            < IDENTITY_WIDTH_FIRST_VISIT_ORDER.load(Ordering::Relaxed),
        "the precharge event must precede the first Identity match"
    );
}

#[test]
fn total_identity_width_is_grammar_total_and_checked() {
    fn common_prefix_bytes(left: &str, right: &str) -> usize {
        left.bytes()
            .zip(right.bytes())
            .take_while(|(left, right)| left == right)
            .count()
    }

    fn same_arm_common_prefix_bytes(left: &Identity, right: &Identity) -> Option<usize> {
        match (left, right) {
            (
                Identity::Repository { repository: left },
                Identity::Repository { repository: right },
            ) => Some(common_prefix_bytes(left.as_str(), right.as_str())),
            (Identity::Product { product: left }, Identity::Product { product: right }) => {
                Some(common_prefix_bytes(left.as_str(), right.as_str()))
            }
            (Identity::Workspace { workspace: left }, Identity::Workspace { workspace: right }) => {
                Some(common_prefix_bytes(left.as_str(), right.as_str()))
            }
            (
                Identity::PackageFace { package_face: left },
                Identity::PackageFace {
                    package_face: right,
                },
            ) => Some(common_prefix_bytes(left.as_str(), right.as_str())),
            (Identity::Context { context: left }, Identity::Context { context: right }) => {
                Some(common_prefix_bytes(left.as_str(), right.as_str()))
            }
            (Identity::Worktree { worktree: left }, Identity::Worktree { worktree: right }) => {
                Some(common_prefix_bytes(left.as_str(), right.as_str()))
            }
            (
                Identity::Documentation {
                    documentation: left,
                },
                Identity::Documentation {
                    documentation: right,
                },
            ) => Some(common_prefix_bytes(left.as_str(), right.as_str())),
            (Identity::Physical { physical: left }, Identity::Physical { physical: right }) => {
                Some(
                    common_prefix_bytes(left.repository.as_str(), right.repository.as_str())
                        .min(common_prefix_bytes(left.path.as_str(), right.path.as_str())),
                )
            }
            (
                Identity::Virtual {
                    namespace: left_namespace,
                    key: left_key,
                },
                Identity::Virtual {
                    namespace: right_namespace,
                    key: right_key,
                },
            ) => Some(
                common_prefix_bytes(left_namespace.as_str(), right_namespace.as_str())
                    .min(common_prefix_bytes(left_key.as_str(), right_key.as_str())),
            ),
            _ => None,
        }
    }

    const MIN_SAME_ARM_PREFIX_BYTES: usize = 96;
    let common = "identity-width-common-prefix-".repeat(16);
    let physical_path_prefix = "shared/physical/path/".repeat(8);
    let virtual_key_prefix = "shared-virtual-key-".repeat(8);
    let cases = [
        (
            "repository",
            Identity::Repository {
                repository: RepositoryId::new(format!("{common}repository-maximum"))
                    .expect("Repository maximum"),
            },
            Identity::Repository {
                repository: RepositoryId::new(format!("{common}repository-next"))
                    .expect("Repository counterpart"),
            },
            NodeKind::Repository,
            0usize,
        ),
        (
            "product",
            Identity::Product {
                product: ProductId::new(format!("{common}product-maximum"))
                    .expect("Product maximum"),
            },
            Identity::Product {
                product: ProductId::new(format!("{common}product-next"))
                    .expect("Product counterpart"),
            },
            NodeKind::Product,
            1,
        ),
        (
            "workspace",
            Identity::Workspace {
                workspace: WorkspaceId::new(format!("{common}workspace-maximum"))
                    .expect("Workspace maximum"),
            },
            Identity::Workspace {
                workspace: WorkspaceId::new(format!("{common}workspace-next"))
                    .expect("Workspace counterpart"),
            },
            NodeKind::Workspace,
            2,
        ),
        (
            "package-face",
            Identity::PackageFace {
                package_face: PackageFaceId::new(format!("{common}package-face-maximum"))
                    .expect("PackageFace maximum"),
            },
            Identity::PackageFace {
                package_face: PackageFaceId::new(format!("{common}package-face-next"))
                    .expect("PackageFace counterpart"),
            },
            NodeKind::PackageFace,
            3,
        ),
        (
            "context",
            Identity::Context {
                context: ContextId::new(format!("{common}context-maximum"))
                    .expect("Context maximum"),
            },
            Identity::Context {
                context: ContextId::new(format!("{common}context-next"))
                    .expect("Context counterpart"),
            },
            NodeKind::Context,
            4,
        ),
        (
            "worktree",
            Identity::Worktree {
                worktree: WorktreeId::new(format!("{common}worktree-maximum"))
                    .expect("Worktree maximum"),
            },
            Identity::Worktree {
                worktree: WorktreeId::new(format!("{common}worktree-next"))
                    .expect("Worktree counterpart"),
            },
            NodeKind::Worktree,
            127,
        ),
        (
            "documentation",
            Identity::Documentation {
                documentation: DocumentationId::new(format!("{common}documentation-maximum"))
                    .expect("Documentation maximum"),
            },
            Identity::Documentation {
                documentation: DocumentationId::new(format!("{common}documentation-next"))
                    .expect("Documentation counterpart"),
            },
            NodeKind::Documentation,
            128,
        ),
        (
            "physical",
            Identity::Physical {
                physical: PhysicalIdentity {
                    repository: RepositoryId::new(format!("{common}physical-repository-maximum"))
                        .expect("Physical maximum repository"),
                    path: RepoPath::new(format!("{physical_path_prefix}{common}maximum"))
                        .expect("Physical maximum path"),
                    kind: PhysicalKind::File,
                },
            },
            Identity::Physical {
                physical: PhysicalIdentity {
                    repository: RepositoryId::new(format!("{common}physical-repository-next"))
                        .expect("Physical counterpart repository"),
                    path: RepoPath::new(format!("{physical_path_prefix}next"))
                        .expect("Physical counterpart path"),
                    kind: PhysicalKind::File,
                },
            },
            NodeKind::PhysicalFile,
            129,
        ),
        (
            "virtual",
            Identity::Virtual {
                namespace: CanonicalText::new(format!("{common}virtual-namespace-maximum"))
                    .expect("Virtual maximum namespace"),
                key: CanonicalText::new(format!("{virtual_key_prefix}{common}maximum"))
                    .expect("Virtual maximum key"),
            },
            Identity::Virtual {
                namespace: CanonicalText::new(format!("{common}virtual-namespace-next"))
                    .expect("Virtual counterpart namespace"),
                key: CanonicalText::new(format!("{virtual_key_prefix}next"))
                    .expect("Virtual counterpart key"),
            },
            NodeKind::VirtualArtifact,
            1,
        ),
    ];

    let required_node_counts = [0usize, 1, 2, 3, 4, 127, 128, 129]
        .into_iter()
        .collect::<BTreeSet<_>>();
    let mut covered_node_counts = BTreeSet::new();
    for (case_index, (name, maximum, counterpart, counterpart_kind, assigned_nodes)) in
        cases.into_iter().enumerate()
    {
        let maximum_width = fixture_identity_total_dynamic_string_bytes(&maximum);
        let counterpart_width = fixture_identity_total_dynamic_string_bytes(&counterpart);
        assert!(
            maximum_width > counterpart_width,
            "{name}: selected endpoint must be wider than its counterpart"
        );
        assert!(
            same_arm_common_prefix_bytes(&maximum, &counterpart)
                .is_some_and(|bytes| bytes >= MIN_SAME_ARM_PREFIX_BYTES),
            "{name}: counterpart must share a long same-arm prefix"
        );
        match (&maximum, &counterpart) {
            (
                Identity::Physical { physical: maximum },
                Identity::Physical {
                    physical: counterpart,
                },
            ) => {
                let first = maximum.repository.as_str().len();
                let second = maximum.path.as_str().len();
                assert!(first > 0 && second > 0);
                assert!(
                    !counterpart.repository.as_str().is_empty()
                        && !counterpart.path.as_str().is_empty()
                );
                assert!(counterpart_width > first.max(second));
                assert_eq!(maximum_width, first + second);
                assert!(counterpart_width < maximum_width);
            }
            (
                Identity::Virtual {
                    namespace: maximum_namespace,
                    key: maximum_key,
                },
                Identity::Virtual {
                    namespace: counterpart_namespace,
                    key: counterpart_key,
                },
            ) => {
                let first = maximum_namespace.as_str().len();
                let second = maximum_key.as_str().len();
                assert!(first > 0 && second > 0);
                assert!(
                    !counterpart_namespace.as_str().is_empty()
                        && !counterpart_key.as_str().is_empty()
                );
                assert!(counterpart_width > first.max(second));
                assert_eq!(maximum_width, first + second);
                assert!(counterpart_width < maximum_width);
            }
            _ => {}
        }

        let mut node_counts = vec![assigned_nodes];
        if assigned_nodes == 0 {
            node_counts.push(1);
        }
        for nodes in node_counts {
            covered_node_counts.insert(nodes);
            let mut graph = GraphArtifact::new();
            if nodes > 0 {
                graph.exact(
                    exact_node(
                        counterpart.clone(),
                        counterpart_kind,
                        format!("{name}-counterpart"),
                        Vec::new(),
                    )
                    .expect("same-arm counterpart node"),
                );
                for index in 1..nodes {
                    graph.exact(
                        exact_node(
                            Identity::Virtual {
                                namespace: CanonicalText::new("identity-width-node")
                                    .expect("node namespace"),
                                key: CanonicalText::new(format!("{case_index}-{index:03}"))
                                    .expect("node key"),
                            },
                            NodeKind::VirtualArtifact,
                            format!("{name}-node-{index:03}"),
                            Vec::new(),
                        )
                        .expect("width fixture node"),
                    );
                }
            }
            let other_endpoint = Identity::Virtual {
                namespace: CanonicalText::new("identity-width-other-endpoint")
                    .expect("other endpoint namespace"),
                key: CanonicalText::new(format!("case-{case_index}")).expect("other endpoint key"),
            };
            graph.exact(
                xtask::architecture::schema::exact_edge(
                    maximum.clone(),
                    other_endpoint.clone(),
                    xtask::architecture::schema::EdgeKind::RustUses,
                    xtask::architecture::schema::Condition::Always,
                    SourceAnchor::Synthesized {
                        authority: CanonicalText::new("identity-width-search").expect("authority"),
                    },
                    xtask::architecture::schema::ResolverId::new(format!(
                        "identity-width-{name}-{nodes}"
                    ))
                    .expect("resolver"),
                    Vec::new(),
                )
                .expect("width fixture edge"),
            );

            let node_identities = graph
                .nodes()
                .into_iter()
                .map(|node| &node.identity)
                .collect::<Vec<_>>();
            assert!(!node_identities.contains(&&maximum));
            assert!(!node_identities.contains(&&other_endpoint));
            let maximum_occurrences = graph.truth.iter().fold(0usize, |count, truth| {
                count
                    + match truth {
                        Truth::Exact {
                            fact: Fact::Node { node },
                        } => {
                            (fixture_identity_total_dynamic_string_bytes(&node.identity)
                                == maximum_width) as usize
                        }
                        Truth::Exact {
                            fact: Fact::Edge { edge },
                        } => {
                            (fixture_identity_total_dynamic_string_bytes(&edge.record.from)
                                == maximum_width) as usize
                                + (fixture_identity_total_dynamic_string_bytes(&edge.record.to)
                                    == maximum_width) as usize
                        }
                        Truth::Unknown { .. } => 0,
                    }
            });
            assert_eq!(
                maximum_occurrences, 1,
                "{name}, N={nodes}: W maximum must be unique"
            );
            assert_eq!(
                fixture_graph_identity_total_dynamic_string_bytes(&graph),
                maximum_width,
                "{name}, N={nodes}: selected absent endpoint must determine W"
            );

            let truth = graph.truth.len();
            let edges = graph.edges().len();
            assert_eq!((truth, graph.nodes().len(), edges), (nodes + 1, nodes, 1));
            let construction = truth
                .checked_mul(4)
                .and_then(|work| nodes.checked_mul(9).and_then(|term| work.checked_add(term)))
                .and_then(|work| {
                    edges
                        .checked_mul(19)
                        .and_then(|term| work.checked_add(term))
                })
                .expect("fixture G");
            let depth = if nodes == 0 {
                0
            } else {
                nodes
                    .checked_next_power_of_two()
                    .and_then(|power| (power.ilog2() as usize).checked_add(1))
                    .expect("fixture L")
            };
            let search = edges
                .checked_mul(2)
                .and_then(|work| work.checked_mul(depth))
                .and_then(|work| {
                    maximum_width
                        .checked_mul(2)
                        .and_then(|bytes| bytes.checked_add(32))
                        .and_then(|per_comparison| work.checked_mul(per_comparison))
                })
                .expect("fixture search");
            let ledger = complete_canonical_ledger(&graph);
            assert_eq!(ledger.max_identity_width, maximum_width);
            let non_search = ledger
                .structural
                .checked_add(ledger.raw)
                .and_then(|volume| volume.checked_mul(6))
                .expect("fixture non-search comparison");
            let comparison = non_search
                .checked_add(search)
                .expect("fixture complete comparison");
            let comparison_prefix = fixture_setup_work(&graph)
                .checked_add(ledger.preflight())
                .and_then(|work| work.checked_add(construction))
                .expect("fixture comparison prefix");

            reset_validation_audit();
            let error = validate_canonical_prefix_for_test(
                &graph,
                ProjectionName::RustModule,
                comparison_prefix + comparison - 1,
                usize::MAX,
                observe_validation,
            )
            .expect_err("one below the grammar-total comparison boundary");
            assert!(
                error
                    .to_string()
                    .contains("validation-canonical-comparison-complete"),
                "{name}, N={nodes}: {error}"
            );
            assert_eq!(
                phase_counts(ValidationPhase::IdentityWidthConstruction),
                [1, nodes + 2, 0],
                "{name}, N={nodes}: production must traverse every Identity"
            );

            reset_validation_audit();
            let error = validate_canonical_prefix_for_test(
                &graph,
                ProjectionName::RustModule,
                comparison_prefix + comparison,
                usize::MAX,
                observe_validation,
            )
            .expect_err("the exact comparison boundary reaches the following canonical charge");
            assert!(
                error
                    .to_string()
                    .contains("validation-canonical-privacy-complete"),
                "{name}, N={nodes}: {error}"
            );
            assert_eq!(
                phase_counts(ValidationPhase::IdentityWidthConstruction),
                [1, nodes + 2, 0],
                "{name}, N={nodes}: exact boundary must retain the product traversal"
            );
        }
    }
    assert_eq!(covered_node_counts, required_node_counts);

    assert!(identity_two_field_width_arithmetic_for_test(usize::MAX, 1).is_err());
    assert!(identity_width_construction_arithmetic_for_test(usize::MAX, 0, 0).is_err());
    assert!(identity_width_construction_arithmetic_for_test(usize::MAX / 4, 1, 0).is_err());
    assert!(usize::MAX.checked_mul(9).is_none());
    assert!(identity_width_construction_arithmetic_for_test(0, usize::MAX, 0).is_err());
    assert!(identity_width_construction_arithmetic_for_test(0, 0, usize::MAX).is_err());
    let truth_term = (usize::MAX / 4).checked_mul(4).expect("isolated 4T term");
    let edge_term = 19usize.checked_mul(1).expect("isolated 19E term");
    assert!(truth_term.checked_add(edge_term).is_none());
    assert!(identity_width_construction_arithmetic_for_test(usize::MAX / 4, 0, 1).is_err());
    assert!(canonical_endpoint_search_arithmetic_for_test(usize::MAX, 0, 0).is_err());
    assert!(canonical_endpoint_search_arithmetic_for_test(1, 0, usize::MAX).is_err());
    let half_width = usize::MAX / 2;
    let doubled_half_width = half_width
        .checked_mul(2)
        .expect("2W must succeed before the isolated fixed addition");
    assert!(doubled_half_width.checked_add(32).is_none());
    assert!(canonical_endpoint_search_arithmetic_for_test(1, 0, half_width).is_err());
    assert!(canonical_endpoint_search_arithmetic_for_test(1, usize::MAX, 0).is_err());
    assert!(canonical_endpoint_search_arithmetic_for_test(2, usize::MAX / 2, 0).is_err());
    let wide_but_valid = (usize::MAX - 32) / 2;
    assert!(
        wide_but_valid
            .checked_mul(2)
            .and_then(|work| work.checked_add(32))
            .is_some()
    );
    assert!(canonical_endpoint_search_arithmetic_for_test(1, 1, wide_but_valid).is_err());
}

#[test]
fn canonical_prefix_has_independent_cumulative_boundaries_for_hostile_shapes() {
    const HOSTILE_STRING_BYTES: usize = 24 * 1024;

    let mut empty = GraphArtifact::new();
    empty.canonicalize().expect("empty graph");
    let singleton = canonical_fixture(&["a"], &["a".into()], &[]);
    let long_label = canonical_fixture(&["a"], &["p".repeat(HOSTILE_STRING_BYTES)], &[]);
    let parallel = canonical_fixture(
        &["a", "b"],
        &["a".into(), "b".into()],
        &["parallel-a".into(), "parallel-b".into()],
    );
    let long_edge = canonical_fixture(
        &["a", "b"],
        &["a".into(), "b".into()],
        &["r".repeat(HOSTILE_STRING_BYTES)],
    );
    let nested_percent = canonical_fixture(
        &["a"],
        &[format!("prefix-{}-suffix", "%25".repeat(8 * 1024))],
        &[],
    );
    let mut nested_order = GraphArtifact::new();
    nested_order.exact(
        exact_node(
            crate::project::identity("nested-order"),
            NodeKind::VirtualArtifact,
            "nested-order",
            vec![
                Attribute::Feature {
                    feature: CanonicalText::new("z").expect("feature"),
                },
                Attribute::Feature {
                    feature: CanonicalText::new("a").expect("feature"),
                },
            ],
        )
        .expect("nested-order node"),
    );
    nested_order.canonicalize().expect("nested-order graph");
    let shallow_comparison = canonical_fixture(
        &["aaaaaaa", "zzzzzzz"],
        &["same".into(), "same".into()],
        &[],
    );
    let deep_comparison = canonical_fixture(
        &["aaaaaab", "aaaaaac"],
        &["same".into(), "same".into()],
        &[],
    );

    let fixtures = [
        ("empty", &empty),
        ("singleton", &singleton),
        ("long-label", &long_label),
        ("parallel-edge", &parallel),
        ("long-edge-record", &long_edge),
        ("nested-percent", &nested_percent),
        ("nested-order", &nested_order),
        ("shallow-comparison", &shallow_comparison),
        ("deep-comparison", &deep_comparison),
    ];
    let totals = fixtures
        .iter()
        .map(|(name, graph)| {
            let expected = fixture_canonical_prefix(graph);
            let preflight_prefix = expected.setup + expected.preflight;
            let comparison_prefix = preflight_prefix + expected.comparison;
            let total = comparison_prefix + expected.canonical;
            assert!(total <= 8_000_000, "{name} raised the standard work cap");

            reset_validation_audit();
            CANONICAL_PREFLIGHT_TOUCHES.store(0, Ordering::Relaxed);
            CANONICAL_SOURCE_SCANS.store(0, Ordering::Relaxed);
            CANONICAL_PERCENT_DECODES.store(0, Ordering::Relaxed);
            with_json_observer_for_test(observe_canonical_json, || {
                validate_canonical_prefix_for_test(
                    graph,
                    ProjectionName::RustModule,
                    preflight_prefix - 1,
                    usize::MAX,
                    observe_validation,
                )
            })
            .expect_err("one below independent dry-preflight prefix");
            assert_eq!(
                CANONICAL_PREFLIGHT_TOUCHES.load(Ordering::Relaxed),
                0,
                "{name}"
            );
            assert_eq!(CANONICAL_SOURCE_SCANS.load(Ordering::Relaxed), 0, "{name}");
            assert_eq!(
                CANONICAL_PERCENT_DECODES.load(Ordering::Relaxed),
                0,
                "{name}"
            );
            assert_eq!(phase_counts(ValidationPhase::CanonicalPrivacy), [0, 0, 0]);

            if expected.comparison > 0 {
                reset_validation_audit();
                CANONICAL_PREFLIGHT_TOUCHES.store(0, Ordering::Relaxed);
                CANONICAL_SOURCE_SCANS.store(0, Ordering::Relaxed);
                CANONICAL_PERCENT_DECODES.store(0, Ordering::Relaxed);
                with_percent_decode_observer_for_test(observe_canonical_percent, || {
                    with_json_observer_for_test(observe_canonical_json, || {
                        validate_canonical_prefix_for_test(
                            graph,
                            ProjectionName::RustModule,
                            comparison_prefix - 1,
                            usize::MAX,
                            observe_validation,
                        )
                    })
                })
                .expect_err("one below independent comparison-sizing prefix");
                assert_eq!(
                    CANONICAL_PREFLIGHT_TOUCHES.load(Ordering::Relaxed),
                    fixture_raw_string_bytes(graph),
                    "{name}"
                );
                assert_eq!(CANONICAL_SOURCE_SCANS.load(Ordering::Relaxed), 0, "{name}");
                assert_eq!(
                    CANONICAL_PERCENT_DECODES.load(Ordering::Relaxed),
                    0,
                    "{name}"
                );
                assert_eq!(phase_counts(ValidationPhase::CanonicalPrivacy), [0, 0, 0]);
            }

            reset_validation_audit();
            CANONICAL_PREFLIGHT_TOUCHES.store(0, Ordering::Relaxed);
            CANONICAL_SOURCE_SCANS.store(0, Ordering::Relaxed);
            CANONICAL_PERCENT_DECODES.store(0, Ordering::Relaxed);
            with_percent_decode_observer_for_test(observe_canonical_percent, || {
                with_json_observer_for_test(observe_canonical_json, || {
                    validate_canonical_prefix_for_test(
                        graph,
                        ProjectionName::RustModule,
                        total - 1,
                        usize::MAX,
                        observe_validation,
                    )
                })
            })
            .expect_err("one below independent canonical/privacy phase");
            assert_eq!(
                CANONICAL_PREFLIGHT_TOUCHES.load(Ordering::Relaxed),
                fixture_raw_string_bytes(graph),
                "{name}"
            );
            assert_eq!(CANONICAL_SOURCE_SCANS.load(Ordering::Relaxed), 0, "{name}");
            assert_eq!(
                CANONICAL_PERCENT_DECODES.load(Ordering::Relaxed),
                0,
                "{name}"
            );
            assert_eq!(phase_counts(ValidationPhase::CanonicalPrivacy), [0, 0, 0]);

            reset_validation_audit();
            assert_eq!(
                validate_canonical_prefix_for_test(
                    graph,
                    ProjectionName::RustModule,
                    total,
                    usize::MAX,
                    observe_validation,
                )
                .expect("exact independent cumulative canonical boundary"),
                total,
                "{name}"
            );
            assert_eq!(phase_counts(ValidationPhase::CanonicalPrivacy), [1, 1, 1]);
            (name, expected, total)
        })
        .collect::<Vec<_>>();

    let total = |name: &str| {
        totals
            .iter()
            .find(|(fixture, _, _)| **fixture == name)
            .map(|(_, _, total)| *total)
            .expect("fixture total")
    };
    assert!(total("long-label") > total("singleton"));
    assert!(total("parallel-edge") > total("singleton"));
    assert!(total("long-edge-record") > total("parallel-edge"));
    assert!(total("nested-percent") > total("singleton"));
    assert!(total("nested-order") > total("singleton"));
    assert_eq!(
        serde_json::to_vec(&shallow_comparison)
            .expect("shallow bytes")
            .len(),
        serde_json::to_vec(&deep_comparison)
            .expect("deep bytes")
            .len()
    );
    assert_eq!(
        total("deep-comparison"),
        total("shallow-comparison"),
        "the generalized grammar bound must not depend on observed prefix depth"
    );
}

#[test]
fn canonical_phase_has_one_independent_exact_allocator_envelope() {
    const DECODED_BYTES: usize = 4_096;
    const ENCODED_BYTES: usize = DECODED_BYTES * 3;
    const VEC_MIN_NON_ZERO_CAPACITY: usize = 4;

    let labels = ["%41".repeat(DECODED_BYTES), "b".into()];
    let resolvers = ["allocator-a".into(), "allocator-b".into()];
    let graph = canonical_fixture(&["a", "b"], &labels, &resolvers);
    let expected = fixture_canonical_prefix(&graph);
    let prefix = expected.setup + expected.preflight + expected.comparison;
    let total = prefix + expected.canonical;

    reset_validation_audit();
    CANONICAL_PHASE_HEAP[0].store(0, Ordering::Relaxed);
    CANONICAL_PHASE_HEAP[1].store(0, Ordering::Relaxed);
    CANONICAL_PHASE_HEAP[2].store(0, Ordering::Relaxed);
    validate_canonical_prefix_for_test(
        &graph,
        ProjectionName::RustModule,
        total - 1,
        usize::MAX,
        observe_validation,
    )
    .expect_err("one below the complete canonical phase");
    assert_eq!(phase_counts(ValidationPhase::CanonicalPrivacy), [0, 0, 0]);
    assert_eq!(
        [
            CANONICAL_PHASE_HEAP[1].load(Ordering::Relaxed),
            CANONICAL_PHASE_HEAP[2].load(Ordering::Relaxed),
        ],
        [0, 0],
        "rejected canonical phase reported allocator activity"
    );

    // The canonical validator retains one exact-capacity node-reference Vec
    // while its exact-capacity edge-reference Vec is live. Edge validation
    // streams through fixed SHA state. The later decoder peak retains only the
    // node-reference Vec.
    let node_references = VEC_MIN_NON_ZERO_CAPACITY * std::mem::size_of::<&NodeFact>();
    let edge_references =
        VEC_MIN_NON_ZERO_CAPACITY * std::mem::size_of::<&xtask::architecture::schema::EdgeFact>();
    let edge_validation_peak = node_references + edge_references;
    let arena_nodes = ENCODED_BYTES + 2 * (ENCODED_BYTES / 3);
    let decoder_peak = arena_nodes * std::mem::size_of::<FixtureDecodeByte>()
        + 2 * ENCODED_BYTES * std::mem::size_of::<usize>()
        + 2 * ENCODED_BYTES
        + DECODED_BYTES;
    let expected_retained = 0usize;
    let expected_peak = edge_validation_peak.max(node_references + decoder_peak);

    reset_validation_audit();
    validate_canonical_prefix_for_test(
        &graph,
        ProjectionName::RustModule,
        total,
        usize::MAX,
        observe_validation,
    )
    .expect("exact canonical allocator boundary");
    assert_eq!(phase_counts(ValidationPhase::CanonicalPrivacy), [1, 1, 1]);
    assert_eq!(
        CANONICAL_PHASE_HEAP[1].load(Ordering::Relaxed) as usize,
        expected_retained
    );
    assert_eq!(
        CANONICAL_PHASE_HEAP[2].load(Ordering::Relaxed) as usize,
        expected_peak
    );
}

#[test]
fn decoder_heap_uses_max_percent_string_bytes_at_the_exact_byte_ceiling() {
    const DECODED_BYTES: usize = 4_096;
    const ENCODED_BYTES: usize = DECODED_BYTES * 3;
    const SHORT_PERCENT_BYTES: usize = 3;
    const AGGREGATE_PERCENT_BYTES: usize = ENCODED_BYTES + SHORT_PERCENT_BYTES;
    const VEC_MIN_NON_ZERO_CAPACITY: usize = 4;
    const EXACT_BYTE_CEILING: usize = 1_380_384;

    let graph = canonical_fixture(
        &["a", "b"],
        &["%41".repeat(DECODED_BYTES), "%42".into()],
        &[],
    );
    let (percent_bytes, max_percent_string_bytes) = fixture_percent_string_metrics(&graph);
    assert_eq!(percent_bytes, AGGREGATE_PERCENT_BYTES);
    assert_eq!(max_percent_string_bytes, ENCODED_BYTES);
    assert!(percent_bytes > max_percent_string_bytes);

    let ledger = complete_canonical_ledger(&graph);
    let aggregate_decoder_work = fixture_percent_decode_work_bound(percent_bytes);
    let max_only_decoder_work = fixture_percent_decode_work_bound(max_percent_string_bytes);
    assert_eq!(ledger.percent, percent_bytes);
    assert_eq!(ledger.decoder, aggregate_decoder_work);
    assert!(aggregate_decoder_work > max_only_decoder_work);
    let exact_work_ceiling = fixture_setup_work(&graph)
        .checked_add(ledger.preflight())
        .and_then(|work| work.checked_add(ledger.comparison()))
        .and_then(|work| work.checked_add(ledger.canonical()))
        .expect("independent aggregate-P work boundary");

    reset_validation_audit();
    CANONICAL_PERCENT_DECODES.store(0, Ordering::Relaxed);
    let error = with_percent_decode_observer_for_test(observe_canonical_percent, || {
        validate_canonical_prefix_for_test(
            &graph,
            ProjectionName::RustModule,
            exact_work_ceiling - 1,
            usize::MAX,
            observe_validation,
        )
    })
    .expect_err("one below aggregate-P work must reject before canonical/privacy");
    assert!(
        error
            .to_string()
            .contains("validation-canonical-privacy-complete"),
        "{error}"
    );
    assert_eq!(phase_counts(ValidationPhase::CanonicalPrivacy), [0, 0, 0]);
    assert_eq!(CANONICAL_PERCENT_DECODES.load(Ordering::Relaxed), 0);

    reset_validation_audit();
    CANONICAL_PERCENT_DECODES.store(0, Ordering::Relaxed);
    with_percent_decode_observer_for_test(observe_canonical_percent, || {
        validate_canonical_prefix_for_test(
            &graph,
            ProjectionName::RustModule,
            exact_work_ceiling,
            usize::MAX,
            observe_validation,
        )
    })
    .expect("the exact aggregate-P work boundary must admit");
    assert_eq!(phase_counts(ValidationPhase::CanonicalPrivacy), [1, 1, 1]);
    assert!(CANONICAL_PERCENT_DECODES.load(Ordering::Relaxed) > 0);

    let node_references = VEC_MIN_NON_ZERO_CAPACITY * std::mem::size_of::<&NodeFact>();
    let decoder_reservation = fixture_percent_decode_allocation_bound(max_percent_string_bytes);
    let exact_byte_ceiling = node_references
        .checked_add(decoder_reservation)
        .expect("independent decoder heap boundary");
    assert_eq!(decoder_reservation, 1_380_352);
    assert_eq!(exact_byte_ceiling, EXACT_BYTE_CEILING);

    reset_validation_audit();
    CANONICAL_PERCENT_DECODES.store(0, Ordering::Relaxed);
    CANONICAL_PHASE_HEAP[1].store(0, Ordering::Relaxed);
    CANONICAL_PHASE_HEAP[2].store(0, Ordering::Relaxed);
    with_percent_decode_observer_for_test(observe_canonical_percent, || {
        validate_canonical_prefix_for_test(
            &graph,
            ProjectionName::RustModule,
            usize::MAX,
            exact_byte_ceiling - 1,
            observe_validation,
        )
    })
    .expect_err("one below the exact decoder reservation must reject before the phase");
    assert_eq!(phase_counts(ValidationPhase::CanonicalPrivacy), [0, 0, 0]);
    assert_eq!(CANONICAL_PERCENT_DECODES.load(Ordering::Relaxed), 0);
    assert_eq!(CANONICAL_PHASE_HEAP[1].load(Ordering::Relaxed), 0);
    assert_eq!(CANONICAL_PHASE_HEAP[2].load(Ordering::Relaxed), 0);

    reset_validation_audit();
    CANONICAL_PERCENT_DECODES.store(0, Ordering::Relaxed);
    CANONICAL_PHASE_HEAP[1].store(0, Ordering::Relaxed);
    CANONICAL_PHASE_HEAP[2].store(0, Ordering::Relaxed);
    with_percent_decode_observer_for_test(observe_canonical_percent, || {
        validate_canonical_prefix_for_test(
            &graph,
            ProjectionName::RustModule,
            usize::MAX,
            exact_byte_ceiling,
            observe_validation,
        )
    })
    .expect("the exact decoder reservation must admit");
    assert_eq!(phase_counts(ValidationPhase::CanonicalPrivacy), [1, 1, 1]);
    assert!(CANONICAL_PERCENT_DECODES.load(Ordering::Relaxed) > 0);
    assert_eq!(CANONICAL_PHASE_HEAP[1].load(Ordering::Relaxed), 0);
    assert!(CANONICAL_PHASE_HEAP[2].load(Ordering::Relaxed) as usize <= exact_byte_ceiling);
}

#[test]
fn canonical_phase_byte_admission_conservatively_covers_the_actual_reference_peak() {
    const LONG_RESOLVER_BYTES: usize = 32 * 1024;
    const VEC_CAPACITY_AFTER_FIVE_PUSHES: usize = 8;

    let labels = ["a".into(), "b".into(), "c".into(), "d".into(), "e".into()];
    let resolvers = [
        "a".repeat(LONG_RESOLVER_BYTES),
        "b".into(),
        "c".into(),
        "d".into(),
        "e".into(),
    ];
    let graph = canonical_fixture(&["a", "b", "c", "d", "e"], &labels, &resolvers);
    let expected = fixture_canonical_prefix(&graph);
    let total = expected.setup + expected.preflight + expected.comparison + expected.canonical;

    let node_references = VEC_CAPACITY_AFTER_FIVE_PUSHES * std::mem::size_of::<&NodeFact>();
    let edge_references = VEC_CAPACITY_AFTER_FIVE_PUSHES
        * std::mem::size_of::<&xtask::architecture::schema::EdgeFact>();
    let non_decoder_peak = node_references + edge_references;
    let max_percent_string_bytes = fixture_percent_string_metrics(&graph).1;
    let decoder_reservation =
        node_references + fixture_percent_decode_allocation_bound(max_percent_string_bytes);
    let admitted_peak = non_decoder_peak.max(decoder_reservation);
    reset_validation_audit();
    CANONICAL_SOURCE_SCANS.store(0, Ordering::Relaxed);
    CANONICAL_PERCENT_DECODES.store(0, Ordering::Relaxed);
    validate_canonical_prefix_for_test(
        &graph,
        ProjectionName::RustModule,
        total,
        admitted_peak - 1,
        observe_validation,
    )
    .expect_err("one below exact canonical phase storage");
    assert_eq!(phase_counts(ValidationPhase::CanonicalPrivacy), [0, 0, 0]);
    assert_eq!(CANONICAL_SOURCE_SCANS.load(Ordering::Relaxed), 0);
    assert_eq!(CANONICAL_PERCENT_DECODES.load(Ordering::Relaxed), 0);

    reset_validation_audit();
    validate_canonical_prefix_for_test(
        &graph,
        ProjectionName::RustModule,
        total,
        admitted_peak,
        observe_validation,
    )
    .expect("exact canonical phase storage");
    assert_eq!(phase_counts(ValidationPhase::CanonicalPrivacy), [1, 1, 1]);
    assert_eq!(CANONICAL_PHASE_HEAP[1].load(Ordering::Relaxed), 0);
    assert_eq!(
        CANONICAL_PHASE_HEAP[2].load(Ordering::Relaxed) as usize,
        non_decoder_peak
    );
}

#[test]
fn canonical_error_paths_are_rejected_or_admitted_before_the_phase() {
    let mut empty = GraphArtifact::new();
    empty.canonicalize().expect("empty graph");
    let empty_expected = fixture_canonical_prefix(&empty);
    let empty_total = empty_expected.setup
        + empty_expected.preflight
        + empty_expected.comparison
        + empty_expected.canonical;
    let empty_peak =
        fixture_percent_decode_allocation_bound(fixture_percent_string_metrics(&empty).1);
    reset_validation_audit();
    validate_canonical_prefix_for_test(
        &empty,
        ProjectionName::RustModule,
        empty_total,
        empty_peak,
        observe_validation,
    )
    .expect("valid empty canonical phase has an exact conservative reservation");
    assert_eq!(phase_counts(ValidationPhase::CanonicalPrivacy), [1, 1, 1]);
    assert_eq!(
        [
            CANONICAL_PHASE_HEAP[1].load(Ordering::Relaxed),
            CANONICAL_PHASE_HEAP[2].load(Ordering::Relaxed),
        ],
        [0, 0]
    );

    let mut wrong_schema = empty.clone();
    wrong_schema.schema = CanonicalText::new("wrong-schema").expect("wrong schema text");
    let wrong_expected = fixture_canonical_prefix(&wrong_schema);
    let wrong_total = wrong_expected.setup
        + wrong_expected.preflight
        + wrong_expected.comparison
        + wrong_expected.canonical;
    reset_validation_audit();
    let error = validate_canonical_prefix_for_test(
        &wrong_schema,
        ProjectionName::RustModule,
        wrong_total,
        usize::MAX,
        observe_validation,
    )
    .expect_err("wrong schema");
    assert!(error.to_string().contains("GRAPH_SCHEMA_MISMATCH"));
    assert_eq!(
        phase_counts(ValidationPhase::CanonicalPrivacy),
        [0, 0, 0],
        "static schema failure entered the credited canonical phase"
    );

    let long_key = "k".repeat(32 * 1024);
    let mut invalid_kind = GraphArtifact::new();
    let Fact::Node { mut node } = exact_node(
        crate::project::identity(&long_key),
        NodeKind::VirtualArtifact,
        "invalid-kind",
        Vec::new(),
    )
    .expect("long-key node") else {
        unreachable!("node constructor")
    };
    node.kind = NodeKind::Product;
    invalid_kind.exact(Fact::Node { node });
    let invalid_expected = fixture_canonical_prefix(&invalid_kind);
    let invalid_total = invalid_expected.setup
        + invalid_expected.preflight
        + invalid_expected.comparison
        + invalid_expected.canonical;
    reset_validation_audit();
    let error = validate_canonical_prefix_for_test(
        &invalid_kind,
        ProjectionName::RustModule,
        invalid_total,
        usize::MAX,
        observe_validation,
    )
    .expect_err("admitted invalid node kind");
    assert!(error.to_string().contains("NODE_IDENTITY_KIND_MISMATCH"));
    assert_eq!(phase_counts(ValidationPhase::CanonicalPrivacy), [1, 1, 0]);
}

#[test]
fn reachable_canonical_failures_return_allocation_free_markers_at_one_below_and_exact() {
    fn total(graph: &GraphArtifact) -> usize {
        let expected = fixture_canonical_prefix(graph);
        expected.setup + expected.preflight + expected.comparison + expected.canonical
    }

    let long_key = "k".repeat(32 * 1024);
    let mut invalid_kind = GraphArtifact::new();
    let Fact::Node { mut node } = exact_node(
        crate::project::identity(&long_key),
        NodeKind::VirtualArtifact,
        "invalid-kind",
        Vec::new(),
    )
    .expect("node-kind fixture") else {
        unreachable!("node constructor")
    };
    node.kind = NodeKind::Product;
    invalid_kind.exact(Fact::Node { node });

    let mut conflicting = canonical_fixture(&["conflict"], &["a".into()], &[]);
    let mut second = conflicting.nodes()[0].clone();
    second.label = CanonicalText::new("b").expect("conflicting label");
    conflicting.exact(Fact::Node { node: second });
    conflicting.truth.sort();

    let missing_graph = canonical_fixture(
        &["source", "target"],
        &["source".into(), "target".into()],
        &["missing-endpoint".into()],
    );
    let mut missing_source = missing_graph.clone();
    let source = crate::project::identity("source");
    missing_source.truth.retain(|truth| {
        !matches!(truth, Truth::Exact { fact: Fact::Node { node } } if node.identity == source)
    });
    let mut missing_target = missing_graph;
    let target = crate::project::identity("target");
    missing_target.truth.retain(|truth| {
        !matches!(truth, Truth::Exact { fact: Fact::Node { node } } if node.identity == target)
    });

    let mut nested_order = GraphArtifact::new();
    nested_order.exact(
        exact_node(
            crate::project::identity("nested"),
            NodeKind::VirtualArtifact,
            "nested",
            vec![
                Attribute::Feature {
                    feature: CanonicalText::new("a").expect("feature"),
                },
                Attribute::Feature {
                    feature: CanonicalText::new("z").expect("feature"),
                },
            ],
        )
        .expect("nested fixture"),
    );
    nested_order
        .canonicalize()
        .expect("canonical nested fixture");
    let Truth::Exact {
        fact: Fact::Node { node },
    } = &mut nested_order.truth[0]
    else {
        unreachable!("nested node")
    };
    node.attributes.reverse();

    let mut edge_binding =
        canonical_fixture(&["a", "b"], &["a".into(), "b".into()], &["bound".into()]);
    let edge = edge_binding
        .truth
        .iter_mut()
        .find_map(|truth| match truth {
            Truth::Exact {
                fact: Fact::Edge { edge },
            } => Some(edge),
            _ => None,
        })
        .expect("edge fixture");
    edge.record.resolver =
        xtask::architecture::schema::ResolverId::new("forged").expect("resolver");
    edge_binding.truth.sort();

    let cases = [
        (invalid_kind, "NODE_IDENTITY_KIND_MISMATCH"),
        (conflicting, "conflicting typed node identity"),
        (missing_source, "missing source"),
        (missing_target, "missing target"),
        (nested_order, "GRAPH_NONCANONICAL"),
        (edge_binding, "edge identity does not bind"),
    ];
    let mut one_below_heap = [0u64; 6];
    let mut one_below_peak = [0u64; 6];
    let mut exact_marker_heap = [0u64; 6];
    let mut one_below_touches = [0usize; 6];
    let mut one_below_scans = [0usize; 6];
    for (index, (graph, expected)) in cases.iter().enumerate() {
        let complete = total(graph);
        CANONICAL_PREFLIGHT_TOUCHES.store(0, Ordering::Relaxed);
        CANONICAL_SOURCE_SCANS.store(0, Ordering::Relaxed);
        let (baseline, _) = crate::take_heap_usage();
        let error = with_json_observer_for_test(observe_canonical_json, || {
            validate_canonical_prefix_for_test(
                graph,
                ProjectionName::RustModule,
                complete - 1,
                usize::MAX,
                observe_validation,
            )
        })
        .expect_err("one below complete canonical charge");
        let (active, peak) = crate::take_heap_usage();
        one_below_heap[index] = active - baseline;
        one_below_peak[index] = peak - baseline;
        one_below_touches[index] = CANONICAL_PREFLIGHT_TOUCHES.load(Ordering::Relaxed);
        one_below_scans[index] = CANONICAL_SOURCE_SCANS.load(Ordering::Relaxed);
        let _ = error;

        let (baseline, _) = crate::take_heap_usage();
        let error = validate_canonical_prefix_for_test(
            graph,
            ProjectionName::RustModule,
            complete,
            usize::MAX,
            observe_validation,
        )
        .expect_err("exact canonical charge reaches the typed failure");
        exact_marker_heap[index] = crate::take_heap_usage().0 - baseline;
        assert!(error.to_string().contains(expected), "{error:#}");
        let _ = error;
    }
    assert_eq!(
        exact_marker_heap, [0; 6],
        "exact canonical failure markers allocated"
    );
    assert_eq!(one_below_heap, [0; 6], "one-below markers allocated");
    assert_eq!(
        one_below_touches,
        std::array::from_fn(|index| fixture_raw_string_bytes(&cases[index].0)),
        "one-below C did not perform exactly the P-admitted dry preflight"
    );
    assert_eq!(
        one_below_scans, [0; 6],
        "one-below charge streamed canonical payload bytes"
    );
    assert_eq!(
        one_below_peak, [0; 6],
        "one-below markers had transient allocations"
    );
}

#[test]
fn size_serialized_ceiling_and_privacy_failures_return_allocation_free_markers() {
    let singleton = canonical_fixture(&["size"], &["size".into()], &[]);
    let serialized_limit = serde_json::to_vec(&singleton)
        .expect("singleton bytes")
        .len()
        - 1;
    let mut privacy = singleton.clone();
    let Truth::Exact {
        fact: Fact::Node { node },
    } = &mut privacy.truth[0]
    else {
        unreachable!("privacy node")
    };
    node.label = unchecked_canonical_text_for_test("%2FUsers%2Fprivate".into());

    let mut retained = [0u64; 3];
    let (baseline, _) = crate::take_heap_usage();
    let error = validate_canonical_limits_for_test(&singleton, 0, usize::MAX)
        .expect_err("truth-row ceiling");
    retained[0] = crate::take_heap_usage().0 - baseline;
    let _ = error;

    let (baseline, _) = crate::take_heap_usage();
    let error = validate_canonical_limits_for_test(&singleton, usize::MAX, serialized_limit)
        .expect_err("serialized ceiling");
    retained[1] = crate::take_heap_usage().0 - baseline;
    let _ = error;

    let (baseline, _) = crate::take_heap_usage();
    let error = validate_canonical_limits_for_test(&privacy, usize::MAX, usize::MAX)
        .expect_err("privacy validation");
    retained[2] = crate::take_heap_usage().0 - baseline;
    let _ = error;

    assert_eq!(
        retained, [0; 3],
        "size/serialized/privacy markers allocated"
    );
}

#[test]
fn graph_consumers_require_exact_nonmutating_canonical_truth() {
    let graph = crate::project::graph("canonical-input", true, true);
    graph.validate_canonical().expect("canonical baseline");

    let mut reversed = graph.clone();
    reversed.truth.reverse();
    assert_graph_error(&reversed, "GRAPH_NONCANONICAL");

    let mut duplicate = graph.clone();
    duplicate.truth.push(duplicate.truth[0].clone());
    duplicate.truth.sort();
    assert_graph_error(&duplicate, "GRAPH_NONCANONICAL");

    let mut conflicting = graph.clone();
    let node = (*conflicting.nodes().first().expect("node")).clone();
    let mut conflict = node.clone();
    conflict.label = CanonicalText::new("conflicting-label").expect("label");
    conflicting.truth.push(Truth::Exact {
        fact: Fact::Node { node: conflict },
    });
    conflicting.truth.sort();
    assert_graph_error(&conflicting, "conflicting typed node identity");

    let mut wrong_schema = graph.clone();
    wrong_schema.schema = CanonicalText::new("bbnf.architecture.graph.forged").expect("schema");
    assert_graph_error(&wrong_schema, "GRAPH_SCHEMA_MISMATCH");

    let mut forged_edge = graph.clone();
    let Truth::Exact {
        fact: Fact::Edge { edge },
    } = forged_edge
        .truth
        .iter_mut()
        .find(|truth| {
            matches!(
                truth,
                Truth::Exact {
                    fact: Fact::Edge { .. }
                }
            )
        })
        .expect("edge")
    else {
        unreachable!("edge match")
    };
    edge.record.resolver =
        xtask::architecture::schema::ResolverId::new("forged").expect("forged resolver");
    forged_edge.truth.sort();
    assert_graph_error(&forged_edge, "edge identity does not bind");

    let projections = project(&graph).expect("projections");
    let error =
        xtask::architecture::evidence::validate::validate_structural(&reversed, &projections)
            .expect_err("structural validation must reject noncanonical graph input");
    assert!(
        error.to_string().contains("GRAPH_NONCANONICAL"),
        "{error:#}"
    );
}

#[test]
fn graph_and_projection_json_reject_unknown_and_escaped_fields_recursively() {
    let graph = crate::project::graph("raw-json", true, true);
    let mut raw = serde_json::to_value(&graph).expect("graph JSON");
    raw.as_object_mut()
        .expect("graph object")
        .insert("unknown".into(), serde_json::json!(true));
    assert!(serde_json::from_value::<xtask::architecture::schema::GraphArtifact>(raw).is_err());

    let mut raw = serde_json::to_value(&graph).expect("graph JSON");
    let node = raw["truth"]
        .as_array_mut()
        .expect("truth")
        .iter_mut()
        .find(|truth| {
            truth
                .get("fact")
                .and_then(|fact| fact.get("node"))
                .is_some()
        })
        .expect("node truth");
    node["fact"]["node"]
        .as_object_mut()
        .expect("node object")
        .insert("unknown".into(), serde_json::json!(true));
    assert!(serde_json::from_value::<xtask::architecture::schema::GraphArtifact>(raw).is_err());

    let mut raw = serde_json::to_value(&graph).expect("graph JSON");
    let node = raw["truth"]
        .as_array_mut()
        .expect("truth")
        .iter_mut()
        .find(|truth| {
            truth
                .get("fact")
                .and_then(|fact| fact.get("node"))
                .is_some()
        })
        .expect("node truth");
    node["fact"]["node"]["label"] = serde_json::json!("%2FUsers%2Fleak");
    assert!(serde_json::from_value::<xtask::architecture::schema::GraphArtifact>(raw).is_err());

    let projections = project(&graph).expect("projections");
    let mut raw = serde_json::to_value(&projections).expect("projection JSON");
    raw["projections"][0]["clusters"]
        .as_object_mut()
        .expect("cluster set")
        .insert("unknown".into(), serde_json::json!(true));
    assert!(
        serde_json::from_value::<xtask::architecture::project::ProjectionSet>(raw).is_err(),
        "nested projection wire structs must reject unknown fields"
    );
}

#[test]
fn validator_rebuilds_exact_cluster_set_and_order() {
    let graph = crate::project::signal_graph(None, false, false);
    let projections = project(&graph).expect("projections");
    let mut changed = projections.clone();
    let projection = rust_projection_mut(&mut changed);
    let cluster = projection
        .clusters
        .clusters
        .first_mut()
        .expect("explicit cluster");
    cluster.signals.push(ClusterSignal::Feature);
    cluster.signals.sort();
    cluster.signals.dedup();
    rehash_cluster(cluster);
    rehash_projection(projection);
    assert_error(&graph, &changed, "CLUSTER_CUT_TOTALITY_MISMATCH");

    let mut changed = projections;
    let projection = rust_projection_mut(&mut changed);
    assert!(projection.clusters.clusters.len() > 1, "ordering fixture");
    projection.clusters.clusters.reverse();
    rehash_projection(projection);
    assert_error(&graph, &changed, "CLUSTER_CUT_TOTALITY_MISMATCH");
}

#[test]
fn validator_reconstructs_failures_constants_structural_bit_and_hashes() {
    let mut graph = crate::project::graph("validate-metadata", true, true);
    graph.unknown(
        Obligation::required(
            ObligationCode::RustSemanticsUnproven,
            ObligationSubject::new("validate-metadata").expect("subject"),
            vec![ProjectionName::RustModule],
            PipelineStage::Rust,
            SourceAnchor::Synthesized {
                authority: CanonicalText::new("validate-metadata").expect("authority"),
            },
            CanonicalText::new("required failure").expect("reason"),
        )
        .expect("obligation"),
    );
    graph.canonicalize().expect("graph");
    let projections = project(&graph).expect("projections");
    xtask::architecture::evidence::validate::validate_structural(&graph, &projections)
        .expect("baseline");

    let mut changed = projections.clone();
    changed.schema = CanonicalText::new("bbnf.architecture.projections.forged").expect("schema");
    assert_error(&graph, &changed, "projection-set schema differs");

    let mut changed = projections.clone();
    rust_projection_mut(&mut changed).schema =
        CanonicalText::new("bbnf.architecture.projection.forged").expect("schema");
    assert_error(&graph, &changed, "projection schema constants differ");

    let mut changed = projections.clone();
    rust_projection_mut(&mut changed).implicit_singletons =
        CanonicalText::new("forged implicit singleton rule").expect("implicit singleton");
    assert_error(&graph, &changed, "projection schema constants differ");

    let mut changed = projections.clone();
    rust_projection_mut(&mut changed).clusters.schema =
        CanonicalText::new("bbnf.architecture.clusters.forged").expect("schema");
    assert_error(&graph, &changed, "projection schema constants differ");

    let mut changed = projections.clone();
    rust_projection_mut(&mut changed).quotient.schema =
        CanonicalText::new("bbnf.architecture.quotient.forged").expect("schema");
    assert_error(&graph, &changed, "projection schema constants differ");

    let mut changed = projections.clone();
    rust_projection_mut(&mut changed).required_failures.clear();
    assert_error(&graph, &changed, "PROJECTION_REQUIRED_FAILURE_MISMATCH");

    let mut changed = projections.clone();
    rust_projection_mut(&mut changed).structurally_valid_for_selected_edges = false;
    assert_error(&graph, &changed, "PROJECTION_STRUCTURAL_VALIDITY_MISMATCH");

    let mut changed = projections;
    rust_projection_mut(&mut changed).hash = Sha256Digest::new("0".repeat(64)).expect("hash");
    assert_error(&graph, &changed, "projection hash omits");
}

#[test]
fn validator_rejects_missing_scc_and_every_wrong_cut_role() {
    let graph = crate::project::signal_graph(None, false, false);
    let projections = project(&graph).expect("projections");

    let mut missing = projections.clone();
    rust_projection_mut(&mut missing).components.pop();
    assert_error(&graph, &missing, "SCC_TOTALITY_MISMATCH");

    for role in [
        xtask::architecture::project::cluster::CutRole::InternalOwner,
        xtask::architecture::project::cluster::CutRole::OutgoingOwner,
        xtask::architecture::project::cluster::CutRole::IncomingReference,
    ] {
        let mut changed = projections.clone();
        let cluster = rust_projection_mut(&mut changed)
            .clusters
            .clusters
            .first_mut()
            .expect("cluster");
        let original = cluster.edge_records[0].role;
        cluster.edge_records[0].role = if role == original {
            match original {
                xtask::architecture::project::cluster::CutRole::InternalOwner => {
                    xtask::architecture::project::cluster::CutRole::OutgoingOwner
                }
                xtask::architecture::project::cluster::CutRole::OutgoingOwner => {
                    xtask::architecture::project::cluster::CutRole::IncomingReference
                }
                xtask::architecture::project::cluster::CutRole::IncomingReference => {
                    xtask::architecture::project::cluster::CutRole::InternalOwner
                }
            }
        } else {
            role
        };
        assert_error(&graph, &changed, "CLUSTER_CUT_TOTALITY_MISMATCH");
    }
}

#[test]
fn validator_rejects_every_parallel_typed_edge_record_mutation_after_rehash() {
    let graph = crate::project::multigraph(false);
    let projections = project(&graph).expect("projections");
    let from = crate::project::identity("a");
    let to = crate::project::identity("b");
    let parallel = graph
        .edges()
        .into_iter()
        .filter(|edge| edge.record.from == from && edge.record.to == to)
        .map(|edge| edge.identity.clone())
        .collect::<Vec<_>>();
    assert_eq!(parallel.len(), 2, "two typed edges share exact endpoints");
    for mutation in ["remove", "duplicate", "substitute", "reorder", "role"] {
        let mut changed = projections.clone();
        let projection = rust_projection_mut(&mut changed);
        let cluster_index = projection
            .clusters
            .clusters
            .iter()
            .position(|cluster| {
                parallel.iter().all(|edge| {
                    cluster
                        .edge_records
                        .iter()
                        .any(|record| &record.edge == edge)
                })
            })
            .expect("parallel typed-edge record fixture");
        let substitute = projection.clusters.clusters[cluster_index]
            .edge_records
            .iter()
            .map(|record| &record.edge)
            .find(|edge| !parallel.contains(edge))
            .expect("nonparallel substitute")
            .clone();
        let cluster = &mut projection.clusters.clusters[cluster_index];
        let first = cluster
            .edge_records
            .iter()
            .position(|record| record.edge == parallel[0])
            .expect("first parallel record");
        let second = cluster
            .edge_records
            .iter()
            .position(|record| record.edge == parallel[1])
            .expect("second parallel record");
        match mutation {
            "remove" => {
                cluster.edge_records.remove(first);
            }
            "duplicate" => cluster
                .edge_records
                .push(cluster.edge_records[first].clone()),
            "substitute" => cluster.edge_records[first].edge = substitute,
            "reorder" => cluster.edge_records.swap(first, second),
            "role" => {
                cluster.edge_records[first].role =
                    xtask::architecture::project::cluster::CutRole::IncomingReference
            }
            _ => unreachable!(),
        }
        rehash_cluster(cluster);
        rehash_projection(projection);
        assert_error(&graph, &changed, "CLUSTER_CUT_TOTALITY_MISMATCH");
    }
}

struct FixtureJsonWork {
    work: usize,
    source_passes: usize,
    output_passes: usize,
    map_key: bool,
    raw_string_bytes: usize,
    percent_string_bytes: usize,
    max_percent_string_bytes: usize,
    traversal_work: usize,
}

impl FixtureJsonWork {
    fn add(&mut self, amount: usize) -> Result<(), serde_json::Error> {
        self.work = self
            .work
            .checked_add(amount)
            .ok_or_else(|| serde::ser::Error::custom("fixture JSON work overflow"))?;
        Ok(())
    }

    fn record_traversal(&mut self) -> Result<(), serde_json::Error> {
        self.traversal_work = self
            .traversal_work
            .checked_add(1)
            .ok_or_else(|| serde::ser::Error::custom("fixture traversal work overflow"))?;
        Ok(())
    }

    fn visit(&mut self, output_bytes: usize) -> Result<(), serde_json::Error> {
        let width = output_bytes
            .checked_add(usize::from(self.map_key) * 2)
            .ok_or_else(|| serde::ser::Error::custom("fixture JSON width overflow"))?;
        self.add(
            width
                .checked_mul(self.output_passes)
                .and_then(|work| work.checked_add(32))
                .ok_or_else(|| serde::ser::Error::custom("fixture JSON scalar overflow"))?,
        )?;
        self.record_traversal()
    }

    fn container(&mut self, length: Option<usize>) -> Result<(), serde_json::Error> {
        let length = length.unwrap_or(0);
        self.add(
            length
                .checked_add(1)
                .and_then(|visits| visits.checked_mul(32))
                .ok_or_else(|| serde::ser::Error::custom("fixture JSON container overflow"))?,
        )?;
        self.record_traversal()
    }

    fn string(&mut self, value: &str, dynamic: bool) -> Result<(), serde_json::Error> {
        self.raw_string_bytes = self
            .raw_string_bytes
            .checked_add(value.len())
            .ok_or_else(|| serde::ser::Error::custom("fixture raw string overflow"))?;
        if dynamic && value.as_bytes().contains(&b'%') {
            self.percent_string_bytes = self
                .percent_string_bytes
                .checked_add(value.len())
                .ok_or_else(|| serde::ser::Error::custom("fixture percent string overflow"))?;
            self.max_percent_string_bytes = self.max_percent_string_bytes.max(value.len());
        }
        let output = value
            .len()
            .checked_mul(6)
            .and_then(|bytes| bytes.checked_add(2))
            .ok_or_else(|| serde::ser::Error::custom("fixture JSON string width overflow"))?;
        let source = value
            .len()
            .checked_mul(self.source_passes)
            .ok_or_else(|| serde::ser::Error::custom("fixture JSON string source overflow"))?;
        self.add(
            output
                .checked_mul(self.output_passes)
                .and_then(|bytes| bytes.checked_add(source))
                .and_then(|bytes| bytes.checked_add(32))
                .ok_or_else(|| serde::ser::Error::custom("fixture JSON string work overflow"))?,
        )?;
        self.record_traversal()
    }
}

struct FixtureJsonCompound<'a> {
    serializer: &'a mut FixtureJsonWork,
}

impl SerializeSeq for FixtureJsonCompound<'_> {
    type Ok = ();
    type Error = serde_json::Error;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.serializer.record_traversal()?;
        value.serialize(&mut *self.serializer)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.serializer.record_traversal()?;
        Ok(())
    }
}

impl SerializeTuple for FixtureJsonCompound<'_> {
    type Ok = ();
    type Error = serde_json::Error;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.serializer.record_traversal()?;
        value.serialize(&mut *self.serializer)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.serializer.record_traversal()?;
        Ok(())
    }
}

impl SerializeTupleStruct for FixtureJsonCompound<'_> {
    type Ok = ();
    type Error = serde_json::Error;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.serializer.record_traversal()?;
        value.serialize(&mut *self.serializer)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.serializer.record_traversal()?;
        Ok(())
    }
}

impl SerializeTupleVariant for FixtureJsonCompound<'_> {
    type Ok = ();
    type Error = serde_json::Error;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.serializer.record_traversal()?;
        value.serialize(&mut *self.serializer)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.serializer.record_traversal()?;
        Ok(())
    }
}

impl SerializeMap for FixtureJsonCompound<'_> {
    type Ok = ();
    type Error = serde_json::Error;

    fn serialize_key<T: Serialize + ?Sized>(&mut self, key: &T) -> Result<(), Self::Error> {
        self.serializer.record_traversal()?;
        self.serializer.map_key = true;
        let result = key.serialize(&mut *self.serializer);
        self.serializer.map_key = false;
        result
    }

    fn serialize_value<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.serializer.record_traversal()?;
        value.serialize(&mut *self.serializer)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.serializer.record_traversal()?;
        Ok(())
    }
}

impl SerializeStruct for FixtureJsonCompound<'_> {
    type Ok = ();
    type Error = serde_json::Error;

    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        self.serializer.record_traversal()?;
        self.serializer.string(key, false)?;
        value.serialize(&mut *self.serializer)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.serializer.record_traversal()?;
        Ok(())
    }
}

impl SerializeStructVariant for FixtureJsonCompound<'_> {
    type Ok = ();
    type Error = serde_json::Error;

    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        self.serializer.record_traversal()?;
        self.serializer.string(key, false)?;
        value.serialize(&mut *self.serializer)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.serializer.record_traversal()?;
        Ok(())
    }
}

impl<'a> Serializer for &'a mut FixtureJsonWork {
    type Ok = ();
    type Error = serde_json::Error;
    type SerializeSeq = FixtureJsonCompound<'a>;
    type SerializeTuple = FixtureJsonCompound<'a>;
    type SerializeTupleStruct = FixtureJsonCompound<'a>;
    type SerializeTupleVariant = FixtureJsonCompound<'a>;
    type SerializeMap = FixtureJsonCompound<'a>;
    type SerializeStruct = FixtureJsonCompound<'a>;
    type SerializeStructVariant = FixtureJsonCompound<'a>;

    fn serialize_bool(self, value: bool) -> Result<Self::Ok, Self::Error> {
        self.visit(if value { 4 } else { 5 })
    }

    fn serialize_i8(self, value: i8) -> Result<Self::Ok, Self::Error> {
        self.visit(value.to_string().len())
    }

    fn serialize_i16(self, value: i16) -> Result<Self::Ok, Self::Error> {
        self.visit(value.to_string().len())
    }

    fn serialize_i32(self, value: i32) -> Result<Self::Ok, Self::Error> {
        self.visit(value.to_string().len())
    }

    fn serialize_i64(self, value: i64) -> Result<Self::Ok, Self::Error> {
        self.visit(value.to_string().len())
    }

    fn serialize_i128(self, value: i128) -> Result<Self::Ok, Self::Error> {
        self.visit(value.to_string().len())
    }

    fn serialize_u8(self, value: u8) -> Result<Self::Ok, Self::Error> {
        self.visit(value.to_string().len())
    }

    fn serialize_u16(self, value: u16) -> Result<Self::Ok, Self::Error> {
        self.visit(value.to_string().len())
    }

    fn serialize_u32(self, value: u32) -> Result<Self::Ok, Self::Error> {
        self.visit(value.to_string().len())
    }

    fn serialize_u64(self, value: u64) -> Result<Self::Ok, Self::Error> {
        self.visit(value.to_string().len())
    }

    fn serialize_u128(self, value: u128) -> Result<Self::Ok, Self::Error> {
        self.visit(value.to_string().len())
    }

    fn serialize_f32(self, value: f32) -> Result<Self::Ok, Self::Error> {
        self.visit(serde_json::to_string(&value)?.len())
    }

    fn serialize_f64(self, value: f64) -> Result<Self::Ok, Self::Error> {
        self.visit(serde_json::to_string(&value)?.len())
    }

    fn serialize_char(self, value: char) -> Result<Self::Ok, Self::Error> {
        let mut buffer = [0u8; 4];
        self.string(value.encode_utf8(&mut buffer), true)
    }

    fn serialize_str(self, value: &str) -> Result<Self::Ok, Self::Error> {
        self.string(value, true)
    }

    fn serialize_bytes(self, value: &[u8]) -> Result<Self::Ok, Self::Error> {
        self.raw_string_bytes = self
            .raw_string_bytes
            .checked_add(value.len())
            .ok_or_else(|| serde::ser::Error::custom("fixture raw byte overflow"))?;
        let output = value
            .len()
            .checked_mul(4)
            .and_then(|bytes| bytes.checked_add(2))
            .and_then(|bytes| bytes.checked_sub(usize::from(!value.is_empty())))
            .ok_or_else(|| serde::ser::Error::custom("fixture byte width overflow"))?;
        self.add(
            value
                .len()
                .checked_mul(self.source_passes)
                .and_then(|source| {
                    output
                        .checked_mul(self.output_passes)
                        .and_then(|output| source.checked_add(output))
                })
                .and_then(|work| work.checked_add(32))
                .ok_or_else(|| serde::ser::Error::custom("fixture byte work overflow"))?,
        )?;
        self.record_traversal()
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.visit(4)
    }

    fn serialize_some<T: Serialize + ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error> {
        self.add(32)?;
        self.record_traversal()?;
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        self.visit(4)
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        self.visit(4)
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.add(32)?;
        self.record_traversal()?;
        self.string(variant, false)
    }

    fn serialize_newtype_struct<T: Serialize + ?Sized>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        self.add(32)?;
        self.record_traversal()?;
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: Serialize + ?Sized>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        self.add(32)?;
        self.record_traversal()?;
        self.string(variant, false)?;
        value.serialize(self)
    }

    fn serialize_seq(self, length: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        self.container(length)?;
        Ok(FixtureJsonCompound { serializer: self })
    }

    fn serialize_tuple(self, length: usize) -> Result<Self::SerializeTuple, Self::Error> {
        self.container(Some(length))?;
        Ok(FixtureJsonCompound { serializer: self })
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        length: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        self.container(Some(length))?;
        Ok(FixtureJsonCompound { serializer: self })
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        length: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        self.container(Some(length))?;
        self.string(variant, false)?;
        Ok(FixtureJsonCompound { serializer: self })
    }

    fn serialize_map(self, length: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        self.container(length)?;
        Ok(FixtureJsonCompound { serializer: self })
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        length: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        self.container(Some(length))?;
        Ok(FixtureJsonCompound { serializer: self })
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        length: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        self.container(Some(length))?;
        self.string(variant, false)?;
        Ok(FixtureJsonCompound { serializer: self })
    }

    fn collect_str<T: std::fmt::Display + ?Sized>(
        self,
        _value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        Err(serde::ser::Error::custom(
            "fixture sealed JSON rejects collect_str",
        ))
    }

    fn is_human_readable(&self) -> bool {
        true
    }
}

fn fixture_json_work(value: &(impl Serialize + ?Sized), passes: usize) -> usize {
    let mut serializer = FixtureJsonWork {
        work: 0,
        source_passes: passes
            .checked_add(1)
            .expect("fixture JSON preflight source pass"),
        output_passes: passes,
        map_key: false,
        raw_string_bytes: 0,
        percent_string_bytes: 0,
        max_percent_string_bytes: 0,
        traversal_work: 0,
    };
    value
        .serialize(&mut serializer)
        .expect("sealed fixture JSON shape");
    serializer.work
}

fn fixture_percent_string_metrics(value: &(impl Serialize + ?Sized)) -> (usize, usize) {
    let mut serializer = FixtureJsonWork {
        work: 0,
        source_passes: 0,
        output_passes: 0,
        map_key: false,
        raw_string_bytes: 0,
        percent_string_bytes: 0,
        max_percent_string_bytes: 0,
        traversal_work: 0,
    };
    value
        .serialize(&mut serializer)
        .expect("sealed fixture percent string shape");
    (
        serializer.percent_string_bytes,
        serializer.max_percent_string_bytes,
    )
}

fn fixture_raw_string_bytes(value: &(impl Serialize + ?Sized)) -> usize {
    let mut serializer = FixtureJsonWork {
        work: 0,
        source_passes: 0,
        output_passes: 0,
        map_key: false,
        raw_string_bytes: 0,
        percent_string_bytes: 0,
        max_percent_string_bytes: 0,
        traversal_work: 0,
    };
    value
        .serialize(&mut serializer)
        .expect("sealed fixture raw string shape");
    serializer.raw_string_bytes
}

#[derive(Debug)]
struct WorkTerm {
    scope: String,
    family: &'static str,
    amount: usize,
}

#[derive(Debug, Default)]
struct WorkGroup {
    terms: Vec<WorkTerm>,
}

impl WorkGroup {
    fn push(&mut self, scope: impl Into<String>, family: &'static str, amount: usize) {
        self.terms.push(WorkTerm {
            scope: scope.into(),
            family,
            amount,
        });
    }
}

#[derive(Debug)]
struct ExplicitClusterWork {
    index: usize,
    identity: String,
    signature: WorkGroup,
    scc: WorkGroup,
    basis: WorkGroup,
    connectivity: WorkGroup,
}

#[derive(Debug, Default)]
struct ValidationWorkLedger {
    setup: WorkGroup,
    sealed_json: WorkGroup,
    canonical_privacy: WorkGroup,
    indexes_selection: WorkGroup,
    primary_scc: WorkGroup,
    component_member_hash_basis: WorkGroup,
    cluster_indexing: WorkGroup,
    explicit_clusters: Vec<ExplicitClusterWork>,
    quotient: WorkGroup,
}

impl ValidationWorkLedger {
    fn terms(&self) -> Vec<&WorkTerm> {
        let mut terms = Vec::new();
        for group in [
            &self.setup,
            &self.sealed_json,
            &self.canonical_privacy,
            &self.indexes_selection,
            &self.primary_scc,
            &self.component_member_hash_basis,
            &self.cluster_indexing,
        ] {
            terms.extend(&group.terms);
        }
        for cluster in &self.explicit_clusters {
            for group in [
                &cluster.signature,
                &cluster.scc,
                &cluster.basis,
                &cluster.connectivity,
            ] {
                terms.extend(&group.terms);
            }
        }
        terms.extend(&self.quotient.terms);
        terms
    }

    fn total(&self) -> usize {
        self.terms()
            .into_iter()
            .try_fold(0usize, |sum, term| sum.checked_add(term.amount))
            .expect("validation work ledger sum")
    }

    fn print_receipt(&self, fixture: &str) {
        for cluster in &self.explicit_clusters {
            eprintln!(
                "WORK_LEDGER fixture={fixture} cluster_index={} cluster_identity={}",
                cluster.index, cluster.identity
            );
        }
        let terms = self.terms();
        for term in &terms {
            eprintln!(
                "WORK_LEDGER fixture={fixture} scope={} family={} amount={}",
                term.scope, term.family, term.amount
            );
        }
        eprintln!(
            "WORK_LEDGER fixture={fixture} scope=all family=term-count amount={}",
            terms.len()
        );
        eprintln!(
            "WORK_LEDGER fixture={fixture} scope=all family=checked-sum amount={}",
            self.total()
        );
    }
}

fn fixture_add(left: usize, right: usize, owner: &str) -> usize {
    left.checked_add(right)
        .unwrap_or_else(|| panic!("{owner} overflow"))
}

fn fixture_mul(left: usize, right: usize, owner: &str) -> usize {
    left.checked_mul(right)
        .unwrap_or_else(|| panic!("{owner} overflow"))
}

struct FixtureScc<'a> {
    members: BTreeSet<Identity>,
    edges: Vec<&'a EdgeFact>,
    cyclic: bool,
}

fn fixture_reachable(start: &Identity, target: &Identity, edges: &[&EdgeFact]) -> bool {
    let mut pending = vec![start];
    let mut visited = BTreeSet::new();
    while let Some(node) = pending.pop() {
        if node == target {
            return true;
        }
        if !visited.insert(node) {
            continue;
        }
        pending.extend(
            edges
                .iter()
                .filter(|edge| &edge.record.from == node)
                .map(|edge| &edge.record.to),
        );
    }
    false
}

fn fixture_sccs<'a>(vertices: &BTreeSet<Identity>, edges: &[&'a EdgeFact]) -> Vec<FixtureScc<'a>> {
    let mut remaining = vertices.clone();
    let mut components = Vec::new();
    while let Some(start) = remaining.first().cloned() {
        let members = remaining
            .iter()
            .filter(|candidate| {
                fixture_reachable(&start, candidate, edges)
                    && fixture_reachable(candidate, &start, edges)
            })
            .cloned()
            .collect::<BTreeSet<_>>();
        for member in &members {
            remaining.remove(member);
        }
        let internal = edges
            .iter()
            .copied()
            .filter(|edge| members.contains(&edge.record.from) && members.contains(&edge.record.to))
            .collect::<Vec<_>>();
        let cyclic = members.len() > 1
            || internal
                .iter()
                .any(|edge| edge.record.from == edge.record.to);
        components.push(FixtureScc {
            members,
            edges: internal,
            cyclic,
        });
    }
    components
}

#[derive(Default)]
struct FixtureLinearBasis {
    rows: Vec<Option<Vec<u64>>>,
    columns: usize,
    rank: usize,
}

impl FixtureLinearBasis {
    fn new(columns: usize) -> Self {
        Self {
            rows: vec![None; columns],
            columns,
            rank: 0,
        }
    }

    fn insert_path(&mut self, path: &[usize]) -> bool {
        let mut candidate = vec![0u64; self.columns.div_ceil(64)];
        for index in path {
            candidate[*index / 64] ^= 1u64 << (*index % 64);
        }
        for pivot in (0..self.columns).rev() {
            if candidate[pivot / 64] & (1u64 << (pivot % 64)) == 0 {
                continue;
            }
            if let Some(row) = &self.rows[pivot] {
                for (left, right) in candidate.iter_mut().zip(row) {
                    *left ^= right;
                }
            } else {
                self.rows[pivot] = Some(candidate);
                self.rank += 1;
                return true;
            }
        }
        false
    }
}

#[derive(Default)]
struct FixtureSearchLedger {
    edge_steps: usize,
    considered_lengths: Vec<usize>,
}

struct FixtureSearch<'a> {
    edges: Vec<&'a EdgeFact>,
    members: Vec<&'a Identity>,
    outgoing: Vec<Vec<usize>>,
    target_rank: usize,
    linear: FixtureLinearBasis,
    accepted: usize,
    ledger: FixtureSearchLedger,
}

impl FixtureSearch<'_> {
    fn enumerate(
        &mut self,
        current: usize,
        target_node: usize,
        visited: &mut [bool],
        path: &mut Vec<usize>,
    ) {
        let candidates = self.outgoing[current].clone();
        for index in candidates {
            if self.accepted == self.target_rank {
                return;
            }
            self.ledger.edge_steps += 1;
            let edge = self.edges[index];
            let target = self
                .members
                .binary_search(&&edge.record.to)
                .expect("fixture cycle target");
            path.push(index);
            if target == target_node {
                self.ledger.considered_lengths.push(path.len());
                if self.linear.insert_path(path) {
                    self.accepted += 1;
                }
            } else if !visited[target] {
                visited[target] = true;
                self.enumerate(target, target_node, visited, path);
                visited[target] = false;
            }
            path.pop();
        }
    }
}

fn fixture_cycle_search(
    members: &BTreeSet<Identity>,
    edges: &[&EdgeFact],
    target_rank: usize,
) -> FixtureSearchLedger {
    if target_rank == 0 {
        return FixtureSearchLedger::default();
    }
    let members = members.iter().collect::<Vec<_>>();
    let mut edges = edges.to_vec();
    edges.sort_by(|left, right| left.identity.cmp(&right.identity));
    let mut outgoing = vec![Vec::new(); members.len()];
    for (index, edge) in edges.iter().enumerate() {
        let source = members
            .binary_search(&&edge.record.from)
            .expect("fixture cycle source");
        outgoing[source].push(index);
    }
    let mut search = FixtureSearch {
        linear: FixtureLinearBasis::new(edges.len()),
        edges,
        members,
        outgoing,
        target_rank,
        accepted: 0,
        ledger: FixtureSearchLedger::default(),
    };
    for start in 0..search.edges.len() {
        if search.accepted == target_rank {
            break;
        }
        let edge = search.edges[start];
        let from = search
            .members
            .binary_search(&&edge.record.from)
            .expect("fixture start source");
        let to = search
            .members
            .binary_search(&&edge.record.to)
            .expect("fixture start target");
        let mut path = vec![start];
        if from == to {
            search.ledger.considered_lengths.push(1);
            if search.linear.insert_path(&path) {
                search.accepted += 1;
            }
            continue;
        }
        let mut visited = vec![false; search.members.len()];
        visited[from] = true;
        visited[to] = true;
        search.enumerate(to, from, &mut visited, &mut path);
    }
    assert_eq!(search.accepted, target_rank, "fixture cycle rank");
    search.ledger
}

fn fixture_selected_edges<'a>(
    graph: &'a GraphArtifact,
    projection: &xtask::architecture::project::Projection,
) -> Vec<&'a EdgeFact> {
    let selected = projection.edge_ids.iter().collect::<BTreeSet<_>>();
    graph
        .edges()
        .into_iter()
        .filter(|edge| selected.contains(&edge.identity))
        .collect()
}

fn fixture_directed_rank(members: &BTreeSet<Identity>, edges: &[&EdgeFact]) -> usize {
    fixture_sccs(members, edges)
        .into_iter()
        .filter(|component| component.cyclic)
        .map(|component| {
            fixture_add(component.edges.len(), 1, "fixture directed rank")
                .checked_sub(component.members.len())
                .expect("fixture directed rank underflow")
        })
        .sum()
}

fn fixture_cycle_cost(nodes: usize, edges: usize, rank: usize) -> usize {
    let setup = fixture_add(
        fixture_mul(nodes, 2, "fixture cycle nodes"),
        fixture_mul(edges, 3, "fixture cycle edges"),
        "fixture cycle setup",
    );
    let words = edges.div_ceil(64);
    let algebra = fixture_mul(
        fixture_mul(
            rank,
            fixture_add(rank, 1, "fixture cycle rows"),
            "fixture cycle rows",
        ),
        words,
        "fixture cycle algebra",
    );
    fixture_add(
        fixture_add(setup, algebra, "fixture cycle cost"),
        fixture_mul(rank, nodes.min(edges), "fixture cycle witnesses"),
        "fixture cycle cost",
    )
}

fn fixture_completion_cost(edges: usize, seed: usize, rank: usize) -> usize {
    let rows = fixture_add(seed, rank, "fixture completion rows");
    fixture_add(
        fixture_mul(
            fixture_mul(
                rows,
                fixture_add(rows, 1, "fixture completion elimination rows"),
                "fixture completion rows",
            ),
            edges.div_ceil(64),
            "fixture completion algebra",
        ),
        edges,
        "fixture completion cost",
    )
}

fn fixture_linear_terms(columns: usize, witness_lengths: &[usize]) -> (usize, usize) {
    let rows = witness_lengths.len();
    let witness_edges = witness_lengths.iter().copied().sum::<usize>();
    let words = columns.div_ceil(64);
    let algebra = fixture_mul(
        fixture_mul(
            rows,
            fixture_add(rows, 1, "fixture linear rows"),
            "fixture linear rows",
        ),
        words,
        "fixture linear algebra",
    );
    let allocation = fixture_add(
        fixture_add(
            fixture_mul(rows, words, "fixture linear row allocation"),
            witness_edges,
            "fixture linear allocation",
        ),
        columns,
        "fixture linear allocation",
    );
    (
        rows,
        fixture_add(algebra, allocation, "fixture linear work"),
    )
}

fn push_edge_domain_terms(
    group: &mut WorkGroup,
    scope: &str,
    members: &BTreeSet<Identity>,
    edges: &[&EdgeFact],
) {
    group.push(
        scope,
        "cycle-domain-materialization",
        fixture_add(
            members.len(),
            fixture_mul(edges.len(), 3, "fixture cycle domain edges"),
            "fixture cycle domain",
        ),
    );
    group.push(
        scope,
        "cycle-edge-record-hash",
        edges
            .iter()
            .map(|edge| fixture_json_work(&edge.record, 2))
            .sum(),
    );
    group.push(
        scope,
        "cycle-domain-serialized-preflight",
        fixture_json_work(&(members, edges), 1),
    );
}

fn push_edge_domain_ref_terms(
    group: &mut WorkGroup,
    scope: &str,
    members: &[&Identity],
    edges: &[&EdgeFact],
) {
    group.push(
        scope,
        "cycle-domain-materialization",
        fixture_add(
            members.len(),
            fixture_mul(edges.len(), 3, "fixture cycle domain edges"),
            "fixture cycle domain",
        ),
    );
    group.push(
        scope,
        "cycle-edge-record-hash",
        edges
            .iter()
            .map(|edge| fixture_json_work(&edge.record, 2))
            .sum(),
    );
    group.push(
        scope,
        "cycle-domain-serialized-preflight",
        fixture_json_work(&(members, edges), 1),
    );
}

fn push_scc_terms<'a>(
    group: &mut WorkGroup,
    scope: &str,
    projection: ProjectionName,
    members: &BTreeSet<Identity>,
    edges: &[&'a EdgeFact],
) -> Vec<FixtureScc<'a>> {
    group.push(
        scope,
        "scc-serialization-domain-index",
        fixture_add(
            fixture_mul(members.len(), 5, "fixture SCC vertices"),
            fixture_mul(edges.len(), 6, "fixture SCC edges"),
            "fixture SCC phase",
        ),
    );
    group.push(
        scope,
        "scc-edge-record-hash",
        edges
            .iter()
            .map(|edge| fixture_json_work(&edge.record, 2))
            .sum(),
    );
    group.push(
        scope,
        "scc-serialization-domain",
        fixture_json_work(&(members, edges), 1),
    );
    let components = fixture_sccs(members, edges);
    for (index, component) in components.iter().enumerate() {
        let component_members = component.members.iter().cloned().collect::<Vec<_>>();
        group.push(
            format!("{scope}.component[{index}]"),
            "component-generation-member-hash",
            fixture_json_work(&(projection, &component_members), 2),
        );
    }
    components
}

fn push_search_terms(
    group: &mut WorkGroup,
    scope: &str,
    members: &BTreeSet<Identity>,
    edges: &[&EdgeFact],
    rank: usize,
) {
    let search = fixture_cycle_search(members, edges, rank);
    group.push(scope, "cycle-search", search.edge_steps);
    for (index, length) in search.considered_lengths.iter().copied().enumerate() {
        let (sizing, work) = fixture_linear_terms(edges.len(), &[length]);
        group.push(
            format!("{scope}.candidate[{index}]"),
            "cycle-linear-sizing",
            sizing,
        );
        group.push(format!("{scope}.candidate[{index}]"), "cycle-witness", work);
    }
}

fn fixture_validation_work_ledger(
    graph: &GraphArtifact,
    projection: &xtask::architecture::project::Projection,
) -> ValidationWorkLedger {
    let mut ledger = ValidationWorkLedger::default();
    let nodes = graph.nodes();
    let graph_edges = graph.edges();
    let selected = fixture_selected_edges(graph, projection);
    let vertices = nodes
        .iter()
        .map(|node| node.identity.clone())
        .collect::<BTreeSet<_>>();
    let truth = graph.truth.len();

    ledger.setup.push(
        "graph",
        "validation-setup-counting",
        fixture_mul(truth, 2, "fixture setup counting"),
    );
    ledger.setup.push(
        "graph",
        "validation-setup",
        fixture_add(
            fixture_add(
                fixture_mul(truth, 3, "fixture setup truth"),
                fixture_mul(nodes.len(), 2, "fixture setup nodes"),
                "fixture setup",
            ),
            fixture_mul(graph_edges.len(), 3, "fixture setup edges"),
            "fixture setup",
        ),
    );
    ledger
        .setup
        .push("graph", "validation-setup-serialized-sizing", truth);
    ledger.sealed_json.push(
        "graph",
        "validation-setup-serialized-preflight",
        fixture_graph_preflight(graph),
    );
    let canonical = complete_canonical_ledger(graph);
    ledger.canonical_privacy.push(
        "graph",
        "validation-identity-width-construction",
        canonical.identity_width_construction,
    );
    ledger.canonical_privacy.push(
        "graph",
        "validation-canonical-comparison-complete",
        canonical.comparison_charge(),
    );
    ledger.canonical_privacy.push(
        "graph",
        "validation-canonical-privacy-complete",
        canonical.canonical(),
    );

    ledger.indexes_selection.push(
        "projection",
        "validation-selected-edges",
        fixture_add(
            fixture_add(
                fixture_mul(projection.edge_ids.len(), 3, "fixture selected identities"),
                fixture_mul(graph_edges.len(), 2, "fixture selected graph edges"),
                "fixture selected work",
            ),
            projection.spec.edge_kinds.len(),
            "fixture selected work",
        ),
    );
    ledger.indexes_selection.push(
        "projection",
        "validation-selected-edge-serialized-sizing",
        fixture_add(
            projection.edge_ids.len(),
            projection.spec.edge_kinds.len(),
            "fixture selected sizing",
        ),
    );

    let primary_components = push_scc_terms(
        &mut ledger.primary_scc,
        "primary-scc",
        projection.spec.name,
        &vertices,
        &selected,
    );

    let explicit_component_members = projection
        .components
        .iter()
        .map(|component| component.members.len())
        .sum::<usize>();
    ledger.component_member_hash_basis.push(
        "components",
        "validation-component-member-counting",
        projection.components.len(),
    );
    ledger.component_member_hash_basis.push(
        "components",
        "validation-components",
        fixture_add(
            fixture_mul(primary_components.len(), 2, "fixture component identities"),
            fixture_mul(explicit_component_members, 3, "fixture component members"),
            "fixture component validation",
        ),
    );
    ledger.component_member_hash_basis.push(
        "components",
        "validation-component-serialized-sizing",
        fixture_add(
            projection.components.len(),
            projection.component_bases.len(),
            "fixture component sizing",
        ),
    );
    for (index, component) in projection.components.iter().enumerate() {
        ledger.component_member_hash_basis.push(
            format!("component[{index}]"),
            "component-member-hash",
            fixture_json_work(&(projection.spec.name, &component.members), 2),
        );
    }
    if !primary_components.is_empty() {
        ledger.component_member_hash_basis.push(
            "component-bases",
            "cycle-basis-sizing",
            primary_components.len(),
        );
    }
    for (index, component) in primary_components
        .iter()
        .filter(|component| component.cyclic)
        .enumerate()
    {
        let explicit = projection
            .components
            .iter()
            .find(|candidate| {
                candidate.members.iter().cloned().collect::<BTreeSet<_>>() == component.members
            })
            .expect("fixture cyclic component evidence");
        let basis = projection
            .component_bases
            .iter()
            .find(|basis| basis.component == explicit.identity)
            .expect("fixture component basis");
        let scope = format!("component-basis[{index}]");
        ledger.component_member_hash_basis.push(
            &scope,
            "cycle-member-materialization",
            component.members.len(),
        );
        let member_refs = component.members.iter().collect::<Vec<_>>();
        push_edge_domain_ref_terms(
            &mut ledger.component_member_hash_basis,
            &scope,
            &member_refs,
            &component.edges,
        );
        ledger.component_member_hash_basis.push(
            &scope,
            "directed-rank",
            fixture_mul(
                fixture_add(
                    component.members.len(),
                    component.edges.len(),
                    "fixture component rank domain",
                ),
                3,
                "fixture component rank",
            ),
        );
        ledger.component_member_hash_basis.push(
            &scope,
            "cycle",
            fixture_cycle_cost(component.members.len(), component.edges.len(), basis.rank),
        );
        ledger.component_member_hash_basis.push(
            format!("{scope}.identity"),
            "component-member-hash",
            fixture_json_work(&(projection.spec.name, &member_refs), 2),
        );
        push_search_terms(
            &mut ledger.component_member_hash_basis,
            &format!("{scope}.search"),
            &component.members,
            &component.edges,
            basis.rank,
        );
        ledger.component_member_hash_basis.push(
            format!("{scope}.validation"),
            "cycle-member-hash-sizing",
            component.members.len(),
        );
        ledger.component_member_hash_basis.push(
            format!("{scope}.validation"),
            "cycle-member-hash-preflight",
            fixture_json_work(&(projection.spec.name, &member_refs), 1),
        );
        ledger.component_member_hash_basis.push(
            format!("{scope}.validation"),
            "component-member-hash",
            fixture_json_work(&(projection.spec.name, &member_refs), 2),
        );
        let witness_lengths = basis
            .cycles
            .iter()
            .map(|witness| witness.edges.len())
            .collect::<Vec<_>>();
        let (sizing, linear) = fixture_linear_terms(component.edges.len(), &witness_lengths);
        ledger.component_member_hash_basis.push(
            format!("{scope}.validation"),
            "cycle-linear-sizing",
            sizing,
        );
        ledger.component_member_hash_basis.push(
            format!("{scope}.validation"),
            "cycle-basis-validation",
            linear,
        );
        ledger.component_member_hash_basis.push(
            format!("{scope}.validation"),
            "directed-rank",
            fixture_mul(
                fixture_add(
                    component.members.len(),
                    component.edges.len(),
                    "fixture basis validation rank domain",
                ),
                3,
                "fixture basis validation rank",
            ),
        );
    }

    let explicit_members = projection
        .clusters
        .clusters
        .iter()
        .map(|cluster| cluster.nodes.len())
        .sum::<usize>();
    let component_members = projection
        .components
        .iter()
        .map(|component| component.members.len())
        .sum::<usize>();
    let edge_records = projection
        .clusters
        .clusters
        .iter()
        .map(|cluster| cluster.edge_records.len())
        .sum::<usize>();
    ledger.cluster_indexing.push(
        "clusters",
        "cluster-member-counting",
        fixture_add(
            projection.clusters.clusters.len(),
            projection.components.len(),
            "fixture cluster member counting",
        ),
    );
    ledger.cluster_indexing.push(
        "clusters",
        "cluster-member-validation",
        fixture_add(
            fixture_add(
                fixture_mul(projection.clusters.clusters.len(), 4, "fixture clusters"),
                fixture_mul(explicit_members, 6, "fixture explicit members"),
                "fixture cluster members",
            ),
            fixture_mul(component_members, 4, "fixture component owners"),
            "fixture cluster members",
        ),
    );
    ledger.cluster_indexing.push(
        "clusters",
        "cluster-validation-serialized-sizing",
        fixture_add(
            projection.clusters.clusters.len(),
            selected.len(),
            "fixture cluster sizing",
        ),
    );
    ledger.cluster_indexing.push(
        "clusters",
        "cluster-incidence-counting",
        projection.clusters.clusters.len(),
    );
    ledger.cluster_indexing.push(
        "clusters",
        "cluster-incidence-validation",
        fixture_add(
            fixture_add(
                fixture_mul(selected.len(), 6, "fixture incidence edges"),
                fixture_mul(edge_records, 3, "fixture incidence records"),
                "fixture incidence",
            ),
            fixture_mul(
                projection.clusters.clusters.len(),
                2,
                "fixture incidence clusters",
            ),
            "fixture incidence",
        ),
    );

    for (index, cluster) in projection.clusters.clusters.iter().enumerate() {
        let identity = cluster.identity.digest.as_str().to_owned();
        let scope = format!("cluster[{index}]:{identity}");
        let mut work = ExplicitClusterWork {
            index,
            identity,
            signature: WorkGroup::default(),
            scc: WorkGroup::default(),
            basis: WorkGroup::default(),
            connectivity: WorkGroup::default(),
        };
        work.signature
            .push(&scope, "cluster-signature-sizing", cluster.nodes.len());
        let signature_work = cluster
            .nodes
            .iter()
            .map(|identity| {
                let node = nodes
                    .iter()
                    .find(|node| &node.identity == identity)
                    .expect("fixture cluster node");
                fixture_add(node.attributes.len(), 1, "fixture signature")
            })
            .sum();
        work.signature
            .push(&scope, "cluster-signature-validation", signature_work);
        let members = cluster.nodes.iter().cloned().collect::<BTreeSet<_>>();
        let internal_ids = cluster
            .edge_records
            .iter()
            .filter(|record| {
                record.role == xtask::architecture::project::cluster::CutRole::InternalOwner
            })
            .map(|record| &record.edge)
            .collect::<BTreeSet<_>>();
        let internal = selected
            .iter()
            .copied()
            .filter(|edge| internal_ids.contains(&edge.identity))
            .collect::<Vec<_>>();
        work.connectivity.push(
            &scope,
            "cluster-connectivity-validation",
            fixture_mul(
                cluster.nodes.len(),
                fixture_add(
                    fixture_mul(internal.len(), 2, "fixture connectivity edges"),
                    2,
                    "fixture connectivity",
                ),
                "fixture connectivity",
            ),
        );
        push_edge_domain_terms(&mut work.basis, &scope, &members, &internal);
        let rank = fixture_directed_rank(&members, &internal);
        work.basis.push(
            &scope,
            "directed-rank",
            fixture_mul(
                fixture_add(members.len(), internal.len(), "fixture cluster rank domain"),
                3,
                "fixture cluster rank",
            ),
        );
        let _ = push_scc_terms(
            &mut work.scc,
            &format!("{scope}.rank-scc"),
            projection.spec.name,
            &members,
            &internal,
        );
        work.basis.push(
            &scope,
            "cycle",
            fixture_cycle_cost(members.len(), internal.len(), rank),
        );
        push_search_terms(
            &mut work.basis,
            &format!("{scope}.search"),
            &members,
            &internal,
            rank,
        );
        assert_eq!(
            (
                cluster.internal_basis.rank,
                cluster.internal_basis.cycles.len()
            ),
            (rank, rank),
            "fixture cluster basis"
        );
        ledger.explicit_clusters.push(work);
    }

    let quotient_edges = projection.quotient.edges.len();
    let quotient_edge_ids = projection
        .quotient
        .edges
        .iter()
        .map(|edge| edge.edges.len())
        .sum::<usize>();
    let quotient_cycles = projection
        .quotient
        .bases
        .iter()
        .map(|basis| basis.cycles.len())
        .sum::<usize>();
    let quotient_cycle_edges = projection
        .quotient
        .bases
        .iter()
        .flat_map(|basis| &basis.cycles)
        .map(|cycle| cycle.edges.len())
        .sum::<usize>();
    let seed_cycles = projection
        .clusters
        .clusters
        .iter()
        .map(|cluster| cluster.internal_basis.cycles.len())
        .sum::<usize>();
    let quotient_component_members = primary_components
        .iter()
        .map(|component| component.members.len())
        .sum::<usize>();
    ledger
        .quotient
        .push("quotient", "quotient-edge-counting", quotient_edges);
    ledger.quotient.push(
        "quotient",
        "quotient-cycle-counting",
        projection.quotient.bases.len(),
    );
    ledger
        .quotient
        .push("quotient", "quotient-cycle-edge-counting", quotient_cycles);
    ledger.quotient.push(
        "quotient",
        "quotient-component-member-counting",
        primary_components.len(),
    );
    ledger.quotient.push(
        "quotient",
        "quotient-seed-counting",
        projection.clusters.clusters.len(),
    );
    ledger.quotient.push(
        "quotient",
        "quotient-validation",
        [
            fixture_mul(quotient_edges, 6, "fixture quotient edges"),
            fixture_mul(quotient_edge_ids, 3, "fixture quotient edge ids"),
            fixture_mul(selected.len(), 5, "fixture quotient selected edges"),
            fixture_mul(projection.quotient.bases.len(), 5, "fixture quotient bases"),
            fixture_mul(quotient_cycle_edges, 2, "fixture quotient cycle edges"),
            fixture_mul(quotient_component_members, 4, "fixture quotient members"),
            fixture_mul(seed_cycles, 2, "fixture quotient seeds"),
        ]
        .into_iter()
        .try_fold(0usize, usize::checked_add)
        .expect("fixture quotient validation"),
    );
    ledger.quotient.push(
        "quotient",
        "quotient-validation-serialized-sizing",
        [
            quotient_edges,
            projection.quotient.bases.len(),
            selected.len(),
        ]
        .into_iter()
        .try_fold(0usize, usize::checked_add)
        .expect("fixture quotient sizing"),
    );
    for (index, component) in primary_components
        .iter()
        .filter(|component| component.cyclic)
        .enumerate()
    {
        let explicit = projection
            .components
            .iter()
            .find(|candidate| {
                candidate.members.iter().cloned().collect::<BTreeSet<_>>() == component.members
            })
            .expect("fixture quotient component");
        let seed = projection
            .clusters
            .clusters
            .iter()
            .filter(|cluster| cluster.component == explicit.identity)
            .flat_map(|cluster| &cluster.internal_basis.cycles)
            .collect::<Vec<_>>();
        let complement = projection
            .quotient
            .bases
            .iter()
            .find(|basis| basis.component == explicit.identity)
            .map(|basis| basis.cycles.as_slice())
            .unwrap_or(&[]);
        let declared_rank = projection
            .quotient
            .bases
            .iter()
            .find(|basis| basis.component == explicit.identity)
            .map_or(0, |basis| basis.rank);
        let scope = format!("quotient.component[{index}]");
        let member_refs = component.members.iter().collect::<Vec<_>>();
        push_edge_domain_ref_terms(&mut ledger.quotient, &scope, &member_refs, &component.edges);
        ledger.quotient.push(
            &scope,
            "cycle-validation",
            fixture_completion_cost(component.edges.len(), seed.len(), declared_rank),
        );
        let witness_lengths = seed
            .iter()
            .copied()
            .chain(complement.iter())
            .map(|witness| witness.edges.len())
            .collect::<Vec<_>>();
        let (sizing, linear) = fixture_linear_terms(component.edges.len(), &witness_lengths);
        ledger.quotient.push(&scope, "cycle-linear-sizing", sizing);
        ledger
            .quotient
            .push(&scope, "cycle-validation-algebra", linear);
        // The caller's primary decomposition has already established this
        // exact component. The Refs completion entry charges rank arithmetic
        // only and deliberately does not re-enter SCC.
        ledger.quotient.push(
            &scope,
            "directed-rank",
            fixture_mul(
                fixture_add(
                    component.members.len(),
                    component.edges.len(),
                    "fixture quotient rank domain",
                ),
                3,
                "fixture quotient rank",
            ),
        );
    }

    ledger
}

struct ExpectedValidation {
    fixture: &'static str,
    ledger: ValidationWorkLedger,
    visits: [usize; 8],
    phases: [[usize; 3]; 9],
}

#[derive(Debug, Eq, PartialEq)]
struct ValidationComposite {
    work: usize,
    visits: [usize; 8],
    phases: [[usize; 3]; 9],
}

fn expected_validation(
    fixture: &'static str,
    graph: &GraphArtifact,
    visits: [usize; 8],
    mut phases: [[usize; 3]; 9],
) -> ExpectedValidation {
    let projection = crate::project::rust_projection(graph);
    phases[ValidationPhase::IdentityWidthConstruction as usize] = [
        1,
        graph
            .nodes()
            .len()
            .checked_add(
                graph
                    .edges()
                    .len()
                    .checked_mul(2)
                    .expect("fixture endpoint visits"),
            )
            .expect("fixture Identity visits"),
        0,
    ];
    ExpectedValidation {
        fixture,
        ledger: fixture_validation_work_ledger(graph, &projection),
        visits,
        phases,
    }
}

fn expected_scc_validation_phase(
    projection: &xtask::architecture::project::Projection,
) -> [usize; 3] {
    let calls = 1 + projection.clusters.clusters.len();
    let internal_edges = projection
        .clusters
        .clusters
        .iter()
        .flat_map(|cluster| &cluster.edge_records)
        .filter(|record| {
            record.role == xtask::architecture::project::cluster::CutRole::InternalOwner
        })
        .count();
    let visits = calls + projection.edge_ids.len() + internal_edges;
    [calls, visits, calls]
}

fn assert_validation_oracle(
    graph: &GraphArtifact,
    expected: ExpectedValidation,
    allocator_evidence: bool,
) -> (usize, usize, usize) {
    let projections = project(graph).expect("projections");
    let projection = projections
        .projections
        .iter()
        .find(|projection| projection.spec.name == ProjectionName::RustModule)
        .expect("Rust projection");
    reset_validation_audit();
    let (baseline, _) = crate::take_heap_usage();
    VALIDATION_AUDIT_BASELINE.store(baseline, Ordering::Relaxed);
    let (_, exact_work, exact_bytes, _, owner_prefix_bytes, owner_admission_bytes) =
        xtask::architecture::test_support::validate_projection_budget_with_observer_for_test(
            graph,
            projection,
            usize::MAX,
            usize::MAX,
            observe_validation,
        )
        .expect("measure exact validation budget");
    let (_, peak) = crate::take_heap_usage();
    expected.ledger.print_receipt(expected.fixture);
    let expected_composite = ValidationComposite {
        work: expected.ledger.total(),
        visits: expected.visits,
        phases: expected.phases,
    };
    let actual_composite = ValidationComposite {
        work: exact_work,
        visits: validation_counts(),
        phases: all_phase_counts(),
    };
    assert_eq!(
        actual_composite, expected_composite,
        "test-owned work/traversal/phase composite oracle drifted"
    );
    if allocator_evidence {
        assert_eq!(
            VALIDATION_AUDIT_FIRST_ACTIVE.load(Ordering::Relaxed),
            VALIDATION_AUDIT_BASELINE.load(Ordering::Relaxed),
            "validation-derived allocation occurred before the first admitted operation"
        );
        assert!(
            peak.saturating_sub(baseline) <= exact_bytes as u64,
            "allocator-observed validation peak exceeded admitted bytes"
        );
    }
    reset_validation_audit();
    xtask::architecture::test_support::validate_projection_budget_with_observer_for_test(
        graph,
        projection,
        expected.ledger.total(),
        exact_bytes,
        observe_validation,
    )
    .expect("exact validation boundary");
    for (work, bytes) in [
        (expected.ledger.total() - 1, exact_bytes),
        (expected.ledger.total(), exact_bytes - 1),
    ] {
        let first =
            xtask::architecture::test_support::validate_projection_budget_with_observer_for_test(
                graph,
                projection,
                work,
                bytes,
                observe_validation,
            )
            .expect_err("one below exact validation budget")
            .to_string();
        let second =
            xtask::architecture::test_support::validate_projection_budget_with_observer_for_test(
                graph,
                projection,
                work,
                bytes,
                observe_validation,
            )
            .expect_err("repeat one below exact validation budget")
            .to_string();
        assert_eq!(first, second);
        assert!(first.contains("RESOURCE_BUDGET_EXCEEDED"), "{first}");
    }
    (exact_bytes, owner_prefix_bytes, owner_admission_bytes)
}

#[test]
fn empty_and_singleton_validation_have_test_owned_work_oracles() {
    let mut empty = GraphArtifact::new();
    empty.canonicalize().expect("empty graph");
    assert_validation_oracle(
        &empty,
        expected_validation(
            "empty",
            &empty,
            [1, 0, 0, 0, 0, 0, 0, 0],
            [
                [1, 1, 1],
                [0, 0, 0],
                [1, 1, 0],
                [0, 0, 0],
                [0, 0, 0],
                [0, 0, 0],
                [0, 0, 0],
                [0, 0, 0],
                [0, 0, 0],
            ],
        ),
        false,
    );

    let mut singleton = GraphArtifact::new();
    singleton.exact(
        exact_node(
            crate::project::identity("singleton"),
            NodeKind::VirtualArtifact,
            "singleton",
            Vec::new(),
        )
        .expect("singleton node"),
    );
    singleton.canonicalize().expect("singleton graph");
    assert_validation_oracle(
        &singleton,
        expected_validation(
            "singleton",
            &singleton,
            [1, 0, 1, 0, 0, 0, 0, 0],
            [
                [1, 1, 1],
                [0, 0, 0],
                [1, 1, 1],
                [0, 0, 0],
                [0, 0, 0],
                [0, 0, 0],
                [1, 0, 1],
                [0, 0, 0],
                [0, 0, 0],
            ],
        ),
        false,
    );
}

#[test]
fn parallel_validation_has_test_owned_work_oracle_and_allocator_bound() {
    let graph = crate::project::multigraph(false);
    let projection = crate::project::rust_projection(&graph);
    assert_eq!(projection.clusters.clusters.len(), 1);
    assert_eq!(projection.component_bases.len(), 1);
    let scc_phase = expected_scc_validation_phase(&projection);
    let from = crate::project::identity("a");
    let to = crate::project::identity("b");
    assert_eq!(
        graph
            .edges()
            .into_iter()
            .filter(|edge| edge.record.from == from && edge.record.to == to)
            .count(),
        2,
        "budget fixture is the asserted two-edge same-endpoint multigraph"
    );
    assert_validation_oracle(
        &graph,
        expected_validation(
            "parallel",
            &graph,
            [1, 24, 21, 3, 3, 17, 32, 18],
            [
                [1, 1, 1],
                [0, 0, 0],
                scc_phase,
                [1, 3, 1],
                [3, 27, 3],
                [1, 3, 1],
                [3, 0, 3],
                [38, 48, 0],
                [2, 0, 2],
            ],
        ),
        true,
    );
}

#[test]
fn attribute_cluster_validation_has_test_owned_oracle_and_owner_delta_boundary() {
    let graph = crate::project::signal_graph(
        Some(Attribute::Feature {
            feature: CanonicalText::new("r8-owner-split").expect("feature"),
        }),
        false,
        false,
    );
    let projection = crate::project::rust_projection(&graph);
    assert_eq!(projection.clusters.clusters.len(), 2);
    assert_eq!(projection.component_bases.len(), 1);
    let scc_phase = expected_scc_validation_phase(&projection);
    let (_, owner_prefix_bytes, owner_admission_bytes) = assert_validation_oracle(
        &graph,
        expected_validation(
            "attribute-cluster",
            &graph,
            [1, 75, 154, 25, 26, 50, 576, 54],
            [
                [1, 1, 1],
                [0, 0, 0],
                scc_phase,
                [1, 25, 1],
                [4, 77, 4],
                [1, 25, 1],
                [2, 0, 2],
                [3, 3, 0],
                [2, 0, 2],
            ],
        ),
        false,
    );
    assert!(owner_admission_bytes > 0, "owner admission delta");
    let projections = project(&graph).expect("projections");
    let projection = projections
        .projections
        .iter()
        .find(|projection| projection.spec.name == ProjectionName::RustModule)
        .expect("Rust projection");
    reset_validation_audit();
    let owner_error =
        xtask::architecture::test_support::validate_projection_budget_with_observer_for_test(
            &graph,
            projection,
            usize::MAX,
            owner_prefix_bytes + owner_admission_bytes - 1,
            observe_validation,
        )
        .expect_err("one byte below owner admission must fail before owner insertion")
        .to_string();
    assert!(
        owner_error.contains("cluster validation")
            && owner_error.contains("RESOURCE_BUDGET_EXCEEDED"),
        "{owner_error}"
    );
    assert_eq!(
        validation_counts()[3],
        0,
        "owner insertion ran before its complete byte admission"
    );
}

#[test]
fn cyclic_quotient_validation_has_a_test_owned_work_oracle() {
    let graph = crate::project::signal_graph(None, false, false);
    let projection = crate::project::rust_projection(&graph);
    assert_eq!(projection.clusters.clusters.len(), 2);
    assert_eq!(projection.component_bases.len(), 1);
    let scc_phase = expected_scc_validation_phase(&projection);
    assert!(
        projection
            .quotient
            .bases
            .iter()
            .map(|basis| basis.rank)
            .sum::<usize>()
            > 0,
        "cyclic quotient fixture"
    );
    assert_validation_oracle(
        &graph,
        expected_validation(
            "cyclic-quotient",
            &graph,
            [1, 75, 154, 25, 25, 50, 576, 54],
            [
                [1, 1, 1],
                [0, 0, 0],
                scc_phase,
                [1, 25, 1],
                [4, 77, 4],
                [1, 25, 1],
                [2, 0, 2],
                [3, 3, 0],
                [2, 0, 2],
            ],
        ),
        false,
    );
}

#[test]
fn canonical_privacy_admission_is_length_sensitive_before_validation_allocation() {
    fn graph_with_label(label: String) -> GraphArtifact {
        let mut graph = GraphArtifact::new();
        graph.exact(
            exact_node(
                crate::project::identity("length-sensitive"),
                NodeKind::VirtualArtifact,
                label,
                Vec::new(),
            )
            .expect("portable node"),
        );
        graph.canonicalize().expect("canonical graph");
        graph
    }

    fn measured_work(graph: &GraphArtifact) -> usize {
        let projection = crate::project::rust_projection(graph);
        reset_validation_audit();
        let (_, work, _, _, _, _) =
            xtask::architecture::test_support::validate_projection_budget_with_observer_for_test(
                graph,
                &projection,
                usize::MAX,
                usize::MAX,
                observe_validation,
            )
            .expect("measure validation");
        assert_eq!(
            phase_counts(ValidationPhase::CanonicalPrivacy),
            [1, 1, 1],
            "canonical/privacy phase must be admitted once before one visit/allocation boundary"
        );
        work
    }

    let short = graph_with_label("portable".into());
    let long = graph_with_label("p".repeat(512 * 1024));
    let short_work = measured_work(&short);
    let long_work = measured_work(&long);
    assert!(
        long_work > short_work,
        "same-cardinality canonical work ignored length"
    );

    let projection = crate::project::rust_projection(&long);
    let (_, _, _, canonical_prefix_work, _, _) =
        xtask::architecture::test_support::validate_projection_budget_with_observer_for_test(
            &long,
            &projection,
            usize::MAX,
            usize::MAX,
            observe_validation,
        )
        .expect("measure canonical prefix");
    reset_validation_audit();
    let error =
        xtask::architecture::test_support::validate_projection_budget_with_observer_for_test(
            &long,
            &projection,
            canonical_prefix_work - 1,
            usize::MAX,
            observe_validation,
        )
        .expect_err("one below the exact long canonical/privacy work boundary")
        .to_string();
    assert!(error.contains("RESOURCE_BUDGET_EXCEEDED"), "{error}");
    assert_eq!(
        phase_counts(ValidationPhase::CanonicalPrivacy),
        [0, 0, 0],
        "canonical/privacy validation or allocation began before exact work admission"
    );

    let nested = graph_with_label(format!("%{}", "25".repeat(256 * 1024)));
    let nested_projection = crate::project::rust_projection(&nested);
    reset_validation_audit();
    CANONICAL_SOURCE_SCANS.store(0, Ordering::Relaxed);
    CANONICAL_PERCENT_DECODES.store(0, Ordering::Relaxed);
    let error = with_percent_decode_observer_for_test(observe_canonical_percent, || {
        with_json_observer_for_test(observe_canonical_json, || {
            xtask::architecture::test_support::validate_projection_budget_with_observer_for_test(
                &nested,
                &nested_projection,
                8_000_000,
                512 * 1024 * 1024,
                observe_validation,
            )
        })
    })
    .expect_err("standard budget rejects the derived whole canonical/privacy phase")
    .to_string();
    assert!(error.contains("RESOURCE_BUDGET_EXCEEDED"), "{error}");
    assert_eq!(phase_counts(ValidationPhase::CanonicalPrivacy), [0, 0, 0]);
    assert_eq!(CANONICAL_SOURCE_SCANS.load(Ordering::Relaxed), 0);
    assert_eq!(CANONICAL_PERCENT_DECODES.load(Ordering::Relaxed), 0);
}

fn rust_projection_mut(
    projections: &mut xtask::architecture::project::ProjectionSet,
) -> &mut xtask::architecture::project::Projection {
    projections
        .projections
        .iter_mut()
        .find(|projection| projection.spec.name == ProjectionName::RustModule)
        .expect("Rust projection")
}

fn assert_graph_error(graph: &xtask::architecture::schema::GraphArtifact, expected: &str) {
    let error = project(graph).expect_err(expected);
    assert!(
        error.to_string().contains(expected),
        "expected {expected:?}, got {error:#}"
    );
}

fn rehash_cluster(cluster: &mut xtask::architecture::project::cluster::Cluster) {
    cluster.hash = Sha256Digest::of(
        &serde_json::to_vec(&(
            &cluster.identity,
            &cluster.component,
            &cluster.nodes,
            &cluster.signals,
            &cluster.edge_records,
            &cluster.internal_basis,
            cluster.movement_authorized,
        ))
        .expect("cluster hash input"),
    );
}

fn rehash_projection(projection: &mut xtask::architecture::project::Projection) {
    projection.hash = Sha256Digest::of(
        &serde_json::to_vec(&(
            &projection.schema,
            &projection.spec,
            &projection.node_universe_hash,
            projection.node_count,
            &projection.implicit_singletons,
            &projection.edge_ids,
            &projection.components,
            &projection.component_bases,
            &projection.clusters,
            &projection.quotient,
            &projection.required_failures,
            projection.structurally_valid_for_selected_edges,
            projection.movement_authorized,
        ))
        .expect("projection hash input"),
    );
}

fn assert_error(
    graph: &xtask::architecture::schema::GraphArtifact,
    projections: &xtask::architecture::project::ProjectionSet,
    expected: &str,
) {
    let error = xtask::architecture::evidence::validate::validate_structural(graph, projections)
        .expect_err(expected);
    assert!(
        error.to_string().contains(expected),
        "expected {expected:?}, got {error:#}"
    );
}
