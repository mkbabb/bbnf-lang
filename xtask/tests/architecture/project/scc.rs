use std::collections::BTreeSet;

use xtask::architecture::project::scc::{ProjectionBudget, decompose, strongly_connected};
use xtask::architecture::schema::{
    CanonicalText, Condition, EdgeFact, EdgeKind, Fact, Identity, ProjectionName, ResolverId,
    Sha256Digest, SourceAnchor, exact_edge,
};
use xtask::architecture::test_support::{
    JsonOperation, ValidationOperation, ValidationPhase, sha256,
    validate_component_members_for_test, validate_scc_serialization_for_test,
    with_json_observer_for_test, with_validation_observer_for_test,
};

use std::sync::atomic::{AtomicUsize, Ordering};

static SCC_PHASE: [AtomicUsize; 3] = [const { AtomicUsize::new(0) }; 3];
static SCC_SOURCE_BYTES: AtomicUsize = AtomicUsize::new(0);

fn observe_scc_json(operation: JsonOperation) {
    if let JsonOperation::SourceBytesScanned(bytes) = operation {
        SCC_SOURCE_BYTES.fetch_add(bytes, Ordering::Relaxed);
    }
}

fn observe_scc_phase(operation: ValidationOperation) {
    let slot = match operation {
        ValidationOperation::PhasePrecharged(ValidationPhase::SccSerializationIndex) => Some(0),
        ValidationOperation::PhaseVisit(ValidationPhase::SccSerializationIndex) => Some(1),
        ValidationOperation::PhaseAllocation(ValidationPhase::SccSerializationIndex) => Some(2),
        _ => None,
    };
    if let Some(slot) = slot {
        SCC_PHASE[slot].fetch_add(1, Ordering::Relaxed);
    }
}

fn reset_scc_phase() {
    for count in &SCC_PHASE {
        count.store(0, Ordering::Relaxed);
    }
}

fn scc_phase() -> [usize; 3] {
    std::array::from_fn(|index| SCC_PHASE[index].load(Ordering::Relaxed))
}

fn identity(index: usize) -> Identity {
    Identity::Virtual {
        namespace: CanonicalText::new("kernel-test").expect("namespace"),
        key: CanonicalText::new(format!("n{index}")).expect("key"),
    }
}

fn long_identity(length: usize) -> Identity {
    Identity::Virtual {
        namespace: CanonicalText::new("kernel-test").expect("namespace"),
        key: CanonicalText::new("n".repeat(length)).expect("key"),
    }
}

fn edge(from: usize, to: usize, resolver: &str) -> EdgeFact {
    match exact_edge(
        identity(from),
        identity(to),
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

fn independent_scc_work(
    _projection: ProjectionName,
    vertices: &BTreeSet<Identity>,
    edges: &[EdgeFact],
) -> usize {
    let declared = vertices.len() * 5 + edges.len() * 6;
    let edge_hashes = edges
        .iter()
        .map(|edge| edge_record_work(edge, 2))
        .sum::<usize>();
    let domain = container_work(2)
        + container_work(vertices.len())
        + vertices
            .iter()
            .map(|identity| virtual_identity_work(identity, 1))
            .sum::<usize>()
        + container_work(edges.len())
        + edges
            .iter()
            .map(|edge| edge_fact_work(edge, 1))
            .sum::<usize>();
    declared + edge_hashes + domain
}

fn independent_scc_phase_bytes(vertices: &BTreeSet<Identity>, edges: &[EdgeFact]) -> usize {
    let vertex_indexes = std::mem::size_of::<Identity>()
        .checked_mul(vertices.len())
        .and_then(|bytes| bytes.checked_mul(8))
        .expect("fixture vertex index bytes");
    let edge_indexes = std::mem::size_of::<&EdgeFact>()
        .checked_mul(edges.len())
        .and_then(|bytes| bytes.checked_mul(6))
        .expect("fixture edge index bytes");
    let domain = serde_json::to_vec(&(vertices, edges)).expect("SCC fixture domain bytes");
    vertex_indexes
        .checked_add(edge_indexes)
        .and_then(|bytes| {
            domain
                .len()
                .checked_mul(4)
                .and_then(|domain| bytes.checked_add(domain))
        })
        .expect("fixture SCC phase bytes")
}

pub(super) fn container_work(length: usize) -> usize {
    (length + 1) * 32
}

pub(super) fn ascii_string_work(value: &str, passes: usize) -> usize {
    assert!(
        value.bytes().all(|byte| byte.is_ascii()
            && !byte.is_ascii_control()
            && !matches!(byte, b'"' | b'\\')),
        "fixture string needs escaping: {value:?}"
    );
    let length = value.len();
    passes
        .checked_add(1)
        .and_then(|source_passes| source_passes.checked_mul(length))
        .and_then(|source| {
            length
                .checked_mul(6)
                .and_then(|output| output.checked_add(2))
                .and_then(|output| passes.checked_mul(output))
                .and_then(|output| source.checked_add(output))
        })
        .and_then(|work| work.checked_add(32))
        .expect("fixture checked JSON string work")
}

pub(super) fn newtype_string_work(value: &str, passes: usize) -> usize {
    ascii_string_work(value, passes)
}

pub(super) fn unit_variant_work(name: &str, passes: usize) -> usize {
    32 + ascii_string_work(name, passes)
}

pub(super) fn virtual_identity_work(identity: &Identity, passes: usize) -> usize {
    let Identity::Virtual { namespace, key } = identity else {
        panic!("SCC fixture must use virtual identities")
    };
    container_work(3)
        + ascii_string_work("kind", passes)
        + ascii_string_work("virtual", passes)
        + ascii_string_work("namespace", passes)
        + newtype_string_work(namespace.as_str(), passes)
        + ascii_string_work("key", passes)
        + newtype_string_work(key.as_str(), passes)
}

fn tagged_unit_work(tag: &str, variant: &str, passes: usize) -> usize {
    container_work(1) + ascii_string_work(tag, passes) + ascii_string_work(variant, passes)
}

pub(super) fn edge_record_work(edge: &EdgeFact, passes: usize) -> usize {
    let SourceAnchor::Synthesized { authority } = &edge.record.source else {
        panic!("SCC fixture source must be synthesized")
    };
    assert!(matches!(edge.record.condition, Condition::Always));
    assert!(edge.record.attributes.is_empty());
    container_work(8)
        + ascii_string_work("from", passes)
        + virtual_identity_work(&edge.record.from, passes)
        + ascii_string_work("to", passes)
        + virtual_identity_work(&edge.record.to, passes)
        + ascii_string_work("kind", passes)
        + unit_variant_work("rust_uses", passes)
        + ascii_string_work("class", passes)
        + unit_variant_work("resolved_build", passes)
        + ascii_string_work("condition", passes)
        + tagged_unit_work("kind", "always", passes)
        + ascii_string_work("source", passes)
        + container_work(2)
        + ascii_string_work("kind", passes)
        + ascii_string_work("synthesized", passes)
        + ascii_string_work("authority", passes)
        + newtype_string_work(authority.as_str(), passes)
        + ascii_string_work("resolver", passes)
        + newtype_string_work(edge.record.resolver.as_str(), passes)
        + ascii_string_work("attributes", passes)
        + container_work(0)
}

pub(super) fn edge_fact_work(edge: &EdgeFact, passes: usize) -> usize {
    container_work(2)
        + ascii_string_work("identity", passes)
        + newtype_string_work(edge.identity.as_str(), passes)
        + ascii_string_work("record", passes)
        + edge_record_work(edge, passes)
}

pub(super) fn component_member_work(
    projection: ProjectionName,
    members: &[Identity],
    passes: usize,
) -> usize {
    let projection = match projection {
        ProjectionName::RustModule => "rust_module",
        _ => panic!("fixture only derives the Rust projection"),
    };
    container_work(2)
        + unit_variant_work(projection, passes)
        + container_work(members.len())
        + members
            .iter()
            .map(|member| virtual_identity_work(member, passes))
            .sum::<usize>()
}

pub(super) fn independent_component(
    projection: ProjectionName,
    members: &[Identity],
) -> xtask::architecture::project::scc::ComponentIdentity {
    let canonical = serde_json::to_vec(&(projection, members)).expect("component canonical bytes");
    xtask::architecture::project::scc::ComponentIdentity {
        projection,
        digest: Sha256Digest::new(sha256(&canonical)).expect("component digest"),
    }
}

#[test]
fn duplicate_typed_edge_identity_is_rejected_before_scc_work() {
    let vertices = (0..2).map(identity).collect::<BTreeSet<_>>();
    let edge = edge(0, 1, "same");
    let edges = vec![edge.clone(), edge];
    let mut budget = ProjectionBudget::standard(ProjectionName::RustModule);
    let error = decompose(ProjectionName::RustModule, &vertices, &edges, &mut budget)
        .expect_err("duplicate edge identity");
    assert!(error.to_string().contains("SCC_DOMAIN_MISMATCH"));
}

#[test]
fn forged_edge_identity_is_rejected_before_traversal() {
    let vertices = (0..2).map(identity).collect::<BTreeSet<_>>();
    let mut forged = edge(0, 1, "original");
    forged.record.resolver = ResolverId::new("mutated-after-identity").expect("resolver");
    let mut budget = ProjectionBudget::standard(ProjectionName::RustModule);
    let error = decompose(
        ProjectionName::RustModule,
        &vertices,
        &[forged],
        &mut budget,
    )
    .expect_err("forged edge identity");
    assert!(error.to_string().contains("typed edge identity is invalid"));
}

#[test]
fn escaped_endpoint_fails_before_budget_or_indexing() {
    let vertices = BTreeSet::from([identity(0)]);
    let edges = vec![edge(0, 1, "escaped")];
    let mut budget = ProjectionBudget::standard(ProjectionName::RustModule);
    let error = decompose(ProjectionName::RustModule, &vertices, &edges, &mut budget)
        .expect_err("escaped endpoint");
    assert!(error.to_string().contains("SCC_DOMAIN_MISMATCH"));
}

#[test]
fn scc_declared_cost_is_checked_before_traversal() {
    let vertices = (0..2).map(identity).collect::<BTreeSet<_>>();
    let edges = vec![edge(0, 1, "one")];
    let mut budget = ProjectionBudget::with_limit(ProjectionName::RustModule, 0);
    let first = decompose(ProjectionName::RustModule, &vertices, &edges, &mut budget)
        .expect_err("zero projection budget")
        .to_string();
    let mut budget = ProjectionBudget::with_limit(ProjectionName::RustModule, 0);
    let second = decompose(ProjectionName::RustModule, &vertices, &edges, &mut budget)
        .expect_err("repeat zero projection budget")
        .to_string();
    assert_eq!(first, second);
    assert!(first.contains("RESOURCE_BUDGET_EXCEEDED"));
}

#[test]
fn public_scc_entrypoint_spends_only_the_caller_owned_budget() {
    let vertices = (0..2).map(identity).collect::<BTreeSet<_>>();
    let edges = vec![edge(0, 1, "one")];
    let mut budget = ProjectionBudget::with_limit(ProjectionName::RustModule, 0);
    let error = strongly_connected(ProjectionName::RustModule, &vertices, &edges, &mut budget)
        .expect_err("public SCC API must not mint a replacement budget");
    assert!(error.to_string().contains("RESOURCE_BUDGET_EXCEEDED"));
    assert_eq!(budget.used(), 0);
}

#[test]
fn scc_phase_reserves_one_aggregate_byte_budget_before_derived_allocation() {
    let vertices = (0..3).map(identity).collect::<BTreeSet<_>>();
    let edges = vec![edge(0, 1, "01"), edge(1, 2, "12"), edge(2, 0, "20")];
    let exact_bytes = independent_scc_phase_bytes(&vertices, &edges);
    let phase_work = independent_scc_work(ProjectionName::RustModule, &vertices, &edges);
    assert!(exact_bytes > 0);

    let mut short =
        ProjectionBudget::with_limits(ProjectionName::RustModule, usize::MAX, exact_bytes - 1);
    reset_scc_phase();
    let error = with_validation_observer_for_test(observe_scc_phase, || {
        decompose(ProjectionName::RustModule, &vertices, &edges, &mut short)
    })
    .expect_err("one byte below exact aggregate reservation")
    .to_string();
    assert!(error.contains("RESOURCE_BUDGET_EXCEEDED"));
    assert_eq!(
        short.used(),
        phase_work,
        "the exact SCC phase precharge drifted"
    );
    assert_eq!(
        scc_phase()[2],
        0,
        "SCC index allocation began before its exact byte admission"
    );

    let mut exact =
        ProjectionBudget::with_limits(ProjectionName::RustModule, usize::MAX, exact_bytes);
    reset_scc_phase();
    let error = with_validation_observer_for_test(observe_scc_phase, || {
        decompose(ProjectionName::RustModule, &vertices, &edges, &mut exact)
    })
    .expect_err("the next component digest reservation exceeds the exact SCC prefix")
    .to_string();
    assert!(error.contains("RESOURCE_BUDGET_EXCEEDED"));
    assert_eq!(
        scc_phase(),
        [1, 4, 1],
        "the exact SCC byte prefix did not admit exactly one index allocation"
    );
}

#[test]
fn scc_serialization_domain_and_indexes_follow_one_exact_precharge() {
    let vertices = (0..3).map(identity).collect::<BTreeSet<_>>();
    let edges = vec![edge(0, 1, "01"), edge(1, 2, "12"), edge(2, 0, "20")];

    let phase_work = independent_scc_work(ProjectionName::RustModule, &vertices, &edges);
    reset_scc_phase();
    let mut short = ProjectionBudget::with_limit(ProjectionName::RustModule, phase_work - 1);
    with_validation_observer_for_test(observe_scc_phase, || {
        decompose(ProjectionName::RustModule, &vertices, &edges, &mut short)
    })
    .expect_err("one below the test-owned SCC phase prefix");
    assert_eq!(
        scc_phase(),
        [0, 0, 0],
        "SCC serialization/domain/index work began before its phase precharge"
    );

    reset_scc_phase();
    let mut exact = ProjectionBudget::with_limit(ProjectionName::RustModule, phase_work);
    with_validation_observer_for_test(observe_scc_phase, || {
        decompose(ProjectionName::RustModule, &vertices, &edges, &mut exact)
    })
    .expect_err("the next component-hash phase exceeds the exact SCC prefix");
    assert_eq!(
        scc_phase(),
        [1, 4, 1],
        "one serializer visit, three domain visits, and one aggregate index allocation"
    );
}

#[test]
fn scc_and_component_hash_work_are_content_sensitive_before_source_scans() {
    let short_vertices = BTreeSet::from([identity(0)]);
    let long_vertices = BTreeSet::from([long_identity(64 * 1024)]);
    let short_work = independent_scc_work(ProjectionName::RustModule, &short_vertices, &[]);
    let long_work = independent_scc_work(ProjectionName::RustModule, &long_vertices, &[]);
    assert!(long_work > short_work);

    SCC_SOURCE_BYTES.store(0, Ordering::Relaxed);
    let error = with_json_observer_for_test(observe_scc_json, || {
        validate_scc_serialization_for_test(
            ProjectionName::RustModule,
            &long_vertices,
            &[],
            long_work - 1,
        )
    })
    .expect_err("one below long SCC preflight");
    assert!(error.to_string().contains("RESOURCE_BUDGET_EXCEEDED"));
    assert_eq!(
        SCC_SOURCE_BYTES.load(Ordering::Relaxed),
        0,
        "error={error:#}"
    );

    assert_eq!(
        validate_scc_serialization_for_test(
            ProjectionName::RustModule,
            &long_vertices,
            &[],
            long_work,
        )
        .expect("exact independently computed SCC boundary"),
        long_work
    );

    let short_members = short_vertices.into_iter().collect::<Vec<_>>();
    let long_members = long_vertices.into_iter().collect::<Vec<_>>();
    let short_member_work = component_member_work(ProjectionName::RustModule, &short_members, 2);
    let member_work = component_member_work(ProjectionName::RustModule, &long_members, 2);
    assert!(member_work > short_member_work);
    let component = independent_component(ProjectionName::RustModule, &long_members);
    SCC_SOURCE_BYTES.store(0, Ordering::Relaxed);
    with_json_observer_for_test(observe_scc_json, || {
        validate_component_members_for_test(
            &component,
            ProjectionName::RustModule,
            &long_members,
            member_work,
        )
    })
    .expect("exact independently computed member hash boundary");
    assert!(SCC_SOURCE_BYTES.load(Ordering::Relaxed) > 0);
    SCC_SOURCE_BYTES.store(0, Ordering::Relaxed);
    with_json_observer_for_test(observe_scc_json, || {
        validate_component_members_for_test(
            &component,
            ProjectionName::RustModule,
            &long_members,
            member_work - 1,
        )
    })
    .expect_err("one below component-member hash");
    assert_eq!(SCC_SOURCE_BYTES.load(Ordering::Relaxed), 0);
}
