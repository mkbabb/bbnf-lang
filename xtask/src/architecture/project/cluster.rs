use std::collections::{BTreeMap, BTreeSet, VecDeque};

use anyhow::{Context, Result, bail};
use serde::ser::{SerializeSeq, Serializer};
use serde::{Deserialize, Serialize};

use crate::architecture::project::cycle::{CycleWitness, basis_for_subgraph};
use crate::architecture::project::scc::{ComponentIdentity, Decomposition, ProjectionBudget};
use crate::architecture::schema::{
    Attribute, CanonicalText, EdgeFact, EdgeIdentity, Identity, NodeFact, NodeKind, ProjectionName,
    Sha256Digest,
};

const TARGET_NODES: usize = 24;
const TARGET_INTERNAL_EDGES: usize = 80;
const MAX_NODES: usize = 32;
const MAX_INTERNAL_EDGES: usize = 120;

pub const CLUSTER_SCHEMA: &str = "bbnf.architecture.clusters.v3.1";
pub const IMPLICIT_SINGLETONS: &str =
    "every node absent from clusters expands to one projection-qualified singleton";

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CutRole {
    InternalOwner,
    OutgoingOwner,
    IncomingReference,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TypedEdgeRecord {
    pub role: CutRole,
    pub edge: crate::architecture::schema::EdgeIdentity,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ClusterSignal {
    Scc,
    Articulation,
    Fan,
    Ownership,
    Feature,
    GeneratedLineage,
    PackageFace,
    TestReachability,
    FrontendFeature,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ClusterIdentity {
    pub projection: ProjectionName,
    pub digest: Sha256Digest,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ShardBasis {
    pub rank: usize,
    pub cycles: Vec<CycleWitness>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Cluster {
    pub identity: ClusterIdentity,
    pub component: ComponentIdentity,
    pub nodes: Vec<Identity>,
    pub signals: Vec<ClusterSignal>,
    pub edge_records: Vec<TypedEdgeRecord>,
    pub internal_basis: ShardBasis,
    pub movement_authorized: bool,
    pub hash: Sha256Digest,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ClusterSet {
    pub schema: CanonicalText,
    pub implicit_singletons: CanonicalText,
    pub clusters: Vec<Cluster>,
}

pub struct ClusterBuild {
    set: ClusterSet,
    ownership: BTreeMap<Identity, ClusterIdentity>,
    components: BTreeMap<ClusterIdentity, ComponentIdentity>,
    edge_domain_seal: Sha256Digest,
}

#[cfg(feature = "architecture-test-seams")]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[doc(hidden)]
pub enum QuotientDomainOperation {
    SerializerSizingPrecharged { work: usize },
    DomainScanPrecharged { work: usize, bytes: usize },
    Scan,
    CompleteBasisPrecharged { work: usize },
    CompleteBasisVisit,
    Derived,
}

#[cfg(feature = "architecture-test-seams")]
thread_local! {
    static QUOTIENT_DOMAIN_OBSERVER: std::cell::Cell<Option<fn(QuotientDomainOperation)>> =
        const { std::cell::Cell::new(None) };
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn with_quotient_domain_observer_for_test<T>(
    observer: fn(QuotientDomainOperation),
    operation: impl FnOnce() -> T,
) -> T {
    struct Restore(Option<fn(QuotientDomainOperation)>);

    impl Drop for Restore {
        fn drop(&mut self) {
            QUOTIENT_DOMAIN_OBSERVER.with(|slot| slot.set(self.0));
        }
    }

    let previous = QUOTIENT_DOMAIN_OBSERVER.with(|slot| slot.replace(Some(observer)));
    let restore = Restore(previous);
    let result = operation();
    drop(restore);
    result
}

#[cfg(feature = "architecture-test-seams")]
pub(crate) fn observe_quotient_domain(operation: QuotientDomainOperation) {
    QUOTIENT_DOMAIN_OBSERVER.with(|slot| {
        if let Some(observer) = slot.get() {
            observer(operation);
        }
    });
}

impl ClusterBuild {
    pub(crate) fn set(&self) -> &ClusterSet {
        &self.set
    }

    pub(crate) fn owner(&self, member: &Identity) -> Option<&ClusterIdentity> {
        self.ownership.get(member)
    }

    pub(crate) fn ownership_len(&self) -> usize {
        self.ownership.len()
    }

    pub(crate) fn into_set(self) -> ClusterSet {
        self.set
    }

    pub(crate) fn validate_domain(
        &self,
        decomposition: &Decomposition<'_>,
        budget: &mut ProjectionBudget,
    ) -> Result<()> {
        let projection = decomposition.projection();
        let serializer_sizing_work = declared_cluster_domain_validation_cost(
            self.ownership.len(),
            decomposition.edges().len(),
            decomposition.components().len(),
            self.set.clusters.len(),
        )?;
        budget.charge(
            projection,
            "cluster-domain-serializer-sizing",
            serializer_sizing_work,
        )?;
        #[cfg(feature = "architecture-test-seams")]
        observe_quotient_domain(QuotientDomainOperation::SerializerSizingPrecharged {
            work: serializer_sizing_work,
        });
        let _declared_bytes = admit_cluster_domain_validation_bytes(self, decomposition, budget)?;
        let declared_work = declared_cluster_domain_validation_cost(
            self.ownership.len(),
            decomposition.edges().len(),
            decomposition.components().len(),
            self.set.clusters.len(),
        )?;
        budget.charge(projection, "cluster-domain-validation", declared_work)?;
        #[cfg(feature = "architecture-test-seams")]
        observe_quotient_domain(QuotientDomainOperation::DomainScanPrecharged {
            work: declared_work,
            bytes: _declared_bytes,
        });
        if self.edge_domain_seal != edge_domain_seal(decomposition)? {
            bail!("CLUSTER_DOMAIN_MISMATCH: typed edge domain differs");
        }
        let expected_members =
            decomposition
                .components()
                .iter()
                .try_fold(0usize, |sum, component| {
                    #[cfg(feature = "architecture-test-seams")]
                    observe_quotient_domain(QuotientDomainOperation::Scan);
                    sum.checked_add(component.members.len()).ok_or_else(|| {
                        anyhow::anyhow!("RESOURCE_BUDGET_EXCEEDED: cluster domain count overflowed")
                    })
                })?;
        if self.ownership.len() != expected_members {
            bail!("CLUSTER_DOMAIN_MISMATCH: cluster ownership domain differs");
        }
        if self.set.clusters.windows(2).any(|pair| {
            #[cfg(feature = "architecture-test-seams")]
            observe_quotient_domain(QuotientDomainOperation::Scan);
            pair[0].identity >= pair[1].identity
        }) {
            bail!("CLUSTER_DOMAIN_MISMATCH: explicit clusters are not canonical");
        }
        let mut explicit_members = 0usize;
        for cluster in &self.set.clusters {
            #[cfg(feature = "architecture-test-seams")]
            observe_quotient_domain(QuotientDomainOperation::Scan);
            if cluster.identity.projection != projection
                || cluster.component.projection != projection
                || self.components.get(&cluster.identity) != Some(&cluster.component)
                || cluster.nodes.is_empty()
                || cluster.nodes.windows(2).any(|pair| pair[0] >= pair[1])
            {
                bail!("CLUSTER_DOMAIN_MISMATCH: explicit cluster domain differs");
            }
            explicit_members = explicit_members
                .checked_add(cluster.nodes.len())
                .ok_or_else(|| {
                    anyhow::anyhow!("RESOURCE_BUDGET_EXCEEDED: explicit cluster count overflowed")
                })?;
            for member in &cluster.nodes {
                #[cfg(feature = "architecture-test-seams")]
                observe_quotient_domain(QuotientDomainOperation::Scan);
                if self.ownership.get(member) != Some(&cluster.identity)
                    || decomposition.owner(member).map(|owner| &owner.identity)
                        != Some(&cluster.component)
                {
                    bail!("CLUSTER_DOMAIN_MISMATCH: explicit cluster ownership differs");
                }
            }
        }
        let mut singleton_count = 0usize;
        for component in decomposition.components() {
            #[cfg(feature = "architecture-test-seams")]
            observe_quotient_domain(QuotientDomainOperation::Scan);
            if component.members.len() == 1 && !component.cyclic {
                singleton_count = singleton_count.checked_add(1).ok_or_else(|| {
                    anyhow::anyhow!("RESOURCE_BUDGET_EXCEEDED: singleton count overflowed")
                })?;
                let member = &component.members[0];
                let identity = cluster_identity(
                    projection,
                    &component.identity,
                    std::slice::from_ref(member),
                )?;
                if self.ownership.get(member) != Some(&identity)
                    || self.components.get(&identity) != Some(&component.identity)
                {
                    bail!("CLUSTER_DOMAIN_MISMATCH: implicit singleton ownership differs");
                }
            }
        }
        if explicit_members
            .checked_add(singleton_count)
            .context("RESOURCE_BUDGET_EXCEEDED: cluster coverage count overflowed")?
            != expected_members
            || self.components.len()
                != self
                    .set
                    .clusters
                    .len()
                    .checked_add(singleton_count)
                    .context("RESOURCE_BUDGET_EXCEEDED: cluster identity count overflowed")?
        {
            bail!("CLUSTER_DOMAIN_MISMATCH: cluster identity/component domain differs");
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct Signature {
    ownership: Vec<CanonicalText>,
    feature: Vec<CanonicalText>,
    lineage: Vec<CanonicalText>,
    faces: Vec<crate::architecture::schema::PackageFaceId>,
    test: Vec<bool>,
    frontend: Vec<CanonicalText>,
}

pub fn cluster(
    decomposition: &Decomposition<'_>,
    nodes: &BTreeMap<Identity, NodeFact>,
    budget: &mut ProjectionBudget,
) -> Result<ClusterBuild> {
    let projection = decomposition.projection();
    let components = decomposition.components();
    let edges = decomposition.edges();
    let member_count = components
        .iter()
        .try_fold(0usize, |sum, component| {
            sum.checked_add(component.members.len())
        })
        .ok_or_else(|| {
            anyhow::anyhow!("RESOURCE_BUDGET_EXCEEDED: cluster member count overflowed")
        })?;
    budget.charge(
        projection,
        "cluster",
        declared_cluster_cost(member_count, edges.len(), components.len())?,
    )?;
    admit_cluster_bytes(projection, components, nodes, edges, member_count, budget)?;
    let edge_domain_seal = edge_domain_seal(decomposition)?;
    let graph = GraphIndex::checked(decomposition, nodes, budget)?;
    let mut ownership = BTreeMap::new();
    let mut cluster_components = BTreeMap::new();
    let mut drafts = Vec::new();
    for component in components {
        if component.members.len() == 1 && !component.cyclic {
            let member = component.members[0].clone();
            let identity = cluster_identity(
                projection,
                &component.identity,
                std::slice::from_ref(&member),
            )?;
            ownership.insert(member.clone(), identity.clone());
            cluster_components.insert(identity, component.identity.clone());
            continue;
        }
        let mut groups = BTreeMap::<Signature, Vec<Identity>>::new();
        for member in &component.members {
            groups
                .entry(signature(nodes.get(member).ok_or_else(|| {
                    anyhow::anyhow!("CLUSTER_DOMAIN_MISMATCH: component member has no node fact")
                })?))
                .or_default()
                .push(member.clone());
        }
        for members in groups.values() {
            for connected in graph.connected_groups(members)? {
                for shard in graph_partition(projection, &connected, &graph, budget)? {
                    let identity = cluster_identity(projection, &component.identity, &shard)?;
                    for member in &shard {
                        if ownership.insert(member.clone(), identity.clone()).is_some() {
                            bail!("cluster ownership intersection is not empty");
                        }
                    }
                    cluster_components.insert(identity.clone(), component.identity.clone());
                    drafts.push((identity, component.identity.clone(), shard));
                }
            }
        }
    }
    let index = drafts
        .iter()
        .enumerate()
        .map(|(index, (identity, _, _))| (identity.clone(), index))
        .collect::<BTreeMap<_, _>>();
    let mut incident = vec![Vec::<TypedEdgeRecord>::new(); drafts.len()];
    for edge in &graph.edges {
        let from = ownership
            .get(&edge.record.from)
            .ok_or_else(|| anyhow::anyhow!("CLUSTER_DOMAIN_MISMATCH: edge source is unowned"))?;
        let to = ownership
            .get(&edge.record.to)
            .ok_or_else(|| anyhow::anyhow!("CLUSTER_DOMAIN_MISMATCH: edge target is unowned"))?;
        if let Some(from_index) = index.get(from) {
            incident[*from_index].push(TypedEdgeRecord {
                role: if from == to {
                    CutRole::InternalOwner
                } else {
                    CutRole::OutgoingOwner
                },
                edge: edge.identity.clone(),
            });
        }
        if from != to
            && let Some(to_index) = index.get(to)
        {
            incident[*to_index].push(TypedEdgeRecord {
                role: CutRole::IncomingReference,
                edge: edge.identity.clone(),
            });
        }
    }
    let mut clusters = Vec::new();
    for ((identity, component, members), mut edge_records) in drafts.into_iter().zip(incident) {
        edge_records.sort();
        let node_records = members
            .iter()
            .map(|identity| {
                nodes.get(identity).cloned().ok_or_else(|| {
                    anyhow::anyhow!("CLUSTER_DOMAIN_MISMATCH: shard member has no node fact")
                })
            })
            .collect::<Result<Vec<_>>>()?;
        let signals = signals(&node_records, &members, &graph)?;
        let internal_edges = edge_records
            .iter()
            .filter(|record| record.role == CutRole::InternalOwner)
            .map(|record| graph.edge(&record.edge))
            .collect::<Result<Vec<_>>>()?;
        let member_set = members.iter().cloned().collect::<BTreeSet<_>>();
        let (rank, cycles) = basis_for_subgraph(projection, &member_set, &internal_edges, budget)?;
        let internal_basis = ShardBasis { rank, cycles };
        let movement_authorized = false;
        let hash = Sha256Digest::new(crate::architecture::hash::hash_serialized(&(
            &identity,
            &component,
            &members,
            &signals,
            &edge_records,
            &internal_basis,
            movement_authorized,
        ))?)?;
        clusters.push(Cluster {
            identity,
            component,
            nodes: members,
            signals,
            edge_records,
            internal_basis,
            movement_authorized,
            hash,
        });
    }
    clusters.sort_by(|left, right| left.identity.cmp(&right.identity));
    Ok(ClusterBuild {
        set: ClusterSet {
            schema: CanonicalText::new(CLUSTER_SCHEMA)?,
            implicit_singletons: CanonicalText::new(IMPLICIT_SINGLETONS)?,
            clusters,
        },
        ownership,
        components: cluster_components,
        edge_domain_seal,
    })
}

fn edge_domain_seal(decomposition: &Decomposition<'_>) -> Result<Sha256Digest> {
    let mut hash = crate::architecture::hash::Sha256::new();
    hash.update(b"bbnf.cluster-edge-domain.v1\0");
    hash.update(match decomposition.projection() {
        ProjectionName::Containment => b"containment",
        ProjectionName::RustModule => b"rust_module",
        ProjectionName::CargoDeclared => b"cargo_declared",
        ProjectionName::CargoResolved => b"cargo_resolved",
        ProjectionName::FrontendBuild => b"frontend_build",
        ProjectionName::RuntimeDelivery => b"runtime_delivery",
        ProjectionName::TestOverlay => b"test_overlay",
        ProjectionName::GeneratedProvenance => b"generated_provenance",
        ProjectionName::Ownership => b"ownership",
        ProjectionName::Call => b"call",
        ProjectionName::Documentation => b"documentation",
    });
    hash.update(&(decomposition.edges().len() as u64).to_le_bytes());
    for edge in decomposition.edges() {
        #[cfg(feature = "architecture-test-seams")]
        observe_quotient_domain(QuotientDomainOperation::Scan);
        let identity = edge.identity.as_str().as_bytes();
        hash.update(&(identity.len() as u64).to_le_bytes());
        hash.update(identity);
    }
    Sha256Digest::new(hash.finalize())
}

fn declared_cluster_domain_validation_cost(
    members: usize,
    edges: usize,
    components: usize,
    clusters: usize,
) -> Result<usize> {
    members
        .checked_mul(4)
        .and_then(|cost| {
            edges
                .checked_mul(2)
                .and_then(|value| cost.checked_add(value))
        })
        .and_then(|cost| {
            components
                .checked_mul(3)
                .and_then(|value| cost.checked_add(value))
        })
        .and_then(|cost| {
            clusters
                .checked_mul(3)
                .and_then(|value| cost.checked_add(value))
        })
        .context("RESOURCE_BUDGET_EXCEEDED: cluster domain validation cost overflowed")
}

fn admit_cluster_domain_validation_bytes(
    clusters: &ClusterBuild,
    decomposition: &Decomposition<'_>,
    budget: &mut ProjectionBudget,
) -> Result<usize> {
    let projection = decomposition.projection();
    let fixed = clusters
        .ownership
        .len()
        .checked_mul(std::mem::size_of::<Identity>() * 2)
        .and_then(|bytes| {
            decomposition
                .edges()
                .len()
                .checked_mul(std::mem::size_of::<&EdgeFact>() * 2)
                .and_then(|value| bytes.checked_add(value))
        })
        .and_then(|bytes| {
            decomposition
                .components()
                .len()
                .checked_mul(std::mem::size_of::<ComponentIdentity>())
                .and_then(|value| bytes.checked_add(value))
        })
        .context("RESOURCE_BUDGET_EXCEEDED: cluster domain reservation overflowed")?;
    let ceiling = budget
        .available_bytes(projection)?
        .checked_sub(fixed)
        .context("RESOURCE_BUDGET_EXCEEDED: cluster domain indexes exceed the byte budget")?
        .checked_div(2)
        .context("RESOURCE_BUDGET_EXCEEDED: cluster domain serialized divisor is zero")?;
    let aggregate = crate::architecture::hash::serialized_len_capped(
        &(
            clusters.set(),
            decomposition.components(),
            decomposition.edges(),
        ),
        ceiling,
    )
    .context("RESOURCE_BUDGET_EXCEEDED: cluster domain exceeds its serialized byte ceiling")?;
    let bytes = aggregate
        .checked_mul(2)
        .and_then(|value| value.checked_add(fixed))
        .context("RESOURCE_BUDGET_EXCEEDED: cluster domain reservation overflowed")?;
    budget.reserve(projection, "cluster-domain-validation", bytes)?;
    Ok(bytes)
}

fn admit_cluster_bytes(
    projection: ProjectionName,
    components: &[crate::architecture::project::scc::Component],
    nodes: &BTreeMap<Identity, NodeFact>,
    edges: &[&EdgeFact],
    members: usize,
    budget: &mut ProjectionBudget,
) -> Result<()> {
    let fixed = members
        .checked_mul(std::mem::size_of::<Identity>() * 4)
        .and_then(|bytes| {
            edges
                .len()
                .checked_mul(
                    std::mem::size_of::<&EdgeFact>() + std::mem::size_of::<TypedEdgeRecord>() * 2,
                )
                .and_then(|edge_bytes| bytes.checked_add(edge_bytes))
        })
        .and_then(|bytes| {
            components
                .len()
                .checked_mul(std::mem::size_of::<ComponentIdentity>() * 2)
                .and_then(|component_bytes| bytes.checked_add(component_bytes))
        })
        .context("RESOURCE_BUDGET_EXCEEDED: cluster fixed byte reservation overflowed")?;
    let ceiling = budget
        .available_bytes(projection)?
        .checked_sub(fixed)
        .context("RESOURCE_BUDGET_EXCEEDED: cluster indexes exceed the byte budget")?
        .checked_div(2)
        .context("RESOURCE_BUDGET_EXCEEDED: cluster serialized divisor is zero")?;
    let aggregate = crate::architecture::hash::serialized_len_capped(
        &(components, NodeValues(nodes), edges),
        ceiling,
    )
    .context("RESOURCE_BUDGET_EXCEEDED: cluster input exceeds its serialized byte ceiling")?;
    budget.reserve(
        projection,
        "cluster-domain-and-indexes",
        aggregate
            .checked_mul(2)
            .and_then(|bytes| bytes.checked_add(fixed))
            .context("RESOURCE_BUDGET_EXCEEDED: cluster byte reservation overflowed")?,
    )
}

struct NodeValues<'a>(&'a BTreeMap<Identity, NodeFact>);

impl Serialize for NodeValues<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut sequence = serializer.serialize_seq(Some(self.0.len()))?;
        for node in self.0.values() {
            sequence.serialize_element(node)?;
        }
        sequence.end()
    }
}

fn declared_cluster_cost(nodes: usize, edges: usize, components: usize) -> Result<usize> {
    nodes
        .checked_mul(4)
        .and_then(|cost| {
            edges
                .checked_mul(8)
                .and_then(|edges| cost.checked_add(edges))
        })
        .and_then(|cost| {
            components
                .checked_mul(2)
                .and_then(|items| cost.checked_add(items))
        })
        .ok_or_else(|| {
            anyhow::anyhow!("RESOURCE_BUDGET_EXCEEDED: cluster declared cost overflowed")
        })
}

struct GraphIndex<'a> {
    identities: Vec<Identity>,
    indices: BTreeMap<Identity, usize>,
    edges: Vec<&'a EdgeFact>,
    adjacency: Vec<Vec<usize>>,
    incident: Vec<Vec<usize>>,
}

impl<'a> GraphIndex<'a> {
    fn checked(
        decomposition: &Decomposition<'a>,
        nodes: &BTreeMap<Identity, NodeFact>,
        budget: &mut ProjectionBudget,
    ) -> Result<Self> {
        let projection = decomposition.projection();
        let components = decomposition.components();
        let edges = decomposition.edges();
        if components
            .windows(2)
            .any(|pair| pair[0].identity >= pair[1].identity)
        {
            bail!("CLUSTER_DOMAIN_MISMATCH: components are not strictly canonical");
        }
        let mut members = BTreeSet::new();
        for component in components {
            component
                .identity
                .validate_members(projection, &component.members, budget)?;
            if component.members.is_empty()
                || component.members.windows(2).any(|pair| pair[0] >= pair[1])
            {
                bail!("CLUSTER_DOMAIN_MISMATCH: component members are not canonical");
            }
            for member in &component.members {
                if !nodes.contains_key(member) {
                    bail!("CLUSTER_DOMAIN_MISMATCH: component member has no node fact");
                }
                if !members.insert(member.clone()) {
                    bail!("CLUSTER_DOMAIN_MISMATCH: component membership overlaps");
                }
            }
        }
        if members.len() != nodes.len() || nodes.keys().any(|node| !members.contains(node)) {
            bail!("CLUSTER_DOMAIN_MISMATCH: components do not cover the node domain");
        }
        let identities = members.into_iter().collect::<Vec<_>>();
        let indices = identities
            .iter()
            .cloned()
            .enumerate()
            .map(|(index, identity)| (identity, index))
            .collect::<BTreeMap<_, _>>();
        let mut ordered = edges.to_vec();
        ordered.sort_by(|left, right| left.identity.cmp(&right.identity));
        if ordered
            .windows(2)
            .any(|pair| pair[0].identity == pair[1].identity)
        {
            bail!("CLUSTER_DOMAIN_MISMATCH: duplicate typed edge identity");
        }
        let mut adjacency = vec![Vec::new(); identities.len()];
        let mut incident = vec![Vec::new(); identities.len()];
        for (edge_index, edge) in ordered.iter().enumerate() {
            edge.validate_identity()
                .map_err(|error| anyhow::anyhow!("CLUSTER_DOMAIN_MISMATCH: {error}"))?;
            let from = *indices.get(&edge.record.from).ok_or_else(|| {
                anyhow::anyhow!("CLUSTER_DOMAIN_MISMATCH: edge source escaped the node domain")
            })?;
            let to = *indices.get(&edge.record.to).ok_or_else(|| {
                anyhow::anyhow!("CLUSTER_DOMAIN_MISMATCH: edge target escaped the node domain")
            })?;
            adjacency[from].push(to);
            incident[from].push(edge_index);
            if from != to {
                adjacency[to].push(from);
                incident[to].push(edge_index);
            }
        }
        for neighbors in &mut adjacency {
            neighbors.sort_unstable();
            neighbors.dedup();
        }
        Ok(Self {
            identities,
            indices,
            edges: ordered,
            adjacency,
            incident,
        })
    }

    fn edge(&self, identity: &EdgeIdentity) -> Result<&'a EdgeFact> {
        self.edges
            .binary_search_by(|edge| edge.identity.cmp(identity))
            .map(|index| self.edges[index])
            .map_err(|_| anyhow::anyhow!("CLUSTER_CUT_TOTALITY_MISMATCH: absent typed edge"))
    }

    fn member_indices(&self, members: &[Identity]) -> Result<BTreeSet<usize>> {
        members
            .iter()
            .map(|member| {
                self.indices.get(member).copied().ok_or_else(|| {
                    anyhow::anyhow!("CLUSTER_DOMAIN_MISMATCH: member escaped the node domain")
                })
            })
            .collect()
    }

    fn identities(&self, members: &BTreeSet<usize>) -> Vec<Identity> {
        members
            .iter()
            .map(|index| self.identities[*index].clone())
            .collect()
    }

    fn connected_groups(&self, members: &[Identity]) -> Result<Vec<Vec<Identity>>> {
        let members = self.member_indices(members)?;
        Ok(self
            .connected_index_groups(&members)
            .into_iter()
            .map(|group| self.identities(&group.members))
            .collect())
    }

    fn connected_index_groups(&self, members: &BTreeSet<usize>) -> Vec<IndexedGroup> {
        let mut remaining = members.clone();
        let mut groups = Vec::new();
        while let Some(seed) = remaining.iter().next().copied() {
            let mut group = BTreeSet::from([seed]);
            let mut pending = VecDeque::from([seed]);
            while let Some(node) = pending.pop_front() {
                for next in &self.adjacency[node] {
                    if members.contains(next) && group.insert(*next) {
                        pending.push_back(*next);
                    }
                }
            }
            remaining.retain(|node| !group.contains(node));
            groups.push(IndexedGroup {
                internal_edges: self.internal_edge_count(&group),
                members: group,
            });
        }
        groups.sort_by(|left, right| left.members.cmp(&right.members));
        groups
    }

    fn internal_edge_count(&self, members: &BTreeSet<usize>) -> usize {
        members
            .iter()
            .map(|node| {
                self.incident[*node]
                    .iter()
                    .filter(|edge| {
                        let record = &self.edges[**edge].record;
                        self.indices[&record.from] == *node
                            && members.contains(&self.indices[&record.to])
                    })
                    .count()
            })
            .sum()
    }

    fn edges_after_add(
        &self,
        members: &BTreeSet<usize>,
        internal_edges: usize,
        candidate: usize,
    ) -> usize {
        internal_edges
            + self.incident[candidate]
                .iter()
                .filter(|edge| {
                    let record = &self.edges[**edge].record;
                    let from = self.indices[&record.from];
                    let to = self.indices[&record.to];
                    (from == candidate && (to == candidate || members.contains(&to)))
                        || (to == candidate && members.contains(&from))
                })
                .count()
    }

    fn fits(members: usize, internal_edges: usize, nodes: usize, edges: usize) -> bool {
        members <= nodes && internal_edges <= edges
    }

    fn degrees(&self, members: &BTreeSet<usize>) -> BTreeMap<usize, usize> {
        members
            .iter()
            .copied()
            .map(|node| {
                let degree = self.incident[node]
                    .iter()
                    .map(|edge| &self.edges[*edge].record)
                    .filter(|record| {
                        members.contains(&self.indices[&record.from])
                            && members.contains(&self.indices[&record.to])
                    })
                    .map(|record| usize::from(record.from == record.to) + 1)
                    .sum();
                (node, degree)
            })
            .collect()
    }

    fn adjacent(&self, candidate: usize, shard: &BTreeSet<usize>) -> bool {
        self.adjacency[candidate]
            .iter()
            .any(|neighbor| shard.contains(neighbor))
    }

    fn articulation_points(&self, members: &BTreeSet<usize>) -> BTreeSet<usize> {
        if members.len() < 3 {
            return BTreeSet::new();
        }
        let mut discovered = vec![None; self.identities.len()];
        let mut low = vec![0usize; self.identities.len()];
        let mut parent = vec![None; self.identities.len()];
        let mut children = vec![0usize; self.identities.len()];
        let mut clock = 0usize;
        let mut articulation = BTreeSet::new();
        for root in members {
            if discovered[*root].is_some() {
                continue;
            }
            discovered[*root] = Some(clock);
            low[*root] = clock;
            clock += 1;
            let mut stack = vec![DfsFrame {
                node: *root,
                next_neighbor: 0,
            }];
            while let Some(frame) = stack.last_mut() {
                let node = frame.node;
                let next = loop {
                    let Some(next) = self.adjacency[node].get(frame.next_neighbor).copied() else {
                        break None;
                    };
                    frame.next_neighbor += 1;
                    if members.contains(&next) {
                        break Some(next);
                    }
                };
                if let Some(next) = next {
                    if discovered[next].is_none() {
                        parent[next] = Some(node);
                        children[node] += 1;
                        discovered[next] = Some(clock);
                        low[next] = clock;
                        clock += 1;
                        stack.push(DfsFrame {
                            node: next,
                            next_neighbor: 0,
                        });
                    } else if parent[node] != Some(next) {
                        low[node] = low[node].min(discovered[next].expect("discovered neighbor"));
                    }
                    continue;
                }
                stack.pop();
                if let Some(owner) = parent[node] {
                    low[owner] = low[owner].min(low[node]);
                    if parent[owner].is_some()
                        && low[node] >= discovered[owner].expect("discovered parent")
                    {
                        articulation.insert(owner);
                    }
                } else if children[node] > 1 {
                    articulation.insert(node);
                }
            }
        }
        articulation
    }
}

struct IndexedGroup {
    members: BTreeSet<usize>,
    internal_edges: usize,
}

struct DfsFrame {
    node: usize,
    next_neighbor: usize,
}

fn graph_partition(
    projection: ProjectionName,
    members: &[Identity],
    graph: &GraphIndex<'_>,
    budget: &mut ProjectionBudget,
) -> Result<Vec<Vec<Identity>>> {
    let initial = graph.member_indices(members)?;
    let mut pending = vec![IndexedGroup {
        internal_edges: graph.internal_edge_count(&initial),
        members: initial,
    }];
    let mut output = Vec::new();
    while let Some(group) = pending.pop() {
        budget.charge(
            projection,
            "cluster-partition",
            declared_partition_cost(group.members.len(), group.internal_edges)?,
        )?;
        if GraphIndex::fits(
            group.members.len(),
            group.internal_edges,
            TARGET_NODES,
            TARGET_INTERNAL_EDGES,
        ) {
            output.push(graph.identities(&group.members));
            continue;
        }
        let articulation = graph.articulation_points(&group.members);
        if !articulation.is_empty() {
            output.extend(
                articulation
                    .iter()
                    .map(|cut| vec![graph.identities[*cut].clone()]),
            );
            let mut remainder = group.members;
            remainder.retain(|node| !articulation.contains(node));
            pending.extend(graph.connected_index_groups(&remainder).into_iter().rev());
            continue;
        }
        let degree = graph.degrees(&group.members);
        let seed = group
            .members
            .iter()
            .max_by_key(|node| {
                (
                    degree.get(*node).copied().unwrap_or_default(),
                    std::cmp::Reverse(graph.identities[**node].clone()),
                )
            })
            .copied()
            .ok_or_else(|| {
                anyhow::anyhow!("DYNAMIC_SHARD_PARTITION_ABSENT: empty partition group")
            })?;
        let mut shard = BTreeSet::from([seed]);
        let mut shard_edges = graph.edges_after_add(&BTreeSet::new(), 0, seed);
        while shard.len() < TARGET_NODES {
            let mut candidates = group
                .members
                .difference(&shard)
                .filter(|candidate| graph.adjacent(**candidate, &shard))
                .copied()
                .collect::<Vec<_>>();
            candidates.sort_by_key(|node| {
                (
                    std::cmp::Reverse(degree.get(node).copied().unwrap_or_default()),
                    graph.identities[*node].clone(),
                )
            });
            let Some(candidate) = candidates.first().copied() else {
                break;
            };
            let proposed_edges = graph.edges_after_add(&shard, shard_edges, candidate);
            if !GraphIndex::fits(
                shard.len() + 1,
                proposed_edges,
                TARGET_NODES,
                TARGET_INTERNAL_EDGES,
            ) {
                break;
            }
            shard.insert(candidate);
            shard_edges = proposed_edges;
        }
        if !GraphIndex::fits(shard.len(), shard_edges, MAX_NODES, MAX_INTERNAL_EDGES) {
            bail!("RESOURCE_BUDGET_EXCEEDED: shard exceeds the 32 node or 120 edge hard limit");
        }
        output.push(graph.identities(&shard));
        let remainder = group
            .members
            .difference(&shard)
            .copied()
            .collect::<BTreeSet<_>>();
        pending.extend(graph.connected_index_groups(&remainder).into_iter().rev());
    }
    output.sort();
    Ok(output)
}

fn declared_partition_cost(nodes: usize, edges: usize) -> Result<usize> {
    nodes
        .checked_mul(8)
        .and_then(|cost| {
            edges
                .checked_mul(4)
                .and_then(|edges| cost.checked_add(edges))
        })
        .ok_or_else(|| anyhow::anyhow!("RESOURCE_BUDGET_EXCEEDED: partition cost overflowed"))
}

fn signature(node: &NodeFact) -> Signature {
    let mut signature = Signature {
        ownership: Vec::new(),
        feature: Vec::new(),
        lineage: Vec::new(),
        faces: Vec::new(),
        test: Vec::new(),
        frontend: Vec::new(),
    };
    if matches!(
        node.kind,
        NodeKind::Repository | NodeKind::Product | NodeKind::Worktree
    ) {
        signature
            .ownership
            .push(CanonicalText::new(format!("{:?}", node.kind)).expect("node kind"));
    }
    if let Identity::PackageFace { package_face } = &node.identity {
        signature.faces.push(package_face.clone());
    }
    for attribute in &node.attributes {
        match attribute {
            Attribute::Ownership { owner } => signature.ownership.push(owner.clone()),
            Attribute::Feature { feature } => signature.feature.push(feature.clone()),
            Attribute::GeneratedLineage { lineage } => signature.lineage.push(lineage.clone()),
            Attribute::PackageFace { package_face } => signature.faces.push(package_face.clone()),
            Attribute::TestReachability { reachable } => signature.test.push(*reachable),
            Attribute::FrontendFeature { feature } => signature.frontend.push(feature.clone()),
            _ => {}
        }
    }
    signature
}

fn signals(
    nodes: &[NodeFact],
    members: &[Identity],
    graph: &GraphIndex<'_>,
) -> Result<Vec<ClusterSignal>> {
    let set = graph.member_indices(members)?;
    let articulation = graph.articulation_points(&set);
    let degree = graph.degrees(&set);
    let mut signals = BTreeSet::from([ClusterSignal::Scc]);
    if !articulation.is_empty() {
        signals.insert(ClusterSignal::Articulation);
    }
    if degree.values().copied().max().unwrap_or_default() > 2 {
        signals.insert(ClusterSignal::Fan);
    }
    for node in nodes {
        let signature = signature(node);
        if !signature.ownership.is_empty() {
            signals.insert(ClusterSignal::Ownership);
        }
        if !signature.feature.is_empty() {
            signals.insert(ClusterSignal::Feature);
        }
        if !signature.lineage.is_empty() {
            signals.insert(ClusterSignal::GeneratedLineage);
        }
        if !signature.faces.is_empty() || node.kind == NodeKind::PackageFace {
            signals.insert(ClusterSignal::PackageFace);
        }
        if !signature.test.is_empty() {
            signals.insert(ClusterSignal::TestReachability);
        }
        if !signature.frontend.is_empty() {
            signals.insert(ClusterSignal::FrontendFeature);
        }
    }
    Ok(signals.into_iter().collect())
}

fn cluster_identity(
    projection: ProjectionName,
    component: &ComponentIdentity,
    nodes: &[Identity],
) -> Result<ClusterIdentity> {
    Ok(ClusterIdentity {
        projection,
        digest: Sha256Digest::new(crate::architecture::hash::hash_serialized(&(
            "cluster-v3.1",
            projection,
            component,
            nodes,
        ))?)?,
    })
}
