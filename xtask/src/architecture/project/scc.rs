use std::collections::{BTreeMap, BTreeSet};

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};

use crate::architecture::hash::BoundedKernelError;
use crate::architecture::schema::{EdgeFact, Identity, ProjectionName, Sha256Digest};

const MAX_VERTICES: usize = 200_000;
const MAX_EDGES: usize = 1_000_000;
const MAX_PROJECTION_WORK: usize = 8_000_000;
const MAX_PROJECTION_BYTES: usize = 512 * 1024 * 1024;
const MAX_DOMAIN_BYTES: usize = 256 * 1024 * 1024;

type BoundedSccResult<T> = std::result::Result<T, BoundedKernelError>;

fn scc_failure(code: &'static str, first: usize, second: usize) -> BoundedKernelError {
    BoundedKernelError::Canonical {
        code,
        first,
        second,
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ComponentIdentity {
    pub projection: ProjectionName,
    pub digest: Sha256Digest,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Component {
    pub identity: ComponentIdentity,
    pub members: Vec<Identity>,
    pub cyclic: bool,
}

#[derive(Clone, Debug)]
pub struct ProjectionBudget {
    projection: ProjectionName,
    limit: usize,
    used: usize,
    byte_limit: usize,
    bytes_reserved: usize,
}

impl ProjectionBudget {
    pub fn standard(projection: ProjectionName) -> Self {
        Self {
            projection,
            limit: MAX_PROJECTION_WORK,
            used: 0,
            byte_limit: MAX_PROJECTION_BYTES,
            bytes_reserved: 0,
        }
    }

    #[cfg(feature = "architecture-test-seams")]
    #[doc(hidden)]
    pub fn with_limit(projection: ProjectionName, limit: usize) -> Self {
        Self {
            projection,
            limit,
            used: 0,
            byte_limit: MAX_PROJECTION_BYTES,
            bytes_reserved: 0,
        }
    }

    #[cfg(feature = "architecture-test-seams")]
    #[doc(hidden)]
    pub fn with_limits(projection: ProjectionName, limit: usize, byte_limit: usize) -> Self {
        Self {
            projection,
            limit,
            used: 0,
            byte_limit,
            bytes_reserved: 0,
        }
    }

    pub fn used(&self) -> usize {
        self.used
    }

    pub fn bytes_reserved(&self) -> usize {
        self.bytes_reserved
    }

    pub(crate) fn available_bytes(&self, projection: ProjectionName) -> Result<usize> {
        self.available_bytes_bounded(projection)
            .map_err(anyhow::Error::new)
    }

    pub(crate) fn available_bytes_bounded(
        &self,
        projection: ProjectionName,
    ) -> std::result::Result<usize, crate::architecture::hash::BoundedKernelError> {
        if self.projection != projection {
            return Err(crate::architecture::hash::BoundedKernelError::Canonical {
                code: "PROJECTION_BUDGET_MISMATCH: bytes",
                first: projection as usize,
                second: self.projection as usize,
            });
        }
        self.byte_limit.checked_sub(self.bytes_reserved).ok_or(
            crate::architecture::hash::BoundedKernelError::Arithmetic(
                "projection byte budget is overdrawn",
            ),
        )
    }

    pub(crate) fn charge(
        &mut self,
        projection: ProjectionName,
        owner: &'static str,
        amount: usize,
    ) -> Result<()> {
        self.charge_bounded(projection, owner, amount)
            .map_err(anyhow::Error::new)
    }

    pub(crate) fn charge_bounded(
        &mut self,
        projection: ProjectionName,
        owner: &'static str,
        amount: usize,
    ) -> std::result::Result<(), crate::architecture::hash::BoundedKernelError> {
        if self.projection != projection {
            return Err(crate::architecture::hash::BoundedKernelError::Canonical {
                code: "PROJECTION_BUDGET_MISMATCH: work",
                first: projection as usize,
                second: self.projection as usize,
            });
        }
        let next = self.used.checked_add(amount).ok_or(
            crate::architecture::hash::BoundedKernelError::Arithmetic(
                "projection work counter overflowed",
            ),
        )?;
        if next > self.limit {
            return Err(crate::architecture::hash::BoundedKernelError::Budget {
                owner,
                amount,
                limit: self.limit,
                used: self.used,
            });
        }
        self.used = next;
        Ok(())
    }

    pub(crate) fn reserve(
        &mut self,
        projection: ProjectionName,
        owner: &'static str,
        bytes: usize,
    ) -> Result<()> {
        self.reserve_bounded(projection, owner, bytes)
            .map_err(anyhow::Error::new)
    }

    pub(crate) fn reserve_bounded(
        &mut self,
        projection: ProjectionName,
        owner: &'static str,
        bytes: usize,
    ) -> std::result::Result<(), crate::architecture::hash::BoundedKernelError> {
        if self.projection != projection {
            return Err(crate::architecture::hash::BoundedKernelError::Canonical {
                code: "PROJECTION_BUDGET_MISMATCH: reserve",
                first: projection as usize,
                second: self.projection as usize,
            });
        }
        let next = self.bytes_reserved.checked_add(bytes).ok_or(
            crate::architecture::hash::BoundedKernelError::Arithmetic(
                "projection byte reservation overflowed",
            ),
        )?;
        if next > self.byte_limit {
            return Err(crate::architecture::hash::BoundedKernelError::Budget {
                owner,
                amount: bytes,
                limit: self.byte_limit,
                used: self.bytes_reserved,
            });
        }
        self.bytes_reserved = next;
        Ok(())
    }
}

impl crate::architecture::hash::JsonCharge for ProjectionBudget {
    fn charge_json(
        &mut self,
        owner: &'static str,
        amount: usize,
    ) -> std::result::Result<(), crate::architecture::hash::BoundedKernelError> {
        self.charge_bounded(self.projection, owner, amount)
    }

    fn available_json_bytes(
        &self,
    ) -> std::result::Result<usize, crate::architecture::hash::BoundedKernelError> {
        self.available_bytes_bounded(self.projection)
    }
}

impl ComponentIdentity {
    pub(crate) fn validate_members(
        &self,
        projection: ProjectionName,
        members: &[Identity],
        budget: &mut ProjectionBudget,
    ) -> Result<()> {
        self.validate_members_bounded(projection, members, budget)
            .map_err(anyhow::Error::new)
    }

    pub(crate) fn validate_members_bounded(
        &self,
        projection: ProjectionName,
        members: &[Identity],
        budget: &mut ProjectionBudget,
    ) -> std::result::Result<(), crate::architecture::hash::BoundedKernelError> {
        self.validate_projection_bounded(projection)?;
        budget.reserve_bounded(projection, "component-member-digest", 64)?;
        let digest = crate::architecture::hash::execute_component_member_hash(
            projection,
            members,
            budget,
            "component-member-hash",
        )?;
        self.validate_digest_bounded(&digest)
    }

    pub(crate) fn validate_member_set_bounded(
        &self,
        projection: ProjectionName,
        members: &BTreeSet<Identity>,
        budget: &mut ProjectionBudget,
    ) -> std::result::Result<(), crate::architecture::hash::BoundedKernelError> {
        self.validate_projection_bounded(projection)?;
        budget.reserve_bounded(projection, "component-member-digest", 64)?;
        let digest = crate::architecture::hash::execute_component_member_set_hash(
            projection,
            members,
            budget,
            "component-member-hash",
        )?;
        self.validate_digest_bounded(&digest)
    }

    pub(crate) fn validate_member_refs_bounded(
        &self,
        projection: ProjectionName,
        members: &[&Identity],
        budget: &mut ProjectionBudget,
    ) -> std::result::Result<(), crate::architecture::hash::BoundedKernelError> {
        self.validate_projection_bounded(projection)?;
        budget.reserve_bounded(projection, "component-member-digest", 64)?;
        let digest = crate::architecture::hash::execute_component_member_refs_hash(
            projection,
            members,
            budget,
            "component-member-hash",
        )?;
        self.validate_digest_bounded(&digest)
    }

    fn validate_projection_bounded(
        &self,
        projection: ProjectionName,
    ) -> std::result::Result<(), crate::architecture::hash::BoundedKernelError> {
        if self.projection != projection {
            return Err(crate::architecture::hash::BoundedKernelError::Canonical {
                code: "SCC_TOTALITY_MISMATCH: component projection differs",
                first: self.projection as usize,
                second: projection as usize,
            });
        }
        Ok(())
    }

    fn validate_digest_bounded(
        &self,
        digest: &str,
    ) -> std::result::Result<(), crate::architecture::hash::BoundedKernelError> {
        if self.digest.as_str() != digest {
            return Err(crate::architecture::hash::BoundedKernelError::Canonical {
                code: "SCC_TOTALITY_MISMATCH: component digest does not bind its members",
                first: 0,
                second: 0,
            });
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Decomposition<'a> {
    projection: ProjectionName,
    components: Vec<Component>,
    ownership: BTreeMap<Identity, usize>,
    internal_edges: Vec<Vec<&'a EdgeFact>>,
    edges: Vec<&'a EdgeFact>,
}

impl<'a> Decomposition<'a> {
    pub fn components(&self) -> &[Component] {
        &self.components
    }

    pub fn internal_edges(&self, identity: &ComponentIdentity) -> Result<&[&'a EdgeFact]> {
        let index = self
            .components
            .binary_search_by(|component| component.identity.cmp(identity))
            .map_err(|_| anyhow::anyhow!("SCC_TOTALITY_MISMATCH: component identity is absent"))?;
        Ok(&self.internal_edges[index])
    }

    pub(crate) fn component_edges(&self) -> impl Iterator<Item = (&Component, &[&'a EdgeFact])> {
        self.components
            .iter()
            .zip(&self.internal_edges)
            .map(|(component, edges)| (component, edges.as_slice()))
    }

    pub(crate) fn edges(&self) -> &[&'a EdgeFact] {
        &self.edges
    }

    pub fn into_components(self) -> Vec<Component> {
        self.components
    }

    pub fn projection(&self) -> ProjectionName {
        self.projection
    }

    pub fn owner(&self, member: &Identity) -> Option<&Component> {
        self.ownership
            .get(member)
            .map(|index| &self.components[*index])
    }
}

pub fn decompose<'a>(
    projection: ProjectionName,
    vertices: &BTreeSet<Identity>,
    edges: &'a [EdgeFact],
    budget: &mut ProjectionBudget,
) -> Result<Decomposition<'a>> {
    validate_counts(vertices.len(), edges.len())?;
    admit_scc_bytes(projection, vertices, edges.len(), budget, |budget| {
        crate::architecture::hash::execute_owned_edge_domain_json(
            vertices,
            edges,
            budget,
            "scc-edge-record-hash",
            "scc-serialization-domain",
        )
    })?;
    #[cfg(feature = "architecture-test-seams")]
    if !vertices.is_empty() || !edges.is_empty() {
        observe_scc_allocation();
    }
    validate_domain(vertices, edges)?;
    let refs = edges.iter().collect::<Vec<_>>();
    Ok(decompose_refs_checked(projection, vertices, &refs, budget)?)
}

pub fn decompose_refs<'a>(
    projection: ProjectionName,
    vertices: &BTreeSet<Identity>,
    edges: &[&'a EdgeFact],
    budget: &mut ProjectionBudget,
) -> BoundedSccResult<Decomposition<'a>> {
    validate_counts_bounded(vertices.len(), edges.len())?;
    admit_scc_bytes_bounded(projection, vertices, edges.len(), budget, |budget| {
        crate::architecture::hash::execute_borrowed_edge_domain_json(
            vertices,
            edges,
            budget,
            "scc-edge-record-hash",
            "scc-serialization-domain",
        )
    })?;
    #[cfg(feature = "architecture-test-seams")]
    if !vertices.is_empty() || !edges.is_empty() {
        observe_scc_allocation();
    }
    validate_domain_bounded(vertices, edges.iter().copied())?;
    decompose_refs_checked(projection, vertices, edges, budget)
}

fn decompose_refs_checked<'a>(
    projection: ProjectionName,
    vertices: &BTreeSet<Identity>,
    edges: &[&'a EdgeFact],
    budget: &mut ProjectionBudget,
) -> BoundedSccResult<Decomposition<'a>> {
    let mut adjacency: BTreeMap<Identity, BTreeSet<Identity>> = BTreeMap::new();
    let mut reverse: BTreeMap<Identity, BTreeSet<Identity>> = BTreeMap::new();
    for edge in edges {
        adjacency
            .entry(edge.record.from.clone())
            .or_default()
            .insert(edge.record.to.clone());
        reverse
            .entry(edge.record.to.clone())
            .or_default()
            .insert(edge.record.from.clone());
    }
    let mut order = Vec::with_capacity(vertices.len());
    let mut visited = BTreeSet::new();
    for vertex in vertices {
        visit_iterative(vertex, &adjacency, &mut visited, &mut order);
    }
    visited.clear();
    let mut groups = Vec::new();
    while let Some(vertex) = order.pop() {
        if visited.contains(&vertex) {
            continue;
        }
        let mut members = Vec::new();
        collect_iterative(&vertex, &reverse, &mut visited, &mut members);
        members.sort();
        groups.push(members);
    }
    groups.sort();

    let self_loops = edges
        .iter()
        .filter(|edge| edge.record.from == edge.record.to)
        .map(|edge| &edge.record.from)
        .collect::<BTreeSet<_>>();
    let mut components = groups
        .into_iter()
        .map(|members| {
            let cyclic = members.len() > 1 || self_loops.contains(&members[0]);
            #[cfg(feature = "architecture-test-seams")]
            crate::architecture::project::cycle::observe_cycle_storage(
                crate::architecture::project::cycle::CycleStorageOperation::WitnessPrechargeAttempt,
            );
            if let Err(error) =
                budget.reserve_bounded(projection, "component-generation-digest", 64)
            {
                #[cfg(feature = "architecture-test-seams")]
                crate::architecture::project::cycle::observe_cycle_storage(
                    crate::architecture::project::cycle::CycleStorageOperation::WitnessPrechargeFailure,
                );
                return Err(error);
            }
            let digest = Sha256Digest::from_bounded_hex(
                crate::architecture::hash::execute_component_member_hash(
                    projection,
                    &members,
                    budget,
                    "component-generation-member-hash",
                )?,
            )?;
            Ok(Component {
                identity: ComponentIdentity { projection, digest },
                members,
                cyclic,
            })
        })
        .collect::<BoundedSccResult<Vec<_>>>()?;
    components.sort_by(|left, right| left.identity.cmp(&right.identity));

    let mut ownership = BTreeMap::new();
    for (index, component) in components.iter().enumerate() {
        for member in &component.members {
            if ownership.insert(member.clone(), index).is_some() {
                return Err(scc_failure(
                    "SCC_TOTALITY_MISMATCH: component membership overlaps",
                    index,
                    0,
                ));
            }
        }
    }
    if ownership.len() != vertices.len() {
        return Err(scc_failure(
            "SCC_TOTALITY_MISMATCH: component membership is incomplete",
            ownership.len(),
            vertices.len(),
        ));
    }
    let mut internal_edges = vec![Vec::new(); components.len()];
    for edge in edges {
        let from = ownership.get(&edge.record.from).ok_or_else(|| {
            scc_failure(
                "SCC_DOMAIN_MISMATCH: edge source escaped component ownership",
                0,
                0,
            )
        })?;
        let to = ownership.get(&edge.record.to).ok_or_else(|| {
            scc_failure(
                "SCC_DOMAIN_MISMATCH: edge target escaped component ownership",
                0,
                0,
            )
        })?;
        if from == to {
            internal_edges[*from].push(*edge);
        }
    }
    for bucket in &mut internal_edges {
        bucket.sort_by(|left, right| left.identity.cmp(&right.identity));
    }
    let mut all_edges = edges.to_vec();
    all_edges.sort_by(|left, right| left.identity.cmp(&right.identity));
    Ok(Decomposition {
        projection,
        components,
        ownership,
        internal_edges,
        edges: all_edges,
    })
}

fn validate_counts(vertices: usize, edges: usize) -> Result<()> {
    if vertices > MAX_VERTICES || edges > MAX_EDGES {
        bail!(
            "RESOURCE_BUDGET_EXCEEDED: SCC input has {vertices} vertices and {edges} edges; limits are {MAX_VERTICES} and {MAX_EDGES}"
        );
    }
    Ok(())
}

fn validate_counts_bounded(vertices: usize, edges: usize) -> BoundedSccResult<()> {
    if vertices > MAX_VERTICES || edges > MAX_EDGES {
        return Err(BoundedKernelError::Budget {
            owner: "SCC input counts",
            amount: vertices.max(edges),
            limit: MAX_VERTICES.max(MAX_EDGES),
            used: 0,
        });
    }
    Ok(())
}

fn admit_scc_bytes_bounded(
    projection: ProjectionName,
    vertices: &BTreeSet<Identity>,
    edge_count: usize,
    budget: &mut ProjectionBudget,
    execute_json: impl FnOnce(
        &mut ProjectionBudget,
    ) -> BoundedSccResult<crate::architecture::hash::EdgeDomainJsonMetrics>,
) -> BoundedSccResult<()> {
    let declared_work = declared_scc_cost_bounded(vertices.len(), edge_count)?;
    let phase_work = vertices
        .len()
        .checked_add(edge_count)
        .and_then(|sizing| sizing.checked_add(declared_work))
        .ok_or(BoundedKernelError::Arithmetic("SCC phase cost overflowed"))?;
    budget.charge_bounded(projection, "scc-serialization-domain-index", phase_work)?;
    let fixed = declared_scc_fixed_bytes_bounded(vertices.len(), edge_count)?;
    budget.reserve_bounded(projection, "scc-fixed-indexes", fixed)?;
    let metrics = execute_json(budget)?;
    #[cfg(feature = "architecture-test-seams")]
    crate::architecture::evidence::validate::observe_validation(
        crate::architecture::evidence::validate::ValidationOperation::PhasePrecharged(
            crate::architecture::evidence::validate::ValidationPhase::SccSerializationIndex,
        ),
    );
    #[cfg(feature = "architecture-test-seams")]
    crate::architecture::evidence::validate::observe_validation(
        crate::architecture::evidence::validate::ValidationOperation::PhaseVisit(
            crate::architecture::evidence::validate::ValidationPhase::SccSerializationIndex,
        ),
    );
    if metrics.length > MAX_DOMAIN_BYTES {
        return Err(BoundedKernelError::OutputCeiling {
            ceiling: MAX_DOMAIN_BYTES,
        });
    }
    budget.reserve_bounded(
        projection,
        "scc-domain-and-indexes",
        metrics
            .length
            .checked_mul(4)
            .ok_or(BoundedKernelError::Arithmetic(
                "SCC byte reservation overflowed",
            ))?,
    )?;
    if let Some(index) = metrics.invalid_edge {
        return Err(scc_failure(
            "SCC_DOMAIN_MISMATCH: typed edge identity is invalid",
            index,
            0,
        ));
    }
    Ok(())
}

fn admit_scc_bytes(
    projection: ProjectionName,
    vertices: &BTreeSet<Identity>,
    edge_count: usize,
    budget: &mut ProjectionBudget,
    execute_json: impl FnOnce(
        &mut ProjectionBudget,
    ) -> Result<crate::architecture::hash::EdgeDomainJsonMetrics>,
) -> Result<()> {
    let declared_work = declared_scc_cost(vertices.len(), edge_count)?;
    let phase_work = vertices
        .len()
        .checked_add(edge_count)
        .and_then(|sizing| sizing.checked_add(declared_work))
        .context("RESOURCE_BUDGET_EXCEEDED: SCC phase cost overflowed")?;
    budget.charge(projection, "scc-serialization-domain-index", phase_work)?;
    let fixed = declared_scc_fixed_bytes(vertices.len(), edge_count)?;
    budget.reserve(projection, "scc-fixed-indexes", fixed)?;
    let metrics = execute_json(budget)?;
    #[cfg(feature = "architecture-test-seams")]
    crate::architecture::evidence::validate::observe_validation(
        crate::architecture::evidence::validate::ValidationOperation::PhasePrecharged(
            crate::architecture::evidence::validate::ValidationPhase::SccSerializationIndex,
        ),
    );
    #[cfg(feature = "architecture-test-seams")]
    crate::architecture::evidence::validate::observe_validation(
        crate::architecture::evidence::validate::ValidationOperation::PhaseVisit(
            crate::architecture::evidence::validate::ValidationPhase::SccSerializationIndex,
        ),
    );
    let bytes = metrics.length;
    if bytes > MAX_DOMAIN_BYTES {
        bail!("RESOURCE_BUDGET_EXCEEDED: SCC input exceeds its serialized byte ceiling");
    }
    budget.reserve(
        projection,
        "scc-domain-and-indexes",
        bytes
            .checked_mul(4)
            .context("RESOURCE_BUDGET_EXCEEDED: SCC byte reservation overflowed")?,
    )?;
    if metrics.invalid_edge.is_some() {
        bail!(
            "SCC_DOMAIN_MISMATCH: typed edge identity is invalid: edge identity does not bind its complete record"
        );
    }
    Ok(())
}

fn allocation_bytes<T>(count: usize) -> Result<usize> {
    count
        .checked_mul(std::mem::size_of::<T>())
        .context("RESOURCE_BUDGET_EXCEEDED: allocation byte count overflowed")
}

fn allocation_bytes_bounded<T>(count: usize) -> BoundedSccResult<usize> {
    count
        .checked_mul(std::mem::size_of::<T>())
        .ok_or(BoundedKernelError::Arithmetic(
            "SCC allocation byte count overflowed",
        ))
}

fn declared_scc_fixed_bytes(vertices: usize, edges: usize) -> Result<usize> {
    allocation_bytes::<Identity>(vertices)
        .and_then(|bytes| {
            bytes
                .checked_mul(8)
                .context("RESOURCE_BUDGET_EXCEEDED: SCC vertex indexes overflowed")
        })
        .and_then(|bytes| {
            allocation_bytes::<&EdgeFact>(edges)?
                .checked_mul(6)
                .and_then(|fixed| bytes.checked_add(fixed))
                .context("RESOURCE_BUDGET_EXCEEDED: SCC edge indexes overflowed")
        })
        .context("RESOURCE_BUDGET_EXCEEDED: SCC byte reservation overflowed")
}

fn declared_scc_fixed_bytes_bounded(vertices: usize, edges: usize) -> BoundedSccResult<usize> {
    allocation_bytes_bounded::<Identity>(vertices)
        .and_then(|bytes| {
            bytes.checked_mul(8).ok_or(BoundedKernelError::Arithmetic(
                "SCC vertex indexes overflowed",
            ))
        })
        .and_then(|bytes| {
            allocation_bytes_bounded::<&EdgeFact>(edges)?
                .checked_mul(6)
                .and_then(|fixed| bytes.checked_add(fixed))
                .ok_or(BoundedKernelError::Arithmetic(
                    "SCC edge indexes overflowed",
                ))
        })
}

pub fn strongly_connected(
    projection: ProjectionName,
    vertices: &BTreeSet<Identity>,
    edges: &[EdgeFact],
    budget: &mut ProjectionBudget,
) -> Result<Vec<Component>> {
    Ok(decompose(projection, vertices, edges, budget)?.into_components())
}

fn validate_domain<'a>(
    vertices: &BTreeSet<Identity>,
    edges: impl IntoIterator<Item = &'a EdgeFact>,
) -> Result<()> {
    let mut identities = BTreeSet::new();
    for edge in edges {
        #[cfg(feature = "architecture-test-seams")]
        crate::architecture::evidence::validate::observe_validation(
            crate::architecture::evidence::validate::ValidationOperation::PhaseVisit(
                crate::architecture::evidence::validate::ValidationPhase::SccSerializationIndex,
            ),
        );
        if !vertices.contains(&edge.record.from) || !vertices.contains(&edge.record.to) {
            bail!("SCC_DOMAIN_MISMATCH: edge endpoint escaped the declared vertex set");
        }
        if !identities.insert(&edge.identity) {
            bail!("SCC_DOMAIN_MISMATCH: duplicate typed edge identity");
        }
    }
    Ok(())
}

fn validate_domain_bounded<'a>(
    vertices: &BTreeSet<Identity>,
    edges: impl IntoIterator<Item = &'a EdgeFact>,
) -> BoundedSccResult<()> {
    let mut identities = BTreeSet::new();
    for (index, edge) in edges.into_iter().enumerate() {
        #[cfg(feature = "architecture-test-seams")]
        crate::architecture::evidence::validate::observe_validation(
            crate::architecture::evidence::validate::ValidationOperation::PhaseVisit(
                crate::architecture::evidence::validate::ValidationPhase::SccSerializationIndex,
            ),
        );
        if !vertices.contains(&edge.record.from) || !vertices.contains(&edge.record.to) {
            return Err(scc_failure(
                "SCC_DOMAIN_MISMATCH: edge endpoint escaped the declared vertex set",
                index,
                0,
            ));
        }
        if !identities.insert(&edge.identity) {
            return Err(scc_failure(
                "SCC_DOMAIN_MISMATCH: duplicate typed edge identity",
                index,
                0,
            ));
        }
    }
    Ok(())
}

#[cfg(feature = "architecture-test-seams")]
fn observe_scc_allocation() {
    crate::architecture::evidence::validate::observe_validation(
        crate::architecture::evidence::validate::ValidationOperation::PhaseAllocation(
            crate::architecture::evidence::validate::ValidationPhase::SccSerializationIndex,
        ),
    );
}

fn declared_scc_cost(vertices: usize, edges: usize) -> Result<usize> {
    vertices
        .checked_mul(4)
        .and_then(|cost| {
            edges
                .checked_mul(5)
                .and_then(|edges| cost.checked_add(edges))
        })
        .context("RESOURCE_BUDGET_EXCEEDED: SCC declared cost overflowed")
}

fn declared_scc_cost_bounded(vertices: usize, edges: usize) -> BoundedSccResult<usize> {
    vertices
        .checked_mul(4)
        .and_then(|cost| {
            edges
                .checked_mul(5)
                .and_then(|edges| cost.checked_add(edges))
        })
        .ok_or(BoundedKernelError::Arithmetic(
            "SCC declared cost overflowed",
        ))
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn validate_scc_serialization_for_test(
    projection: ProjectionName,
    vertices: &BTreeSet<Identity>,
    edges: &[EdgeFact],
    work_limit: usize,
) -> Result<usize> {
    let mut budget = ProjectionBudget::with_limits(projection, work_limit, usize::MAX);
    admit_scc_bytes(projection, vertices, edges.len(), &mut budget, |budget| {
        crate::architecture::hash::execute_owned_edge_domain_json(
            vertices,
            edges,
            budget,
            "scc-edge-record-hash",
            "scc-serialization-domain",
        )
    })?;
    Ok(budget.used())
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn validate_component_members_for_test(
    identity: &ComponentIdentity,
    projection: ProjectionName,
    members: &[Identity],
    work_limit: usize,
) -> Result<usize> {
    let mut budget = ProjectionBudget::with_limits(projection, work_limit, usize::MAX);
    identity.validate_members(projection, members, &mut budget)?;
    Ok(budget.used())
}

fn visit_iterative(
    node: &Identity,
    adjacency: &BTreeMap<Identity, BTreeSet<Identity>>,
    visited: &mut BTreeSet<Identity>,
    order: &mut Vec<Identity>,
) {
    let mut work = vec![(node.clone(), false)];
    while let Some((current, expanded)) = work.pop() {
        if expanded {
            order.push(current);
            continue;
        }
        if !visited.insert(current.clone()) {
            continue;
        }
        work.push((current.clone(), true));
        if let Some(children) = adjacency.get(&current) {
            for child in children.iter().rev() {
                work.push((child.clone(), false));
            }
        }
    }
}

fn collect_iterative(
    node: &Identity,
    reverse: &BTreeMap<Identity, BTreeSet<Identity>>,
    visited: &mut BTreeSet<Identity>,
    members: &mut Vec<Identity>,
) {
    let mut work = vec![node.clone()];
    while let Some(current) = work.pop() {
        if !visited.insert(current.clone()) {
            continue;
        }
        members.push(current.clone());
        if let Some(children) = reverse.get(&current) {
            for child in children.iter().rev() {
                work.push(child.clone());
            }
        }
    }
}
