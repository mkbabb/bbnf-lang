use std::collections::BTreeSet;

#[cfg(feature = "architecture-test-seams")]
use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::architecture::hash::BoundedKernelError;
use crate::architecture::project::scc::{
    ComponentIdentity, Decomposition, ProjectionBudget, decompose_refs,
};
use crate::architecture::schema::{EdgeFact, EdgeIdentity, Identity, ProjectionName};

const MAX_COMPONENT_NODES: usize = 4_096;
const MAX_COMPONENT_EDGES: usize = 8_192;
const MAX_SEARCH_DEPTH: usize = 4_096;
const MAX_CYCLE_DOMAIN_BYTES: usize = 64 * 1024 * 1024;

type BoundedCycleResult<T> = std::result::Result<T, BoundedKernelError>;

fn cycle_failure(code: &'static str, first: usize, second: usize) -> BoundedKernelError {
    BoundedKernelError::Canonical {
        code,
        first,
        second,
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct CycleWitness {
    pub edges: Vec<EdgeIdentity>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ComponentBasis {
    pub component: ComponentIdentity,
    pub rank: usize,
    pub cycles: Vec<CycleWitness>,
}

#[derive(Clone, Copy)]
enum CycleMembers<'m> {
    Set(&'m BTreeSet<Identity>),
    Refs(&'m [&'m Identity]),
}

impl CycleMembers<'_> {
    fn len(self) -> usize {
        match self {
            Self::Set(members) => members.len(),
            Self::Refs(members) => members.len(),
        }
    }

    fn contains(self, identity: &Identity) -> bool {
        match self {
            Self::Set(members) => members.contains(identity),
            Self::Refs(members) => members.binary_search(&identity).is_ok(),
        }
    }
}

struct EdgeDomain<'a, 'm> {
    projection: ProjectionName,
    members: CycleMembers<'m>,
    ordered: Vec<&'a EdgeFact>,
}

impl<'a, 'm> EdgeDomain<'a, 'm> {
    fn checked(
        projection: ProjectionName,
        members: &'m BTreeSet<Identity>,
        edges: &[&'a EdgeFact],
        budget: &mut ProjectionBudget,
        owner: &'static str,
    ) -> BoundedCycleResult<Self> {
        Self::checked_members(projection, CycleMembers::Set(members), edges, budget, owner)
    }

    fn checked_refs(
        projection: ProjectionName,
        members: &'m [&'m Identity],
        edges: &[&'a EdgeFact],
        budget: &mut ProjectionBudget,
        owner: &'static str,
    ) -> BoundedCycleResult<Self> {
        Self::checked_members(
            projection,
            CycleMembers::Refs(members),
            edges,
            budget,
            owner,
        )
    }

    fn checked_members(
        projection: ProjectionName,
        members: CycleMembers<'m>,
        edges: &[&'a EdgeFact],
        budget: &mut ProjectionBudget,
        owner: &'static str,
    ) -> BoundedCycleResult<Self> {
        if members.len() > MAX_COMPONENT_NODES || edges.len() > MAX_COMPONENT_EDGES {
            return Err(BoundedKernelError::Budget {
                owner,
                amount: members.len().max(edges.len()),
                limit: MAX_COMPONENT_NODES.max(MAX_COMPONENT_EDGES),
                used: 0,
            });
        }
        let phase_work = members
            .len()
            .checked_add(
                edges
                    .len()
                    .checked_mul(3)
                    .ok_or(BoundedKernelError::Arithmetic(
                        "cycle-domain work overflowed",
                    ))?,
            )
            .ok_or(BoundedKernelError::Arithmetic(
                "cycle-domain work overflowed",
            ))?;
        budget.charge_bounded(projection, "cycle-domain-materialization", phase_work)?;
        let fixed = declared_domain_fixed_bytes(members.len(), edges.len())?;
        budget.reserve_bounded(projection, "cycle-domain-fixed-indexes", fixed)?;
        let json = match members {
            CycleMembers::Set(members) => {
                crate::architecture::hash::execute_borrowed_edge_domain_json(
                    members,
                    edges,
                    budget,
                    "cycle-edge-record-hash",
                    "cycle-domain-serialized-preflight",
                )?
            }
            CycleMembers::Refs(members) => {
                crate::architecture::hash::execute_borrowed_member_edge_domain_json(
                    members,
                    edges,
                    budget,
                    "cycle-edge-record-hash",
                    "cycle-domain-serialized-preflight",
                )?
            }
        };
        #[cfg(feature = "architecture-test-seams")]
        observe_phase_precharged(ValidationPhase::CycleDomain);
        #[cfg(feature = "architecture-test-seams")]
        observe_phase_visit(ValidationPhase::CycleDomain);
        let aggregate = json.length;
        if aggregate > MAX_CYCLE_DOMAIN_BYTES {
            return Err(BoundedKernelError::OutputCeiling {
                ceiling: MAX_CYCLE_DOMAIN_BYTES,
            });
        }
        budget.reserve_bounded(
            projection,
            owner,
            aggregate
                .checked_mul(2)
                .ok_or(BoundedKernelError::Arithmetic(
                    "cycle-domain byte reservation overflowed",
                ))?,
        )?;
        for edge in edges {
            #[cfg(feature = "architecture-test-seams")]
            observe_phase_visit(ValidationPhase::CycleDomain);
            if !members.contains(&edge.record.from) || !members.contains(&edge.record.to) {
                return Err(cycle_failure("CYCLE_DOMAIN_MISMATCH: edge endpoint", 0, 0));
            }
        }
        if let Some(index) = json.invalid_edge {
            return Err(cycle_failure(
                "CYCLE_DOMAIN_MISMATCH: typed edge identity is invalid",
                index,
                0,
            ));
        }
        #[cfg(feature = "architecture-test-seams")]
        observe_phase_allocation(ValidationPhase::CycleDomain);
        let mut ordered = edges.to_vec();
        ordered.sort_by(|left, right| left.identity.cmp(&right.identity));
        if ordered
            .windows(2)
            .any(|pair| pair[0].identity == pair[1].identity)
        {
            return Err(cycle_failure(
                "CYCLE_DOMAIN_MISMATCH: duplicate typed edge identity",
                0,
                0,
            ));
        }
        Ok(Self {
            projection,
            members,
            ordered,
        })
    }

    fn edge_index(&self, identity: &EdgeIdentity) -> BoundedCycleResult<usize> {
        self.ordered
            .binary_search_by(|edge| edge.identity.cmp(identity))
            .map_err(|_| cycle_failure("CYCLE_BASIS_INCOMPLETE: absent typed edge", 0, 0))
    }
}

fn validate_component_counts(
    nodes: usize,
    edges: usize,
    owner: &'static str,
) -> BoundedCycleResult<()> {
    if nodes > MAX_COMPONENT_NODES || edges > MAX_COMPONENT_EDGES {
        return Err(BoundedKernelError::Budget {
            owner,
            amount: nodes.max(edges),
            limit: MAX_COMPONENT_NODES.max(MAX_COMPONENT_EDGES),
            used: 0,
        });
    }
    Ok(())
}

fn directed_rank(
    domain: &EdgeDomain<'_, '_>,
    budget: &mut ProjectionBudget,
) -> BoundedCycleResult<usize> {
    budget.charge_bounded(
        domain.projection,
        "directed-rank",
        declared_rank_cost(domain.members.len(), domain.ordered.len())?,
    )?;
    match domain.members {
        CycleMembers::Set(members) => {
            let decomposition =
                decompose_refs(domain.projection, members, &domain.ordered, budget)?;
            decomposition
                .component_edges()
                .filter(|(component, _)| component.cyclic)
                .try_fold(0usize, |rank, (component, internal)| {
                    let component_rank = internal
                        .len()
                        .checked_add(1)
                        .and_then(|sum| sum.checked_sub(component.members.len()))
                        .ok_or_else(|| {
                            cycle_failure(
                                "CYCLE_DOMAIN_MISMATCH: invalid directed component rank",
                                internal.len(),
                                component.members.len(),
                            )
                        })?;
                    rank.checked_add(component_rank)
                        .ok_or(BoundedKernelError::Arithmetic("directed rank overflowed"))
                })
        }
        CycleMembers::Refs(members) => domain
            .ordered
            .len()
            .checked_add(1)
            .and_then(|sum| sum.checked_sub(members.len()))
            .ok_or_else(|| {
                cycle_failure(
                    "CYCLE_DOMAIN_MISMATCH: invalid directed component rank",
                    domain.ordered.len(),
                    members.len(),
                )
            }),
    }
}

pub fn component_bases(
    decomposition: &Decomposition<'_>,
    budget: &mut ProjectionBudget,
) -> BoundedCycleResult<Vec<ComponentBasis>> {
    let basis_capacity = decomposition.components().len();
    if basis_capacity == 0 {
        return Ok(Vec::new());
    }
    budget.charge_bounded(
        decomposition.projection(),
        "cycle-basis-sizing",
        basis_capacity,
    )?;
    budget.reserve_bounded(
        decomposition.projection(),
        "cycle-basis-allocation",
        allocation_bytes::<ComponentBasis>(basis_capacity)?,
    )?;
    #[cfg(feature = "architecture-test-seams")]
    observe_phase_precharged(ValidationPhase::CycleBasis);
    #[cfg(feature = "architecture-test-seams")]
    observe_phase_allocation(ValidationPhase::CycleBasis);
    let mut bases = Vec::with_capacity(basis_capacity);
    for (component, internal) in decomposition
        .component_edges()
        .filter(|(component, _)| component.cyclic)
    {
        let member_count = component.members.len();
        validate_component_counts(member_count, internal.len(), "cycle-component-domain")?;
        budget.charge_bounded(
            decomposition.projection(),
            "cycle-member-materialization",
            member_count,
        )?;
        let member_bytes = allocation_bytes::<&Identity>(member_count)?;
        #[cfg(feature = "architecture-test-seams")]
        observe_cycle_storage(CycleStorageOperation::MemberPrechargeAttempt);
        if let Err(error) = budget.reserve_bounded(
            decomposition.projection(),
            "cycle-member-materialization",
            member_bytes,
        ) {
            #[cfg(feature = "architecture-test-seams")]
            observe_cycle_storage(CycleStorageOperation::MemberPrechargeFailure);
            return Err(error);
        }
        #[cfg(feature = "architecture-test-seams")]
        observe_phase_precharged(ValidationPhase::CycleMember);
        #[cfg(feature = "architecture-test-seams")]
        observe_cycle_storage(CycleStorageOperation::MemberPrecharged);
        let mut members = Vec::with_capacity(member_count);
        #[cfg(feature = "architecture-test-seams")]
        observe_cycle_storage(CycleStorageOperation::MemberAllocation);
        for member in &component.members {
            #[cfg(feature = "architecture-test-seams")]
            observe_phase_visit(ValidationPhase::CycleMember);
            #[cfg(feature = "architecture-test-seams")]
            observe_cycle_storage(CycleStorageOperation::MemberVisit);
            members.push(member);
        }
        #[cfg(feature = "architecture-test-seams")]
        observe_phase_allocation(ValidationPhase::CycleMember);
        #[cfg(feature = "architecture-test-seams")]
        observe_cycle_storage(CycleStorageOperation::MemberRetainedComplete);
        let domain = EdgeDomain::checked_refs(
            decomposition.projection(),
            &members,
            internal,
            budget,
            "cycle-component-domain",
        )?;
        let rank = directed_rank(&domain, budget)?;
        budget.charge_bounded(
            decomposition.projection(),
            "cycle",
            declared_cycle_cost(member_count, internal.len(), rank)?,
        )?;
        component.identity.validate_member_refs_bounded(
            decomposition.projection(),
            &members,
            budget,
        )?;
        let cycles = independent_directed_cycles(&domain, rank, budget)?;
        let basis = ComponentBasis {
            component: component.identity.clone(),
            rank,
            cycles,
        };
        validate_basis_domain(&domain, &basis, budget)?;
        bases.push(basis);
    }
    bases.sort_by(|left, right| left.component.cmp(&right.component));
    Ok(bases)
}

pub fn basis_for_subgraph(
    projection: ProjectionName,
    members: &BTreeSet<Identity>,
    edges: &[&EdgeFact],
    budget: &mut ProjectionBudget,
) -> BoundedCycleResult<(usize, Vec<CycleWitness>)> {
    let domain = EdgeDomain::checked(projection, members, edges, budget, "cycle-subgraph-domain")?;
    let rank = directed_rank(&domain, budget)?;
    budget.charge_bounded(
        projection,
        "cycle",
        declared_cycle_cost(members.len(), edges.len(), rank)?,
    )?;
    Ok((rank, independent_directed_cycles(&domain, rank, budget)?))
}

pub fn complement_basis(
    members: &BTreeSet<Identity>,
    edges: &[&EdgeFact],
    seed: &[CycleWitness],
    complete: &ComponentBasis,
    budget: &mut ProjectionBudget,
) -> BoundedCycleResult<(usize, Vec<CycleWitness>)> {
    let domain = EdgeDomain::checked(
        complete.component.projection,
        members,
        edges,
        budget,
        "cycle-complement-domain",
    )?;
    budget.charge_bounded(
        complete.component.projection,
        "cycle-complement",
        declared_completion_cost(edges.len(), seed.len(), complete.rank)?,
    )?;
    complete.component.validate_member_set_bounded(
        complete.component.projection,
        members,
        budget,
    )?;
    validate_basis_domain(&domain, complete, budget)?;
    let linear_rows =
        seed.len()
            .checked_add(complete.cycles.len())
            .ok_or(BoundedKernelError::Arithmetic(
                "cycle-complement row count overflowed",
            ))?;
    charge_linear_work(
        budget,
        complete.component.projection,
        "cycle-complement-algebra",
        domain.ordered.len(),
        linear_rows,
        seed.iter()
            .chain(&complete.cycles)
            .map(|witness| witness.edges.len()),
        true,
    )?;
    let mut linear = LinearBasis::new(domain.ordered.len());
    let mut witnessed = vec![0u8; domain.ordered.len().div_ceil(8)];
    for witness in seed {
        validate_witness(&domain, witness, &mut witnessed)?;
        witness_vector(witness, &domain, &mut linear)?;
        linear.insert_candidate();
    }
    let seed_rank = linear.rank();
    if seed_rank > complete.rank {
        return Err(cycle_failure(
            "CYCLE_BASIS_INCOMPLETE: shard bases exceed component rank",
            seed_rank,
            complete.rank,
        ));
    }
    let target = complete.rank - seed_rank;
    let mut selected = [0usize; MAX_COMPONENT_EDGES];
    let mut selected_count = 0usize;
    for (index, witness) in complete.cycles.iter().enumerate() {
        witness_vector(witness, &domain, &mut linear)?;
        if linear.insert_candidate() {
            selected[selected_count] = index;
            selected_count += 1;
            if selected_count == target {
                break;
            }
        }
    }
    let selected = &selected[..selected_count];
    let output_bytes = complement_storage_bytes(&complete.cycles, selected)?;
    #[cfg(feature = "architecture-test-seams")]
    observe_cycle_storage(CycleStorageOperation::ComplementPrechargeAttempt);
    if let Err(error) = budget.reserve_bounded(
        complete.component.projection,
        "cycle-complement-output",
        output_bytes,
    ) {
        #[cfg(feature = "architecture-test-seams")]
        observe_cycle_storage(CycleStorageOperation::ComplementPrechargeFailure);
        return Err(error);
    }
    #[cfg(feature = "architecture-test-seams")]
    observe_cycle_storage(CycleStorageOperation::ComplementPrecharged);
    let mut complement = Vec::with_capacity(selected.len());
    #[cfg(feature = "architecture-test-seams")]
    observe_cycle_storage(CycleStorageOperation::ComplementAllocation);
    for index in selected {
        #[cfg(feature = "architecture-test-seams")]
        observe_cycle_storage(CycleStorageOperation::ComplementClone);
        complement.push(complete.cycles[*index].clone());
    }
    #[cfg(feature = "architecture-test-seams")]
    observe_cycle_storage(CycleStorageOperation::ComplementRetainedComplete);
    validate_cycle_completion(
        complete.component.projection,
        members,
        edges,
        seed,
        &complement,
        target,
        budget,
    )?;
    Ok((target, complement))
}

fn complement_storage_bytes(
    cycles: &[CycleWitness],
    selected: &[usize],
) -> BoundedCycleResult<usize> {
    selected.iter().try_fold(
        allocation_bytes::<CycleWitness>(selected.len())?,
        |bytes, index| {
            let witness = cycles.get(*index).ok_or_else(|| {
                cycle_failure("CYCLE_BASIS_INCOMPLETE: absent complement index", *index, 0)
            })?;
            let identities = allocation_bytes::<EdgeIdentity>(witness.edges.len())?;
            let strings = witness.edges.iter().try_fold(0usize, |sum, edge| {
                sum.checked_add(edge.as_str().len())
                    .ok_or(BoundedKernelError::Arithmetic(
                        "complement identity bytes overflowed",
                    ))
            })?;
            bytes
                .checked_add(identities)
                .and_then(|bytes| bytes.checked_add(strings))
                .ok_or(BoundedKernelError::Arithmetic(
                    "complement storage overflowed",
                ))
        },
    )
}

pub fn validate_cycle_completion(
    projection: ProjectionName,
    members: &BTreeSet<Identity>,
    edges: &[&EdgeFact],
    seed: &[CycleWitness],
    complement: &[CycleWitness],
    declared_rank: usize,
    budget: &mut ProjectionBudget,
) -> BoundedCycleResult<()> {
    let domain = EdgeDomain::checked(
        projection,
        members,
        edges,
        budget,
        "cycle-validation-domain",
    )?;
    validate_cycle_completion_domain(&domain, seed, complement, declared_rank, budget)
}

pub(crate) fn validate_cycle_completion_refs(
    projection: ProjectionName,
    members: &[&Identity],
    edges: &[&EdgeFact],
    seed: &[CycleWitness],
    complement: &[CycleWitness],
    declared_rank: usize,
    budget: &mut ProjectionBudget,
) -> BoundedCycleResult<()> {
    let domain = EdgeDomain::checked_refs(
        projection,
        members,
        edges,
        budget,
        "cycle-validation-domain",
    )?;
    validate_cycle_completion_domain(&domain, seed, complement, declared_rank, budget)
}

fn validate_cycle_completion_domain(
    domain: &EdgeDomain<'_, '_>,
    seed: &[CycleWitness],
    complement: &[CycleWitness],
    declared_rank: usize,
    budget: &mut ProjectionBudget,
) -> BoundedCycleResult<()> {
    let projection = domain.projection;
    budget.charge_bounded(
        projection,
        "cycle-validation",
        declared_completion_cost(domain.ordered.len(), seed.len(), declared_rank)?,
    )?;
    let linear_rows =
        seed.len()
            .checked_add(complement.len())
            .ok_or(BoundedKernelError::Arithmetic(
                "cycle-validation row count overflowed",
            ))?;
    charge_linear_work(
        budget,
        projection,
        "cycle-validation-algebra",
        domain.ordered.len(),
        linear_rows,
        seed.iter()
            .chain(complement)
            .map(|witness| witness.edges.len()),
        true,
    )?;
    let mut linear = LinearBasis::new(domain.ordered.len());
    let mut witnessed = vec![0u8; domain.ordered.len().div_ceil(8)];
    for witness in seed {
        validate_witness(domain, witness, &mut witnessed)?;
        witness_vector(witness, domain, &mut linear)?;
        linear.insert_candidate();
    }
    let seed_rank = linear.rank();
    for witness in complement {
        validate_witness(domain, witness, &mut witnessed)?;
        witness_vector(witness, domain, &mut linear)?;
        if !linear.insert_candidate() {
            return Err(cycle_failure(
                "CYCLE_BASIS_INCOMPLETE: quotient witness is dependent",
                linear.rank(),
                declared_rank,
            ));
        }
    }
    let expected_total = directed_rank(domain, budget)?;
    let expected_complement = expected_total.checked_sub(seed_rank).ok_or_else(|| {
        cycle_failure(
            "CYCLE_BASIS_INCOMPLETE: shard rank exceeds component rank",
            seed_rank,
            expected_total,
        )
    })?;
    if declared_rank != expected_complement
        || complement.len() != expected_complement
        || linear.rank() != expected_total
    {
        return Err(cycle_failure(
            "CYCLE_BASIS_INCOMPLETE: quotient evidence does not complete original graph rank",
            declared_rank,
            expected_complement,
        ));
    }
    Ok(())
}

pub fn directed_cycle_rank(
    projection: ProjectionName,
    members: &BTreeSet<Identity>,
    edges: &[&EdgeFact],
    budget: &mut ProjectionBudget,
) -> BoundedCycleResult<usize> {
    let domain = EdgeDomain::checked(projection, members, edges, budget, "directed-rank-domain")?;
    directed_rank(&domain, budget)
}

pub fn validate_basis(
    members: &BTreeSet<Identity>,
    edges: &[&EdgeFact],
    basis: &ComponentBasis,
    budget: &mut ProjectionBudget,
) -> BoundedCycleResult<()> {
    let domain = EdgeDomain::checked(
        basis.component.projection,
        members,
        edges,
        budget,
        "cycle-basis-domain",
    )?;
    validate_basis_domain(&domain, basis, budget)
}

fn validate_basis_domain(
    domain: &EdgeDomain<'_, '_>,
    basis: &ComponentBasis,
    budget: &mut ProjectionBudget,
) -> BoundedCycleResult<()> {
    admit_member_hash(domain, budget)?;
    match domain.members {
        CycleMembers::Set(members) => {
            basis
                .component
                .validate_member_set_bounded(domain.projection, members, budget)?;
        }
        CycleMembers::Refs(members) => {
            basis
                .component
                .validate_member_refs_bounded(domain.projection, members, budget)?;
        }
    }
    #[cfg(feature = "architecture-test-seams")]
    observe_cycle_storage(CycleStorageOperation::WitnessPrechargeAttempt);
    if let Err(error) = charge_linear_work(
        budget,
        domain.projection,
        "cycle-basis-validation",
        domain.ordered.len(),
        basis.cycles.len(),
        basis.cycles.iter().map(|witness| witness.edges.len()),
        true,
    ) {
        #[cfg(feature = "architecture-test-seams")]
        observe_cycle_storage(CycleStorageOperation::WitnessPrechargeFailure);
        return Err(error);
    }
    let expected = directed_rank(domain, budget)?;
    #[cfg(feature = "architecture-test-seams")]
    observe_cycle_storage(CycleStorageOperation::WitnessPrecharged);
    let mut vectors = LinearBasis::new(domain.ordered.len());
    let mut witnessed = vec![0u8; domain.ordered.len().div_ceil(8)];
    #[cfg(feature = "architecture-test-seams")]
    observe_cycle_storage(CycleStorageOperation::WitnessAllocation);
    for witness in &basis.cycles {
        #[cfg(feature = "architecture-test-seams")]
        observe_cycle_storage(CycleStorageOperation::WitnessVisit);
        validate_witness(domain, witness, &mut witnessed)?;
        witness_vector(witness, domain, &mut vectors)?;
        if !vectors.insert_candidate() {
            #[cfg(feature = "architecture-test-seams")]
            observe_cycle_storage(CycleStorageOperation::WitnessFailureAttempt);
            let error = cycle_failure(
                "CYCLE_BASIS_INCOMPLETE: witnesses are linearly dependent",
                vectors.rank(),
                basis.cycles.len(),
            );
            #[cfg(feature = "architecture-test-seams")]
            observe_cycle_storage(CycleStorageOperation::WitnessFailure);
            return Err(error);
        }
    }
    if basis.rank != expected || basis.cycles.len() != expected || vectors.rank() != expected {
        return Err(cycle_failure(
            "CYCLE_BASIS_INCOMPLETE: basis rank differs",
            basis.rank,
            expected,
        ));
    }
    #[cfg(feature = "architecture-test-seams")]
    observe_cycle_storage(CycleStorageOperation::WitnessRetainedComplete);
    Ok(())
}

fn validate_witness(
    domain: &EdgeDomain<'_, '_>,
    witness: &CycleWitness,
    witnessed: &mut [u8],
) -> BoundedCycleResult<()> {
    if witness.edges.is_empty() {
        return Err(cycle_failure(
            "CYCLE_BASIS_INCOMPLETE: empty cycle witness",
            0,
            0,
        ));
    }
    if witnessed.len() != domain.ordered.len().div_ceil(8) {
        return Err(BoundedKernelError::Arithmetic(
            "witness scratch bitset has the wrong size",
        ));
    }
    witnessed.fill(0);
    let mut first = None;
    let mut previous = None;
    for edge_id in &witness.edges {
        let index = domain.edge_index(edge_id)?;
        let mask = 1u8 << (index % 8);
        if witnessed[index / 8] & mask != 0 {
            #[cfg(feature = "architecture-test-seams")]
            observe_cycle_storage(CycleStorageOperation::WitnessFailureAttempt);
            let error = cycle_failure(
                "CYCLE_BASIS_INCOMPLETE: witness repeats a typed edge",
                index,
                witness.edges.len(),
            );
            #[cfg(feature = "architecture-test-seams")]
            observe_cycle_storage(CycleStorageOperation::WitnessFailure);
            return Err(error);
        }
        witnessed[index / 8] |= mask;
        let edge = domain.ordered[index];
        if let Some(target) = previous
            && target != &edge.record.from
        {
            return Err(cycle_failure(
                "CYCLE_BASIS_INCOMPLETE: witness is not head-to-tail replayable",
                index,
                0,
            ));
        }
        first.get_or_insert(&edge.record.from);
        previous = Some(&edge.record.to);
    }
    if first != previous {
        return Err(cycle_failure(
            "CYCLE_BASIS_INCOMPLETE: witness does not close",
            witness.edges.len(),
            0,
        ));
    }
    Ok(())
}

fn witness_vector(
    witness: &CycleWitness,
    domain: &EdgeDomain<'_, '_>,
    linear: &mut LinearBasis,
) -> BoundedCycleResult<()> {
    linear.clear_candidate();
    for edge in &witness.edges {
        let index = domain.edge_index(edge)?;
        linear.candidate_mut()[index / 64] ^= 1u64 << (index % 64);
    }
    Ok(())
}

fn independent_directed_cycles(
    domain: &EdgeDomain<'_, '_>,
    target: usize,
    budget: &mut ProjectionBudget,
) -> BoundedCycleResult<Vec<CycleWitness>> {
    if target == 0 {
        return Ok(Vec::new());
    }
    admit_basis_search_bytes(domain, target, budget)?;
    let outgoing = PackedAdjacency::new(domain)?;
    let mut linear = LinearBasis::new(domain.ordered.len());
    let mut witnesses = Vec::with_capacity(target);
    let mut visited = vec![0u8; domain.members.len()];
    let mut path = Vec::with_capacity(domain.members.len().saturating_add(1));
    let mut frames = Vec::with_capacity(domain.members.len());
    #[cfg(feature = "architecture-test-seams")]
    observe_cycle_storage(CycleStorageOperation::RowAllocation);
    for start in 0..domain.ordered.len() {
        if witnesses.len() == target {
            break;
        }
        let edge = domain.ordered[start];
        path.clear();
        path.push(start);
        if edge.record.from == edge.record.to {
            consider(
                domain.projection,
                &domain.ordered,
                &path,
                &mut linear,
                &mut witnesses,
                budget,
            )?;
            continue;
        }
        visited.fill(0);
        let from = outgoing.member_index(&edge.record.from)?;
        let to = outgoing.member_index(&edge.record.to)?;
        visited[from] = 1;
        visited[to] = 1;
        frames.clear();
        enumerate_iterative(
            domain.projection,
            to,
            from,
            &outgoing,
            &domain.ordered,
            &mut visited,
            &mut path,
            &mut frames,
            &mut linear,
            &mut witnesses,
            target,
            budget,
            MAX_SEARCH_DEPTH,
        )?;
    }
    if witnesses.len() != target {
        return Err(cycle_failure(
            "CYCLE_BASIS_INCOMPLETE: independent cycle rank differs",
            witnesses.len(),
            target,
        ));
    }
    #[cfg(feature = "architecture-test-seams")]
    observe_cycle_storage(CycleStorageOperation::RowTransientPeak);
    linear.release_candidate();
    #[cfg(feature = "architecture-test-seams")]
    observe_cycle_storage(CycleStorageOperation::RowRetainedComplete);
    #[cfg(feature = "architecture-test-seams")]
    observe_phase_allocation(ValidationPhase::CycleBasis);
    Ok(witnesses)
}

struct PackedAdjacency<'a> {
    members: Vec<&'a Identity>,
    offsets: Vec<usize>,
    edge_indices: Vec<usize>,
}

impl<'a> PackedAdjacency<'a> {
    fn new(domain: &'a EdgeDomain<'_, '_>) -> BoundedCycleResult<Self> {
        let members = match domain.members {
            CycleMembers::Set(members) => members.iter().collect::<Vec<_>>(),
            CycleMembers::Refs(members) => members.to_vec(),
        };
        let mut offsets = vec![0usize; members.len().saturating_add(1)];
        for edge in &domain.ordered {
            let source = members
                .binary_search(&&edge.record.from)
                .map_err(|_| cycle_failure("CYCLE_DOMAIN_MISMATCH: absent edge source", 0, 0))?;
            offsets[source + 1] =
                offsets[source + 1]
                    .checked_add(1)
                    .ok_or(BoundedKernelError::Arithmetic(
                        "packed adjacency count overflowed",
                    ))?;
        }
        for index in 1..offsets.len() {
            offsets[index] = offsets[index].checked_add(offsets[index - 1]).ok_or(
                BoundedKernelError::Arithmetic("packed adjacency offset overflowed"),
            )?;
        }
        let mut cursors = offsets[..members.len()].to_vec();
        let mut edge_indices = vec![0usize; domain.ordered.len()];
        for (edge_index, edge) in domain.ordered.iter().enumerate() {
            let source = members
                .binary_search(&&edge.record.from)
                .map_err(|_| cycle_failure("CYCLE_DOMAIN_MISMATCH: absent edge source", 0, 0))?;
            edge_indices[cursors[source]] = edge_index;
            cursors[source] += 1;
        }
        Ok(Self {
            members,
            offsets,
            edge_indices,
        })
    }

    fn member_index(&self, member: &Identity) -> BoundedCycleResult<usize> {
        self.members
            .binary_search(&member)
            .map_err(|_| cycle_failure("CYCLE_DOMAIN_MISMATCH: absent cycle member", 0, 0))
    }

    fn edges_from(&self, member: usize) -> &[usize] {
        &self.edge_indices[self.offsets[member]..self.offsets[member + 1]]
    }
}

struct SearchFrame {
    current: usize,
    next_candidate: usize,
    inserted_node: Option<usize>,
}

#[allow(clippy::too_many_arguments)]
fn enumerate_iterative(
    projection: ProjectionName,
    current: usize,
    target_node: usize,
    outgoing: &PackedAdjacency<'_>,
    edges: &[&EdgeFact],
    visited: &mut [u8],
    path: &mut Vec<usize>,
    frames: &mut Vec<SearchFrame>,
    linear: &mut LinearBasis,
    witnesses: &mut Vec<CycleWitness>,
    target_rank: usize,
    budget: &mut ProjectionBudget,
    max_depth: usize,
) -> BoundedCycleResult<()> {
    frames.push(SearchFrame {
        current,
        next_candidate: 0,
        inserted_node: None,
    });
    while witnesses.len() != target_rank {
        let Some(frame) = frames.last_mut() else {
            break;
        };
        let candidates = outgoing.edges_from(frame.current);
        let Some(index) = candidates.get(frame.next_candidate).copied() else {
            let frame = frames.pop().expect("active search frame");
            if let Some(node) = frame.inserted_node {
                visited[node] = 0;
                path.pop();
            }
            continue;
        };
        frame.next_candidate += 1;
        budget.charge_bounded(projection, "cycle-search", 1)?;
        let edge = edges[index];
        let target = outgoing.member_index(&edge.record.to)?;
        path.push(index);
        if path.len() > max_depth {
            return Err(BoundedKernelError::Budget {
                owner: "directed-cycle search depth",
                amount: path.len(),
                limit: max_depth,
                used: path.len().saturating_sub(1),
            });
        }
        if target == target_node {
            consider(projection, edges, path, linear, witnesses, budget)?;
            path.pop();
        } else if visited[target] == 0 {
            visited[target] = 1;
            frames.push(SearchFrame {
                current: target,
                next_candidate: 0,
                inserted_node: Some(target),
            });
        } else {
            path.pop();
        }
    }
    Ok(())
}

fn consider(
    projection: ProjectionName,
    edges: &[&EdgeFact],
    path: &[usize],
    linear: &mut LinearBasis,
    witnesses: &mut Vec<CycleWitness>,
    budget: &mut ProjectionBudget,
) -> BoundedCycleResult<()> {
    charge_linear_work(
        budget,
        projection,
        "cycle-witness",
        edges.len(),
        1,
        std::iter::once(path.len()),
        false,
    )?;
    #[cfg(feature = "architecture-test-seams")]
    observe_cycle_storage(CycleStorageOperation::RowVisit);
    linear.clear_candidate();
    for index in path {
        linear.candidate_mut()[*index / 64] ^= 1u64 << (*index % 64);
    }
    if linear.insert_candidate() {
        let ids = canonical_cycle(
            path.iter()
                .map(|index| edges[*index].identity.clone())
                .collect(),
        );
        witnesses.push(CycleWitness { edges: ids });
    }
    Ok(())
}

fn canonical_cycle(mut edges: Vec<EdgeIdentity>) -> Vec<EdgeIdentity> {
    if let Some((offset, _)) = edges.iter().enumerate().min_by_key(|(_, edge)| *edge) {
        edges.rotate_left(offset);
    }
    edges
}

fn declared_cycle_cost(nodes: usize, edges: usize, rank: usize) -> BoundedCycleResult<usize> {
    let setup = nodes
        .checked_mul(2)
        .and_then(|nodes| {
            edges
                .checked_mul(3)
                .and_then(|edges| nodes.checked_add(edges))
        })
        .ok_or(BoundedKernelError::Arithmetic(
            "cycle setup cost overflowed",
        ))?;
    let words = edges.div_ceil(64);
    let elimination_rows = rank
        .checked_add(1)
        .ok_or(BoundedKernelError::Arithmetic("cycle row count overflowed"))?;
    let algebra = rank
        .checked_mul(elimination_rows)
        .and_then(|cost| cost.checked_mul(words))
        .ok_or(BoundedKernelError::Arithmetic(
            "cycle algebra cost overflowed",
        ))?;
    let witnesses = rank
        .checked_mul(nodes.min(edges))
        .ok_or(BoundedKernelError::Arithmetic(
            "cycle witness cost overflowed",
        ))?;
    setup
        .checked_add(algebra)
        .and_then(|cost| cost.checked_add(witnesses))
        .ok_or(BoundedKernelError::Arithmetic(
            "cycle declared cost overflowed",
        ))
}

fn declared_completion_cost(edges: usize, seed: usize, rank: usize) -> BoundedCycleResult<usize> {
    let words = edges.div_ceil(64);
    seed.checked_add(rank)
        .and_then(|rows| rows.checked_add(1).and_then(|next| rows.checked_mul(next)))
        .and_then(|work| work.checked_mul(words))
        .and_then(|work| work.checked_add(edges))
        .ok_or(BoundedKernelError::Arithmetic(
            "cycle-complement declared cost overflowed",
        ))
}

fn declared_rank_cost(nodes: usize, edges: usize) -> BoundedCycleResult<usize> {
    nodes
        .checked_mul(3)
        .and_then(|cost| {
            edges
                .checked_mul(3)
                .and_then(|edges| cost.checked_add(edges))
        })
        .ok_or(BoundedKernelError::Arithmetic(
            "cycle-rank declared cost overflowed",
        ))
}

fn declared_domain_fixed_bytes(nodes: usize, edges: usize) -> BoundedCycleResult<usize> {
    nodes
        .checked_mul(std::mem::size_of::<Identity>())
        .and_then(|bytes| bytes.checked_mul(2))
        .and_then(|bytes| {
            edges
                .checked_mul(
                    std::mem::size_of::<&EdgeFact>() + std::mem::size_of::<(EdgeIdentity, usize)>(),
                )
                .and_then(|fixed| bytes.checked_add(fixed))
        })
        .ok_or(BoundedKernelError::Arithmetic(
            "cycle-domain byte reservation overflowed",
        ))
}

fn charge_linear_work(
    budget: &mut ProjectionBudget,
    projection: ProjectionName,
    owner: &'static str,
    columns: usize,
    declared_rows: usize,
    mut witness_lengths: impl Iterator<Item = usize>,
    reserve_storage: bool,
) -> BoundedCycleResult<(usize, usize)> {
    budget.charge_bounded(projection, "cycle-linear-sizing", declared_rows)?;
    let sizing_prefix = budget.used();
    #[cfg(feature = "architecture-test-seams")]
    observe_phase_precharged(ValidationPhase::CycleLinearSizing);
    let (rows, witness_edges) =
        witness_lengths.try_fold((0usize, 0usize), |(rows, edges), length| {
            #[cfg(feature = "architecture-test-seams")]
            observe_phase_visit(ValidationPhase::CycleLinearSizing);
            Ok::<_, BoundedKernelError>((
                rows.checked_add(1)
                    .ok_or(BoundedKernelError::Arithmetic("cycle row count overflowed"))?,
                edges
                    .checked_add(length)
                    .ok_or(BoundedKernelError::Arithmetic(
                        "cycle witness length overflowed",
                    ))?,
            ))
        })?;
    if rows != declared_rows {
        return Err(cycle_failure(
            "CYCLE_BASIS_INCOMPLETE: declared linear row count differs",
            rows,
            declared_rows,
        ));
    }
    let words = columns.div_ceil(64);
    let elimination_rows = rows
        .checked_add(1)
        .ok_or(BoundedKernelError::Arithmetic("GF2 row count overflowed"))?;
    let algebra = rows
        .checked_mul(elimination_rows)
        .and_then(|work| work.checked_mul(words))
        .ok_or(BoundedKernelError::Arithmetic("GF2 work overflowed"))?;
    let allocation = rows
        .checked_mul(words)
        .and_then(|work| work.checked_add(witness_edges))
        .and_then(|work| work.checked_add(columns))
        .ok_or(BoundedKernelError::Arithmetic(
            "cycle allocation cost overflowed",
        ))?;
    let allocation_bytes = rows
        .checked_add(1)
        .and_then(|rows| rows.checked_mul(words))
        .and_then(|words| words.checked_mul(std::mem::size_of::<u64>()))
        .and_then(|bytes| {
            columns
                .checked_mul(std::mem::size_of::<Option<Box<[u64]>>>())
                .and_then(|slots| bytes.checked_add(slots))
        })
        .and_then(|bytes| {
            witness_edges
                .checked_mul(std::mem::size_of::<EdgeIdentity>())
                .and_then(|witnesses| bytes.checked_add(witnesses))
        })
        .and_then(|bytes| bytes.checked_add(columns.div_ceil(8)))
        .ok_or(BoundedKernelError::Arithmetic(
            "cycle linear byte reservation overflowed",
        ))?;
    budget.charge_bounded(
        projection,
        owner,
        algebra
            .checked_add(allocation)
            .ok_or(BoundedKernelError::Arithmetic(
                "cycle linear work overflowed",
            ))?,
    )?;
    let allocation_prefix = budget.used();
    if reserve_storage {
        budget.reserve_bounded(projection, owner, allocation_bytes)?;
    }
    #[cfg(feature = "architecture-test-seams")]
    if reserve_storage {
        observe_phase_precharged(ValidationPhase::CycleLinearAllocation);
    }
    #[cfg(feature = "architecture-test-seams")]
    if reserve_storage {
        observe_phase_allocation(ValidationPhase::CycleLinearAllocation);
    }
    Ok((sizing_prefix, allocation_prefix))
}

fn allocation_bytes<T>(count: usize) -> BoundedCycleResult<usize> {
    count
        .checked_mul(std::mem::size_of::<T>())
        .ok_or(BoundedKernelError::Arithmetic(
            "cycle allocation byte count overflowed",
        ))
}

fn admit_member_hash(
    domain: &EdgeDomain<'_, '_>,
    budget: &mut ProjectionBudget,
) -> BoundedCycleResult<()> {
    budget.charge_bounded(
        domain.projection,
        "cycle-member-hash-sizing",
        domain.members.len(),
    )?;
    budget.reserve_bounded(
        domain.projection,
        "cycle-member-vector",
        allocation_bytes::<&Identity>(domain.members.len())?,
    )?;
    let serialized = match domain.members {
        CycleMembers::Set(members) => crate::architecture::hash::execute_cycle_member_measure(
            domain.projection,
            members,
            budget,
            "cycle-member-hash-preflight",
        )?,
        CycleMembers::Refs(members) => {
            crate::architecture::hash::execute_cycle_member_refs_measure(
                domain.projection,
                members,
                budget,
                "cycle-member-hash-preflight",
            )?
        }
    };
    #[cfg(feature = "architecture-test-seams")]
    observe_phase_precharged(ValidationPhase::CycleMemberHash);
    for _ in 0..domain.members.len() {
        #[cfg(feature = "architecture-test-seams")]
        observe_phase_visit(ValidationPhase::CycleMemberHash);
    }
    budget.reserve_bounded(domain.projection, "cycle-member-hash", serialized)?;
    let member_view = match domain.members {
        CycleMembers::Set(members) => members.iter().collect::<Vec<_>>(),
        CycleMembers::Refs(members) => members.to_vec(),
    };
    #[cfg(feature = "architecture-test-seams")]
    observe_phase_allocation(ValidationPhase::CycleMemberHash);
    drop(member_view);
    Ok(())
}

fn admit_basis_search_bytes(
    domain: &EdgeDomain<'_, '_>,
    target: usize,
    budget: &mut ProjectionBudget,
) -> BoundedCycleResult<()> {
    let bytes = basis_search_storage_bytes(domain.members.len(), domain.ordered.len(), target)?;
    #[cfg(feature = "architecture-test-seams")]
    observe_cycle_storage(CycleStorageOperation::RowPrechargeAttempt);
    if let Err(error) = budget.reserve_bounded(domain.projection, "cycle-basis-search", bytes) {
        #[cfg(feature = "architecture-test-seams")]
        observe_cycle_storage(CycleStorageOperation::RowPrechargeFailure);
        return Err(error);
    }
    #[cfg(feature = "architecture-test-seams")]
    observe_cycle_storage(CycleStorageOperation::RowPrecharged);
    #[cfg(feature = "architecture-test-seams")]
    observe_phase_precharged(ValidationPhase::CycleBasis);
    Ok(())
}

fn basis_search_storage_bytes(
    nodes: usize,
    edges: usize,
    target: usize,
) -> BoundedCycleResult<usize> {
    let words = edges.div_ceil(64);
    let nodes_plus_one = nodes.checked_add(1).ok_or(BoundedKernelError::Arithmetic(
        "cycle node storage overflowed",
    ))?;
    let witness_edges =
        target
            .checked_mul(nodes.min(edges))
            .ok_or(BoundedKernelError::Arithmetic(
                "cycle witness edge bound overflowed",
            ))?;
    let row_words = target
        .checked_add(1)
        .and_then(|rows| rows.checked_mul(words))
        .ok_or(BoundedKernelError::Arithmetic(
            "cycle basis row storage overflowed",
        ))?;
    let mut bytes = 0usize;
    for allocation in [
        allocation_bytes::<&Identity>(nodes)?,
        allocation_bytes::<usize>(nodes_plus_one)?,
        allocation_bytes::<usize>(edges)?,
        allocation_bytes::<usize>(nodes)?,
        allocation_bytes::<u8>(nodes)?,
        allocation_bytes::<usize>(nodes_plus_one)?,
        allocation_bytes::<SearchFrame>(nodes)?,
        allocation_bytes::<CycleWitness>(target)?,
        allocation_bytes::<Option<Box<[u64]>>>(edges)?,
        allocation_bytes::<u64>(row_words)?,
        allocation_bytes::<EdgeIdentity>(witness_edges)?,
        witness_edges
            .checked_mul(b"edge:sha256:".len() + 64)
            .ok_or(BoundedKernelError::Arithmetic(
                "cycle witness payload overflowed",
            ))?,
    ] {
        bytes = bytes
            .checked_add(allocation)
            .ok_or(BoundedKernelError::Arithmetic(
                "cycle basis allocation overflowed",
            ))?;
    }
    Ok(bytes)
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn cycle_basis_fresh_prefix_for_test(
    projection: ProjectionName,
    members: &BTreeSet<Identity>,
    edges: &[&EdgeFact],
) -> Result<usize> {
    let mut budget = ProjectionBudget::with_limits(projection, usize::MAX, usize::MAX);
    let domain = EdgeDomain::checked(projection, members, edges, &mut budget, "cycle-test-prefix")?;
    directed_rank(&domain, &mut budget)?;
    Ok(budget.bytes_reserved())
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn validate_cycle_domain_for_test(
    projection: ProjectionName,
    members: &BTreeSet<Identity>,
    edges: &[&EdgeFact],
    work_limit: usize,
) -> Result<usize> {
    let mut budget = ProjectionBudget::with_limits(projection, work_limit, usize::MAX);
    EdgeDomain::checked(projection, members, edges, &mut budget, "cycle-test-domain")?;
    Ok(budget.used())
}

#[cfg(feature = "architecture-test-seams")]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[doc(hidden)]
pub enum CycleStorageOperation {
    WitnessPrecharged,
    WitnessVisit,
    WitnessAllocation,
    WitnessRetainedComplete,
    ComplementPrecharged,
    ComplementClone,
    ComplementAllocation,
    ComplementRetainedComplete,
    MemberPrecharged,
    MemberVisit,
    MemberAllocation,
    MemberRetainedComplete,
    RowPrecharged,
    RowVisit,
    RowAllocation,
    RowRetainedComplete,
    WitnessPrechargeAttempt,
    ComplementPrechargeAttempt,
    MemberPrechargeAttempt,
    RowPrechargeAttempt,
    WitnessFailure,
    WitnessPrechargeFailure,
    ComplementPrechargeFailure,
    MemberPrechargeFailure,
    RowPrechargeFailure,
    WitnessFailureAttempt,
    RowTransientPeak,
}

#[cfg(feature = "architecture-test-seams")]
thread_local! {
    static CYCLE_STORAGE_OBSERVER: std::cell::Cell<Option<fn(CycleStorageOperation)>> =
        const { std::cell::Cell::new(None) };
}

#[cfg(feature = "architecture-test-seams")]
pub(crate) fn observe_cycle_storage(operation: CycleStorageOperation) {
    CYCLE_STORAGE_OBSERVER.with(|slot| {
        if let Some(observer) = slot.get() {
            observer(operation);
        }
    });
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn with_cycle_storage_observer_for_test<T>(
    observer: fn(CycleStorageOperation),
    operation: impl FnOnce() -> T,
) -> T {
    struct Restore(Option<fn(CycleStorageOperation)>);
    impl Drop for Restore {
        fn drop(&mut self) {
            CYCLE_STORAGE_OBSERVER.with(|slot| slot.set(self.0));
        }
    }
    let previous = CYCLE_STORAGE_OBSERVER.with(|slot| slot.replace(Some(observer)));
    let restore = Restore(previous);
    let result = operation();
    drop(restore);
    result
}

#[cfg(feature = "architecture-test-seams")]
use crate::architecture::evidence::validate::ValidationPhase;

#[cfg(feature = "architecture-test-seams")]
fn observe_phase_precharged(phase: ValidationPhase) {
    crate::architecture::evidence::validate::observe_validation(
        crate::architecture::evidence::validate::ValidationOperation::PhasePrecharged(phase),
    );
}

#[cfg(feature = "architecture-test-seams")]
fn observe_phase_visit(phase: ValidationPhase) {
    crate::architecture::evidence::validate::observe_validation(
        crate::architecture::evidence::validate::ValidationOperation::PhaseVisit(phase),
    );
}

#[cfg(feature = "architecture-test-seams")]
fn observe_phase_allocation(phase: ValidationPhase) {
    crate::architecture::evidence::validate::observe_validation(
        crate::architecture::evidence::validate::ValidationOperation::PhaseAllocation(phase),
    );
}

struct LinearBasis {
    rows: Vec<Option<Box<[u64]>>>,
    candidate: Vec<u64>,
    columns: usize,
    rank: usize,
}

impl LinearBasis {
    fn new(columns: usize) -> Self {
        Self {
            rows: std::iter::repeat_with(|| None).take(columns).collect(),
            candidate: vec![0; columns.div_ceil(64)],
            columns,
            rank: 0,
        }
    }

    fn clear_candidate(&mut self) {
        self.candidate.fill(0);
    }

    fn candidate_mut(&mut self) -> &mut [u64] {
        &mut self.candidate
    }

    fn insert_candidate(&mut self) -> bool {
        for pivot in (0..self.columns).rev() {
            if self.candidate[pivot / 64] & (1u64 << (pivot % 64)) == 0 {
                continue;
            }
            if let Some(row) = &self.rows[pivot] {
                for (left, right) in self.candidate.iter_mut().zip(row) {
                    *left ^= right;
                }
            } else {
                self.rows[pivot] = Some(self.candidate.clone().into_boxed_slice());
                self.rank += 1;
                return true;
            }
        }
        false
    }

    fn release_candidate(&mut self) {
        self.candidate = Vec::new();
    }

    fn rank(&self) -> usize {
        self.rank
    }
}
