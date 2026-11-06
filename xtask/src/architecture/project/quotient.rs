use std::collections::{BTreeMap, BTreeSet};

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};

use crate::architecture::project::cluster::{ClusterBuild, ClusterIdentity};
use crate::architecture::project::cycle::{
    ComponentBasis, CycleWitness, complement_basis, validate_basis,
};
use crate::architecture::project::scc::{ComponentIdentity, Decomposition, ProjectionBudget};
use crate::architecture::schema::{CanonicalText, EdgeIdentity, Identity, Sha256Digest};

pub const QUOTIENT_SCHEMA: &str = "bbnf.architecture.quotient.v3.1";

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct QuotientEdge {
    pub from: ClusterIdentity,
    pub to: ClusterIdentity,
    pub edges: Vec<EdgeIdentity>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct QuotientBasis {
    pub component: ComponentIdentity,
    pub rank: usize,
    pub cycles: Vec<CycleWitness>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Quotient {
    pub schema: CanonicalText,
    pub edges: Vec<QuotientEdge>,
    pub bases: Vec<QuotientBasis>,
    pub movement_authorized: bool,
    pub hash: Sha256Digest,
}

pub fn build(
    clusters: &ClusterBuild,
    decomposition: &Decomposition<'_>,
    complete_bases: &[ComponentBasis],
    budget: &mut ProjectionBudget,
) -> Result<Quotient> {
    let projection = decomposition.projection();
    let edges = decomposition.edges();
    clusters.validate_domain(decomposition, budget)?;
    validate_complete_basis_domain(decomposition, complete_bases, budget)?;
    budget.charge(
        projection,
        "quotient",
        declared_quotient_cost(
            clusters.ownership_len(),
            edges.len(),
            clusters.set().clusters.len(),
            complete_bases.len(),
        )?,
    )?;
    admit_quotient_bytes(clusters, decomposition, complete_bases, budget)?;
    #[cfg(feature = "architecture-test-seams")]
    crate::architecture::project::cluster::observe_quotient_domain(
        crate::architecture::project::cluster::QuotientDomainOperation::Derived,
    );
    validate_complete_bases(decomposition, complete_bases, budget)?;
    for edge in edges {
        edge.validate_identity()
            .context("CYCLE_DOMAIN_MISMATCH: typed edge identity is invalid")?;
    }
    let edge_map = edges
        .iter()
        .map(|edge| (&edge.identity, *edge))
        .collect::<BTreeMap<_, _>>();
    if edge_map.len() != edges.len() {
        anyhow::bail!("CYCLE_DOMAIN_MISMATCH: duplicate typed edge identity");
    }
    let mut grouped = BTreeMap::<(ClusterIdentity, ClusterIdentity), Vec<EdgeIdentity>>::new();
    for edge in edges {
        let from = clusters
            .owner(&edge.record.from)
            .ok_or_else(|| anyhow::anyhow!("SCC_DOMAIN_MISMATCH: quotient source is unowned"))?
            .clone();
        let to = clusters
            .owner(&edge.record.to)
            .ok_or_else(|| anyhow::anyhow!("SCC_DOMAIN_MISMATCH: quotient target is unowned"))?
            .clone();
        if from != to {
            grouped
                .entry((from, to))
                .or_default()
                .push(edge.identity.clone());
        }
    }
    let quotient_edges = grouped
        .into_iter()
        .map(|((from, to), mut edges)| {
            edges.sort();
            QuotientEdge { from, to, edges }
        })
        .collect::<Vec<_>>();
    let mut bases = Vec::new();
    for component in decomposition.components() {
        if component.members.len() > 4_096 {
            anyhow::bail!("RESOURCE_BUDGET_EXCEEDED: quotient component exceeds 4096 members");
        }
        let members = component.members.iter().cloned().collect::<BTreeSet<_>>();
        let internal = decomposition.internal_edges(&component.identity)?;
        let seed = clusters
            .set()
            .clusters
            .iter()
            .filter(|cluster| cluster.component == component.identity)
            .flat_map(|cluster| cluster.internal_basis.cycles.iter().cloned())
            .collect::<Vec<_>>();
        let complete = complete_bases
            .binary_search_by(|basis| basis.component.cmp(&component.identity))
            .ok()
            .map(|index| &complete_bases[index]);
        let total = complete.map_or(0, |basis| basis.rank);
        let (rank, cycles) = if total != 0 {
            let basis = complete.ok_or_else(|| {
                anyhow::anyhow!("CYCLE_BASIS_INCOMPLETE: complete component basis is absent")
            })?;
            complement_basis(&members, internal, &seed, basis, budget)?
        } else {
            (0, Vec::new())
        };
        if rank != 0 {
            let crossing = cycles.iter().all(|cycle| {
                cycle.edges.iter().any(|edge_id| {
                    edge_map.get(edge_id).is_some_and(|edge| {
                        clusters.owner(&edge.record.from) != clusters.owner(&edge.record.to)
                    })
                })
            });
            if !crossing {
                anyhow::bail!(
                    "CYCLE_BASIS_INCOMPLETE: quotient complement omitted a crossing edge"
                );
            }
            bases.push(QuotientBasis {
                component: component.identity.clone(),
                rank,
                cycles,
            });
        }
    }
    bases.sort_by(|left, right| left.component.cmp(&right.component));
    let movement_authorized = false;
    let schema = CanonicalText::new(QUOTIENT_SCHEMA)?;
    let hash = Sha256Digest::new(crate::architecture::hash::hash_serialized(&(
        &schema,
        &quotient_edges,
        &bases,
        movement_authorized,
    ))?)?;
    Ok(Quotient {
        schema,
        edges: quotient_edges,
        bases,
        movement_authorized,
        hash,
    })
}

fn validate_complete_bases(
    decomposition: &Decomposition<'_>,
    complete_bases: &[ComponentBasis],
    budget: &mut ProjectionBudget,
) -> Result<()> {
    for (component, basis) in decomposition
        .components()
        .iter()
        .filter(|component| component.cyclic)
        .zip(complete_bases)
    {
        let members = component.members.iter().cloned().collect::<BTreeSet<_>>();
        let internal = decomposition.internal_edges(&component.identity)?;
        validate_basis(&members, internal, basis, budget)?;
    }
    Ok(())
}

fn validate_complete_basis_domain(
    decomposition: &Decomposition<'_>,
    complete_bases: &[ComponentBasis],
    budget: &mut ProjectionBudget,
) -> Result<()> {
    let declared_work = complete_bases
        .len()
        .saturating_sub(1)
        .checked_add(decomposition.components().len())
        .and_then(|work| work.checked_add(complete_bases.len()))
        .and_then(|work| work.checked_add(1))
        .context("RESOURCE_BUDGET_EXCEEDED: complete-basis domain cost overflowed")?;
    budget.charge(
        decomposition.projection(),
        "complete-basis-domain",
        declared_work,
    )?;
    #[cfg(feature = "architecture-test-seams")]
    crate::architecture::project::cluster::observe_quotient_domain(
        crate::architecture::project::cluster::QuotientDomainOperation::CompleteBasisPrecharged {
            work: declared_work,
        },
    );
    for pair in complete_bases.windows(2) {
        #[cfg(feature = "architecture-test-seams")]
        crate::architecture::project::cluster::observe_quotient_domain(
            crate::architecture::project::cluster::QuotientDomainOperation::CompleteBasisVisit,
        );
        if pair[0].component == pair[1].component {
            anyhow::bail!("CYCLE_BASIS_INCOMPLETE: duplicate complete component basis");
        }
        if pair[0].component > pair[1].component {
            anyhow::bail!("CYCLE_BASIS_INCOMPLETE: noncanonical complete component basis");
        }
    }
    let mut actual = complete_bases.iter();
    for component in decomposition.components().iter().filter(|component| {
        #[cfg(feature = "architecture-test-seams")]
        crate::architecture::project::cluster::observe_quotient_domain(
            crate::architecture::project::cluster::QuotientDomainOperation::CompleteBasisVisit,
        );
        component.cyclic
    }) {
        let basis = actual
            .next()
            .context("CYCLE_BASIS_INCOMPLETE: complete component basis domain differs")?;
        if basis.component != component.identity {
            anyhow::bail!("CYCLE_BASIS_INCOMPLETE: complete component basis domain differs");
        }
    }
    if actual.next().is_some() {
        anyhow::bail!("CYCLE_BASIS_INCOMPLETE: complete component basis domain differs");
    }
    Ok(())
}

fn admit_quotient_bytes(
    clusters: &ClusterBuild,
    decomposition: &Decomposition<'_>,
    complete_bases: &[ComponentBasis],
    budget: &mut ProjectionBudget,
) -> Result<()> {
    let projection = decomposition.projection();
    let fixed = clusters
        .ownership_len()
        .checked_mul(
            std::mem::size_of::<(
                Identity,
                crate::architecture::project::cluster::ClusterIdentity,
            )>() * 2,
        )
        .and_then(|bytes| {
            decomposition
                .edges()
                .len()
                .checked_mul(std::mem::size_of::<&crate::architecture::schema::EdgeFact>() * 3)
                .and_then(|edge_bytes| bytes.checked_add(edge_bytes))
        })
        .and_then(|bytes| {
            complete_bases
                .len()
                .checked_mul(std::mem::size_of::<ComponentBasis>())
                .and_then(|basis_bytes| bytes.checked_add(basis_bytes))
        })
        .context("RESOURCE_BUDGET_EXCEEDED: quotient fixed byte reservation overflowed")?;
    let ceiling = budget
        .available_bytes(projection)?
        .checked_sub(fixed)
        .context("RESOURCE_BUDGET_EXCEEDED: quotient indexes exceed the byte budget")?
        .checked_div(2)
        .context("RESOURCE_BUDGET_EXCEEDED: quotient serialized divisor is zero")?;
    let aggregate = crate::architecture::hash::serialized_len_capped(
        &(
            clusters.set(),
            decomposition.components(),
            decomposition.edges(),
            complete_bases,
        ),
        ceiling,
    )
    .context("RESOURCE_BUDGET_EXCEEDED: quotient input exceeds its serialized byte ceiling")?;
    budget.reserve(
        projection,
        "quotient-domain-and-indexes",
        aggregate
            .checked_mul(2)
            .and_then(|bytes| bytes.checked_add(fixed))
            .context("RESOURCE_BUDGET_EXCEEDED: quotient byte reservation overflowed")?,
    )
}

fn declared_quotient_cost(
    nodes: usize,
    edges: usize,
    clusters: usize,
    bases: usize,
) -> Result<usize> {
    nodes
        .checked_mul(2)
        .and_then(|cost| {
            edges
                .checked_mul(5)
                .and_then(|edges| cost.checked_add(edges))
        })
        .and_then(|cost| {
            clusters
                .checked_mul(2)
                .and_then(|items| cost.checked_add(items))
        })
        .and_then(|cost| {
            bases
                .checked_mul(2)
                .and_then(|items| cost.checked_add(items))
        })
        .context("RESOURCE_BUDGET_EXCEEDED: quotient declared cost overflowed")
}
