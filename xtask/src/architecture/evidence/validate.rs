use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};

use anyhow::{Context, Result, bail};

use crate::architecture::evidence::binding::SourceBinding;
use crate::architecture::hash::BoundedKernelError;
use crate::architecture::inventory::Inventory;
use crate::architecture::manifest::ConstellationManifest;
use crate::architecture::project::cycle::{
    basis_for_subgraph, component_bases, validate_cycle_completion_refs,
};
use crate::architecture::project::scc::{
    ComponentIdentity, Decomposition, ProjectionBudget, decompose_refs,
};
use crate::architecture::project::{ProjectionSet, cluster::ClusterIdentity};
use crate::architecture::schema::{
    Attribute, EdgeFact, EdgeIdentity, GRAPH_SCHEMA, GraphArtifact, Identity, NodeFact, NodeKind,
    PhysicalKind, Sha256Digest,
};

type NodeIndex<'a> = BTreeMap<&'a Identity, &'a NodeFact>;
type EdgeIndex<'a> = BTreeMap<&'a EdgeIdentity, &'a EdgeFact>;

#[cfg(feature = "architecture-test-seams")]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
#[doc(hidden)]
pub enum ValidationPhase {
    CanonicalPrivacy,
    IdentityWidthConstruction,
    SccSerializationIndex,
    CycleMember,
    CycleDomain,
    CycleMemberHash,
    CycleBasis,
    CycleLinearSizing,
    CycleLinearAllocation,
}

#[cfg(feature = "architecture-test-seams")]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[doc(hidden)]
pub enum ValidationOperation {
    Setup,
    Selected,
    Member,
    Owner,
    Signature,
    Incidence,
    Connectivity,
    Quotient,
    PhasePrecharged(ValidationPhase),
    PhaseVisit(ValidationPhase),
    PhaseAllocation(ValidationPhase),
}

#[cfg(feature = "architecture-test-seams")]
thread_local! {
    static VALIDATION_OBSERVER: std::cell::Cell<Option<fn(ValidationOperation)>> =
        const { std::cell::Cell::new(None) };
}

#[cfg(feature = "architecture-test-seams")]
pub(crate) fn observe_validation(operation: ValidationOperation) {
    VALIDATION_OBSERVER.with(|slot| {
        if let Some(observer) = slot.get() {
            observer(operation);
        }
    });
}

#[cfg(feature = "architecture-test-seams")]
fn with_validation_observer<T>(
    observer: fn(ValidationOperation),
    operation: impl FnOnce() -> T,
) -> T {
    struct Restore(Option<fn(ValidationOperation)>);

    impl Drop for Restore {
        fn drop(&mut self) {
            VALIDATION_OBSERVER.with(|slot| slot.set(self.0));
        }
    }

    let previous = VALIDATION_OBSERVER.with(|slot| slot.replace(Some(observer)));
    let restore = Restore(previous);
    let result = operation();
    drop(restore);
    result
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn with_validation_observer_for_test<T>(
    observer: fn(ValidationOperation),
    operation: impl FnOnce() -> T,
) -> T {
    with_validation_observer(observer, operation)
}

pub fn validate_all(
    manifest: &ConstellationManifest,
    inventory: &Inventory,
    binding: &SourceBinding,
    graph: &mut GraphArtifact,
    projections: &ProjectionSet,
) -> Result<()> {
    crate::architecture::evidence::write::failpoint("global-validation")?;
    manifest.validate()?;
    inventory.validate(manifest)?;
    if !binding.complete || !binding.unavailable.is_empty() {
        bail!("SOURCE_STATE_MANIFEST_ABSENT: source binding is incomplete");
    }
    graph.canonicalize()?;
    let nodes = node_index(graph);
    validate_inventory_nodes(inventory, &nodes)?;
    validate_worktree_nominations(binding, graph)?;
    validate_projections(graph, projections, &nodes)?;
    Ok(())
}

fn node_index(graph: &GraphArtifact) -> NodeIndex<'_> {
    graph
        .nodes()
        .into_iter()
        .map(|node| (&node.identity, node))
        .collect()
}

fn edge_index(graph: &GraphArtifact) -> Result<EdgeIndex<'_>> {
    let edges = graph.edges();
    for edge in &edges {
        edge.validate_identity()
            .context("PROJECTION_EDGE_TOTALITY_MISMATCH: typed edge identity is invalid")?;
    }
    let index = edges
        .into_iter()
        .map(|edge| (&edge.identity, edge))
        .collect::<EdgeIndex<'_>>();
    if index.len() != graph.edges().len() {
        bail!("PROJECTION_EDGE_TOTALITY_MISMATCH: duplicate typed edge identity");
    }
    Ok(index)
}

fn validate_inventory_nodes(inventory: &Inventory, nodes: &NodeIndex<'_>) -> Result<()> {
    for record in &inventory.records {
        let identity = Identity::Physical {
            physical: record.identity.clone(),
        };
        let node = nodes
            .get(&identity)
            .ok_or_else(|| anyhow::anyhow!("inventory physical identity has no graph node"))?;
        let expected = match record.identity.kind {
            PhysicalKind::File => NodeKind::PhysicalFile,
            PhysicalKind::Directory => NodeKind::PhysicalDirectory,
            PhysicalKind::Symlink => NodeKind::PhysicalSymlink,
        };
        if node.kind != expected {
            bail!("INVENTORY_NODE_KIND_MISMATCH: physical node kind differs");
        }
    }
    Ok(())
}

fn validate_worktree_nominations(binding: &SourceBinding, graph: &GraphArtifact) -> Result<()> {
    let edges = graph.edges();
    for nomination in &binding.nominations {
        let target = Identity::Worktree {
            worktree: nomination.worktree.clone(),
        };
        let owned = edges.iter().any(|edge| {
            edge.record.kind == crate::architecture::schema::EdgeKind::RepositoryOwnsWorktree
                && edge.record.to == target
                && edge.record.from
                    == Identity::Repository {
                        repository: nomination.repository.clone(),
                    }
        });
        let nominated = edges.iter().any(|edge| {
            edge.record.kind == crate::architecture::schema::EdgeKind::ProductNominatesWorktree
                && edge.record.to == target
                && edge.record.from
                    == Identity::Product {
                        product: nomination.product.clone(),
                    }
                && edge.record.attributes.contains(
                    &crate::architecture::schema::Attribute::Nomination {
                        nomination: nomination.nomination.clone(),
                    },
                )
        });
        if !owned || !nominated {
            bail!(
                "PRODUCT_WORKTREE_UNNOMINATED: nomination does not target one measured owned worktree"
            );
        }
    }
    Ok(())
}

fn validate_projections(
    graph: &GraphArtifact,
    projections: &ProjectionSet,
    nodes: &NodeIndex<'_>,
) -> Result<()> {
    graph.validate_canonical()?;
    if projections.schema.as_str() != crate::architecture::project::spec::PROJECTION_SET_SCHEMA {
        bail!("PROJECTION_SET_INCOMPLETE: projection-set schema differs");
    }
    let node_hash = Sha256Digest::new(crate::architecture::hash::hash_serialized(
        &nodes.values().copied().collect::<Vec<_>>(),
    )?)?;
    if projections.node_universe_hash != node_hash || projections.node_count != nodes.len() {
        bail!("projection node-universe binding is incomplete");
    }
    let required_failures = graph.required_failures();
    let expected_specs = crate::architecture::project::spec::projection_specs();
    if projections.projections.len() != expected_specs.len()
        || projections
            .projections
            .iter()
            .zip(&expected_specs)
            .any(|(projection, expected)| projection.spec != *expected)
    {
        bail!("PROJECTION_SET_INCOMPLETE: closed projection spec set differs");
    }
    for projection in &projections.projections {
        if projection.schema.as_str() != crate::architecture::project::spec::PROJECTION_SCHEMA
            || projection.implicit_singletons.as_str()
                != crate::architecture::project::spec::IMPLICIT_SINGLETONS
            || projection.clusters.schema.as_str()
                != crate::architecture::project::cluster::CLUSTER_SCHEMA
            || projection.clusters.implicit_singletons.as_str()
                != crate::architecture::project::cluster::IMPLICIT_SINGLETONS
            || projection.quotient.schema.as_str()
                != crate::architecture::project::quotient::QUOTIENT_SCHEMA
        {
            bail!("PROJECTION_SET_INCOMPLETE: projection schema constants differ");
        }
        if projection.node_universe_hash != node_hash || projection.node_count != nodes.len() {
            bail!("projection lost the total node universe");
        }
        let expected_failures = required_failures
            .iter()
            .filter(|failure| failure.projection == projection.spec.name)
            .cloned()
            .collect::<Vec<_>>();
        if projection.required_failures != expected_failures {
            bail!("PROJECTION_REQUIRED_FAILURE_MISMATCH: projection failures differ");
        }
        if !projection.structurally_valid_for_selected_edges {
            bail!("PROJECTION_STRUCTURAL_VALIDITY_MISMATCH: selected-edge structure is not valid");
        }
        if projection.movement_authorized
            || projection.quotient.movement_authorized
            || projection
                .clusters
                .clusters
                .iter()
                .any(|cluster| cluster.movement_authorized)
        {
            bail!("movement authorization is not globally denied");
        }
        let mut budget = ProjectionBudget::standard(projection.spec.name);
        validate_projection_from_graph(graph, projection, &mut budget)?;
        let expected_hash = Sha256Digest::new(crate::architecture::hash::hash_serialized(&(
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
        ))?)?;
        if projection.hash != expected_hash {
            bail!("projection hash omits a structural or authorization field");
        }
    }
    let rebuilt = crate::architecture::project::spec::rebuild(graph)?;
    for (actual, expected) in projections.projections.iter().zip(&rebuilt.projections) {
        if actual.clusters != expected.clusters {
            bail!(
                "CLUSTER_CUT_TOTALITY_MISMATCH: exact deterministic cluster set or order differs"
            );
        }
        if actual.quotient != expected.quotient {
            bail!("BOUNDARY_QUOTIENT_ABSENT: exact deterministic quotient or order differs");
        }
        if actual != expected {
            bail!("PROJECTION_REBUILD_MISMATCH: exact deterministic projection differs");
        }
    }
    if *projections != rebuilt {
        bail!("PROJECTION_REBUILD_MISMATCH: exact deterministic projection set differs");
    }
    Ok(())
}

pub fn validate_structural(graph: &GraphArtifact, projections: &ProjectionSet) -> Result<()> {
    graph.validate_canonical()?;
    validate_projections(graph, projections, &node_index(graph))
}

fn validate_projection_structure<'a>(
    graph_edges: &EdgeIndex<'a>,
    vertices: &BTreeSet<Identity>,
    projection: &crate::architecture::project::Projection,
    nodes: &NodeIndex<'_>,
    budget: &mut ProjectionBudget,
) -> Result<(usize, usize, usize)> {
    let selected_work = projection
        .edge_ids
        .len()
        .checked_mul(3)
        .and_then(|cost| {
            graph_edges
                .len()
                .checked_mul(2)
                .and_then(|edges| cost.checked_add(edges))
        })
        .and_then(|cost| cost.checked_add(projection.spec.edge_kinds.len()))
        .context("RESOURCE_BUDGET_EXCEEDED: selected-edge validation cost overflowed")?;
    budget.charge(
        projection.spec.name,
        "validation-selected-edges",
        selected_work,
    )?;
    admit_selected_validation_bytes(projection, budget)?;
    let selected = projection
        .edge_ids
        .iter()
        .map(|identity| {
            #[cfg(feature = "architecture-test-seams")]
            observe_validation(ValidationOperation::Selected);
            graph_edges
                .get(identity)
                .copied()
                .ok_or_else(|| anyhow::anyhow!("projection names absent edge"))
        })
        .collect::<Result<Vec<_>>>()?;
    if selected.iter().any(|edge| {
        #[cfg(feature = "architecture-test-seams")]
        observe_validation(ValidationOperation::Selected);
        !projection.spec.edge_kinds.contains(&edge.record.kind)
    }) {
        bail!("projection contains an edge outside its closed spec");
    }
    let expected_edges = graph_edges
        .values()
        .copied()
        .filter(|edge| {
            #[cfg(feature = "architecture-test-seams")]
            observe_validation(ValidationOperation::Selected);
            projection.spec.edge_kinds.contains(&edge.record.kind)
        })
        .map(|edge| &edge.identity);
    if !projection.edge_ids.iter().eq(expected_edges) {
        bail!("PROJECTION_EDGE_TOTALITY_MISMATCH: selected edge set differs");
    }
    let decomposition = decompose_refs(projection.spec.name, vertices, &selected, budget)?;
    validate_components(projection, nodes, &decomposition, budget)?;
    let cluster_validation = validate_clusters(projection, &decomposition, nodes, budget)?;
    validate_quotient(
        projection,
        &selected,
        &cluster_validation.explicit_owners,
        &decomposition,
        budget,
    )?;
    Ok((
        cluster_validation.explicit_owners.len(),
        cluster_validation.admission_prefix_bytes,
        cluster_validation.admission_bytes,
    ))
}

fn validate_projection_from_graph(
    graph: &GraphArtifact,
    projection: &crate::architecture::project::Projection,
    budget: &mut ProjectionBudget,
) -> Result<(usize, usize, usize, usize)> {
    let (node_count, edge_count, canonical_prefix_work) =
        validate_canonical_phase_bounded(graph, projection.spec.name, budget)
            .map_err(anyhow::Error::new)?;
    admit_validation_index_bytes(projection.spec.name, node_count, edge_count, budget)?;
    let nodes = node_index(graph);
    let graph_edges = edge_index(graph)?;
    let vertices = nodes.keys().copied().cloned().collect::<BTreeSet<_>>();
    let (owners, owner_prefix_bytes, owner_admission_bytes) =
        validate_projection_structure(&graph_edges, &vertices, projection, &nodes, budget)?;
    Ok((
        owners,
        canonical_prefix_work,
        owner_prefix_bytes,
        owner_admission_bytes,
    ))
}

fn validate_canonical_phase_bounded(
    graph: &GraphArtifact,
    projection: crate::architecture::schema::ProjectionName,
    budget: &mut ProjectionBudget,
) -> std::result::Result<(usize, usize, usize), BoundedKernelError> {
    if graph.schema.as_str() != GRAPH_SCHEMA {
        return Err(BoundedKernelError::Canonical {
            code: "GRAPH_SCHEMA_MISMATCH",
            first: 0,
            second: 0,
        });
    }
    let setup_counting_work =
        graph
            .truth
            .len()
            .checked_mul(2)
            .ok_or(BoundedKernelError::Arithmetic(
                "validation setup counting cost overflowed",
            ))?;
    budget.charge_bounded(projection, "validation-setup-counting", setup_counting_work)?;
    let node_count = graph
        .truth
        .iter()
        .filter(|truth| {
            matches!(
                truth,
                crate::architecture::schema::Truth::Exact {
                    fact: crate::architecture::schema::Fact::Node { .. }
                }
            )
        })
        .count();
    let edge_count = graph
        .truth
        .iter()
        .filter(|truth| {
            matches!(
                truth,
                crate::architecture::schema::Truth::Exact {
                    fact: crate::architecture::schema::Fact::Edge { .. }
                }
            )
        })
        .count();
    let setup_work = graph
        .truth
        .len()
        .checked_mul(3)
        .and_then(|cost| {
            node_count
                .checked_mul(2)
                .and_then(|value| cost.checked_add(value))
        })
        .and_then(|cost| {
            edge_count
                .checked_mul(3)
                .and_then(|value| cost.checked_add(value))
        })
        .ok_or(BoundedKernelError::Arithmetic(
            "validation setup cost overflowed",
        ))?;
    budget.charge_bounded(projection, "validation-setup", setup_work)?;
    let serialized_length =
        admit_validation_setup_bytes(graph, projection, node_count, edge_count, budget)?;
    let canonical_prefix_work = budget.used();
    #[cfg(feature = "architecture-test-seams")]
    observe_validation(ValidationOperation::Setup);
    graph.validate_canonical_bounded_with_serialized_length(serialized_length)?;
    #[cfg(feature = "architecture-test-seams")]
    complete_canonical_privacy_phase();
    Ok((node_count, edge_count, canonical_prefix_work))
}

fn admit_validation_setup_bytes(
    graph: &GraphArtifact,
    projection: crate::architecture::schema::ProjectionName,
    nodes: usize,
    edges: usize,
    budget: &mut ProjectionBudget,
) -> std::result::Result<usize, BoundedKernelError> {
    budget.charge_bounded(
        projection,
        "validation-setup-serialized-sizing",
        graph.truth.len(),
    )?;
    let mut admitted_length = None;
    let aggregate = crate::architecture::hash::execute_graph_measure_with_admission(
        graph,
        budget,
        "validation-setup-serialized-preflight",
        |preflight, budget| {
            let max_identity_width =
                construct_max_identity_width(graph, projection, nodes, edges, budget)?;
            let comparison_work =
                canonical_comparison_work(preflight, nodes, edges, max_identity_width)?;
            budget.charge_bounded(
                projection,
                "validation-canonical-comparison-complete",
                comparison_work,
            )?;
            admit_canonical_privacy(graph, projection, nodes, edges, preflight, budget)?;
            admitted_length = Some(preflight.serialized_length);
            Ok(())
        },
    )?;
    let admitted_length = admitted_length.ok_or(BoundedKernelError::Arithmetic(
        "canonical/privacy admission was skipped",
    ))?;
    if aggregate != admitted_length {
        return Err(BoundedKernelError::OutputDrift);
    }
    Ok(admitted_length)
}

fn admit_validation_index_bytes(
    projection: crate::architecture::schema::ProjectionName,
    nodes: usize,
    edges: usize,
    budget: &mut ProjectionBudget,
) -> Result<()> {
    let bytes = nodes
        .checked_mul(
            std::mem::size_of::<&NodeFact>()
                + std::mem::size_of::<(&Identity, &NodeFact)>() * 2
                + std::mem::size_of::<Identity>() * 2,
        )
        .and_then(|bytes| {
            edges
                .checked_mul(
                    std::mem::size_of::<&EdgeFact>()
                        + std::mem::size_of::<(&EdgeIdentity, &EdgeFact)>() * 2,
                )
                .and_then(|value| bytes.checked_add(value))
        })
        .context("RESOURCE_BUDGET_EXCEEDED: validation index reservation overflowed")?;
    budget.reserve(projection, "validation-post-canonical-indexes", bytes)
}

fn filter_collect_capacity(count: usize) -> std::result::Result<usize, BoundedKernelError> {
    if count == 0 {
        return Ok(0);
    }
    count
        .checked_next_power_of_two()
        .map(|capacity| capacity.max(4))
        .ok_or(BoundedKernelError::Arithmetic(
            "reference vector capacity overflowed",
        ))
}

fn canonical_phase_peak_bytes(
    graph: &GraphArtifact,
    nodes: usize,
    edges: usize,
    max_percent_string_bytes: usize,
) -> std::result::Result<usize, BoundedKernelError> {
    let node_references = filter_collect_capacity(nodes)?
        .checked_mul(std::mem::size_of::<&NodeFact>())
        .ok_or(BoundedKernelError::Arithmetic(
            "node reference storage overflowed",
        ))?;
    let edge_references = filter_collect_capacity(edges)?
        .checked_mul(std::mem::size_of::<&EdgeFact>())
        .ok_or(BoundedKernelError::Arithmetic(
            "edge reference storage overflowed",
        ))?;
    let obligations = graph
        .truth
        .len()
        .checked_sub(nodes)
        .and_then(|rows| rows.checked_sub(edges))
        .ok_or(BoundedKernelError::Arithmetic(
            "obligation count underflowed",
        ))?;
    let obligation_references = filter_collect_capacity(obligations)?
        .checked_mul(std::mem::size_of::<&crate::architecture::schema::Obligation>())
        .ok_or(BoundedKernelError::Arithmetic(
            "obligation reference storage overflowed",
        ))?;
    let edge_peak =
        node_references
            .checked_add(edge_references)
            .ok_or(BoundedKernelError::Arithmetic(
                "canonical edge reference peak overflowed",
            ))?;
    let decoder_peak = node_references
        .checked_add(
            crate::architecture::schema::percent_decode_allocation_bound(max_percent_string_bytes)?,
        )
        .ok_or(BoundedKernelError::Arithmetic(
            "canonical decoder peak overflowed",
        ))?;
    let obligation_peak = node_references.checked_add(obligation_references).ok_or(
        BoundedKernelError::Arithmetic("canonical obligation peak overflowed"),
    )?;
    Ok(node_references
        .max(edge_peak)
        .max(decoder_peak)
        .max(obligation_peak))
}

fn checked_identity_two_field_width(
    first: usize,
    second: usize,
) -> std::result::Result<usize, BoundedKernelError> {
    first
        .checked_add(second)
        .ok_or(BoundedKernelError::Arithmetic(
            "canonical Identity width overflowed",
        ))
}

fn identity_width_construction_work(
    truth: usize,
    nodes: usize,
    edges: usize,
) -> std::result::Result<usize, BoundedKernelError> {
    truth
        .checked_mul(4)
        .and_then(|work| nodes.checked_mul(9).and_then(|term| work.checked_add(term)))
        .and_then(|work| {
            edges
                .checked_mul(19)
                .and_then(|term| work.checked_add(term))
        })
        .ok_or(BoundedKernelError::Arithmetic(
            "canonical Identity-width construction overflowed",
        ))
}

fn canonical_endpoint_search_work(
    nodes: usize,
    edges: usize,
    max_identity_width: usize,
) -> std::result::Result<usize, BoundedKernelError> {
    let search_depth = if nodes == 0 {
        0
    } else {
        nodes
            .checked_next_power_of_two()
            .and_then(|power| (power.ilog2() as usize).checked_add(1))
            .ok_or(BoundedKernelError::Arithmetic(
                "canonical search depth overflowed",
            ))?
    };
    let identity_comparison = max_identity_width
        .checked_mul(2)
        .and_then(|work| work.checked_add(32))
        .ok_or(BoundedKernelError::Arithmetic(
            "canonical identity comparison overflowed",
        ))?;
    edges
        .checked_mul(2)
        .and_then(|work| work.checked_mul(search_depth))
        .and_then(|work| work.checked_mul(identity_comparison))
        .ok_or(BoundedKernelError::Arithmetic(
            "canonical endpoint search overflowed",
        ))
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn identity_two_field_width_arithmetic_for_test(
    first: usize,
    second: usize,
) -> std::result::Result<usize, BoundedKernelError> {
    checked_identity_two_field_width(first, second)
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn identity_width_construction_arithmetic_for_test(
    truth: usize,
    nodes: usize,
    edges: usize,
) -> std::result::Result<usize, BoundedKernelError> {
    identity_width_construction_work(truth, nodes, edges)
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn canonical_endpoint_search_arithmetic_for_test(
    nodes: usize,
    edges: usize,
    max_identity_width: usize,
) -> std::result::Result<usize, BoundedKernelError> {
    canonical_endpoint_search_work(nodes, edges, max_identity_width)
}

fn identity_total_dynamic_string_bytes(
    identity: &Identity,
) -> std::result::Result<usize, BoundedKernelError> {
    #[cfg(feature = "architecture-test-seams")]
    observe_validation(ValidationOperation::PhaseVisit(
        ValidationPhase::IdentityWidthConstruction,
    ));
    match identity {
        Identity::Repository { repository } => Ok(repository.as_str().len()),
        Identity::Product { product } => Ok(product.as_str().len()),
        Identity::Workspace { workspace } => Ok(workspace.as_str().len()),
        Identity::PackageFace { package_face } => Ok(package_face.as_str().len()),
        Identity::Context { context } => Ok(context.as_str().len()),
        Identity::Worktree { worktree } => Ok(worktree.as_str().len()),
        Identity::Documentation { documentation } => Ok(documentation.as_str().len()),
        Identity::Physical { physical } => checked_identity_two_field_width(
            physical.repository.as_str().len(),
            physical.path.as_str().len(),
        ),
        Identity::Virtual { namespace, key } => {
            checked_identity_two_field_width(namespace.as_str().len(), key.as_str().len())
        }
    }
}

fn construct_max_identity_width(
    graph: &GraphArtifact,
    projection: crate::architecture::schema::ProjectionName,
    nodes: usize,
    edges: usize,
    budget: &mut ProjectionBudget,
) -> std::result::Result<usize, BoundedKernelError> {
    let work = identity_width_construction_work(graph.truth.len(), nodes, edges)?;
    budget.charge_bounded(projection, "validation-identity-width-construction", work)?;
    #[cfg(feature = "architecture-test-seams")]
    observe_validation(ValidationOperation::PhasePrecharged(
        ValidationPhase::IdentityWidthConstruction,
    ));
    graph.truth.iter().try_fold(0usize, |maximum, truth| {
        let width = match truth {
            crate::architecture::schema::Truth::Exact {
                fact: crate::architecture::schema::Fact::Node { node },
            } => identity_total_dynamic_string_bytes(&node.identity)?,
            crate::architecture::schema::Truth::Exact {
                fact: crate::architecture::schema::Fact::Edge { edge },
            } => {
                let record = &edge.record;
                identity_total_dynamic_string_bytes(&record.from)?
                    .max(identity_total_dynamic_string_bytes(&record.to)?)
            }
            crate::architecture::schema::Truth::Unknown { .. } => 0,
        };
        Ok(maximum.max(width))
    })
}

fn canonical_comparison_work(
    preflight: &crate::architecture::hash::GraphJsonPreflight,
    nodes: usize,
    edges: usize,
    max_identity_width: usize,
) -> std::result::Result<usize, BoundedKernelError> {
    // Six complete V bounds cover adjacent Truth, nested-value, and
    // duplicate-identity comparisons. Each edge then performs two endpoint
    // searches of at most L comparisons apiece. W is the greatest complete
    // dynamic width of any node or endpoint Identity, including both fields
    // of Physical and Virtual, so one comparison reads at most 2W bytes.
    let volume = preflight
        .structural_work
        .checked_add(preflight.raw_string_bytes)
        .ok_or(BoundedKernelError::Arithmetic(
            "canonical comparison volume overflowed",
        ))?;
    let non_search = volume.checked_mul(6).ok_or(BoundedKernelError::Arithmetic(
        "canonical non-search comparison overflowed",
    ))?;
    let search = canonical_endpoint_search_work(nodes, edges, max_identity_width)?;
    non_search
        .checked_add(search)
        .ok_or(BoundedKernelError::Arithmetic(
            "canonical comparison work overflowed",
        ))
}

fn admit_canonical_privacy(
    graph: &GraphArtifact,
    projection: crate::architecture::schema::ProjectionName,
    nodes: usize,
    edges: usize,
    preflight: &crate::architecture::hash::GraphJsonPreflight,
    budget: &mut ProjectionBudget,
) -> std::result::Result<(), BoundedKernelError> {
    const TYPED_TRUTH_VISITS: usize = 16;
    let decoder_work =
        crate::architecture::schema::percent_decode_work_bound(preflight.percent_string_bytes)?;
    // One graph writer, two stable edge writers, one privacy traversal with
    // no-percent and decoded-DFA source roles, and fixed semantic visits:
    // C = 4S + 5R + 3J + D + 16T.
    let work = preflight
        .structural_work
        .checked_mul(4)
        .and_then(|work| {
            preflight
                .raw_string_bytes
                .checked_mul(5)
                .and_then(|raw| work.checked_add(raw))
        })
        .and_then(|work| {
            preflight
                .serialized_length
                .checked_mul(3)
                .and_then(|json| work.checked_add(json))
        })
        .and_then(|work| work.checked_add(decoder_work))
        .and_then(|work| {
            graph
                .truth
                .len()
                .checked_mul(TYPED_TRUTH_VISITS)
                .and_then(|rows| work.checked_add(rows))
        })
        .ok_or(BoundedKernelError::Arithmetic(
            "canonical/privacy work overflowed",
        ))?;
    budget.charge_bounded(projection, "validation-canonical-privacy-complete", work)?;
    let bytes =
        canonical_phase_peak_bytes(graph, nodes, edges, preflight.max_percent_string_bytes)?;
    budget.reserve_bounded(projection, "validation-canonical-phase-peak", bytes)?;
    #[cfg(feature = "architecture-test-seams")]
    observe_validation(ValidationOperation::PhasePrecharged(
        ValidationPhase::CanonicalPrivacy,
    ));
    #[cfg(feature = "architecture-test-seams")]
    observe_validation(ValidationOperation::PhaseVisit(
        ValidationPhase::CanonicalPrivacy,
    ));
    Ok(())
}

#[cfg(feature = "architecture-test-seams")]
fn complete_canonical_privacy_phase() {
    observe_validation(ValidationOperation::PhaseAllocation(
        ValidationPhase::CanonicalPrivacy,
    ));
}

fn admit_selected_validation_bytes(
    projection: &crate::architecture::project::Projection,
    budget: &mut ProjectionBudget,
) -> Result<()> {
    let sizing_work = projection
        .edge_ids
        .len()
        .checked_add(projection.spec.edge_kinds.len())
        .context("RESOURCE_BUDGET_EXCEEDED: selected-edge sizing cost overflowed")?;
    budget.charge(
        projection.spec.name,
        "validation-selected-edge-serialized-sizing",
        sizing_work,
    )?;
    let fixed = projection
        .edge_ids
        .len()
        .checked_mul(std::mem::size_of::<&EdgeFact>() * 2)
        .context("RESOURCE_BUDGET_EXCEEDED: selected-edge reservation overflowed")?;
    let ceiling = budget
        .available_bytes(projection.spec.name)?
        .checked_sub(fixed)
        .context("RESOURCE_BUDGET_EXCEEDED: selected-edge indexes exceed the byte budget")?;
    let aggregate = crate::architecture::hash::serialized_len_capped(
        &(&projection.edge_ids, &projection.spec.edge_kinds),
        ceiling,
    )
    .context("RESOURCE_BUDGET_EXCEEDED: selected edges exceed their serialized byte ceiling")?;
    budget.reserve(
        projection.spec.name,
        "validation-selected-edges",
        aggregate
            .checked_add(fixed)
            .context("RESOURCE_BUDGET_EXCEEDED: selected-edge reservation overflowed")?,
    )
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn validate_projection_budget_for_test(
    graph: &GraphArtifact,
    projection: &crate::architecture::project::Projection,
    work_limit: usize,
    byte_limit: usize,
) -> Result<(usize, usize, usize, usize, usize, usize)> {
    let mut budget = ProjectionBudget::with_limits(projection.spec.name, work_limit, byte_limit);
    let (owners, canonical_prefix_work, owner_prefix_bytes, owner_admission_bytes) =
        validate_projection_from_graph(graph, projection, &mut budget)?;
    Ok((
        owners,
        budget.used(),
        budget.bytes_reserved(),
        canonical_prefix_work,
        owner_prefix_bytes,
        owner_admission_bytes,
    ))
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn validate_projection_budget_with_observer_for_test(
    graph: &GraphArtifact,
    projection: &crate::architecture::project::Projection,
    work_limit: usize,
    byte_limit: usize,
    observer: fn(ValidationOperation),
) -> Result<(usize, usize, usize, usize, usize, usize)> {
    with_validation_observer(observer, || {
        validate_projection_budget_for_test(graph, projection, work_limit, byte_limit)
    })
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn validate_canonical_prefix_for_test(
    graph: &GraphArtifact,
    projection: crate::architecture::schema::ProjectionName,
    work_limit: usize,
    byte_limit: usize,
    observer: fn(ValidationOperation),
) -> std::result::Result<usize, BoundedKernelError> {
    with_validation_observer(observer, || {
        let mut budget = ProjectionBudget::with_limits(projection, work_limit, byte_limit);
        validate_canonical_phase_bounded(graph, projection, &mut budget)?;
        Ok(budget.used())
    })
}

fn validate_components(
    projection: &crate::architecture::project::Projection,
    nodes: &NodeIndex<'_>,
    decomposition: &Decomposition<'_>,
    budget: &mut ProjectionBudget,
) -> Result<()> {
    budget.charge(
        projection.spec.name,
        "validation-component-member-counting",
        projection.components.len(),
    )?;
    let member_count = projection
        .components
        .iter()
        .try_fold(0usize, |sum, component| {
            sum.checked_add(component.members.len())
                .context("RESOURCE_BUDGET_EXCEEDED: component member count overflowed")
        })?;
    let component_work = decomposition
        .components()
        .len()
        .checked_mul(2)
        .and_then(|cost| {
            member_count
                .checked_mul(3)
                .and_then(|value| cost.checked_add(value))
        })
        .context("RESOURCE_BUDGET_EXCEEDED: component validation cost overflowed")?;
    budget.charge(
        projection.spec.name,
        "validation-components",
        component_work,
    )?;
    admit_component_validation_bytes(projection, member_count, budget)?;
    let expected_components = decomposition.components().iter().filter(|component| {
        #[cfg(feature = "architecture-test-seams")]
        observe_validation(ValidationOperation::Member);
        component.members.len() > 1 || component.cyclic
    });
    if !projection.components.iter().eq(expected_components) {
        bail!("SCC_TOTALITY_MISMATCH: explicit SCC set or membership differs");
    }
    for component in &projection.components {
        #[cfg(feature = "architecture-test-seams")]
        observe_validation(ValidationOperation::Member);
        component
            .identity
            .validate_members(projection.spec.name, &component.members, budget)?;
    }
    let expected_bases = component_bases(decomposition, budget)?;
    if projection.component_bases != expected_bases {
        bail!("CYCLE_BASIS_INCOMPLETE: complete SCC basis set differs");
    }
    if projection
        .components
        .iter()
        .flat_map(|component| &component.members)
        .any(|member| {
            #[cfg(feature = "architecture-test-seams")]
            observe_validation(ValidationOperation::Member);
            !nodes.contains_key(member)
        })
    {
        bail!("SCC_TOTALITY_MISMATCH: explicit SCC membership is absent");
    }
    Ok(())
}

fn admit_component_validation_bytes(
    projection: &crate::architecture::project::Projection,
    members: usize,
    budget: &mut ProjectionBudget,
) -> Result<()> {
    let sizing_work = projection
        .components
        .len()
        .checked_add(projection.component_bases.len())
        .context("RESOURCE_BUDGET_EXCEEDED: component serialized sizing cost overflowed")?;
    budget.charge(
        projection.spec.name,
        "validation-component-serialized-sizing",
        sizing_work,
    )?;
    let fixed = members
        .checked_mul(std::mem::size_of::<Identity>() * 2)
        .and_then(|bytes| {
            projection
                .components
                .len()
                .checked_mul(std::mem::size_of::<ComponentIdentity>())
                .and_then(|value| bytes.checked_add(value))
        })
        .context("RESOURCE_BUDGET_EXCEEDED: component validation reservation overflowed")?;
    let ceiling = budget
        .available_bytes(projection.spec.name)?
        .checked_sub(fixed)
        .context("RESOURCE_BUDGET_EXCEEDED: component validation indexes exceed the byte budget")?
        .checked_div(2)
        .context("RESOURCE_BUDGET_EXCEEDED: component serialized divisor is zero")?;
    let aggregate = crate::architecture::hash::serialized_len_capped(
        &(&projection.components, &projection.component_bases),
        ceiling,
    )
    .context("RESOURCE_BUDGET_EXCEEDED: components exceed their serialized byte ceiling")?;
    budget.reserve(
        projection.spec.name,
        "validation-components",
        aggregate
            .checked_mul(2)
            .and_then(|value| value.checked_add(fixed))
            .context("RESOURCE_BUDGET_EXCEEDED: component validation reservation overflowed")?,
    )
}

fn explicit_cluster_owners(
    projection: &crate::architecture::project::Projection,
) -> Result<BTreeMap<&Identity, &ClusterIdentity>> {
    let mut owners = BTreeMap::new();
    for cluster in &projection.clusters.clusters {
        for node in &cluster.nodes {
            #[cfg(feature = "architecture-test-seams")]
            observe_validation(ValidationOperation::Owner);
            if owners.insert(node, &cluster.identity).is_some() {
                bail!("cluster ownership overlaps");
            }
        }
    }
    Ok(owners)
}

fn cluster_owner<'a>(
    explicit: &BTreeMap<&'a Identity, &'a ClusterIdentity>,
    projection: crate::architecture::schema::ProjectionName,
    node: &Identity,
) -> Result<Cow<'a, ClusterIdentity>> {
    explicit.get(node).copied().map_or_else(
        || implicit_cluster(projection, node).map(Cow::Owned),
        |owner| Ok(Cow::Borrowed(owner)),
    )
}

struct ClusterValidation<'a> {
    explicit_owners: BTreeMap<&'a Identity, &'a ClusterIdentity>,
    admission_prefix_bytes: usize,
    admission_bytes: usize,
}

fn validate_clusters<'a>(
    projection: &'a crate::architecture::project::Projection,
    decomposition: &Decomposition<'_>,
    nodes: &NodeIndex<'_>,
    budget: &mut ProjectionBudget,
) -> Result<ClusterValidation<'a>> {
    let edges = decomposition.edges();
    let member_counting_work = projection
        .clusters
        .clusters
        .len()
        .checked_add(projection.components.len())
        .context("RESOURCE_BUDGET_EXCEEDED: cluster member counting cost overflowed")?;
    budget.charge(
        projection.spec.name,
        "cluster-member-counting",
        member_counting_work,
    )?;
    let explicit_members =
        projection
            .clusters
            .clusters
            .iter()
            .try_fold(0usize, |sum, cluster| {
                sum.checked_add(cluster.nodes.len())
                    .context("RESOURCE_BUDGET_EXCEEDED: explicit owner count overflowed")
            })?;
    let component_members = projection
        .components
        .iter()
        .try_fold(0usize, |sum, component| {
            sum.checked_add(component.members.len())
                .context("RESOURCE_BUDGET_EXCEEDED: component member count overflowed")
        })?;
    let member_work = projection
        .clusters
        .clusters
        .len()
        .checked_mul(4)
        .and_then(|cost| {
            explicit_members
                .checked_mul(6)
                .and_then(|value| cost.checked_add(value))
        })
        .and_then(|cost| {
            component_members
                .checked_mul(4)
                .and_then(|value| cost.checked_add(value))
        })
        .context("RESOURCE_BUDGET_EXCEEDED: cluster member validation cost overflowed")?;
    budget.charge(
        projection.spec.name,
        "cluster-member-validation",
        member_work,
    )?;
    let admission_prefix_bytes = budget.bytes_reserved();
    let admission_bytes =
        admit_cluster_validation_bytes(projection, edges, explicit_members, budget)?;
    let explicit_owners = explicit_cluster_owners(projection)?;
    let component_by_node = projection
        .components
        .iter()
        .flat_map(|component| {
            component.members.iter().map(|node| {
                #[cfg(feature = "architecture-test-seams")]
                observe_validation(ValidationOperation::Member);
                (node, &component.identity)
            })
        })
        .collect::<BTreeMap<_, _>>();
    let required_explicit = projection
        .components
        .iter()
        .flat_map(|component| component.members.iter())
        .inspect(|_| {
            #[cfg(feature = "architecture-test-seams")]
            observe_validation(ValidationOperation::Member);
        })
        .collect::<BTreeSet<_>>();
    let actual_explicit = projection
        .clusters
        .clusters
        .iter()
        .flat_map(|cluster| cluster.nodes.iter())
        .inspect(|_| {
            #[cfg(feature = "architecture-test-seams")]
            observe_validation(ValidationOperation::Member);
        })
        .collect::<BTreeSet<_>>();
    if required_explicit != actual_explicit {
        bail!("CLUSTER_CUT_TOTALITY_MISMATCH: explicit SCC cluster ownership differs");
    }
    budget.charge(
        projection.spec.name,
        "cluster-incidence-counting",
        projection.clusters.clusters.len(),
    )?;
    let edge_records = projection
        .clusters
        .clusters
        .iter()
        .try_fold(0usize, |sum, cluster| {
            sum.checked_add(cluster.edge_records.len())
                .context("RESOURCE_BUDGET_EXCEEDED: cluster incidence count overflowed")
        })?;
    let incidence_work = edges
        .len()
        .checked_mul(6)
        .and_then(|cost| {
            edge_records
                .checked_mul(3)
                .and_then(|value| cost.checked_add(value))
        })
        .and_then(|cost| {
            projection
                .clusters
                .clusters
                .len()
                .checked_mul(2)
                .and_then(|value| cost.checked_add(value))
        })
        .context("RESOURCE_BUDGET_EXCEEDED: cluster incidence validation cost overflowed")?;
    budget.charge(
        projection.spec.name,
        "cluster-incidence-validation",
        incidence_work,
    )?;
    let mut expected_incidence = projection
        .clusters
        .clusters
        .iter()
        .map(|cluster| (&cluster.identity, Vec::new()))
        .collect::<BTreeMap<_, Vec<_>>>();
    for edge in edges {
        #[cfg(feature = "architecture-test-seams")]
        observe_validation(ValidationOperation::Incidence);
        let from = cluster_owner(&explicit_owners, projection.spec.name, &edge.record.from)?;
        let to = cluster_owner(&explicit_owners, projection.spec.name, &edge.record.to)?;
        if let Some(records) = expected_incidence.get_mut(from.as_ref()) {
            records.push(crate::architecture::project::cluster::TypedEdgeRecord {
                role: if from == to {
                    crate::architecture::project::cluster::CutRole::InternalOwner
                } else {
                    crate::architecture::project::cluster::CutRole::OutgoingOwner
                },
                edge: edge.identity.clone(),
            });
        }
        if from != to
            && let Some(records) = expected_incidence.get_mut(to.as_ref())
        {
            records.push(crate::architecture::project::cluster::TypedEdgeRecord {
                role: crate::architecture::project::cluster::CutRole::IncomingReference,
                edge: edge.identity.clone(),
            });
        }
    }
    for records in expected_incidence.values_mut() {
        #[cfg(feature = "architecture-test-seams")]
        observe_validation(ValidationOperation::Incidence);
        records.sort();
    }
    for cluster in &projection.clusters.clusters {
        #[cfg(feature = "architecture-test-seams")]
        observe_validation(ValidationOperation::Member);
        if cluster.nodes.is_empty() || cluster.nodes.len() > 32 {
            bail!("RESOURCE_BUDGET_EXCEEDED: invalid shard node count");
        }
        for node in &cluster.nodes {
            #[cfg(feature = "architecture-test-seams")]
            observe_validation(ValidationOperation::Member);
            if !nodes.contains_key(node) {
                bail!("cluster ownership is absent");
            }
        }
        if cluster.nodes.iter().any(|node| {
            component_by_node
                .get(node)
                .is_none_or(|owner| **owner != cluster.component)
        }) {
            bail!("SCC_TOTALITY_MISMATCH: cluster names the wrong complete SCC");
        }
        budget.charge(
            projection.spec.name,
            "cluster-signature-sizing",
            cluster.nodes.len(),
        )?;
        let signature_work = cluster.nodes.iter().try_fold(0usize, |cost, node| {
            nodes
                .get(node)
                .context("cluster ownership is absent")?
                .attributes
                .len()
                .checked_add(1)
                .and_then(|value| cost.checked_add(value))
                .context("RESOURCE_BUDGET_EXCEEDED: cluster signature cost overflowed")
        })?;
        budget.charge(
            projection.spec.name,
            "cluster-signature-validation",
            signature_work,
        )?;
        let signatures = cluster
            .nodes
            .iter()
            .map(|node| {
                #[cfg(feature = "architecture-test-seams")]
                observe_validation(ValidationOperation::Signature);
                hard_signature(nodes[node])
            })
            .collect::<BTreeSet<_>>();
        if signatures.len() != 1 {
            bail!("DYNAMIC_SHARD_PARTITION_ABSENT: incompatible hard signatures merged");
        }
        let expected_records = expected_incidence
            .get(&cluster.identity)
            .context("CLUSTER_CUT_TOTALITY_MISMATCH: explicit cluster incidence is absent")?;
        if &cluster.edge_records != expected_records {
            bail!("CLUSTER_CUT_TOTALITY_MISMATCH: internal/outgoing/incoming incidence differs");
        }
        let internal = cluster
            .edge_records
            .iter()
            .filter(|record| {
                record.role == crate::architecture::project::cluster::CutRole::InternalOwner
            })
            .map(|record| {
                #[cfg(feature = "architecture-test-seams")]
                observe_validation(ValidationOperation::Incidence);
                edges
                    .binary_search_by(|edge| edge.identity.cmp(&record.edge))
                    .map(|index| edges[index])
                    .map_err(|_| anyhow::anyhow!("cluster cut names absent edge"))
            })
            .collect::<Result<Vec<_>>>()?;
        if internal.len() > 120 {
            bail!("RESOURCE_BUDGET_EXCEEDED: shard exceeds internal-edge hard limit");
        }
        let connectivity_work = cluster
            .nodes
            .len()
            .checked_mul(
                internal
                    .len()
                    .checked_mul(2)
                    .and_then(|value| value.checked_add(2))
                    .context("RESOURCE_BUDGET_EXCEEDED: connectivity edge cost overflowed")?,
            )
            .context("RESOURCE_BUDGET_EXCEEDED: connectivity validation cost overflowed")?;
        budget.charge(
            projection.spec.name,
            "cluster-connectivity-validation",
            connectivity_work,
        )?;
        let member_set = cluster
            .nodes
            .iter()
            .map(|node| {
                #[cfg(feature = "architecture-test-seams")]
                observe_validation(ValidationOperation::Member);
                node.clone()
            })
            .collect::<BTreeSet<_>>();
        validate_connected_cluster(&member_set, &internal)?;
        let (rank, cycles) =
            basis_for_subgraph(projection.spec.name, &member_set, &internal, budget)?;
        if cluster.internal_basis.rank != rank || cluster.internal_basis.cycles != cycles {
            bail!("CYCLE_BASIS_INCOMPLETE: shard-local basis is not exact");
        }
        let expected = Sha256Digest::new(crate::architecture::hash::hash_serialized(&(
            &cluster.identity,
            &cluster.component,
            &cluster.nodes,
            &cluster.signals,
            &cluster.edge_records,
            &cluster.internal_basis,
            cluster.movement_authorized,
        ))?)?;
        if cluster.hash != expected {
            bail!("shard hash omitted a node edge basis signal or authorization field");
        }
    }
    Ok(ClusterValidation {
        explicit_owners,
        admission_prefix_bytes,
        admission_bytes,
    })
}

fn admit_cluster_validation_bytes(
    projection: &crate::architecture::project::Projection,
    edges: &[&EdgeFact],
    explicit_members: usize,
    budget: &mut ProjectionBudget,
) -> Result<usize> {
    let clusters = projection.clusters.clusters.len();
    let sizing_work = clusters
        .checked_add(edges.len())
        .context("RESOURCE_BUDGET_EXCEEDED: cluster serialized sizing cost overflowed")?;
    budget.charge(
        projection.spec.name,
        "cluster-validation-serialized-sizing",
        sizing_work,
    )?;
    let fixed = clusters
        .checked_mul(std::mem::size_of::<ClusterIdentity>() * 3)
        .and_then(|bytes| {
            explicit_members
                .checked_mul(std::mem::size_of::<(&Identity, &ClusterIdentity)>() * 2)
                .and_then(|owner_bytes| bytes.checked_add(owner_bytes))
        })
        .and_then(|bytes| {
            edges
                .len()
                .checked_mul(
                    std::mem::size_of::<&EdgeFact>()
                        + std::mem::size_of::<
                            crate::architecture::project::cluster::TypedEdgeRecord,
                        >() * 2,
                )
                .and_then(|edge_bytes| bytes.checked_add(edge_bytes))
        })
        .context("RESOURCE_BUDGET_EXCEEDED: cluster validation reservation overflowed")?;
    let ceiling = budget
        .available_bytes(projection.spec.name)?
        .checked_sub(fixed)
        .context("RESOURCE_BUDGET_EXCEEDED: cluster validation indexes exceed the byte budget")?;
    let aggregate = crate::architecture::hash::serialized_len_capped(
        &(&projection.clusters, edges),
        ceiling / 3,
    )
    .context("RESOURCE_BUDGET_EXCEEDED: cluster validation exceeds its serialized byte ceiling")?;
    let bytes = aggregate
        .checked_mul(3)
        .and_then(|value| value.checked_add(fixed))
        .context("RESOURCE_BUDGET_EXCEEDED: cluster validation reservation overflowed")?;
    budget.reserve(projection.spec.name, "cluster-validation-indexes", bytes)?;
    Ok(bytes)
}

fn hard_signature(node: &crate::architecture::schema::NodeFact) -> Vec<String> {
    let mut signature = Vec::new();
    if matches!(
        node.kind,
        NodeKind::Repository | NodeKind::Product | NodeKind::Worktree
    ) {
        signature.push(format!("owner-kind:{:?}", node.kind));
    }
    if let Identity::PackageFace { package_face } = &node.identity {
        signature.push(format!("package-face:{package_face}"));
    }
    for attribute in &node.attributes {
        #[cfg(feature = "architecture-test-seams")]
        observe_validation(ValidationOperation::Signature);
        match attribute {
            Attribute::Ownership { owner } => signature.push(format!("owner:{owner}")),
            Attribute::Feature { feature } => signature.push(format!("feature:{feature}")),
            Attribute::GeneratedLineage { lineage } => signature.push(format!("lineage:{lineage}")),
            Attribute::PackageFace { package_face } => {
                signature.push(format!("package-face:{package_face}"))
            }
            Attribute::TestReachability { reachable } => {
                signature.push(format!("test:{reachable}"))
            }
            Attribute::FrontendFeature { feature } => signature.push(format!("frontend:{feature}")),
            _ => {}
        }
    }
    signature.sort();
    signature.dedup();
    signature
}

fn validate_connected_cluster(members: &BTreeSet<Identity>, edges: &[&EdgeFact]) -> Result<()> {
    if members.len() < 2 {
        return Ok(());
    }
    let mut seen = BTreeSet::new();
    let mut pending = vec![members.iter().next().expect("cluster member").clone()];
    while let Some(node) = pending.pop() {
        #[cfg(feature = "architecture-test-seams")]
        observe_validation(ValidationOperation::Connectivity);
        if !seen.insert(node.clone()) {
            continue;
        }
        for edge in edges {
            #[cfg(feature = "architecture-test-seams")]
            observe_validation(ValidationOperation::Connectivity);
            let next = if edge.record.from == node {
                Some(&edge.record.to)
            } else if edge.record.to == node {
                Some(&edge.record.from)
            } else {
                None
            };
            if let Some(next) = next
                && members.contains(next)
                && !seen.contains(next)
            {
                pending.push(next.clone());
            }
        }
    }
    if &seen != members {
        bail!("DYNAMIC_SHARD_PARTITION_ABSENT: cluster is not graph-connected");
    }
    Ok(())
}

fn validate_quotient(
    projection: &crate::architecture::project::Projection,
    edges: &[&EdgeFact],
    explicit_owners: &BTreeMap<&Identity, &ClusterIdentity>,
    decomposition: &Decomposition<'_>,
    budget: &mut ProjectionBudget,
) -> Result<()> {
    budget.charge(
        projection.spec.name,
        "quotient-edge-counting",
        projection.quotient.edges.len(),
    )?;
    let quotient_edge_id_count =
        projection
            .quotient
            .edges
            .iter()
            .try_fold(0usize, |sum, edge| {
                sum.checked_add(edge.edges.len())
                    .context("RESOURCE_BUDGET_EXCEEDED: quotient edge count overflowed")
            })?;
    budget.charge(
        projection.spec.name,
        "quotient-cycle-counting",
        projection.quotient.bases.len(),
    )?;
    let quotient_cycle_count =
        projection
            .quotient
            .bases
            .iter()
            .try_fold(0usize, |sum, basis| {
                sum.checked_add(basis.cycles.len())
                    .context("RESOURCE_BUDGET_EXCEEDED: quotient cycle count overflowed")
            })?;
    budget.charge(
        projection.spec.name,
        "quotient-cycle-edge-counting",
        quotient_cycle_count,
    )?;
    let quotient_cycle_edge_count = projection
        .quotient
        .bases
        .iter()
        .flat_map(|basis| &basis.cycles)
        .try_fold(0usize, |sum, cycle| {
            sum.checked_add(cycle.edges.len())
                .context("RESOURCE_BUDGET_EXCEEDED: quotient cycle count overflowed")
        })?;
    budget.charge(
        projection.spec.name,
        "quotient-component-member-counting",
        decomposition.components().len(),
    )?;
    let component_members =
        decomposition
            .components()
            .iter()
            .try_fold(0usize, |sum, component| {
                sum.checked_add(component.members.len())
                    .context("RESOURCE_BUDGET_EXCEEDED: quotient member count overflowed")
            })?;
    budget.charge(
        projection.spec.name,
        "quotient-seed-counting",
        projection.clusters.clusters.len(),
    )?;
    let seed_cycles = projection
        .clusters
        .clusters
        .iter()
        .try_fold(0usize, |sum, cluster| {
            sum.checked_add(cluster.internal_basis.cycles.len())
                .context("RESOURCE_BUDGET_EXCEEDED: quotient seed count overflowed")
        })?;
    let quotient_work = projection
        .quotient
        .edges
        .len()
        .checked_mul(6)
        .and_then(|cost| {
            quotient_edge_id_count
                .checked_mul(3)
                .and_then(|value| cost.checked_add(value))
        })
        .and_then(|cost| {
            edges
                .len()
                .checked_mul(5)
                .and_then(|value| cost.checked_add(value))
        })
        .and_then(|cost| {
            projection
                .quotient
                .bases
                .len()
                .checked_mul(5)
                .and_then(|value| cost.checked_add(value))
        })
        .and_then(|cost| {
            quotient_cycle_edge_count
                .checked_mul(2)
                .and_then(|value| cost.checked_add(value))
        })
        .and_then(|cost| {
            component_members
                .checked_mul(4)
                .and_then(|value| cost.checked_add(value))
        })
        .and_then(|cost| {
            seed_cycles
                .checked_mul(2)
                .and_then(|value| cost.checked_add(value))
        })
        .context("RESOURCE_BUDGET_EXCEEDED: quotient validation cost overflowed")?;
    budget.charge(projection.spec.name, "quotient-validation", quotient_work)?;
    admit_quotient_validation_bytes(projection, edges, budget)?;
    let mut quotient_edges = BTreeMap::new();
    let mut actual_crossing_edges = 0usize;
    for edge in &projection.quotient.edges {
        #[cfg(feature = "architecture-test-seams")]
        observe_validation(ValidationOperation::Quotient);
        if edge.from == edge.to
            || edge.edges.is_empty()
            || edge.edges.windows(2).any(|window| window[0] >= window[1])
            || quotient_edges
                .insert((&edge.from, &edge.to), edge)
                .is_some()
        {
            bail!("BOUNDARY_QUOTIENT_ABSENT: quotient contains a duplicate or internal edge");
        }
        actual_crossing_edges += edge.edges.len();
    }
    let mut expected_crossing_edges = 0usize;
    for edge in edges {
        #[cfg(feature = "architecture-test-seams")]
        observe_validation(ValidationOperation::Quotient);
        let from = cluster_owner(explicit_owners, projection.spec.name, &edge.record.from)?;
        let to = cluster_owner(explicit_owners, projection.spec.name, &edge.record.to)?;
        if from != to {
            expected_crossing_edges += 1;
            let quotient = quotient_edges
                .get(&(from.as_ref(), to.as_ref()))
                .context("BOUNDARY_QUOTIENT_ABSENT: crossing pair is absent")?;
            if quotient.edges.binary_search(&edge.identity).is_err() {
                bail!("BOUNDARY_QUOTIENT_ABSENT: selected crossing edge is absent");
            }
        }
    }
    if actual_crossing_edges != expected_crossing_edges {
        bail!("BOUNDARY_QUOTIENT_ABSENT: quotient does not reconcile selected edges");
    }
    let mut quotient_bases = BTreeMap::new();
    for basis in &projection.quotient.bases {
        #[cfg(feature = "architecture-test-seams")]
        observe_validation(ValidationOperation::Quotient);
        if quotient_bases.insert(&basis.component, basis).is_some() {
            bail!("CYCLE_BASIS_INCOMPLETE: duplicate quotient component basis");
        }
    }
    let mut validated_bases = BTreeSet::new();
    for component in decomposition
        .components()
        .iter()
        .filter(|component| component.members.len() > 1 || component.cyclic)
    {
        #[cfg(feature = "architecture-test-seams")]
        observe_validation(ValidationOperation::Quotient);
        let members = component
            .members
            .iter()
            .inspect(|_| {
                #[cfg(feature = "architecture-test-seams")]
                observe_validation(ValidationOperation::Quotient);
            })
            .collect::<Vec<_>>();
        let component_edges = decomposition.internal_edges(&component.identity)?;
        let seed = projection
            .clusters
            .clusters
            .iter()
            .filter(|cluster| cluster.component == component.identity)
            .flat_map(|cluster| cluster.internal_basis.cycles.iter().cloned())
            .inspect(|_| {
                #[cfg(feature = "architecture-test-seams")]
                observe_validation(ValidationOperation::Quotient);
            })
            .collect::<Vec<_>>();
        let (complement, rank) = quotient_bases
            .get(&component.identity)
            .map_or((&[][..], 0), |basis| (basis.cycles.as_slice(), basis.rank));
        validate_cycle_completion_refs(
            projection.spec.name,
            &members,
            component_edges,
            &seed,
            complement,
            rank,
            budget,
        )?;
        if quotient_bases.contains_key(&component.identity) {
            validated_bases.insert(component.identity.clone());
        }
    }
    if validated_bases.len() != quotient_bases.len() {
        bail!("CYCLE_BASIS_INCOMPLETE: quotient basis component is absent");
    }
    let expected_hash = Sha256Digest::new(crate::architecture::hash::hash_serialized(&(
        &projection.quotient.schema,
        &projection.quotient.edges,
        &projection.quotient.bases,
        projection.quotient.movement_authorized,
    ))?)?;
    if projection.quotient.hash != expected_hash {
        bail!("boundary quotient hash is incomplete");
    }
    Ok(())
}

fn admit_quotient_validation_bytes(
    projection: &crate::architecture::project::Projection,
    edges: &[&EdgeFact],
    budget: &mut ProjectionBudget,
) -> Result<()> {
    let sizing_work = projection
        .quotient
        .edges
        .len()
        .checked_add(projection.quotient.bases.len())
        .and_then(|work| work.checked_add(edges.len()))
        .context("RESOURCE_BUDGET_EXCEEDED: quotient serialized sizing cost overflowed")?;
    budget.charge(
        projection.spec.name,
        "quotient-validation-serialized-sizing",
        sizing_work,
    )?;
    let fixed = projection
        .quotient
        .edges
        .len()
        .checked_mul(
            std::mem::size_of::<crate::architecture::project::quotient::QuotientEdge>() * 2,
        )
        .and_then(|bytes| {
            projection
                .quotient
                .bases
                .len()
                .checked_mul(
                    std::mem::size_of::<crate::architecture::project::quotient::QuotientBasis>()
                        * 2,
                )
                .and_then(|basis_bytes| bytes.checked_add(basis_bytes))
        })
        .and_then(|bytes| {
            edges
                .len()
                .checked_mul(std::mem::size_of::<&EdgeFact>() * 2)
                .and_then(|edge_bytes| bytes.checked_add(edge_bytes))
        })
        .context("RESOURCE_BUDGET_EXCEEDED: quotient validation reservation overflowed")?;
    let ceiling = budget
        .available_bytes(projection.spec.name)?
        .checked_sub(fixed)
        .context("RESOURCE_BUDGET_EXCEEDED: quotient validation indexes exceed the byte budget")?;
    let aggregate = crate::architecture::hash::serialized_len_capped(
        &(&projection.quotient, edges),
        ceiling / 3,
    )
    .context("RESOURCE_BUDGET_EXCEEDED: quotient validation exceeds its serialized byte ceiling")?;
    budget.reserve(
        projection.spec.name,
        "quotient-validation-indexes",
        aggregate
            .checked_mul(3)
            .and_then(|value| value.checked_add(fixed))
            .context("RESOURCE_BUDGET_EXCEEDED: quotient validation reservation overflowed")?,
    )
}

fn implicit_cluster(
    projection: crate::architecture::schema::ProjectionName,
    node: &Identity,
) -> Result<ClusterIdentity> {
    let component = ComponentIdentity {
        projection,
        digest: Sha256Digest::new(crate::architecture::hash::hash_serialized(&(
            projection,
            std::slice::from_ref(node),
        ))?)?,
    };
    Ok(ClusterIdentity {
        projection,
        digest: Sha256Digest::new(crate::architecture::hash::hash_serialized(&(
            "cluster-v3.1",
            projection,
            component,
            std::slice::from_ref(node),
        ))?)?,
    })
}
