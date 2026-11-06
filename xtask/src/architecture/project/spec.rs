use std::collections::BTreeSet;

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};

use crate::architecture::project::cluster::{ClusterSet, cluster};
use crate::architecture::project::cycle::{ComponentBasis, component_bases};
use crate::architecture::project::quotient::{Quotient, build as build_quotient};
use crate::architecture::project::scc::{Component, ProjectionBudget, decompose};
use crate::architecture::schema::{
    CanonicalText, EdgeIdentity, EdgeKind, GraphArtifact, ProjectionName, RequiredFailure,
    Sha256Digest,
};

pub const PROJECTION_SCHEMA: &str = "bbnf.architecture.projection.v3.1";
pub const PROJECTION_SET_SCHEMA: &str = "bbnf.architecture.projections.v3.1";
pub const IMPLICIT_SINGLETONS: &str = "nodes absent from explicit components and clusters expand as deterministic projection-qualified singletons";

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ProjectionSpec {
    pub name: ProjectionName,
    pub version: u32,
    pub edge_kinds: Vec<EdgeKind>,
    pub required_total: bool,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Projection {
    pub schema: CanonicalText,
    pub spec: ProjectionSpec,
    pub node_universe_hash: Sha256Digest,
    pub node_count: usize,
    pub implicit_singletons: CanonicalText,
    pub edge_ids: Vec<EdgeIdentity>,
    pub components: Vec<Component>,
    pub component_bases: Vec<ComponentBasis>,
    pub clusters: ClusterSet,
    pub quotient: Quotient,
    pub required_failures: Vec<RequiredFailure>,
    pub structurally_valid_for_selected_edges: bool,
    pub movement_authorized: bool,
    pub hash: Sha256Digest,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ProjectionSet {
    pub schema: CanonicalText,
    pub node_universe_hash: Sha256Digest,
    pub node_count: usize,
    pub projections: Vec<Projection>,
}

pub fn project(graph: &GraphArtifact) -> Result<ProjectionSet> {
    graph.validate_canonical()?;
    crate::architecture::evidence::write::failpoint("projection-build")?;
    derive(graph, true)
}

pub(crate) fn rebuild(graph: &GraphArtifact) -> Result<ProjectionSet> {
    graph.validate_canonical()?;
    derive(graph, false)
}

fn derive(graph: &GraphArtifact, inject: bool) -> Result<ProjectionSet> {
    let mut plans = projection_specs()
        .into_iter()
        .map(|spec| {
            let mut budget = ProjectionBudget::standard(spec.name);
            admit_projection_input(graph, &spec, &mut budget)?;
            Ok((spec, budget))
        })
        .collect::<Result<Vec<_>>>()?;
    let node_map = graph.node_map();
    let vertices = node_map.keys().cloned().collect::<BTreeSet<_>>();
    let node_universe_hash = Sha256Digest::new(crate::architecture::hash::hash_serialized(
        &node_map.values().collect::<Vec<_>>(),
    )?)?;
    let failures = graph.required_failures();
    let mut projections = Vec::new();
    for (spec, mut budget) in plans.drain(..) {
        let allowed = spec.edge_kinds.iter().copied().collect::<BTreeSet<_>>();
        let edges = graph
            .edges()
            .into_iter()
            .filter(|edge| allowed.contains(&edge.record.kind))
            .cloned()
            .collect::<Vec<_>>();
        let decomposition = decompose(spec.name, &vertices, &edges, &mut budget)?;
        if inject {
            crate::architecture::evidence::write::stage_injection(
                crate::architecture::schema::PipelineStage::Cycle,
            )
            .context("PIPELINE_STAGE_CYCLE")?;
            crate::architecture::evidence::write::failpoint("cycle-basis")?;
        }
        let bases = component_bases(&decomposition, &mut budget).context("PIPELINE_STAGE_CYCLE")?;
        if inject {
            crate::architecture::evidence::write::stage_injection(
                crate::architecture::schema::PipelineStage::Cluster,
            )
            .context("PIPELINE_STAGE_CLUSTER")?;
            crate::architecture::evidence::write::failpoint("dynamic-cluster")?;
        }
        let cluster_build =
            cluster(&decomposition, &node_map, &mut budget).context("PIPELINE_STAGE_CLUSTER")?;
        if inject {
            crate::architecture::evidence::write::failpoint("quotient")?;
        }
        let quotient = build_quotient(&cluster_build, &decomposition, &bases, &mut budget)
            .context("PIPELINE_STAGE_CLUSTER")?;
        admit_projection_output(
            &spec,
            &decomposition,
            &bases,
            &cluster_build,
            &quotient,
            &mut budget,
        )?;
        let components = decomposition
            .into_components()
            .into_iter()
            .filter(|component| component.members.len() > 1 || component.cyclic)
            .collect::<Vec<_>>();
        let required_failures = failures
            .iter()
            .filter(|failure| failure.projection == spec.name)
            .cloned()
            .collect::<Vec<_>>();
        let edge_ids = edges
            .iter()
            .map(|edge| edge.identity.clone())
            .collect::<Vec<_>>();
        let structurally_valid_for_selected_edges = true;
        let movement_authorized = false;
        let schema = CanonicalText::new(PROJECTION_SCHEMA)?;
        let implicit_singletons = CanonicalText::new(IMPLICIT_SINGLETONS)?;
        let clusters = cluster_build.into_set();
        let hash = Sha256Digest::new(crate::architecture::hash::hash_serialized(&(
            &schema,
            &spec,
            &node_universe_hash,
            node_map.len(),
            &implicit_singletons,
            &edge_ids,
            &components,
            &bases,
            &clusters,
            &quotient,
            &required_failures,
            structurally_valid_for_selected_edges,
            movement_authorized,
        ))?)?;
        projections.push(Projection {
            schema,
            spec,
            node_universe_hash: node_universe_hash.clone(),
            node_count: node_map.len(),
            implicit_singletons,
            edge_ids,
            components,
            component_bases: bases,
            clusters,
            quotient,
            required_failures,
            structurally_valid_for_selected_edges,
            movement_authorized,
            hash,
        });
    }
    projections.sort_by(|left, right| left.spec.cmp(&right.spec));
    Ok(ProjectionSet {
        schema: CanonicalText::new(PROJECTION_SET_SCHEMA)?,
        node_universe_hash,
        node_count: node_map.len(),
        projections,
    })
}

fn admit_projection_input(
    graph: &GraphArtifact,
    spec: &ProjectionSpec,
    budget: &mut ProjectionBudget,
) -> Result<()> {
    let fixed = graph
        .truth
        .len()
        .checked_mul(std::mem::size_of::<crate::architecture::schema::Truth>() * 2)
        .and_then(|bytes| {
            spec.edge_kinds
                .len()
                .checked_mul(std::mem::size_of::<EdgeKind>() * 2)
                .and_then(|spec_bytes| bytes.checked_add(spec_bytes))
        })
        .context("RESOURCE_BUDGET_EXCEEDED: projection input reservation overflowed")?;
    let ceiling = budget
        .available_bytes(spec.name)?
        .checked_sub(fixed)
        .context("RESOURCE_BUDGET_EXCEEDED: projection input indexes exceed the byte budget")?
        .checked_div(2)
        .context("RESOURCE_BUDGET_EXCEEDED: projection input serialized divisor is zero")?;
    let aggregate = crate::architecture::hash::serialized_len_capped(&(spec, graph), ceiling)
        .context(
            "RESOURCE_BUDGET_EXCEEDED: projection input exceeds its serialized byte ceiling",
        )?;
    budget.reserve(
        spec.name,
        "projection-input",
        aggregate
            .checked_mul(2)
            .and_then(|bytes| bytes.checked_add(fixed))
            .context("RESOURCE_BUDGET_EXCEEDED: projection input reservation overflowed")?,
    )
}

fn admit_projection_output(
    spec: &ProjectionSpec,
    decomposition: &crate::architecture::project::scc::Decomposition<'_>,
    bases: &[ComponentBasis],
    clusters: &crate::architecture::project::cluster::ClusterBuild,
    quotient: &Quotient,
    budget: &mut ProjectionBudget,
) -> Result<()> {
    let fixed = decomposition
        .components()
        .len()
        .checked_mul(std::mem::size_of::<Component>() * 2)
        .and_then(|bytes| {
            decomposition
                .edges()
                .len()
                .checked_mul(std::mem::size_of::<EdgeIdentity>() * 2)
                .and_then(|edge_bytes| bytes.checked_add(edge_bytes))
        })
        .and_then(|bytes| {
            bases
                .len()
                .checked_mul(std::mem::size_of::<ComponentBasis>() * 2)
                .and_then(|basis_bytes| bytes.checked_add(basis_bytes))
        })
        .context("RESOURCE_BUDGET_EXCEEDED: projection output reservation overflowed")?;
    let ceiling = budget
        .available_bytes(spec.name)?
        .checked_sub(fixed)
        .context("RESOURCE_BUDGET_EXCEEDED: projection output vectors exceed the byte budget")?
        .checked_div(2)
        .context("RESOURCE_BUDGET_EXCEEDED: projection output serialized divisor is zero")?;
    let aggregate = crate::architecture::hash::serialized_len_capped(
        &(
            spec,
            decomposition.components(),
            bases,
            clusters.set(),
            quotient,
        ),
        ceiling,
    )
    .context("RESOURCE_BUDGET_EXCEEDED: projection output exceeds its serialized byte ceiling")?;
    budget.reserve(
        spec.name,
        "projection-output",
        aggregate
            .checked_mul(2)
            .and_then(|bytes| bytes.checked_add(fixed))
            .context("RESOURCE_BUDGET_EXCEEDED: projection output reservation overflowed")?,
    )
}

pub fn projection_specs() -> Vec<ProjectionSpec> {
    use EdgeKind as Kind;
    use ProjectionName as Name;
    let rows = [
        (
            Name::Containment,
            vec![
                Kind::RepositoryContainsProduct,
                Kind::RepositoryContainsWorkspace,
                Kind::ProductContainsWorkspace,
                Kind::WorkspaceContainsPackageFace,
                Kind::ProductContainsPackageFace,
                Kind::RepositoryContainsContext,
                Kind::WorkspaceContainsContext,
                Kind::ContextUsesPackageFace,
                Kind::ProductContainsBackend,
                Kind::PackageFaceRoot,
                Kind::PackageFaceManifest,
            ],
        ),
        (
            Name::RustModule,
            vec![
                Kind::RustModuleContains,
                Kind::RustUses,
                Kind::RustReexports,
                Kind::BackendConsumesIr,
                Kind::ConstraintEquates,
                Kind::TypeDerivedFromGrammarModule,
            ],
        ),
        (Name::CargoDeclared, vec![Kind::CargoDeclares]),
        (
            Name::CargoResolved,
            vec![Kind::CargoResolves, Kind::CargoActivatesFeature],
        ),
        (
            Name::FrontendBuild,
            vec![
                Kind::ContextUsesManifest,
                Kind::ContextUsesLock,
                Kind::ContextUsesConfig,
                Kind::FrontendRuntimeImport,
                Kind::FrontendTypeImport,
                Kind::FrontendDynamicImport,
                Kind::FrontendConfigures,
                Kind::CssImports,
                Kind::CssLoadsAsset,
            ],
        ),
        (
            Name::RuntimeDelivery,
            vec![
                Kind::FrontendRuntimeImport,
                Kind::FrontendDynamicImport,
                Kind::CssImports,
                Kind::CssLoadsAsset,
                Kind::WasmLoads,
            ],
        ),
        (Name::TestOverlay, vec![Kind::RustTestUses]),
        (
            Name::GeneratedProvenance,
            vec![
                Kind::Generates,
                Kind::GeneratedFrom,
                Kind::GrammarDerivesRegexHir,
                Kind::TypeDerivedFromGrammarModule,
                Kind::BackendProducesArtifact,
            ],
        ),
        (
            Name::Ownership,
            vec![
                Kind::RepositoryOwnsPhysical,
                Kind::RepositoryOwnsWorktree,
                Kind::ProductNominatesWorktree,
            ],
        ),
        (
            Name::Call,
            vec![
                Kind::Calls,
                Kind::AssemblyImplementsKernel,
                Kind::SharesCostGraph,
            ],
        ),
        (
            Name::Documentation,
            vec![Kind::DocumentationSupersedes, Kind::DocumentationPhysical],
        ),
    ];
    rows.into_iter()
        .map(|(name, edge_kinds)| ProjectionSpec {
            name,
            version: 31,
            edge_kinds,
            required_total: true,
        })
        .collect()
}
