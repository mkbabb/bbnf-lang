use std::collections::{BTreeMap, BTreeSet};

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};

use crate::architecture::hash::BoundedKernelError;
use crate::architecture::hash::hash_serialized;

use super::{
    CanonicalText, ContextId, DocumentationId, EdgeIdentity, Identity, Obligation, PackageFaceId,
    PhysicalKind, RepoPath, RepositoryId, RequiredFailure, ResolverId, Sha256Digest,
};

pub const GRAPH_SCHEMA: &str = "bbnf.architecture.graph.v3.1";
const MAX_GRAPH_TRUTH: usize = 500_000;
const MAX_GRAPH_SERIALIZED_BYTES: usize = 512 * 1024 * 1024;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum NodeKind {
    Repository,
    Product,
    Workspace,
    PackageFace,
    Context,
    Worktree,
    Documentation,
    PhysicalFile,
    PhysicalDirectory,
    PhysicalSymlink,
    VirtualArtifact,
    Backend,
    IntermediateRepresentation,
    CostGraph,
    ConstraintSystem,
    TypeModel,
    AssemblyKernel,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RelationClass {
    Containment,
    DeclaredDependency,
    ResolvedBuild,
    Runtime,
    TypeOnly,
    Test,
    Configuration,
    Asset,
    GeneratedProvenance,
    Ownership,
    Call,
    Documentation,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum EdgeKind {
    RepositoryContainsProduct,
    RepositoryContainsWorkspace,
    ProductContainsWorkspace,
    WorkspaceContainsPackageFace,
    ProductContainsPackageFace,
    RepositoryContainsContext,
    WorkspaceContainsContext,
    ContextUsesPackageFace,
    ContextUsesManifest,
    ContextUsesLock,
    ContextUsesConfig,
    PackageFaceRoot,
    PackageFaceManifest,
    RepositoryOwnsPhysical,
    RepositoryOwnsWorktree,
    ProductNominatesWorktree,
    DocumentationSupersedes,
    DocumentationPhysical,
    CargoDeclares,
    CargoResolves,
    CargoActivatesFeature,
    RustModuleContains,
    RustUses,
    RustReexports,
    RustTestUses,
    FrontendRuntimeImport,
    FrontendTypeImport,
    FrontendDynamicImport,
    FrontendConfigures,
    CssImports,
    CssLoadsAsset,
    Generates,
    GeneratedFrom,
    WasmLoads,
    Calls,
    ProductContainsBackend,
    BackendConsumesIr,
    GrammarDerivesRegexHir,
    SharesCostGraph,
    ConstraintEquates,
    TypeDerivedFromGrammarModule,
    AssemblyImplementsKernel,
    BackendProducesArtifact,
}

impl EdgeKind {
    pub const fn class(self) -> RelationClass {
        match self {
            Self::RepositoryContainsProduct
            | Self::RepositoryContainsWorkspace
            | Self::ProductContainsWorkspace
            | Self::WorkspaceContainsPackageFace
            | Self::ProductContainsPackageFace
            | Self::RepositoryContainsContext
            | Self::WorkspaceContainsContext
            | Self::ContextUsesPackageFace
            | Self::ProductContainsBackend
            | Self::PackageFaceRoot
            | Self::PackageFaceManifest => RelationClass::Containment,
            Self::ContextUsesManifest | Self::ContextUsesLock | Self::ContextUsesConfig => {
                RelationClass::Configuration
            }
            Self::RepositoryOwnsPhysical
            | Self::RepositoryOwnsWorktree
            | Self::ProductNominatesWorktree => RelationClass::Ownership,
            Self::DocumentationSupersedes | Self::DocumentationPhysical => {
                RelationClass::Documentation
            }
            Self::CargoDeclares => RelationClass::DeclaredDependency,
            Self::BackendConsumesIr | Self::SharesCostGraph => RelationClass::DeclaredDependency,
            Self::CargoResolves
            | Self::CargoActivatesFeature
            | Self::RustModuleContains
            | Self::RustUses
            | Self::RustReexports => RelationClass::ResolvedBuild,
            Self::RustTestUses => RelationClass::Test,
            Self::FrontendRuntimeImport | Self::FrontendDynamicImport | Self::WasmLoads => {
                RelationClass::Runtime
            }
            Self::FrontendTypeImport => RelationClass::TypeOnly,
            Self::FrontendConfigures => RelationClass::Configuration,
            Self::CssImports | Self::CssLoadsAsset => RelationClass::Asset,
            Self::Generates | Self::GeneratedFrom => RelationClass::GeneratedProvenance,
            Self::GrammarDerivesRegexHir
            | Self::TypeDerivedFromGrammarModule
            | Self::BackendProducesArtifact => RelationClass::GeneratedProvenance,
            Self::ConstraintEquates => RelationClass::ResolvedBuild,
            Self::Calls | Self::AssemblyImplementsKernel => RelationClass::Call,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ProjectionName {
    Containment,
    RustModule,
    CargoDeclared,
    CargoResolved,
    FrontendBuild,
    RuntimeDelivery,
    TestOverlay,
    GeneratedProvenance,
    Ownership,
    Call,
    Documentation,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum Condition {
    Always,
    Cargo {
        context: ContextId,
        target: CanonicalText,
        profile: CanonicalText,
        features: Vec<CanonicalText>,
    },
    RustCfg {
        context: ContextId,
        expression: CanonicalText,
    },
    Frontend {
        context: ContextId,
        export_conditions: Vec<CanonicalText>,
        query: Option<CanonicalText>,
    },
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum Attribute {
    ContentHash {
        digest: Sha256Digest,
    },
    PackageFace {
        package_face: PackageFaceId,
    },
    PhysicalKind {
        physical_kind: PhysicalKind,
    },
    Context {
        context: ContextId,
    },
    Feature {
        feature: CanonicalText,
    },
    GeneratedLineage {
        lineage: CanonicalText,
    },
    TestReachability {
        reachable: bool,
    },
    FrontendFeature {
        feature: CanonicalText,
    },
    Ownership {
        owner: CanonicalText,
    },
    ContextTarget {
        target: CanonicalText,
        profile: CanonicalText,
    },
    DocumentationRole {
        role: CanonicalText,
    },
    WorktreeAdministrativeKey {
        key: CanonicalText,
    },
    WorktreeHead {
        head: CanonicalText,
    },
    WorktreeBranch {
        branch: CanonicalText,
    },
    WorktreeState {
        detached: bool,
        bare: bool,
        dirty: bool,
    },
    IgnoredContentPolicy {
        policy: CanonicalText,
    },
    Nomination {
        nomination: crate::architecture::schema::WorktreeId,
    },
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum SourceAnchor {
    File {
        repository: RepositoryId,
        path: RepoPath,
        start_line: u32,
        start_column: u32,
        end_line: u32,
        end_column: u32,
    },
    Manifest {
        repository: RepositoryId,
        path: RepoPath,
        key: CanonicalText,
    },
    Documentation {
        documentation: DocumentationId,
    },
    Synthesized {
        authority: CanonicalText,
    },
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct NodeFact {
    pub identity: Identity,
    pub kind: NodeKind,
    pub label: CanonicalText,
    pub attributes: Vec<Attribute>,
}

impl NodeFact {
    fn identity_kind_compatible(&self) -> bool {
        match &self.identity {
            Identity::Repository { .. } => self.kind == NodeKind::Repository,
            Identity::Product { .. } => self.kind == NodeKind::Product,
            Identity::Workspace { .. } => self.kind == NodeKind::Workspace,
            Identity::PackageFace { .. } => self.kind == NodeKind::PackageFace,
            Identity::Context { .. } => self.kind == NodeKind::Context,
            Identity::Worktree { .. } => self.kind == NodeKind::Worktree,
            Identity::Documentation { .. } => self.kind == NodeKind::Documentation,
            Identity::Physical { physical } => matches!(
                (&physical.kind, self.kind),
                (PhysicalKind::File, NodeKind::PhysicalFile)
                    | (PhysicalKind::Directory, NodeKind::PhysicalDirectory)
                    | (PhysicalKind::Symlink, NodeKind::PhysicalSymlink)
            ),
            Identity::Virtual { .. } => matches!(
                self.kind,
                NodeKind::VirtualArtifact
                    | NodeKind::Backend
                    | NodeKind::IntermediateRepresentation
                    | NodeKind::CostGraph
                    | NodeKind::ConstraintSystem
                    | NodeKind::TypeModel
                    | NodeKind::AssemblyKernel
            ),
        }
    }

    pub fn validate_identity_kind(&self) -> Result<()> {
        if !self.identity_kind_compatible() {
            bail!(
                "NODE_IDENTITY_KIND_MISMATCH: {:?} cannot have kind {:?}",
                self.identity,
                self.kind
            );
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EdgeRecord {
    pub from: Identity,
    pub to: Identity,
    pub kind: EdgeKind,
    pub class: RelationClass,
    pub condition: Condition,
    pub source: SourceAnchor,
    pub resolver: ResolverId,
    pub attributes: Vec<Attribute>,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EdgeFact {
    pub identity: EdgeIdentity,
    pub record: EdgeRecord,
}

impl EdgeFact {
    pub fn exact(record: EdgeRecord) -> Result<Self> {
        if record.class != record.kind.class() {
            bail!("edge relation class does not match its typed kind");
        }
        let digest = Sha256Digest::new(hash_serialized(&record)?)?;
        Ok(Self {
            identity: EdgeIdentity::from_digest(&digest),
            record,
        })
    }

    pub fn validate_identity(&self) -> Result<()> {
        let expected = Self::exact(self.record.clone())?.identity;
        if self.identity != expected {
            bail!("edge identity does not bind its complete record");
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum Fact {
    Node { node: NodeFact },
    Edge { edge: Box<EdgeFact> },
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(tag = "truth", rename_all = "snake_case", deny_unknown_fields)]
pub enum Truth {
    Exact { fact: Fact },
    Unknown { obligation: Obligation },
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct GraphArtifact {
    pub schema: CanonicalText,
    pub truth: Vec<Truth>,
}

impl GraphArtifact {
    pub fn new() -> Self {
        Self {
            schema: CanonicalText::new(GRAPH_SCHEMA).expect("schema"),
            truth: Vec::new(),
        }
    }

    pub fn exact(&mut self, fact: Fact) {
        self.truth.push(Truth::Exact { fact });
    }

    pub fn unknown(&mut self, obligation: Obligation) {
        self.truth.push(Truth::Unknown { obligation });
    }

    pub fn canonicalize(&mut self) -> Result<()> {
        self.validate_size()?;
        if self.schema.as_str() != GRAPH_SCHEMA {
            bail!("GRAPH_SCHEMA_MISMATCH: graph schema differs");
        }
        for truth in &mut self.truth {
            match truth {
                Truth::Exact {
                    fact: Fact::Node { node },
                } => {
                    node.attributes.sort();
                    node.attributes.dedup();
                }
                Truth::Exact {
                    fact: Fact::Edge { edge },
                } => {
                    edge.record.attributes.sort();
                    edge.record.attributes.dedup();
                    match &mut edge.record.condition {
                        Condition::Cargo { features, .. } => {
                            features.sort();
                            features.dedup();
                        }
                        Condition::Frontend {
                            export_conditions, ..
                        } => {
                            export_conditions.sort();
                            export_conditions.dedup();
                        }
                        Condition::Always | Condition::RustCfg { .. } => {}
                    }
                    edge.identity = EdgeFact::exact(edge.record.clone())?.identity;
                }
                Truth::Unknown { obligation } => {
                    obligation
                        .applicable_projections
                        .sort_by_key(|projection| projection_wire_name(*projection));
                    obligation.applicable_projections.dedup();
                }
            }
        }
        self.truth.sort();
        self.truth.dedup();
        self.validate_semantics()
    }

    fn validate_semantics(&self) -> Result<()> {
        self.validate_semantics_bounded()
            .map_err(anyhow::Error::new)
    }

    pub fn validate_canonical(&self) -> Result<()> {
        self.validate_canonical_with_limits(MAX_GRAPH_TRUTH, MAX_GRAPH_SERIALIZED_BYTES)
    }

    fn validate_canonical_with_limits(
        &self,
        max_truth: usize,
        max_serialized_bytes: usize,
    ) -> Result<()> {
        self.validate_canonical_bounded_with_limits(max_truth, max_serialized_bytes)
            .map_err(anyhow::Error::new)
    }

    fn validate_canonical_bounded_with_limits(
        &self,
        max_truth: usize,
        max_serialized_bytes: usize,
    ) -> std::result::Result<(), BoundedKernelError> {
        self.validate_size_bounded(max_truth, max_serialized_bytes)?;
        self.validate_canonical_after_size_bounded()
    }

    pub(crate) fn validate_canonical_bounded_with_serialized_length(
        &self,
        serialized_length: usize,
    ) -> std::result::Result<(), BoundedKernelError> {
        self.validate_size_bounded_with_serialized_length(
            MAX_GRAPH_TRUTH,
            MAX_GRAPH_SERIALIZED_BYTES,
            serialized_length,
        )?;
        self.validate_canonical_after_size_bounded()
    }

    fn validate_canonical_after_size_bounded(&self) -> std::result::Result<(), BoundedKernelError> {
        if self.schema.as_str() != GRAPH_SCHEMA {
            return Err(BoundedKernelError::Canonical {
                code: "GRAPH_SCHEMA_MISMATCH",
                first: 0,
                second: 0,
            });
        }
        require_strict_order_bounded(&self.truth, "GRAPH_NONCANONICAL: graph truth")?;
        validate_nested_order_bounded(self)?;
        self.validate_semantics_bounded()
    }

    fn validate_size(&self) -> Result<()> {
        self.validate_size_with_limits(MAX_GRAPH_TRUTH, MAX_GRAPH_SERIALIZED_BYTES)
    }

    fn validate_size_with_limits(
        &self,
        max_truth: usize,
        max_serialized_bytes: usize,
    ) -> Result<()> {
        if self.truth.len() > max_truth {
            bail!(
                "RESOURCE_BUDGET_EXCEEDED: graph has {} truth rows; limit is {max_truth}",
                self.truth.len()
            );
        }
        crate::architecture::hash::serialized_len_capped(self, max_serialized_bytes)
            .context("RESOURCE_BUDGET_EXCEEDED: graph exceeds its serialized byte limit")?;
        Ok(())
    }

    fn validate_size_bounded(
        &self,
        max_truth: usize,
        max_serialized_bytes: usize,
    ) -> std::result::Result<(), BoundedKernelError> {
        self.validate_truth_count_bounded(max_truth)?;
        let serialized = crate::architecture::hash::bounded_serialized_length(self)?;
        self.validate_serialized_length_bounded(max_serialized_bytes, serialized)
    }

    fn validate_truth_count_bounded(
        &self,
        max_truth: usize,
    ) -> std::result::Result<(), BoundedKernelError> {
        if self.truth.len() > max_truth {
            return Err(BoundedKernelError::Canonical {
                code: "RESOURCE_BUDGET_EXCEEDED: graph truth rows",
                first: self.truth.len(),
                second: max_truth,
            });
        }
        Ok(())
    }

    fn validate_size_bounded_with_serialized_length(
        &self,
        max_truth: usize,
        max_serialized_bytes: usize,
        serialized: usize,
    ) -> std::result::Result<(), BoundedKernelError> {
        self.validate_truth_count_bounded(max_truth)?;
        self.validate_serialized_length_bounded(max_serialized_bytes, serialized)
    }

    fn validate_serialized_length_bounded(
        &self,
        max_serialized_bytes: usize,
        serialized: usize,
    ) -> std::result::Result<(), BoundedKernelError> {
        if serialized > max_serialized_bytes {
            return Err(BoundedKernelError::Canonical {
                code: "RESOURCE_BUDGET_EXCEEDED: graph serialized bytes",
                first: serialized,
                second: max_serialized_bytes,
            });
        }
        Ok(())
    }

    fn validate_semantics_bounded(&self) -> std::result::Result<(), BoundedKernelError> {
        let nodes = self.nodes();
        for (index, node) in nodes.iter().enumerate() {
            if !node.identity_kind_compatible() {
                return Err(BoundedKernelError::Canonical {
                    code: "NODE_IDENTITY_KIND_MISMATCH",
                    first: index,
                    second: 0,
                });
            }
        }
        if let Some((index, _)) = nodes
            .windows(2)
            .enumerate()
            .find(|(_, pair)| pair[0].identity == pair[1].identity)
        {
            return Err(BoundedKernelError::Canonical {
                code: "conflicting typed node identity",
                first: index + 1,
                second: 0,
            });
        }
        for (index, edge) in self.edges().into_iter().enumerate() {
            if edge.record.class != edge.record.kind.class()
                || !crate::architecture::hash::edge_identity_is_bound(edge)?
            {
                return Err(BoundedKernelError::Canonical {
                    code: "edge identity does not bind its complete record",
                    first: index,
                    second: 0,
                });
            }
            if nodes
                .binary_search_by(|node| node.identity.cmp(&edge.record.from))
                .is_err()
            {
                return Err(BoundedKernelError::Canonical {
                    code: "edge has missing source",
                    first: index,
                    second: 0,
                });
            }
            if nodes
                .binary_search_by(|node| node.identity.cmp(&edge.record.to))
                .is_err()
            {
                return Err(BoundedKernelError::Canonical {
                    code: "edge has missing target",
                    first: index,
                    second: 0,
                });
            }
        }
        for (index, obligation) in self.obligations().into_iter().enumerate() {
            if obligation.applicable_projections.is_empty() {
                return Err(BoundedKernelError::Canonical {
                    code: "Unknown has no applicable required projection",
                    first: index,
                    second: 0,
                });
            }
        }
        super::validate_serialized_strings_bounded(self)
    }

    pub fn nodes(&self) -> Vec<&NodeFact> {
        self.truth
            .iter()
            .filter_map(|truth| match truth {
                Truth::Exact {
                    fact: Fact::Node { node },
                } => Some(node),
                _ => None,
            })
            .collect()
    }

    pub fn edges(&self) -> Vec<&EdgeFact> {
        self.truth
            .iter()
            .filter_map(|truth| match truth {
                Truth::Exact {
                    fact: Fact::Edge { edge },
                } => Some(edge.as_ref()),
                _ => None,
            })
            .collect()
    }

    pub fn obligations(&self) -> Vec<&Obligation> {
        self.truth
            .iter()
            .filter_map(|truth| match truth {
                Truth::Unknown { obligation } => Some(obligation),
                _ => None,
            })
            .collect()
    }

    pub fn required_failures(&self) -> Vec<RequiredFailure> {
        let mut failures = self
            .obligations()
            .into_iter()
            .flat_map(|obligation| {
                obligation
                    .applicable_projections
                    .iter()
                    .copied()
                    .map(|projection| RequiredFailure {
                        code: obligation.code,
                        subject: obligation.subject.clone(),
                        projection,
                        stage: obligation.stage,
                        source: obligation.source.clone(),
                        reason: obligation.reason.clone(),
                    })
            })
            .collect::<Vec<_>>();
        failures.sort();
        failures
    }

    pub fn node_map(&self) -> BTreeMap<Identity, NodeFact> {
        self.nodes()
            .into_iter()
            .map(|node| (node.identity.clone(), node.clone()))
            .collect()
    }

    pub fn edge_map(&self) -> BTreeMap<EdgeIdentity, EdgeFact> {
        self.edges()
            .into_iter()
            .map(|edge| (edge.identity.clone(), edge.clone()))
            .collect()
    }

    pub fn unknown_projections(&self) -> BTreeSet<ProjectionName> {
        self.obligations()
            .into_iter()
            .flat_map(|item| item.applicable_projections.iter().copied())
            .collect()
    }
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn validate_canonical_limits_for_test(
    graph: &GraphArtifact,
    max_truth: usize,
    max_serialized_bytes: usize,
) -> std::result::Result<(), BoundedKernelError> {
    graph.validate_canonical_bounded_with_limits(max_truth, max_serialized_bytes)
}

fn validate_nested_order_bounded(
    graph: &GraphArtifact,
) -> std::result::Result<(), BoundedKernelError> {
    for (index, truth) in graph.truth.iter().enumerate() {
        match truth {
            Truth::Exact {
                fact: Fact::Node { node },
            } => require_strict_order_bounded(
                &node.attributes,
                "GRAPH_NONCANONICAL: node attributes",
            )?,
            Truth::Exact {
                fact: Fact::Edge { edge },
            } => {
                require_strict_order_bounded(
                    &edge.record.attributes,
                    "GRAPH_NONCANONICAL: edge attributes",
                )?;
                match &edge.record.condition {
                    Condition::Cargo { features, .. } => require_strict_order_bounded(
                        features,
                        "GRAPH_NONCANONICAL: Cargo condition features",
                    )?,
                    Condition::Frontend {
                        export_conditions, ..
                    } => require_strict_order_bounded(
                        export_conditions,
                        "GRAPH_NONCANONICAL: frontend export conditions",
                    )?,
                    Condition::Always | Condition::RustCfg { .. } => {}
                }
            }
            Truth::Unknown { obligation } => {
                if obligation
                    .applicable_projections
                    .windows(2)
                    .any(|pair| projection_wire_name(pair[0]) >= projection_wire_name(pair[1]))
                {
                    return Err(BoundedKernelError::Canonical {
                        code: "GRAPH_NONCANONICAL: obligation projections",
                        first: index,
                        second: 0,
                    });
                }
            }
        }
    }
    Ok(())
}

fn projection_wire_name(projection: ProjectionName) -> &'static str {
    match projection {
        ProjectionName::Containment => "containment",
        ProjectionName::RustModule => "rust_module",
        ProjectionName::CargoDeclared => "cargo_declared",
        ProjectionName::CargoResolved => "cargo_resolved",
        ProjectionName::FrontendBuild => "frontend_build",
        ProjectionName::RuntimeDelivery => "runtime_delivery",
        ProjectionName::TestOverlay => "test_overlay",
        ProjectionName::GeneratedProvenance => "generated_provenance",
        ProjectionName::Ownership => "ownership",
        ProjectionName::Call => "call",
        ProjectionName::Documentation => "documentation",
    }
}

fn require_strict_order_bounded<T: Ord>(
    values: &[T],
    code: &'static str,
) -> std::result::Result<(), BoundedKernelError> {
    if let Some((index, _)) = values
        .windows(2)
        .enumerate()
        .find(|(_, pair)| pair[0] >= pair[1])
    {
        return Err(BoundedKernelError::Canonical {
            code,
            first: index,
            second: index + 1,
        });
    }
    Ok(())
}

impl Default for GraphArtifact {
    fn default() -> Self {
        Self::new()
    }
}

pub fn exact_node(
    identity: Identity,
    kind: NodeKind,
    label: impl Into<String>,
    mut attributes: Vec<Attribute>,
) -> Result<Fact> {
    attributes.sort();
    attributes.dedup();
    let node = NodeFact {
        identity,
        kind,
        label: CanonicalText::new(label.into()).context("node label")?,
        attributes,
    };
    node.validate_identity_kind()?;
    Ok(Fact::Node { node })
}

pub fn exact_edge(
    from: Identity,
    to: Identity,
    kind: EdgeKind,
    mut condition: Condition,
    source: SourceAnchor,
    resolver: ResolverId,
    mut attributes: Vec<Attribute>,
) -> Result<Fact> {
    match &mut condition {
        Condition::Cargo { features, .. } => {
            features.sort();
            features.dedup();
        }
        Condition::Frontend {
            export_conditions, ..
        } => {
            export_conditions.sort();
            export_conditions.dedup();
        }
        Condition::Always | Condition::RustCfg { .. } => {}
    }
    attributes.sort();
    attributes.dedup();
    Ok(Fact::Edge {
        edge: Box::new(EdgeFact::exact(EdgeRecord {
            from,
            to,
            kind,
            class: kind.class(),
            condition,
            source,
            resolver,
            attributes,
        })?),
    })
}
