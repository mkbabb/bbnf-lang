use anyhow::{Result, bail};
use serde::{Deserialize, Serialize};

use super::{CanonicalText, ObligationSubject, ProjectionName, SourceAnchor};

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum ObligationCode {
    ConstellationManifestMissing,
    ProductIdentityMissing,
    SemanticContextUntyped,
    InventoryScopeFaceMismatch,
    DocumentationAuthorityMissing,
    InventoryPathAmbiguous,
    InventoryNodeKindMismatch,
    InventorySemanticFaceUnlinked,
    RustInlineModuleAttributionUnsound,
    RustSelfPathFalseCycle,
    RustPathPrefixGuessRejected,
    RustExternRenameUnresolved,
    RustCfgDiscoveryUnevaluated,
    RustTestAttributionUnsound,
    SourceSpanIncomplete,
    RustExternalPackageUnlinked,
    RustSemanticsUnproven,
    CargoResolutionContextCollapsed,
    CargoLockChecksumUnbound,
    CargoProfileTargetFeatureUnbound,
    CargoDeclarationIdOrderDependent,
    CargoFailureCauseDropped,
    FrontendApplicationScopeOmitted,
    FrontendTestOverlayIncomplete,
    ViteAliasResolutionUnsound,
    GlobSemanticsUnsound,
    WorkerQuerySemanticsUnsound,
    TypeOnlyReexportMisclassified,
    DynamicImportBoundarySilent,
    RequireBoundarySilent,
    VueScriptSourceOmitted,
    VueBlockSpanMisattributed,
    RouteContextUnproven,
    CssImportClassificationUnsound,
    ProjectionHashSemanticsUnbound,
    ShardHashCutUnbound,
    ComponentIdProjectionUnbound,
    HashAlgorithmNoncryptographic,
    CycleEdgeIdentityIncomplete,
    CycleBasisIncomplete,
    DynamicShardPartitionAbsent,
    BoundaryQuotientAbsent,
    GeneratedLineageAbsent,
    WasmPackageLineageIncomplete,
    CallContextBoundaryUnattributed,
    DocumentationSupersessionUnmodeled,
    WorktreeContentHashIncomplete,
    WorktreeStatusErrorMasked,
    ProductWorktreeUnnominated,
    WorktreeIdentityCollision,
    CollectorFailureEvidenceIncomplete,
    ValidationFailureEvidenceIncomplete,
    EvidencePublicationNonatomic,
    PrivacyValidatorIncomplete,
    ExternalPathIdRelocationUnstable,
    SourceStateManifestAbsent,
    SourceSnapshotDrift,
    MovementAuthorizationReceiptAbsent,
    ResourceBudgetExceeded,
    ParseThatRustTsUnionUnproven,
    BbnfModularBackendsUnproven,
    GrammarRegexHirUnproven,
    SharedCostAlgebraUnproven,
    L4TypeValueApiUnproven,
    AppleArmAssemblyKernelUnproven,
    CompleteLockCoverageUnproven,
    GrammarModuleTypeProvenanceUnproven,
    MutationLocalityUnproven,
    LocaleStabilityUnproven,
    RelocatedConstellationBoundary,
    PermutationTotalityBoundary,
    GeneratedProvenanceBoundary,
    WasmProducerManifestBoundary,
    UnknownExactFactRejected,
    ToolUnavailable,
    InjectedFailure,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PipelineStage {
    Bootstrap,
    Manifest,
    Inventory,
    Worktree,
    Foundation,
    Cargo,
    Rust,
    Node,
    Frontend,
    Artifact,
    Projection,
    Cycle,
    Cluster,
    Validation,
    Recensus,
    Generation,
    Write,
    Complete,
}

impl PipelineStage {
    pub fn parse(value: &str) -> Option<Self> {
        match value {
            "bootstrap" => Some(Self::Bootstrap),
            "manifest" => Some(Self::Manifest),
            "inventory" => Some(Self::Inventory),
            "worktree" => Some(Self::Worktree),
            "foundation" => Some(Self::Foundation),
            "cargo" => Some(Self::Cargo),
            "rust" => Some(Self::Rust),
            "node" => Some(Self::Node),
            "frontend" => Some(Self::Frontend),
            "artifact" => Some(Self::Artifact),
            "projection" => Some(Self::Projection),
            "cycle" => Some(Self::Cycle),
            "cluster" => Some(Self::Cluster),
            "validation" => Some(Self::Validation),
            "recensus" => Some(Self::Recensus),
            "generation" => Some(Self::Generation),
            "write" => Some(Self::Write),
            "complete" => Some(Self::Complete),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Obligation {
    pub code: ObligationCode,
    pub subject: ObligationSubject,
    pub applicable_projections: Vec<ProjectionName>,
    pub stage: PipelineStage,
    pub source: SourceAnchor,
    pub reason: CanonicalText,
}

impl Obligation {
    pub fn required(
        code: ObligationCode,
        subject: ObligationSubject,
        applicable_projections: Vec<ProjectionName>,
        stage: PipelineStage,
        source: SourceAnchor,
        reason: CanonicalText,
    ) -> Result<Self> {
        if applicable_projections.is_empty() {
            bail!("Unknown obligation must name every applicable projection");
        }
        let mut applicable_projections = applicable_projections;
        applicable_projections.sort();
        applicable_projections.dedup();
        Ok(Self {
            code,
            subject,
            applicable_projections,
            stage,
            source,
            reason,
        })
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RequiredFailure {
    pub code: ObligationCode,
    pub subject: ObligationSubject,
    pub projection: ProjectionName,
    pub stage: PipelineStage,
    pub source: SourceAnchor,
    pub reason: CanonicalText,
}
