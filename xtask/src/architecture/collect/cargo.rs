use anyhow::Result;

use crate::architecture::manifest::{ConstellationManifest, ContextKind, ResolvedRepository};
use crate::architecture::schema::{
    CanonicalText, GraphArtifact, ObligationCode, PipelineStage, ProjectionName, SourceAnchor,
};

use super::unknown;

pub fn collect(
    manifest: &ConstellationManifest,
    repositories: &[ResolvedRepository],
    graph: &mut GraphArtifact,
) -> Result<()> {
    crate::architecture::evidence::write::failpoint("cargo-boundary")?;
    let _ = repositories;
    for context in manifest
        .contexts
        .iter()
        .filter(|item| item.kind == ContextKind::Cargo)
    {
        let source = SourceAnchor::Manifest {
            repository: context.repository.clone(),
            path: context.manifest.clone(),
            key: CanonicalText::new("cargo resolution deferred")?,
        };
        for (code, reason, projections) in [
            (
                ObligationCode::CargoResolutionContextCollapsed,
                "per-invocation resolve equality is not yet promoted to Exact facts",
                vec![ProjectionName::CargoResolved],
            ),
            (
                ObligationCode::CargoLockChecksumUnbound,
                "lock digest is source-bound but package checksum facts are not yet joined",
                vec![ProjectionName::CargoResolved],
            ),
            (
                ObligationCode::CargoProfileTargetFeatureUnbound,
                "profile target feature definitions and activations remain collector Unknown",
                vec![ProjectionName::CargoDeclared, ProjectionName::CargoResolved],
            ),
            (
                ObligationCode::CargoDeclarationIdOrderDependent,
                "stable declaration span identity is not yet emitted",
                vec![ProjectionName::CargoDeclared],
            ),
        ] {
            graph.unknown(unknown(
                code,
                context.id.as_str(),
                projections,
                PipelineStage::Cargo,
                source.clone(),
                reason,
            )?);
        }
    }
    Ok(())
}
