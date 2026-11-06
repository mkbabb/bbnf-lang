use anyhow::Result;

use crate::architecture::manifest::{ConstellationManifest, PackageFaceKind};
use crate::architecture::schema::{
    CanonicalText, GraphArtifact, ObligationCode, PipelineStage, ProjectionName, SourceAnchor,
};

use super::unknown;

pub fn collect(manifest: &ConstellationManifest, graph: &mut GraphArtifact) -> Result<()> {
    crate::architecture::evidence::write::failpoint("rust-boundary")?;
    for face in manifest
        .package_faces
        .iter()
        .filter(|face| face.kind == PackageFaceKind::Rust)
    {
        let source = SourceAnchor::Manifest {
            repository: face.repository.clone(),
            path: face.manifest.clone(),
            key: CanonicalText::new("rust semantic face")?,
        };
        for (code, reason, projections) in [
            (
                ObligationCode::RustInlineModuleAttributionUnsound,
                "nested inline module origins remain Unknown; no ancestor edge is emitted",
                vec![ProjectionName::RustModule],
            ),
            (
                ObligationCode::RustSelfPathFalseCycle,
                "self path attribution remains Unknown; no self-cycle is guessed",
                vec![ProjectionName::RustModule],
            ),
            (
                ObligationCode::RustPathPrefixGuessRejected,
                "deep path suffixes are never truncated to an exact module edge",
                vec![ProjectionName::RustModule],
            ),
            (
                ObligationCode::RustExternRenameUnresolved,
                "renamed extern ownership awaits an exact Cargo context join",
                vec![ProjectionName::RustModule, ProjectionName::CargoResolved],
            ),
            (
                ObligationCode::RustCfgDiscoveryUnevaluated,
                "cfg discovery matrix is typed Unknown and cannot create a missing-module exact fact",
                vec![ProjectionName::RustModule, ProjectionName::TestOverlay],
            ),
            (
                ObligationCode::RustTestAttributionUnsound,
                "item-level test attribution is not promoted to a shipping edge",
                vec![ProjectionName::RustModule, ProjectionName::TestOverlay],
            ),
            (
                ObligationCode::SourceSpanIncomplete,
                "Rust semantic sites require real compiler-backed file spans before Exact",
                vec![ProjectionName::RustModule, ProjectionName::TestOverlay],
            ),
            (
                ObligationCode::RustExternalPackageUnlinked,
                "external crate ownership is not joined to context-qualified Cargo package identity",
                vec![ProjectionName::RustModule, ProjectionName::CargoResolved],
            ),
            (
                ObligationCode::RustSemanticsUnproven,
                "qualified paths includes macros traits methods and dynamic dispatch remain bounded",
                vec![ProjectionName::RustModule, ProjectionName::Call],
            ),
        ] {
            graph.unknown(unknown(
                code,
                face.id.as_str(),
                projections,
                PipelineStage::Rust,
                source.clone(),
                reason,
            )?);
        }
    }
    Ok(())
}
