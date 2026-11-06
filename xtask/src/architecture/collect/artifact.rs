use anyhow::Result;
use serde::Serialize;

use crate::architecture::schema::{
    CanonicalText, DocumentationId, GraphArtifact, ObligationCode, PipelineStage, ProjectionName,
    SourceAnchor,
};

use super::unknown;

#[derive(Serialize)]
pub struct SemanticFixtureLedger {
    schema: CanonicalText,
    fixtures: Vec<SemanticFixture>,
}

#[derive(Serialize)]
struct SemanticFixture {
    id: CanonicalText,
    owner: CanonicalText,
    status: CanonicalText,
    site_specific_execution: bool,
}

pub fn required_red_ledger() -> Result<SemanticFixtureLedger> {
    let rows = [
        ("cargo-locked", "collect/cargo"),
        ("cargo-same-name", "collect/cargo"),
        ("rust-basic", "collect/rust"),
        ("rust-multi-super", "collect/rust"),
        ("frontend-planes", "collect/frontend"),
        ("frontend-p3-counterexamples", "collect/frontend"),
        ("p2-rust-inline-module", "collect/rust"),
        ("p2-rust-self-path", "collect/rust"),
        ("p2-rust-prefix", "collect/rust"),
        ("p2-rust-extern-rename", "collect/rust"),
        ("p2-rust-cfg", "collect/rust"),
        ("p2-rust-test-attribution", "collect/rust"),
        ("p2-rust-source-span", "collect/rust"),
        ("p2-frontend-alias", "collect/frontend"),
        ("p2-frontend-glob", "collect/frontend"),
        ("p2-frontend-worker-query", "collect/frontend"),
        ("p2-frontend-type-reexport", "collect/frontend"),
        ("p2-frontend-dynamic-import", "collect/frontend"),
        ("p2-frontend-require", "collect/frontend"),
        ("p2-vue-script-source", "collect/frontend"),
        ("p2-vue-block-span", "collect/frontend"),
        ("p2-route-context", "collect/frontend"),
        ("p2-css-import-asset", "collect/frontend"),
        ("p2-wasm-generated-lineage", "collect/artifact"),
    ];
    Ok(SemanticFixtureLedger {
        schema: CanonicalText::new("bbnf.architecture.semantic-fixture-ledger.v3.1")?,
        fixtures: rows
            .into_iter()
            .map(|(id, owner)| {
                Ok(SemanticFixture {
                    id: CanonicalText::new(id)?,
                    owner: CanonicalText::new(owner)?,
                    status: CanonicalText::new("required_red")?,
                    site_specific_execution: false,
                })
            })
            .collect::<Result<Vec<_>>>()?,
    })
}

pub fn collect(graph: &mut GraphArtifact) -> Result<()> {
    crate::architecture::evidence::write::failpoint("artifact-boundary")?;
    let source = SourceAnchor::Documentation {
        documentation: DocumentationId::new("p31-gestalt1-verdict-authority")?,
    };
    for (code, subject, reason, projections) in [
        (
            ObligationCode::GeneratedLineageAbsent,
            "generated-lineage",
            "producer input generator configuration command output hash and stale state remain Unknown",
            vec![ProjectionName::GeneratedProvenance],
        ),
        (
            ObligationCode::WasmPackageLineageIncomplete,
            "wasm-package-lineage",
            "WASM declaration runtime package copy and load lineage remains Unknown",
            vec![
                ProjectionName::GeneratedProvenance,
                ProjectionName::RuntimeDelivery,
            ],
        ),
        (
            ObligationCode::CallContextBoundaryUnattributed,
            "call-context",
            "Rust and frontend call contexts are bounded Unknown",
            vec![ProjectionName::Call],
        ),
        (
            ObligationCode::RelocatedConstellationBoundary,
            "relocated-constellation",
            "full two-repository relocated constellation proof remains pending",
            vec![ProjectionName::Ownership],
        ),
        (
            ObligationCode::PermutationTotalityBoundary,
            "permutation-totality",
            "one thousand full-input permutations remain pending",
            vec![ProjectionName::Ownership],
        ),
        (
            ObligationCode::MutationLocalityUnproven,
            "mutation-locality",
            "full source mutation bank remains pending beyond kernel locality fixtures",
            vec![ProjectionName::Ownership],
        ),
        (
            ObligationCode::LocaleStabilityUnproven,
            "locale-stability",
            "full exporter locale variation remains pending",
            vec![ProjectionName::Ownership],
        ),
        (
            ObligationCode::ParseThatRustTsUnionUnproven,
            "parse-that-rust-ts-union",
            "parse-that Rust and TypeScript union remains future exact semantic work",
            vec![ProjectionName::RustModule, ProjectionName::FrontendBuild],
        ),
        (
            ObligationCode::BbnfModularBackendsUnproven,
            "bbnf-modular-backends",
            "BBNF modular IR and backend topology remains future exact semantic work",
            vec![
                ProjectionName::RustModule,
                ProjectionName::GeneratedProvenance,
            ],
        ),
        (
            ObligationCode::GrammarRegexHirUnproven,
            "grammar-regex-hir",
            "grammar-derived regex HIR provenance remains future exact semantic work",
            vec![ProjectionName::GeneratedProvenance],
        ),
        (
            ObligationCode::SharedCostAlgebraUnproven,
            "shared-cost-csp-equality",
            "shared cost graph CSP and equality algebra remain future exact semantic work",
            vec![ProjectionName::CargoResolved, ProjectionName::Call],
        ),
        (
            ObligationCode::L4TypeValueApiUnproven,
            "l4-type-value-api",
            "generalized L4 typing and value API remain future exact semantic work",
            vec![ProjectionName::RustModule, ProjectionName::Call],
        ),
        (
            ObligationCode::AppleArmAssemblyKernelUnproven,
            "apple-arm-assembly-kernels",
            "real Apple ARM assembly kernel lineage remains future exact semantic work",
            vec![ProjectionName::GeneratedProvenance, ProjectionName::Call],
        ),
        (
            ObligationCode::CompleteLockCoverageUnproven,
            "complete-lock-coverage",
            "complete PT and BBNF lock coverage remains future adjudicated semantic work",
            vec![ProjectionName::Documentation],
        ),
        (
            ObligationCode::GrammarModuleTypeProvenanceUnproven,
            "grammar-module-type-provenance",
            "fully formed Rust type provenance solely from BBNF modules remains future exact work; sidecars tables and shims are forbidden",
            vec![
                ProjectionName::RustModule,
                ProjectionName::GeneratedProvenance,
            ],
        ),
    ] {
        graph.unknown(unknown(
            code,
            subject,
            projections,
            PipelineStage::Artifact,
            source.clone(),
            reason,
        )?);
    }
    let _ = CanonicalText::new("artifact boundary")?;
    Ok(())
}
