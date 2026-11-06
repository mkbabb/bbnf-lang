use std::collections::BTreeMap;
use std::io::Write as _;
use std::process::Stdio;

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};

use crate::architecture::evidence::binding::SourceBinding;
use crate::architecture::inventory::{CapturedInput, CapturedInputs, Inventory};
use crate::architecture::manifest::{ConstellationManifest, PackageFaceKind};
use crate::architecture::schema::{
    GraphArtifact, ObligationCode, PipelineStage, ProjectionName, RepoPath, Sha256Digest,
    SourceAnchor,
};

use super::unknown;

#[derive(Serialize)]
struct HelperInput<'a> {
    schema: &'static str,
    face: &'a str,
    files: Vec<HelperInputFile<'a>>,
}

#[derive(Serialize)]
struct HelperInputFile<'a> {
    path: String,
    hash: &'a Sha256Digest,
    text: &'a str,
}

type HelperAnchors<'a> = BTreeMap<String, (RepoPath, &'a str)>;

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct HelperOutput {
    schema: String,
    edges: Vec<serde_json::Value>,
    unknowns: Vec<HelperUnknown>,
}

#[derive(Clone, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd)]
#[serde(deny_unknown_fields)]
struct HelperUnknown {
    code: String,
    path: String,
    line: u32,
    column: u32,
    reason: String,
}

pub fn collect(
    manifest: &ConstellationManifest,
    inventory: &Inventory,
    captured: &CapturedInputs,
    binding: &SourceBinding,
    graph: &mut GraphArtifact,
) -> Result<()> {
    crate::architecture::evidence::write::failpoint("frontend-boundary")?;
    captured.validate(inventory)?;
    let helper_dir = tempfile::tempdir().context("create bound frontend helper directory")?;
    let helper = binding.write_bound_helpers(helper_dir.path())?;
    let node = binding.verified_tool_path("node", &["--version"])?;
    for face in manifest.package_faces.iter().filter(|face| {
        matches!(
            face.kind,
            PackageFaceKind::Frontend | PackageFaceKind::GeneratedFrontend
        )
    }) {
        let inputs = captured.frontend_face(face);
        let (payload, anchors) = helper_input(face.root.as_str(), face.id.as_str(), &inputs)?;
        crate::architecture::evidence::write::failpoint("frontend-node-spawn")?;
        let mut child = crate::architecture::canonical_child_command(&node)
            .arg(&helper)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .context("execute bound frontend boundary helper")?;
        child
            .stdin
            .take()
            .context("frontend helper stdin")?
            .write_all(&serde_json::to_vec(&payload)?)?;
        let output = child.wait_with_output()?;
        if !output.status.success() {
            bail!(
                "frontend boundary helper failed: {}",
                String::from_utf8_lossy(&output.stderr).trim()
            );
        }
        crate::architecture::evidence::write::failpoint("helper-decode")?;
        let mut output: HelperOutput = serde_json::from_slice(&output.stdout)
            .context("decode frontend boundary helper output")?;
        if output.schema != "bbnf.architecture.frontend-boundary.v3.1" {
            bail!("FRONTEND_HELPER_SCHEMA_MISMATCH: helper response schema differs");
        }
        if !output.edges.is_empty() {
            bail!("FRONTEND_HELPER_EDGE_UNVALIDATED: helper emitted unsupported exact edges");
        }
        output.unknowns.sort();
        if output
            .unknowns
            .windows(2)
            .any(|window| window[0] == window[1])
        {
            bail!("FRONTEND_HELPER_SCHEMA_MISMATCH: duplicate helper obligation");
        }
        for item in output.unknowns {
            let code = parse_code(&item.code).context("unknown frontend obligation code")?;
            let (full_path, text) = anchors
                .get(&item.path)
                .with_context(|| format!("FRONTEND_HELPER_ANCHOR_MISMATCH: {}", item.path))?;
            validate_anchor(text, item.line, item.column)?;
            let record = inventory
                .find(&face.repository, full_path)
                .context("FRONTEND_HELPER_ANCHOR_MISMATCH: anchor is absent from inventory")?;
            let input = captured
                .find(&face.repository, full_path)
                .context("FRONTEND_HELPER_ANCHOR_MISMATCH: anchor bytes were not captured")?;
            if record.identity.kind != crate::architecture::schema::PhysicalKind::File
                || record.content_hash.as_ref() != Some(&input.content_hash)
            {
                bail!("FRONTEND_HELPER_ANCHOR_MISMATCH: anchor identity or bytes differ");
            }
            graph.unknown(unknown(
                code,
                face.id.as_str(),
                frontend_projections(code),
                PipelineStage::Frontend,
                SourceAnchor::File {
                    repository: face.repository.clone(),
                    path: full_path.clone(),
                    start_line: item.line,
                    start_column: item.column,
                    end_line: item.line,
                    end_column: item.column,
                },
                &item.reason,
            )?);
        }
    }
    Ok(())
}

fn helper_input<'a>(
    face_root: &str,
    face: &'a str,
    inputs: &'a [&'a CapturedInput],
) -> Result<(HelperInput<'a>, HelperAnchors<'a>)> {
    let mut files = Vec::new();
    let mut anchors = BTreeMap::new();
    for input in inputs {
        let relative = if face_root == "." {
            input.identity.path.as_str()
        } else if input.identity.path.as_str() == face_root {
            "."
        } else {
            input
                .identity
                .path
                .as_str()
                .strip_prefix(&format!("{face_root}/"))
                .context("captured frontend input escaped its face")?
        };
        let text = std::str::from_utf8(&input.bytes)
            .with_context(|| format!("frontend typed input is not UTF-8: {relative}"))?;
        let relative = RepoPath::new(relative)?.as_str().to_string();
        if anchors
            .insert(relative.clone(), (input.identity.path.clone(), text))
            .is_some()
        {
            bail!("FRONTEND_HELPER_ANCHOR_MISMATCH: duplicate typed input");
        }
        files.push(HelperInputFile {
            path: relative,
            hash: &input.content_hash,
            text,
        });
    }
    files.sort_by(|left, right| left.path.cmp(&right.path));
    Ok((
        HelperInput {
            schema: "bbnf.architecture.frontend-boundary-input.v3.1",
            face,
            files,
        },
        anchors,
    ))
}

fn validate_anchor(text: &str, line: u32, column: u32) -> Result<()> {
    if line == 0 || column == 0 {
        bail!("FRONTEND_HELPER_ANCHOR_MISMATCH: line and column are one-based");
    }
    let line_text = text
        .lines()
        .nth((line - 1) as usize)
        .context("FRONTEND_HELPER_ANCHOR_MISMATCH: line is outside captured bytes")?;
    if column as usize > line_text.chars().count() + 1 {
        bail!("FRONTEND_HELPER_ANCHOR_MISMATCH: column is outside captured bytes");
    }
    Ok(())
}

fn parse_code(code: &str) -> Option<ObligationCode> {
    Some(match code {
        "FRONTEND_APPLICATION_SCOPE_OMITTED" => ObligationCode::FrontendApplicationScopeOmitted,
        "FRONTEND_TEST_OVERLAY_INCOMPLETE" => ObligationCode::FrontendTestOverlayIncomplete,
        "VITE_ALIAS_RESOLUTION_UNSOUND" => ObligationCode::ViteAliasResolutionUnsound,
        "GLOB_SEMANTICS_UNSOUND" => ObligationCode::GlobSemanticsUnsound,
        "WORKER_QUERY_SEMANTICS_UNSOUND" => ObligationCode::WorkerQuerySemanticsUnsound,
        "TYPE_ONLY_REEXPORT_MISCLASSIFIED" => ObligationCode::TypeOnlyReexportMisclassified,
        "DYNAMIC_IMPORT_BOUNDARY_SILENT" => ObligationCode::DynamicImportBoundarySilent,
        "REQUIRE_BOUNDARY_SILENT" => ObligationCode::RequireBoundarySilent,
        "VUE_SCRIPT_SOURCE_OMITTED" => ObligationCode::VueScriptSourceOmitted,
        "VUE_BLOCK_SPAN_MISATTRIBUTED" => ObligationCode::VueBlockSpanMisattributed,
        "ROUTE_CONTEXT_UNPROVEN" => ObligationCode::RouteContextUnproven,
        "CSS_IMPORT_CLASSIFICATION_UNSOUND" => ObligationCode::CssImportClassificationUnsound,
        _ => return None,
    })
}

fn frontend_projections(code: ObligationCode) -> Vec<ProjectionName> {
    match code {
        ObligationCode::FrontendTestOverlayIncomplete => vec![ProjectionName::TestOverlay],
        _ => vec![
            ProjectionName::FrontendBuild,
            ProjectionName::RuntimeDelivery,
        ],
    }
}
