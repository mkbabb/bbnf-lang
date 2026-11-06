use std::collections::BTreeMap;

use anyhow::{Result, bail};
use serde::Serialize;

use super::write::GenerationStatus;
use crate::architecture::project::ProjectionSet;
use crate::architecture::schema::{GraphArtifact, PipelineStage, RequiredFailure};

const GENERATION_BUDGET: usize = 48 * 1024 * 1024;
const PROJECTION_BUDGET: usize = 24 * 1024 * 1024;
const FAILURE_BUDGET: usize = 128 * 1024;

pub(super) fn json<T: Serialize + ?Sized>(value: &T) -> Result<Vec<u8>> {
    let mut bytes = serde_json::to_vec(value)?;
    bytes.push(b'\n');
    Ok(bytes)
}

pub(super) fn dot(graph: &GraphArtifact) -> Result<Vec<u8>> {
    let nodes = graph.nodes();
    let index = nodes
        .iter()
        .enumerate()
        .map(|(index, node)| (node.identity.clone(), index))
        .collect::<BTreeMap<_, _>>();
    let mut output = String::from("digraph architecture {\n");
    for (number, node) in nodes.iter().enumerate() {
        output.push_str(&format!(
            "  n{number} [label={}];\n",
            serde_json::to_string(node.label.as_str())?
        ));
    }
    for edge in graph.edges() {
        output.push_str(&format!(
            "  n{} -> n{} [label={}];\n",
            index[&edge.record.from],
            index[&edge.record.to],
            serde_json::to_string(&format!("{:?}", edge.record.kind).to_ascii_lowercase())?
        ));
    }
    output.push_str("}\n");
    Ok(output.into_bytes())
}

pub(super) fn report(
    graph: &GraphArtifact,
    projections: Option<&ProjectionSet>,
    failures: Option<&[RequiredFailure]>,
) -> Vec<u8> {
    format!(
        "architecture exporter v3.1\nstatus: RED\nmovement: denied\nnodes: {}\nedges: {}\nprojections: {}\nrequired failures: {}\n",
        graph.nodes().len(),
        graph.edges().len(),
        projections.map_or(0, |set| set.projections.len()),
        failures.map_or(0, <[RequiredFailure]>::len),
    )
    .into_bytes()
}

pub(super) fn enforce_budgets(
    total: usize,
    projection_bytes: Option<usize>,
    status: GenerationStatus,
    stage: PipelineStage,
) -> Result<()> {
    if total > GENERATION_BUDGET {
        bail!("RESOURCE_BUDGET_EXCEEDED: generation uses {total} bytes");
    }
    if projection_bytes.is_some_and(|bytes| bytes > PROJECTION_BUDGET) {
        bail!("RESOURCE_BUDGET_EXCEEDED: projection payload exceeds 24 MiB");
    }
    if status == GenerationStatus::Failure
        && matches!(stage, PipelineStage::Bootstrap | PipelineStage::Manifest)
        && total > FAILURE_BUDGET
    {
        bail!("RESOURCE_BUDGET_EXCEEDED: failure generation exceeds 128 KiB");
    }
    Ok(())
}
