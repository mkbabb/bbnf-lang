pub mod cluster;
pub mod cycle;
pub mod quotient;
pub mod scc;
pub mod spec;

pub use spec::{Projection, ProjectionSet, project};

pub(crate) fn failure_stage(error: &anyhow::Error) -> crate::architecture::schema::PipelineStage {
    use crate::architecture::schema::PipelineStage;
    if error
        .chain()
        .any(|cause| cause.to_string() == "PIPELINE_STAGE_CYCLE")
    {
        PipelineStage::Cycle
    } else if error
        .chain()
        .any(|cause| cause.to_string() == "PIPELINE_STAGE_CLUSTER")
    {
        PipelineStage::Cluster
    } else {
        PipelineStage::Projection
    }
}
