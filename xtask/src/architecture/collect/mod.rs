pub mod artifact;
pub mod cargo;
pub mod foundation;
pub mod frontend;
pub mod rust;

use anyhow::Result;

use crate::architecture::schema::{
    CanonicalText, Obligation, ObligationCode, ObligationSubject, PipelineStage, ProjectionName,
    SourceAnchor,
};

pub fn unknown(
    code: ObligationCode,
    subject: &str,
    projections: Vec<ProjectionName>,
    stage: PipelineStage,
    source: SourceAnchor,
    reason: &str,
) -> Result<Obligation> {
    Obligation::required(
        code,
        ObligationSubject::new(subject)?,
        projections,
        stage,
        source,
        CanonicalText::new(reason)?,
    )
}
