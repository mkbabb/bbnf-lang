use std::sync::Arc;

use serde::Serialize;

use super::{Digest, VerticalError, budget::Budget, canonical_digest, program::Program};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
pub struct NodeRemap {
    pub source: u16,
    pub normalized: u16,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct NormalizationReport {
    pub semantic_rewrites: u32,
    pub deleted_nodes: u32,
    pub remap: Arc<[NodeRemap]>,
}

#[derive(Clone, Debug)]
pub struct NormalizedProgram {
    program: Program,
    digest: Digest,
    report: NormalizationReport,
}

impl NormalizedProgram {
    pub fn identity(program: &Program, budget: Budget) -> Result<Self, VerticalError> {
        Budget::require(budget.nodes, program.rules().len(), "nodes")?;
        let remap: Arc<[NodeRemap]> = program
            .rules()
            .iter()
            .enumerate()
            .map(|(index, _)| {
                let index = u16::try_from(index).expect("rule budget is below u16 maximum");
                NodeRemap {
                    source: index,
                    normalized: index,
                }
            })
            .collect();
        let report = NormalizationReport {
            semantic_rewrites: 0,
            deleted_nodes: 0,
            remap,
        };
        let digest = canonical_digest(&(program.revision(), &report))?;
        Ok(Self {
            program: program.clone(),
            digest,
            report,
        })
    }

    #[must_use]
    pub const fn program(&self) -> &Program {
        &self.program
    }

    #[must_use]
    pub const fn digest(&self) -> Digest {
        self.digest
    }

    #[must_use]
    pub const fn report(&self) -> &NormalizationReport {
        &self.report
    }
}
