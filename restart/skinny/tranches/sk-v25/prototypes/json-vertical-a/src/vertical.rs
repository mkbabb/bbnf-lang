use serde::{Deserialize, Serialize};
use sha2::{Digest as _, Sha256};
use thiserror::Error;

pub mod budget;
pub mod graph;
pub mod normalize;
pub mod program;
pub mod source;
pub mod types;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct Digest([u8; 32]);

impl Digest {
    #[must_use]
    pub fn of(bytes: &[u8]) -> Self {
        Self(Sha256::digest(bytes).into())
    }

    #[must_use]
    pub fn hex(self) -> String {
        self.0.iter().map(|byte| format!("{byte:02x}")).collect()
    }

    #[must_use]
    pub const fn bytes(self) -> [u8; 32] {
        self.0
    }
}

#[derive(Clone, Debug, Eq, Error, PartialEq)]
pub enum VerticalError {
    #[error("locked source mismatch: {0}")]
    Source(String),
    #[error("invalid program: {0}")]
    Program(String),
    #[error("identity mismatch at {stage}")]
    Identity { stage: &'static str },
    #[error("resource {resource} exceeded: used {used}, limit {limit}")]
    Budget {
        resource: &'static str,
        used: u64,
        limit: u64,
    },
    #[error("graph error: {0}")]
    Graph(String),
    #[error("type error: {0}")]
    Type(String),
    #[error("incomplete module semantics: {0}")]
    Incomplete(&'static str),
}

pub(crate) fn canonical_digest<T: Serialize>(value: &T) -> Result<Digest, VerticalError> {
    serde_json::to_vec(value)
        .map(|bytes| Digest::of(&bytes))
        .map_err(|error| VerticalError::Program(error.to_string()))
}

#[derive(Clone, Debug)]
pub struct AssayStages {
    pub program: program::Program,
    pub normalized: normalize::NormalizedProgram,
    pub graph: graph::DependencyGraph,
}

pub fn assay(
    source: &source::SourceUnit,
    budget: budget::Budget,
) -> Result<AssayStages, VerticalError> {
    let program = program::Program::from_source(source, budget)?;
    let normalized = normalize::NormalizedProgram::identity(&program, budget)?;
    let graph = graph::DependencyGraph::derive(&normalized, budget)?;
    Ok(AssayStages {
        program,
        normalized,
        graph,
    })
}

/// The full product pipeline is deliberately unavailable until the BBNF module
/// itself supplies enough semantics to derive types, hosts, facts, plans, and
/// values without a schema or lowering sidecar.
pub fn compile(_source: &source::SourceUnit, _budget: budget::Budget) -> Result<(), VerticalError> {
    Err(VerticalError::Incomplete(
        "module-derived host/type/fact/plan/value semantics are not implemented",
    ))
}
