//! Semantic type-kernel authority boundaries.

use std::collections::BTreeSet;
use std::fmt;

pub mod infer;
pub mod layout;
pub mod project;
pub mod scheme;
pub mod solve;
pub mod store;

/// Stable source or boundary attribution carried by proof nodes.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Origin {
    site: String,
    detail: String,
}

impl Origin {
    #[must_use]
    pub fn new(site: impl Into<String>, detail: impl Into<String>) -> Self {
        Self {
            site: site.into(),
            detail: detail.into(),
        }
    }

    #[must_use]
    pub fn boundary(site: impl Into<String>) -> Self {
        Self::new(site, "boundary")
    }

    #[must_use]
    pub fn site(&self) -> &str {
        &self.site
    }

    #[must_use]
    pub fn detail(&self) -> &str {
        &self.detail
    }
}

/// An operation or inference step in the deterministic diagnostic DAG.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ProofNode {
    id: usize,
    operation: String,
    parents: Vec<usize>,
    origins: BTreeSet<Origin>,
}

impl ProofNode {
    #[must_use]
    pub fn id(&self) -> usize {
        self.id
    }

    #[must_use]
    pub fn operation(&self) -> &str {
        &self.operation
    }

    #[must_use]
    pub fn parents(&self) -> &[usize] {
        &self.parents
    }

    #[must_use]
    pub fn origins(&self) -> &BTreeSet<Origin> {
        &self.origins
    }
}

/// Minimal parent-reachable portion of the diagnostic DAG for one failure.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ProofSlice {
    nodes: Vec<ProofNode>,
}

impl ProofSlice {
    #[must_use]
    pub fn nodes(&self) -> &[ProofNode] {
        &self.nodes
    }

    #[must_use]
    pub fn origins(&self) -> BTreeSet<Origin> {
        self.nodes
            .iter()
            .flat_map(|node| node.origins.iter().cloned())
            .collect()
    }
}

/// Typed failures shared by inference, solving, and total-budget boundaries.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeErrorKind {
    Exhausted { limit: usize },
    CrossStore,
    DanglingType,
    DanglingMeta,
    DanglingRow,
    Mismatch { left: String, right: String },
    TypeOccurs { meta: u32 },
    RowOccurs { row: u32 },
    RowLacks { row: u32, label: String },
    ClosedRow { labels: Vec<String> },
    DuplicateRowLabel { label: String },
    MissingRule { rule: u32 },
    MissingHostFunction { function: u32 },
    MissingHostSignature { function: u32 },
    MissingString { string: u32 },
    Construction { detail: String },
    InternalCycle,
}

/// Failure plus its attributable proof-DAG slice.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeError {
    kind: TypeErrorKind,
    proof: ProofSlice,
}

impl TypeError {
    #[must_use]
    pub fn kind(&self) -> &TypeErrorKind {
        &self.kind
    }

    #[must_use]
    pub fn proof(&self) -> &ProofSlice {
        &self.proof
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{:?}", self.kind)
    }
}

impl std::error::Error for TypeError {}

/// One total operation budget and proof DAG for all kernel phases.
#[derive(Clone, Debug)]
pub struct OperationContext {
    limit: usize,
    used: usize,
    graph: Vec<ProofNode>,
}

impl OperationContext {
    #[must_use]
    pub fn new(limit: usize) -> Self {
        Self {
            limit,
            used: 0,
            graph: Vec::new(),
        }
    }

    #[must_use]
    pub fn unlimited() -> Self {
        Self::new(usize::MAX)
    }

    #[must_use]
    pub fn used(&self) -> usize {
        self.used
    }

    #[must_use]
    pub fn limit(&self) -> usize {
        self.limit
    }

    #[must_use]
    pub fn graph(&self) -> &[ProofNode] {
        &self.graph
    }

    pub(crate) fn step(
        &mut self,
        operation: impl Into<String>,
        parents: &[usize],
        origins: impl IntoIterator<Item = Origin>,
    ) -> Result<usize, TypeError> {
        let operation = operation.into();
        let origins = origins.into_iter().collect::<BTreeSet<_>>();
        if self.used >= self.limit {
            let fallback = if origins.is_empty() {
                [Origin::new(operation.clone(), "budget exhaustion")]
                    .into_iter()
                    .collect()
            } else {
                origins
            };
            let synthetic = ProofNode {
                id: self.graph.len(),
                operation,
                parents: parents.to_vec(),
                origins: fallback,
            };
            let mut nodes = self.reachable(parents);
            nodes.push(synthetic);
            return Err(TypeError {
                kind: TypeErrorKind::Exhausted { limit: self.limit },
                proof: ProofSlice { nodes },
            });
        }
        self.used += 1;
        let id = self.graph.len();
        self.graph.push(ProofNode {
            id,
            operation,
            parents: canonical_parents(parents),
            origins,
        });
        Ok(id)
    }

    pub(crate) fn fail(&self, kind: TypeErrorKind, parents: &[usize], site: &str) -> TypeError {
        let mut nodes = self.reachable(parents);
        if nodes.is_empty() {
            nodes.push(ProofNode {
                id: self.graph.len(),
                operation: site.to_owned(),
                parents: Vec::new(),
                origins: [Origin::boundary(site)].into_iter().collect(),
            });
        }
        TypeError {
            kind,
            proof: ProofSlice { nodes },
        }
    }

    fn reachable(&self, roots: &[usize]) -> Vec<ProofNode> {
        let mut pending = roots.to_vec();
        let mut seen = BTreeSet::new();
        while let Some(id) = pending.pop() {
            if !seen.insert(id) {
                continue;
            }
            if let Some(node) = self.graph.get(id) {
                pending.extend(node.parents.iter().copied());
            }
        }
        seen.into_iter()
            .filter_map(|id| self.graph.get(id).cloned())
            .collect()
    }
}

fn canonical_parents(parents: &[usize]) -> Vec<usize> {
    let mut parents = parents.to_vec();
    parents.sort_unstable();
    parents.dedup();
    parents
}
