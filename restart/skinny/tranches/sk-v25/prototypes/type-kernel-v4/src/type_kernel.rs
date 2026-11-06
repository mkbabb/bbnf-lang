//! Bounded public facade for the isolated V4 proof carrier.

use std::collections::BTreeSet;
use std::fmt;

pub mod infer;
pub mod layout;
pub mod project;
pub mod scheme;
pub mod solve;
pub mod store;

pub use infer::{
    FnDescriptor, GrammarNode, GrammarSpec, HostEffect, HostTypeId, HostTypeNode, InputDigest,
    KernelInput, NodeId, RefMode, RuleId, RuleSpec, SeqPolicy, infer,
};
pub use layout::{
    Carrier, LayoutClaim, LayoutEvidence, LayoutProfile, SemanticProfile, TargetProfile,
    ToolchainIdentity, lower, measure_carrier,
};
pub use project::{ArtifactProvenance, GeneratedArtifact, TargetLanguage, project};
pub use scheme::{
    BindingForm, InstantiatedRoot, RigidContext, Scheme, generalize, generalize_roots, instantiate,
};
pub use solve::{
    RootId, SemanticDigest, SolvedRowId, SolvedRowView, SolvedTypeId, SolvedTypeView, TypeSolution,
    solve,
};
pub use store::{
    ClosedProblem, DraftRecVar, DraftRowId, DraftTypeId, ExportKey, ExportMap, Primitive,
    ProblemBuilder, RowKind,
};

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

/// One deterministic node in the causal proof DAG.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ProofNode {
    id: usize,
    operation: String,
    units: usize,
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
    pub fn units(&self) -> usize {
        self.units
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

/// Parent-reachable causal evidence for one typed failure.
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

/// Typed failures for every arbitrary-input boundary.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeErrorKind {
    Exhausted {
        limit: usize,
        requested: usize,
    },
    CrossStore,
    DanglingType,
    DanglingRow,
    DanglingRecursion,
    EscapingRecursion {
        site: String,
    },
    VacuousRecursion,
    DuplicateRowLabel {
        label: String,
    },
    RowKindMismatch,
    Mismatch {
        left: String,
        right: String,
    },
    TypeOccurs {
        meta: u32,
    },
    RowOccurs {
        row: u32,
    },
    RowLacks {
        row: u32,
        label: String,
    },
    ClosedRow {
        missing: Vec<String>,
        extra: Vec<String>,
    },
    OpenTypeVariable,
    OpenRow,
    RecursiveTypeRequiresCarrier,
    MissingRule {
        rule: u32,
    },
    MissingNode {
        node: u32,
    },
    MissingHostSignature {
        function: u32,
    },
    InvalidInput {
        detail: String,
    },
    UnsupportedLayout {
        detail: String,
    },
    Projection {
        detail: String,
    },
    ArithmeticOverflow,
    InternalInvariant {
        detail: String,
    },
}

/// Failure plus its minimal reachable proof slice.
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

/// One checked work budget and proof graph shared by every kernel phase.
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
    pub fn used(&self) -> usize {
        self.used
    }

    #[must_use]
    pub fn remaining(&self) -> usize {
        self.limit.saturating_sub(self.used)
    }

    #[must_use]
    pub fn limit(&self) -> usize {
        self.limit
    }

    #[must_use]
    pub fn graph(&self) -> &[ProofNode] {
        &self.graph
    }

    pub(crate) fn charge(
        &mut self,
        units: usize,
        operation: impl Into<String>,
        parents: &[usize],
        origins: impl IntoIterator<Item = Origin>,
    ) -> Result<usize, TypeError> {
        let operation = operation.into();
        let origins = origins.into_iter().collect::<BTreeSet<_>>();
        let Some(next) = self.used.checked_add(units) else {
            return Err(self.failure(
                TypeErrorKind::ArithmeticOverflow,
                parents,
                &operation,
                origins,
                units,
            ));
        };
        if next > self.limit {
            return Err(self.failure(
                TypeErrorKind::Exhausted {
                    limit: self.limit,
                    requested: units,
                },
                parents,
                &operation,
                origins,
                units,
            ));
        }
        self.used = next;
        let id = self.graph.len();
        self.graph.push(ProofNode {
            id,
            operation,
            units,
            parents: canonical_parents(parents),
            origins,
        });
        Ok(id)
    }

    pub(crate) fn fail(&self, kind: TypeErrorKind, parents: &[usize], site: &str) -> TypeError {
        self.failure(kind, parents, site, [Origin::boundary(site)], 0)
    }

    fn failure(
        &self,
        kind: TypeErrorKind,
        parents: &[usize],
        operation: &str,
        origins: impl IntoIterator<Item = Origin>,
        units: usize,
    ) -> TypeError {
        let mut nodes = self.reachable(parents);
        nodes.push(ProofNode {
            id: self.graph.len(),
            operation: operation.to_owned(),
            units,
            parents: canonical_parents(parents),
            origins: origins.into_iter().collect(),
        });
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

pub(crate) fn charge_text(
    operations: &mut OperationContext,
    operation: &str,
    text: &str,
    parents: &[usize],
    origin: Origin,
) -> Result<usize, TypeError> {
    let units = text
        .len()
        .checked_add(text.chars().count())
        .ok_or_else(|| operations.fail(TypeErrorKind::ArithmeticOverflow, parents, operation))?;
    operations.charge(units.max(1), operation, parents, [origin])
}

pub(crate) fn fnv1a64(bytes: &[u8]) -> u64 {
    let mut hash = 0xcbf2_9ce4_8422_2325_u64;
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    hash
}

fn canonical_parents(parents: &[usize]) -> Vec<usize> {
    let mut parents = parents.to_vec();
    parents.sort_unstable();
    parents.dedup();
    parents
}
