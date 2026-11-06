//! Deterministic equality/row solving, total budgets, and immutable solutions.

use std::collections::{BTreeMap, BTreeSet};

use super::store::{MetaId, Node, RecVar, RowId, RowSpec, TypeId, TypeStore};
use super::{OperationContext, Origin, ProofNode, TypeError, TypeErrorKind};

mod row;
mod unify;
mod zonk;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Constraint {
    Equal {
        left: TypeId,
        right: TypeId,
        origin: Origin,
    },
    Lacks {
        row: RowId,
        label: String,
        origin: Origin,
    },
}

impl Constraint {
    #[must_use]
    pub fn equal(left: TypeId, right: TypeId, origin: Origin) -> Self {
        Self::Equal {
            left,
            right,
            origin,
        }
    }

    #[must_use]
    pub fn lacks(row: RowId, label: impl Into<String>, origin: Origin) -> Self {
        Self::Lacks {
            row,
            label: label.into(),
            origin,
        }
    }

    fn key(&self) -> (u8, u32, u32, &str, &str) {
        match self {
            Self::Lacks { row, label, origin } => (0, row.slot, 0, label.as_str(), origin.site()),
            Self::Equal {
                left,
                right,
                origin,
            } => {
                let (left, right) = if left.slot <= right.slot {
                    (left.slot, right.slot)
                } else {
                    (right.slot, left.slot)
                };
                (1, left, right, "", origin.site())
            }
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct FreeTypeVar {
    id: u32,
    level: u32,
}

impl FreeTypeVar {
    #[must_use]
    pub fn id(self) -> u32 {
        self.id
    }

    #[must_use]
    pub fn level(self) -> u32 {
        self.level
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct FreeRowVar {
    id: u32,
    level: u32,
}

impl FreeRowVar {
    #[must_use]
    pub fn id(self) -> u32 {
        self.id
    }

    #[must_use]
    pub fn level(self) -> u32 {
        self.level
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SolvedRow {
    fields: BTreeMap<String, SolvedType>,
    tail: Option<FreeRowVar>,
}

impl SolvedRow {
    #[must_use]
    pub fn fields(&self) -> &BTreeMap<String, SolvedType> {
        &self.fields
    }

    #[must_use]
    pub fn tail(&self) -> Option<FreeRowVar> {
        self.tail
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SolvedType {
    Free(FreeTypeVar),
    Rec(u32),
    Primitive(super::store::Primitive),
    Named(String),
    Tuple(Vec<SolvedType>),
    List(Box<SolvedType>),
    Option(Box<SolvedType>),
    Function {
        parameters: Vec<SolvedType>,
        result: Box<SolvedType>,
    },
    Record(SolvedRow),
    Variant(SolvedRow),
    Mu(Box<SolvedType>),
}

#[derive(Clone, Debug)]
pub struct TypeSolution {
    revision: u64,
    roots: BTreeMap<TypeId, SolvedType>,
    row_lacks: BTreeMap<FreeRowVar, BTreeSet<String>>,
    proof: Vec<ProofNode>,
    identity: String,
}

impl TypeSolution {
    #[must_use]
    pub fn revision(&self) -> u64 {
        self.revision
    }

    #[must_use]
    pub fn type_of(&self, root: TypeId) -> Option<&SolvedType> {
        (root.scope == self.revision)
            .then(|| self.roots.get(&root))
            .flatten()
    }

    #[must_use]
    pub fn row_lacks(&self, row: FreeRowVar) -> Option<&BTreeSet<String>> {
        self.row_lacks.get(&row)
    }

    #[must_use]
    pub fn proof(&self) -> &[ProofNode] {
        &self.proof
    }

    #[must_use]
    pub fn identity(&self) -> &str {
        &self.identity
    }
}

#[derive(Clone, Debug)]
struct TypeBinding {
    ty: TypeId,
    proof: usize,
}

#[derive(Clone, Debug)]
pub(crate) struct RowExpr {
    fields: BTreeMap<String, TypeId>,
    tail: Option<usize>,
}

impl RowExpr {
    fn from_spec(spec: &RowSpec) -> Self {
        Self {
            fields: spec.fields.iter().cloned().collect(),
            tail: spec.tail.map(|row| row.slot as usize),
        }
    }

    fn empty(tail: Option<usize>) -> Self {
        Self {
            fields: BTreeMap::new(),
            tail,
        }
    }
}

#[derive(Clone, Debug)]
struct RowBinding {
    expr: RowExpr,
    proof: usize,
}

#[derive(Clone, Debug)]
pub(crate) struct State<'store> {
    store: &'store TypeStore,
    type_subst: Vec<Option<TypeBinding>>,
    type_levels: Vec<u32>,
    row_subst: Vec<Option<RowBinding>>,
    row_levels: Vec<u32>,
    row_lacks: Vec<BTreeMap<String, usize>>,
}

impl<'store> State<'store> {
    fn new(store: &'store TypeStore) -> Self {
        Self {
            store,
            type_subst: vec![None; store.meta_count()],
            type_levels: store.meta_levels.clone(),
            row_subst: vec![None; store.row_count()],
            row_levels: store.row_levels.clone(),
            row_lacks: vec![BTreeMap::new(); store.row_count()],
        }
    }

    fn validate_type(
        &self,
        ty: TypeId,
        proof: usize,
        operations: &OperationContext,
    ) -> Result<(), TypeError> {
        if ty.scope != self.store.scope {
            return Err(operations.fail(TypeErrorKind::CrossStore, &[proof], "solve.ingest"));
        }
        if ty.slot as usize >= self.store.nodes.len() {
            return Err(operations.fail(TypeErrorKind::DanglingType, &[proof], "solve.ingest"));
        }
        Ok(())
    }

    fn validate_row(
        &self,
        row: RowId,
        proof: usize,
        operations: &OperationContext,
    ) -> Result<(), TypeError> {
        if row.scope != self.store.scope {
            return Err(operations.fail(TypeErrorKind::CrossStore, &[proof], "solve.ingest"));
        }
        if row.slot as usize >= self.row_subst.len() {
            return Err(operations.fail(TypeErrorKind::DanglingRow, &[proof], "solve.ingest"));
        }
        Ok(())
    }

    fn fresh_row(&mut self, level: u32) -> usize {
        let row = self.row_subst.len();
        self.row_subst.push(None);
        self.row_levels.push(level);
        self.row_lacks.push(BTreeMap::new());
        row
    }

    fn node(&self, ty: TypeId) -> Option<&Node> {
        self.store.node(ty)
    }

    fn node_name(&self, ty: TypeId) -> String {
        match self.node(ty) {
            Some(Node::Meta(meta)) => format!("?{}", meta.slot),
            Some(Node::Rec(rec)) => format!("rec{}", rec.slot),
            Some(Node::Primitive(primitive)) => format!("{primitive:?}"),
            Some(Node::Named(name)) => format!("named({name})"),
            Some(Node::Tuple(items)) => format!("tuple/{}", items.len()),
            Some(Node::Join(items)) => format!("join/{}", items.len()),
            Some(Node::List(_)) => "list".to_owned(),
            Some(Node::Option(_)) => "option".to_owned(),
            Some(Node::Function { parameters, .. }) => format!("function/{}", parameters.len()),
            Some(Node::Record(_)) => "record".to_owned(),
            Some(Node::Variant(_)) => "variant".to_owned(),
            Some(Node::Mu { .. }) => "mu".to_owned(),
            None => "dangling".to_owned(),
        }
    }
}

#[derive(Clone, Debug)]
enum PendingConstraint {
    Equal {
        left: TypeId,
        right: TypeId,
        proof: usize,
    },
    Lacks {
        row: RowId,
        label: String,
        proof: usize,
    },
}

/// Solve and fully reify the requested roots.  The caller passes the same
/// operation context used by inference so preprocessing through projection is
/// charged to one total limit.
pub fn solve<I>(
    store: &TypeStore,
    constraints: &[Constraint],
    roots: I,
    operations: &mut OperationContext,
) -> Result<TypeSolution, TypeError>
where
    I: IntoIterator<Item = TypeId>,
{
    let roots = roots.into_iter().collect::<Vec<_>>();
    let mut state = State::new(store);
    let mut pending = Vec::with_capacity(constraints.len());

    for constraint in constraints {
        let (operation, origin) = match constraint {
            Constraint::Equal { origin, .. } => ("solve.ingest.equal", origin.clone()),
            Constraint::Lacks { origin, .. } => ("solve.ingest.lacks", origin.clone()),
        };
        let proof = operations.step(operation, &[], [origin])?;
        match constraint {
            Constraint::Equal { left, right, .. } => {
                state.validate_type(*left, proof, operations)?;
                state.validate_type(*right, proof, operations)?;
                pending.push(PendingConstraint::Equal {
                    left: *left,
                    right: *right,
                    proof,
                });
            }
            Constraint::Lacks { row, label, .. } => {
                state.validate_row(*row, proof, operations)?;
                pending.push(PendingConstraint::Lacks {
                    row: *row,
                    label: label.clone(),
                    proof,
                });
            }
        }
    }

    for root in &roots {
        let proof = operations.step(
            "solve.ingest.root",
            &[],
            [Origin::new("solve.root", format!("type {}", root.slot))],
        )?;
        state.validate_type(*root, proof, operations)?;
    }

    operations.step(
        "solve.order",
        &[],
        [Origin::boundary("solve.constraint_order")],
    )?;
    pending.sort_by(|left, right| pending_key(left).cmp(&pending_key(right)));

    state.seed_row_prefix_lacks(&pending, &roots, operations)?;

    for constraint in pending {
        match constraint {
            PendingConstraint::Equal { left, right, proof } => {
                state.unify_types(left, right, proof, operations)?;
            }
            PendingConstraint::Lacks { row, label, proof } => {
                state.add_lacks(row.slot as usize, &label, proof, operations)?;
            }
        }
    }

    let (reified, row_lacks) = state.reify_roots(&roots, operations)?;
    operations.step(
        "solve.fingerprint",
        &[],
        [Origin::boundary("solve.solution_identity")],
    )?;
    let identity = stable_identity(store.scope, &reified, &row_lacks);
    Ok(TypeSolution {
        revision: store.scope,
        roots: reified,
        row_lacks,
        proof: operations.graph().to_vec(),
        identity,
    })
}

fn pending_key(constraint: &PendingConstraint) -> (u8, u32, u32, &str) {
    match constraint {
        PendingConstraint::Lacks { row, label, .. } => (0, row.slot, 0, label),
        PendingConstraint::Equal { left, right, .. } => {
            if left.slot <= right.slot {
                (1, left.slot, right.slot, "")
            } else {
                (1, right.slot, left.slot, "")
            }
        }
    }
}

fn stable_identity(
    revision: u64,
    roots: &BTreeMap<TypeId, SolvedType>,
    lacks: &BTreeMap<FreeRowVar, BTreeSet<String>>,
) -> String {
    let material = format!("revision={revision};roots={roots:?};lacks={lacks:?}");
    let mut hash = 0xcbf2_9ce4_8422_2325_u64;
    for byte in material.bytes() {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    format!("fnv1a64:{hash:016x}")
}

impl Constraint {
    #[allow(dead_code)]
    fn canonical_key(&self) -> (u8, u32, u32, &str, &str) {
        self.key()
    }
}

pub(crate) fn meta_of(node: &Node) -> Option<MetaId> {
    if let Node::Meta(meta) = node {
        Some(*meta)
    } else {
        None
    }
}

pub(crate) fn rec_pair_matches(
    left: RecVar,
    right: RecVar,
    pairs: &BTreeMap<RecVar, RecVar>,
) -> bool {
    pairs.get(&left).copied() == Some(right) || (left == right && !pairs.contains_key(&left))
}
