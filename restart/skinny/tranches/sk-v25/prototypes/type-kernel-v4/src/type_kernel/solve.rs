//! Transactional equality solving into immutable flat semantic arenas.

use std::collections::BTreeMap;
use std::fmt;

use super::store::{
    ClosedProblem, Constraint, DraftNode, DraftRowNode, DraftTypeId, ExportKey, Primitive, RowKind,
};
use super::{InputDigest, OperationContext, Origin, TypeError, TypeErrorKind, fnv1a64};

mod row;
mod unify;
mod zonk;

/// Opaque solved export root.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct RootId(pub(crate) u32);

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct SolvedTypeId(pub(crate) u32);

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct SolvedRowId(pub(crate) u32);

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum SolvedNode {
    Free {
        index: u32,
        level: u32,
        source: u32,
    },
    Rec(u32),
    Primitive(Primitive),
    Named(String),
    Tuple(Vec<SolvedTypeId>),
    List(SolvedTypeId),
    Option(SolvedTypeId),
    Function {
        parameters: Vec<SolvedTypeId>,
        result: SolvedTypeId,
    },
    Record(SolvedRowId),
    Variant(SolvedRowId),
    Mu {
        binder: u32,
        body: SolvedTypeId,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct SolvedRow {
    pub(crate) kind: RowKind,
    pub(crate) fields: Vec<(String, SolvedTypeId)>,
    pub(crate) tail: Option<SolvedRowTail>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct SolvedRowTail {
    pub(crate) index: u32,
    pub(crate) level: u32,
    pub(crate) lacks: Vec<String>,
    pub(crate) source: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SolvedTypeView<'a> {
    Free {
        index: u32,
        level: u32,
    },
    Rec(u32),
    Primitive(Primitive),
    Named(&'a str),
    Tuple(&'a [SolvedTypeId]),
    List(SolvedTypeId),
    Option(SolvedTypeId),
    Function {
        parameters: &'a [SolvedTypeId],
        result: SolvedTypeId,
    },
    Record(SolvedRowId),
    Variant(SolvedRowId),
    Mu {
        binder: u32,
        body: SolvedTypeId,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SolvedRowView<'a> {
    kind: RowKind,
    fields: &'a [(String, SolvedTypeId)],
    tail: Option<(u32, u32, &'a [String])>,
}

impl SolvedRowView<'_> {
    #[must_use]
    pub fn kind(&self) -> RowKind {
        self.kind
    }

    #[must_use]
    pub fn fields(&self) -> &[(String, SolvedTypeId)] {
        self.fields
    }

    #[must_use]
    pub fn tail(&self) -> Option<(u32, u32, &[String])> {
        self.tail
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct SemanticDigest(String);

impl SemanticDigest {
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for SemanticDigest {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}

#[derive(Clone, Debug)]
pub struct TypeSolution {
    pub(crate) validation_revision: u64,
    pub(crate) input_digest: Option<InputDigest>,
    pub(crate) nodes: Vec<SolvedNode>,
    pub(crate) rows: Vec<SolvedRow>,
    roots: Vec<SolvedTypeId>,
    exports: BTreeMap<ExportKey, RootId>,
    root_bytes: Vec<Vec<u8>>,
    digest: SemanticDigest,
}

impl TypeSolution {
    #[must_use]
    pub fn semantic_digest(&self) -> &SemanticDigest {
        &self.digest
    }

    #[must_use]
    pub fn input_digest(&self) -> Option<&InputDigest> {
        self.input_digest.as_ref()
    }

    #[must_use]
    pub fn export_root(&self, key: &ExportKey) -> Option<RootId> {
        self.exports.get(key).copied()
    }

    pub fn export_keys(&self) -> impl Iterator<Item = &ExportKey> {
        self.exports.keys()
    }

    #[must_use]
    pub fn root_type(&self, root: RootId) -> Option<SolvedTypeId> {
        self.roots.get(root.0 as usize).copied()
    }

    #[must_use]
    pub fn root_bytes(&self, root: RootId) -> Option<&[u8]> {
        self.root_bytes.get(root.0 as usize).map(Vec::as_slice)
    }

    #[must_use]
    pub fn view(&self, ty: SolvedTypeId) -> Option<SolvedTypeView<'_>> {
        self.nodes.get(ty.0 as usize).map(|node| match node {
            SolvedNode::Free {
                index,
                level,
                source: _,
            } => SolvedTypeView::Free {
                index: *index,
                level: *level,
            },
            SolvedNode::Rec(index) => SolvedTypeView::Rec(*index),
            SolvedNode::Primitive(primitive) => SolvedTypeView::Primitive(*primitive),
            SolvedNode::Named(name) => SolvedTypeView::Named(name),
            SolvedNode::Tuple(items) => SolvedTypeView::Tuple(items),
            SolvedNode::List(item) => SolvedTypeView::List(*item),
            SolvedNode::Option(item) => SolvedTypeView::Option(*item),
            SolvedNode::Function { parameters, result } => SolvedTypeView::Function {
                parameters,
                result: *result,
            },
            SolvedNode::Record(row) => SolvedTypeView::Record(*row),
            SolvedNode::Variant(row) => SolvedTypeView::Variant(*row),
            SolvedNode::Mu { binder, body } => SolvedTypeView::Mu {
                binder: *binder,
                body: *body,
            },
        })
    }

    #[must_use]
    pub fn row(&self, row: SolvedRowId) -> Option<SolvedRowView<'_>> {
        self.rows.get(row.0 as usize).map(|row| SolvedRowView {
            kind: row.kind,
            fields: &row.fields,
            tail: row
                .tail
                .as_ref()
                .map(|tail| (tail.index, tail.level, tail.lacks.as_slice())),
        })
    }
}

#[derive(Clone, Debug)]
struct TypeBinding {
    ty: DraftTypeId,
    proof: usize,
}

#[derive(Clone, Debug)]
struct RowBinding {
    expr: RowExpr,
    proof: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct RowExpr {
    kind: RowKind,
    fields: BTreeMap<String, DraftTypeId>,
    tail: Option<usize>,
}

#[derive(Debug)]
struct State {
    scope: u64,
    nodes: Vec<DraftNode>,
    rows: Vec<DraftRowNode>,
    type_subst: Vec<Option<TypeBinding>>,
    type_levels: Vec<u32>,
    row_subst: Vec<Option<RowBinding>>,
    row_levels: Vec<u32>,
    row_lacks: Vec<BTreeMap<String, usize>>,
}

impl State {
    fn new(problem: &ClosedProblem) -> Self {
        let meta_count = problem
            .nodes
            .iter()
            .filter_map(|node| match node {
                DraftNode::Meta { ordinal, .. } => Some(*ordinal as usize + 1),
                _ => None,
            })
            .max()
            .unwrap_or(0);
        let mut type_levels = vec![0; meta_count];
        for node in &problem.nodes {
            if let DraftNode::Meta { level, ordinal } = node {
                type_levels[*ordinal as usize] = *level;
            }
        }
        let row_levels = problem
            .rows
            .iter()
            .map(|row| match row {
                DraftRowNode::Meta { level, .. } => *level,
                DraftRowNode::Extend { tail, .. } => tail
                    .and_then(|tail| problem.rows.get(tail.slot as usize))
                    .and_then(|row| match row {
                        DraftRowNode::Meta { level, .. } => Some(*level),
                        DraftRowNode::Extend { .. } => None,
                    })
                    .unwrap_or(0),
            })
            .collect::<Vec<_>>();
        Self {
            scope: problem.validation_revision,
            nodes: problem.nodes.clone(),
            rows: problem.rows.clone(),
            type_subst: vec![None; meta_count],
            type_levels,
            row_subst: vec![None; problem.rows.len()],
            row_levels,
            row_lacks: vec![BTreeMap::new(); problem.rows.len()],
        }
    }

    fn node(&self, ty: DraftTypeId) -> Option<&DraftNode> {
        (ty.scope == self.scope)
            .then(|| self.nodes.get(ty.slot as usize))
            .flatten()
    }

    fn row_expr(&self, slot: usize) -> Option<RowExpr> {
        self.rows.get(slot).map(|row| match row {
            DraftRowNode::Meta { kind, .. } => RowExpr {
                kind: *kind,
                fields: BTreeMap::new(),
                tail: Some(slot),
            },
            DraftRowNode::Extend { kind, fields, tail } => RowExpr {
                kind: *kind,
                fields: fields.clone(),
                tail: tail.map(|tail| tail.slot as usize),
            },
        })
    }

    fn node_name(&self, ty: DraftTypeId) -> String {
        self.node(ty).map_or_else(
            || "dangling".to_owned(),
            |node| match node {
                DraftNode::Meta { ordinal, .. } => format!("meta({ordinal})"),
                DraftNode::Rec(_) => "rec".to_owned(),
                DraftNode::Primitive(value) => format!("{value:?}"),
                DraftNode::Named(value) => format!("named({value})"),
                DraftNode::Tuple(items) => format!("tuple({})", items.len()),
                DraftNode::List(_) => "list".to_owned(),
                DraftNode::Option(_) => "option".to_owned(),
                DraftNode::Function { parameters, .. } => {
                    format!("function({})", parameters.len())
                }
                DraftNode::Record(_) => "record".to_owned(),
                DraftNode::Variant(_) => "variant".to_owned(),
                DraftNode::Mu { .. } => "mu".to_owned(),
            },
        )
    }
}

/// Solve consumes the certified problem. Failure never publishes a partial solution.
pub fn solve(
    problem: ClosedProblem,
    operations: &mut OperationContext,
) -> Result<TypeSolution, TypeError> {
    let mut state = State::new(&problem);
    state.seed_prefix_lacks(operations)?;
    let mut ordered = problem.constraints.clone();
    ordered.sort_by_key(constraint_key);
    operations.charge(
        ordered.len().max(1),
        "solve.order.constraints",
        &[],
        [Origin::boundary("solve")],
    )?;
    for constraint in ordered {
        match constraint {
            Constraint::Equal {
                left,
                right,
                origin,
            } => {
                let proof = operations.charge(1, "solve.constraint.equal", &[], [origin])?;
                state.unify_types(left, right, proof, operations)?;
            }
            Constraint::Lacks { row, label, origin } => {
                let proof = operations.charge(1, "solve.constraint.lacks", &[], [origin])?;
                state.add_lacks(row.slot as usize, &label, proof, operations)?;
            }
        }
    }
    let mut nodes = Vec::new();
    let mut rows = Vec::new();
    let mut roots = Vec::new();
    let mut root_bytes = Vec::new();
    let mut exports = BTreeMap::new();
    for export in &problem.exports {
        let (root, bytes) = state.zonk_root(export.ty, &mut nodes, &mut rows, operations)?;
        let root_id = RootId(u32::try_from(roots.len()).map_err(|_| {
            operations.fail(TypeErrorKind::ArithmeticOverflow, &[], "solve.publish")
        })?);
        roots.push(root);
        root_bytes.push(bytes);
        exports.insert(export.key.clone(), root_id);
    }
    let mut digest_material = Vec::new();
    for (key, root) in &exports {
        digest_material.extend_from_slice(&(key.as_str().len() as u64).to_le_bytes());
        digest_material.extend_from_slice(key.as_str().as_bytes());
        let bytes = &root_bytes[root.0 as usize];
        digest_material.extend_from_slice(&(bytes.len() as u64).to_le_bytes());
        digest_material.extend_from_slice(bytes);
    }
    operations.charge(
        digest_material.len().max(1),
        "solve.semantic_digest.bytes",
        &[],
        [Origin::boundary("solve.digest")],
    )?;
    let digest = SemanticDigest(format!("fnv1a64:{:016x}", fnv1a64(&digest_material)));
    Ok(TypeSolution {
        validation_revision: problem.validation_revision,
        input_digest: problem.input_digest,
        nodes,
        rows,
        roots,
        exports,
        root_bytes,
        digest,
    })
}

fn constraint_key(constraint: &Constraint) -> (u8, u32, u32, String) {
    match constraint {
        Constraint::Equal { left, right, .. } => (
            0,
            left.slot.min(right.slot),
            left.slot.max(right.slot),
            String::new(),
        ),
        Constraint::Lacks { row, label, .. } => (1, row.slot, row.slot, label.clone()),
    }
}

fn meta_ordinal(node: &DraftNode) -> Option<u32> {
    match node {
        DraftNode::Meta { ordinal, .. } => Some(*ordinal),
        _ => None,
    }
}

fn rec_pair_matches(
    left: super::store::DraftRecVar,
    right: super::store::DraftRecVar,
    pairs: &BTreeMap<super::store::DraftRecVar, super::store::DraftRecVar>,
) -> bool {
    pairs.get(&left).is_some_and(|paired| *paired == right)
}
