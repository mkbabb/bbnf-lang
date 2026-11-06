//! Flat batch schemes, qualified rows, value restriction, and charged instantiation.

use std::collections::{BTreeMap, BTreeSet};

use super::solve::{RootId, SolvedNode, SolvedTypeId, TypeSolution};
use super::store::{DraftRecVar, DraftRowId, DraftTypeId, ProblemBuilder, RowKind};
use super::{OperationContext, Origin, TypeError, TypeErrorKind, fnv1a64};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BindingForm {
    SyntacticValue,
    Expansive,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct PolyTypeId(u32);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct PolyRowId(u32);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum PolyVar {
    Quantified(u32),
    Rigid {
        canonical: u32,
        key: u64,
        level: u32,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum PolyNode {
    Var(PolyVar),
    Rec {
        group: u32,
        binder: u32,
    },
    Primitive(super::Primitive),
    Named(String),
    Tuple(Vec<PolyTypeId>),
    List(PolyTypeId),
    Option(PolyTypeId),
    Function {
        parameters: Vec<PolyTypeId>,
        result: PolyTypeId,
    },
    Record(PolyRowId),
    Variant(PolyRowId),
    Mu {
        group: u32,
        binder: u32,
        body: PolyTypeId,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct PolyRow {
    kind: RowKind,
    fields: Vec<(String, PolyTypeId)>,
    tail: Option<PolyRowTail>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct PolyRowTail {
    var: PolyVar,
    lacks: Vec<String>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Scheme {
    nodes: Vec<PolyNode>,
    rows: Vec<PolyRow>,
    roots: Vec<PolyTypeId>,
    type_quantifiers: u32,
    row_quantifiers: u32,
    digest: String,
}

impl Scheme {
    #[must_use]
    pub fn root_count(&self) -> usize {
        self.roots.len()
    }

    #[must_use]
    pub fn type_quantifiers(&self) -> u32 {
        self.type_quantifiers
    }

    #[must_use]
    pub fn row_quantifiers(&self) -> u32 {
        self.row_quantifiers
    }

    #[must_use]
    pub fn digest(&self) -> &str {
        &self.digest
    }
}

#[derive(Clone, Debug, Default)]
pub struct RigidContext {
    types: BTreeMap<u64, DraftTypeId>,
    rows: BTreeMap<u64, DraftRowId>,
}

impl RigidContext {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Clone, Debug)]
pub struct InstantiatedRoot {
    roots: Vec<DraftTypeId>,
}

impl InstantiatedRoot {
    #[must_use]
    pub fn roots(&self) -> &[DraftTypeId] {
        &self.roots
    }

    #[must_use]
    pub fn root(&self) -> Option<DraftTypeId> {
        self.roots.first().copied()
    }
}

pub fn generalize(
    solution: &TypeSolution,
    root: RootId,
    environment_level: u32,
    binding: BindingForm,
    operations: &mut OperationContext,
) -> Result<Scheme, TypeError> {
    generalize_roots(solution, &[root], environment_level, binding, operations)
}

pub fn generalize_roots(
    solution: &TypeSolution,
    roots: &[RootId],
    environment_level: u32,
    binding: BindingForm,
    operations: &mut OperationContext,
) -> Result<Scheme, TypeError> {
    let mut poly_nodes = Vec::new();
    let mut poly_rows = Vec::new();
    let mut poly_roots = Vec::new();
    let mut type_quantified = BTreeMap::<u32, u32>::new();
    let mut row_quantified = BTreeMap::<u32, u32>::new();
    let mut type_rigids = BTreeMap::<u32, u32>::new();
    let mut row_rigids = BTreeMap::<u32, u32>::new();
    for (group, root) in roots.iter().enumerate() {
        let solved_root = solution.root_type(*root).ok_or_else(|| {
            operations.fail(
                TypeErrorKind::InvalidInput {
                    detail: "unknown solution root".to_owned(),
                },
                &[],
                "scheme.generalize",
            )
        })?;
        let (reachable_types, _reachable_rows) = reachable(solution, solved_root, operations)?;
        let mut type_map = BTreeMap::<SolvedTypeId, PolyTypeId>::new();
        let mut row_map = BTreeMap::<super::SolvedRowId, PolyRowId>::new();
        for solved in reachable_types {
            let node = &solution.nodes[solved.0 as usize];
            let poly = match node {
                SolvedNode::Free { level, source, .. } => PolyNode::Var(classify_type_var(
                    *source,
                    *level,
                    environment_level,
                    binding,
                    solution.validation_revision,
                    &mut type_quantified,
                    &mut type_rigids,
                )),
                SolvedNode::Rec(binder) => PolyNode::Rec {
                    group: u32::try_from(group).map_err(|_| {
                        operations.fail(TypeErrorKind::ArithmeticOverflow, &[], "scheme.generalize")
                    })?,
                    binder: *binder,
                },
                SolvedNode::Primitive(value) => PolyNode::Primitive(*value),
                SolvedNode::Named(value) => PolyNode::Named(value.clone()),
                SolvedNode::Tuple(items) => {
                    PolyNode::Tuple(map_types(items, &type_map, operations)?)
                }
                SolvedNode::List(item) => PolyNode::List(type_map[item]),
                SolvedNode::Option(item) => PolyNode::Option(type_map[item]),
                SolvedNode::Function { parameters, result } => PolyNode::Function {
                    parameters: map_types(parameters, &type_map, operations)?,
                    result: type_map[result],
                },
                SolvedNode::Record(row) => PolyNode::Record(materialize_poly_row(
                    *row,
                    solution,
                    &type_map,
                    &mut row_map,
                    &mut poly_rows,
                    environment_level,
                    binding,
                    &mut row_quantified,
                    &mut row_rigids,
                    operations,
                )?),
                SolvedNode::Variant(row) => PolyNode::Variant(materialize_poly_row(
                    *row,
                    solution,
                    &type_map,
                    &mut row_map,
                    &mut poly_rows,
                    environment_level,
                    binding,
                    &mut row_quantified,
                    &mut row_rigids,
                    operations,
                )?),
                SolvedNode::Mu { binder, body } => PolyNode::Mu {
                    group: u32::try_from(group).map_err(|_| {
                        operations.fail(TypeErrorKind::ArithmeticOverflow, &[], "scheme.generalize")
                    })?,
                    binder: *binder,
                    body: type_map[body],
                },
            };
            operations.charge(
                1,
                "scheme.generalize.node",
                &[],
                [Origin::boundary("scheme.generalize")],
            )?;
            let poly_id = PolyTypeId(u32::try_from(poly_nodes.len()).map_err(|_| {
                operations.fail(
                    TypeErrorKind::ArithmeticOverflow,
                    &[],
                    "scheme.generalize.node",
                )
            })?);
            poly_nodes.push(poly);
            type_map.insert(solved, poly_id);
        }
        poly_roots.push(type_map[&solved_root]);
    }
    let digest_material = format!(
        "TKV4-SCHEME\n{:?}\n{:?}\n{:?}\n{}\n{}\n",
        poly_nodes,
        poly_rows,
        poly_roots,
        type_quantified.len(),
        row_quantified.len()
    );
    operations.charge(
        digest_material.len().max(1),
        "scheme.digest.bytes",
        &[],
        [Origin::boundary("scheme.digest")],
    )?;
    Ok(Scheme {
        nodes: poly_nodes,
        rows: poly_rows,
        roots: poly_roots,
        type_quantifiers: u32::try_from(type_quantified.len()).map_err(|_| {
            operations.fail(TypeErrorKind::ArithmeticOverflow, &[], "scheme.generalize")
        })?,
        row_quantifiers: u32::try_from(row_quantified.len()).map_err(|_| {
            operations.fail(TypeErrorKind::ArithmeticOverflow, &[], "scheme.generalize")
        })?,
        digest: format!("fnv1a64:{:016x}", fnv1a64(digest_material.as_bytes())),
    })
}

pub fn instantiate(
    scheme: &Scheme,
    builder: &mut ProblemBuilder,
    rigids: &mut RigidContext,
    level: u32,
    origin: Origin,
    operations: &mut OperationContext,
) -> Result<InstantiatedRoot, TypeError> {
    let mut types = Vec::<DraftTypeId>::with_capacity(scheme.nodes.len());
    let mut quantified_types = BTreeMap::<u32, DraftTypeId>::new();
    let mut quantified_rows = BTreeMap::<u32, DraftRowId>::new();
    let mut rec_vars = BTreeMap::<(u32, u32), DraftRecVar>::new();
    for node in &scheme.nodes {
        operations.charge(1, "scheme.instantiate.node", &[], [origin.clone()])?;
        let ty = match node {
            PolyNode::Var(var) => instantiate_type_var(
                *var,
                builder,
                rigids,
                &mut quantified_types,
                level,
                operations,
            )?,
            PolyNode::Rec { group, binder } => {
                let rec = rec_var((*group, *binder), &mut rec_vars, builder, operations)?;
                builder.rec_var(rec, operations)?
            }
            PolyNode::Primitive(value) => builder.primitive(*value, operations)?,
            PolyNode::Named(value) => builder.named(value.clone(), operations)?,
            PolyNode::Tuple(items) => {
                builder.tuple(items.iter().map(|id| types[id.0 as usize]), operations)?
            }
            PolyNode::List(item) => builder.list(types[item.0 as usize], operations)?,
            PolyNode::Option(item) => builder.option(types[item.0 as usize], operations)?,
            PolyNode::Function { parameters, result } => builder.function(
                parameters.iter().map(|id| types[id.0 as usize]),
                types[result.0 as usize],
                operations,
            )?,
            PolyNode::Record(row) | PolyNode::Variant(row) => {
                let row = &scheme.rows[row.0 as usize];
                let tail = row
                    .tail
                    .as_ref()
                    .map(|tail| {
                        instantiate_row_var(
                            tail.var,
                            row.kind,
                            builder,
                            rigids,
                            &mut quantified_rows,
                            level,
                            operations,
                        )
                    })
                    .transpose()?;
                if let (Some(tail_id), Some(tail_spec)) = (tail, row.tail.as_ref()) {
                    for label in &tail_spec.lacks {
                        builder.lacks(tail_id, label.clone(), origin.clone(), operations)?;
                    }
                }
                let row_id = builder.row(
                    row.kind,
                    row.fields
                        .iter()
                        .map(|(label, id)| (label.clone(), types[id.0 as usize])),
                    tail,
                    origin.clone(),
                    operations,
                )?;
                match row.kind {
                    RowKind::Record => builder.record(row_id, operations)?,
                    RowKind::Variant => builder.variant(row_id, operations)?,
                }
            }
            PolyNode::Mu {
                group,
                binder,
                body,
            } => {
                let rec = rec_var((*group, *binder), &mut rec_vars, builder, operations)?;
                builder.mu(rec, types[body.0 as usize], operations)?
            }
        };
        types.push(ty);
    }
    Ok(InstantiatedRoot {
        roots: scheme
            .roots
            .iter()
            .map(|root| types[root.0 as usize])
            .collect(),
    })
}

#[allow(clippy::too_many_arguments)]
fn materialize_poly_row(
    row: super::SolvedRowId,
    solution: &TypeSolution,
    type_map: &BTreeMap<SolvedTypeId, PolyTypeId>,
    row_map: &mut BTreeMap<super::SolvedRowId, PolyRowId>,
    poly_rows: &mut Vec<PolyRow>,
    environment_level: u32,
    binding: BindingForm,
    quantified: &mut BTreeMap<u32, u32>,
    rigids: &mut BTreeMap<u32, u32>,
    operations: &OperationContext,
) -> Result<PolyRowId, TypeError> {
    if let Some(existing) = row_map.get(&row) {
        return Ok(*existing);
    }
    let solved = &solution.rows[row.0 as usize];
    let fields = solved
        .fields
        .iter()
        .map(|(label, ty)| {
            type_map
                .get(ty)
                .copied()
                .map(|ty| (label.clone(), ty))
                .ok_or_else(|| {
                    operations.fail(
                        TypeErrorKind::InternalInvariant {
                            detail: "non-topological row field".to_owned(),
                        },
                        &[],
                        "scheme.generalize.row",
                    )
                })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let tail = solved.tail.as_ref().map(|tail| PolyRowTail {
        var: classify_row_var(
            tail.source,
            tail.level,
            environment_level,
            binding,
            solution.validation_revision,
            quantified,
            rigids,
        ),
        lacks: tail.lacks.clone(),
    });
    let id = PolyRowId(u32::try_from(poly_rows.len()).map_err(|_| {
        operations.fail(
            TypeErrorKind::ArithmeticOverflow,
            &[],
            "scheme.generalize.row",
        )
    })?);
    poly_rows.push(PolyRow {
        kind: solved.kind,
        fields,
        tail,
    });
    row_map.insert(row, id);
    Ok(id)
}

fn reachable(
    solution: &TypeSolution,
    root: SolvedTypeId,
    operations: &mut OperationContext,
) -> Result<(Vec<SolvedTypeId>, Vec<super::SolvedRowId>), TypeError> {
    let mut pending_types = vec![root];
    let mut pending_rows = Vec::new();
    let mut types = BTreeSet::new();
    let mut rows = BTreeSet::new();
    while let Some(ty) = pending_types.pop() {
        operations.charge(
            1,
            "scheme.reachable.type",
            &[],
            [Origin::boundary("scheme.generalize")],
        )?;
        if !types.insert(ty) {
            continue;
        }
        match &solution.nodes[ty.0 as usize] {
            SolvedNode::Tuple(items) => pending_types.extend(items),
            SolvedNode::List(item) | SolvedNode::Option(item) => pending_types.push(*item),
            SolvedNode::Function { parameters, result } => {
                pending_types.extend(parameters);
                pending_types.push(*result);
            }
            SolvedNode::Record(row) | SolvedNode::Variant(row) => pending_rows.push(*row),
            SolvedNode::Mu { body, .. } => pending_types.push(*body),
            SolvedNode::Free { .. }
            | SolvedNode::Rec(_)
            | SolvedNode::Primitive(_)
            | SolvedNode::Named(_) => {}
        }
        while let Some(row) = pending_rows.pop() {
            operations.charge(
                1,
                "scheme.reachable.row",
                &[],
                [Origin::boundary("scheme.generalize")],
            )?;
            if rows.insert(row) {
                pending_types.extend(
                    solution.rows[row.0 as usize]
                        .fields
                        .iter()
                        .map(|(_, ty)| *ty),
                );
            }
        }
    }
    Ok((types.into_iter().collect(), rows.into_iter().collect()))
}

fn classify_type_var(
    source: u32,
    level: u32,
    environment_level: u32,
    binding: BindingForm,
    revision: u64,
    quantified: &mut BTreeMap<u32, u32>,
    rigids: &mut BTreeMap<u32, u32>,
) -> PolyVar {
    if binding == BindingForm::SyntacticValue && level > environment_level {
        let next = quantified.len() as u32;
        PolyVar::Quantified(*quantified.entry(source).or_insert(next))
    } else {
        let next = rigids.len() as u32;
        let canonical = *rigids.entry(source).or_insert(next);
        PolyVar::Rigid {
            canonical,
            key: revision.rotate_left(17) ^ u64::from(source),
            level,
        }
    }
}

fn classify_row_var(
    source: u32,
    level: u32,
    environment_level: u32,
    binding: BindingForm,
    revision: u64,
    quantified: &mut BTreeMap<u32, u32>,
    rigids: &mut BTreeMap<u32, u32>,
) -> PolyVar {
    classify_type_var(
        source,
        level,
        environment_level,
        binding,
        revision.rotate_left(7),
        quantified,
        rigids,
    )
}

fn map_types(
    items: &[SolvedTypeId],
    map: &BTreeMap<SolvedTypeId, PolyTypeId>,
    operations: &OperationContext,
) -> Result<Vec<PolyTypeId>, TypeError> {
    items
        .iter()
        .map(|item| {
            map.get(item).copied().ok_or_else(|| {
                operations.fail(
                    TypeErrorKind::InternalInvariant {
                        detail: "non-topological solved arena".to_owned(),
                    },
                    &[],
                    "scheme.generalize",
                )
            })
        })
        .collect()
}

fn instantiate_type_var(
    var: PolyVar,
    builder: &mut ProblemBuilder,
    rigids: &mut RigidContext,
    quantified: &mut BTreeMap<u32, DraftTypeId>,
    level: u32,
    operations: &mut OperationContext,
) -> Result<DraftTypeId, TypeError> {
    match var {
        PolyVar::Quantified(index) => {
            if let Some(existing) = quantified.get(&index) {
                Ok(*existing)
            } else {
                let fresh = builder.meta(level, operations)?;
                quantified.insert(index, fresh);
                Ok(fresh)
            }
        }
        PolyVar::Rigid {
            key,
            level: rigid_level,
            canonical: _,
        } => {
            if let Some(existing) = rigids.types.get(&key) {
                Ok(*existing)
            } else {
                let fresh = builder.meta(rigid_level, operations)?;
                rigids.types.insert(key, fresh);
                Ok(fresh)
            }
        }
    }
}

fn instantiate_row_var(
    var: PolyVar,
    kind: RowKind,
    builder: &mut ProblemBuilder,
    rigids: &mut RigidContext,
    quantified: &mut BTreeMap<u32, DraftRowId>,
    level: u32,
    operations: &mut OperationContext,
) -> Result<DraftRowId, TypeError> {
    match var {
        PolyVar::Quantified(index) => {
            if let Some(existing) = quantified.get(&index) {
                Ok(*existing)
            } else {
                let fresh = builder.row_meta(kind, level, operations)?;
                quantified.insert(index, fresh);
                Ok(fresh)
            }
        }
        PolyVar::Rigid {
            key,
            level: rigid_level,
            canonical: _,
        } => {
            if let Some(existing) = rigids.rows.get(&key) {
                Ok(*existing)
            } else {
                let fresh = builder.row_meta(kind, rigid_level, operations)?;
                rigids.rows.insert(key, fresh);
                Ok(fresh)
            }
        }
    }
}

fn rec_var(
    key: (u32, u32),
    vars: &mut BTreeMap<(u32, u32), DraftRecVar>,
    builder: &mut ProblemBuilder,
    operations: &mut OperationContext,
) -> Result<DraftRecVar, TypeError> {
    if let Some(existing) = vars.get(&key) {
        Ok(*existing)
    } else {
        let fresh = builder.fresh_rec_var(operations)?;
        vars.insert(key, fresh);
        Ok(fresh)
    }
}
