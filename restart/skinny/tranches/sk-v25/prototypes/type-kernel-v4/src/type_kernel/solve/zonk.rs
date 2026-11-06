//! Root-local iterative zonking into flat solved type and row arenas.

use std::collections::BTreeMap;

use super::{SolvedNode, SolvedRow, SolvedRowId, SolvedRowTail, SolvedTypeId, State, meta_ordinal};
use crate::type_kernel::store::{DraftNode, DraftRecVar, DraftTypeId, Primitive, RowKind};
use crate::type_kernel::{OperationContext, Origin, TypeError, TypeErrorKind};

enum Frame {
    Enter(DraftTypeId),
    Tuple(usize),
    List,
    Option,
    Function(usize),
    Row {
        kind: RowKind,
        fields: Vec<String>,
        tail: Option<usize>,
    },
    Mu {
        binder: DraftRecVar,
        canonical: u32,
        previous: Option<u32>,
    },
}

impl State {
    pub(super) fn zonk_root(
        &self,
        root: DraftTypeId,
        solved_nodes: &mut Vec<SolvedNode>,
        solved_rows: &mut Vec<SolvedRow>,
        operations: &mut OperationContext,
    ) -> Result<(SolvedTypeId, Vec<u8>), TypeError> {
        let node_start = solved_nodes.len();
        let row_start = solved_rows.len();
        let mut pending = vec![Frame::Enter(root)];
        let mut values = Vec::<SolvedTypeId>::new();
        let mut binders = BTreeMap::<DraftRecVar, u32>::new();
        let mut next_binder = 0_u32;
        let mut free_types = BTreeMap::<u32, u32>::new();
        let mut free_rows = BTreeMap::<usize, u32>::new();
        while let Some(frame) = pending.pop() {
            operations.charge(1, "solve.zonk.frame", &[], [Origin::boundary("solve.zonk")])?;
            match frame {
                Frame::Enter(mut ty) => {
                    let mut seen = std::collections::BTreeSet::new();
                    loop {
                        if !seen.insert(ty) {
                            return Err(operations.fail(
                                TypeErrorKind::InternalInvariant {
                                    detail: "cyclic substitution during zonk".to_owned(),
                                },
                                &[],
                                "solve.zonk",
                            ));
                        }
                        let Some(ordinal) = self.node(ty).and_then(meta_ordinal) else {
                            break;
                        };
                        let Some(binding) = &self.type_subst[ordinal as usize] else {
                            break;
                        };
                        operations.charge(1, "solve.zonk.prune", &[binding.proof], [])?;
                        ty = binding.ty;
                    }
                    let node = self.node(ty).cloned().ok_or_else(|| {
                        operations.fail(TypeErrorKind::DanglingType, &[], "solve.zonk")
                    })?;
                    match node {
                        DraftNode::Meta { ordinal, .. } => {
                            let next = u32::try_from(free_types.len()).map_err(|_| {
                                operations.fail(
                                    TypeErrorKind::ArithmeticOverflow,
                                    &[],
                                    "solve.zonk.free",
                                )
                            })?;
                            let index = *free_types.entry(ordinal).or_insert(next);
                            push_node(
                                solved_nodes,
                                SolvedNode::Free {
                                    index,
                                    level: self.type_levels[ordinal as usize],
                                    source: ordinal,
                                },
                                &mut values,
                                operations,
                            )?;
                        }
                        DraftNode::Rec(binder) => {
                            let canonical = binders.get(&binder).copied().ok_or_else(|| {
                                operations.fail(
                                    TypeErrorKind::EscapingRecursion {
                                        site: format!("type:{}", ty.slot),
                                    },
                                    &[],
                                    "solve.zonk.rec",
                                )
                            })?;
                            push_node(
                                solved_nodes,
                                SolvedNode::Rec(canonical),
                                &mut values,
                                operations,
                            )?;
                        }
                        DraftNode::Primitive(primitive) => push_node(
                            solved_nodes,
                            SolvedNode::Primitive(primitive),
                            &mut values,
                            operations,
                        )?,
                        DraftNode::Named(name) => push_node(
                            solved_nodes,
                            SolvedNode::Named(name),
                            &mut values,
                            operations,
                        )?,
                        DraftNode::Tuple(items) => {
                            let count = items.len();
                            pending.push(Frame::Tuple(count));
                            pending.extend(items.into_iter().rev().map(Frame::Enter));
                        }
                        DraftNode::List(item) => {
                            pending.push(Frame::List);
                            pending.push(Frame::Enter(item));
                        }
                        DraftNode::Option(item) => {
                            pending.push(Frame::Option);
                            pending.push(Frame::Enter(item));
                        }
                        DraftNode::Function { parameters, result } => {
                            let count = parameters.len();
                            pending.push(Frame::Function(count));
                            pending.push(Frame::Enter(result));
                            pending.extend(parameters.into_iter().rev().map(Frame::Enter));
                        }
                        DraftNode::Record(row) | DraftNode::Variant(row) => {
                            let proof = operations.charge(
                                1,
                                "solve.zonk.row",
                                &[],
                                [Origin::boundary("solve.zonk.row")],
                            )?;
                            let expr = self.row_expr(row.slot as usize).ok_or_else(|| {
                                operations.fail(
                                    TypeErrorKind::DanglingRow,
                                    &[proof],
                                    "solve.zonk.row",
                                )
                            })?;
                            let expr = self.flatten_row(expr, proof, operations)?;
                            let fields = expr.fields.keys().cloned().collect::<Vec<_>>();
                            pending.push(Frame::Row {
                                kind: expr.kind,
                                fields,
                                tail: expr.tail,
                            });
                            pending.extend(expr.fields.values().rev().copied().map(Frame::Enter));
                        }
                        DraftNode::Mu { binder, body } => {
                            let canonical = next_binder;
                            next_binder = next_binder.checked_add(1).ok_or_else(|| {
                                operations.fail(
                                    TypeErrorKind::ArithmeticOverflow,
                                    &[],
                                    "solve.zonk.mu",
                                )
                            })?;
                            let previous = binders.insert(binder, canonical);
                            pending.push(Frame::Mu {
                                binder,
                                canonical,
                                previous,
                            });
                            pending.push(Frame::Enter(body));
                        }
                    }
                }
                Frame::Tuple(count) => {
                    let items = take_values(&mut values, count, operations)?;
                    push_node(
                        solved_nodes,
                        SolvedNode::Tuple(items),
                        &mut values,
                        operations,
                    )?;
                }
                Frame::List => {
                    let item = pop_value(&mut values, operations)?;
                    push_node(
                        solved_nodes,
                        SolvedNode::List(item),
                        &mut values,
                        operations,
                    )?;
                }
                Frame::Option => {
                    let item = pop_value(&mut values, operations)?;
                    push_node(
                        solved_nodes,
                        SolvedNode::Option(item),
                        &mut values,
                        operations,
                    )?;
                }
                Frame::Function(count) => {
                    let result = pop_value(&mut values, operations)?;
                    let parameters = take_values(&mut values, count, operations)?;
                    push_node(
                        solved_nodes,
                        SolvedNode::Function { parameters, result },
                        &mut values,
                        operations,
                    )?;
                }
                Frame::Row { kind, fields, tail } => {
                    let field_types = take_values(&mut values, fields.len(), operations)?;
                    let fields = fields.into_iter().zip(field_types).collect::<Vec<_>>();
                    let tail = if let Some(tail) = tail {
                        let next = u32::try_from(free_rows.len()).map_err(|_| {
                            operations.fail(
                                TypeErrorKind::ArithmeticOverflow,
                                &[],
                                "solve.zonk.row_tail",
                            )
                        })?;
                        let index = *free_rows.entry(tail).or_insert(next);
                        Some(SolvedRowTail {
                            index,
                            level: self.row_levels[tail],
                            lacks: self.row_lacks[tail].keys().cloned().collect(),
                            source: u32::try_from(tail).map_err(|_| {
                                operations.fail(
                                    TypeErrorKind::ArithmeticOverflow,
                                    &[],
                                    "solve.zonk.row_tail",
                                )
                            })?,
                        })
                    } else {
                        None
                    };
                    let row_id = SolvedRowId(u32::try_from(solved_rows.len()).map_err(|_| {
                        operations.fail(TypeErrorKind::ArithmeticOverflow, &[], "solve.zonk.row")
                    })?);
                    solved_rows.push(SolvedRow { kind, fields, tail });
                    let node = match kind {
                        RowKind::Record => SolvedNode::Record(row_id),
                        RowKind::Variant => SolvedNode::Variant(row_id),
                    };
                    push_node(solved_nodes, node, &mut values, operations)?;
                }
                Frame::Mu {
                    binder,
                    canonical,
                    previous,
                } => {
                    let body = pop_value(&mut values, operations)?;
                    if let Some(previous) = previous {
                        binders.insert(binder, previous);
                    } else {
                        binders.remove(&binder);
                    }
                    push_node(
                        solved_nodes,
                        SolvedNode::Mu {
                            binder: canonical,
                            body,
                        },
                        &mut values,
                        operations,
                    )?;
                }
            }
        }
        if values.len() != 1 {
            return Err(operations.fail(
                TypeErrorKind::InternalInvariant {
                    detail: "zonk value stack was not singular".to_owned(),
                },
                &[],
                "solve.zonk",
            ));
        }
        let root = values.pop().expect("length checked");
        let bytes = canonical_bytes(
            solved_nodes,
            solved_rows,
            root,
            node_start,
            row_start,
            operations,
        )?;
        Ok((root, bytes))
    }
}

fn push_node(
    nodes: &mut Vec<SolvedNode>,
    node: SolvedNode,
    values: &mut Vec<SolvedTypeId>,
    operations: &OperationContext,
) -> Result<(), TypeError> {
    let id =
        SolvedTypeId(u32::try_from(nodes.len()).map_err(|_| {
            operations.fail(TypeErrorKind::ArithmeticOverflow, &[], "solve.zonk.node")
        })?);
    nodes.push(node);
    values.push(id);
    Ok(())
}

fn pop_value(
    values: &mut Vec<SolvedTypeId>,
    operations: &OperationContext,
) -> Result<SolvedTypeId, TypeError> {
    values.pop().ok_or_else(|| {
        operations.fail(
            TypeErrorKind::InternalInvariant {
                detail: "zonk value stack underflow".to_owned(),
            },
            &[],
            "solve.zonk",
        )
    })
}

fn take_values(
    values: &mut Vec<SolvedTypeId>,
    count: usize,
    operations: &OperationContext,
) -> Result<Vec<SolvedTypeId>, TypeError> {
    if values.len() < count {
        return Err(operations.fail(
            TypeErrorKind::InternalInvariant {
                detail: "zonk value stack underflow".to_owned(),
            },
            &[],
            "solve.zonk",
        ));
    }
    Ok(values.split_off(values.len() - count))
}

fn canonical_bytes(
    nodes: &[SolvedNode],
    rows: &[SolvedRow],
    root: SolvedTypeId,
    node_start: usize,
    row_start: usize,
    operations: &mut OperationContext,
) -> Result<Vec<u8>, TypeError> {
    let mut bytes = b"TKV4\0".to_vec();
    put_u32(&mut bytes, nodes.len() - node_start, operations)?;
    put_u32(&mut bytes, rows.len() - row_start, operations)?;
    for node in &nodes[node_start..] {
        match node {
            SolvedNode::Free {
                index,
                level,
                source: _,
            } => {
                bytes.push(0);
                bytes.extend_from_slice(&index.to_le_bytes());
                bytes.extend_from_slice(&level.to_le_bytes());
            }
            SolvedNode::Rec(index) => {
                bytes.push(1);
                bytes.extend_from_slice(&index.to_le_bytes());
            }
            SolvedNode::Primitive(primitive) => {
                bytes.push(2);
                bytes.push(primitive_tag(*primitive));
            }
            SolvedNode::Named(name) => {
                bytes.push(3);
                put_text(&mut bytes, name, operations)?;
            }
            SolvedNode::Tuple(items) => {
                bytes.push(4);
                put_u32(&mut bytes, items.len(), operations)?;
                for item in items {
                    put_local(&mut bytes, item.0, node_start, operations)?;
                }
            }
            SolvedNode::List(item) => {
                bytes.push(5);
                put_local(&mut bytes, item.0, node_start, operations)?;
            }
            SolvedNode::Option(item) => {
                bytes.push(6);
                put_local(&mut bytes, item.0, node_start, operations)?;
            }
            SolvedNode::Function { parameters, result } => {
                bytes.push(7);
                put_u32(&mut bytes, parameters.len(), operations)?;
                for parameter in parameters {
                    put_local(&mut bytes, parameter.0, node_start, operations)?;
                }
                put_local(&mut bytes, result.0, node_start, operations)?;
            }
            SolvedNode::Record(row) => {
                bytes.push(8);
                put_local(&mut bytes, row.0, row_start, operations)?;
            }
            SolvedNode::Variant(row) => {
                bytes.push(9);
                put_local(&mut bytes, row.0, row_start, operations)?;
            }
            SolvedNode::Mu { binder, body } => {
                bytes.push(10);
                bytes.extend_from_slice(&binder.to_le_bytes());
                put_local(&mut bytes, body.0, node_start, operations)?;
            }
        }
    }
    for row in &rows[row_start..] {
        bytes.push(match row.kind {
            RowKind::Record => 11,
            RowKind::Variant => 12,
        });
        put_u32(&mut bytes, row.fields.len(), operations)?;
        for (label, ty) in &row.fields {
            put_text(&mut bytes, label, operations)?;
            put_local(&mut bytes, ty.0, node_start, operations)?;
        }
        if let Some(tail) = &row.tail {
            bytes.push(1);
            bytes.extend_from_slice(&tail.index.to_le_bytes());
            bytes.extend_from_slice(&tail.level.to_le_bytes());
            put_u32(&mut bytes, tail.lacks.len(), operations)?;
            for label in &tail.lacks {
                put_text(&mut bytes, label, operations)?;
            }
        } else {
            bytes.push(0);
        }
    }
    put_local(&mut bytes, root.0, node_start, operations)?;
    operations.charge(
        bytes.len().max(1),
        "solve.zonk.canonical_bytes",
        &[],
        [Origin::boundary("solve.zonk")],
    )?;
    Ok(bytes)
}

fn put_u32(
    bytes: &mut Vec<u8>,
    value: usize,
    operations: &OperationContext,
) -> Result<(), TypeError> {
    let value = u32::try_from(value)
        .map_err(|_| operations.fail(TypeErrorKind::ArithmeticOverflow, &[], "solve.zonk.bytes"))?;
    bytes.extend_from_slice(&value.to_le_bytes());
    Ok(())
}

fn put_local(
    bytes: &mut Vec<u8>,
    value: u32,
    start: usize,
    operations: &OperationContext,
) -> Result<(), TypeError> {
    let start = u32::try_from(start)
        .map_err(|_| operations.fail(TypeErrorKind::ArithmeticOverflow, &[], "solve.zonk.bytes"))?;
    let local = value.checked_sub(start).ok_or_else(|| {
        operations.fail(
            TypeErrorKind::InternalInvariant {
                detail: "cross-root solved edge".to_owned(),
            },
            &[],
            "solve.zonk.bytes",
        )
    })?;
    bytes.extend_from_slice(&local.to_le_bytes());
    Ok(())
}

fn put_text(
    bytes: &mut Vec<u8>,
    text: &str,
    operations: &OperationContext,
) -> Result<(), TypeError> {
    put_u32(bytes, text.len(), operations)?;
    bytes.extend_from_slice(text.as_bytes());
    Ok(())
}

const fn primitive_tag(primitive: Primitive) -> u8 {
    match primitive {
        Primitive::Bool => 0,
        Primitive::Bytes => 1,
        Primitive::Diagnostic => 2,
        Primitive::Enum => 3,
        Primitive::BoxedEnum => 4,
        Primitive::F64 => 5,
        Primitive::I8 => 6,
        Primitive::I16 => 7,
        Primitive::I32 => 8,
        Primitive::I64 => 9,
        Primitive::ParseResult => 10,
        Primitive::Span => 11,
        Primitive::Str => 12,
        Primitive::U8 => 13,
        Primitive::U16 => 14,
        Primitive::U32 => 15,
        Primitive::U64 => 16,
        Primitive::Unit => 17,
    }
}
