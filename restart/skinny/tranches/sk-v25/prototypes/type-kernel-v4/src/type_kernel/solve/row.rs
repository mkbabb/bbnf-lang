//! Complete record/variant row unification, lacks, occurs, and lowering laws.

use std::collections::{BTreeMap, BTreeSet};

use super::{RowBinding, RowExpr, State};
use crate::type_kernel::store::{DraftNode, DraftRowNode, DraftTypeId, RowKind};
use crate::type_kernel::{OperationContext, Origin, TypeError, TypeErrorKind};

impl State {
    pub(super) fn seed_prefix_lacks(
        &mut self,
        operations: &mut OperationContext,
    ) -> Result<(), TypeError> {
        let prefixes = self
            .rows
            .iter()
            .filter_map(|row| match row {
                DraftRowNode::Extend {
                    fields,
                    tail: Some(tail),
                    ..
                } => Some((
                    tail.slot as usize,
                    fields.keys().cloned().collect::<Vec<_>>(),
                )),
                DraftRowNode::Meta { .. } | DraftRowNode::Extend { tail: None, .. } => None,
            })
            .collect::<Vec<_>>();
        for (tail, labels) in prefixes {
            for label in labels {
                let proof = operations.charge(
                    1,
                    "solve.row.prefix_lacks",
                    &[],
                    [Origin::new("solve.row", label.clone())],
                )?;
                self.add_lacks(tail, &label, proof, operations)?;
            }
        }
        Ok(())
    }

    pub(super) fn add_lacks(
        &mut self,
        row: usize,
        label: &str,
        proof: usize,
        operations: &mut OperationContext,
    ) -> Result<(), TypeError> {
        operations.charge(1, "solve.row.add_lacks", &[proof], [])?;
        let expr = self.row_expr(row).ok_or_else(|| {
            operations.fail(TypeErrorKind::DanglingRow, &[proof], "solve.row.lacks")
        })?;
        let flattened = self.flatten_row(expr, proof, operations)?;
        if flattened.fields.contains_key(label) {
            let mut parents = vec![proof];
            if let Some(binding) = self.row_subst.get(row).and_then(Option::as_ref) {
                parents.push(binding.proof);
            }
            return Err(operations.fail(
                TypeErrorKind::RowLacks {
                    row: u32::try_from(row).unwrap_or(u32::MAX),
                    label: label.to_owned(),
                },
                &parents,
                "solve.row.lacks",
            ));
        }
        if let Some(tail) = flattened.tail {
            self.row_lacks[tail]
                .entry(label.to_owned())
                .or_insert(proof);
        }
        Ok(())
    }

    pub(super) fn unify_rows(
        &mut self,
        left: RowExpr,
        right: RowExpr,
        proof: usize,
        operations: &mut OperationContext,
    ) -> Result<Vec<(DraftTypeId, DraftTypeId, usize)>, TypeError> {
        if left.kind != right.kind {
            return Err(operations.fail(TypeErrorKind::RowKindMismatch, &[proof], "solve.row"));
        }
        let left = self.flatten_row(left, proof, operations)?;
        let right = self.flatten_row(right, proof, operations)?;
        let mut equations = Vec::new();
        for (label, left_ty) in &left.fields {
            operations.charge(1, "solve.row.compare_label", &[proof], [])?;
            if let Some(right_ty) = right.fields.get(label) {
                equations.push((*left_ty, *right_ty, proof));
            }
        }
        let left_only = difference(&left.fields, &right.fields);
        let right_only = difference(&right.fields, &left.fields);
        match (left.tail, right.tail) {
            (None, None) => {
                if !left_only.is_empty() || !right_only.is_empty() {
                    return Err(operations.fail(
                        TypeErrorKind::ClosedRow {
                            missing: right_only.keys().cloned().collect(),
                            extra: left_only.keys().cloned().collect(),
                        },
                        &[proof],
                        "solve.row.closed",
                    ));
                }
            }
            (Some(left_tail), None) => {
                if !left_only.is_empty() {
                    return Err(operations.fail(
                        TypeErrorKind::ClosedRow {
                            missing: Vec::new(),
                            extra: left_only.keys().cloned().collect(),
                        },
                        &[proof],
                        "solve.row.closed_right",
                    ));
                }
                self.bind_row(
                    left_tail,
                    RowExpr {
                        kind: left.kind,
                        fields: right_only,
                        tail: None,
                    },
                    proof,
                    operations,
                )?;
            }
            (None, Some(right_tail)) => {
                if !right_only.is_empty() {
                    return Err(operations.fail(
                        TypeErrorKind::ClosedRow {
                            missing: Vec::new(),
                            extra: right_only.keys().cloned().collect(),
                        },
                        &[proof],
                        "solve.row.closed_left",
                    ));
                }
                self.bind_row(
                    right_tail,
                    RowExpr {
                        kind: right.kind,
                        fields: left_only,
                        tail: None,
                    },
                    proof,
                    operations,
                )?;
            }
            (Some(left_tail), Some(right_tail)) => {
                if left_tail == right_tail {
                    if !left_only.is_empty() || !right_only.is_empty() {
                        return Err(operations.fail(
                            TypeErrorKind::RowOccurs {
                                row: u32::try_from(left_tail).unwrap_or(u32::MAX),
                            },
                            &[proof],
                            "solve.row.shared_tail",
                        ));
                    }
                } else if left_only.is_empty() && right_only.is_empty() {
                    let (bind, target) = if left_tail > right_tail {
                        (left_tail, right_tail)
                    } else {
                        (right_tail, left_tail)
                    };
                    self.bind_row(
                        bind,
                        RowExpr {
                            kind: left.kind,
                            fields: BTreeMap::new(),
                            tail: Some(target),
                        },
                        proof,
                        operations,
                    )?;
                } else {
                    let level = self.row_levels[left_tail].min(self.row_levels[right_tail]);
                    let residual = self.fresh_row(left.kind, level, operations)?;
                    self.bind_row(
                        left_tail,
                        RowExpr {
                            kind: left.kind,
                            fields: right_only,
                            tail: Some(residual),
                        },
                        proof,
                        operations,
                    )?;
                    self.bind_row(
                        right_tail,
                        RowExpr {
                            kind: left.kind,
                            fields: left_only,
                            tail: Some(residual),
                        },
                        proof,
                        operations,
                    )?;
                }
            }
        }
        Ok(equations)
    }

    pub(super) fn flatten_row(
        &self,
        mut expr: RowExpr,
        proof: usize,
        operations: &mut OperationContext,
    ) -> Result<RowExpr, TypeError> {
        let mut path = BTreeSet::new();
        while let Some(tail) = expr.tail {
            operations.charge(1, "solve.row.flatten", &[proof], [])?;
            if !path.insert(tail) {
                return Err(operations.fail(
                    TypeErrorKind::RowOccurs {
                        row: u32::try_from(tail).unwrap_or(u32::MAX),
                    },
                    &[proof],
                    "solve.row.flatten",
                ));
            }
            if let Some(DraftRowNode::Extend {
                kind,
                fields,
                tail: next,
            }) = self.rows.get(tail)
            {
                if *kind != expr.kind {
                    return Err(operations.fail(
                        TypeErrorKind::RowKindMismatch,
                        &[proof],
                        "solve.row.flatten",
                    ));
                }
                for (label, ty) in fields {
                    if expr.fields.insert(label.clone(), *ty).is_some() {
                        return Err(operations.fail(
                            TypeErrorKind::DuplicateRowLabel {
                                label: label.clone(),
                            },
                            &[proof],
                            "solve.row.flatten",
                        ));
                    }
                }
                expr.tail = next.map(|row| row.slot as usize);
                continue;
            }
            let Some(binding) = self.row_subst.get(tail).and_then(Option::as_ref) else {
                break;
            };
            if binding.expr.kind != expr.kind {
                return Err(operations.fail(
                    TypeErrorKind::RowKindMismatch,
                    &[proof, binding.proof],
                    "solve.row.flatten",
                ));
            }
            for (label, ty) in &binding.expr.fields {
                if expr.fields.insert(label.clone(), *ty).is_some() {
                    return Err(operations.fail(
                        TypeErrorKind::DuplicateRowLabel {
                            label: label.clone(),
                        },
                        &[proof, binding.proof],
                        "solve.row.flatten",
                    ));
                }
            }
            expr.tail = binding.expr.tail;
        }
        Ok(expr)
    }

    fn bind_row(
        &mut self,
        row: usize,
        expr: RowExpr,
        proof: usize,
        operations: &mut OperationContext,
    ) -> Result<(), TypeError> {
        let expr = self.flatten_row(expr, proof, operations)?;
        if expr.fields.is_empty() && expr.tail == Some(row) {
            return Ok(());
        }
        let expected_kind = match self.rows.get(row) {
            Some(DraftRowNode::Meta { kind, .. }) => *kind,
            Some(DraftRowNode::Extend { .. }) => {
                return Err(operations.fail(
                    TypeErrorKind::InternalInvariant {
                        detail: "attempted to bind a concrete row".to_owned(),
                    },
                    &[proof],
                    "solve.row.bind",
                ));
            }
            None => {
                return Err(operations.fail(
                    TypeErrorKind::DanglingRow,
                    &[proof],
                    "solve.row.bind",
                ));
            }
        };
        if expected_kind != expr.kind {
            return Err(operations.fail(
                TypeErrorKind::RowKindMismatch,
                &[proof],
                "solve.row.bind",
            ));
        }
        if self.row_occurs(row, &expr, proof, operations)? {
            return Err(operations.fail(
                TypeErrorKind::RowOccurs {
                    row: u32::try_from(row).unwrap_or(u32::MAX),
                },
                &[proof],
                "solve.row.occurs",
            ));
        }
        let inherited = self.row_lacks[row].clone();
        for (label, lacks_proof) in &inherited {
            if expr.fields.contains_key(label) {
                return Err(operations.fail(
                    TypeErrorKind::RowLacks {
                        row: u32::try_from(row).unwrap_or(u32::MAX),
                        label: label.clone(),
                    },
                    &[proof, *lacks_proof],
                    "solve.row.lacks",
                ));
            }
        }
        let level = self.row_levels[row];
        for ty in expr.fields.values() {
            self.lower_levels_from_type(*ty, level, &[proof], operations)?;
        }
        if let Some(tail) = expr.tail {
            for (label, lacks_proof) in inherited {
                self.row_lacks[tail].entry(label).or_insert(lacks_proof);
            }
            for label in expr.fields.keys() {
                self.row_lacks[tail].entry(label.clone()).or_insert(proof);
            }
            self.row_levels[tail] = self.row_levels[tail].min(level);
        }
        let binding_proof = operations.charge(1, "solve.row.bind", &[proof], [])?;
        self.row_subst[row] = Some(RowBinding {
            expr,
            proof: binding_proof,
        });
        Ok(())
    }

    fn row_occurs(
        &self,
        target: usize,
        expr: &RowExpr,
        proof: usize,
        operations: &mut OperationContext,
    ) -> Result<bool, TypeError> {
        let mut types = expr.fields.values().copied().collect::<Vec<_>>();
        let mut rows = expr.tail.into_iter().collect::<Vec<_>>();
        let mut seen_types = BTreeSet::new();
        let mut seen_rows = BTreeSet::new();
        loop {
            while let Some(row) = rows.pop() {
                operations.charge(1, "solve.row.occurs.tail", &[proof], [])?;
                if row == target {
                    return Ok(true);
                }
                if !seen_rows.insert(row) {
                    continue;
                }
                let source = self.row_expr(row).ok_or_else(|| {
                    operations.fail(TypeErrorKind::DanglingRow, &[proof], "solve.row.occurs")
                })?;
                if let Some(binding) = &self.row_subst[row] {
                    types.extend(binding.expr.fields.values().copied());
                    rows.extend(binding.expr.tail);
                } else {
                    types.extend(source.fields.values().copied());
                    rows.extend(source.tail.filter(|tail| *tail != row));
                }
            }
            let Some(ty) = types.pop() else {
                break;
            };
            operations.charge(1, "solve.row.occurs.field", &[proof], [])?;
            if !seen_types.insert(ty) {
                continue;
            }
            match self.node(ty).ok_or_else(|| {
                operations.fail(TypeErrorKind::DanglingType, &[proof], "solve.row.occurs")
            })? {
                DraftNode::Meta { ordinal, .. } => {
                    if let Some(binding) = &self.type_subst[*ordinal as usize] {
                        types.push(binding.ty);
                    }
                }
                DraftNode::Tuple(items) => types.extend(items.iter().copied()),
                DraftNode::List(item) | DraftNode::Option(item) => types.push(*item),
                DraftNode::Function { parameters, result } => {
                    types.extend(parameters.iter().copied());
                    types.push(*result);
                }
                DraftNode::Record(row) | DraftNode::Variant(row) => {
                    rows.push(row.slot as usize);
                }
                DraftNode::Mu { body, .. } => types.push(*body),
                DraftNode::Rec(_) | DraftNode::Primitive(_) | DraftNode::Named(_) => {}
            }
        }
        Ok(false)
    }

    fn fresh_row(
        &mut self,
        kind: RowKind,
        level: u32,
        operations: &mut OperationContext,
    ) -> Result<usize, TypeError> {
        operations.charge(
            1,
            "solve.row.fresh_residual",
            &[],
            [Origin::boundary("solve.row")],
        )?;
        let slot = self.rows.len();
        self.rows.push(DraftRowNode::Meta { kind, level });
        self.row_subst.push(None);
        self.row_levels.push(level);
        self.row_lacks.push(BTreeMap::new());
        Ok(slot)
    }
}

fn difference(
    left: &BTreeMap<String, DraftTypeId>,
    right: &BTreeMap<String, DraftTypeId>,
) -> BTreeMap<String, DraftTypeId> {
    left.iter()
        .filter(|(label, _)| !right.contains_key(*label))
        .map(|(label, ty)| (label.clone(), *ty))
        .collect()
}
