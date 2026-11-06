//! Flexible row-tail authority, lacks propagation, and complete row occurs.

use std::collections::{BTreeMap, BTreeSet};

use super::{PendingConstraint, RowBinding, RowExpr, State};
use crate::type_kernel::store::{Node, TypeId};
use crate::type_kernel::{OperationContext, Origin, TypeError, TypeErrorKind};

impl State<'_> {
    pub(super) fn seed_row_prefix_lacks(
        &mut self,
        constraints: &[PendingConstraint],
        roots: &[TypeId],
        operations: &mut OperationContext,
    ) -> Result<(), TypeError> {
        let mut pending = roots.to_vec();
        for constraint in constraints {
            if let PendingConstraint::Equal { left, right, .. } = constraint {
                pending.push(*left);
                pending.push(*right);
            }
        }
        let mut seen = BTreeSet::new();
        while let Some(ty) = pending.pop() {
            let proof = operations.step(
                "solve.seed_row_lacks",
                &[],
                [Origin::new("solve.row_prefix", format!("type {}", ty.slot))],
            )?;
            if !seen.insert(ty) {
                continue;
            }
            match self.node(ty).cloned().ok_or_else(|| {
                operations.fail(TypeErrorKind::DanglingType, &[proof], "solve.row_prefix")
            })? {
                Node::Tuple(items) | Node::Join(items) => pending.extend(items),
                Node::List(item) | Node::Option(item) => pending.push(item),
                Node::Function { parameters, result } => {
                    pending.extend(parameters);
                    pending.push(result);
                }
                Node::Record(row) | Node::Variant(row) => {
                    pending.extend(row.fields.iter().map(|(_, ty)| *ty));
                    if let Some(tail) = row.tail {
                        for (label, _) in &row.fields {
                            self.add_lacks(tail.slot as usize, label, proof, operations)?;
                        }
                    }
                }
                Node::Mu { body, .. } => pending.push(body),
                Node::Meta(_) | Node::Rec(_) | Node::Primitive(_) | Node::Named(_) => {}
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
        operations.step("solve.row.add_lacks", &[proof], [])?;
        let flattened = self.flatten_row(RowExpr::empty(Some(row)), proof, operations)?;
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
                "solve.row_lacks",
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
    ) -> Result<Vec<(TypeId, TypeId, usize)>, TypeError> {
        let left = self.flatten_row(left, proof, operations)?;
        let right = self.flatten_row(right, proof, operations)?;
        let mut equations = Vec::new();
        for (label, left_ty) in &left.fields {
            if let Some(right_ty) = right.fields.get(label) {
                equations.push((*left_ty, *right_ty, proof));
            }
        }
        let left_only = left
            .fields
            .iter()
            .filter(|(label, _)| !right.fields.contains_key(*label))
            .map(|(label, ty)| (label.clone(), *ty))
            .collect::<BTreeMap<_, _>>();
        let right_only = right
            .fields
            .iter()
            .filter(|(label, _)| !left.fields.contains_key(*label))
            .map(|(label, ty)| (label.clone(), *ty))
            .collect::<BTreeMap<_, _>>();

        match (left.tail, right.tail) {
            (None, None) => {
                if !left_only.is_empty() || !right_only.is_empty() {
                    return Err(operations.fail(
                        TypeErrorKind::ClosedRow {
                            labels: left_only.keys().chain(right_only.keys()).cloned().collect(),
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
                            labels: left_only.keys().cloned().collect(),
                        },
                        &[proof],
                        "solve.row.closed_right",
                    ));
                }
                self.bind_row(
                    left_tail,
                    RowExpr {
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
                            labels: right_only.keys().cloned().collect(),
                        },
                        &[proof],
                        "solve.row.closed_left",
                    ));
                }
                self.bind_row(
                    right_tail,
                    RowExpr {
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
                            TypeErrorKind::ClosedRow {
                                labels: left_only
                                    .keys()
                                    .chain(right_only.keys())
                                    .cloned()
                                    .collect(),
                            },
                            &[proof],
                            "solve.row.shared_tail",
                        ));
                    }
                } else if left_only.is_empty() && right_only.is_empty() {
                    self.bind_row(
                        left_tail.max(right_tail),
                        RowExpr::empty(Some(left_tail.min(right_tail))),
                        proof,
                        operations,
                    )?;
                } else {
                    let level = self.row_levels[left_tail].min(self.row_levels[right_tail]);
                    let residual = self.fresh_row(level);
                    self.bind_row(
                        left_tail,
                        RowExpr {
                            fields: right_only,
                            tail: Some(residual),
                        },
                        proof,
                        operations,
                    )?;
                    self.bind_row(
                        right_tail,
                        RowExpr {
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

    pub(crate) fn flatten_row(
        &self,
        mut expr: RowExpr,
        proof: usize,
        operations: &mut OperationContext,
    ) -> Result<RowExpr, TypeError> {
        let mut path = BTreeSet::new();
        while let Some(tail) = expr.tail {
            operations.step("solve.row.prune", &[proof], [])?;
            if !path.insert(tail) {
                return Err(operations.fail(
                    TypeErrorKind::InternalCycle,
                    &[proof],
                    "solve.row.prune",
                ));
            }
            let Some(binding) = self.row_subst.get(tail).and_then(Option::as_ref) else {
                break;
            };
            for (label, ty) in &binding.expr.fields {
                if expr.fields.insert(label.clone(), *ty).is_some() {
                    return Err(operations.fail(
                        TypeErrorKind::DuplicateRowLabel {
                            label: label.clone(),
                        },
                        &[proof, binding.proof],
                        "solve.row.prune",
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
        if self.row_occurs(row, &expr, proof, operations)? {
            return Err(operations.fail(
                TypeErrorKind::RowOccurs {
                    row: u32::try_from(row).unwrap_or(u32::MAX),
                },
                &[proof],
                "solve.row_occurs",
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
                    "solve.row_lacks",
                ));
            }
        }
        if let Some(tail) = expr.tail {
            for (label, lacks_proof) in inherited {
                self.row_lacks[tail].entry(label).or_insert(lacks_proof);
            }
            for label in expr.fields.keys() {
                self.row_lacks[tail].entry(label.clone()).or_insert(proof);
            }
            self.row_levels[tail] = self.row_levels[tail].min(self.row_levels[row]);
        }
        let binding_proof = operations.step("solve.row.bind", &[proof], [])?;
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
        while let Some(row) = rows.pop() {
            operations.step("solve.row_occurs.tail", &[proof], [])?;
            if row == target {
                return Ok(true);
            }
            if !seen_rows.insert(row) {
                continue;
            }
            if let Some(binding) = self.row_subst.get(row).and_then(Option::as_ref) {
                types.extend(binding.expr.fields.values().copied());
                rows.extend(binding.expr.tail);
            }
        }
        while let Some(ty) = types.pop() {
            operations.step("solve.row_occurs.field", &[proof], [])?;
            if !seen_types.insert(ty) {
                continue;
            }
            match self.node(ty).ok_or_else(|| {
                operations.fail(TypeErrorKind::DanglingType, &[proof], "solve.row_occurs")
            })? {
                Node::Meta(meta) => {
                    if let Some(binding) = &self.type_subst[meta.slot as usize] {
                        types.push(binding.ty);
                    }
                }
                Node::Tuple(items) | Node::Join(items) => types.extend(items.iter().copied()),
                Node::List(item) | Node::Option(item) => types.push(*item),
                Node::Function { parameters, result } => {
                    types.extend(parameters.iter().copied());
                    types.push(*result);
                }
                Node::Record(row) | Node::Variant(row) => {
                    types.extend(row.fields.iter().map(|(_, ty)| *ty));
                    if let Some(row) = row.tail {
                        if row.slot as usize == target {
                            return Ok(true);
                        }
                        rows.push(row.slot as usize);
                    }
                }
                Node::Mu { body, .. } => types.push(*body),
                Node::Rec(_) | Node::Primitive(_) | Node::Named(_) => {}
            }
            while let Some(row) = rows.pop() {
                if row == target {
                    return Ok(true);
                }
                if seen_rows.insert(row)
                    && let Some(binding) = self.row_subst.get(row).and_then(Option::as_ref)
                {
                    types.extend(binding.expr.fields.values().copied());
                    rows.extend(binding.expr.tail);
                }
            }
        }
        Ok(false)
    }
}
