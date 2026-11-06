//! Iterative type unification, occurs checks, and complete level lowering.

use std::collections::{BTreeMap, BTreeSet};

use super::{State, TypeBinding, meta_ordinal, rec_pair_matches};
use crate::type_kernel::store::{DraftNode, DraftRecVar, DraftTypeId};
use crate::type_kernel::{OperationContext, TypeError, TypeErrorKind};

#[derive(Clone)]
struct Equation {
    left: DraftTypeId,
    right: DraftTypeId,
    proof: usize,
    rec_pairs: BTreeMap<DraftRecVar, DraftRecVar>,
}

impl State {
    pub(super) fn unify_types(
        &mut self,
        left: DraftTypeId,
        right: DraftTypeId,
        proof: usize,
        operations: &mut OperationContext,
    ) -> Result<(), TypeError> {
        let mut pending = vec![Equation {
            left,
            right,
            proof,
            rec_pairs: BTreeMap::new(),
        }];
        while let Some(equation) = pending.pop() {
            let step = operations.charge(1, "solve.unify", &[equation.proof], [])?;
            let (left, mut parents) = self.prune_type(equation.left, step, operations)?;
            let (right, right_parents) = self.prune_type(equation.right, step, operations)?;
            parents.extend(right_parents);
            parents.push(step);
            parents.sort_unstable();
            parents.dedup();
            if left == right {
                continue;
            }
            let left_node = self.node(left).cloned().ok_or_else(|| {
                operations.fail(TypeErrorKind::DanglingType, &parents, "solve.unify")
            })?;
            let right_node = self.node(right).cloned().ok_or_else(|| {
                operations.fail(TypeErrorKind::DanglingType, &parents, "solve.unify")
            })?;
            if let Some(left_meta) = meta_ordinal(&left_node) {
                if let Some(right_meta) = meta_ordinal(&right_node) {
                    let (bind, target) = if left_meta > right_meta {
                        (left_meta, right)
                    } else {
                        (right_meta, left)
                    };
                    self.bind_meta(bind, target, &parents, operations)?;
                } else {
                    self.bind_meta(left_meta, right, &parents, operations)?;
                }
                continue;
            }
            if let Some(right_meta) = meta_ordinal(&right_node) {
                self.bind_meta(right_meta, left, &parents, operations)?;
                continue;
            }
            match (left_node, right_node) {
                (DraftNode::Primitive(left), DraftNode::Primitive(right)) if left == right => {}
                (DraftNode::Named(left), DraftNode::Named(right)) if left == right => {}
                (DraftNode::Rec(left), DraftNode::Rec(right))
                    if rec_pair_matches(left, right, &equation.rec_pairs) => {}
                (DraftNode::Tuple(left), DraftNode::Tuple(right)) if left.len() == right.len() => {
                    push_pairs(&mut pending, left, right, step, &equation.rec_pairs);
                }
                (DraftNode::List(left), DraftNode::List(right))
                | (DraftNode::Option(left), DraftNode::Option(right)) => {
                    pending.push(Equation {
                        left,
                        right,
                        proof: step,
                        rec_pairs: equation.rec_pairs,
                    });
                }
                (
                    DraftNode::Function {
                        parameters: left_parameters,
                        result: left_result,
                    },
                    DraftNode::Function {
                        parameters: right_parameters,
                        result: right_result,
                    },
                ) if left_parameters.len() == right_parameters.len() => {
                    pending.push(Equation {
                        left: left_result,
                        right: right_result,
                        proof: step,
                        rec_pairs: equation.rec_pairs.clone(),
                    });
                    push_pairs(
                        &mut pending,
                        left_parameters,
                        right_parameters,
                        step,
                        &equation.rec_pairs,
                    );
                }
                (DraftNode::Record(left), DraftNode::Record(right))
                | (DraftNode::Variant(left), DraftNode::Variant(right)) => {
                    let left = self.row_expr(left.slot as usize).ok_or_else(|| {
                        operations.fail(TypeErrorKind::DanglingRow, &[step], "solve.row")
                    })?;
                    let right = self.row_expr(right.slot as usize).ok_or_else(|| {
                        operations.fail(TypeErrorKind::DanglingRow, &[step], "solve.row")
                    })?;
                    let equations = self.unify_rows(left, right, step, operations)?;
                    for (left, right, proof) in equations.into_iter().rev() {
                        pending.push(Equation {
                            left,
                            right,
                            proof,
                            rec_pairs: equation.rec_pairs.clone(),
                        });
                    }
                }
                (
                    DraftNode::Mu {
                        binder: left_binder,
                        body: left_body,
                    },
                    DraftNode::Mu {
                        binder: right_binder,
                        body: right_body,
                    },
                ) => {
                    let mut rec_pairs = equation.rec_pairs;
                    rec_pairs.insert(left_binder, right_binder);
                    pending.push(Equation {
                        left: left_body,
                        right: right_body,
                        proof: step,
                        rec_pairs,
                    });
                }
                _ => {
                    return Err(operations.fail(
                        TypeErrorKind::Mismatch {
                            left: self.node_name(left),
                            right: self.node_name(right),
                        },
                        &parents,
                        "solve.unify",
                    ));
                }
            }
        }
        Ok(())
    }

    fn prune_type(
        &self,
        mut ty: DraftTypeId,
        proof: usize,
        operations: &mut OperationContext,
    ) -> Result<(DraftTypeId, Vec<usize>), TypeError> {
        let mut parents = Vec::new();
        let mut seen = BTreeSet::new();
        loop {
            operations.charge(1, "solve.prune_type", &[proof], [])?;
            if !seen.insert(ty) {
                return Err(operations.fail(
                    TypeErrorKind::InternalInvariant {
                        detail: "cyclic type substitution".to_owned(),
                    },
                    &parents,
                    "solve.prune_type",
                ));
            }
            let Some(ordinal) = self.node(ty).and_then(meta_ordinal) else {
                return Ok((ty, parents));
            };
            let Some(binding) = self
                .type_subst
                .get(ordinal as usize)
                .and_then(Option::as_ref)
            else {
                return Ok((ty, parents));
            };
            parents.push(binding.proof);
            ty = binding.ty;
        }
    }

    fn bind_meta(
        &mut self,
        meta: u32,
        ty: DraftTypeId,
        parents: &[usize],
        operations: &mut OperationContext,
    ) -> Result<(), TypeError> {
        if self.type_occurs(meta, ty, parents, operations)? {
            return Err(operations.fail(
                TypeErrorKind::TypeOccurs { meta },
                parents,
                "solve.type_occurs",
            ));
        }
        let level = self.type_levels[meta as usize];
        self.lower_levels_from_type(ty, level, parents, operations)?;
        let proof = operations.charge(1, "solve.bind_type", parents, [])?;
        self.type_subst[meta as usize] = Some(TypeBinding { ty, proof });
        Ok(())
    }

    fn type_occurs(
        &self,
        target: u32,
        root: DraftTypeId,
        parents: &[usize],
        operations: &mut OperationContext,
    ) -> Result<bool, TypeError> {
        let mut types = vec![root];
        let mut rows = Vec::new();
        let mut seen_types = BTreeSet::new();
        let mut seen_rows = BTreeSet::new();
        while let Some(ty) = types.pop() {
            operations.charge(1, "solve.type_occurs", parents, [])?;
            if !seen_types.insert(ty) {
                continue;
            }
            match self.node(ty).ok_or_else(|| {
                operations.fail(TypeErrorKind::DanglingType, parents, "solve.type_occurs")
            })? {
                DraftNode::Meta { ordinal, .. } if *ordinal == target => return Ok(true),
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
            self.drain_rows_for_type_walk(
                &mut rows,
                &mut types,
                &mut seen_rows,
                parents,
                operations,
            )?;
        }
        Ok(false)
    }

    pub(super) fn lower_levels_from_type(
        &mut self,
        root: DraftTypeId,
        level: u32,
        parents: &[usize],
        operations: &mut OperationContext,
    ) -> Result<(), TypeError> {
        let mut types = vec![root];
        let mut rows = Vec::new();
        let mut seen_types = BTreeSet::new();
        let mut seen_rows = BTreeSet::new();
        while let Some(ty) = types.pop() {
            operations.charge(1, "solve.lower.type", parents, [])?;
            if !seen_types.insert(ty) {
                continue;
            }
            match self.node(ty).cloned().ok_or_else(|| {
                operations.fail(TypeErrorKind::DanglingType, parents, "solve.lower")
            })? {
                DraftNode::Meta { ordinal, .. } => {
                    self.type_levels[ordinal as usize] =
                        self.type_levels[ordinal as usize].min(level);
                    if let Some(binding) = &self.type_subst[ordinal as usize] {
                        types.push(binding.ty);
                    }
                }
                DraftNode::Tuple(items) => types.extend(items),
                DraftNode::List(item) | DraftNode::Option(item) => types.push(item),
                DraftNode::Function { parameters, result } => {
                    types.extend(parameters);
                    types.push(result);
                }
                DraftNode::Record(row) | DraftNode::Variant(row) => {
                    rows.push(row.slot as usize);
                }
                DraftNode::Mu { body, .. } => types.push(body),
                DraftNode::Rec(_) | DraftNode::Primitive(_) | DraftNode::Named(_) => {}
            }
            while let Some(row) = rows.pop() {
                operations.charge(1, "solve.lower.row", parents, [])?;
                if !seen_rows.insert(row) {
                    continue;
                }
                self.row_levels[row] = self.row_levels[row].min(level);
                let expr = self.row_expr(row).ok_or_else(|| {
                    operations.fail(TypeErrorKind::DanglingRow, parents, "solve.lower")
                })?;
                types.extend(expr.fields.values().copied());
                if let Some(binding) = &self.row_subst[row] {
                    types.extend(binding.expr.fields.values().copied());
                    rows.extend(binding.expr.tail);
                } else {
                    rows.extend(expr.tail.filter(|tail| *tail != row));
                }
            }
        }
        Ok(())
    }

    fn drain_rows_for_type_walk(
        &self,
        rows: &mut Vec<usize>,
        types: &mut Vec<DraftTypeId>,
        seen_rows: &mut BTreeSet<usize>,
        parents: &[usize],
        operations: &mut OperationContext,
    ) -> Result<(), TypeError> {
        while let Some(row) = rows.pop() {
            operations.charge(1, "solve.type_occurs.row", parents, [])?;
            if !seen_rows.insert(row) {
                continue;
            }
            let expr = self.row_expr(row).ok_or_else(|| {
                operations.fail(TypeErrorKind::DanglingRow, parents, "solve.type_occurs")
            })?;
            types.extend(expr.fields.values().copied());
            if let Some(binding) = &self.row_subst[row] {
                types.extend(binding.expr.fields.values().copied());
                rows.extend(binding.expr.tail);
            } else {
                rows.extend(expr.tail.filter(|tail| *tail != row));
            }
        }
        Ok(())
    }
}

fn push_pairs(
    pending: &mut Vec<Equation>,
    left: Vec<DraftTypeId>,
    right: Vec<DraftTypeId>,
    proof: usize,
    rec_pairs: &BTreeMap<DraftRecVar, DraftRecVar>,
) {
    for (left, right) in left.into_iter().zip(right).rev() {
        pending.push(Equation {
            left,
            right,
            proof,
            rec_pairs: rec_pairs.clone(),
        });
    }
}
