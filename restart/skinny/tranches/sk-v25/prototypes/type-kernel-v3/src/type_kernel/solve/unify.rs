//! Canonical iterative type unification and complete type-occurs checks.

use std::collections::{BTreeMap, BTreeSet};

use super::{State, TypeBinding, meta_of, rec_pair_matches};
use crate::type_kernel::store::{MetaId, Node, RecVar, TypeId};
use crate::type_kernel::{OperationContext, TypeError, TypeErrorKind};

#[derive(Clone)]
struct Equation {
    left: TypeId,
    right: TypeId,
    proof: usize,
    rec_pairs: BTreeMap<RecVar, RecVar>,
}

impl State<'_> {
    pub(super) fn unify_types(
        &mut self,
        left: TypeId,
        right: TypeId,
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
            let step = operations.step("solve.unify", &[equation.proof], [])?;
            let (left, mut parents) = self.prune_type(equation.left, operations, step)?;
            let (right, right_parents) = self.prune_type(equation.right, operations, step)?;
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

            if let Some(left_meta) = meta_of(&left_node) {
                if let Some(right_meta) = meta_of(&right_node) {
                    let (bind, target) = if left_meta.slot > right_meta.slot {
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
            if let Some(right_meta) = meta_of(&right_node) {
                self.bind_meta(right_meta, left, &parents, operations)?;
                continue;
            }

            match (left_node, right_node) {
                (Node::Primitive(left), Node::Primitive(right)) if left == right => {}
                (Node::Named(left), Node::Named(right)) if left == right => {}
                (Node::Rec(left), Node::Rec(right))
                    if rec_pair_matches(left, right, &equation.rec_pairs) => {}
                (Node::Tuple(left), Node::Tuple(right)) if left.len() == right.len() => {
                    for (left, right) in left.into_iter().zip(right).rev() {
                        pending.push(Equation {
                            left,
                            right,
                            proof: step,
                            rec_pairs: equation.rec_pairs.clone(),
                        });
                    }
                }
                (Node::Join(left), Node::Join(right)) if left.len() == right.len() => {
                    for (left, right) in left.into_iter().zip(right).rev() {
                        pending.push(Equation {
                            left,
                            right,
                            proof: step,
                            rec_pairs: equation.rec_pairs.clone(),
                        });
                    }
                }
                (Node::Join(items), _) => {
                    for item in items.into_iter().rev() {
                        pending.push(Equation {
                            left: item,
                            right,
                            proof: step,
                            rec_pairs: equation.rec_pairs.clone(),
                        });
                    }
                }
                (_, Node::Join(items)) => {
                    for item in items.into_iter().rev() {
                        pending.push(Equation {
                            left,
                            right: item,
                            proof: step,
                            rec_pairs: equation.rec_pairs.clone(),
                        });
                    }
                }
                (Node::List(left), Node::List(right))
                | (Node::Option(left), Node::Option(right)) => pending.push(Equation {
                    left,
                    right,
                    proof: step,
                    rec_pairs: equation.rec_pairs,
                }),
                (
                    Node::Function {
                        parameters: left_parameters,
                        result: left_result,
                    },
                    Node::Function {
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
                    for (left, right) in left_parameters.into_iter().zip(right_parameters).rev() {
                        pending.push(Equation {
                            left,
                            right,
                            proof: step,
                            rec_pairs: equation.rec_pairs.clone(),
                        });
                    }
                }
                (Node::Record(left), Node::Record(right))
                | (Node::Variant(left), Node::Variant(right)) => {
                    let equations = self.unify_rows(
                        super::RowExpr::from_spec(&left),
                        super::RowExpr::from_spec(&right),
                        step,
                        operations,
                    )?;
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
                    Node::Mu {
                        binder: left_binder,
                        body: left_body,
                    },
                    Node::Mu {
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
        mut ty: TypeId,
        operations: &mut OperationContext,
        proof: usize,
    ) -> Result<(TypeId, Vec<usize>), TypeError> {
        let mut parents = Vec::new();
        let mut seen = BTreeSet::new();
        loop {
            operations.step("solve.prune_type", &[proof], [])?;
            if !seen.insert(ty) {
                return Err(operations.fail(TypeErrorKind::InternalCycle, &parents, "solve.prune"));
            }
            let Some(Node::Meta(meta)) = self.node(ty) else {
                return Ok((ty, parents));
            };
            let Some(binding) = self
                .type_subst
                .get(meta.slot as usize)
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
        meta: MetaId,
        ty: TypeId,
        parents: &[usize],
        operations: &mut OperationContext,
    ) -> Result<(), TypeError> {
        let occurs = self.type_occurs(meta, ty, operations, parents)?;
        if occurs {
            return Err(operations.fail(
                TypeErrorKind::TypeOccurs { meta: meta.slot },
                parents,
                "solve.type_occurs",
            ));
        }
        let level = self.type_levels[meta.slot as usize];
        self.lower_levels(ty, level, operations, parents)?;
        let proof = operations.step("solve.bind_type", parents, [])?;
        self.type_subst[meta.slot as usize] = Some(TypeBinding { ty, proof });
        Ok(())
    }

    fn type_occurs(
        &self,
        target: MetaId,
        root: TypeId,
        operations: &mut OperationContext,
        parents: &[usize],
    ) -> Result<bool, TypeError> {
        let mut types = vec![root];
        let mut rows = Vec::new();
        let mut seen_types = BTreeSet::new();
        let mut seen_rows = BTreeSet::new();
        while let Some(ty) = types.pop() {
            operations.step("solve.type_occurs", parents, [])?;
            if !seen_types.insert(ty) {
                continue;
            }
            let node = self.node(ty).ok_or_else(|| {
                operations.fail(TypeErrorKind::DanglingType, parents, "solve.type_occurs")
            })?;
            match node {
                Node::Meta(meta) if *meta == target => return Ok(true),
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
                    rows.extend(row.tail.map(|row| row.slot as usize));
                }
                Node::Mu { body, .. } => types.push(*body),
                Node::Rec(_) | Node::Primitive(_) | Node::Named(_) => {}
            }
            while let Some(row) = rows.pop() {
                if !seen_rows.insert(row) {
                    continue;
                }
                if let Some(binding) = self.row_subst.get(row).and_then(Option::as_ref) {
                    types.extend(binding.expr.fields.values().copied());
                    rows.extend(binding.expr.tail);
                }
            }
        }
        Ok(false)
    }

    fn lower_levels(
        &mut self,
        root: TypeId,
        level: u32,
        operations: &mut OperationContext,
        parents: &[usize],
    ) -> Result<(), TypeError> {
        let mut types = vec![root];
        let mut rows = Vec::new();
        let mut seen_types = BTreeSet::new();
        let mut seen_rows = BTreeSet::new();
        while let Some(ty) = types.pop() {
            operations.step("solve.lower_level", parents, [])?;
            if !seen_types.insert(ty) {
                continue;
            }
            match self.node(ty).cloned().ok_or_else(|| {
                operations.fail(TypeErrorKind::DanglingType, parents, "solve.lower_level")
            })? {
                Node::Meta(meta) => {
                    let meta_level = &mut self.type_levels[meta.slot as usize];
                    *meta_level = (*meta_level).min(level);
                    if let Some(binding) = &self.type_subst[meta.slot as usize] {
                        types.push(binding.ty);
                    }
                }
                Node::Tuple(items) | Node::Join(items) => types.extend(items),
                Node::List(item) | Node::Option(item) => types.push(item),
                Node::Function { parameters, result } => {
                    types.extend(parameters);
                    types.push(result);
                }
                Node::Record(row) | Node::Variant(row) => {
                    types.extend(row.fields.into_iter().map(|(_, ty)| ty));
                    rows.extend(row.tail.map(|row| row.slot as usize));
                }
                Node::Mu { body, .. } => types.push(body),
                Node::Rec(_) | Node::Primitive(_) | Node::Named(_) => {}
            }
            while let Some(row) = rows.pop() {
                if !seen_rows.insert(row) {
                    continue;
                }
                self.row_levels[row] = self.row_levels[row].min(level);
                if let Some(binding) = &self.row_subst[row] {
                    types.extend(binding.expr.fields.values().copied());
                    rows.extend(binding.expr.tail);
                }
            }
        }
        Ok(())
    }
}
