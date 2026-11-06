//! Path-local reification with completed row memoization.

use std::collections::{BTreeMap, BTreeSet};

use super::{FreeRowVar, FreeTypeVar, RowExpr, SolvedRow, SolvedType, State};
use crate::type_kernel::store::{Node, RecVar, TypeId};
use crate::type_kernel::{OperationContext, Origin, TypeError, TypeErrorKind};

type ReifiedRoots = (
    BTreeMap<TypeId, SolvedType>,
    BTreeMap<FreeRowVar, BTreeSet<String>>,
);

struct Zonker<'state, 'store, 'operations> {
    state: &'state State<'store>,
    operations: &'operations mut OperationContext,
    visiting_rows: BTreeSet<usize>,
    row_memo: BTreeMap<usize, SolvedRow>,
    public_rows: BTreeMap<usize, FreeRowVar>,
    next_public_row: u32,
    binders: BTreeMap<RecVar, u32>,
    next_binder: u32,
    lacks: BTreeMap<FreeRowVar, BTreeSet<String>>,
}

impl State<'_> {
    pub(super) fn reify_roots(
        &self,
        roots: &[TypeId],
        operations: &mut OperationContext,
    ) -> Result<ReifiedRoots, TypeError> {
        let mut zonker = Zonker {
            state: self,
            operations,
            visiting_rows: BTreeSet::new(),
            row_memo: BTreeMap::new(),
            public_rows: BTreeMap::new(),
            next_public_row: 0,
            binders: BTreeMap::new(),
            next_binder: 0,
            lacks: BTreeMap::new(),
        };
        let mut reified = BTreeMap::new();
        let mut roots = roots.to_vec();
        roots.sort();
        roots.dedup();
        for root in roots {
            let ty = zonker.ty(root)?;
            reified.insert(root, ty);
        }
        Ok((reified, zonker.lacks))
    }
}

impl Zonker<'_, '_, '_> {
    fn ty(&mut self, mut id: TypeId) -> Result<SolvedType, TypeError> {
        let proof = self.operations.step(
            "solve.zonk.type",
            &[],
            [Origin::new("solve.zonk", format!("type {}", id.slot))],
        )?;
        let mut seen = BTreeSet::new();
        loop {
            if !seen.insert(id) {
                return Err(self.operations.fail(
                    TypeErrorKind::InternalCycle,
                    &[proof],
                    "solve.zonk.type",
                ));
            }
            let node = self.state.node(id).cloned().ok_or_else(|| {
                self.operations
                    .fail(TypeErrorKind::DanglingType, &[proof], "solve.zonk.type")
            })?;
            match node {
                Node::Meta(meta) => {
                    if let Some(binding) = &self.state.type_subst[meta.slot as usize] {
                        id = binding.ty;
                    } else {
                        return Ok(SolvedType::Free(FreeTypeVar {
                            id: meta.slot,
                            level: self.state.type_levels[meta.slot as usize],
                        }));
                    }
                }
                Node::Rec(rec) => {
                    let binder = self.binders.get(&rec).copied().ok_or_else(|| {
                        self.operations.fail(
                            TypeErrorKind::InternalCycle,
                            &[proof],
                            "solve.zonk.free_rec",
                        )
                    })?;
                    return Ok(SolvedType::Rec(binder));
                }
                Node::Primitive(primitive) => return Ok(SolvedType::Primitive(primitive)),
                Node::Named(name) => return Ok(SolvedType::Named(name)),
                Node::Tuple(items) => {
                    let mut solved = Vec::with_capacity(items.len());
                    for item in items {
                        solved.push(self.ty(item)?);
                    }
                    return Ok(SolvedType::Tuple(solved));
                }
                Node::Join(items) => {
                    let mut solved = Vec::with_capacity(items.len());
                    for item in items {
                        solved.push(self.ty(item)?);
                    }
                    if let Some(first) = solved.first().cloned()
                        && solved.iter().all(|item| item == &first)
                    {
                        return Ok(first);
                    }
                    let fields = solved
                        .into_iter()
                        .enumerate()
                        .map(|(index, ty)| (format!("branch_{index:04}"), ty))
                        .collect();
                    return Ok(SolvedType::Variant(SolvedRow { fields, tail: None }));
                }
                Node::List(item) => return Ok(SolvedType::List(Box::new(self.ty(item)?))),
                Node::Option(item) => return Ok(SolvedType::Option(Box::new(self.ty(item)?))),
                Node::Function { parameters, result } => {
                    let mut solved = Vec::with_capacity(parameters.len());
                    for parameter in parameters {
                        solved.push(self.ty(parameter)?);
                    }
                    return Ok(SolvedType::Function {
                        parameters: solved,
                        result: Box::new(self.ty(result)?),
                    });
                }
                Node::Record(row) => {
                    return Ok(SolvedType::Record(
                        self.row(RowExpr::from_spec(&row), proof)?,
                    ));
                }
                Node::Variant(row) => {
                    return Ok(SolvedType::Variant(
                        self.row(RowExpr::from_spec(&row), proof)?,
                    ));
                }
                Node::Mu { binder, body } => {
                    let canonical = self.next_binder;
                    self.next_binder = self.next_binder.saturating_add(1);
                    let previous = self.binders.insert(binder, canonical);
                    let solved = self.ty(body)?;
                    if let Some(previous) = previous {
                        self.binders.insert(binder, previous);
                    } else {
                        self.binders.remove(&binder);
                    }
                    return Ok(SolvedType::Mu(Box::new(solved)));
                }
            }
        }
    }

    fn row(&mut self, expr: RowExpr, proof: usize) -> Result<SolvedRow, TypeError> {
        self.operations.step("solve.zonk.row", &[proof], [])?;
        let flattened = self.state.flatten_row(expr, proof, self.operations)?;
        let mut fields = BTreeMap::new();
        for (label, ty) in flattened.fields {
            fields.insert(label, self.ty(ty)?);
        }
        let tail = if let Some(row) = flattened.tail {
            if let Some(memo) = self.row_memo.get(&row).cloned() {
                for (label, ty) in memo.fields {
                    if fields.insert(label.clone(), ty).is_some() {
                        return Err(self.operations.fail(
                            TypeErrorKind::DuplicateRowLabel { label },
                            &[proof],
                            "solve.zonk.row",
                        ));
                    }
                }
                memo.tail
            } else {
                if !self.visiting_rows.insert(row) {
                    return Err(self.operations.fail(
                        TypeErrorKind::InternalCycle,
                        &[proof],
                        "solve.zonk.row_cycle",
                    ));
                }
                let free = if let Some(free) = self.public_rows.get(&row) {
                    *free
                } else {
                    let free = FreeRowVar {
                        id: self.next_public_row,
                        level: self.state.row_levels[row],
                    };
                    self.next_public_row = self.next_public_row.saturating_add(1);
                    self.public_rows.insert(row, free);
                    free
                };
                let lacks = self.state.row_lacks[row].keys().cloned().collect();
                self.lacks.insert(free, lacks);
                let completed = SolvedRow {
                    fields: BTreeMap::new(),
                    tail: Some(free),
                };
                self.visiting_rows.remove(&row);
                self.row_memo.insert(row, completed.clone());
                completed.tail
            }
        } else {
            None
        };
        Ok(SolvedRow { fields, tail })
    }
}
