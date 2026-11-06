use super::zonk::{zonk_internal_row, zonk_internal_type};
use super::*;

pub fn solve(
    store: &TypeStore,
    constraints: &[Constraint],
    budget: Budget,
) -> Result<Solution, SolveError> {
    let mut origins = constraints
        .iter()
        .map(|constraint| constraint.origin.clone())
        .collect::<Vec<_>>();
    origins.sort();
    origins.dedup();

    let mut ordered = constraints.to_vec();
    ordered.sort_by_key(|constraint| constraint_key(store, constraint));
    let mut solver = Solver {
        store,
        budget,
        steps: 0,
        types: BTreeMap::new(),
        rows: BTreeMap::new(),
        lacks: BTreeMap::new(),
        next_residual: 0,
        origins,
    };
    solver.seed_implicit_lacks();
    for constraint in &ordered {
        if let ConstraintKind::Lacks(row, label) = &constraint.kind {
            solver.validate_row(*row)?;
            solver.add_lacks(Tail::Open(*row), label.clone())?;
        }
    }
    for constraint in &ordered {
        if let ConstraintKind::Equal(left, right) = constraint.kind {
            solver.validate_root(left)?;
            solver.validate_root(right)?;
            let left = solver.load(left)?;
            let right = solver.load(right)?;
            solver.unify(left, right, &mut Vec::new(), 0)?;
        }
    }
    solver.finish()
}

struct Solver<'a> {
    store: &'a TypeStore,
    budget: Budget,
    steps: usize,
    types: BTreeMap<MetaId, Ty>,
    rows: BTreeMap<RowId, Row>,
    lacks: BTreeMap<Tail, BTreeSet<String>>,
    next_residual: u32,
    origins: Vec<Origin>,
}

impl Solver<'_> {
    fn validate_root(&self, root: TypeId) -> Result<(), SolveError> {
        if root.scope != self.store.scope {
            return Err(SolveError::CrossStore);
        }
        self.store
            .validate_closed(root)
            .map_err(|_| SolveError::DanglingType(root))
    }

    fn validate_row(&self, row: RowId) -> Result<(), SolveError> {
        if row.scope != self.store.scope || row.slot >= self.store.rows {
            return Err(SolveError::CrossStore);
        }
        Ok(())
    }

    fn load(&self, root: TypeId) -> Result<Ty, SolveError> {
        load(self.store, root, 0, self.budget.depth)
    }

    fn seed_implicit_lacks(&mut self) {
        for node in &self.store.nodes {
            if let Node::Record(row) | Node::Variant(row) = node {
                if let Some(tail) = row.tail {
                    let labels = self.lacks.entry(Tail::Open(tail)).or_default();
                    labels.extend(row.fields.iter().map(|field| field.0.clone()));
                }
            }
        }
    }

    fn tick(&mut self, depth: usize) -> Result<(), SolveError> {
        if depth > self.budget.depth {
            return Err(SolveError::Exhausted {
                kind: Exhaustion::Depth,
                limit: self.budget.depth,
            });
        }
        self.steps = self.steps.saturating_add(1);
        if self.steps > self.budget.steps {
            return Err(SolveError::Exhausted {
                kind: Exhaustion::Steps,
                limit: self.budget.steps,
            });
        }
        Ok(())
    }

    fn unify(
        &mut self,
        left: Ty,
        right: Ty,
        path: &mut Vec<String>,
        depth: usize,
    ) -> Result<(), SolveError> {
        self.tick(depth)?;
        let left = self.prune_type(left);
        let right = self.prune_type(right);
        if left == right {
            return Ok(());
        }
        match (left, right) {
            (Ty::Meta(left), Ty::Meta(right)) => {
                let (bind, keep) = if left > right {
                    (left, right)
                } else {
                    (right, left)
                };
                self.bind_type(bind, Ty::Meta(keep), depth)
            }
            (Ty::Meta(meta), ty) | (ty, Ty::Meta(meta)) => self.bind_type(meta, ty, depth),
            (Ty::Primitive(left), Ty::Primitive(right)) if left == right => Ok(()),
            (Ty::Bound(left), Ty::Bound(right)) if left == right => Ok(()),
            (Ty::Tuple(left), Ty::Tuple(right)) if left.len() == right.len() => {
                for (index, (left, right)) in left.into_iter().zip(right).enumerate() {
                    path.push(format!("tuple[{index}]"));
                    self.unify(left, right, path, depth + 1)?;
                    path.pop();
                }
                Ok(())
            }
            (Ty::List(left), Ty::List(right)) => {
                path.push("list".into());
                let result = self.unify(*left, *right, path, depth + 1);
                path.pop();
                result
            }
            (Ty::Option(left), Ty::Option(right)) => {
                path.push("option".into());
                let result = self.unify(*left, *right, path, depth + 1);
                path.pop();
                result
            }
            (
                Ty::Function {
                    parameters: left_parameters,
                    result: left_result,
                },
                Ty::Function {
                    parameters: right_parameters,
                    result: right_result,
                },
            ) if left_parameters.len() == right_parameters.len() => {
                for (index, (left, right)) in left_parameters
                    .into_iter()
                    .zip(right_parameters)
                    .enumerate()
                {
                    path.push(format!("parameter[{index}]"));
                    self.unify(left, right, path, depth + 1)?;
                    path.pop();
                }
                path.push("result".into());
                let result = self.unify(*left_result, *right_result, path, depth + 1);
                path.pop();
                result
            }
            (Ty::Record(left), Ty::Record(right)) | (Ty::Variant(left), Ty::Variant(right)) => {
                self.unify_rows(left, right, path, depth + 1)
            }
            (Ty::Mu(left), Ty::Mu(right)) => self.unify(*left, *right, path, depth + 1),
            (left, right) => Err(self.mismatch(left, right, path)),
        }
    }

    fn unify_rows(
        &mut self,
        left: Row,
        right: Row,
        path: &mut Vec<String>,
        depth: usize,
    ) -> Result<(), SolveError> {
        self.tick(depth)?;
        let left = self.prune_row(left, &mut BTreeSet::new());
        let right = self.prune_row(right, &mut BTreeSet::new());
        for (label, left_ty) in &left.fields {
            if let Some(right_ty) = right.fields.get(label) {
                path.push(format!("field.{label}"));
                self.unify(left_ty.clone(), right_ty.clone(), path, depth + 1)?;
                path.pop();
            }
        }
        let left_only = row_difference(&left.fields, &right.fields);
        let right_only = row_difference(&right.fields, &left.fields);
        if left_only.is_empty() && right_only.is_empty() {
            return self.unify_tails(left.tail, right.tail, depth + 1);
        }
        match (left.tail.clone(), right.tail.clone()) {
            (Tail::Closed, _) if !right_only.is_empty() => {
                Err(self.row_mismatch(&left, &right, path))
            }
            (_, Tail::Closed) if !left_only.is_empty() => {
                Err(self.row_mismatch(&left, &right, path))
            }
            (Tail::Open(left_tail), Tail::Open(right_tail)) if left_tail == right_tail => {
                Err(SolveError::RowOccurs {
                    row: left_tail,
                    within: format!(
                        "{}+{}",
                        labels(&left_only).join(","),
                        labels(&right_only).join(",")
                    ),
                    origins: self.origins.clone(),
                })
            }
            (Tail::Open(left_tail), Tail::Open(right_tail)) => {
                let residual = Tail::Residual(self.next_residual);
                self.next_residual = self.next_residual.saturating_add(1);
                self.bind_row(
                    left_tail,
                    Row {
                        fields: right_only,
                        tail: residual.clone(),
                    },
                    depth + 1,
                )?;
                self.bind_row(
                    right_tail,
                    Row {
                        fields: left_only,
                        tail: residual,
                    },
                    depth + 1,
                )
            }
            (Tail::Open(row), tail) => self.bind_row(
                row,
                Row {
                    fields: right_only,
                    tail,
                },
                depth + 1,
            ),
            (tail, Tail::Open(row)) => self.bind_row(
                row,
                Row {
                    fields: left_only,
                    tail,
                },
                depth + 1,
            ),
            _ => Err(self.row_mismatch(&left, &right, path)),
        }
    }

    fn unify_tails(&mut self, left: Tail, right: Tail, depth: usize) -> Result<(), SolveError> {
        if left == right {
            return Ok(());
        }
        let left_debug = format!("{left:?}");
        let right_debug = format!("{right:?}");
        match (left, right) {
            (Tail::Open(left), Tail::Open(right)) => {
                let (bind, keep) = if left > right {
                    (left, right)
                } else {
                    (right, left)
                };
                self.bind_row(
                    bind,
                    Row {
                        fields: BTreeMap::new(),
                        tail: Tail::Open(keep),
                    },
                    depth,
                )
            }
            (Tail::Open(row), tail) | (tail, Tail::Open(row)) => self.bind_row(
                row,
                Row {
                    fields: BTreeMap::new(),
                    tail,
                },
                depth,
            ),
            (Tail::Residual(left), Tail::Residual(right)) if left == right => Ok(()),
            _ => Err(SolveError::Mismatch {
                left: left_debug,
                right: right_debug,
                path: vec!["row-tail".into()],
                origins: self.origins.clone(),
            }),
        }
    }

    fn bind_type(&mut self, meta: MetaId, ty: Ty, depth: usize) -> Result<(), SolveError> {
        self.tick(depth)?;
        let ty = self.prune_type(ty);
        if ty == Ty::Meta(meta) {
            return Ok(());
        }
        if self.occurs_type(meta, &ty, &mut BTreeSet::new(), depth + 1)? {
            return Err(SolveError::Occurs {
                meta,
                within: render_ty(&ty),
                origins: self.origins.clone(),
            });
        }
        self.types.insert(meta, ty);
        Ok(())
    }

    fn bind_row(&mut self, row: RowId, value: Row, depth: usize) -> Result<(), SolveError> {
        self.tick(depth)?;
        let value = self.prune_row(value, &mut BTreeSet::new());
        if value.fields.is_empty() && value.tail == Tail::Open(row) {
            return Ok(());
        }
        if self.occurs_row(row, &value, &mut BTreeSet::new(), depth + 1)? {
            return Err(SolveError::RowOccurs {
                row,
                within: render_row_internal(&value),
                origins: self.origins.clone(),
            });
        }
        let required_lacks = self
            .lacks
            .get(&Tail::Open(row))
            .cloned()
            .unwrap_or_default();
        for label in &required_lacks {
            if value.fields.contains_key(label) {
                return Err(SolveError::Lacks {
                    row,
                    label: label.clone(),
                    origins: self.origins.clone(),
                });
            }
        }
        for label in value.fields.keys() {
            self.add_lacks(value.tail.clone(), label.clone())?;
        }
        for label in required_lacks {
            self.add_lacks(value.tail.clone(), label)?;
        }
        self.rows.insert(row, value);
        Ok(())
    }

    fn add_lacks(&mut self, tail: Tail, label: String) -> Result<(), SolveError> {
        if let Tail::Closed = tail {
            return Ok(());
        }
        if let Tail::Open(row) = tail.clone() {
            if let Some(value) = self.rows.get(&row).cloned() {
                if value.fields.contains_key(&label) {
                    return Err(SolveError::Lacks {
                        row,
                        label,
                        origins: self.origins.clone(),
                    });
                }
                return self.add_lacks(value.tail, label);
            }
        }
        self.lacks.entry(tail).or_default().insert(label);
        Ok(())
    }

    fn prune_type(&self, ty: Ty) -> Ty {
        match ty {
            Ty::Meta(meta) => self
                .types
                .get(&meta)
                .cloned()
                .map_or(Ty::Meta(meta), |bound| self.prune_type(bound)),
            other => other,
        }
    }

    fn prune_row(&self, mut row: Row, visiting: &mut BTreeSet<RowId>) -> Row {
        while let Tail::Open(id) = row.tail {
            if !visiting.insert(id) {
                break;
            }
            let Some(bound) = self.rows.get(&id).cloned() else {
                break;
            };
            for (label, ty) in bound.fields {
                row.fields.entry(label).or_insert(ty);
            }
            row.tail = bound.tail;
        }
        row
    }

    fn occurs_type(
        &mut self,
        needle: MetaId,
        ty: &Ty,
        visiting_rows: &mut BTreeSet<RowId>,
        depth: usize,
    ) -> Result<bool, SolveError> {
        self.tick(depth)?;
        let ty = self.prune_type(ty.clone());
        Ok(match &ty {
            Ty::Meta(meta) => *meta == needle,
            Ty::Bound(_) | Ty::Primitive(_) => false,
            Ty::Tuple(items) => {
                for item in items {
                    if self.occurs_type(needle, item, visiting_rows, depth + 1)? {
                        return Ok(true);
                    }
                }
                false
            }
            Ty::List(item) | Ty::Option(item) | Ty::Mu(item) => {
                self.occurs_type(needle, item, visiting_rows, depth + 1)?
            }
            Ty::Function { parameters, result } => {
                for parameter in parameters {
                    if self.occurs_type(needle, parameter, visiting_rows, depth + 1)? {
                        return Ok(true);
                    }
                }
                self.occurs_type(needle, result, visiting_rows, depth + 1)?
            }
            Ty::Record(row) | Ty::Variant(row) => {
                for field in row.fields.values() {
                    if self.occurs_type(needle, field, visiting_rows, depth + 1)? {
                        return Ok(true);
                    }
                }
                if let Tail::Open(row) = row.tail {
                    if visiting_rows.insert(row) {
                        if let Some(bound) = self.rows.get(&row).cloned() {
                            if self.occurs_type_in_row(needle, &bound, visiting_rows, depth + 1)? {
                                return Ok(true);
                            }
                        }
                    }
                }
                false
            }
        })
    }

    fn occurs_type_in_row(
        &mut self,
        needle: MetaId,
        row: &Row,
        visiting: &mut BTreeSet<RowId>,
        depth: usize,
    ) -> Result<bool, SolveError> {
        for field in row.fields.values() {
            if self.occurs_type(needle, field, visiting, depth + 1)? {
                return Ok(true);
            }
        }
        if let Tail::Open(tail) = row.tail {
            if visiting.insert(tail) {
                if let Some(bound) = self.rows.get(&tail).cloned() {
                    return self.occurs_type_in_row(needle, &bound, visiting, depth + 1);
                }
            }
        }
        Ok(false)
    }

    fn occurs_row(
        &mut self,
        needle: RowId,
        row: &Row,
        visiting: &mut BTreeSet<RowId>,
        depth: usize,
    ) -> Result<bool, SolveError> {
        self.tick(depth)?;
        match row.tail {
            Tail::Open(row) if row == needle => Ok(true),
            Tail::Open(row) if visiting.insert(row) => {
                self.rows.get(&row).cloned().map_or(Ok(false), |bound| {
                    self.occurs_row(needle, &bound, visiting, depth + 1)
                })
            }
            _ => Ok(false),
        }
    }

    fn mismatch(&self, left: Ty, right: Ty, path: &[String]) -> SolveError {
        let (left, right) = ordered_pair(render_ty(&left), render_ty(&right));
        SolveError::Mismatch {
            left,
            right,
            path: path.to_vec(),
            origins: self.origins.clone(),
        }
    }

    fn row_mismatch(&self, left: &Row, right: &Row, path: &[String]) -> SolveError {
        let (left, right) = ordered_pair(render_row_internal(left), render_row_internal(right));
        SolveError::Mismatch {
            left,
            right,
            path: path.to_vec(),
            origins: self.origins.clone(),
        }
    }

    fn finish(self) -> Result<Solution, SolveError> {
        let mut type_bindings = BTreeMap::new();
        for (meta, ty) in &self.types {
            type_bindings.insert(
                *meta,
                zonk_internal_type(
                    ty.clone(),
                    &self.types,
                    &self.rows,
                    &mut BTreeSet::new(),
                    &mut BTreeSet::new(),
                ),
            );
        }
        let mut row_bindings = BTreeMap::new();
        for (row, value) in &self.rows {
            row_bindings.insert(
                *row,
                zonk_internal_row(
                    value.clone(),
                    &self.types,
                    &self.rows,
                    &mut BTreeSet::new(),
                    &mut BTreeSet::new(),
                ),
            );
        }
        let lacks = self
            .lacks
            .into_iter()
            .map(|(tail, labels)| (public_tail(tail), labels))
            .collect();
        Ok(Solution {
            scope: self.store.scope,
            type_bindings,
            row_bindings,
            lacks,
            origins: self.origins,
        })
    }
}

pub(super) fn load(
    store: &TypeStore,
    root: TypeId,
    depth: usize,
    limit: usize,
) -> Result<Ty, SolveError> {
    if depth > limit {
        return Err(SolveError::Exhausted {
            kind: Exhaustion::Depth,
            limit,
        });
    }
    store
        .validate(root)
        .map_err(|_| SolveError::DanglingType(root))?;
    Ok(match store.node(root) {
        Node::Meta(meta) => Ty::Meta(*meta),
        Node::Bound(index) => Ty::Bound(*index),
        Node::Primitive(primitive) => Ty::Primitive(*primitive),
        Node::Tuple(items) => Ty::Tuple(
            items
                .iter()
                .map(|item| load(store, *item, depth + 1, limit))
                .collect::<Result<_, _>>()?,
        ),
        Node::List(item) => Ty::List(Box::new(load(store, *item, depth + 1, limit)?)),
        Node::Option(item) => Ty::Option(Box::new(load(store, *item, depth + 1, limit)?)),
        Node::Function { parameters, result } => Ty::Function {
            parameters: parameters
                .iter()
                .map(|item| load(store, *item, depth + 1, limit))
                .collect::<Result<_, _>>()?,
            result: Box::new(load(store, *result, depth + 1, limit)?),
        },
        Node::Record(row) => Ty::Record(load_row(store, row, depth, limit)?),
        Node::Variant(row) => Ty::Variant(load_row(store, row, depth, limit)?),
        Node::Mu(body) => Ty::Mu(Box::new(load(store, *body, depth + 1, limit)?)),
    })
}

fn load_row(
    store: &TypeStore,
    row: &crate::RowSpec,
    depth: usize,
    limit: usize,
) -> Result<Row, SolveError> {
    Ok(Row {
        fields: row
            .fields
            .iter()
            .map(|(label, ty)| Ok((label.clone(), load(store, *ty, depth + 1, limit)?)))
            .collect::<Result<_, SolveError>>()?,
        tail: row.tail.map_or(Tail::Closed, Tail::Open),
    })
}

fn constraint_key(store: &TypeStore, constraint: &Constraint) -> String {
    match &constraint.kind {
        ConstraintKind::Equal(left, right) => {
            let left = store
                .validate(*left)
                .map_or_else(|_| format!("invalid:{left:?}"), |_| store.render(*left));
            let right = store
                .validate(*right)
                .map_or_else(|_| format!("invalid:{right:?}"), |_| store.render(*right));
            let (left, right) = ordered_pair(left, right);
            format!("1:{left}={right}")
        }
        ConstraintKind::Lacks(row, label) => format!("0:{row:?}:{label}"),
    }
}

fn row_difference(
    left: &BTreeMap<String, Ty>,
    right: &BTreeMap<String, Ty>,
) -> BTreeMap<String, Ty> {
    left.iter()
        .filter(|(label, _)| !right.contains_key(*label))
        .map(|(label, ty)| (label.clone(), ty.clone()))
        .collect()
}

fn labels(row: &BTreeMap<String, Ty>) -> Vec<String> {
    row.keys().cloned().collect()
}

fn ordered_pair(left: String, right: String) -> (String, String) {
    if left <= right {
        (left, right)
    } else {
        (right, left)
    }
}

fn render_ty(ty: &Ty) -> String {
    shallow_public(ty.clone()).render()
}

fn render_row_internal(row: &Row) -> String {
    render_row("row", &shallow_public_row(row.clone()))
}

fn shallow_public(ty: Ty) -> SolvedType {
    match ty {
        Ty::Meta(meta) => SolvedType::Meta(meta),
        Ty::Bound(index) => SolvedType::Bound(index),
        Ty::Primitive(primitive) => SolvedType::Primitive(primitive),
        Ty::Tuple(items) => SolvedType::Tuple(items.into_iter().map(shallow_public).collect()),
        Ty::List(item) => SolvedType::List(Box::new(shallow_public(*item))),
        Ty::Option(item) => SolvedType::Option(Box::new(shallow_public(*item))),
        Ty::Function { parameters, result } => SolvedType::Function {
            parameters: parameters.into_iter().map(shallow_public).collect(),
            result: Box::new(shallow_public(*result)),
        },
        Ty::Record(row) => SolvedType::Record(shallow_public_row(row)),
        Ty::Variant(row) => SolvedType::Variant(shallow_public_row(row)),
        Ty::Mu(body) => SolvedType::Mu(Box::new(shallow_public(*body))),
    }
}

fn shallow_public_row(row: Row) -> SolvedRow {
    SolvedRow {
        fields: row
            .fields
            .into_iter()
            .map(|(label, ty)| (label, shallow_public(ty)))
            .collect(),
        tail: public_tail(row.tail),
    }
}

pub(super) fn public_tail(tail: Tail) -> SolvedTail {
    match tail {
        Tail::Closed => SolvedTail::Closed,
        Tail::Open(row) => SolvedTail::Open(row),
        Tail::Residual(row) => SolvedTail::Residual(row),
    }
}
