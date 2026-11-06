use std::collections::{BTreeMap, BTreeSet};

use crate::{
    ErrorKind, Origin, Row, RowSide, RowTail, RowVarId, TypeError, TypeId, TypeStore, TypeTerm,
    TypeVarId,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Constraint {
    pub left: TypeId,
    pub right: TypeId,
    pub origin: Origin,
}

impl Constraint {
    pub fn equal(left: TypeId, right: TypeId, origin: Origin) -> Self {
        Self {
            left,
            right,
            origin,
        }
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Solution {
    pub type_substitutions: BTreeMap<TypeVarId, TypeId>,
    pub row_substitutions: BTreeMap<RowVarId, Row>,
}

impl Solution {
    pub fn solve(
        store: &mut TypeStore,
        mut constraints: Vec<Constraint>,
    ) -> Result<Self, TypeError> {
        constraints.sort_by_key(|constraint| constraint_key(store, constraint));
        let mut solution = Self::default();
        for constraint in constraints {
            solution.unify(
                store,
                constraint.left,
                constraint.right,
                &constraint.origin,
                &mut Vec::new(),
            )?;
        }
        solution.compress(store);
        Ok(solution)
    }

    pub fn materialize(&self, store: &mut TypeStore, root: TypeId) -> TypeId {
        self.copy_type(store, root, &mut BTreeMap::new(), &mut BTreeMap::new())
    }

    pub fn fingerprint(&self, store: &TypeStore, root: TypeId) -> String {
        self.fingerprint_type(store, root, &BTreeSet::new(), &mut BTreeSet::new())
    }

    fn unify(
        &mut self,
        store: &mut TypeStore,
        left: TypeId,
        right: TypeId,
        origin: &Origin,
        path: &mut Vec<String>,
    ) -> Result<(), TypeError> {
        let left = self.prune(store, left);
        let right = self.prune(store, right);
        if left == right {
            return Ok(());
        }
        let left_term = store.term(left).clone();
        let right_term = store.term(right).clone();
        match (left_term, right_term) {
            (TypeTerm::Var(variable), _) => {
                self.bind_type_variable(store, variable, left, right, origin, path)
            }
            (_, TypeTerm::Var(variable)) => {
                self.bind_type_variable(store, variable, right, left, origin, path)
            }
            (TypeTerm::Primitive(left), TypeTerm::Primitive(right)) if left == right => Ok(()),
            (TypeTerm::Tuple(left), TypeTerm::Tuple(right)) => {
                self.unify_many(store, "tuple", left, right, origin, path)
            }
            (TypeTerm::List(left), TypeTerm::List(right)) => {
                self.with_path("list element", path, |this, path| {
                    this.unify(store, left, right, origin, path)
                })
            }
            (TypeTerm::Option(left), TypeTerm::Option(right)) => {
                self.with_path("option element", path, |this, path| {
                    this.unify(store, left, right, origin, path)
                })
            }
            (
                TypeTerm::Function {
                    parameters: left_parameters,
                    result: left_result,
                },
                TypeTerm::Function {
                    parameters: right_parameters,
                    result: right_result,
                },
            ) => {
                self.unify_many(
                    store,
                    "function",
                    left_parameters,
                    right_parameters,
                    origin,
                    path,
                )?;
                self.with_path("function result", path, |this, path| {
                    this.unify(store, left_result, right_result, origin, path)
                })
            }
            (TypeTerm::Record(left), TypeTerm::Record(right)) => {
                self.unify_rows(store, left, right, origin, path)
            }
            (TypeTerm::Variant(left), TypeTerm::Variant(right)) => {
                self.unify_rows(store, left, right, origin, path)
            }
            (TypeTerm::Mu { binder: _, body: _ }, TypeTerm::Mu { binder: _, body: _ })
                if store.fingerprint(left) == store.fingerprint(right) =>
            {
                Ok(())
            }
            (TypeTerm::Mu { .. }, TypeTerm::Mu { .. }) => Err(TypeError::new(
                ErrorKind::RecursiveMismatch { left, right },
                origin,
                path,
            )),
            (left_term, right_term) => Err(TypeError::new(
                ErrorKind::ConstructorMismatch {
                    left: left_term.constructor(),
                    right: right_term.constructor(),
                },
                origin,
                path,
            )),
        }
    }

    fn unify_many(
        &mut self,
        store: &mut TypeStore,
        constructor: &'static str,
        left: Vec<TypeId>,
        right: Vec<TypeId>,
        origin: &Origin,
        path: &mut Vec<String>,
    ) -> Result<(), TypeError> {
        if left.len() != right.len() {
            return Err(TypeError::new(
                ErrorKind::ArityMismatch {
                    constructor,
                    left: left.len(),
                    right: right.len(),
                },
                origin,
                path,
            ));
        }
        for (index, (left, right)) in left.into_iter().zip(right).enumerate() {
            self.with_path(format!("{constructor} item {index}"), path, |this, path| {
                this.unify(store, left, right, origin, path)
            })?;
        }
        Ok(())
    }

    fn unify_rows(
        &mut self,
        store: &mut TypeStore,
        left: Row,
        right: Row,
        origin: &Origin,
        path: &mut Vec<String>,
    ) -> Result<(), TypeError> {
        let left = self.expand_row(left);
        let right = self.expand_row(right);
        for (label, left_type) in &left.fields {
            if let Some(right_type) = right.fields.get(label) {
                self.with_path(format!("field {label}"), path, |this, path| {
                    this.unify(store, *left_type, *right_type, origin, path)
                })?;
            }
        }
        let left_only = difference(&left.fields, &right.fields);
        let right_only = difference(&right.fields, &left.fields);

        match (left_only.is_empty(), right_only.is_empty()) {
            (true, true) => self.unify_row_tails(store, left.tail, right.tail, origin, path),
            (false, true) => match right.tail {
                RowTail::Closed => Err(TypeError::new(
                    ErrorKind::MissingFields {
                        closed: RowSide::Right,
                        labels: left_only.keys().cloned().collect(),
                    },
                    origin,
                    path,
                )),
                RowTail::Open(variable) => self.bind_row_variable(
                    store,
                    variable,
                    Row {
                        fields: left_only,
                        tail: left.tail,
                    },
                    origin,
                    path,
                ),
            },
            (true, false) => match left.tail {
                RowTail::Closed => Err(TypeError::new(
                    ErrorKind::MissingFields {
                        closed: RowSide::Left,
                        labels: right_only.keys().cloned().collect(),
                    },
                    origin,
                    path,
                )),
                RowTail::Open(variable) => self.bind_row_variable(
                    store,
                    variable,
                    Row {
                        fields: right_only,
                        tail: right.tail,
                    },
                    origin,
                    path,
                ),
            },
            (false, false) => match (left.tail, right.tail) {
                (RowTail::Open(left_tail), RowTail::Open(right_tail))
                    if left_tail != right_tail =>
                {
                    let shared = store.fresh_row_variable();
                    self.bind_row_variable(
                        store,
                        left_tail,
                        Row {
                            fields: right_only,
                            tail: RowTail::Open(shared),
                        },
                        origin,
                        path,
                    )?;
                    self.bind_row_variable(
                        store,
                        right_tail,
                        Row {
                            fields: left_only,
                            tail: RowTail::Open(shared),
                        },
                        origin,
                        path,
                    )
                }
                (RowTail::Closed, _) => Err(TypeError::new(
                    ErrorKind::MissingFields {
                        closed: RowSide::Left,
                        labels: right_only.keys().cloned().collect(),
                    },
                    origin,
                    path,
                )),
                (_, RowTail::Closed) => Err(TypeError::new(
                    ErrorKind::MissingFields {
                        closed: RowSide::Right,
                        labels: left_only.keys().cloned().collect(),
                    },
                    origin,
                    path,
                )),
                (RowTail::Open(variable), RowTail::Open(_)) => Err(TypeError::new(
                    ErrorKind::RowOccursCheck {
                        variable,
                        labels: left_only.keys().chain(right_only.keys()).cloned().collect(),
                    },
                    origin,
                    path,
                )),
            },
        }
    }

    fn unify_row_tails(
        &mut self,
        store: &TypeStore,
        left: RowTail,
        right: RowTail,
        origin: &Origin,
        path: &[String],
    ) -> Result<(), TypeError> {
        match (left, right) {
            (RowTail::Closed, RowTail::Closed) => Ok(()),
            (RowTail::Open(left), RowTail::Open(right)) if left == right => Ok(()),
            (RowTail::Open(left), RowTail::Open(right)) => {
                let (larger, smaller) = if left > right {
                    (left, right)
                } else {
                    (right, left)
                };
                self.bind_row_variable(
                    store,
                    larger,
                    Row::new(
                        std::iter::empty::<(String, TypeId)>(),
                        RowTail::Open(smaller),
                    ),
                    origin,
                    path,
                )
            }
            (RowTail::Open(variable), RowTail::Closed)
            | (RowTail::Closed, RowTail::Open(variable)) => self.bind_row_variable(
                store,
                variable,
                Row::closed(std::iter::empty::<(String, TypeId)>()),
                origin,
                path,
            ),
        }
    }

    fn bind_type_variable(
        &mut self,
        store: &TypeStore,
        variable: TypeVarId,
        variable_type: TypeId,
        target: TypeId,
        origin: &Origin,
        path: &[String],
    ) -> Result<(), TypeError> {
        let target = self.prune(store, target);
        if let TypeTerm::Var(other) = store.term(target) {
            if variable == *other {
                return Ok(());
            }
            if variable < *other {
                self.type_substitutions.insert(*other, variable_type);
            } else {
                self.type_substitutions.insert(variable, target);
            }
            return Ok(());
        }
        if self.type_occurs(
            store,
            variable,
            target,
            &mut BTreeSet::new(),
            &BTreeSet::new(),
        ) {
            return Err(TypeError::new(
                ErrorKind::OccursCheck {
                    variable,
                    within: target,
                },
                origin,
                path,
            ));
        }
        self.type_substitutions.insert(variable, target);
        Ok(())
    }

    fn bind_row_variable(
        &mut self,
        store: &TypeStore,
        variable: RowVarId,
        row: Row,
        origin: &Origin,
        path: &[String],
    ) -> Result<(), TypeError> {
        let row = self.expand_row(row);
        if row.fields.is_empty() && row.tail == RowTail::Open(variable) {
            return Ok(());
        }
        if self.row_occurs(store, variable, &row, &mut BTreeSet::new()) {
            return Err(TypeError::new(
                ErrorKind::RowOccursCheck {
                    variable,
                    labels: row.fields.keys().cloned().collect(),
                },
                origin,
                path,
            ));
        }
        self.row_substitutions.insert(variable, row);
        Ok(())
    }

    fn prune(&self, store: &TypeStore, mut id: TypeId) -> TypeId {
        let mut visited = BTreeSet::new();
        loop {
            if !visited.insert(id) {
                return id;
            }
            match store.term(id) {
                TypeTerm::Var(variable) => match self.type_substitutions.get(variable) {
                    Some(next) => id = *next,
                    None => return id,
                },
                _ => return id,
            }
        }
    }

    fn expand_row(&self, mut row: Row) -> Row {
        let mut visited = BTreeSet::new();
        loop {
            let RowTail::Open(variable) = row.tail else {
                return row;
            };
            if !visited.insert(variable) {
                return row;
            }
            let Some(extension) = self.row_substitutions.get(&variable) else {
                return row;
            };
            for (name, ty) in &extension.fields {
                row.fields.entry(name.clone()).or_insert(*ty);
            }
            row.tail = extension.tail;
        }
    }

    fn type_occurs(
        &self,
        store: &TypeStore,
        needle: TypeVarId,
        id: TypeId,
        visiting: &mut BTreeSet<TypeId>,
        bound: &BTreeSet<TypeVarId>,
    ) -> bool {
        let id = self.prune(store, id);
        if !visiting.insert(id) {
            return false;
        }
        let occurs = match store.term(id) {
            TypeTerm::Var(variable) => *variable == needle && !bound.contains(variable),
            TypeTerm::Primitive(_) => false,
            TypeTerm::Tuple(elements) => elements
                .iter()
                .any(|element| self.type_occurs(store, needle, *element, visiting, bound)),
            TypeTerm::List(element) | TypeTerm::Option(element) => {
                self.type_occurs(store, needle, *element, visiting, bound)
            }
            TypeTerm::Function { parameters, result } => {
                parameters
                    .iter()
                    .any(|parameter| self.type_occurs(store, needle, *parameter, visiting, bound))
                    || self.type_occurs(store, needle, *result, visiting, bound)
            }
            TypeTerm::Record(row) | TypeTerm::Variant(row) => row
                .fields
                .values()
                .any(|field| self.type_occurs(store, needle, *field, visiting, bound)),
            TypeTerm::Mu { binder, body } => {
                let mut nested = bound.clone();
                nested.insert(*binder);
                self.type_occurs(store, needle, *body, visiting, &nested)
            }
        };
        visiting.remove(&id);
        occurs
    }

    fn row_occurs(
        &self,
        store: &TypeStore,
        needle: RowVarId,
        row: &Row,
        visiting: &mut BTreeSet<RowVarId>,
    ) -> bool {
        if row.tail == RowTail::Open(needle) {
            return true;
        }
        if let RowTail::Open(tail) = row.tail {
            if visiting.insert(tail) {
                if let Some(extension) = self.row_substitutions.get(&tail) {
                    if self.row_occurs(store, needle, extension, visiting) {
                        return true;
                    }
                }
            }
        }
        row.fields
            .values()
            .any(|field| self.type_contains_row(store, needle, *field, &mut BTreeSet::new()))
    }

    fn type_contains_row(
        &self,
        store: &TypeStore,
        needle: RowVarId,
        id: TypeId,
        visiting: &mut BTreeSet<TypeId>,
    ) -> bool {
        let id = self.prune(store, id);
        if !visiting.insert(id) {
            return false;
        }
        match store.term(id) {
            TypeTerm::Var(_) | TypeTerm::Primitive(_) => false,
            TypeTerm::Tuple(elements) => elements
                .iter()
                .any(|element| self.type_contains_row(store, needle, *element, visiting)),
            TypeTerm::List(element) | TypeTerm::Option(element) => {
                self.type_contains_row(store, needle, *element, visiting)
            }
            TypeTerm::Function { parameters, result } => {
                parameters
                    .iter()
                    .any(|parameter| self.type_contains_row(store, needle, *parameter, visiting))
                    || self.type_contains_row(store, needle, *result, visiting)
            }
            TypeTerm::Record(row) | TypeTerm::Variant(row) => {
                row.tail == RowTail::Open(needle)
                    || row
                        .fields
                        .values()
                        .any(|field| self.type_contains_row(store, needle, *field, visiting))
            }
            TypeTerm::Mu { body, .. } => self.type_contains_row(store, needle, *body, visiting),
        }
    }

    fn with_path<T>(
        &mut self,
        segment: impl Into<String>,
        path: &mut Vec<String>,
        operation: impl FnOnce(&mut Self, &mut Vec<String>) -> Result<T, TypeError>,
    ) -> Result<T, TypeError> {
        path.push(segment.into());
        let result = operation(self, path);
        path.pop();
        result
    }

    fn compress(&mut self, store: &TypeStore) {
        let variables = self.type_substitutions.keys().copied().collect::<Vec<_>>();
        for variable in variables {
            let target = self.type_substitutions[&variable];
            let target = self.prune(store, target);
            self.type_substitutions.insert(variable, target);
        }
        let rows = self.row_substitutions.keys().copied().collect::<Vec<_>>();
        for variable in rows {
            let row = self.expand_row(self.row_substitutions[&variable].clone());
            self.row_substitutions.insert(variable, row);
        }
    }

    fn copy_type(
        &self,
        store: &mut TypeStore,
        id: TypeId,
        type_variables: &mut BTreeMap<TypeVarId, TypeId>,
        row_variables: &mut BTreeMap<RowVarId, RowVarId>,
    ) -> TypeId {
        let id = self.prune(store, id);
        match store.term(id).clone() {
            TypeTerm::Var(variable) => type_variables.get(&variable).copied().unwrap_or(id),
            TypeTerm::Primitive(primitive) => store.primitive(primitive),
            TypeTerm::Tuple(elements) => {
                let elements = elements
                    .into_iter()
                    .map(|element| self.copy_type(store, element, type_variables, row_variables))
                    .collect::<Vec<_>>();
                store.tuple(elements)
            }
            TypeTerm::List(element) => {
                let element = self.copy_type(store, element, type_variables, row_variables);
                store.list(element)
            }
            TypeTerm::Option(element) => {
                let element = self.copy_type(store, element, type_variables, row_variables);
                store.option(element)
            }
            TypeTerm::Function { parameters, result } => {
                let parameters = parameters
                    .into_iter()
                    .map(|parameter| {
                        self.copy_type(store, parameter, type_variables, row_variables)
                    })
                    .collect::<Vec<_>>();
                let result = self.copy_type(store, result, type_variables, row_variables);
                store.function(parameters, result)
            }
            TypeTerm::Record(row) => {
                let row = self.copy_row(store, row, type_variables, row_variables);
                store.record(row)
            }
            TypeTerm::Variant(row) => {
                let row = self.copy_row(store, row, type_variables, row_variables);
                store.variant(row)
            }
            TypeTerm::Mu { binder, body } => {
                let (fresh_binder, fresh_type) = store.fresh_type_variable();
                let previous = type_variables.insert(binder, fresh_type);
                let body = self.copy_type(store, body, type_variables, row_variables);
                match previous {
                    Some(previous) => {
                        type_variables.insert(binder, previous);
                    }
                    None => {
                        type_variables.remove(&binder);
                    }
                }
                store.mu(fresh_binder, body)
            }
        }
    }

    fn copy_row(
        &self,
        store: &mut TypeStore,
        row: Row,
        type_variables: &mut BTreeMap<TypeVarId, TypeId>,
        row_variables: &mut BTreeMap<RowVarId, RowVarId>,
    ) -> Row {
        let row = self.expand_row(row);
        let fields = row
            .fields
            .into_iter()
            .map(|(name, ty)| {
                (
                    name,
                    self.copy_type(store, ty, type_variables, row_variables),
                )
            })
            .collect::<BTreeMap<_, _>>();
        let tail = match row.tail {
            RowTail::Closed => RowTail::Closed,
            RowTail::Open(variable) => RowTail::Open(
                *row_variables
                    .entry(variable)
                    .or_insert_with(|| store.fresh_row_variable()),
            ),
        };
        Row { fields, tail }
    }

    fn fingerprint_type(
        &self,
        store: &TypeStore,
        id: TypeId,
        bound: &BTreeSet<TypeVarId>,
        visiting: &mut BTreeSet<TypeId>,
    ) -> String {
        let id = self.prune(store, id);
        if !visiting.insert(id) {
            return format!("#{}", id.0);
        }
        let value = match store.term(id) {
            TypeTerm::Var(variable) if bound.contains(variable) => format!("bound{}", variable.0),
            TypeTerm::Var(variable) => format!("t{}", variable.0),
            TypeTerm::Primitive(primitive) => format!("{primitive:?}"),
            TypeTerm::Tuple(elements) => format!(
                "({})",
                elements
                    .iter()
                    .map(|element| self.fingerprint_type(store, *element, bound, visiting))
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            TypeTerm::List(element) => format!(
                "List<{}>",
                self.fingerprint_type(store, *element, bound, visiting)
            ),
            TypeTerm::Option(element) => format!(
                "Option<{}>",
                self.fingerprint_type(store, *element, bound, visiting)
            ),
            TypeTerm::Function { parameters, result } => format!(
                "fn({})->{}",
                parameters
                    .iter()
                    .map(|parameter| self.fingerprint_type(store, *parameter, bound, visiting))
                    .collect::<Vec<_>>()
                    .join(","),
                self.fingerprint_type(store, *result, bound, visiting)
            ),
            TypeTerm::Record(row) => self.fingerprint_row(store, "record", row, bound, visiting),
            TypeTerm::Variant(row) => self.fingerprint_row(store, "variant", row, bound, visiting),
            TypeTerm::Mu { binder, body } => {
                let mut nested = bound.clone();
                nested.insert(*binder);
                format!(
                    "mu.{}",
                    self.fingerprint_type(store, *body, &nested, visiting)
                )
            }
        };
        visiting.remove(&id);
        value
    }

    fn fingerprint_row(
        &self,
        store: &TypeStore,
        constructor: &str,
        row: &Row,
        bound: &BTreeSet<TypeVarId>,
        visiting: &mut BTreeSet<TypeId>,
    ) -> String {
        let row = self.expand_row(row.clone());
        let fields = row
            .fields
            .iter()
            .map(|(name, ty)| {
                format!(
                    "{name}:{}",
                    self.fingerprint_type(store, *ty, bound, visiting)
                )
            })
            .collect::<Vec<_>>()
            .join(",");
        let tail = match row.tail {
            RowTail::Closed => "closed".to_owned(),
            RowTail::Open(variable) => format!("r{}", variable.0),
        };
        format!("{constructor}{{{fields}|{tail}}}")
    }
}

fn difference(
    left: &BTreeMap<String, TypeId>,
    right: &BTreeMap<String, TypeId>,
) -> BTreeMap<String, TypeId> {
    left.iter()
        .filter(|(name, _)| !right.contains_key(*name))
        .map(|(name, ty)| (name.clone(), *ty))
        .collect()
}

fn constraint_key(store: &TypeStore, constraint: &Constraint) -> (String, String, Origin) {
    let left = store.fingerprint(constraint.left);
    let right = store.fingerprint(constraint.right);
    let (first, second) = if left <= right {
        (left, right)
    } else {
        (right, left)
    };
    (first, second, constraint.origin.clone())
}
