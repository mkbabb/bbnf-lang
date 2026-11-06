use std::collections::{BTreeMap, BTreeSet};

use crate::{Row, RowTail, RowVarId, TypeId, TypeStore, TypeTerm, TypeVarId};

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct FreeVariables {
    pub types: BTreeSet<TypeVarId>,
    pub rows: BTreeSet<RowVarId>,
}

impl FreeVariables {
    fn subtract(&mut self, other: &Self) {
        self.types
            .retain(|variable| !other.types.contains(variable));
        self.rows.retain(|variable| !other.rows.contains(variable));
    }

    fn extend(&mut self, other: Self) {
        self.types.extend(other.types);
        self.rows.extend(other.rows);
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct ReuseBoundary(pub String);

impl ReuseBoundary {
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Scheme {
    pub boundary: ReuseBoundary,
    pub quantified_types: BTreeSet<TypeVarId>,
    pub quantified_rows: BTreeSet<RowVarId>,
    pub body: TypeId,
}

impl Scheme {
    /// Generalization requires the named reusable boundary and every scheme
    /// visible from its environment.
    pub fn generalize_at(
        boundary: ReuseBoundary,
        store: &TypeStore,
        body: TypeId,
        environment: &[Scheme],
    ) -> Self {
        let mut free = free_in_type(store, body, &BTreeSet::new(), &mut BTreeSet::new());
        let environment_free =
            environment
                .iter()
                .fold(FreeVariables::default(), |mut all, scheme| {
                    all.extend(scheme.free_variables(store));
                    all
                });
        free.subtract(&environment_free);
        Self {
            boundary,
            quantified_types: free.types,
            quantified_rows: free.rows,
            body,
        }
    }

    pub fn monomorphic_at(boundary: ReuseBoundary, body: TypeId) -> Self {
        Self {
            boundary,
            quantified_types: BTreeSet::new(),
            quantified_rows: BTreeSet::new(),
            body,
        }
    }

    /// Each instantiation gets fresh stable IDs; no generalized variable leaks
    /// between uses.
    pub fn instantiate(&self, store: &mut TypeStore) -> TypeId {
        let mut type_replacements = BTreeMap::new();
        for variable in &self.quantified_types {
            type_replacements.insert(*variable, store.fresh_type_variable().1);
        }
        let mut row_replacements = BTreeMap::new();
        for variable in &self.quantified_rows {
            row_replacements.insert(*variable, store.fresh_row_variable());
        }
        copy_type(
            store,
            self.body,
            &mut type_replacements,
            &mut row_replacements,
        )
    }

    pub fn free_variables(&self, store: &TypeStore) -> FreeVariables {
        let mut free = free_in_type(store, self.body, &BTreeSet::new(), &mut BTreeSet::new());
        free.types
            .retain(|variable| !self.quantified_types.contains(variable));
        free.rows
            .retain(|variable| !self.quantified_rows.contains(variable));
        free
    }
}

fn free_in_type(
    store: &TypeStore,
    id: TypeId,
    bound: &BTreeSet<TypeVarId>,
    visiting: &mut BTreeSet<TypeId>,
) -> FreeVariables {
    if !visiting.insert(id) {
        return FreeVariables::default();
    }
    let mut free = match store.term(id) {
        TypeTerm::Var(variable) if !bound.contains(variable) => FreeVariables {
            types: BTreeSet::from([*variable]),
            rows: BTreeSet::new(),
        },
        TypeTerm::Var(_) | TypeTerm::Primitive(_) => FreeVariables::default(),
        TypeTerm::Tuple(elements) => {
            elements
                .iter()
                .fold(FreeVariables::default(), |mut all, element| {
                    all.extend(free_in_type(store, *element, bound, visiting));
                    all
                })
        }
        TypeTerm::List(element) | TypeTerm::Option(element) => {
            free_in_type(store, *element, bound, visiting)
        }
        TypeTerm::Function { parameters, result } => {
            let mut all = parameters
                .iter()
                .fold(FreeVariables::default(), |mut all, parameter| {
                    all.extend(free_in_type(store, *parameter, bound, visiting));
                    all
                });
            all.extend(free_in_type(store, *result, bound, visiting));
            all
        }
        TypeTerm::Record(row) | TypeTerm::Variant(row) => free_in_row(store, row, bound, visiting),
        TypeTerm::Mu { binder, body } => {
            let mut nested = bound.clone();
            nested.insert(*binder);
            free_in_type(store, *body, &nested, visiting)
        }
    };
    visiting.remove(&id);
    if let TypeTerm::Mu { binder, .. } = store.term(id) {
        free.types.remove(binder);
    }
    free
}

fn free_in_row(
    store: &TypeStore,
    row: &Row,
    bound: &BTreeSet<TypeVarId>,
    visiting: &mut BTreeSet<TypeId>,
) -> FreeVariables {
    let mut free = row
        .fields
        .values()
        .fold(FreeVariables::default(), |mut all, field| {
            all.extend(free_in_type(store, *field, bound, visiting));
            all
        });
    if let RowTail::Open(variable) = row.tail {
        free.rows.insert(variable);
    }
    free
}

fn copy_type(
    store: &mut TypeStore,
    id: TypeId,
    type_replacements: &mut BTreeMap<TypeVarId, TypeId>,
    row_replacements: &mut BTreeMap<RowVarId, RowVarId>,
) -> TypeId {
    match store.term(id).clone() {
        TypeTerm::Var(variable) => type_replacements.get(&variable).copied().unwrap_or(id),
        TypeTerm::Primitive(primitive) => store.primitive(primitive),
        TypeTerm::Tuple(elements) => {
            let elements = elements
                .into_iter()
                .map(|element| copy_type(store, element, type_replacements, row_replacements))
                .collect::<Vec<_>>();
            store.tuple(elements)
        }
        TypeTerm::List(element) => {
            let element = copy_type(store, element, type_replacements, row_replacements);
            store.list(element)
        }
        TypeTerm::Option(element) => {
            let element = copy_type(store, element, type_replacements, row_replacements);
            store.option(element)
        }
        TypeTerm::Function { parameters, result } => {
            let parameters = parameters
                .into_iter()
                .map(|parameter| copy_type(store, parameter, type_replacements, row_replacements))
                .collect::<Vec<_>>();
            let result = copy_type(store, result, type_replacements, row_replacements);
            store.function(parameters, result)
        }
        TypeTerm::Record(row) => {
            let row = copy_row(store, row, type_replacements, row_replacements);
            store.record(row)
        }
        TypeTerm::Variant(row) => {
            let row = copy_row(store, row, type_replacements, row_replacements);
            store.variant(row)
        }
        TypeTerm::Mu { binder, body } => {
            let (fresh_binder, fresh_type) = store.fresh_type_variable();
            let previous = type_replacements.insert(binder, fresh_type);
            let body = copy_type(store, body, type_replacements, row_replacements);
            match previous {
                Some(previous) => {
                    type_replacements.insert(binder, previous);
                }
                None => {
                    type_replacements.remove(&binder);
                }
            }
            store.mu(fresh_binder, body)
        }
    }
}

fn copy_row(
    store: &mut TypeStore,
    row: Row,
    type_replacements: &mut BTreeMap<TypeVarId, TypeId>,
    row_replacements: &mut BTreeMap<RowVarId, RowVarId>,
) -> Row {
    let fields = row
        .fields
        .into_iter()
        .map(|(name, ty)| {
            (
                name,
                copy_type(store, ty, type_replacements, row_replacements),
            )
        })
        .collect::<BTreeMap<_, _>>();
    let tail = match row.tail {
        RowTail::Closed => RowTail::Closed,
        RowTail::Open(variable) => RowTail::Open(
            *row_replacements
                .entry(variable)
                .or_insert_with(|| store.fresh_row_variable()),
        ),
    };
    Row { fields, tail }
}
