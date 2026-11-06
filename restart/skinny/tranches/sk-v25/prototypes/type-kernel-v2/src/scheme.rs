use std::collections::{BTreeMap, BTreeSet};

use crate::{
    Binder, BuildError, MetaId, Primitive, RowId, RowSpec, Solution, SolvedRow, SolvedTail,
    SolvedType, StoreBuilder, TypeId, TypeStore,
};

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Boundary(String);

impl Boundary {
    pub fn new(name: impl Into<String>) -> Result<Self, BuildError> {
        let name = name.into();
        if name.trim().is_empty() {
            return Err(BuildError::EmptyLabel);
        }
        Ok(Self(name))
    }

    pub fn name(&self) -> &str {
        &self.0
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum PolyTail {
    Closed,
    Quantified(u32),
    Rigid(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct PolyRow {
    fields: BTreeMap<String, PolyType>,
    tail: PolyTail,
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum PolyType {
    Quantified(u32),
    Rigid(String),
    Bound(u32),
    Primitive(Primitive),
    Tuple(Vec<PolyType>),
    List(Box<PolyType>),
    Option(Box<PolyType>),
    Function {
        parameters: Vec<PolyType>,
        result: Box<PolyType>,
    },
    Record(PolyRow),
    Variant(PolyRow),
    Mu(Box<PolyType>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Scheme {
    boundary: Boundary,
    body: PolyType,
    quantified_types: usize,
    quantified_rows: usize,
}

impl Scheme {
    pub fn generalize(
        solution: &Solution,
        store: &TypeStore,
        root: TypeId,
        environment: &[SolvedType],
        boundary: Boundary,
    ) -> Result<Self, crate::SolveError> {
        let body = solution.resolve(store, root)?;
        let mut body_types = BTreeSet::new();
        let mut body_rows = BTreeSet::new();
        collect_free(&body, &mut body_types, &mut body_rows);
        for environment_type in environment {
            let mut types = BTreeSet::new();
            let mut rows = BTreeSet::new();
            collect_free(environment_type, &mut types, &mut rows);
            body_types.retain(|meta| !types.contains(meta));
            body_rows.retain(|row| !rows.contains(row));
        }
        let type_indices = body_types
            .into_iter()
            .enumerate()
            .map(|(index, meta)| (meta, index as u32))
            .collect::<BTreeMap<_, _>>();
        let row_indices = body_rows
            .into_iter()
            .enumerate()
            .map(|(index, row)| (row, index as u32))
            .collect::<BTreeMap<_, _>>();
        Ok(Self {
            boundary,
            body: poly_type(body, &type_indices, &row_indices),
            quantified_types: type_indices.len(),
            quantified_rows: row_indices.len(),
        })
    }

    pub fn boundary(&self) -> &Boundary {
        &self.boundary
    }

    pub fn quantified_types(&self) -> usize {
        self.quantified_types
    }

    pub fn quantified_rows(&self) -> usize {
        self.quantified_rows
    }

    pub fn instantiate(&self, builder: &mut StoreBuilder) -> Result<TypeId, BuildError> {
        let mut types = BTreeMap::new();
        let mut rows = BTreeMap::new();
        let mut binders = Vec::new();
        instantiate_type(&self.body, builder, &mut types, &mut rows, &mut binders)
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum FreeRow {
    Open(RowId),
    Residual(u32),
}

fn collect_free(ty: &SolvedType, types: &mut BTreeSet<MetaId>, rows: &mut BTreeSet<FreeRow>) {
    match ty {
        SolvedType::Meta(meta) => {
            types.insert(*meta);
        }
        SolvedType::Bound(_) | SolvedType::Primitive(_) => {}
        SolvedType::Tuple(items) => {
            for item in items {
                collect_free(item, types, rows);
            }
        }
        SolvedType::List(item) | SolvedType::Option(item) | SolvedType::Mu(item) => {
            collect_free(item, types, rows);
        }
        SolvedType::Function { parameters, result } => {
            for parameter in parameters {
                collect_free(parameter, types, rows);
            }
            collect_free(result, types, rows);
        }
        SolvedType::Record(row) | SolvedType::Variant(row) => {
            for field in row.fields.values() {
                collect_free(field, types, rows);
            }
            match row.tail {
                SolvedTail::Closed => {}
                SolvedTail::Open(row) => {
                    rows.insert(FreeRow::Open(row));
                }
                SolvedTail::Residual(row) => {
                    rows.insert(FreeRow::Residual(row));
                }
            }
        }
    }
}

fn poly_type(
    ty: SolvedType,
    types: &BTreeMap<MetaId, u32>,
    rows: &BTreeMap<FreeRow, u32>,
) -> PolyType {
    match ty {
        SolvedType::Meta(meta) => types.get(&meta).map_or_else(
            || PolyType::Rigid(format!("m{meta:?}")),
            |index| PolyType::Quantified(*index),
        ),
        SolvedType::Bound(index) => PolyType::Bound(index),
        SolvedType::Primitive(primitive) => PolyType::Primitive(primitive),
        SolvedType::Tuple(items) => PolyType::Tuple(
            items
                .into_iter()
                .map(|item| poly_type(item, types, rows))
                .collect(),
        ),
        SolvedType::List(item) => PolyType::List(Box::new(poly_type(*item, types, rows))),
        SolvedType::Option(item) => PolyType::Option(Box::new(poly_type(*item, types, rows))),
        SolvedType::Function { parameters, result } => PolyType::Function {
            parameters: parameters
                .into_iter()
                .map(|item| poly_type(item, types, rows))
                .collect(),
            result: Box::new(poly_type(*result, types, rows)),
        },
        SolvedType::Record(row) => PolyType::Record(poly_row(row, types, rows)),
        SolvedType::Variant(row) => PolyType::Variant(poly_row(row, types, rows)),
        SolvedType::Mu(body) => PolyType::Mu(Box::new(poly_type(*body, types, rows))),
    }
}

fn poly_row(
    row: SolvedRow,
    types: &BTreeMap<MetaId, u32>,
    rows: &BTreeMap<FreeRow, u32>,
) -> PolyRow {
    let tail_key = match row.tail {
        SolvedTail::Closed => None,
        SolvedTail::Open(row) => Some(FreeRow::Open(row)),
        SolvedTail::Residual(row) => Some(FreeRow::Residual(row)),
    };
    PolyRow {
        fields: row
            .fields
            .into_iter()
            .map(|(label, ty)| (label, poly_type(ty, types, rows)))
            .collect(),
        tail: tail_key.map_or(PolyTail::Closed, |key| {
            rows.get(&key).map_or_else(
                || PolyTail::Rigid(format!("{key:?}")),
                |index| PolyTail::Quantified(*index),
            )
        }),
    }
}

fn instantiate_type(
    ty: &PolyType,
    builder: &mut StoreBuilder,
    types: &mut BTreeMap<u32, TypeId>,
    rows: &mut BTreeMap<u32, RowId>,
    binders: &mut Vec<Binder>,
) -> Result<TypeId, BuildError> {
    match ty {
        PolyType::Quantified(index) => {
            if let Some(ty) = types.get(index) {
                Ok(*ty)
            } else {
                let (_, ty) = builder.fresh_meta()?;
                types.insert(*index, ty);
                Ok(ty)
            }
        }
        PolyType::Rigid(_) => Err(BuildError::DanglingMeta),
        PolyType::Bound(index) => binders
            .iter()
            .rev()
            .nth(*index as usize)
            .copied()
            .ok_or(BuildError::BinderEscapes)
            .and_then(|binder| builder.bound(binder)),
        PolyType::Primitive(primitive) => builder.primitive(*primitive),
        PolyType::Tuple(items) => {
            let items = items
                .iter()
                .map(|item| instantiate_type(item, builder, types, rows, binders))
                .collect::<Result<Vec<_>, _>>()?;
            builder.tuple(items)
        }
        PolyType::List(item) => {
            let item = instantiate_type(item, builder, types, rows, binders)?;
            builder.list(item)
        }
        PolyType::Option(item) => {
            let item = instantiate_type(item, builder, types, rows, binders)?;
            builder.option(item)
        }
        PolyType::Function { parameters, result } => {
            let parameters = parameters
                .iter()
                .map(|item| instantiate_type(item, builder, types, rows, binders))
                .collect::<Result<Vec<_>, _>>()?;
            let result = instantiate_type(result, builder, types, rows, binders)?;
            builder.function(parameters, result)
        }
        PolyType::Record(row) => instantiate_row(row, builder, types, rows, binders, true),
        PolyType::Variant(row) => instantiate_row(row, builder, types, rows, binders, false),
        PolyType::Mu(body) => builder.mu(|builder, binder| {
            binders.push(binder);
            let result = instantiate_type(body, builder, types, rows, binders);
            binders.pop();
            result
        }),
    }
}

fn instantiate_row(
    row: &PolyRow,
    builder: &mut StoreBuilder,
    types: &mut BTreeMap<u32, TypeId>,
    rows: &mut BTreeMap<u32, RowId>,
    binders: &mut Vec<Binder>,
    record: bool,
) -> Result<TypeId, BuildError> {
    let fields = row
        .fields
        .iter()
        .map(|(label, ty)| {
            Ok((
                label.clone(),
                instantiate_type(ty, builder, types, rows, binders)?,
            ))
        })
        .collect::<Result<Vec<_>, BuildError>>()?;
    let spec = match &row.tail {
        PolyTail::Closed => RowSpec::closed(fields)?,
        PolyTail::Rigid(_) => return Err(BuildError::DanglingRow),
        PolyTail::Quantified(index) => {
            let tail = if let Some(row) = rows.get(index) {
                *row
            } else {
                let row = builder.fresh_row()?;
                rows.insert(*index, row);
                row
            };
            RowSpec::open(fields, tail)?
        }
    };
    if record {
        builder.record(spec)
    } else {
        builder.variant(spec)
    }
}
