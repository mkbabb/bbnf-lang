//! Qualified schemes, levels, rigid environments, and the value restriction.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

use super::Origin;
use super::solve::{Constraint, FreeRowVar, FreeTypeVar, SolvedRow, SolvedType, TypeSolution};
use super::store::{BuildError, Primitive, RecVar, RowId, RowSpec, StoreBuilder, TypeId};

/// The carrier's named value restriction: only syntactic values generalize.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BindingForm {
    /// Literals, constructors, tuples/records of values, and functions.
    SyntacticValue,
    /// Parser execution, host calls, maps, and other computations.
    Expansive,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ValueRestriction {
    SyntacticValuesOnly,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct RigidType {
    revision: u64,
    id: u32,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct RigidRow {
    revision: u64,
    id: u32,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum PolyTypeVar {
    Quantified(u32),
    Rigid(RigidType),
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum PolyRowVar {
    Quantified(u32),
    Rigid(RigidRow),
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct PolyRow {
    fields: BTreeMap<String, PolyType>,
    tail: Option<PolyRowVar>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum PolyType {
    Variable(PolyTypeVar),
    Rec(u32),
    Primitive(Primitive),
    Named(String),
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
struct LacksPredicate {
    row: PolyRowVar,
    label: String,
}

/// Owned `forall type_vars, row_vars. lacks predicates => body`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Scheme {
    revision: u64,
    type_variables: u32,
    row_variables: u32,
    predicates: Vec<LacksPredicate>,
    body: PolyType,
    restriction: ValueRestriction,
}

impl Scheme {
    pub fn generalize(
        solution: &TypeSolution,
        root: TypeId,
        environment_level: u32,
        binding: BindingForm,
    ) -> Result<Self, SchemeError> {
        let solved = solution
            .type_of(root)
            .ok_or_else(|| SchemeError::new(SchemeErrorKind::MissingRoot, "scheme.generalize"))?;
        let can_generalize = binding == BindingForm::SyntacticValue;
        let mut type_quantifiers = BTreeMap::new();
        let mut row_quantifiers = BTreeMap::new();
        collect_variables(
            solved,
            environment_level,
            can_generalize,
            &mut type_quantifiers,
            &mut row_quantifiers,
        );
        let body = poly_type(
            solved,
            solution.revision(),
            &type_quantifiers,
            &row_quantifiers,
        );
        let mut predicates = Vec::new();
        collect_rows(solved, &mut |row| {
            if let Some(lacks) = solution.row_lacks(row) {
                let poly_row = row_quantifiers.get(&row).copied().map_or_else(
                    || {
                        PolyRowVar::Rigid(RigidRow {
                            revision: solution.revision(),
                            id: row.id(),
                        })
                    },
                    PolyRowVar::Quantified,
                );
                for label in lacks {
                    predicates.push(LacksPredicate {
                        row: poly_row,
                        label: label.clone(),
                    });
                }
            }
        });
        predicates.sort_by(|left, right| (left.row, &left.label).cmp(&(right.row, &right.label)));
        predicates.dedup();
        Ok(Self {
            revision: solution.revision(),
            type_variables: u32::try_from(type_quantifiers.len()).unwrap_or(u32::MAX),
            row_variables: u32::try_from(row_quantifiers.len()).unwrap_or(u32::MAX),
            predicates,
            body,
            restriction: ValueRestriction::SyntacticValuesOnly,
        })
    }

    #[must_use]
    pub fn quantified_type_count(&self) -> u32 {
        self.type_variables
    }

    #[must_use]
    pub fn quantified_row_count(&self) -> u32 {
        self.row_variables
    }

    #[must_use]
    pub fn predicate_count(&self) -> usize {
        self.predicates.len()
    }

    #[must_use]
    pub fn value_restriction(&self) -> ValueRestriction {
        self.restriction
    }

    pub fn instantiate(
        &self,
        builder: &mut StoreBuilder,
        rigids: &mut RigidContext,
        level: u32,
        origin: Origin,
    ) -> Result<Instantiation, SchemeError> {
        rigids.check_scope(builder.revision())?;
        let mut quantified_types = BTreeMap::new();
        for variable in 0..self.type_variables {
            let (_, ty) = builder.fresh_meta(level).map_err(SchemeError::build)?;
            quantified_types.insert(variable, ty);
        }
        let mut quantified_rows = BTreeMap::new();
        for variable in 0..self.row_variables {
            let row = builder.fresh_row(level).map_err(SchemeError::build)?;
            quantified_rows.insert(variable, row);
        }
        let mut state = InstantiationState {
            quantified_types: &quantified_types,
            quantified_rows: &quantified_rows,
            binders: BTreeMap::new(),
            next_binder: 0,
        };
        let root = instantiate_type(&self.body, builder, rigids, level, &mut state)?;
        let mut constraints = Vec::with_capacity(self.predicates.len());
        for predicate in &self.predicates {
            let row = instantiate_row_var(predicate.row, builder, rigids, level, &quantified_rows)?;
            constraints.push(Constraint::lacks(
                row,
                predicate.label.clone(),
                origin.clone(),
            ));
        }
        Ok(Instantiation { root, constraints })
    }

    #[must_use]
    pub fn revision(&self) -> u64 {
        self.revision
    }
}

#[derive(Clone, Debug)]
pub struct Instantiation {
    root: TypeId,
    constraints: Vec<Constraint>,
}

impl Instantiation {
    #[must_use]
    pub fn root(&self) -> TypeId {
        self.root
    }

    #[must_use]
    pub fn constraints(&self) -> &[Constraint] {
        &self.constraints
    }
}

/// A caller-owned rigid substitution context.  Reusing it preserves the same
/// environment variable across multiple instantiations; quantified variables
/// remain fresh per instantiation.
#[derive(Clone, Debug, Default)]
pub struct RigidContext {
    builder_revision: Option<u64>,
    types: BTreeMap<RigidType, TypeId>,
    rows: BTreeMap<RigidRow, RowId>,
}

impl RigidContext {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    fn check_scope(&mut self, revision: u64) -> Result<(), SchemeError> {
        match self.builder_revision {
            Some(existing) if existing != revision => Err(SchemeError::new(
                SchemeErrorKind::RigidContextScope,
                "scheme.instantiate",
            )),
            Some(_) => Ok(()),
            None => {
                self.builder_revision = Some(revision);
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SchemeErrorKind {
    MissingRoot,
    RigidContextScope,
    Build,
    InvalidRecursiveReference,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SchemeError {
    kind: SchemeErrorKind,
    site: String,
}

impl SchemeError {
    fn new(kind: SchemeErrorKind, site: impl Into<String>) -> Self {
        Self {
            kind,
            site: site.into(),
        }
    }

    fn build(error: BuildError) -> Self {
        Self::new(SchemeErrorKind::Build, error.site())
    }

    #[must_use]
    pub fn kind(&self) -> &SchemeErrorKind {
        &self.kind
    }

    #[must_use]
    pub fn site(&self) -> &str {
        &self.site
    }
}

impl fmt::Display for SchemeError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{:?} at {}", self.kind, self.site)
    }
}

impl std::error::Error for SchemeError {}

fn collect_variables(
    ty: &SolvedType,
    environment_level: u32,
    can_generalize: bool,
    types: &mut BTreeMap<FreeTypeVar, u32>,
    rows: &mut BTreeMap<FreeRowVar, u32>,
) {
    let mut pending = vec![ty];
    while let Some(ty) = pending.pop() {
        match ty {
            SolvedType::Free(variable)
                if can_generalize && variable.level() > environment_level =>
            {
                let next = u32::try_from(types.len()).unwrap_or(u32::MAX);
                types.entry(*variable).or_insert(next);
            }
            SolvedType::Tuple(items) => pending.extend(items),
            SolvedType::List(item) | SolvedType::Option(item) | SolvedType::Mu(item) => {
                pending.push(item)
            }
            SolvedType::Function { parameters, result } => {
                pending.extend(parameters);
                pending.push(result);
            }
            SolvedType::Record(row) | SolvedType::Variant(row) => {
                pending.extend(row.fields().values());
                if let Some(variable) = row.tail()
                    && can_generalize
                    && variable.level() > environment_level
                {
                    let next = u32::try_from(rows.len()).unwrap_or(u32::MAX);
                    rows.entry(variable).or_insert(next);
                }
            }
            SolvedType::Free(_)
            | SolvedType::Rec(_)
            | SolvedType::Primitive(_)
            | SolvedType::Named(_) => {}
        }
    }
}

fn poly_type(
    ty: &SolvedType,
    revision: u64,
    type_quantifiers: &BTreeMap<FreeTypeVar, u32>,
    row_quantifiers: &BTreeMap<FreeRowVar, u32>,
) -> PolyType {
    match ty {
        SolvedType::Free(variable) => {
            PolyType::Variable(type_quantifiers.get(variable).copied().map_or_else(
                || {
                    PolyTypeVar::Rigid(RigidType {
                        revision,
                        id: variable.id(),
                    })
                },
                PolyTypeVar::Quantified,
            ))
        }
        SolvedType::Rec(index) => PolyType::Rec(*index),
        SolvedType::Primitive(primitive) => PolyType::Primitive(*primitive),
        SolvedType::Named(name) => PolyType::Named(name.clone()),
        SolvedType::Tuple(items) => PolyType::Tuple(
            items
                .iter()
                .map(|item| poly_type(item, revision, type_quantifiers, row_quantifiers))
                .collect(),
        ),
        SolvedType::List(item) => PolyType::List(Box::new(poly_type(
            item,
            revision,
            type_quantifiers,
            row_quantifiers,
        ))),
        SolvedType::Option(item) => PolyType::Option(Box::new(poly_type(
            item,
            revision,
            type_quantifiers,
            row_quantifiers,
        ))),
        SolvedType::Function { parameters, result } => PolyType::Function {
            parameters: parameters
                .iter()
                .map(|item| poly_type(item, revision, type_quantifiers, row_quantifiers))
                .collect(),
            result: Box::new(poly_type(
                result,
                revision,
                type_quantifiers,
                row_quantifiers,
            )),
        },
        SolvedType::Record(row) => {
            PolyType::Record(poly_row(row, revision, type_quantifiers, row_quantifiers))
        }
        SolvedType::Variant(row) => {
            PolyType::Variant(poly_row(row, revision, type_quantifiers, row_quantifiers))
        }
        SolvedType::Mu(body) => PolyType::Mu(Box::new(poly_type(
            body,
            revision,
            type_quantifiers,
            row_quantifiers,
        ))),
    }
}

fn poly_row(
    row: &SolvedRow,
    revision: u64,
    type_quantifiers: &BTreeMap<FreeTypeVar, u32>,
    row_quantifiers: &BTreeMap<FreeRowVar, u32>,
) -> PolyRow {
    PolyRow {
        fields: row
            .fields()
            .iter()
            .map(|(label, ty)| {
                (
                    label.clone(),
                    poly_type(ty, revision, type_quantifiers, row_quantifiers),
                )
            })
            .collect(),
        tail: row.tail().map(|row| {
            row_quantifiers.get(&row).copied().map_or_else(
                || {
                    PolyRowVar::Rigid(RigidRow {
                        revision,
                        id: row.id(),
                    })
                },
                PolyRowVar::Quantified,
            )
        }),
    }
}

fn collect_rows(ty: &SolvedType, visit: &mut impl FnMut(FreeRowVar)) {
    let mut pending = vec![ty];
    let mut seen = BTreeSet::new();
    while let Some(ty) = pending.pop() {
        match ty {
            SolvedType::Tuple(items) => pending.extend(items),
            SolvedType::List(item) | SolvedType::Option(item) | SolvedType::Mu(item) => {
                pending.push(item)
            }
            SolvedType::Function { parameters, result } => {
                pending.extend(parameters);
                pending.push(result);
            }
            SolvedType::Record(row) | SolvedType::Variant(row) => {
                pending.extend(row.fields().values());
                if let Some(row) = row.tail()
                    && seen.insert(row)
                {
                    visit(row);
                }
            }
            SolvedType::Free(_)
            | SolvedType::Rec(_)
            | SolvedType::Primitive(_)
            | SolvedType::Named(_) => {}
        }
    }
}

struct InstantiationState<'quantified> {
    quantified_types: &'quantified BTreeMap<u32, TypeId>,
    quantified_rows: &'quantified BTreeMap<u32, RowId>,
    binders: BTreeMap<u32, RecVar>,
    next_binder: u32,
}

fn instantiate_type(
    ty: &PolyType,
    builder: &mut StoreBuilder,
    rigids: &mut RigidContext,
    level: u32,
    state: &mut InstantiationState<'_>,
) -> Result<TypeId, SchemeError> {
    match ty {
        PolyType::Variable(PolyTypeVar::Quantified(variable)) => state
            .quantified_types
            .get(variable)
            .copied()
            .ok_or_else(|| SchemeError::new(SchemeErrorKind::Build, "scheme.quantified_type")),
        PolyType::Variable(PolyTypeVar::Rigid(rigid)) => {
            if let Some(ty) = rigids.types.get(rigid) {
                Ok(*ty)
            } else {
                let (_, ty) = builder.fresh_meta(level).map_err(SchemeError::build)?;
                rigids.types.insert(*rigid, ty);
                Ok(ty)
            }
        }
        PolyType::Rec(index) => {
            let binder = state.binders.get(index).copied().ok_or_else(|| {
                SchemeError::new(SchemeErrorKind::InvalidRecursiveReference, "scheme.rec")
            })?;
            builder.rec_var(binder).map_err(SchemeError::build)
        }
        PolyType::Primitive(primitive) => builder.primitive(*primitive).map_err(SchemeError::build),
        PolyType::Named(name) => builder.named(name.clone()).map_err(SchemeError::build),
        PolyType::Tuple(items) => {
            let items = items
                .iter()
                .map(|item| instantiate_type(item, builder, rigids, level, state))
                .collect::<Result<Vec<_>, _>>()?;
            builder.tuple(items).map_err(SchemeError::build)
        }
        PolyType::List(item) | PolyType::Option(item) => {
            let item = instantiate_type(item, builder, rigids, level, state)?;
            if matches!(ty, PolyType::List(_)) {
                builder.list(item).map_err(SchemeError::build)
            } else {
                builder.option(item).map_err(SchemeError::build)
            }
        }
        PolyType::Function { parameters, result } => {
            let parameters = parameters
                .iter()
                .map(|parameter| instantiate_type(parameter, builder, rigids, level, state))
                .collect::<Result<Vec<_>, _>>()?;
            let result = instantiate_type(result, builder, rigids, level, state)?;
            builder
                .function(parameters, result)
                .map_err(SchemeError::build)
        }
        PolyType::Record(row) | PolyType::Variant(row) => {
            let fields = row
                .fields
                .iter()
                .map(|(label, ty)| {
                    instantiate_type(ty, builder, rigids, level, state)
                        .map(|ty| (label.clone(), ty))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let tail = row
                .tail
                .map(|row| instantiate_row_var(row, builder, rigids, level, state.quantified_rows))
                .transpose()?;
            let row = if let Some(tail) = tail {
                RowSpec::open(fields, tail)
            } else {
                RowSpec::closed(fields)
            }
            .map_err(SchemeError::build)?;
            if matches!(ty, PolyType::Record(_)) {
                builder.record(row).map_err(SchemeError::build)
            } else {
                builder.variant(row).map_err(SchemeError::build)
            }
        }
        PolyType::Mu(body) => {
            let binder = builder.fresh_rec_var().map_err(SchemeError::build)?;
            let canonical = state.next_binder;
            state.next_binder = state.next_binder.saturating_add(1);
            state.binders.insert(canonical, binder);
            let body = instantiate_type(body, builder, rigids, level, state)?;
            state.binders.remove(&canonical);
            builder.mu(binder, body).map_err(SchemeError::build)
        }
    }
}

fn instantiate_row_var(
    row: PolyRowVar,
    builder: &mut StoreBuilder,
    rigids: &mut RigidContext,
    level: u32,
    quantified_rows: &BTreeMap<u32, RowId>,
) -> Result<RowId, SchemeError> {
    match row {
        PolyRowVar::Quantified(variable) => quantified_rows
            .get(&variable)
            .copied()
            .ok_or_else(|| SchemeError::new(SchemeErrorKind::Build, "scheme.quantified_row")),
        PolyRowVar::Rigid(rigid) => {
            if let Some(row) = rigids.rows.get(&rigid) {
                Ok(*row)
            } else {
                let row = builder.fresh_row(level).map_err(SchemeError::build)?;
                rigids.rows.insert(rigid, row);
                Ok(row)
            }
        }
    }
}
