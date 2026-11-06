use super::state::public_tail;
use super::*;

pub(super) fn zonk_internal_type(
    ty: Ty,
    types: &BTreeMap<MetaId, Ty>,
    rows: &BTreeMap<RowId, Row>,
    visiting_types: &mut BTreeSet<MetaId>,
    visiting_rows: &mut BTreeSet<RowId>,
) -> SolvedType {
    match ty {
        Ty::Meta(meta) if visiting_types.insert(meta) => {
            let solved = types
                .get(&meta)
                .cloned()
                .map_or(SolvedType::Meta(meta), |bound| {
                    zonk_internal_type(bound, types, rows, visiting_types, visiting_rows)
                });
            visiting_types.remove(&meta);
            solved
        }
        Ty::Meta(meta) => SolvedType::Meta(meta),
        Ty::Bound(index) => SolvedType::Bound(index),
        Ty::Primitive(primitive) => SolvedType::Primitive(primitive),
        Ty::Tuple(items) => SolvedType::Tuple(
            items
                .into_iter()
                .map(|item| zonk_internal_type(item, types, rows, visiting_types, visiting_rows))
                .collect(),
        ),
        Ty::List(item) => SolvedType::List(Box::new(zonk_internal_type(
            *item,
            types,
            rows,
            visiting_types,
            visiting_rows,
        ))),
        Ty::Option(item) => SolvedType::Option(Box::new(zonk_internal_type(
            *item,
            types,
            rows,
            visiting_types,
            visiting_rows,
        ))),
        Ty::Function { parameters, result } => SolvedType::Function {
            parameters: parameters
                .into_iter()
                .map(|item| zonk_internal_type(item, types, rows, visiting_types, visiting_rows))
                .collect(),
            result: Box::new(zonk_internal_type(
                *result,
                types,
                rows,
                visiting_types,
                visiting_rows,
            )),
        },
        Ty::Record(row) => SolvedType::Record(zonk_internal_row(
            row,
            types,
            rows,
            visiting_types,
            visiting_rows,
        )),
        Ty::Variant(row) => SolvedType::Variant(zonk_internal_row(
            row,
            types,
            rows,
            visiting_types,
            visiting_rows,
        )),
        Ty::Mu(body) => SolvedType::Mu(Box::new(zonk_internal_type(
            *body,
            types,
            rows,
            visiting_types,
            visiting_rows,
        ))),
    }
}

pub(super) fn zonk_internal_row(
    mut row: Row,
    types: &BTreeMap<MetaId, Ty>,
    rows: &BTreeMap<RowId, Row>,
    visiting_types: &mut BTreeSet<MetaId>,
    visiting_rows: &mut BTreeSet<RowId>,
) -> SolvedRow {
    while let Tail::Open(id) = row.tail {
        if !visiting_rows.insert(id) {
            break;
        }
        let Some(bound) = rows.get(&id).cloned() else {
            break;
        };
        row.fields.extend(bound.fields);
        row.tail = bound.tail;
    }
    SolvedRow {
        fields: row
            .fields
            .into_iter()
            .map(|(label, ty)| {
                (
                    label,
                    zonk_internal_type(ty, types, rows, visiting_types, visiting_rows),
                )
            })
            .collect(),
        tail: public_tail(row.tail),
    }
}

pub(super) fn zonk_public(
    ty: Ty,
    types: &BTreeMap<MetaId, SolvedType>,
    rows: &BTreeMap<RowId, SolvedRow>,
    visiting_types: &mut BTreeSet<MetaId>,
    visiting_rows: &mut BTreeSet<RowId>,
) -> SolvedType {
    match ty {
        Ty::Meta(meta) if visiting_types.insert(meta) => {
            let solved = types.get(&meta).cloned().unwrap_or(SolvedType::Meta(meta));
            visiting_types.remove(&meta);
            solved
        }
        Ty::Meta(meta) => SolvedType::Meta(meta),
        Ty::Bound(index) => SolvedType::Bound(index),
        Ty::Primitive(primitive) => SolvedType::Primitive(primitive),
        Ty::Tuple(items) => SolvedType::Tuple(
            items
                .into_iter()
                .map(|item| zonk_public(item, types, rows, visiting_types, visiting_rows))
                .collect(),
        ),
        Ty::List(item) => SolvedType::List(Box::new(zonk_public(
            *item,
            types,
            rows,
            visiting_types,
            visiting_rows,
        ))),
        Ty::Option(item) => SolvedType::Option(Box::new(zonk_public(
            *item,
            types,
            rows,
            visiting_types,
            visiting_rows,
        ))),
        Ty::Function { parameters, result } => SolvedType::Function {
            parameters: parameters
                .into_iter()
                .map(|item| zonk_public(item, types, rows, visiting_types, visiting_rows))
                .collect(),
            result: Box::new(zonk_public(
                *result,
                types,
                rows,
                visiting_types,
                visiting_rows,
            )),
        },
        Ty::Record(row) => SolvedType::Record(zonk_public_row(
            row,
            types,
            rows,
            visiting_types,
            visiting_rows,
        )),
        Ty::Variant(row) => SolvedType::Variant(zonk_public_row(
            row,
            types,
            rows,
            visiting_types,
            visiting_rows,
        )),
        Ty::Mu(body) => SolvedType::Mu(Box::new(zonk_public(
            *body,
            types,
            rows,
            visiting_types,
            visiting_rows,
        ))),
    }
}

fn zonk_public_row(
    mut row: Row,
    types: &BTreeMap<MetaId, SolvedType>,
    rows: &BTreeMap<RowId, SolvedRow>,
    visiting_types: &mut BTreeSet<MetaId>,
    visiting_rows: &mut BTreeSet<RowId>,
) -> SolvedRow {
    let mut fields = BTreeMap::new();
    for (label, ty) in row.fields {
        fields.insert(
            label,
            zonk_public(ty, types, rows, visiting_types, visiting_rows),
        );
    }
    while let Tail::Open(id) = row.tail {
        if !visiting_rows.insert(id) {
            break;
        }
        let Some(bound) = rows.get(&id) else {
            break;
        };
        fields.extend(bound.fields.clone());
        row.tail = match bound.tail {
            SolvedTail::Closed => Tail::Closed,
            SolvedTail::Open(row) => Tail::Open(row),
            SolvedTail::Residual(row) => Tail::Residual(row),
        };
    }
    SolvedRow {
        fields,
        tail: public_tail(row.tail),
    }
}

impl From<BuildError> for SolveError {
    fn from(_: BuildError) -> Self {
        SolveError::CrossStore
    }
}
