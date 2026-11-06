use crate::{
    Exhaustion, LayoutError, Solution, SolvedRow, SolvedTail, SolvedType, TypeId, TypeStore,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Abi {
    AppleAarch64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Layout {
    Zero,
    Scalar { bytes: u8, align: u8 },
    Pointer,
    Aggregate(Vec<Layout>),
    Tagged { payloads: Vec<Layout> },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LayoutPlan {
    semantic: String,
    abi: Abi,
    layout: Layout,
}

impl LayoutPlan {
    pub fn semantic(&self) -> &str {
        &self.semantic
    }

    pub fn abi(&self) -> Abi {
        self.abi
    }

    pub fn layout(&self) -> &Layout {
        &self.layout
    }
}

pub fn lower_layout(
    solution: &Solution,
    store: &TypeStore,
    root: TypeId,
    abi: Abi,
) -> Result<LayoutPlan, LayoutError> {
    if solution.scope() != store.revision() {
        return Err(LayoutError::CrossStore);
    }
    let semantic = solution
        .resolve(store, root)
        .map_err(|_| LayoutError::CrossStore)?;
    let layout = lower(&semantic, 0, 256)?;
    Ok(LayoutPlan {
        semantic: semantic.render(),
        abi,
        layout,
    })
}

fn lower(ty: &SolvedType, depth: usize, limit: usize) -> Result<Layout, LayoutError> {
    if depth > limit {
        return Err(LayoutError::Exhausted(Exhaustion::Depth));
    }
    Ok(match ty {
        SolvedType::Meta(_) | SolvedType::Bound(_) => {
            return Err(LayoutError::Unresolved(ty.render()));
        }
        SolvedType::Primitive(crate::Primitive::Unit) => Layout::Zero,
        SolvedType::Primitive(crate::Primitive::Bool) => Layout::Scalar { bytes: 1, align: 1 },
        SolvedType::Primitive(crate::Primitive::I64 | crate::Primitive::F64) => {
            Layout::Scalar { bytes: 8, align: 8 }
        }
        SolvedType::Primitive(crate::Primitive::Str | crate::Primitive::Bytes)
        | SolvedType::List(_)
        | SolvedType::Function { .. }
        | SolvedType::Mu(_) => Layout::Pointer,
        SolvedType::Option(item) => Layout::Tagged {
            payloads: vec![Layout::Zero, lower(item, depth + 1, limit)?],
        },
        SolvedType::Tuple(items) => Layout::Aggregate(
            items
                .iter()
                .map(|item| lower(item, depth + 1, limit))
                .collect::<Result<_, _>>()?,
        ),
        SolvedType::Record(row) => lower_record(row, depth, limit)?,
        SolvedType::Variant(row) => {
            ensure_closed(row)?;
            Layout::Tagged {
                payloads: row
                    .fields
                    .values()
                    .map(|item| lower(item, depth + 1, limit))
                    .collect::<Result<_, _>>()?,
            }
        }
    })
}

fn lower_record(row: &SolvedRow, depth: usize, limit: usize) -> Result<Layout, LayoutError> {
    ensure_closed(row)?;
    Ok(Layout::Aggregate(
        row.fields
            .values()
            .map(|item| lower(item, depth + 1, limit))
            .collect::<Result<_, _>>()?,
    ))
}

fn ensure_closed(row: &SolvedRow) -> Result<(), LayoutError> {
    if row.tail == SolvedTail::Closed {
        Ok(())
    } else {
        Err(LayoutError::Unresolved(format!("row tail {:?}", row.tail)))
    }
}
