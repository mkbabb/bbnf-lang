use crate::{MetaId, RowId, TypeId};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BuildError {
    CrossStore,
    DanglingType,
    DanglingMeta,
    DanglingRow,
    DuplicateLabel(String),
    EmptyLabel,
    BinderNotActive,
    BinderEscapes,
    BuilderPoisoned,
    TooManyTerms,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Exhaustion {
    Steps,
    Depth,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SolveError {
    CrossStore,
    DanglingType(TypeId),
    Exhausted {
        kind: Exhaustion,
        limit: usize,
    },
    Occurs {
        meta: MetaId,
        within: String,
        origins: Vec<crate::Origin>,
    },
    RowOccurs {
        row: RowId,
        within: String,
        origins: Vec<crate::Origin>,
    },
    Lacks {
        row: RowId,
        label: String,
        origins: Vec<crate::Origin>,
    },
    Mismatch {
        left: String,
        right: String,
        path: Vec<String>,
        origins: Vec<crate::Origin>,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LayoutError {
    CrossStore,
    Unresolved(String),
    Unsupported(String),
    Exhausted(Exhaustion),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ProjectError {
    InvalidName,
    Unresolved(String),
    Unsupported(String),
}
