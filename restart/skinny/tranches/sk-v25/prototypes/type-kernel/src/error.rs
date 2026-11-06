use crate::{RowVarId, TypeId, TypeVarId};

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Origin {
    pub source: String,
    pub fact: String,
}

impl Origin {
    pub fn new(source: impl Into<String>, fact: impl Into<String>) -> Self {
        Self {
            source: source.into(),
            fact: fact.into(),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RowSide {
    Left,
    Right,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ErrorKind {
    OccursCheck {
        variable: TypeVarId,
        within: TypeId,
    },
    RowOccursCheck {
        variable: RowVarId,
        labels: Vec<String>,
    },
    ConstructorMismatch {
        left: &'static str,
        right: &'static str,
    },
    ArityMismatch {
        constructor: &'static str,
        left: usize,
        right: usize,
    },
    MissingFields {
        closed: RowSide,
        labels: Vec<String>,
    },
    RecursiveMismatch {
        left: TypeId,
        right: TypeId,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeError {
    pub kind: ErrorKind,
    pub origin: Origin,
    pub path: Vec<String>,
}

impl TypeError {
    pub(crate) fn new(kind: ErrorKind, origin: &Origin, path: &[String]) -> Self {
        Self {
            kind,
            origin: origin.clone(),
            path: path.to_vec(),
        }
    }
}
