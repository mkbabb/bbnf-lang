use std::collections::{BTreeMap, BTreeSet};

use crate::term::Node;
use crate::{BuildError, Exhaustion, MetaId, Primitive, RowId, SolveError, TypeId, TypeStore};

mod state;
mod zonk;

use state::load;
pub use state::solve;
use zonk::zonk_public;

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

#[derive(Clone, Debug, Eq, PartialEq)]
enum ConstraintKind {
    Equal(TypeId, TypeId),
    Lacks(RowId, String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Constraint {
    kind: ConstraintKind,
    origin: Origin,
}

impl Constraint {
    pub fn equal(left: TypeId, right: TypeId, origin: Origin) -> Self {
        Self {
            kind: ConstraintKind::Equal(left, right),
            origin,
        }
    }

    pub fn lacks(row: RowId, label: impl Into<String>, origin: Origin) -> Self {
        Self {
            kind: ConstraintKind::Lacks(row, label.into()),
            origin,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Budget {
    pub steps: usize,
    pub depth: usize,
}

impl Budget {
    pub const fn new(steps: usize, depth: usize) -> Self {
        Self { steps, depth }
    }
}

impl Default for Budget {
    fn default() -> Self {
        Self::new(100_000, 512)
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum SolvedTail {
    Closed,
    Open(RowId),
    Residual(u32),
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct SolvedRow {
    pub fields: BTreeMap<String, SolvedType>,
    pub tail: SolvedTail,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum SolvedType {
    Meta(MetaId),
    Bound(u32),
    Primitive(Primitive),
    Tuple(Vec<SolvedType>),
    List(Box<SolvedType>),
    Option(Box<SolvedType>),
    Function {
        parameters: Vec<SolvedType>,
        result: Box<SolvedType>,
    },
    Record(SolvedRow),
    Variant(SolvedRow),
    Mu(Box<SolvedType>),
}

impl SolvedType {
    pub fn render(&self) -> String {
        match self {
            Self::Meta(meta) => format!("?{}", meta.slot),
            Self::Bound(index) => format!("^{index}"),
            Self::Primitive(primitive) => format!("{primitive:?}"),
            Self::Tuple(items) => format!(
                "({})",
                items.iter().map(Self::render).collect::<Vec<_>>().join(",")
            ),
            Self::List(item) => format!("List<{}>", item.render()),
            Self::Option(item) => format!("Option<{}>", item.render()),
            Self::Function { parameters, result } => format!(
                "fn({})->{}",
                parameters
                    .iter()
                    .map(Self::render)
                    .collect::<Vec<_>>()
                    .join(","),
                result.render()
            ),
            Self::Record(row) => render_row("record", row),
            Self::Variant(row) => render_row("variant", row),
            Self::Mu(body) => format!("mu.{}", body.render()),
        }
    }
}

fn render_row(kind: &str, row: &SolvedRow) -> String {
    let fields = row
        .fields
        .iter()
        .map(|(label, ty)| format!("{label}:{}", ty.render()))
        .collect::<Vec<_>>()
        .join(",");
    let tail = match row.tail {
        SolvedTail::Closed => "closed".to_owned(),
        SolvedTail::Open(row) => format!("?r{}", row.slot),
        SolvedTail::Residual(row) => format!("!r{row}"),
    };
    format!("{kind}{{{fields}|{tail}}}")
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Solution {
    scope: u64,
    type_bindings: BTreeMap<MetaId, SolvedType>,
    row_bindings: BTreeMap<RowId, SolvedRow>,
    lacks: BTreeMap<SolvedTail, BTreeSet<String>>,
    origins: Vec<Origin>,
}

impl Solution {
    pub fn resolve(&self, store: &TypeStore, root: TypeId) -> Result<SolvedType, SolveError> {
        if store.revision() != self.scope {
            return Err(SolveError::CrossStore);
        }
        if root.scope != store.scope {
            return Err(SolveError::CrossStore);
        }
        store
            .validate_closed(root)
            .map_err(|_| SolveError::DanglingType(root))?;
        let raw = load(store, root, 0, usize::MAX)?;
        Ok(zonk_public(
            raw,
            &self.type_bindings,
            &self.row_bindings,
            &mut BTreeSet::new(),
            &mut BTreeSet::new(),
        ))
    }

    pub fn fingerprint(&self, store: &TypeStore, root: TypeId) -> Result<String, SolveError> {
        Ok(self.resolve(store, root)?.render())
    }

    pub fn render_bytes(&self) -> Vec<u8> {
        let mut lines = Vec::new();
        for (meta, ty) in &self.type_bindings {
            lines.push(format!("m{}={}", meta.slot, ty.render()));
        }
        for (row, value) in &self.row_bindings {
            lines.push(format!("r{}={}", row.slot, render_row("row", value)));
        }
        for (tail, labels) in &self.lacks {
            lines.push(format!(
                "lacks:{tail:?}:{}",
                labels.iter().cloned().collect::<Vec<_>>().join(",")
            ));
        }
        for origin in &self.origins {
            lines.push(format!("origin:{}:{}", origin.source, origin.fact));
        }
        lines.join("\n").into_bytes()
    }

    pub fn origins(&self) -> &[Origin] {
        &self.origins
    }

    pub(crate) fn scope(&self) -> u64 {
        self.scope
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Tail {
    Closed,
    Open(RowId),
    Residual(u32),
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct Row {
    fields: BTreeMap<String, Ty>,
    tail: Tail,
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum Ty {
    Meta(MetaId),
    Bound(u32),
    Primitive(Primitive),
    Tuple(Vec<Ty>),
    List(Box<Ty>),
    Option(Box<Ty>),
    Function {
        parameters: Vec<Ty>,
        result: Box<Ty>,
    },
    Record(Row),
    Variant(Row),
    Mu(Box<Ty>),
}
