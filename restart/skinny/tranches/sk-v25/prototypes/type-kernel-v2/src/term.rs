use std::collections::BTreeSet;

use crate::{BuildError, RowId, TypeId};

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Primitive {
    Bool,
    Bytes,
    F64,
    I64,
    Str,
    Unit,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct RowSpec {
    pub(crate) fields: Vec<(String, TypeId)>,
    pub(crate) tail: Option<RowId>,
}

impl RowSpec {
    pub fn closed<I, S>(fields: I) -> Result<Self, BuildError>
    where
        I: IntoIterator<Item = (S, TypeId)>,
        S: Into<String>,
    {
        Self::new(fields, None)
    }

    pub fn open<I, S>(fields: I, tail: RowId) -> Result<Self, BuildError>
    where
        I: IntoIterator<Item = (S, TypeId)>,
        S: Into<String>,
    {
        Self::new(fields, Some(tail))
    }

    fn new<I, S>(fields: I, tail: Option<RowId>) -> Result<Self, BuildError>
    where
        I: IntoIterator<Item = (S, TypeId)>,
        S: Into<String>,
    {
        let mut seen = BTreeSet::new();
        let mut fields = fields
            .into_iter()
            .map(|(label, ty)| (label.into(), ty))
            .collect::<Vec<_>>();
        for (label, _) in &fields {
            if label.is_empty() {
                return Err(BuildError::EmptyLabel);
            }
            if !seen.insert(label.clone()) {
                return Err(BuildError::DuplicateLabel(label.clone()));
            }
        }
        fields.sort_by(|left, right| left.0.cmp(&right.0));
        Ok(Self { fields, tail })
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) enum Node {
    Meta(crate::MetaId),
    Bound(u32),
    Primitive(Primitive),
    Tuple(Vec<TypeId>),
    List(TypeId),
    Option(TypeId),
    Function {
        parameters: Vec<TypeId>,
        result: TypeId,
    },
    Record(RowSpec),
    Variant(RowSpec),
    Mu(TypeId),
}

#[derive(Clone, Debug)]
pub struct TypeStore {
    pub(crate) scope: u64,
    pub(crate) nodes: Vec<Node>,
    pub(crate) required_depth: Vec<u32>,
    pub(crate) rows: u32,
}

impl TypeStore {
    pub fn revision(&self) -> u64 {
        self.scope
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    pub fn fingerprint(&self, root: TypeId) -> Result<String, BuildError> {
        self.validate_closed(root)?;
        Ok(self.render(root))
    }

    pub(crate) fn validate(&self, id: TypeId) -> Result<(), BuildError> {
        if id.scope != self.scope {
            return Err(BuildError::CrossStore);
        }
        if id.slot as usize >= self.nodes.len() {
            return Err(BuildError::DanglingType);
        }
        Ok(())
    }

    pub(crate) fn validate_closed(&self, id: TypeId) -> Result<(), BuildError> {
        self.validate(id)?;
        if self.required_depth[id.slot as usize] != 0 {
            return Err(BuildError::BinderEscapes);
        }
        Ok(())
    }

    pub(crate) fn node(&self, id: TypeId) -> &Node {
        &self.nodes[id.slot as usize]
    }

    pub(crate) fn render(&self, root: TypeId) -> String {
        self.render_at(root)
    }

    fn render_at(&self, id: TypeId) -> String {
        match self.node(id) {
            Node::Meta(meta) => format!("?{}", meta.slot),
            Node::Bound(index) => format!("^{index}"),
            Node::Primitive(primitive) => format!("{primitive:?}"),
            Node::Tuple(items) => format!(
                "({})",
                items
                    .iter()
                    .map(|item| self.render_at(*item))
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            Node::List(item) => format!("List<{}>", self.render_at(*item)),
            Node::Option(item) => format!("Option<{}>", self.render_at(*item)),
            Node::Function { parameters, result } => format!(
                "fn({})->{}",
                parameters
                    .iter()
                    .map(|item| self.render_at(*item))
                    .collect::<Vec<_>>()
                    .join(","),
                self.render_at(*result)
            ),
            Node::Record(row) => self.render_row("record", row),
            Node::Variant(row) => self.render_row("variant", row),
            Node::Mu(body) => format!("mu.{}", self.render_at(*body)),
        }
    }

    fn render_row(&self, kind: &str, row: &RowSpec) -> String {
        let fields = row
            .fields
            .iter()
            .map(|(label, ty)| format!("{label}:{}", self.render_at(*ty)))
            .collect::<Vec<_>>()
            .join(",");
        let tail = row
            .tail
            .map_or_else(|| "closed".to_owned(), |row| format!("?r{}", row.slot));
        format!("{kind}{{{fields}|{tail}}}")
    }
}
