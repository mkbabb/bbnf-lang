use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TypeId(pub u32);

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TypeVarId(pub u32);

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct RowVarId(pub u32);

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Primitive {
    Bool,
    Bytes,
    Int,
    Str,
    Unit,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum RowTail {
    Closed,
    Open(RowVarId),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Row {
    pub fields: BTreeMap<String, TypeId>,
    pub tail: RowTail,
}

impl Row {
    pub fn closed(fields: impl IntoIterator<Item = (impl Into<String>, TypeId)>) -> Self {
        Self::new(fields, RowTail::Closed)
    }

    pub fn open(
        fields: impl IntoIterator<Item = (impl Into<String>, TypeId)>,
        tail: RowVarId,
    ) -> Self {
        Self::new(fields, RowTail::Open(tail))
    }

    pub fn new(
        fields: impl IntoIterator<Item = (impl Into<String>, TypeId)>,
        tail: RowTail,
    ) -> Self {
        Self {
            fields: fields
                .into_iter()
                .map(|(name, ty)| (name.into(), ty))
                .collect(),
            tail,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeTerm {
    Var(TypeVarId),
    Primitive(Primitive),
    Tuple(Vec<TypeId>),
    List(TypeId),
    Option(TypeId),
    Function {
        parameters: Vec<TypeId>,
        result: TypeId,
    },
    Record(Row),
    Variant(Row),
    Mu {
        binder: TypeVarId,
        body: TypeId,
    },
}

impl TypeTerm {
    pub(crate) fn constructor(&self) -> &'static str {
        match self {
            Self::Var(_) => "variable",
            Self::Primitive(_) => "primitive",
            Self::Tuple(_) => "tuple",
            Self::List(_) => "list",
            Self::Option(_) => "option",
            Self::Function { .. } => "function",
            Self::Record(_) => "record",
            Self::Variant(_) => "variant",
            Self::Mu { .. } => "mu",
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeStore {
    terms: Vec<TypeTerm>,
    next_type_variable: u32,
    next_row_variable: u32,
}

impl Default for TypeStore {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeStore {
    pub fn new() -> Self {
        Self {
            terms: Vec::new(),
            next_type_variable: 0,
            next_row_variable: 0,
        }
    }

    pub fn term(&self, id: TypeId) -> &TypeTerm {
        &self.terms[id.0 as usize]
    }

    pub fn insert(&mut self, term: TypeTerm) -> TypeId {
        let id = TypeId(self.terms.len() as u32);
        self.terms.push(term);
        id
    }

    pub fn fresh_type_variable(&mut self) -> (TypeVarId, TypeId) {
        let variable = TypeVarId(self.next_type_variable);
        self.next_type_variable += 1;
        let ty = self.insert(TypeTerm::Var(variable));
        (variable, ty)
    }

    pub fn fresh_row_variable(&mut self) -> RowVarId {
        let variable = RowVarId(self.next_row_variable);
        self.next_row_variable += 1;
        variable
    }

    pub fn primitive(&mut self, primitive: Primitive) -> TypeId {
        self.insert(TypeTerm::Primitive(primitive))
    }

    pub fn tuple(&mut self, elements: impl IntoIterator<Item = TypeId>) -> TypeId {
        self.insert(TypeTerm::Tuple(elements.into_iter().collect()))
    }

    pub fn list(&mut self, element: TypeId) -> TypeId {
        self.insert(TypeTerm::List(element))
    }

    pub fn option(&mut self, element: TypeId) -> TypeId {
        self.insert(TypeTerm::Option(element))
    }

    pub fn function(
        &mut self,
        parameters: impl IntoIterator<Item = TypeId>,
        result: TypeId,
    ) -> TypeId {
        self.insert(TypeTerm::Function {
            parameters: parameters.into_iter().collect(),
            result,
        })
    }

    pub fn record(&mut self, row: Row) -> TypeId {
        self.insert(TypeTerm::Record(row))
    }

    pub fn variant(&mut self, row: Row) -> TypeId {
        self.insert(TypeTerm::Variant(row))
    }

    pub fn mu(&mut self, binder: TypeVarId, body: TypeId) -> TypeId {
        self.insert(TypeTerm::Mu { binder, body })
    }

    pub fn fingerprint(&self, root: TypeId) -> String {
        self.fingerprint_with(root, &BTreeMap::new(), &mut BTreeSet::new())
    }

    fn fingerprint_with(
        &self,
        id: TypeId,
        bound: &BTreeMap<TypeVarId, usize>,
        visiting: &mut BTreeSet<TypeId>,
    ) -> String {
        if !visiting.insert(id) {
            return format!("#{}", id.0);
        }
        let rendered = match self.term(id) {
            TypeTerm::Var(variable) => bound
                .get(variable)
                .map_or_else(|| format!("t{}", variable.0), |depth| format!("^{depth}")),
            TypeTerm::Primitive(primitive) => format!("{primitive:?}"),
            TypeTerm::Tuple(elements) => format!(
                "({})",
                elements
                    .iter()
                    .map(|element| self.fingerprint_with(*element, bound, visiting))
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            TypeTerm::List(element) => {
                format!("List<{}>", self.fingerprint_with(*element, bound, visiting))
            }
            TypeTerm::Option(element) => format!(
                "Option<{}>",
                self.fingerprint_with(*element, bound, visiting)
            ),
            TypeTerm::Function { parameters, result } => format!(
                "fn({})->{}",
                parameters
                    .iter()
                    .map(|parameter| self.fingerprint_with(*parameter, bound, visiting))
                    .collect::<Vec<_>>()
                    .join(","),
                self.fingerprint_with(*result, bound, visiting)
            ),
            TypeTerm::Record(row) => self.fingerprint_row("record", row, bound, visiting),
            TypeTerm::Variant(row) => self.fingerprint_row("variant", row, bound, visiting),
            TypeTerm::Mu { binder, body } => {
                let mut nested = bound.clone();
                nested.insert(*binder, bound.len());
                format!("mu.{}", self.fingerprint_with(*body, &nested, visiting))
            }
        };
        visiting.remove(&id);
        rendered
    }

    fn fingerprint_row(
        &self,
        constructor: &str,
        row: &Row,
        bound: &BTreeMap<TypeVarId, usize>,
        visiting: &mut BTreeSet<TypeId>,
    ) -> String {
        let fields = row
            .fields
            .iter()
            .map(|(name, ty)| format!("{name}:{}", self.fingerprint_with(*ty, bound, visiting)))
            .collect::<Vec<_>>()
            .join(",");
        let tail = match row.tail {
            RowTail::Closed => "closed".to_owned(),
            RowTail::Open(variable) => format!("r{}", variable.0),
        };
        format!("{constructor}{{{fields}|{tail}}}")
    }
}
