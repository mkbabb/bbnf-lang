//! Immutable semantic terms and explicit recursive identities.

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt;
use std::sync::atomic::{AtomicU64, Ordering};

use super::{OperationContext, Origin, TypeError};

static NEXT_SCOPE: AtomicU64 = AtomicU64::new(1);

macro_rules! scoped_id {
    ($name:ident) => {
        #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub struct $name {
            pub(crate) scope: u64,
            pub(crate) slot: u32,
        }

        impl $name {
            #[must_use]
            pub fn ordinal(self) -> u32 {
                self.slot
            }
        }
    };
}

scoped_id!(TypeId);
scoped_id!(MetaId);
scoped_id!(RowId);
scoped_id!(RecVar);

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Primitive {
    Bool,
    Bytes,
    Diagnostic,
    F64,
    I8,
    I16,
    I32,
    I64,
    ParseResult,
    Span,
    Str,
    U8,
    U16,
    U32,
    U64,
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
        let mut fields = fields
            .into_iter()
            .map(|(label, ty)| (label.into(), ty))
            .collect::<Vec<_>>();
        fields.sort_by(|left, right| left.0.cmp(&right.0));
        for window in fields.windows(2) {
            if window[0].0 == window[1].0 {
                return Err(BuildError::new(
                    BuildErrorKind::DuplicateLabel(window[0].0.clone()),
                    "store.row",
                ));
            }
        }
        if let Some((label, _)) = fields.iter().find(|(label, _)| label.is_empty()) {
            return Err(BuildError::new(
                BuildErrorKind::EmptyLabel,
                format!("store.row[{label}]"),
            ));
        }
        Ok(Self { fields, tail })
    }

    #[must_use]
    pub fn fields(&self) -> &[(String, TypeId)] {
        &self.fields
    }

    #[must_use]
    pub fn tail(&self) -> Option<RowId> {
        self.tail
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) enum Node {
    Meta(MetaId),
    Rec(RecVar),
    Primitive(Primitive),
    Named(String),
    Tuple(Vec<TypeId>),
    Join(Vec<TypeId>),
    List(TypeId),
    Option(TypeId),
    Function {
        parameters: Vec<TypeId>,
        result: TypeId,
    },
    Record(RowSpec),
    Variant(RowSpec),
    Mu {
        binder: RecVar,
        body: TypeId,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BuildErrorKind {
    CrossStore,
    DanglingType,
    DanglingRow,
    DanglingRecVar,
    DuplicateLabel(String),
    EmptyLabel,
    TooManyTerms,
    VacuousRecursion(RecVar),
    EscapingRecursion(Vec<RecVar>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BuildError {
    kind: BuildErrorKind,
    site: String,
}

impl BuildError {
    fn new(kind: BuildErrorKind, site: impl Into<String>) -> Self {
        Self {
            kind,
            site: site.into(),
        }
    }

    #[must_use]
    pub fn kind(&self) -> &BuildErrorKind {
        &self.kind
    }

    #[must_use]
    pub fn site(&self) -> &str {
        &self.site
    }
}

impl fmt::Display for BuildError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{:?} at {}", self.kind, self.site)
    }
}

impl std::error::Error for BuildError {}

#[derive(Clone, Debug)]
pub struct TypeStore {
    pub(crate) scope: u64,
    pub(crate) nodes: Vec<Node>,
    pub(crate) meta_levels: Vec<u32>,
    pub(crate) row_levels: Vec<u32>,
}

impl TypeStore {
    #[must_use]
    pub fn revision(&self) -> u64 {
        self.scope
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    pub fn fingerprint(
        &self,
        root: TypeId,
        operations: &mut OperationContext,
    ) -> Result<String, TypeError> {
        if root.scope != self.scope || root.slot as usize >= self.nodes.len() {
            return Err(operations.fail(
                super::TypeErrorKind::DanglingType,
                &[],
                "store.fingerprint",
            ));
        }
        let mut binders = BTreeMap::new();
        let mut next_binder = 0_u32;
        self.render(root, &mut binders, &mut next_binder, operations)
    }

    fn render(
        &self,
        id: TypeId,
        binders: &mut BTreeMap<RecVar, u32>,
        next_binder: &mut u32,
        operations: &mut OperationContext,
    ) -> Result<String, TypeError> {
        operations.step(
            "store.fingerprint.node",
            &[],
            [Origin::new(
                "store.fingerprint",
                format!("type {}", id.slot),
            )],
        )?;
        let node = self.nodes.get(id.slot as usize).ok_or_else(|| {
            operations.fail(super::TypeErrorKind::DanglingType, &[], "store.fingerprint")
        })?;
        match node {
            Node::Meta(meta) => Ok(format!(
                "?{}@{}",
                meta.slot, self.meta_levels[meta.slot as usize]
            )),
            Node::Rec(rec) => Ok(binders.get(rec).map_or_else(
                || format!("free-rec-{}", rec.slot),
                |index| format!("^{index}"),
            )),
            Node::Primitive(primitive) => Ok(format!("{primitive:?}")),
            Node::Named(name) => Ok(format!("named({name:?})")),
            Node::Tuple(items) => {
                let mut rendered = Vec::with_capacity(items.len());
                for item in items {
                    rendered.push(self.render(*item, binders, next_binder, operations)?);
                }
                Ok(format!("({})", rendered.join(",")))
            }
            Node::Join(items) => {
                let mut rendered = Vec::with_capacity(items.len());
                for item in items {
                    rendered.push(self.render(*item, binders, next_binder, operations)?);
                }
                Ok(format!("join({})", rendered.join(",")))
            }
            Node::List(item) => Ok(format!(
                "List<{}>",
                self.render(*item, binders, next_binder, operations)?
            )),
            Node::Option(item) => Ok(format!(
                "Option<{}>",
                self.render(*item, binders, next_binder, operations)?
            )),
            Node::Function { parameters, result } => {
                let mut rendered = Vec::with_capacity(parameters.len());
                for parameter in parameters {
                    rendered.push(self.render(*parameter, binders, next_binder, operations)?);
                }
                Ok(format!(
                    "fn({})->{}",
                    rendered.join(","),
                    self.render(*result, binders, next_binder, operations)?
                ))
            }
            Node::Record(row) => self.render_row("record", row, binders, next_binder, operations),
            Node::Variant(row) => self.render_row("variant", row, binders, next_binder, operations),
            Node::Mu { binder, body } => {
                let canonical = *next_binder;
                *next_binder = next_binder.saturating_add(1);
                let previous = binders.insert(*binder, canonical);
                let body = self.render(*body, binders, next_binder, operations)?;
                if let Some(previous) = previous {
                    binders.insert(*binder, previous);
                } else {
                    binders.remove(binder);
                }
                Ok(format!("mu{canonical}.{body}"))
            }
        }
    }

    fn render_row(
        &self,
        kind: &str,
        row: &RowSpec,
        binders: &mut BTreeMap<RecVar, u32>,
        next_binder: &mut u32,
        operations: &mut OperationContext,
    ) -> Result<String, TypeError> {
        let mut fields = Vec::with_capacity(row.fields.len());
        for (label, ty) in &row.fields {
            fields.push(format!(
                "{label}:{}",
                self.render(*ty, binders, next_binder, operations)?
            ));
        }
        let tail = row
            .tail
            .map_or_else(|| "closed".to_owned(), |tail| format!("?r{}", tail.slot));
        Ok(format!("{kind}{{{}|{tail}}}", fields.join(",")))
    }

    pub(crate) fn node(&self, id: TypeId) -> Option<&Node> {
        (id.scope == self.scope)
            .then(|| self.nodes.get(id.slot as usize))
            .flatten()
    }

    pub(crate) fn meta_count(&self) -> usize {
        self.meta_levels.len()
    }

    pub(crate) fn row_count(&self) -> usize {
        self.row_levels.len()
    }
}

pub struct StoreBuilder {
    scope: u64,
    nodes: Vec<Node>,
    free_rec: Vec<BTreeSet<RecVar>>,
    interned: HashMap<Node, TypeId>,
    meta_levels: Vec<u32>,
    row_levels: Vec<u32>,
    rec_vars: u32,
}

impl Default for StoreBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl StoreBuilder {
    #[must_use]
    pub fn new() -> Self {
        Self {
            scope: NEXT_SCOPE.fetch_add(1, Ordering::Relaxed),
            nodes: Vec::new(),
            free_rec: Vec::new(),
            interned: HashMap::new(),
            meta_levels: Vec::new(),
            row_levels: Vec::new(),
            rec_vars: 0,
        }
    }

    #[must_use]
    pub fn revision(&self) -> u64 {
        self.scope
    }

    pub fn primitive(&mut self, primitive: Primitive) -> Result<TypeId, BuildError> {
        self.intern(Node::Primitive(primitive), BTreeSet::new())
    }

    pub fn named(&mut self, name: impl Into<String>) -> Result<TypeId, BuildError> {
        self.intern(Node::Named(name.into()), BTreeSet::new())
    }

    pub fn fresh_meta(&mut self, level: u32) -> Result<(MetaId, TypeId), BuildError> {
        let slot = u32::try_from(self.meta_levels.len())
            .map_err(|_| BuildError::new(BuildErrorKind::TooManyTerms, "store.fresh_meta"))?;
        let meta = MetaId {
            scope: self.scope,
            slot,
        };
        self.meta_levels.push(level);
        let ty = self.intern(Node::Meta(meta), BTreeSet::new())?;
        Ok((meta, ty))
    }

    pub fn fresh_row(&mut self, level: u32) -> Result<RowId, BuildError> {
        let slot = u32::try_from(self.row_levels.len())
            .map_err(|_| BuildError::new(BuildErrorKind::TooManyTerms, "store.fresh_row"))?;
        self.row_levels.push(level);
        Ok(RowId {
            scope: self.scope,
            slot,
        })
    }

    pub fn fresh_rec_var(&mut self) -> Result<RecVar, BuildError> {
        let slot = self.rec_vars;
        self.rec_vars = self
            .rec_vars
            .checked_add(1)
            .ok_or_else(|| BuildError::new(BuildErrorKind::TooManyTerms, "store.fresh_rec_var"))?;
        Ok(RecVar {
            scope: self.scope,
            slot,
        })
    }

    pub fn rec_var(&mut self, rec: RecVar) -> Result<TypeId, BuildError> {
        self.validate_rec(rec)?;
        self.intern(Node::Rec(rec), [rec].into_iter().collect())
    }

    pub fn tuple<I>(&mut self, items: I) -> Result<TypeId, BuildError>
    where
        I: IntoIterator<Item = TypeId>,
    {
        let items = items.into_iter().collect::<Vec<_>>();
        let free = self.children_free(&items)?;
        self.intern(Node::Tuple(items), free)
    }

    pub(crate) fn join<I>(&mut self, items: I) -> Result<TypeId, BuildError>
    where
        I: IntoIterator<Item = TypeId>,
    {
        let items = items.into_iter().collect::<Vec<_>>();
        let free = self.children_free(&items)?;
        self.intern(Node::Join(items), free)
    }

    pub fn list(&mut self, item: TypeId) -> Result<TypeId, BuildError> {
        let free = self.child_free(item)?;
        self.intern(Node::List(item), free)
    }

    pub fn option(&mut self, item: TypeId) -> Result<TypeId, BuildError> {
        let free = self.child_free(item)?;
        self.intern(Node::Option(item), free)
    }

    pub fn function<I>(&mut self, parameters: I, result: TypeId) -> Result<TypeId, BuildError>
    where
        I: IntoIterator<Item = TypeId>,
    {
        let parameters = parameters.into_iter().collect::<Vec<_>>();
        let mut children = parameters.clone();
        children.push(result);
        let free = self.children_free(&children)?;
        self.intern(Node::Function { parameters, result }, free)
    }

    pub fn record(&mut self, row: RowSpec) -> Result<TypeId, BuildError> {
        self.row(Node::Record(row))
    }

    pub fn variant(&mut self, row: RowSpec) -> Result<TypeId, BuildError> {
        self.row(Node::Variant(row))
    }

    /// Close an explicit recursion identity.  There is no mutable active-binder
    /// stack, so prebuilt outer references compose and unwinding cannot poison
    /// subsequent construction.
    pub fn mu(&mut self, binder: RecVar, body: TypeId) -> Result<TypeId, BuildError> {
        self.validate_rec(binder)?;
        let mut free = self.child_free(body)?;
        if !free.remove(&binder) {
            return Err(BuildError::new(
                BuildErrorKind::VacuousRecursion(binder),
                "store.mu",
            ));
        }
        self.intern(Node::Mu { binder, body }, free)
    }

    /// Seal an immutable store after proving every exported root is closed.
    pub fn finish<I>(self, roots: I) -> Result<TypeStore, BuildError>
    where
        I: IntoIterator<Item = TypeId>,
    {
        for root in roots {
            if root.scope != self.scope {
                return Err(BuildError::new(BuildErrorKind::CrossStore, "store.finish"));
            }
            let free = self
                .free_rec
                .get(root.slot as usize)
                .ok_or_else(|| BuildError::new(BuildErrorKind::DanglingType, "store.finish"))?;
            if !free.is_empty() {
                return Err(BuildError::new(
                    BuildErrorKind::EscapingRecursion(free.iter().copied().collect()),
                    "store.finish",
                ));
            }
        }
        Ok(TypeStore {
            scope: self.scope,
            nodes: self.nodes,
            meta_levels: self.meta_levels,
            row_levels: self.row_levels,
        })
    }

    fn row(&mut self, node: Node) -> Result<TypeId, BuildError> {
        let row = match &node {
            Node::Record(row) | Node::Variant(row) => row,
            _ => unreachable!("row builder only receives row nodes"),
        };
        if let Some(tail) = row.tail {
            self.validate_row(tail)?;
        }
        let free = self.children_free(&row.fields.iter().map(|(_, ty)| *ty).collect::<Vec<_>>())?;
        self.intern(node, free)
    }

    fn validate_rec(&self, rec: RecVar) -> Result<(), BuildError> {
        if rec.scope != self.scope {
            return Err(BuildError::new(BuildErrorKind::CrossStore, "store.rec_var"));
        }
        if rec.slot >= self.rec_vars {
            return Err(BuildError::new(
                BuildErrorKind::DanglingRecVar,
                "store.rec_var",
            ));
        }
        Ok(())
    }

    fn validate_row(&self, row: RowId) -> Result<(), BuildError> {
        if row.scope != self.scope {
            return Err(BuildError::new(BuildErrorKind::CrossStore, "store.row"));
        }
        if row.slot as usize >= self.row_levels.len() {
            return Err(BuildError::new(BuildErrorKind::DanglingRow, "store.row"));
        }
        Ok(())
    }

    fn child_free(&self, child: TypeId) -> Result<BTreeSet<RecVar>, BuildError> {
        if child.scope != self.scope {
            return Err(BuildError::new(BuildErrorKind::CrossStore, "store.child"));
        }
        self.free_rec
            .get(child.slot as usize)
            .cloned()
            .ok_or_else(|| BuildError::new(BuildErrorKind::DanglingType, "store.child"))
    }

    fn children_free(&self, children: &[TypeId]) -> Result<BTreeSet<RecVar>, BuildError> {
        let mut free = BTreeSet::new();
        for child in children {
            free.extend(self.child_free(*child)?);
        }
        Ok(free)
    }

    fn intern(&mut self, node: Node, free: BTreeSet<RecVar>) -> Result<TypeId, BuildError> {
        if let Some(id) = self.interned.get(&node) {
            return Ok(*id);
        }
        let slot = u32::try_from(self.nodes.len())
            .map_err(|_| BuildError::new(BuildErrorKind::TooManyTerms, "store.intern"))?;
        let id = TypeId {
            scope: self.scope,
            slot,
        };
        self.nodes.push(node.clone());
        self.free_rec.push(free);
        self.interned.insert(node, id);
        Ok(id)
    }
}
