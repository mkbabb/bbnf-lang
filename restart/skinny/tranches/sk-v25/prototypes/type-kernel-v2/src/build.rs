use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::term::Node;
use crate::{BuildError, MetaId, Primitive, RowId, RowSpec, TypeId, TypeStore};

static NEXT_REVISION: AtomicU64 = AtomicU64::new(1);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Binder {
    scope: u64,
    depth: u32,
    nonce: u32,
}

pub struct StoreBuilder {
    scope: u64,
    nodes: Vec<Node>,
    required_depth: Vec<u32>,
    interned: HashMap<Node, TypeId>,
    metas: u32,
    rows: u32,
    binders: Vec<Binder>,
    next_binder: u32,
}

impl Default for StoreBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl StoreBuilder {
    pub fn new() -> Self {
        let scope = NEXT_REVISION.fetch_add(1, Ordering::Relaxed);
        Self {
            scope,
            nodes: Vec::new(),
            required_depth: Vec::new(),
            interned: HashMap::new(),
            metas: 0,
            rows: 0,
            binders: Vec::new(),
            next_binder: 0,
        }
    }

    pub fn primitive(&mut self, primitive: Primitive) -> Result<TypeId, BuildError> {
        self.intern(Node::Primitive(primitive), 0)
    }

    pub fn fresh_meta(&mut self) -> Result<(MetaId, TypeId), BuildError> {
        let meta = MetaId {
            scope: self.scope,
            slot: self.metas,
        };
        self.metas = self.metas.checked_add(1).ok_or(BuildError::TooManyTerms)?;
        let ty = self.intern(Node::Meta(meta), 0)?;
        Ok((meta, ty))
    }

    pub fn fresh_row(&mut self) -> Result<RowId, BuildError> {
        let row = RowId {
            scope: self.scope,
            slot: self.rows,
        };
        self.rows = self.rows.checked_add(1).ok_or(BuildError::TooManyTerms)?;
        Ok(row)
    }

    pub fn tuple<I>(&mut self, items: I) -> Result<TypeId, BuildError>
    where
        I: IntoIterator<Item = TypeId>,
    {
        let items = items.into_iter().collect::<Vec<_>>();
        let depth = self.children_depth(&items)?;
        self.intern(Node::Tuple(items), depth)
    }

    pub fn list(&mut self, item: TypeId) -> Result<TypeId, BuildError> {
        self.child_depth(item)
            .and_then(|depth| self.intern(Node::List(item), depth))
    }

    pub fn option(&mut self, item: TypeId) -> Result<TypeId, BuildError> {
        self.child_depth(item)
            .and_then(|depth| self.intern(Node::Option(item), depth))
    }

    pub fn function<I>(&mut self, parameters: I, result: TypeId) -> Result<TypeId, BuildError>
    where
        I: IntoIterator<Item = TypeId>,
    {
        let parameters = parameters.into_iter().collect::<Vec<_>>();
        let mut children = parameters.clone();
        children.push(result);
        let depth = self.children_depth(&children)?;
        self.intern(Node::Function { parameters, result }, depth)
    }

    pub fn record(&mut self, row: RowSpec) -> Result<TypeId, BuildError> {
        self.row(Node::Record(row))
    }

    pub fn variant(&mut self, row: RowSpec) -> Result<TypeId, BuildError> {
        self.row(Node::Variant(row))
    }

    pub fn bound(&mut self, binder: Binder) -> Result<TypeId, BuildError> {
        let position = self
            .binders
            .iter()
            .position(|active| *active == binder)
            .ok_or(BuildError::BinderNotActive)?;
        if binder.scope != self.scope {
            return Err(BuildError::CrossStore);
        }
        let index = self.binders.len() - position - 1;
        let required = u32::try_from(index + 1).map_err(|_| BuildError::TooManyTerms)?;
        self.intern(Node::Bound(index as u32), required)
    }

    pub fn mu<F>(&mut self, body: F) -> Result<TypeId, BuildError>
    where
        F: FnOnce(&mut Self, Binder) -> Result<TypeId, BuildError>,
    {
        let binder = Binder {
            scope: self.scope,
            depth: self.binders.len() as u32,
            nonce: self.next_binder,
        };
        self.next_binder = self
            .next_binder
            .checked_add(1)
            .ok_or(BuildError::TooManyTerms)?;
        self.binders.push(binder);
        let body_result = body(self, binder);
        self.binders.pop();
        let body = body_result?;
        let required = self.child_depth(body)?;
        if required == 0 || required > binder.depth + 1 {
            return Err(BuildError::BinderEscapes);
        }
        self.intern(Node::Mu(body), required - 1)
    }

    pub fn finish(self) -> TypeStore {
        TypeStore {
            scope: self.scope,
            nodes: self.nodes,
            required_depth: self.required_depth,
            rows: self.rows,
        }
    }

    fn row(&mut self, node: Node) -> Result<TypeId, BuildError> {
        let row = match &node {
            Node::Record(row) | Node::Variant(row) => row,
            _ => return Err(BuildError::BuilderPoisoned),
        };
        if let Some(tail) = row.tail {
            self.validate_row(tail)?;
        }
        let depth =
            self.children_depth(&row.fields.iter().map(|field| field.1).collect::<Vec<_>>())?;
        self.intern(node, depth)
    }

    fn validate_row(&self, row: RowId) -> Result<(), BuildError> {
        if row.scope != self.scope {
            return Err(BuildError::CrossStore);
        }
        if row.slot >= self.rows {
            return Err(BuildError::DanglingRow);
        }
        Ok(())
    }

    fn child_depth(&self, child: TypeId) -> Result<u32, BuildError> {
        if child.scope != self.scope {
            return Err(BuildError::CrossStore);
        }
        self.required_depth
            .get(child.slot as usize)
            .copied()
            .ok_or(BuildError::DanglingType)
    }

    fn children_depth(&self, children: &[TypeId]) -> Result<u32, BuildError> {
        children
            .iter()
            .try_fold(0, |depth, child| Ok(depth.max(self.child_depth(*child)?)))
    }

    fn intern(&mut self, node: Node, required: u32) -> Result<TypeId, BuildError> {
        if required > self.binders.len() as u32 {
            return Err(BuildError::BinderEscapes);
        }
        if let Some(id) = self.interned.get(&node) {
            return Ok(*id);
        }
        let slot = u32::try_from(self.nodes.len()).map_err(|_| BuildError::TooManyTerms)?;
        let id = TypeId {
            scope: self.scope,
            slot,
        };
        self.nodes.push(node.clone());
        self.required_depth.push(required);
        self.interned.insert(node, id);
        Ok(id)
    }
}
