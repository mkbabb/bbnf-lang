//! Tranche AA.1 — hash-consed interning for `TypeDesc`.
//!
//! Before AA.1 the type projection CSP held `Option<TypeDesc>` on every
//! variable's lattice domain, so `LatticeDomain::join` cloned deep trees
//! (`TypeDesc::Tuple(Vec<TypeDesc>)` recurses) on every convergence step
//! and `Option<TypeDesc>::clone` was ~5-8 % of `compile_css_l4`'s
//! `project_types` self-time.
//!
//! `TypeDescInterner` is a standard hash-cons: insertion-ordered
//! `Vec<TypeDesc>` plus a `FxHashMap<TypeDesc, TypeDescId>` lookup.
//! Every distinct structural shape maps to a single `u32` id, so
//! equality check collapses to a Copy compare and the CSP lattice
//! domain carries only `Option<TypeDescId>`.
//!
//! The interner sits on `GrammarIR` as `type_desc_interner` so it
//! survives past the CSP pass into any downstream consumer that needs
//! stable type identity (AA.5's `DispatchSignature`, AA.7's
//! `TaggedUnion` narrowing, and AA.15's `TapeView` generator will all
//! consume the same ids).

use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

use super::TypeDesc;

/// Stable, `Copy` identity for a `TypeDesc` structural shape.
///
/// Two `TypeDescId`s compare equal iff they were interned from
/// structurally equal `TypeDesc` values by the same interner. Cross-
/// interner comparison is not supported and not needed — every
/// compile owns one interner on `GrammarIR`.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TypeDescId(pub u32);

/// Hash-cons interner. `intern` is idempotent; `resolve` is O(1) array
/// indexing.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct TypeDescInterner {
    /// The canonical storage — stored in insertion order. `TypeDescId(i)`
    /// maps to `storage[i]`.
    storage: Vec<TypeDesc>,
    /// Lookup to make `intern` idempotent. Not serialized: rebuilt on
    /// load from `storage` to keep the wire format compact and avoid
    /// serializing the hash state.
    #[serde(skip)]
    lookup: FxHashMap<TypeDesc, TypeDescId>,
}

impl TypeDescInterner {
    /// Construct an empty interner.
    pub fn new() -> Self {
        Self::default()
    }

    /// Intern a `TypeDesc`, returning its stable id. Idempotent:
    /// repeated calls with structurally-equal inputs return the same id.
    pub fn intern(&mut self, ty: TypeDesc) -> TypeDescId {
        if let Some(&id) = self.lookup.get(&ty) {
            return id;
        }
        let id = TypeDescId(self.storage.len() as u32);
        self.storage.push(ty.clone());
        self.lookup.insert(ty, id);
        id
    }

    /// Resolve an id back to its canonical `TypeDesc`. Panics on
    /// out-of-range ids; the caller should never construct ids by hand.
    #[inline]
    pub fn resolve(&self, id: TypeDescId) -> &TypeDesc {
        &self.storage[id.0 as usize]
    }

    /// Get a `TypeDesc` value by its id, cloning out of the interner.
    /// Prefer `resolve` (borrow) whenever possible — cloning defeats
    /// the interner's entire purpose on the hot path.
    pub fn get_cloned(&self, id: TypeDescId) -> TypeDesc {
        self.storage[id.0 as usize].clone()
    }

    /// Number of distinct `TypeDesc` values interned so far.
    pub fn len(&self) -> usize {
        self.storage.len()
    }

    /// Is the interner empty?
    pub fn is_empty(&self) -> bool {
        self.storage.is_empty()
    }

    /// Rebuild the `lookup` map after deserialization. Called by the
    /// custom `Deserialize` implementation (invoked via `serde`
    /// attributes on `GrammarIR`).
    pub fn rebuild_lookup(&mut self) {
        self.lookup.clear();
        self.lookup.reserve(self.storage.len());
        for (i, ty) in self.storage.iter().enumerate() {
            self.lookup.insert(ty.clone(), TypeDescId(i as u32));
        }
    }
}
