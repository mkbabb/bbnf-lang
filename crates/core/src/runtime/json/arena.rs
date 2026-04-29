//! AZ-I.W2.A — JSON parse arena.
//!
//! The arena owns every compound child slice ([`JsonValue`] in
//! arrays, [`JsonPair`] in objects). Compounds reference their
//! children via opaque [`JsonArrayId`] / [`JsonObjectId`] handles
//! that resolve to slices through [`JsonArena::array`] /
//! [`JsonArena::object`].
//!
//! # Allocation strategy
//!
//! A simple slab-of-Vec model: arrays and objects each carry a
//! `Vec<Vec<…>>` — outer index is the handle, inner Vec is the slice.
//! This is the simplest substrate that meets the
//! `feedback_no-orthogonal-codepaths` invariant (one collection
//! strategy, no conditional branching). Per-parse the slab gets
//! pre-sized off the [`bbnf_ir::registry::StructRegistry`]'s
//! capacity hints; in-place mutation through the open-frame stack
//! keeps each child push allocation-free except when a Vec spills
//! its capacity.
//!
//! `bumpalo` is a transitive dependency of `parse-that` already; the
//! Vec-backed slab keeps W2.A's surface lean — the arena's
//! representation is internal and a switch to a bump arena later is
//! a private refactor on this module that doesn't ripple beyond
//! [`crate::runtime::json::JsonStructBuilder`].

use crate::runtime::json::value::{JsonPair, JsonValue};

/// Opaque handle for an array compound's element slice.
///
/// The `0` value is reserved for [`Self::EMPTY`]; non-empty handles
/// take values `1..=u32::MAX`. Resolved via [`JsonArena::array`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct JsonArrayId(u32);

impl JsonArrayId {
    /// The empty-array handle. Resolves to `&[]` regardless of the
    /// owning arena's slab state — empty arrays do not allocate.
    pub const EMPTY: Self = Self(0);

    /// True iff this is the empty-array handle.
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    /// Index into the owning arena's slab, less one (since `0` is
    /// reserved for [`Self::EMPTY`]).
    #[inline]
    fn slab_index(self) -> Option<usize> {
        if self.0 == 0 {
            None
        } else {
            Some((self.0 - 1) as usize)
        }
    }
}

/// Opaque handle for an object compound's pair slice.
///
/// Identical discipline to [`JsonArrayId`]; resolved via
/// [`JsonArena::object`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct JsonObjectId(u32);

impl JsonObjectId {
    /// The empty-object handle. Resolves to `&[]`.
    pub const EMPTY: Self = Self(0);

    /// True iff this is the empty-object handle.
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    /// Index into the owning arena's slab, less one.
    #[inline]
    fn slab_index(self) -> Option<usize> {
        if self.0 == 0 {
            None
        } else {
            Some((self.0 - 1) as usize)
        }
    }
}

/// Owning slab for compound child slices.
///
/// Owns one Vec per non-empty array compound (resp. object compound).
/// Empty compounds resolve to `&[]` without consulting the slab —
/// the empty-handle constants ([`JsonArrayId::EMPTY`] /
/// [`JsonObjectId::EMPTY`]) carry the discriminator for a zero-cost
/// empty-resolution branch.
#[derive(Debug, Default)]
pub struct JsonArena<'p> {
    /// Per-array element slabs. Index = `JsonArrayId.0 - 1`.
    arrays: Vec<Vec<JsonValue<'p>>>,
    /// Per-object pair slabs. Index = `JsonObjectId.0 - 1`.
    objects: Vec<Vec<JsonPair<'p>>>,
}

impl<'p> JsonArena<'p> {
    /// Construct an empty arena. The typical caller is the generated
    /// parse function which then mutably borrows the arena to populate
    /// every compound during parsing.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Construct an arena with pre-sized slab capacities. Hints come
    /// from [`bbnf_ir::registry::StructRegistry`] capacity estimates
    /// the emitter folds into the parse function.
    #[inline]
    pub fn with_capacity(arrays: usize, objects: usize) -> Self {
        Self {
            arrays: Vec::with_capacity(arrays),
            objects: Vec::with_capacity(objects),
        }
    }

    /// Push a populated array element vector into the slab and return
    /// the resolving handle. Empty vectors return [`JsonArrayId::EMPTY`]
    /// without allocating a slab entry.
    #[inline]
    pub fn push_array(&mut self, items: Vec<JsonValue<'p>>) -> JsonArrayId {
        if items.is_empty() {
            return JsonArrayId::EMPTY;
        }
        self.arrays.push(items);
        // Indices start at 1 so 0 maps to EMPTY.
        let idx = self.arrays.len() as u32;
        JsonArrayId(idx)
    }

    /// Push a populated object pair vector into the slab. Empty
    /// vectors return [`JsonObjectId::EMPTY`].
    #[inline]
    pub fn push_object(&mut self, pairs: Vec<JsonPair<'p>>) -> JsonObjectId {
        if pairs.is_empty() {
            return JsonObjectId::EMPTY;
        }
        self.objects.push(pairs);
        let idx = self.objects.len() as u32;
        JsonObjectId(idx)
    }

    /// Resolve an array handle to its element slice. Returns `&[]`
    /// for [`JsonArrayId::EMPTY`].
    #[inline]
    pub fn array(&self, id: JsonArrayId) -> &[JsonValue<'p>] {
        match id.slab_index() {
            None => &[],
            Some(i) => self.arrays[i].as_slice(),
        }
    }

    /// Resolve an object handle to its pair slice. Returns `&[]`
    /// for [`JsonObjectId::EMPTY`].
    #[inline]
    pub fn object(&self, id: JsonObjectId) -> &[JsonPair<'p>] {
        match id.slab_index() {
            None => &[],
            Some(i) => self.objects[i].as_slice(),
        }
    }

    /// Number of registered (non-empty) arrays.
    #[inline]
    pub fn array_count(&self) -> usize {
        self.arrays.len()
    }

    /// Number of registered (non-empty) objects.
    #[inline]
    pub fn object_count(&self) -> usize {
        self.objects.len()
    }

    /// Roll back the arena to a prior slab-count snapshot.
    #[inline]
    pub fn truncate(&mut self, arrays: usize, objects: usize) {
        self.arrays.truncate(arrays);
        self.objects.truncate(objects);
    }
}
