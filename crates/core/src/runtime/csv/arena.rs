//! AZ-II.cutover.E (Phase 2) — CSV parse arena.
//!
//! The arena owns every compound child slice ([`CsvValue`] elements
//! across the `record` and `csv` compound rules). Compounds reference
//! their children via opaque [`CsvCompoundId`] handles that resolve to
//! slices through [`CsvArena::compound`].
//!
//! Mirrors [`crate::runtime::bbnf::BbnfArena`] and
//! [`crate::runtime::google_sheets::SheetsArena`] discipline: a slab-
//! of-Vec model with one inner `Vec<CsvValue>` per non-empty compound.
//! Empty compounds resolve to `&[]` without allocating a slab entry —
//! the empty-handle constant ([`CsvCompoundId::EMPTY`]) carries the
//! discriminator for a zero-cost empty-resolution branch.

use crate::runtime::csv::value::CsvValue;

/// Discriminator — the structural shape of a [`CsvValue::Compound`].
///
/// One arm per compound rule in `grammar/misc/csv.bbnf`. Consumers
/// walking the CSV AST match on `kind()` to distinguish a `record`
/// (a row of cell values) from the top-level `csv` (a rows-of-records
/// sequence) without re-parsing the source.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CsvCompoundKind {
    /// `record = field, ( "," >> field ) *` — one row of CSV cells.
    Record,
    /// `csv = record, ( /\r?\n/ >> record ) *` — the document root,
    /// a sequence of records.
    Csv,
    /// `field = escaped | textdata` — Alt rule that under projection
    /// collapses to its chosen leaf's [`CsvValue::Span`]; the
    /// compound arm only surfaces when the structural builder admits
    /// the Alt outer wrapper before its leaf payload finalises. Held
    /// for exhaustiveness with the registry's classification.
    Field,
    /// Catch-all for compound rules not recognised by the
    /// [`CsvCompoundKind`] alphabet — the layout-resolver consults
    /// the rule name when [`crate::runtime::csv::CsvStructBuilder::begin_compound`]
    /// admits an entry; unrecognised rule names land on `Other` so
    /// the runtime stays exhaustive without panicking.
    Other,
}

impl CsvCompoundKind {
    /// Resolve a rule name (from
    /// [`bbnf_ir::registry::StructLayout::rule_name`]) to a kind.
    /// Names not in the alphabet collapse to [`Self::Other`].
    pub fn from_rule_name(name: &str) -> Self {
        match name {
            "record" => Self::Record,
            "csv" => Self::Csv,
            "field" => Self::Field,
            _ => Self::Other,
        }
    }
}

/// A compound entry in the arena — child slice plus structural
/// kind discriminator and optional Alt branch tag.
#[derive(Debug, Clone)]
pub struct CsvCompound<'p> {
    /// Structural shape of this compound (which rule emitted it).
    pub kind: CsvCompoundKind,
    /// Alt sub-variant index, when the rule is Alt-typed; `None`
    /// otherwise. Recorded via
    /// [`crate::runtime::builder::StructBuilder::push_branch_tag`].
    pub branch_tag: Option<u32>,
    /// Child values, in source order.
    pub children: Vec<CsvValue<'p>>,
}

impl<'p> Default for CsvCompound<'p> {
    fn default() -> Self {
        Self {
            kind: CsvCompoundKind::Other,
            branch_tag: None,
            children: Vec::new(),
        }
    }
}

/// Opaque handle for a compound entry.
///
/// The `0` value is reserved for [`Self::EMPTY`]; non-empty handles
/// take values `1..=u32::MAX`. Resolved via [`CsvArena::compound`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CsvCompoundId(u32);

impl CsvCompoundId {
    /// The empty-compound handle. Resolves to a default
    /// [`CsvCompound`] with no children.
    pub const EMPTY: Self = Self(0);

    /// True iff this is the empty-compound handle.
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

/// Owning slab for compound entries.
///
/// Owns one [`CsvCompound`] per non-empty handle; resolves handles
/// to compound entries via [`Self::compound`]. The empty-handle
/// constant ([`CsvCompoundId::EMPTY`]) routes to a stable default
/// without consulting the slab.
#[derive(Debug, Default)]
pub struct CsvArena<'p> {
    /// Per-handle compound entries. Index = `CsvCompoundId.0 - 1`.
    compounds: Vec<CsvCompound<'p>>,
    /// Stable default for the empty-compound resolution branch.
    empty: CsvCompound<'p>,
}

impl<'p> CsvArena<'p> {
    /// Construct an empty arena.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Construct an arena with pre-sized slab capacity.
    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            compounds: Vec::with_capacity(compounds),
            empty: CsvCompound::default(),
        }
    }

    /// Push a populated compound entry into the slab and return the
    /// resolving handle.
    #[inline]
    pub fn push_compound(&mut self, compound: CsvCompound<'p>) -> CsvCompoundId {
        self.compounds.push(compound);
        let idx = self.compounds.len() as u32;
        CsvCompoundId(idx)
    }

    /// Resolve a compound handle to its entry. Returns a stable
    /// default (`Other` kind, no children) for [`CsvCompoundId::EMPTY`].
    #[inline]
    pub fn compound(&self, id: CsvCompoundId) -> &CsvCompound<'p> {
        match id.slab_index() {
            None => &self.empty,
            Some(i) => &self.compounds[i],
        }
    }

    /// Number of registered compounds.
    #[inline]
    pub fn compound_count(&self) -> usize {
        self.compounds.len()
    }

    /// Roll back the arena to a prior compound-count snapshot.
    #[inline]
    pub fn truncate(&mut self, compounds: usize) {
        self.compounds.truncate(compounds);
    }
}
