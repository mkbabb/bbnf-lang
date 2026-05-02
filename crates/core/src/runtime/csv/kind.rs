//! AZ-IV.W5.3 — CSV compound kind discriminator + arena entry shape.
//!
//! Hosts the [`CsvCompoundKind`] enum (one variant per CSV grammar
//! compound rule) and the [`CsvCompound`] entry shape that the
//! [`crate::runtime::arena_template::CompoundSlabArena`] instantiation
//! stores.
//!
//! Pre-W5 these lived in `arena.rs` alongside the slab logic; the
//! W5.3 dedup moved the slab discipline onto the generic template,
//! leaving this file as the grammar-specific compound shape.

use bbnf_ir::registry::{StructLayout, StructRegistry};

use crate::runtime::csv::value::CsvValue;

/// Discriminator — the structural shape of a [`CsvValue::Compound`].
///
/// One arm per compound rule in `grammar/misc/csv.bbnf`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CsvCompoundKind {
    /// `record = field, ( "," >> field ) *` — one row of CSV cells.
    Record,
    /// `csv = record, ( /\r?\n/ >> record ) *` — the document root,
    /// a sequence of records.
    Csv,
    /// `field = escaped | textdata` — Alt rule that under projection
    /// collapses to its chosen leaf's [`CsvValue::Span`].
    Field,
    /// Catch-all for compound rules not recognised by the
    /// [`CsvCompoundKind`] alphabet.
    Other,
}

impl CsvCompoundKind {
    /// Resolve a [`StructLayout`] to its kind.
    pub fn from_layout(layout: &StructLayout) -> Self {
        match StructRegistry::compound_kind_for_layout(layout) {
            "record" => Self::Record,
            "csv" => Self::Csv,
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
    /// otherwise.
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
