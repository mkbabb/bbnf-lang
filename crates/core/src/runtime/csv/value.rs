//! AZ-II.cutover.E (Phase 2) — CSV typed value sum.
//!
//! The CSV grammar (`grammar/misc/csv.bbnf`) carries five rules:
//!
//! ```text
//! DQUOTE   = "\""                                  // literal-only, structural
//! escaped  = DQUOTE >> /[^"]*/ << DQUOTE           -> Span (the quoted body)
//! textdata = /[^,"\r\n]+/                          -> Span (raw cell text)
//! field    = escaped | textdata                    -> the chosen leaf's Span
//! record   = field, ( "," >> field ) *             // compound: child slice of fields
//! csv      = record, ( /\r?\n/ >> record ) *       // compound: child slice of records
//! ```
//!
//! Three observable value shapes:
//!
//! - `CsvValue::Span` — a single borrowed cell text slice (escaped or textdata
//!   leaf, after the `field` Alt has selected one branch).
//! - `CsvValue::Compound` — a record (a row of fields) or the document
//!   (a rows-of-records sequence). Compound discrimination lives on
//!   [`crate::runtime::csv::CsvCompoundKind`].
//! - `CsvValue::Unit` — admitted-but-payload-less placeholder for any
//!   admitted Alt branch whose typed projection yielded `()`.
//!
//! `CsvValue` is `Copy` to mirror [`crate::runtime::json::JsonValue`] —
//! every interior string borrows from the input lifetime `'p`; the
//! `Compound` arm carries an opaque arena handle.

use crate::runtime::csv::arena::CsvCompoundId;

/// A CSV AST value — the closed sum of every typed projection the
/// grammar emits.
///
/// # Variant choice
///
/// Per the per-grammar discipline (BBNF / Sheets / JSON parallel),
/// every CSV compound rule (`record`, `csv`) collapses into the
/// unitary [`CsvValue::Compound`] arm; the rule-level shape lives on
/// the compound's [`crate::runtime::csv::CsvCompoundKind`]
/// discriminator.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CsvValue<'p> {
    /// `escaped` / `textdata` / `field` — borrowed source slice.
    Span(&'p str),
    /// `()` — unit-typed leaf (matches `push_leaf_with_unit`); reserved
    /// for any future Alt-tag projection that admits but carries no
    /// payload byte.
    Unit,
    /// `record` / `csv` — compound rule. The handle resolves through
    /// [`crate::runtime::csv::CsvArena`] to the child slice.
    Compound(CsvCompoundId),
}

impl<'p> Default for CsvValue<'p> {
    fn default() -> Self {
        CsvValue::Unit
    }
}
