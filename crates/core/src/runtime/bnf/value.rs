//! AZ-II.cutover.E (Phase 2) — Bnf typed value sum.
//!
//! See `crates/core/src/runtime/csv/value.rs` for the discipline. The
//! Bnf grammar projects every leaf (terminal / regex) to `Span` and
//! every named compound rule to a `Compound` arena handle. The
//! compound discriminator lives on
//! [`crate::runtime::bnf::BnfCompoundKind`].

use crate::runtime::bnf::arena::BnfCompoundId;

/// A Bnf AST value.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BnfValue<'p> {
    Span(&'p str),
    Unit,
    Compound(BnfCompoundId),
}

impl<'p> Default for BnfValue<'p> {
    fn default() -> Self {
        BnfValue::Unit
    }
}
