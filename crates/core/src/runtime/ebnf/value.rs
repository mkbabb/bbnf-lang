//! AZ-II.cutover.E (Phase 2) — Ebnf typed value sum.
//!
//! See `crates/core/src/runtime/csv/value.rs` for the discipline. The
//! Ebnf grammar projects every leaf (terminal / regex) to `Span` and
//! every named compound rule to a `Compound` arena handle. The
//! compound discriminator lives on
//! [`crate::runtime::ebnf::EbnfCompoundKind`].

use crate::runtime::ebnf::arena::EbnfCompoundId;

/// A Ebnf AST value.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EbnfValue<'p> {
    Span(&'p str),
    Unit,
    Compound(EbnfCompoundId),
}

impl<'p> Default for EbnfValue<'p> {
    fn default() -> Self {
        EbnfValue::Unit
    }
}
