//! AZ-II.cutover.E (Phase 2) — CssPretty typed value sum.
//!
//! See `crates/core/src/runtime/csv/value.rs` for the discipline. The
//! CssPretty grammar projects every leaf (terminal / regex) to `Span` and
//! every named compound rule to a `Compound` arena handle. The
//! compound discriminator lives on
//! [`crate::runtime::css_pretty::CssPrettyCompoundKind`].

use crate::runtime::css_pretty::arena::CssPrettyCompoundId;

/// A CssPretty AST value.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CssPrettyValue<'p> {
    Span(&'p str),
    Unit,
    Compound(CssPrettyCompoundId),
}

impl<'p> Default for CssPrettyValue<'p> {
    fn default() -> Self {
        CssPrettyValue::Unit
    }
}
