//! AZ-II.cutover.E (Phase 2) — Math typed value sum.
//!
//! The math grammar (`grammar/misc/math.bbnf`) is the simplest non-
//! trivial fixture in the workspace: arithmetic with parenthesised
//! sub-expressions, no typed projections at all. Every rule projects
//! to either a borrowed `Span` (the `number` regex leaf) or a
//! `Compound` (the `expr` / `term` / `factor` / `wrapped` /
//! parenthesis-tier rules).
//!
//! ```text
//! expr     = term, { ("+" | "-"), term }      // compound
//! term     = factor, { ("*" | "/"), factor }  // compound
//! p        = "("                              // structural literal
//! pp       = p                                // alias chain
//! ppp      = pp
//! pppp     = ppp
//! ppppp    = pppp
//! pppppp   = ppppp
//! wrapped  = pppppp, expr, ")"                // compound
//! factor   = number | wrapped                 // Alt
//! number   = /(\d+)?(\.\d+)?([eE][-+]?\d+)?/  // Span leaf (default)
//! ```
//!
//! `MathValue` is `Copy` — every interior string borrows from the
//! input lifetime `'p`; the `Compound` arm carries an opaque arena
//! handle. Mirror of `JsonValue` / `BbnfValue` / `CsvValue`.

use crate::runtime::math::arena::MathCompoundId;

/// A math AST value.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MathValue<'p> {
    /// `number` regex leaf — borrowed source slice.
    Span(&'p str),
    /// `()` — unit-typed leaf (matches `push_leaf_with_unit`).
    Unit,
    /// `expr` / `term` / `factor` / `wrapped` / parenthesis-tier
    /// rules — compound. The handle resolves through
    /// [`crate::runtime::math::MathArena`] to the child slice.
    Compound(MathCompoundId),
}

impl<'p> Default for MathValue<'p> {
    fn default() -> Self {
        MathValue::Unit
    }
}
