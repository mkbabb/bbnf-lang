//! Grammar markers for [`super::ir::TypedPath`].
//!
//! Each marker is a zero-sized struct that names a grammar at the type
//! level. The `path!` proc-macro (AZ-IV.W2.4) picks the marker from the
//! caller's grammar identifier; downstream code carries it as the
//! phantom `G` parameter of `TypedPath<G, T>` so different grammars
//! cannot be confused at compile time.
//!
//! Adding a new grammar marker is a pure additive change: define the
//! ZST, re-export it from [`super`], and the macro can route to it.

/// JSON grammar marker. Resolves against the JSON `StructRegistry`
/// (from `grammar/json/json.bbnf`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Json;

/// CSS Level 4 grammar marker. Resolves against the CSS L4
/// `StructRegistry` (from `grammar/css/css_l4.bbnf`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct CssL4;

/// Google Sheets formula grammar marker. Resolves against the Sheets
/// `StructRegistry` (from `grammar/google_sheets/google_sheets.bbnf`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Sheets;

/// BBNF self-host grammar marker. Resolves against the BBNF
/// `StructRegistry` (from `grammar/bbnf/bbnf.bbnf`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Bbnf;
