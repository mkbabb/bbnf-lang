//! AZ-II.cutover.A — BBNF typed value sum.
//!
//! The BBNF parse output is rooted at a
//! [`crate::runtime::bbnf::BbnfDocument`] holding a
//! [`crate::runtime::bbnf::BbnfArena`] (the owner of every compound's
//! child slice) and a root [`BbnfValue<'p>`]. The `'p` lifetime ties
//! every borrowed string slice and every arena-allocated child slice
//! to the parse call site.
//!
//! Shape derivation comes directly from `grammar/bbnf/bbnf.bbnf` plus
//! its imports (`grammar/bbnf/expressions.bbnf`,
//! `grammar/bbnf/types.bbnf`):
//!
//! ```text
//! identifier  = /[_a-zA-Z]…/                 -> Span
//! literal     = ( "\"" , /…/ , "\"" | … )    -> Span
//! regex       = ( "/" , /…/ , "/" )          -> Span
//! big_comment = ( "/*" , /…/ , "*/" ) ?w     -> Span
//! comment     = ( "//" , /…/ ) ?w            -> Span
//! int_lit     = /…/                          -> i64       (expressions)
//! float_lit   = /…/                          -> f64       (expressions)
//! bool_lit    = "true" | "false"                          (expressions)
//! string_lit  = ( "\"" , /…/ , "\"" )        -> Span      (expressions)
//! ```
//!
//! Plus compound rules (`rule`, `term`, `factor`, `mapped_factor`,
//! `binary_factor`, `concatenation`, `alternation`, `closure`, `rhs`,
//! `directive`, `grammar`, …) that carry a slice of child
//! [`BbnfValue`] entries through a [`BbnfCompoundId`] handle.
//!
//! [`BbnfValue`] is the closure of every leaf and the compound handle.
//! The `Compound` arm carries a [`BbnfCompoundId`] arena handle that
//! resolves to a child slice via the document module's
//! [`crate::runtime::bbnf::BbnfDocument::compound`] accessor.

use crate::runtime::bbnf::arena::BbnfCompoundId;

/// A BBNF AST value — the closed sum of every typed projection the
/// grammar emits.
///
/// # Design
///
/// `BbnfValue<'p>` is `Copy` to mirror [`crate::runtime::json::JsonValue`]
/// and [`crate::runtime::google_sheets::SheetsValue`] — every interior
/// byte slice is borrowed from the input, every numeric / boolean
/// payload fits in 8 bytes. The `Compound` handle resolves through
/// the document's arena to the child slice.
///
/// # Variant choice
///
/// Per the Sheets discipline (`grammar/google-sheets/google-sheets.bbnf`'s
/// 17 compound rules collapsing to one [`SheetsValue::Compound`] arm),
/// every BBNF compound rule (`rule`, `term`, `factor`, `mapped_factor`,
/// `binary_factor`, `concatenation`, `alternation`, `closure`, `rhs`,
/// `import_directive`, `pretty_directive`, `recover_directive`,
/// `ws_directive`, `token_directive`, `debug_directive`,
/// `host_directive`, `directive`, `grammar_item`, `grammar`,
/// `import_items`, `pretty_hint`, `lhs`, `call_arg`) collapses into the
/// unitary [`BbnfValue::Compound`] arm. The rule-level shape is
/// preserved on the compound's [`BbnfCompoundKind`] discriminator.
///
/// Operator-tag rules (`modifier` and `binary_operators` if they ever
/// receive Nu8 annotations) project to [`BbnfValue::Tag`]. `Number`
/// arms hold `int_lit` / `float_lit` projections; `Bool` carries
/// `bool_lit`; `Span` carries every borrowed-source leaf.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BbnfValue<'p> {
    /// `int_lit = /…/ -> i64` — signed integer leaf (expressions
    /// grammar; reserved for future rule activation in BBNF proper).
    Int(i64),
    /// `float_lit = /…/ -> f64` — float leaf.
    Float(f64),
    /// `bool_lit = "true" | "false"` — boolean leaf.
    Bool(bool),
    /// `identifier` / `literal` / `regex` / `comment` / `big_comment`
    /// / `string_lit` / `import_path` — borrowed source slice.
    Span(&'p str),
    /// Tagged-enum discriminator — reserved for future Nu8 leaf
    /// projections (`modifier`, `binary_operators`). Today the BBNF
    /// grammar does not declare these as `-> Nu8`; the variant
    /// exists for cutover.B's regen path to populate without
    /// re-shaping the runtime.
    Tag(u8),
    /// `()` — unit-typed leaf (matches `push_leaf_with_unit`).
    Unit,
    /// One compound arm for every multi-child rule. The handle
    /// resolves through [`crate::runtime::bbnf::BbnfArena`] to the
    /// child slice.
    Compound(BbnfCompoundId),
}

impl<'p> Default for BbnfValue<'p> {
    fn default() -> Self {
        BbnfValue::Unit
    }
}
