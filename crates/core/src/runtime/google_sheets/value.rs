//! AZ-I.W2-act.B2 — Google Sheets typed value sum.
//!
//! The parse output is rooted at a
//! [`crate::runtime::google_sheets::SheetsDocument`] holding a
//! [`crate::runtime::google_sheets::SheetsArena`] (the owner of every
//! compound's child slice) and a root [`SheetsValue<'p>`]. The `'p`
//! lifetime ties every borrowed string slice and every arena-allocated
//! child slice to the parse call site.
//!
//! Shape derivation comes directly from `grammar/google-sheets/google-sheets.bbnf`:
//!
//! ```text
//! number       = /(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?/ -> f64
//! string       = /"…"/                 -> input : Span
//! boolean      = /[tT]…/  -> true | /[fF]…/ -> false
//! error_literal = "#N/A" -> 0u8 | … (9 branches)        : Nu8
//! sheet_prefix = /'…'!/  -> 0u8 | /[A-Za-z…]!/  -> 1u8  : Nu8
//! cell_ref     = /\$?[A-Za-z]{1,3}\$?\d+/  -> input : Span
//! identifier   = /[A-Za-z_][A-Za-z0-9_.]*/  -> input : Span
//! compare_op   = "<>" -> 0u8 | … (6 branches)           : Nu8
//! add_op       = "+" -> 0u8 | "-" -> 1u8                : Nu8
//! mul_op       = "*" -> 0u8 | "/" -> 1u8                : Nu8
//! unary_prefix = "+" -> 0u8 | "-" -> 1u8                : Nu8
//! ```
//!
//! Plus compound rules (`expression`, `formula`, `paren_expr`,
//! `func_call`, `let_call`, `lambda_call`, `array_literal`, etc.) that
//! carry a Vec of child [`SheetsValue`] entries through a
//! [`SheetsCompoundId`] handle.
//!
//! [`SheetsValue`] is the closure of every leaf and the compound
//! handle. The `Compound` arm carries a [`SheetsCompoundId`] arena
//! handle that resolves to a child slice via the document module's
//! `SheetsDocument::compound` accessor.

use crate::runtime::google_sheets::arena::SheetsCompoundId;

/// A Sheets formula value — the closed sum of every typed projection
/// the grammar emits. Compounds carry an arena-backed handle; leaves
/// carry the payload directly.
///
/// # Design
///
/// `SheetsValue<'p>` is `Copy` to mirror [`crate::runtime::json::JsonValue`]
/// — every interior byte slice is borrowed from the input (or
/// arena-decoded), every numeric / boolean / discriminant payload
/// fits in 8 bytes. The `Compound` handle resolves through the
/// document's arena to the child slice.
///
/// # Variant choice
///
/// Sheets's grammar is broader than JSON's; every compound rule
/// (expression, paren_expr, func_call, let_call, lambda_call,
/// array_literal, cell, range_ref, postfix_expr, unary_expr,
/// exp_expr, mul_expr, add_expr, concat_expr, comparison_expr,
/// formula) collapses into the unitary [`SheetsValue::Compound`]
/// arm. The `kind` discriminator on the compound's
/// [`crate::runtime::google_sheets::SheetsCompound`] entry preserves
/// the rule-level shape for consumers walking the tree;
/// [`crate::runtime::google_sheets::SheetsCompoundKind`] enumerates
/// the structural alphabet.
///
/// Operator-tag rules (`compare_op`, `add_op`, `mul_op`,
/// `unary_prefix`, `error_literal`, `sheet_prefix`, `boolean` collapsed
/// to bool) project to [`SheetsValue::Tag`] / [`SheetsValue::Bool`];
/// the typed alphabet fits the eight-bit discriminator the grammar
/// declares. `Number(f64)` and `String / CellRef / Identifier` close
/// the leaf-level shape.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SheetsValue<'p> {
    /// `number = /…/ -> f64` — always-present leaf carrying the
    /// Eisel-Lemire decoded f64 value.
    Number(f64),
    /// `string = /"…"/ -> input : Span` — borrowed quoted-string body
    /// (still wrapping double-quotes; escape decoding deferred per the
    /// grammar's TODO note).
    String(&'p str),
    /// `boolean = /TRUE/i -> true | /FALSE/i -> false` — collapsed
    /// to a bool leaf. The branch tag (0 / 1) is implicit in the
    /// boolean value; consumers reading `bool` directly need not
    /// re-derive it.
    Bool(bool),
    /// `error_literal = "#N/A" -> 0u8 | "#VALUE!" -> 1u8 | …` —
    /// 9-branch typed-tag projection. Variant indices match the
    /// grammar declaration order (`#N/A=0`, `#VALUE!=1`, `#REF!=2`,
    /// `#DIV/0!=3`, `#NULL!=4`, `#NAME?=5`, `#NUM!=6`, `#ERROR!=7`,
    /// `#SPILL!=8`).
    Error(u8),
    /// `cell_ref = /\$?[A-Za-z]{1,3}\$?\d+/ -> input : Span` —
    /// canonical cell reference (`A1`, `$B$2`). Sub-decoding into
    /// (row, col, abs_row, abs_col) deferred per the grammar's
    /// AU.6.7 TODO note; today the leaf carries the borrowed span
    /// raw.
    CellRef(&'p str),
    /// `identifier = /[A-Za-z_][A-Za-z0-9_.]*/ -> input : Span` —
    /// function name / variable / named range / symbolic reference.
    Identifier(&'p str),
    /// `sheet_prefix` projection — preserves the branch tag (0 =
    /// quoted, 1 = bare) alongside the borrowed span. Quoted form
    /// includes leading/trailing `'`; bare form is the raw ident.
    SheetPrefix {
        /// `0u8` for quoted (`'Sheet Name'!`), `1u8` for bare
        /// (`Sheet1!`).
        tag: u8,
        /// The matched span including the trailing `!`.
        text: &'p str,
    },
    /// Operator-tag projections — `compare_op`, `add_op`, `mul_op`,
    /// `unary_prefix` rules emit a discriminator byte matching the
    /// grammar's typed Nu8 declaration.
    ///
    /// The variant is intentionally shape-agnostic: consumers
    /// disambiguate the operator's role by walking the enclosing
    /// compound's [`crate::runtime::google_sheets::SheetsCompoundKind`].
    /// A `Tag(0)` inside an `AddExpr` compound means `+`; the same
    /// `Tag(0)` inside a `MulExpr` means `*`.
    Tag(u8),
    /// Compound shape — `expression`, `formula`, `paren_expr`,
    /// `func_call`, `let_call`, `lambda_call`, `array_literal`,
    /// `func_args`, `let_args`, `lambda_params`, `array_rows`,
    /// `array_row`, `range_ref`, `cell`, `cell_or_range`, etc.
    /// Resolves via [`crate::runtime::google_sheets::SheetsDocument::compound`]
    /// to a [`crate::runtime::google_sheets::SheetsCompound`] entry
    /// carrying the child slice + the compound's kind discriminator.
    Compound(SheetsCompoundId),
}

impl<'p> SheetsValue<'p> {
    /// `true` iff this is a numeric leaf.
    #[inline]
    pub fn is_number(&self) -> bool {
        matches!(self, SheetsValue::Number(_))
    }

    /// `true` iff this is a string leaf (quoted-string projection).
    #[inline]
    pub fn is_string(&self) -> bool {
        matches!(self, SheetsValue::String(_))
    }

    /// `true` iff this is a compound (any non-leaf rule).
    #[inline]
    pub fn is_compound(&self) -> bool {
        matches!(self, SheetsValue::Compound(_))
    }

    /// Yield the f64 if this is a number leaf.
    #[inline]
    pub fn as_f64(&self) -> Option<f64> {
        match *self {
            SheetsValue::Number(n) => Some(n),
            _ => None,
        }
    }

    /// Yield the borrowed slice if this is a string-shaped leaf
    /// (`String`, `CellRef`, `Identifier`, or `SheetPrefix.text`).
    #[inline]
    pub fn as_str(&self) -> Option<&'p str> {
        match *self {
            SheetsValue::String(s)
            | SheetsValue::CellRef(s)
            | SheetsValue::Identifier(s)
            | SheetsValue::SheetPrefix { text: s, .. } => Some(s),
            _ => None,
        }
    }

    /// Yield the bool if this is a boolean leaf.
    #[inline]
    pub fn as_bool(&self) -> Option<bool> {
        match *self {
            SheetsValue::Bool(b) => Some(b),
            _ => None,
        }
    }

    /// Yield the discriminator byte if this is a tag / error leaf.
    /// `Error(n)` and `Tag(n)` both project here; the enclosing
    /// compound's kind disambiguates.
    #[inline]
    pub fn as_u8(&self) -> Option<u8> {
        match *self {
            SheetsValue::Tag(t) | SheetsValue::Error(t) => Some(t),
            SheetsValue::SheetPrefix { tag, .. } => Some(tag),
            _ => None,
        }
    }
}
