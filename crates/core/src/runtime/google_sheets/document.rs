//! AZ-I.W2-act.B2 — `SheetsDocument` + view / value / path accessor
//! surface.
//!
//! The struct-direct Sheets parse path returns a [`SheetsDocument`]
//! whose root [`SheetsValue`] borrows from the input lifetime `'p`
//! and whose [`SheetsArena`] owns every compound child slice. This
//! module wraps the document with the same API the JSON runtime
//! exposes (per W2-act.A's accessor contract): `view`, `to_value`,
//! `get::<T>(path)`.
//!
//! The accessor surface mirrors `JsonDocument`; consumers writing
//! against either path observe a uniform shape across the three
//! data grammars.

use crate::runtime::google_sheets::arena::{
    SheetsArena, SheetsCompoundId, SheetsCompoundKind, SheetsCompoundView,
};
use crate::runtime::google_sheets::value::SheetsValue;
use crate::runtime::path::{Path, PathSegment};

/// The root document returned by
/// `bbnf::grammar::generated::google_sheets::GoogleSheetsParser::parse`.
///
/// Holds the parse arena (which owns every compound child slice) and
/// the root value. Borrows the input bytes via the `'p` lifetime.
#[derive(Debug)]
pub struct SheetsDocument<'p> {
    /// The compound child arena — owns every `[SheetsValue]` slice
    /// the document references via handles.
    pub arena: SheetsArena<'p>,
    /// The root value of the document.
    pub root: SheetsValue<'p>,
    /// AZ-I.W2-act.close A.fix — the input slice the parse consumed.
    /// Threaded through `finalise(input)` so [`SheetsView`] can satisfy
    /// the `RuntimeView::input()` surface without re-acquiring the
    /// source from the call site.
    pub input: &'p str,
}

impl<'p> SheetsDocument<'p> {
    /// Construct a document from a populated arena, root value, and
    /// the input slice the parse consumed. The typical caller is the
    /// generated parse function; consumers outside the emitter rarely
    /// build a `SheetsDocument` directly.
    #[inline]
    pub fn new(arena: SheetsArena<'p>, root: SheetsValue<'p>, input: &'p str) -> Self {
        Self { arena, root, input }
    }

    /// Borrow the root [`SheetsValue`].
    #[inline]
    pub fn root(&self) -> &SheetsValue<'p> {
        &self.root
    }

    /// Borrow the underlying [`SheetsArena`].
    #[inline]
    pub fn arena(&self) -> &SheetsArena<'p> {
        &self.arena
    }

    /// AZ-I.W2-act.close A.fix — borrow the input slice the parse
    /// consumed.
    #[inline]
    pub fn input(&self) -> &'p str {
        self.input
    }

    /// Resolve a [`SheetsCompoundId`] handle to the compound entry
    /// (kind + child slice).
    #[inline]
    pub fn compound(&self, id: SheetsCompoundId) -> SheetsCompoundView<'_, 'p> {
        self.arena.compound(id)
    }

    /// Borrowed root view, mirroring the
    /// `JsonDocument::view()` surface.
    #[inline]
    pub fn view<'a>(&'a self) -> SheetsView<'a, 'p> {
        SheetsView {
            doc: self,
            focus: self.root,
        }
    }

    /// Borrowed root value, mirroring `JsonDocument::to_value()`
    /// semantics. The struct-direct path's [`SheetsDocument`]
    /// already carries the typed value tree, so `to_value()` simply
    /// lends its root by reference.
    #[inline]
    pub fn to_value(&self) -> &SheetsValue<'p> {
        &self.root
    }

    /// Typed path query, mirroring `JsonDocument::get::<T>(path)`
    /// semantics.
    ///
    /// The walker descends from `doc.root()` following
    /// [`PathSegment::Index`] steps against
    /// [`SheetsValue::Compound`] child slices. There is no
    /// field-keyed step in Sheets's grammar (compounds are
    /// positional, not keyed); a `PathSegment::Field` step against a
    /// Sheets compound returns `None`.
    #[inline]
    pub fn get<T: SheetsPathQuery>(&self, path: Path<'_>) -> Option<T> {
        T::query(self, path)
    }

    /// AZ-I.W2-act.close B2 — canonical compact serialization of the
    /// struct-tree.
    ///
    /// Walks the [`SheetsValue`] tree depth-first and emits a string
    /// whose tokens reproduce the grammar's surface syntax. Borrowed
    /// leaves (`String`, `CellRef`, `Identifier`, `SheetPrefix`) emit
    /// their borrowed slice verbatim; numeric / bool / tag projections
    /// emit the canonical lexeme matching the grammar's declaration
    /// order (`true` -> `TRUE`, `Tag(0)` inside `AddExpr` -> `+`, etc.);
    /// compound rules emit their children with the structural
    /// separators the grammar requires (commas inside arg-lists,
    /// `:` between range endpoints, `(` `)` around paren-expr,
    /// `{` `}` around array-literal, `;` between array rows).
    ///
    /// Pre-W2-act this surface lived as
    /// `GoogleSheetsParser::serialize_compact(node)` against the
    /// cursor-backed [`::tape::TapeCursor`]; that
    /// emitter retired alongside the tape substrate when the
    /// struct-direct flip activated. The struct-tree walker is the
    /// substrate-with-consumer authentic equivalent.
    pub fn serialize_compact(&self) -> String {
        let mut out = String::with_capacity(self.input.len());
        let SheetsValue::Compound(_) = self.root else {
            // Top-level scalar. Sheets always wraps in a Formula
            // compound under the generated parse fn; this branch
            // covers wire-contract test fixtures that build a leaf
            // root directly.
            write_value(self, &self.root, SheetsCompoundKind::Wrap, &mut out);
            return out;
        };
        // Top-level formula: emit a leading `=` so the canonical form
        // is a parseable Sheets formula. The grammar's `formula`
        // rule is `/=?/ , expression`; the optional `=` is not
        // captured in the value tree, so we re-emit it here.
        out.push('=');
        if let SheetsValue::Compound(id) = self.root {
            write_compound(self, id, &mut out);
        }
        out
    }
}

/// Emit one [`SheetsValue`] into `out`. `parent_kind` is the
/// enclosing compound's [`SheetsCompoundKind`] — operator-tag
/// projections consult it to render the right operator lexeme
/// (a `Tag(0)` inside `AddExpr` is `+`; the same `Tag(0)` inside
/// `MulExpr` is `*`).
fn write_value<'p>(
    doc: &SheetsDocument<'p>,
    value: &SheetsValue<'p>,
    parent_kind: SheetsCompoundKind,
    out: &mut String,
) {
    use core::fmt::Write;
    match *value {
        SheetsValue::Number(n) => {
            if n.fract() == 0.0 && n.is_finite() && n.abs() < 1e16 {
                write!(out, "{}", n as i64).unwrap();
            } else {
                write!(out, "{}", n).unwrap();
            }
        }
        SheetsValue::String(s)
        | SheetsValue::CellRef(s)
        | SheetsValue::Identifier(s)
        | SheetsValue::SheetPrefix { text: s, .. } => {
            out.push_str(s);
        }
        SheetsValue::Bool(b) => out.push_str(if b { "TRUE" } else { "FALSE" }),
        SheetsValue::Error(n) => out.push_str(error_lexeme(n)),
        SheetsValue::Tag(n) => out.push_str(tag_lexeme(parent_kind, n)),
        SheetsValue::Compound(id) => write_compound(doc, id, out),
    }
}

/// Emit one [`SheetsValue::Compound`] into `out`. The compound's
/// [`SheetsCompoundKind`] selects the structural separators between
/// children — comma in arg-lists, `:` in range-refs, parentheses /
/// braces around bracketed compounds, etc.
fn write_compound<'p>(doc: &SheetsDocument<'p>, id: SheetsCompoundId, out: &mut String) {
    let entry = doc.compound(id);
    let kind = entry.kind;
    match kind {
        SheetsCompoundKind::ParenExpr => {
            out.push('(');
            for child in entry.children {
                write_value(doc, child, kind, out);
            }
            out.push(')');
        }
        SheetsCompoundKind::FuncCall => {
            // children: [func_open(identifier + "("), func_args?, then closing ")"]
            // The current builder shape pushes [identifier, args]; we re-emit
            // `name(` + args_csv + `)`.
            write_func_call(doc, &entry, out);
        }
        SheetsCompoundKind::FuncOpen => {
            // identifier + "("
            for child in entry.children {
                write_value(doc, child, kind, out);
            }
            out.push('(');
        }
        SheetsCompoundKind::FuncArgs | SheetsCompoundKind::LetArgs => {
            // Arg list: comma-separated expressions.
            for (i, child) in entry.children.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                write_value(doc, child, kind, out);
            }
        }
        SheetsCompoundKind::Arg => {
            for child in entry.children {
                write_value(doc, child, kind, out);
            }
        }
        SheetsCompoundKind::LetCall => {
            out.push_str("LET(");
            // children: [let_args | binding-list, expression]
            // The grammar emits flattened bindings + final expression;
            // children are already in source order, comma-separated.
            for (i, child) in entry.children.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                write_value(doc, child, kind, out);
            }
            out.push(')');
        }
        SheetsCompoundKind::LetBinding => {
            // (name, value) — comma between.
            for (i, child) in entry.children.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                write_value(doc, child, kind, out);
            }
        }
        SheetsCompoundKind::LambdaCall => {
            out.push_str("LAMBDA(");
            for (i, child) in entry.children.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                write_value(doc, child, kind, out);
            }
            out.push(')');
        }
        SheetsCompoundKind::LambdaParams => {
            for (i, child) in entry.children.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                write_value(doc, child, kind, out);
            }
        }
        SheetsCompoundKind::ArrayLiteral => {
            out.push('{');
            for child in entry.children {
                write_value(doc, child, kind, out);
            }
            out.push('}');
        }
        SheetsCompoundKind::ArrayRows => {
            for (i, child) in entry.children.iter().enumerate() {
                if i > 0 {
                    out.push(';');
                }
                write_value(doc, child, kind, out);
            }
        }
        SheetsCompoundKind::ArrayRow => {
            for (i, child) in entry.children.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                write_value(doc, child, kind, out);
            }
        }
        SheetsCompoundKind::RangeRef => {
            // children: [sheet_prefix?, start, ":", end] — the grammar
            // emits the optional prefix + two range_end values; the
            // `:` separator is not captured. Re-insert `:` between the
            // last two children.
            let n = entry.children.len();
            for (i, child) in entry.children.iter().enumerate() {
                // Insert `:` immediately before the final child when
                // we have at least 2 children — that's the range
                // endpoints.
                if i == n.saturating_sub(1) && n >= 2 {
                    out.push(':');
                }
                write_value(doc, child, kind, out);
            }
        }
        SheetsCompoundKind::Cell => {
            // sheet_prefix? + cell_ref — concatenated.
            for child in entry.children {
                write_value(doc, child, kind, out);
            }
        }
        SheetsCompoundKind::PostfixExpr => {
            // primary + "%" * — children carry the primary plus zero
            // or more empty markers. The PostFixExpr grammar admits
            // just the primary in the value tree; the `%` count is
            // not currently surfaced as children. Emit children
            // verbatim and trust the grammar's projection.
            for child in entry.children {
                write_value(doc, child, kind, out);
            }
        }
        SheetsCompoundKind::UnaryExpr => {
            // unary_prefix * + postfix_expr — children include any
            // leading Tag(prefix) entries followed by the inner expr.
            for child in entry.children {
                write_value(doc, child, kind, out);
            }
        }
        // Operator-tower compounds: the children alternate
        // operand-operator-operand-... and the Tag projections
        // resolve to the correct lexeme via `tag_lexeme(kind, n)`.
        SheetsCompoundKind::AddExpr
        | SheetsCompoundKind::MulExpr
        | SheetsCompoundKind::ExpExpr
        | SheetsCompoundKind::ConcatExpr
        | SheetsCompoundKind::ComparisonExpr => {
            for child in entry.children {
                write_value(doc, child, kind, out);
            }
        }
        // Operator-tag rules — the Flat / Keyword shape lands the
        // typed `Tag(b)` discriminant directly. The compound carries
        // a single Tag child whose lexeme depends on the rule's role
        // alphabet (`compare_op` indexes into the comparison
        // alphabet, `add_op` into +/-, etc.). Render by routing the
        // child through `write_value` with the rule's own kind so
        // `tag_lexeme(kind, n)` picks the right alphabet.
        SheetsCompoundKind::CompareOp
        | SheetsCompoundKind::AddOp
        | SheetsCompoundKind::MulOp
        | SheetsCompoundKind::UnaryPrefix => {
            for child in entry.children {
                write_value(doc, child, kind, out);
            }
        }
        // `error_literal` compound carries the typed `Tag(b)` /
        // `Error(b)` discriminant. Render via `error_lexeme(n)` which
        // maps the byte to the canonical `#N/A` / `#NAME?` lexeme.
        SheetsCompoundKind::ErrorLiteral => {
            for child in entry.children {
                match *child {
                    SheetsValue::Tag(n) | SheetsValue::Error(n) => {
                        out.push_str(error_lexeme(n));
                    }
                    _ => write_value(doc, child, kind, out),
                }
            }
        }
        // `sheet_prefix` compound — the Tag projection alone carries
        // the discriminator; the borrowed span comes through the
        // `SheetPrefix { tag, text }` leaf directly when the
        // specialised builder entry fires.
        SheetsCompoundKind::SheetPrefix => {
            for child in entry.children {
                write_value(doc, child, kind, out);
            }
        }
        // Transparent / wrap / forwarder shapes.
        SheetsCompoundKind::Formula
        | SheetsCompoundKind::Expression
        | SheetsCompoundKind::Primary
        | SheetsCompoundKind::Wrap
        | SheetsCompoundKind::RangeEnd
        | SheetsCompoundKind::CellOrRange
        | SheetsCompoundKind::Unknown => {
            for child in entry.children {
                write_value(doc, child, kind, out);
            }
        }
    }
}

/// Emit a function call `name(args)`.
///
/// The builder produces `func_call` with two children: the
/// identifier head and (optionally) a `func_args` compound. We emit
/// `name(` then the comma-separated args, then `)`.
fn write_func_call<'p>(
    doc: &SheetsDocument<'p>,
    entry: &SheetsCompoundView<'_, 'p>,
    out: &mut String,
) {
    let mut iter = entry.children.iter();
    if let Some(head) = iter.next() {
        // `head` is the function name (Identifier) or a `func_open`
        // compound carrying it.
        write_value(doc, head, SheetsCompoundKind::FuncCall, out);
    }
    // Decide whether the `(` was already emitted by `func_open`.
    let needs_open_paren = !out.ends_with('(');
    if needs_open_paren {
        out.push('(');
    }
    let mut first_arg = true;
    for arg in iter {
        if !first_arg {
            out.push(',');
        }
        first_arg = false;
        write_value(doc, arg, SheetsCompoundKind::FuncCall, out);
    }
    out.push(')');
}

/// Resolve an `error_literal -> Nu8` byte to its grammar lexeme.
fn error_lexeme(n: u8) -> &'static str {
    match n {
        0 => "#N/A",
        1 => "#VALUE!",
        2 => "#REF!",
        3 => "#DIV/0!",
        4 => "#NULL!",
        5 => "#NAME?",
        6 => "#NUM!",
        7 => "#ERROR!",
        8 => "#SPILL!",
        _ => "",
    }
}

/// Resolve a `Tag(n)` discriminator to its operator lexeme. The
/// enclosing compound's [`SheetsCompoundKind`] disambiguates which
/// operator alphabet the tag indexes into.
///
/// The operator-tower compounds (`AddExpr`, `MulExpr`, `ExpExpr`,
/// `ConcatExpr`, `ComparisonExpr`) consume Pratt's
/// `PRECEDENCE_ENTRIES_<rule>` discriminator alphabet — Pratt assigns
/// its own discriminator order (longest-prefix-first by op width)
/// rather than the grammar's declaration order. The
/// `ComparisonExpr` table is `<> = 0`, `<= = 1`, `>= = 2`, `< = 3`,
/// `> = 4`, `= = 5` (per `PRECEDENCE_ENTRIES_comparison_expr`).
///
/// The own-rule keyword compounds (`AddOp`, `MulOp`, `CompareOp`,
/// `UnaryPrefix`) deposit `Tag(b)` directly via the keyword shape's
/// per-branch payload extraction; the grammar's declaration order
/// is the discriminator alphabet for these. The grammar declares:
/// - `compare_op = "<>" -> 0u8 | "<=" -> 1u8 | ">=" -> 2u8 | "=" -> 3u8 | "<" -> 4u8 | ">" -> 5u8`
/// - `add_op` / `unary_prefix = "+" -> 0u8 | "-" -> 1u8`
/// - `mul_op = "*" -> 0u8 | "/" -> 1u8`
fn tag_lexeme(parent: SheetsCompoundKind, n: u8) -> &'static str {
    match (parent, n) {
        // Pratt-tower discriminants (PRECEDENCE_ENTRIES_<rule> order).
        (SheetsCompoundKind::AddExpr, 0) | (SheetsCompoundKind::UnaryExpr, 0) => "+",
        (SheetsCompoundKind::AddExpr, 1) | (SheetsCompoundKind::UnaryExpr, 1) => "-",
        (SheetsCompoundKind::MulExpr, 0) => "*",
        (SheetsCompoundKind::MulExpr, 1) => "/",
        (SheetsCompoundKind::ExpExpr, _) => "^",
        (SheetsCompoundKind::ConcatExpr, _) => "&",
        (SheetsCompoundKind::ComparisonExpr, 0) => "<>",
        (SheetsCompoundKind::ComparisonExpr, 1) => "<=",
        (SheetsCompoundKind::ComparisonExpr, 2) => ">=",
        (SheetsCompoundKind::ComparisonExpr, 3) => "<",
        (SheetsCompoundKind::ComparisonExpr, 4) => ">",
        (SheetsCompoundKind::ComparisonExpr, 5) => "=",
        // Own-rule keyword discriminants (grammar declaration order).
        (SheetsCompoundKind::AddOp, 0) | (SheetsCompoundKind::UnaryPrefix, 0) => "+",
        (SheetsCompoundKind::AddOp, 1) | (SheetsCompoundKind::UnaryPrefix, 1) => "-",
        (SheetsCompoundKind::MulOp, 0) => "*",
        (SheetsCompoundKind::MulOp, 1) => "/",
        (SheetsCompoundKind::CompareOp, 0) => "<>",
        (SheetsCompoundKind::CompareOp, 1) => "<=",
        (SheetsCompoundKind::CompareOp, 2) => ">=",
        (SheetsCompoundKind::CompareOp, 3) => "=",
        (SheetsCompoundKind::CompareOp, 4) => "<",
        (SheetsCompoundKind::CompareOp, 5) => ">",
        // Fallback — unknown tag/parent pairing emits empty so the
        // serializer remains deterministic; consumers asserting the
        // round-trip notice the lossy emission via the fixed-point
        // check.
        _ => "",
    }
}

/// AZ-I.W2-act.B2 — a thin newtype over `&SheetsDocument`.
///
/// Mirrors `JsonView`; the two-lifetime parameter shape preserves
/// compositional invariance through the arena's `Vec<SheetsValue<'p>>`
/// owner.
#[derive(Debug, Clone, Copy)]
pub struct SheetsView<'a, 'p: 'a> {
    pub(crate) doc: &'a SheetsDocument<'p>,
    /// AZ-I.W2-act.close A.fix — the focused [`SheetsValue`] this view
    /// observes. Defaults to `doc.root` for `SheetsDocument::view()`;
    /// `RuntimeView::children()` yields views with the same `doc` but
    /// a different focus.
    pub(crate) focus: SheetsValue<'p>,
}

impl<'a, 'p: 'a> SheetsView<'a, 'p> {
    /// Construct a view focused on a specific [`SheetsValue`] within
    /// the document.
    #[inline]
    pub fn focused(doc: &'a SheetsDocument<'p>, focus: SheetsValue<'p>) -> Self {
        Self { doc, focus }
    }

    /// Borrow the underlying document.
    #[inline]
    pub fn document(&self) -> &'a SheetsDocument<'p> {
        self.doc
    }

    /// AZ-I.W2-act.close A.fix — the focused [`SheetsValue`] this view
    /// observes (root for top-level views; sub-tree for descendants
    /// produced by `children()`).
    #[inline]
    pub fn focus(&self) -> SheetsValue<'p> {
        self.focus
    }

    /// Borrow the root [`SheetsValue`].
    #[inline]
    pub fn root(&self) -> &'a SheetsValue<'p> {
        &self.doc.root
    }

    /// Borrow the underlying arena.
    #[inline]
    pub fn arena(&self) -> &'a SheetsArena<'p> {
        &self.doc.arena
    }

    /// Resolve a compound handle through the document's arena.
    #[inline]
    pub fn compound(&self, id: SheetsCompoundId) -> SheetsCompoundView<'a, 'p> {
        self.doc.compound(id)
    }

    /// Discriminator over the focused value's typed shape.
    #[inline]
    pub fn kind(&self) -> SheetsKind {
        match &self.focus {
            SheetsValue::Number(_) => SheetsKind::Number,
            SheetsValue::String(_) => SheetsKind::String,
            SheetsValue::Bool(_) => SheetsKind::Bool,
            SheetsValue::Error(_) => SheetsKind::Error,
            SheetsValue::CellRef(_) => SheetsKind::CellRef,
            SheetsValue::Identifier(_) => SheetsKind::Identifier,
            SheetsValue::SheetPrefix { .. } => SheetsKind::SheetPrefix,
            SheetsValue::Tag(_) => SheetsKind::Tag,
            SheetsValue::Compound(_) => SheetsKind::Compound,
        }
    }

    /// `true` iff the focused value is a compound (any non-leaf rule).
    #[inline]
    pub fn is_compound(&self) -> bool {
        matches!(self.focus, SheetsValue::Compound(_))
    }

    /// `true` iff the focused value is a number.
    #[inline]
    pub fn is_number(&self) -> bool {
        matches!(self.focus, SheetsValue::Number(_))
    }

    /// `true` iff the focused value is a string-shaped leaf (string /
    /// cell_ref / identifier / sheet_prefix text).
    #[inline]
    pub fn is_string(&self) -> bool {
        matches!(
            self.focus,
            SheetsValue::String(_)
                | SheetsValue::CellRef(_)
                | SheetsValue::Identifier(_)
                | SheetsValue::SheetPrefix { .. }
        )
    }
}

/// Discriminator over the typed shapes a [`SheetsValue`] takes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SheetsKind {
    /// `number = /…/ -> f64`.
    Number,
    /// `string = /"…"/`.
    String,
    /// `boolean = /TRUE/i | /FALSE/i`.
    Bool,
    /// `error_literal = "#N/A" -> 0u8 | …`.
    Error,
    /// `cell_ref = /…/`.
    CellRef,
    /// `identifier = /…/`.
    Identifier,
    /// `sheet_prefix` projection.
    SheetPrefix,
    /// Operator-tag projection (`compare_op`, `add_op`, etc.).
    Tag,
    /// Compound shape — any non-leaf rule.
    Compound,
}

/// AZ-I.W2-act.B2 — typed path-query trait, mirroring
/// `JsonPathQuery` for the Sheets surface.
///
/// Sheets compounds are positional, so the walker uses
/// [`PathSegment::Index`] only; a [`PathSegment::Field`] step against
/// a Sheets compound returns `None`. (Future grammar refinements that
/// expose named fields — e.g. `cell.sheet_prefix`,
/// `cell.cell_ref` — could add field-keyed dispatch by widening this
/// trait without breaking the index path.)
pub trait SheetsPathQuery: Sized {
    /// Resolve `path` against `doc`, yielding the extracted leaf or
    /// `None` if any path segment fails to match.
    fn query<'p>(doc: &SheetsDocument<'p>, path: Path<'_>) -> Option<Self>;
}

/// Walk the document's compound tree following `path` from `root`,
/// returning the resolved [`SheetsValue`] reference (or `None` on
/// out-of-range index / type mismatch).
#[inline]
fn walk_path<'a, 'p>(doc: &'a SheetsDocument<'p>, path: Path<'_>) -> Option<&'a SheetsValue<'p>> {
    let mut current: &'a SheetsValue<'p> = &doc.root;
    for segment in path.iter() {
        current = match (current, segment) {
            (SheetsValue::Compound(id), PathSegment::Index(idx)) => {
                let entry = doc.compound(*id);
                entry.children.get(*idx)?
            }
            // Sheets compounds are positional, not keyed. Field steps
            // are unsupported; any other shape (scalar leaves) cannot
            // accept a step.
            _ => return None,
        };
    }
    Some(current)
}

impl SheetsPathQuery for f64 {
    #[inline]
    fn query<'p>(doc: &SheetsDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            SheetsValue::Number(n) => Some(*n),
            _ => None,
        }
    }
}

impl SheetsPathQuery for bool {
    #[inline]
    fn query<'p>(doc: &SheetsDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            SheetsValue::Bool(b) => Some(*b),
            _ => None,
        }
    }
}

impl SheetsPathQuery for u8 {
    #[inline]
    fn query<'p>(doc: &SheetsDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            SheetsValue::Tag(t) | SheetsValue::Error(t) => Some(*t),
            SheetsValue::SheetPrefix { tag, .. } => Some(*tag),
            _ => None,
        }
    }
}

impl SheetsPathQuery for &str {
    #[inline]
    fn query<'p>(doc: &SheetsDocument<'p>, path: Path<'_>) -> Option<Self> {
        let value = walk_path(doc, path)?;
        match value {
            SheetsValue::String(s)
            | SheetsValue::CellRef(s)
            | SheetsValue::Identifier(s)
            | SheetsValue::SheetPrefix { text: s, .. } => {
                let extended: &'p str = *s;
                // SAFETY: the borrowed `&str` slice lives for `'p`
                // (the document's input lifetime); the trait surface
                // elides the explicit `'p` because `&str` is invariant
                // in lifetime here.
                Some(unsafe { core::mem::transmute::<&'p str, &str>(extended) })
            }
            _ => None,
        }
    }
}

impl SheetsPathQuery for SheetsValue<'_> {
    #[inline]
    fn query<'p>(doc: &SheetsDocument<'p>, path: Path<'_>) -> Option<Self> {
        let value = walk_path(doc, path)?;
        // SAFETY: SheetsValue is Copy and carries a `'p` lifetime
        // that outlives the caller's borrow on `doc`. The transmute
        // re-projects the lifetime to the trait's elided one.
        let copied: SheetsValue<'p> = *value;
        Some(unsafe { core::mem::transmute::<SheetsValue<'p>, SheetsValue<'_>>(copied) })
    }
}
