//! Canonical-form serializer for [`super::SheetsDocument`].
//!
//! Walks the [`SheetsValue`] tree depth-first and emits a string
//! whose tokens reproduce the grammar's surface syntax. The walker
//! re-emits the structural separators each compound rule requires
//! (commas inside arg-lists, `:` between range endpoints, parentheses
//! around paren-expr, braces around array-literal, `;` between array
//! rows). Operator-tag projections route through [`tag_lexeme`] keyed
//! by the enclosing compound's kind so the same `Tag(0)` byte resolves
//! to `+` inside `AddExpr` and `*` inside `MulExpr`.
//!
//! Pre-W2-act this surface lived as
//! `GoogleSheetsParser::serialize_compact(node)` against the
//! cursor-backed `tape::TapeCursor`; that emitter retired alongside
//! the tape substrate when the struct-direct flip activated. The
//! struct-tree walker is the substrate-with-consumer authentic
//! equivalent.

use crate::runtime::google_sheets::arena::{
    SheetsCompoundId, SheetsCompoundKind, SheetsCompoundView,
};
use crate::runtime::google_sheets::value::SheetsValue;

use super::SheetsDocument;

/// Canonicalise the entire document into a fresh String. The Sheets
/// `formula` rule's optional leading `=` is re-emitted at the root so
/// the canonical form parses back through the grammar.
pub(super) fn serialize_compact(doc: &SheetsDocument<'_>) -> String {
    let mut out = String::with_capacity(doc.input.len());
    let SheetsValue::Compound(_) = doc.root else {
        // Top-level scalar. Sheets always wraps in a Formula
        // compound under the generated parse fn; this branch
        // covers wire-contract test fixtures that build a leaf
        // root directly.
        write_value(doc, &doc.root, SheetsCompoundKind::Wrap, &mut out);
        return out;
    };
    // Top-level formula: emit a leading `=` so the canonical form
    // is a parseable Sheets formula. The grammar's `formula`
    // rule is `/=?/ , expression`; the optional `=` is not
    // captured in the value tree, so we re-emit it here.
    out.push('=');
    if let SheetsValue::Compound(id) = doc.root {
        write_compound(doc, id, &mut out);
    }
    out
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
            // AZ-IV.W1-CLOSE.A — array_rows is a Pratt rule with `;`
            // as its operator. The Pratt loop deposits children as
            // `[row, Tag(0), row, Tag(0), …]` where each Tag IS the
            // separator. Skip Tag children when emitting and rely on
            // the index-based `;` insertion between operand
            // positions; mirror the symmetry in `ArrayRow`.
            let mut emitted_operands: usize = 0;
            for child in entry.children.iter() {
                if matches!(child, SheetsValue::Tag(_)) {
                    continue;
                }
                if emitted_operands > 0 {
                    out.push(';');
                }
                write_value(doc, child, kind, out);
                emitted_operands += 1;
            }
        }
        SheetsCompoundKind::ArrayRow => {
            // AZ-IV.W1-CLOSE.A — array_row is a Pratt rule with `,`
            // as its operator. Children are
            // `[expr, Tag(0), expr, Tag(0), …]`; the Tag IS the
            // separator (its `tag_lexeme` is empty). Skip Tag
            // children and emit `,` between operands.
            let mut emitted_operands: usize = 0;
            for child in entry.children.iter() {
                if matches!(child, SheetsValue::Tag(_)) {
                    continue;
                }
                if emitted_operands > 0 {
                    out.push(',');
                }
                write_value(doc, child, kind, out);
                emitted_operands += 1;
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
