use crate::runtime::google_sheets::arena::{
    SheetsCompoundId, SheetsCompoundKind, SheetsCompoundView,
};
use crate::runtime::google_sheets::value::SheetsValue;
use super::SheetsDocument;
pub(super) fn serialize_compact(doc: &SheetsDocument<'_>) -> String {
    let mut out = String::with_capacity(doc.input.len());
    let SheetsValue::Compound(_) = doc.root else {
        write_value(doc, &doc.root, SheetsCompoundKind::Wrap, &mut out);
        return out;
    };
    out.push('=');
    if let SheetsValue::Compound(id) = doc.root {
        write_compound(doc, id, &mut out);
    }
    out
}
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
        | SheetsValue::SheetPrefix { text: s, .. } => out.push_str(s),
        SheetsValue::Bool(b) => out.push_str(if b { "TRUE" } else { "FALSE" }),
        SheetsValue::Error(n) => out.push_str(error_lexeme(n)),
        SheetsValue::Tag(n) => out.push_str(tag_lexeme(parent_kind, n)),
        SheetsValue::Compound(id) => write_compound(doc, id, out),
    }
}
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
        SheetsCompoundKind::FuncCall => write_func_call(doc, &entry, out),
        SheetsCompoundKind::FuncOpen => {
            for child in entry.children {
                write_value(doc, child, kind, out);
            }
            out.push('(');
        }
        SheetsCompoundKind::FuncArgs | SheetsCompoundKind::LetArgs => {
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
            for (i, child) in entry.children.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                write_value(doc, child, kind, out);
            }
            out.push(')');
        }
        SheetsCompoundKind::LetBinding => {
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
            let mut emitted = 0usize;
            for child in entry.children {
                if matches!(child, SheetsValue::Tag(_)) {
                    continue;
                }
                if emitted > 0 {
                    out.push(';');
                }
                write_value(doc, child, kind, out);
                emitted += 1;
            }
        }
        SheetsCompoundKind::ArrayRow => {
            let mut emitted = 0usize;
            for child in entry.children {
                if matches!(child, SheetsValue::Tag(_)) {
                    continue;
                }
                if emitted > 0 {
                    out.push(',');
                }
                write_value(doc, child, kind, out);
                emitted += 1;
            }
        }
        SheetsCompoundKind::RangeRef => {
            let n = entry.children.len();
            for (i, child) in entry.children.iter().enumerate() {
                if i == n.saturating_sub(1) && n >= 2 {
                    out.push(':');
                }
                write_value(doc, child, kind, out);
            }
        }
        SheetsCompoundKind::Cell
        | SheetsCompoundKind::PostfixExpr
        | SheetsCompoundKind::UnaryExpr
        | SheetsCompoundKind::AddExpr
        | SheetsCompoundKind::MulExpr
        | SheetsCompoundKind::ExpExpr
        | SheetsCompoundKind::ConcatExpr
        | SheetsCompoundKind::ComparisonExpr
        | SheetsCompoundKind::CompareOp
        | SheetsCompoundKind::AddOp
        | SheetsCompoundKind::MulOp
        | SheetsCompoundKind::UnaryPrefix
        | SheetsCompoundKind::SheetPrefix
        | SheetsCompoundKind::Formula
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
        SheetsCompoundKind::ErrorLiteral => {
            for child in entry.children {
                match *child {
                    SheetsValue::Tag(n) | SheetsValue::Error(n) => {
                        out.push_str(error_lexeme(n))
                    }
                    _ => write_value(doc, child, kind, out),
                }
            }
        }
    }
}
fn write_func_call<'p>(
    doc: &SheetsDocument<'p>,
    entry: &SheetsCompoundView<'_, 'p>,
    out: &mut String,
) {
    let mut iter = entry.children.iter();
    if let Some(head) = iter.next() {
        write_value(doc, head, SheetsCompoundKind::FuncCall, out);
    }
    if !out.ends_with('(') {
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
fn tag_lexeme(parent: SheetsCompoundKind, n: u8) -> &'static str {
    match (parent, n) {
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
        _ => "",
    }
}
