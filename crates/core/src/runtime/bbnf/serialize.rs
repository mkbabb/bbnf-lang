use std::fmt::Write;
use crate::runtime::bbnf::arena::{BbnfCompoundId, BbnfCompoundKind};
use crate::runtime::bbnf::document::BbnfDocument;
use crate::runtime::bbnf::value::BbnfValue;
pub fn serialize_compact_doc<'p>(doc: &BbnfDocument<'p>) -> String {
    let mut out = String::new();
    emit_value(doc, &doc.root, &mut out);
    out
}
fn emit_value<'p>(doc: &BbnfDocument<'p>, value: &BbnfValue<'p>, out: &mut String) {
    match value {
        BbnfValue::Span(s) => out.push_str(s),
        BbnfValue::Int(i) => write!(out, "{i}").unwrap(),
        BbnfValue::Float(f) => write!(out, "{f}").unwrap(),
        BbnfValue::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        BbnfValue::Tag(t) => write!(out, "{t}").unwrap(),
        BbnfValue::Unit => {}
        BbnfValue::Compound(id) => emit_compound(doc, *id, out),
    }
}
fn emit_compound<'p>(doc: &BbnfDocument<'p>, id: BbnfCompoundId, out: &mut String) {
    let compound = doc.compound(id);
    match compound.kind {
        BbnfCompoundKind::Grammar => {
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push('\n');
                }
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::GrammarItem
        | BbnfCompoundKind::Lhs
        | BbnfCompoundKind::Rhs
        | BbnfCompoundKind::Directive
        | BbnfCompoundKind::Other => {
            for child in &compound.children {
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::Rule => {
            let mut iter = compound.children.iter();
            if let Some(lhs) = iter.next() {
                emit_value(doc, lhs, out);
            }
            out.push_str(" = ");
            if let Some(rhs) = iter.next() {
                emit_value(doc, rhs, out);
            }
            out.push_str(" ;");
        }
        BbnfCompoundKind::Alternation => {
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push_str(" | ");
                }
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::Concatenation => {
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push_str(" , ");
                }
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::BinaryFactor => {
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push_str(" - ");
                }
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::MappedFactor => {
            for (i, child) in compound.children.iter().enumerate() {
                if i == 1 {
                    out.push_str(" -> ");
                }
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::Factor => {
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::Term => {
            let children = &compound.children;
            if let Some(first) = children.first() {
                emit_value(doc, first, out);
                if children.len() > 1 {
                    let starts_bracket = match first {
                        BbnfValue::Span(s) => {
                            s.starts_with('(') || s.starts_with('[')
                                || s.starts_with('{') || s.starts_with("@{")
                        }
                        _ => false,
                    };
                    if starts_bracket {
                        for child in children.iter().skip(1) {
                            emit_value(doc, child, out);
                        }
                    } else {
                        out.push('(');
                        for (i, child) in children.iter().skip(1).enumerate() {
                            if i > 0 {
                                out.push_str(", ");
                            }
                            emit_value(doc, child, out);
                        }
                        out.push(')');
                    }
                }
            }
        }
        BbnfCompoundKind::Closure => {
            let n = compound.children.len();
            if n == 0 {
                return;
            }
            out.push('|');
            for (i, child) in compound.children.iter().take(n - 1).enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                emit_value(doc, child, out);
            }
            out.push_str("| ");
            emit_value(doc, &compound.children[n - 1], out);
        }
        BbnfCompoundKind::CallArg => {
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push_str(" | ");
                }
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::ImportPath => {
            for child in &compound.children {
                if let BbnfValue::Span(s) = child {
                    if s.starts_with('"') && s.ends_with('"') {
                        out.push_str(s);
                    } else {
                        out.push('"');
                        out.push_str(s);
                        out.push('"');
                    }
                } else {
                    emit_value(doc, child, out);
                }
            }
        }
        BbnfCompoundKind::ImportItems => {
            out.push_str("{ ");
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                emit_value(doc, child, out);
            }
            out.push_str(" }");
        }
        BbnfCompoundKind::ImportDirective => {
            out.push_str("@import ");
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    let prev_was_items = matches!(
                        & compound.children[i - 1], BbnfValue::Compound(cid) if doc
                        .compound(* cid).kind == BbnfCompoundKind::ImportItems
                    );
                    let this_is_path = matches!(
                        child, BbnfValue::Compound(cid) if doc.compound(* cid).kind ==
                        BbnfCompoundKind::ImportPath
                    );
                    if prev_was_items && this_is_path {
                        out.push_str(" from ");
                    } else {
                        out.push(' ');
                    }
                }
                emit_value(doc, child, out);
            }
            out.push_str(" ;");
        }
        BbnfCompoundKind::PrettyHint => {
            let mut iter = compound.children.iter();
            if let Some(first) = iter.next() {
                emit_value(doc, first, out);
            }
            if let Some(arg) = iter.next() {
                out.push('(');
                emit_value(doc, arg, out);
                out.push(')');
            }
        }
        BbnfCompoundKind::PrettyDirective => {
            out.push_str("@pretty ");
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                emit_value(doc, child, out);
            }
            out.push_str(" ;");
        }
        BbnfCompoundKind::WsDirective => {
            out.push_str("@ws ");
            for child in &compound.children {
                emit_value(doc, child, out);
            }
            out.push_str(" ;");
        }
        BbnfCompoundKind::TokenDirective => {
            out.push_str("@token ");
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                emit_value(doc, child, out);
            }
            out.push_str(" ;");
        }
        BbnfCompoundKind::DebugDirective => {
            out.push_str("@debug ");
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                emit_value(doc, child, out);
            }
            out.push_str(" ;");
        }
        BbnfCompoundKind::HostDirective => {
            out.push_str("@host ");
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                emit_value(doc, child, out);
            }
            out.push_str(" ;");
        }
        BbnfCompoundKind::RecoverDirective => {
            out.push_str("@recover ");
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                emit_value(doc, child, out);
            }
            out.push_str(" ;");
        }
        BbnfCompoundKind::ValueExpr
        | BbnfCompoundKind::ValueClosure
        | BbnfCompoundKind::ValueOr
        | BbnfCompoundKind::ValueAnd
        | BbnfCompoundKind::ValueCmp
        | BbnfCompoundKind::ValueAdd
        | BbnfCompoundKind::ValueMul
        | BbnfCompoundKind::ValueUnary
        | BbnfCompoundKind::ValueAtom
        | BbnfCompoundKind::ValuePath
        | BbnfCompoundKind::ValueInput
        | BbnfCompoundKind::ValueFnCall => {
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::TypeAnnotation => {
            out.push(':');
            out.push(' ');
            for child in &compound.children {
                emit_value(doc, child, out);
            }
        }
    }
}
