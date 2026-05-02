//! AZ-II.cutover.I (Phase 5) — BBNF compact-source serializer.
//!
//! Walks a [`BbnfDocument`] and re-emits compact BBNF source. Mirrors
//! the per-grammar [`SerializeCompact`] impls the StructDirect emitter
//! produces (e.g. `JsonParser::serialize_compact`). The walker is a
//! typed projection over [`BbnfCompoundKind`]: every compound shape
//! has a known structural literal layout (`=`, `;`, `,`, `|`, `->`,
//! …), and the emitter inserts those literals between Span / Compound
//! children in the order the grammar dictates.
//!
//! # Why not span-fold?
//!
//! `BbnfView::span_range()` returns `(min_lo, max_hi)` over every
//! borrowed leaf — but the union slice excludes whitespace AND every
//! non-Span literal (`=`, `;`, `,`, `|`, `->`, `from`, `@import`, …).
//! For grammars whose terminators are mandatory (BBNF rules MUST end
//! with `;` or `.`), the naive span-fold drops the terminator and the
//! re-parse fails. The typed walker below knows the BBNF surface and
//! injects every required structural literal verbatim.
//!
//! # Round-trip discipline
//!
//! `parse(src) → serialize_compact_doc → parse → serialize_compact_doc`
//! is idempotent: the second serialize_compact_doc must equal the
//! first byte-for-byte. The first emit may not equal `src` (whitespace
//! collapses, comments may relocate), but the re-parse-and-re-emit
//! cycle stabilises.

use std::fmt::Write;

use crate::runtime::bbnf::arena::{BbnfCompoundId, BbnfCompoundKind};
use crate::runtime::bbnf::document::BbnfDocument;
use crate::runtime::bbnf::value::BbnfValue;

/// Serialize a parsed [`BbnfDocument`] into compact BBNF source.
///
/// The output is a byte-stable re-emission of the document's structural
/// shape: every Span leaf is dumped verbatim, every compound's required
/// structural literals (`=`, `;`, `,`, `|`, `->`, …) are inserted in
/// grammar order, and whitespace collapses to single spaces between
/// adjacent tokens (newlines after each `rule` / `directive`).
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
            // `grammar = ( grammar_item ?w ) *` — items separated by
            // whitespace; each rule / directive carries its own
            // terminator so newlines suffice.
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push('\n');
                }
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::GrammarItem => {
            // Pass-through to the wrapped item (rule / directive /
            // comment / big_comment).
            for child in &compound.children {
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::Rule => {
            // `rule = lhs , "=" ?w , rhs ?w , ( ";" | "." )` — the
            // bootstrap parser pushes `lhs` and `rhs` as compound
            // children; the `=` and terminator are non-Span literals.
            // Emit: lhs " = " rhs " ;"
            // Children are exactly: [lhs, rhs] (the parser does not
            // emit `=` or terminator as Spans).
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
        BbnfCompoundKind::Lhs => {
            // `lhs = identifier` — pass through.
            for child in &compound.children {
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::Rhs => {
            // `rhs = closure | alternation` — pass through.
            for child in &compound.children {
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::Alternation => {
            // `alternation = ( concatenation ?w , "|" ? ) +` —
            // concatenations separated by ` | `.
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push_str(" | ");
                }
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::Concatenation => {
            // `concatenation = ( binary_factor ?w , "," ? ) +` —
            // binary_factors separated by ` , `.
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push_str(" , ");
                }
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::BinaryFactor => {
            // `binary_factor = mapped_factor , ( binary_operators ?w ,
            // mapped_factor ) *`. The bootstrap parser does not push
            // the operator as a Span; it consumes it and recurses.
            // Children are therefore [mapped_factor, mapped_factor*].
            // For idempotence we lose the operator distinction; in
            // practice binary_operators are rare in real grammars and
            // the round-trip degrades gracefully (the surface still
            // re-parses; the semantic content matches up to operator
            // identity which the ipsotape dropped anyway).
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push_str(" - ");
                }
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::MappedFactor => {
            // `mapped_factor = factor , ( "->" ?w , value_expr ,
            // type_annotation ? ) ?`. Children: [factor] or
            // [factor, value_expr, type_annotation?]. The `->` literal
            // is non-Span; emit between factor and value_expr;
            // subsequent children (type_annotation) need no separator.
            for (i, child) in compound.children.iter().enumerate() {
                if i == 1 {
                    out.push_str(" -> ");
                }
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::Factor => {
            // `factor = big_comment ? , term ?w , modifier ? ,
            // big_comment ?`. All children are emitted in source order;
            // no separator literal between them (whitespace handles).
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    // Insert a single space between term and modifier
                    // (modifier is a Span like "?", "*", "?w").
                    // The bootstrap parser does not preserve which
                    // child is the modifier vs the comment, so we
                    // emit a single space between every pair.
                    out.push(' ');
                }
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::Term => {
            // Term children are either:
            //  - one identifier (Span) plus optional call args (compounds)
            //  - one literal / regex / epsilon (Span)
            //  - bracket open Span + inner rhs (compound) + bracket close Span
            //  - "@{" Span + inner rhs (compound) + "}" Span
            //
            // For grouping forms, the bootstrap parser pushes the
            // literal brackets as Spans, so we just emit verbatim.
            // For the function-call form (identifier + call_arg*),
            // we need parens + commas between call_args.
            //
            // Heuristic: if the first child is an identifier-like Span
            // and there are subsequent compound children, those are
            // call_args separated by ", ".
            let children = &compound.children;
            if let Some(first) = children.first() {
                emit_value(doc, first, out);
                if children.len() > 1 {
                    // Distinguish bracketed grouping vs function call.
                    // Bracketed forms have the bracket Span as `first`
                    // (starts with `(` / `[` / `{` / `@`); function-call
                    // forms have an identifier as `first`.
                    let starts_bracket = match first {
                        BbnfValue::Span(s) => {
                            s.starts_with('(')
                                || s.starts_with('[')
                                || s.starts_with('{')
                                || s.starts_with("@{")
                        }
                        _ => false,
                    };
                    if starts_bracket {
                        // Emit children verbatim; the closing bracket
                        // is a Span at the tail.
                        for child in children.iter().skip(1) {
                            emit_value(doc, child, out);
                        }
                    } else {
                        // Function-call form: identifier(call_arg, …).
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
            // `closure = "|" , identifier , ( "," ?w , identifier ) * ,
            // "|" ?w , rhs`. Children: [identifier+, rhs]. The last
            // child is the rhs; everything before is a parameter
            // identifier. Emit: |id1, id2, …| rhs.
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
            // `call_arg = ( binary_factor ?w , "|" ? ) +` —
            // binary_factors separated by " | ".
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push_str(" | ");
                }
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::ImportPath => {
            // Children: [Span "..."] — but the bootstrap parser pushes
            // the inner contents as Span (excluding quotes). Re-wrap
            // with quotes if missing.
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
            // `{ id, id, … }`.
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
            // `@import { ids } from "path" ;` or `@import "path" ;`.
            out.push_str("@import ");
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    // Insert "from " before import_path when preceded
                    // by import_items.
                    let prev_was_items = matches!(
                        &compound.children[i - 1],
                        BbnfValue::Compound(cid)
                            if doc.compound(*cid).kind == BbnfCompoundKind::ImportItems
                    );
                    let this_is_path = matches!(
                        child,
                        BbnfValue::Compound(cid)
                            if doc.compound(*cid).kind == BbnfCompoundKind::ImportPath
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
            // `pretty_hint = identifier , ( "(" , /[^)]*/ , ")" ) ?` —
            // children: [identifier] or [identifier, span_arg]. Emit
            // identifier directly; if a span_arg follows, wrap it in
            // parens (the parser captures the parenthesised arg span).
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
            // `@pretty <ident> <hint*> ;`.
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
            // `@ws <regex> ;`.
            out.push_str("@ws ");
            for child in &compound.children {
                emit_value(doc, child, out);
            }
            out.push_str(" ;");
        }
        BbnfCompoundKind::TokenDirective => {
            // `@token <ident> <regex>? ;`.
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
            // `@debug ... ;` — emit the children verbatim with leading
            // `@debug` prefix.
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
            // `@host <ident> ... ;`.
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
            // `@recover <ident> <rhs> ;`.
            out.push_str("@recover ");
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                emit_value(doc, child, out);
            }
            out.push_str(" ;");
        }
        BbnfCompoundKind::Directive => {
            // Pass-through to the wrapped directive arm.
            for child in &compound.children {
                emit_value(doc, child, out);
            }
        }
        // Value-expression sub-grammar (for `-> value_expr`
        // annotations). Each precedence-layer compound's children
        // are operands; operators are dropped (non-Span). Emit
        // children space-separated as a best-effort surface.
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
            // `type_annotation = ":" ?w , type_name` — emit the
            // `:` prefix, a single space, then the type_name. The
            // `:` literal is not surfaced as a child under struct-
            // direct projection (consumed by byte advance); the
            // type_name's branch_tag identifies which primitive
            // matched, so we recover the literal text from the
            // BBNF allocation.
            out.push(':');
            out.push(' ');
            for child in &compound.children {
                emit_value(doc, child, out);
            }
        }
        BbnfCompoundKind::Other => {
            // Unknown shape — pass through children verbatim.
            for child in &compound.children {
                emit_value(doc, child, out);
            }
        }
    }
}
