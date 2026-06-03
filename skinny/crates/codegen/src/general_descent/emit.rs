//! The recursive-descent recognizer emitter over the `SinkOnlyExpr` tower.

use super::regex_first;
use super::{byte_string_literal, rule_ident};
use crate::lower::sink_only::{SinkOnlyExpr, SinkOnlyProgram, SinkOnlyRule};
use ir::AltMode;
use std::collections::BTreeSet;
use std::fmt::Write;

/// Render the full `generated.rs` body. `prefix` is the grammar-derived
/// PascalCase type prefix (`{prefix}Document` etc.) threaded from the emit entry.
pub(super) fn render(program: &SinkOnlyProgram, prefix: &str) -> Result<String, String> {
    if !program.has_rule(&program.entry_rule) {
        return Err(format!(
            "general descent emitter missing entry rule `{}`",
            program.entry_rule
        ));
    }
    let mut out = String::new();
    render_header(program, prefix, &mut out);
    render_prelude(program, prefix, &mut out);
    for rule in &program.rules {
        render_rule(program, rule, &mut out)?;
    }
    Ok(out)
}

fn render_header(program: &SinkOnlyProgram, prefix: &str, out: &mut String) {
    let _ = writeln!(
        out,
        "// general recursive descent lowered from sink-only program: entry={} rules={}",
        program.entry_rule,
        program.rules.len(),
    );
    let _ = writeln!(
        out,
        "use crate::tape::{{OffsetFlags, TapeBuilder}};\nuse super::view::{prefix}Document;",
    );
    out.push_str(
        r#"
/// A position-only recognition error.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct ParseError<'i> {
    pub offset: usize,
    pub message: &'static str,
    #[doc(hidden)]
    pub(crate) _marker: core::marker::PhantomData<&'i ()>,
}

impl<'i> ParseError<'i> {
    #[inline]
    fn at(offset: usize, message: &'static str) -> Self {
        ParseError {
            offset,
            message,
            _marker: core::marker::PhantomData,
        }
    }

    /// Construct an error with a caller-supplied offset + message (the parser
    /// front-door uses this for the UTF-8 boundary error).
    #[inline]
    pub fn new(offset: usize, message: &'static str) -> Self {
        ParseError::at(offset, message)
    }
}

impl std::fmt::Display for ParseError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at byte {}", self.message, self.offset)
    }
}

impl std::error::Error for ParseError<'_> {}

"#,
    );
}

/// The fixed recognizer scaffolding: the entry, the ws-skip, the literal/leaf
/// scan helpers. `prefix` names the grammar-derived `{prefix}Document` handle.
fn render_prelude(program: &SinkOnlyProgram, prefix: &str, out: &mut String) {
    let entry = rule_ident(&program.entry_rule);
    // The leaf continuation-stop alphabet: every distinct first byte of the
    // program's structural ByteLiterals. The delimiter scan halts a leaf span at
    // the first byte in this set (or ASCII whitespace) — the grammar-derived
    // structural boundary the leaf cannot cross. Derived, never literal here.
    let mut stops: BTreeSet<u8> = BTreeSet::new();
    for literal in &program.literals {
        if let Some(byte) = literal.first().copied() {
            stops.insert(byte);
        }
    }
    let stop_array = byte_string_literal(&stops.iter().copied().collect::<Vec<_>>());

    let _ = write!(
        out,
        r#"/// The grammar-derived structural-boundary alphabet: the first byte of every
/// `ByteLiteral` the program carries. A leaf span (the delimiter-scan fallback)
/// stops at the first structural byte — it cannot cross a grammar delimiter.
const LEAF_STOP: &[u8] = {stop_array};

/// Track-1 entry: recognize the formula into the existing offset tape and return
/// the lazy tape-backed document.
pub fn parse_into_tape(input: &str) -> Result<{prefix}Document<'_>, ParseError<'_>> {{
    let bytes = input.as_bytes();
    let mut tape = TapeBuilder::new(bytes, structural_reserve(bytes.len()));
    let mut cursor = 0usize;
    parse_{entry}(bytes, &mut cursor, &mut tape)?;
    let cursor = skip_ws(bytes, cursor);
    if cursor != bytes.len() {{
        return Err(ParseError::at(cursor, "trailing characters"));
    }}
    Ok({prefix}Document::from_tape(tape.finish()))
}}

/// Conservative byte-proportional structural capacity bound.
#[inline]
fn structural_reserve(byte_len: usize) -> usize {{
    (byte_len / 4).max(16)
}}

/// Skip ASCII whitespace (the grammar's optional `?w` markers, uniform between
/// sequence elements in this whitespace-insensitive grammar).
#[inline]
fn skip_ws(bytes: &[u8], mut cursor: usize) -> usize {{
    while let Some(&byte) = bytes.get(cursor) {{
        if byte.is_ascii_whitespace() {{
            cursor += 1;
        }} else {{
            break;
        }}
    }}
    cursor
}}

/// Match an exact byte-literal at `cursor`, advancing past it on success.
#[inline]
fn match_literal<'i>(
    bytes: &'i [u8],
    cursor: &mut usize,
    literal: &[u8],
) -> Result<(), ParseError<'i>> {{
    if bytes[*cursor..].starts_with(literal) {{
        *cursor += literal.len();
        Ok(())
    }} else {{
        Err(ParseError::at(*cursor, "expected literal"))
    }}
}}

/// Match a regex leaf via the grammar-sanctioned delimiter-scan fallback: the
/// leaf head must be in `first`, then the body is the maximal run of bytes that
/// are neither ASCII whitespace nor a structural `LEAF_STOP` byte. A `'`-quoted
/// leaf (sheet name / string) consumes its balanced quote span first so an inner
/// delimiter does not truncate it. Records the leaf's start offset on the tape.
#[inline]
fn match_leaf<'i>(
    bytes: &'i [u8],
    cursor: &mut usize,
    tape: &mut TapeBuilder,
    first: &[u8],
    kind: u8,
    nullable: bool,
) -> Result<(), ParseError<'i>> {{
    let start = *cursor;
    let Some(&head) = bytes.get(start) else {{
        // End of input: a nullable leaf matches the empty string here.
        return if nullable {{
            Ok(())
        }} else {{
            Err(ParseError::at(start, "expected leaf"))
        }};
    }};
    if !first.is_empty() && !first.contains(&head) {{
        // The head is not in the leaf's first set: a nullable leaf still matches
        // (the empty alternative), consuming nothing.
        return if nullable {{
            Ok(())
        }} else {{
            Err(ParseError::at(start, "unexpected leaf head"))
        }};
    }}
    // The leaf's leading byte IS part of the leaf. The continuation class depends
    // on the head category:
    //  - TEXT head (alnum / `_` / `$` / quote): consume the maximal text-class
    //    run (`[A-Za-z0-9_.$]`, or a balanced quoted span) — identifiers, cell
    //    refs, numbers, strings, sheet prefixes.
    //  - PUNCT head (an operator byte, e.g. the entry `=?`): consume only the
    //    maximal run of bytes in the leaf's own first set — so a punctuation leaf
    //    does not swallow the text that follows it.
    // This is the grammar-sanctioned Span delimiter-scan (typed leaves deferred).
    let pos = if matches!(head, b'\'' | b'"') {{
        consume_quoted(bytes, start)
    }} else if is_text_byte(head) {{
        let mut p = start + 1;
        while let Some(&byte) = bytes.get(p) {{
            if is_text_byte(byte) {{
                p += 1;
            }} else if matches!(byte, b'\'' | b'"') {{
                p = consume_quoted(bytes, p);
            }} else {{
                break;
            }}
        }}
        p
    }} else {{
        let mut p = start + 1;
        while let Some(&byte) = bytes.get(p) {{
            if !first.is_empty() && first.contains(&byte) {{
                p += 1;
            }} else {{
                break;
            }}
        }}
        p
    }};
    tape.push_offset(start, OffsetFlags(kind));
    *cursor = pos;
    Ok(())
}}

/// Whether `byte` is a leaf text-class byte (`[A-Za-z0-9_.$]`) — the continuation
/// alphabet of identifier / cell-ref / number / sheet-prefix leaves.
#[inline]
fn is_text_byte(byte: u8) -> bool {{
    byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'.' | b'$')
}}

/// Consume a balanced quoted span beginning at a quote byte, honouring doubled
/// quotes (`''` / `""`) as escapes. Returns the index past the closing quote.
#[inline]
fn consume_quoted(bytes: &[u8], start: usize) -> usize {{
    let quote = bytes[start];
    let mut pos = start + 1;
    while pos < bytes.len() {{
        if bytes[pos] == quote {{
            if bytes.get(pos + 1).copied() == Some(quote) {{
                pos += 2;
                continue;
            }}
            return pos + 1;
        }}
        pos += 1;
    }}
    bytes.len()
}}

/// Record a node boundary (a rule's structural marker) at `offset`.
#[inline]
fn mark(tape: &mut TapeBuilder, offset: usize, kind: u8) {{
    tape.push_offset(offset, OffsetFlags(kind));
}}

"#,
    );
}

/// Emit one `parse_<rule>` recognizer.
fn render_rule(
    program: &SinkOnlyProgram,
    rule: &SinkOnlyRule,
    out: &mut String,
) -> Result<(), String> {
    let ident = rule_ident(&rule.name);
    let kind = super::view::rule_kind_tag(program, &rule.name);
    let _ = writeln!(
        out,
        "#[allow(clippy::needless_lifetimes, unused_variables)]\nfn parse_{ident}<'i>(\n    bytes: &'i [u8],\n    cursor: &mut usize,\n    tape: &mut TapeBuilder,\n) -> Result<(), ParseError<'i>> {{"
    );
    // A node boundary at the rule's entry offset (after ws skip).
    let _ = writeln!(out, "    *cursor = skip_ws(bytes, *cursor);");
    let _ = writeln!(out, "    let _node_start = *cursor;");
    let _ = writeln!(out, "    let _ = ({kind}u8, &mark);");
    let mut body = String::new();
    emit_expr(program, &rule.expr, &mut body, 1, kind)?;
    out.push_str(&body);
    out.push_str("    Ok(())\n}\n\n");
    Ok(())
}

fn indent(level: usize) -> String {
    "    ".repeat(level)
}

/// Emit the recognizer code for one `SinkOnlyExpr`. `node_kind` is the enclosing
/// rule's tape kind tag.
fn emit_expr(
    program: &SinkOnlyProgram,
    expr: &SinkOnlyExpr,
    out: &mut String,
    level: usize,
    node_kind: u8,
) -> Result<(), String> {
    let pad = indent(level);
    match expr {
        SinkOnlyExpr::Entry(inner) => {
            emit_expr(program, inner, out, level, node_kind)?;
        }
        SinkOnlyExpr::Seq(children) => {
            for (index, child) in children.iter().enumerate() {
                if index > 0 {
                    let _ = writeln!(out, "{pad}*cursor = skip_ws(bytes, *cursor);");
                }
                emit_expr(program, child, out, level, node_kind)?;
            }
        }
        SinkOnlyExpr::Alt { mode, branches } => {
            emit_alt(program, *mode, branches, out, level, node_kind)?;
        }
        SinkOnlyExpr::RepeatLoop { body, min } => {
            // CRITICAL progress guard: break when the cursor does not advance —
            // nullable repeats (func_args / lambda_params) infinite-loop without
            // it. `min` is enforced by counting matched iterations.
            let _ = writeln!(out, "{pad}{{");
            let _ = writeln!(out, "{pad}    let mut _count: u32 = 0;");
            let _ = writeln!(out, "{pad}    loop {{");
            let _ = writeln!(out, "{pad}        let _before = *cursor;");
            let _ = writeln!(out, "{pad}        let mut _probe = *cursor;");
            // Try the body speculatively over a probe cursor; commit on success.
            let _ = writeln!(out, "{pad}        let _r: Result<(), ParseError<'i>> = (|| {{");
            let mut probe = String::new();
            emit_expr_probe(program, body, &mut probe, level + 3, node_kind)?;
            out.push_str(&probe);
            let _ = writeln!(out, "{pad}            Ok(())");
            let _ = writeln!(out, "{pad}        }})();");
            let _ = writeln!(out, "{pad}        match _r {{");
            let _ = writeln!(out, "{pad}            Ok(()) => {{");
            let _ = writeln!(out, "{pad}                if _probe == _before {{ break; }}");
            let _ = writeln!(out, "{pad}                *cursor = _probe;");
            let _ = writeln!(out, "{pad}                _count += 1;");
            let _ = writeln!(out, "{pad}            }}");
            let _ = writeln!(out, "{pad}            Err(_) => break,");
            let _ = writeln!(out, "{pad}        }}");
            let _ = writeln!(out, "{pad}    }}");
            if *min > 0 {
                let _ = writeln!(
                    out,
                    "{pad}    if _count < {min} {{ return Err(ParseError::at(*cursor, \"too few repetitions\")); }}"
                );
            }
            let _ = writeln!(out, "{pad}}}");
        }
        SinkOnlyExpr::OptionalBranch(inner) => {
            // Save cursor, try inner over a probe, restore on fail (always Ok).
            let _ = writeln!(out, "{pad}{{");
            let _ = writeln!(out, "{pad}    let mut _probe = *cursor;");
            let _ = writeln!(out, "{pad}    let _r: Result<(), ParseError<'i>> = (|| {{");
            let mut probe = String::new();
            emit_expr_probe(program, inner, &mut probe, level + 2, node_kind)?;
            let _ = writeln!(probe, "{}Ok(())", indent(level + 2));
            out.push_str(&probe);
            let _ = writeln!(out, "{pad}    }})();");
            let _ = writeln!(out, "{pad}    if _r.is_ok() {{ *cursor = _probe; }}");
            let _ = writeln!(out, "{pad}}}");
        }
        SinkOnlyExpr::CallRule { callee } => {
            let callee_ident = rule_ident(callee);
            let _ = writeln!(out, "{pad}parse_{callee_ident}(bytes, cursor, tape)?;");
        }
        SinkOnlyExpr::ByteLiteral(bytes) => {
            let lit = byte_string_literal(bytes);
            let _ = writeln!(out, "{pad}match_literal(bytes, cursor, {lit})?;");
        }
        SinkOnlyExpr::RegexProgram { pattern, .. } => {
            let first = regex_first::first_bytes(pattern);
            let first_lit = byte_string_literal(&first.into_iter().collect::<Vec<_>>());
            // A nullable leaf pattern (e.g. `=?`, `\w*`) may match the empty
            // string — `match_leaf` must accept a zero-length span there.
            let nullable = regex_first::is_nullable(pattern);
            let _ = writeln!(
                out,
                "{pad}match_leaf(bytes, cursor, tape, {first_lit}, {node_kind}u8, {nullable})?;"
            );
        }
        SinkOnlyExpr::SpanMark { .. }
        | SinkOnlyExpr::TapeEmit
        | SinkOnlyExpr::DirectBuild(_)
        | SinkOnlyExpr::ValueProject
        | SinkOnlyExpr::Return => {
            // Tape bookkeeping / no-op for the recognizer. A DirectBuild records
            // a node boundary at the current cursor; the rest are pure markers.
            if matches!(expr, SinkOnlyExpr::DirectBuild(_)) {
                let _ = writeln!(out, "{pad}mark(tape, _node_start, {node_kind}u8);");
            }
        }
    }
    Ok(())
}

/// Emit the body of an Alt — ordered choice (PEG): try each branch over a probe
/// cursor, commit the first that succeeds, error if none do. This is correct for
/// BOTH `Dispatch` (disjoint first bytes — exactly one branch matches) and the
/// ambiguous towers (`primary`: func_call vs identifier share an ident head);
/// ordered try resolves them deterministically.
fn emit_alt(
    program: &SinkOnlyProgram,
    _mode: AltMode,
    branches: &[SinkOnlyExpr],
    out: &mut String,
    level: usize,
    node_kind: u8,
) -> Result<(), String> {
    let pad = indent(level);
    let _ = writeln!(out, "{pad}{{");
    let _ = writeln!(out, "{pad}    let _alt_start = *cursor;");
    let _ = writeln!(out, "{pad}    let mut _matched = false;");
    for branch in branches {
        let _ = writeln!(out, "{pad}    if !_matched {{");
        let _ = writeln!(out, "{pad}        let mut _probe = _alt_start;");
        let _ = writeln!(out, "{pad}        let _r: Result<(), ParseError<'i>> = (|| {{");
        let mut probe = String::new();
        emit_expr_probe(program, branch, &mut probe, level + 3, node_kind)?;
        let _ = writeln!(probe, "{}Ok(())", indent(level + 3));
        out.push_str(&probe);
        let _ = writeln!(out, "{pad}        }})();");
        let _ = writeln!(out, "{pad}        if _r.is_ok() {{");
        let _ = writeln!(out, "{pad}            *cursor = _probe;");
        let _ = writeln!(out, "{pad}            _matched = true;");
        let _ = writeln!(out, "{pad}        }}");
        let _ = writeln!(out, "{pad}    }}");
    }
    let _ = writeln!(
        out,
        "{pad}    if !_matched {{ return Err(ParseError::at(_alt_start, \"no alternative matched\")); }}"
    );
    let _ = writeln!(out, "{pad}}}");
    Ok(())
}

/// Emit an expression over the `_probe` cursor (used inside Alt/Optional/Repeat
/// speculative closures). Identical walk, but reads/writes `_probe` rather than
/// `*cursor`, so a failed branch leaves the real cursor untouched.
fn emit_expr_probe(
    program: &SinkOnlyProgram,
    expr: &SinkOnlyExpr,
    out: &mut String,
    level: usize,
    node_kind: u8,
) -> Result<(), String> {
    // The probe walk reuses the main emitter by re-pointing the cursor: emit the
    // body into a `&mut _probe` shadow. We achieve this by emitting the same code
    // but substituting `cursor` with `&mut _probe`. To keep ONE walk, the
    // recognizers take `cursor: &mut usize`; here we bind a local alias.
    let pad = indent(level);
    let _ = writeln!(out, "{pad}let cursor = &mut _probe;");
    emit_expr(program, expr, out, level, node_kind)
}
