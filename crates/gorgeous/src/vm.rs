//! Pretty backend: interpret `Value` parse tree + `PrettyHints` from IR → FmtOp buffer → formatted string.
//!
//! This is a runtime interpreter — no codegen. It walks the parse tree produced by the
//! bytecode VM, consults the `@pretty` directives stored in `GrammarIR.rules[].meta.directives.pretty`,
//! and builds a flat FmtOp buffer which is then rendered to a string.

use bbnf_ir::interpreter::Value;
use bbnf_ir::{GrammarIR, PrettyHints};
use pprint::FmtBuilder;

use crate::PrinterConfig;

/// Format a parse result using the grammar's `@pretty` hints.
pub fn format_value(
    ir: &GrammarIR,
    value: &Value,
    input: &str,
    config: &PrinterConfig,
) -> Option<String> {
    let mut builder = FmtBuilder::with_capacity(input.len() * 2);
    if !value_to_ops(ir, value, input, &mut builder) {
        return None;
    }
    let ops = builder.finish();
    Some(pprint::render(&ops, config.to_printer()))
}

/// Format a parse result (legacy alias).
pub fn format_ir(
    ir: &GrammarIR,
    value: &Value,
    input: &str,
    config: &PrinterConfig,
) -> Option<String> {
    format_value(ir, value, input, config)
}

/// Walk a Value tree, emitting FmtOps into the builder. Returns false if value is Nil/empty.
fn value_to_ops<'a>(
    ir: &'a GrammarIR,
    value: &Value,
    input: &'a str,
    builder: &mut FmtBuilder<'a>,
) -> bool {
    match value {
        Value::Nil => false,

        Value::Span(start, end) => {
            let text = &input[*start as usize..*end as usize];
            if text.is_empty() {
                false
            } else if text
                .bytes()
                .all(|b| matches!(b, b' ' | b'\t' | b'\n' | b'\r'))
            {
                // Whitespace-only spans are structural — skip emission.
                true
            } else {
                builder.text(text);
                true
            }
        }

        Value::Tagged {
            tag,
            span,
            children,
        } => {
            let rule = ir.rules.iter().find(|r| r.name == *tag);
            let hints = rule.and_then(|r| r.meta.directives.pretty.as_ref());
            let span_text = &input[span.0 as usize..span.1 as usize];

            // Check if children cover the full span (gaps from >> / << or ?w).
            let children_span = children_span_range(children.iter());
            let has_gaps = children_span.map_or(true, |(cs, ce)| {
                let start_gap = &input[span.0 as usize..cs as usize];
                let end_gap = &input[ce as usize..span.1 as usize];
                !start_gap.bytes().all(|b| b.is_ascii_whitespace())
                    || !end_gap.bytes().all(|b| b.is_ascii_whitespace())
            });

            // Count non-nil children.
            let child_count = children.iter().filter(|c| !matches!(c, Value::Nil)).count();

            // Fall back to span text when children don't cover the full span.
            if child_count == 0 || (has_gaps && hints.is_none()) {
                if span_text.is_empty() {
                    return false;
                }
                emit_with_hints(builder, hints, |b| {
                    b.text(span_text);
                });
                return true;
            }

            // Flatten Array children when the node has hints.
            let flat_children: Vec<&Value> = if hints.is_some() {
                let mut flat = Vec::with_capacity(children.len());
                for child in children.iter() {
                    match child {
                        Value::Array(items) if items.len() > 1 => {
                            flat.extend(items.iter());
                        }
                        _ => flat.push(child),
                    }
                }
                flat
            } else {
                children.iter().collect()
            };

            // Detect group+indent wrapped pattern (head + content + close delimiter).
            if let Some(h) = hints {
                if h.group && h.indent && flat_children.len() >= 2 {
                    let last_is_close = match flat_children.last() {
                        Some(Value::Span(s, e)) => {
                            let text = &input[*s as usize..*e as usize];
                            matches!(text, ")" | "}" | "]" | ">")
                        }
                        _ => false,
                    };
                    if last_is_close {
                        builder.group_open();
                        // Head (e.g., function name + open paren)
                        value_to_ops(ir, flat_children[0], input, builder);
                        builder.indent_open();
                        builder.break_line();
                        // Inner content
                        emit_children_with_sep(
                            ir,
                            &flat_children[1..flat_children.len() - 1],
                            input,
                            builder,
                            hints,
                        );
                        builder.indent_close();
                        builder.break_line();
                        // Close delimiter
                        value_to_ops(ir, flat_children.last().unwrap(), input, builder);
                        builder.group_close();
                        return true;
                    }
                }
            }

            // Combine children based on hints.
            emit_with_hints(builder, hints, |b| {
                emit_children_with_sep(ir, &flat_children, input, b, hints);
            });
            true
        }

        Value::Array(items) => {
            let mut any = false;
            for item in items.iter() {
                if value_to_ops(ir, item, input, builder) {
                    any = true;
                }
            }
            any
        }
    }
}

/// Emit children with separator determined by hints.
fn emit_children_with_sep<'a>(
    ir: &'a GrammarIR,
    children: &[&Value],
    input: &'a str,
    builder: &mut FmtBuilder<'a>,
    hints: Option<&PrettyHints>,
) {
    let action = sep_action_from_hints(hints);
    let mut first = true;
    for child in children {
        if matches!(child, Value::Nil) {
            continue;
        }
        let cp = builder.checkpoint();
        if !first {
            emit_separator(builder, action);
        }
        let ok = value_to_ops(ir, child, input, builder);
        if !ok {
            builder.restore(cp);
            continue;
        }
        first = false;
    }
}

/// Pre-computed separator action — avoids re-matching PrettyHints booleans per child.
#[derive(Clone, Copy)]
enum SepAction {
    None,
    Hardline,
    DoubleHardline,
    Softline,
    Space,
    /// Inline flat separator bytes (max 8). Break mode = newline.
    Sep([u8; 8], u8),
}

fn sep_action_from_hints(hints: Option<&PrettyHints>) -> SepAction {
    let Some(h) = hints else {
        return SepAction::None;
    };
    if h.off || h.compact {
        return SepAction::None;
    }
    if let Some(ref sep_str) = h.sep {
        if sep_str.len() <= 8 {
            let mut buf = [0u8; 8];
            buf[..sep_str.len()].copy_from_slice(sep_str.as_bytes());
            return SepAction::Sep(buf, sep_str.len() as u8);
        }
        // >8-byte separators: fall through to default (shouldn't happen in practice)
    }
    if h.blankline {
        SepAction::DoubleHardline
    } else if h.block || h.hardbreak || h.fast {
        SepAction::Hardline
    } else if h.nobreak {
        SepAction::Space
    } else if h.softbreak {
        SepAction::Softline
    } else {
        SepAction::None
    }
}

/// Emit separator between items using pre-computed action.
fn emit_separator(builder: &mut FmtBuilder<'_>, action: SepAction) {
    match action {
        SepAction::None => {}
        SepAction::Hardline => {
            builder.hardline();
        }
        SepAction::DoubleHardline => {
            builder.hardline();
            builder.hardline();
        }
        SepAction::Softline => {
            builder.softline();
        }
        SepAction::Space => {
            builder.text(" ");
        }
        SepAction::Sep(flat, flat_len) => {
            // Sep with empty brk: flat → emit flat bytes, break → newline.
            let flat_str = std::str::from_utf8(&flat[..flat_len as usize]).unwrap_or("");
            builder.sep(flat_str, "");
        }
    }
}

/// Apply structural hints (group, indent) around content.
fn emit_with_hints<'a>(
    builder: &mut FmtBuilder<'a>,
    hints: Option<&PrettyHints>,
    content: impl FnOnce(&mut FmtBuilder<'a>),
) {
    let Some(h) = hints else {
        content(builder);
        return;
    };

    if h.off {
        content(builder);
        return;
    }

    if h.group {
        builder.group_open();
    }
    if h.indent && !h.group {
        builder.indent_open();
        builder.hardline();
    }

    content(builder);

    if h.indent && !h.group {
        builder.indent_close();
        builder.hardline();
    }
    if h.group {
        builder.group_close();
    }
}

/// Compute the (min_start, max_end) span range of all children.
fn children_span_range<'a>(children: impl Iterator<Item = &'a Value>) -> Option<(u32, u32)> {
    let mut min_start = u32::MAX;
    let mut max_end = 0u32;
    let mut any = false;
    for child in children {
        match child {
            Value::Span(s, e) => {
                min_start = min_start.min(*s);
                max_end = max_end.max(*e);
                any = true;
            }
            Value::Tagged { span, .. } => {
                min_start = min_start.min(span.0);
                max_end = max_end.max(span.1);
                any = true;
            }
            Value::Array(items) => {
                if let Some((s, e)) = children_span_range(items.iter()) {
                    min_start = min_start.min(s);
                    max_end = max_end.max(e);
                    any = true;
                }
            }
            Value::Nil => {}
        }
    }
    if any {
        Some((min_start, max_end))
    } else {
        None
    }
}
