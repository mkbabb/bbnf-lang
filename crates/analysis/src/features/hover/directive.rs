use ls_types::*;

use crate::state::DocumentState;

/// Check if the cursor is over the @recover keyword or its directive body.
pub(super) fn hover_recover(state: &DocumentState, offset: usize) -> Option<Hover> {
    for rec in &state.info.recovers {
        // Hover over the entire directive span (keyword + rule name + sync expr).
        if offset >= rec.span.0 && offset <= rec.span.1 {
            // Check if specifically over the rule name — delegate to symbol_at_offset.
            if offset >= rec.rule_name_span.0 && offset <= rec.rule_name_span.1 {
                // Don't handle here — let symbol_at_offset show the rule definition.
                continue;
            }

            let rule_def = state
                .info
                .rule_index
                .get(&rec.rule_name)
                .map(|&i| &state.info.rules[i]);

            let mut content = format!(
                "### `@recover` directive — Error Recovery\n\n\
                 Wraps rule `{}` with error recovery. When parsing fails mid-rule, the parser:\n\n\
                 1. Records the error with position and expected tokens\n\
                 2. Skips forward to the **sync expression**\n\
                 3. Produces a `Recovered` sentinel node\n\
                 4. Continues parsing subsequent rules\n\n",
                rec.rule_name
            );

            if !rec.sync_expr_text.is_empty() {
                content.push_str(&format!(
                    "**Sync expression:** `{}`\n\n\
                     The parser advances input until this expression matches, then resumes \
                     normal parsing from that point.\n\n",
                    rec.sync_expr_text
                ));
            }

            content.push_str(
                "This enables **multi-error diagnostics** — the parser reports all errors \
                 in a single pass instead of stopping at the first failure.\n",
            );

            if let Some(def) = rule_def {
                content.push_str(&format!(
                    "\n---\n```bbnf\n{} = {}\n```",
                    def.name, def.rhs_text
                ));
            }

            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: Some(state.line_index.span_to_range(rec.span.0, rec.span.1)),
            });
        }
    }
    None
}

/// Check if the cursor is over the @debug keyword or its rule name.
pub(super) fn hover_debug(state: &DocumentState, offset: usize) -> Option<Hover> {
    for dbg in &state.info.debugs {
        // Check keyword span: "@debug" is 6 chars.
        let kw_end = dbg.span.0 + 6;
        if offset >= dbg.span.0 && offset <= kw_end {
            let content = if dbg.rule_name == "*" {
                "### `@debug *` — Debug All Rules\n\n\
                 Instruments **all** rules for debug tracing.\n\n\
                 Emits trace output in compiled paths, `DebugBreak` opcodes in bytecode VM."
                    .to_string()
            } else {
                let rule_def = state
                    .info
                    .rule_index
                    .get(&dbg.rule_name)
                    .map(|&i| &state.info.rules[i]);

                let mut s = format!(
                    "### `@debug` — Debug Tracing\n\n\
                     Instruments `{}` for debug tracing. \
                     Emits trace output in compiled paths, `DebugBreak` opcodes in bytecode VM.\n",
                    dbg.rule_name
                );

                if let Some(def) = rule_def {
                    s.push_str(&format!(
                        "\n---\n```bbnf\n{} = {}\n```",
                        def.name, def.rhs_text
                    ));
                }
                s
            };

            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: Some(state.line_index.span_to_range(dbg.span.0, kw_end)),
            });
        }

        // Check rule name span — delegate to symbol_at_offset.
        if dbg.rule_name != "*" && offset >= dbg.rule_name_span.0 && offset <= dbg.rule_name_span.1
        {
            continue;
        }
    }
    None
}

/// Check if the cursor is over the @ws directive.
pub(super) fn hover_ws(state: &DocumentState, offset: usize) -> Option<Hover> {
    let ws = state.info.ws_pattern.as_ref()?;

    // Check keyword span: "@ws" is 3 chars.
    let kw_end = ws.span.0 + 3;
    if offset >= ws.span.0 && offset <= ws.span.1 {
        let content = format!(
            "### `@ws` — Custom Whitespace Pattern\n\n\
             Overrides `?w` (optional whitespace) to use pattern: `/{}/`.\n\n\
             Enables comment-aware whitespace scanning. When set, every `?w` \
             operator compiles to this regex instead of the default `\\s*`.\n",
            ws.pattern
        );

        return Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: content,
            }),
            range: Some(state.line_index.span_to_range(ws.span.0, kw_end)),
        });
    }

    None
}
