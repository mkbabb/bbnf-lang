use crate::directives::hints::{extract_sep_string, extract_split_delim, hint_documentation};
use ls_types::*;

use crate::analysis::{SymbolAtOffset, symbol_at_offset};
use crate::state::DocumentState;

pub fn hover(state: &DocumentState, position: Position) -> Option<Hover> {
    let offset = state.line_index.position_to_offset(position);

    // Check import hovers first.
    if let Some(hover) = hover_import(state, offset) {
        return Some(hover);
    }

    // Check directive keyword hovers first.
    if let Some(hover) = hover_recover(state, offset) {
        return Some(hover);
    }
    if let Some(hover) = hover_pretty(state, offset) {
        return Some(hover);
    }
    if let Some(hover) = hover_debug(state, offset) {
        return Some(hover);
    }
    if let Some(hover) = hover_ws(state, offset) {
        return Some(hover);
    }

    let symbol = symbol_at_offset(&state.info, offset)?;

    match symbol {
        SymbolAtOffset::RuleDefinition(rule) => {
            let ref_count: usize = state
                .info
                .rules
                .iter()
                .flat_map(|r| &r.references)
                .filter(|r| r.name == rule.name)
                .count();

            let mut content = format!(
                "```bbnf\n{} = {}\n```\n\n{} reference{}",
                rule.name,
                rule.rhs_text,
                ref_count,
                if ref_count == 1 { "" } else { "s" }
            );

            // Compact analysis summary line.
            content.push_str("\n\n---\n");
            {
                let mut parts: Vec<String> = Vec::new();

                if let Some(first_label) = state.info.first_set_labels.get(&rule.name) {
                    if first_label != "∅" && !first_label.is_empty() {
                        parts.push(format!("FIRST {}", first_label));
                    }
                }
                if let Some(ir_meta) = state.info.ir_meta.get(&rule.name) {
                    if let Some(ref follow) = ir_meta.follow_set_label {
                        if follow != "∅" && !follow.is_empty() {
                            parts.push(format!("FOLLOW {}", follow));
                        }
                    }
                }
                if state.info.nullable_rules.contains(&rule.name) {
                    parts.push("nullable".into());
                }
                if let Some(cycle_path) = state.info.cyclic_rule_paths.get(&rule.name) {
                    parts.push(format!("cyclic ({})", cycle_path));
                }
                if !parts.is_empty() {
                    content.push_str(&parts.join(" · "));
                    content.push('\n');
                }
            }

            // IR-derived metadata — compact line.
            if let Some(ir_meta) = state.info.ir_meta.get(&rule.name) {
                let mut tags: Vec<&str> = Vec::new();
                if ir_meta.has_dispatch {
                    tags.push("dispatch");
                }
                if ir_meta.span_eligible {
                    tags.push("span");
                }
                if ir_meta.memo_strategy != "None" {
                    tags.push("memo");
                }

                let mut line = String::new();
                if !tags.is_empty() {
                    line.push_str(&tags.join(" · "));
                }
                if let Some(ref ty) = ir_meta.inferred_type {
                    if !line.is_empty() {
                        line.push_str(" · ");
                    }
                    line.push_str(&format!("`{}`", ty));
                }
                if !line.is_empty() {
                    content.push_str(&format!("\n{}\n", line));
                }
            }

            // @pretty hints.
            for p in &state.info.pretties {
                if p.rule_name == rule.name {
                    content.push_str(&format!("\n@pretty `{}`\n", p.hints.join(" ")));
                    break;
                }
            }

            Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: Some(
                    state
                        .line_index
                        .span_to_range(rule.name_span.0, rule.name_span.1),
                ),
            })
        }
        SymbolAtOffset::RuleReference { name, .. } => {
            // Look up the definition.
            let def = state
                .info
                .rule_index
                .get(&name)
                .map(|&i| &state.info.rules[i]);

            let content = if let Some(def) = def {
                let mut s = format!("```bbnf\n{} = {}\n```", def.name, def.rhs_text);

                // Compact summary — same format as RuleDefinition.
                let mut parts: Vec<String> = Vec::new();
                if let Some(first_label) = state.info.first_set_labels.get(&def.name) {
                    if first_label != "∅" && !first_label.is_empty() {
                        parts.push(format!("FIRST {}", first_label));
                    }
                }
                if let Some(ir_meta) = state.info.ir_meta.get(&def.name) {
                    if let Some(ref follow) = ir_meta.follow_set_label {
                        if follow != "∅" && !follow.is_empty() {
                            parts.push(format!("FOLLOW {}", follow));
                        }
                    }
                }
                if state.info.nullable_rules.contains(&def.name) {
                    parts.push("nullable".into());
                }
                if !parts.is_empty() {
                    s.push_str(&format!("\n\n---\n{}", parts.join(" · ")));
                }
                if let Some(ir_meta) = state.info.ir_meta.get(&def.name) {
                    let mut tags: Vec<&str> = Vec::new();
                    if ir_meta.has_dispatch {
                        tags.push("dispatch");
                    }
                    if ir_meta.span_eligible {
                        tags.push("span");
                    }
                    if let Some(ref ty) = ir_meta.inferred_type {
                        let mut line = tags.join(" · ");
                        if !line.is_empty() {
                            line.push_str(" · ");
                        }
                        line.push_str(&format!("`{}`", ty));
                        s.push_str(&format!("\n\n{}", line));
                    } else if !tags.is_empty() {
                        s.push_str(&format!("\n\n{}", tags.join(" · ")));
                    }
                }

                s
            } else {
                format!("`{}` — undefined rule", name)
            };

            Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: None,
            })
        }
    }
}

/// Check if the cursor is over an @import directive or a selectively imported name.
fn hover_import(state: &DocumentState, offset: usize) -> Option<Hover> {
    for imp in &state.info.imports {
        if offset < imp.span.0 || offset > imp.span.1 {
            continue;
        }

        // Check "@import" keyword (7 chars).
        let kw_end = imp.span.0 + 7;
        if offset >= imp.span.0 && offset < kw_end {
            let content = format!(
                "### `@import` — Module Import\n\n\
                 Imports rules from `\"{}\"`.\n\n\
                 - **Glob**: `@import \"path\" ;` imports all rules.\n\
                 - **Selective**: `@import {{ a, b }} from \"path\" ;` imports only named rules \
                 (transitive local deps are expanded automatically).\n",
                imp.path
            );
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: Some(state.line_index.span_to_range(imp.span.0, kw_end)),
            });
        }

        // Check selectively imported names.
        if let Some(ref items) = imp.items {
            for item in items {
                if offset >= item.span.0 && offset <= item.span.1 {
                    let content = format!("Imported rule `{}` from `\"{}\"`", item.name, imp.path);
                    return Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: content,
                        }),
                        range: Some(state.line_index.span_to_range(item.span.0, item.span.1)),
                    });
                }
            }
        }
    }
    None
}

/// Check if the cursor is over the @recover keyword or its directive body.
fn hover_recover(state: &DocumentState, offset: usize) -> Option<Hover> {
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

/// Check if the cursor is over a @pretty hint keyword or rule name.
fn hover_pretty(state: &DocumentState, offset: usize) -> Option<Hover> {
    for pretty in &state.info.pretties {
        // Check hint keywords.
        for (i, hint) in pretty.hints.iter().enumerate() {
            if let Some(&(start, end)) = pretty.hint_spans.get(i) {
                if offset >= start && offset <= end {
                    let content = build_hint_hover(hint, &pretty.rule_name, &pretty.hints);
                    return Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: content,
                        }),
                        range: Some(state.line_index.span_to_range(start, end)),
                    });
                }
            }
        }

        // Check "@pretty" keyword itself (7 chars).
        let kw_end = pretty.span.0 + 7;
        if offset >= pretty.span.0 && offset < kw_end {
            let content = build_pretty_directive_hover(state, pretty);
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: Some(state.line_index.span_to_range(pretty.span.0, kw_end)),
            });
        }

        // Check rule name in @pretty directive.
        let (rs, re) = pretty.rule_name_span;
        if offset >= rs && offset <= re {
            let content = build_pretty_directive_hover(state, pretty);
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: Some(state.line_index.span_to_range(rs, re)),
            });
        }
    }
    None
}

/// Check if the cursor is over the @debug keyword or its rule name.
fn hover_debug(state: &DocumentState, offset: usize) -> Option<Hover> {
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
fn hover_ws(state: &DocumentState, offset: usize) -> Option<Hover> {
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

/// Build rich hover content for a single @pretty hint keyword.
fn build_hint_hover(hint: &str, rule_name: &str, all_hints: &[String]) -> String {
    if let Some(sep_str) = extract_sep_string(hint) {
        return format!(
            "### `sep(\"{}\")` — Custom Separator\n\n\
             Joins elements of `{}` with the separator `\"{}\"`.\n\n\
             When combined with `group`: renders `\"{}\"` inline when the group fits, \
             or `\"{}\"` + newline when the group breaks (trailing whitespace is trimmed \
             on the break branch).\n\n\
             Without `group`: uses `\"{}\"` as a flat separator between all elements.\n\n\
             ```bbnf\n@pretty {} {} ;\n```",
            sep_str,
            rule_name,
            sep_str,
            sep_str,
            sep_str.trim_end(),
            sep_str,
            rule_name,
            all_hints.join(" ")
        );
    }

    if let Some(delim) = extract_split_delim(hint) {
        return format!(
            "### `split(\"{}\")` — Format-Time Splitting\n\n\
             Splits opaque `Span` text from `{}` on the delimiter `\"{}\"` at format time.\n\n\
             The split is **depth-aware**: respects `()`, `[]` nesting and `\"\"`, `''` \
             quoting — only top-level occurrences of the delimiter trigger a split.\n\n\
             Each resulting segment becomes a separate Doc element, which can then be \
             joined with `sep(\"...\")` or formatted with `group`/`indent`.\n\n\
             Uses `memchr` fast-path: skips the full scan when the delimiter isn't present.\n\n\
             ```bbnf\n@pretty {} {} ;\n```",
            delim,
            rule_name,
            delim,
            rule_name,
            all_hints.join(" ")
        );
    }

    if let Some(doc) = hint_documentation(hint) {
        format!(
            "### `{}` — `@pretty` Hint\n\n{}\n\n\
             Applied to rule `{}`.\n\n\
             ```bbnf\n@pretty {} {} ;\n```",
            hint,
            doc,
            rule_name,
            rule_name,
            all_hints.join(" ")
        )
    } else {
        format!("`@pretty` hint: **{}**\n\nUnknown hint.", hint)
    }
}

/// Build hover content for the @pretty directive keyword or rule name.
fn build_pretty_directive_hover(
    state: &DocumentState,
    pretty: &crate::state::pretty::PrettyInfo,
) -> String {
    let def = state
        .info
        .rule_index
        .get(&pretty.rule_name)
        .map(|&i| &state.info.rules[i]);

    let mut content = format!(
        "### `@pretty` — Formatting Directive\n\n\
         Controls how rule `{}` is pretty-printed by the formatter.\n\n",
        pretty.rule_name
    );

    // Show the directive itself.
    content.push_str(&format!(
        "```bbnf\n@pretty {} {} ;\n```\n\n",
        pretty.rule_name,
        pretty.hints.join(" ")
    ));

    // Describe each hint in the combination.
    if !pretty.hints.is_empty() {
        content.push_str("**Applied hints:**\n\n");
        for hint in &pretty.hints {
            if let Some(sep_str) = extract_sep_string(hint) {
                content.push_str(&format!(
                    "- `sep(\"{}\")` — joins elements with `\"{}\"`\n",
                    sep_str, sep_str
                ));
            } else if let Some(delim) = extract_split_delim(hint) {
                content.push_str(&format!(
                    "- `split(\"{}\")` — splits Span text on `\"{}\"` (depth-aware)\n",
                    delim, delim
                ));
            } else if let Some(desc) = crate::directives::hints::hint_description(hint) {
                content.push_str(&format!("- `{}` — {}\n", hint, lowercase_first(desc)));
            }
        }
        content.push('\n');
    }

    // Show the rule definition.
    if let Some(def) = def {
        content.push_str(&format!(
            "---\n```bbnf\n{} = {}\n```",
            def.name, def.rhs_text
        ));
    }

    content
}

/// Lowercase the first character of a string (for inline descriptions).
fn lowercase_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_lowercase().to_string() + chars.as_str(),
    }
}
