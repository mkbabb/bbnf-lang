use bbnf::generate::prettify::hints::{
    extract_sep_string, extract_split_delim, hint_documentation,
};
use ls_types::*;

use crate::analysis::{symbol_at_offset, SymbolAtOffset};
use crate::state::DocumentState;

pub fn hover(state: &DocumentState, position: Position) -> Option<Hover> {
    let offset = state.line_index.position_to_offset(position);

    // Check directive keyword hovers first.
    if let Some(hover) = hover_no_collapse(state, offset) {
        return Some(hover);
    }
    if let Some(hover) = hover_recover(state, offset) {
        return Some(hover);
    }
    if let Some(hover) = hover_pretty(state, offset) {
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

            // Add analysis info block.
            content.push_str("\n\n---\n");
            if let Some(first_label) = state.info.first_set_labels.get(&rule.name) {
                content.push_str(&format!("FIRST: {}\n\n", first_label));
            }
            let nullable = state.info.nullable_rules.contains(&rule.name);
            content.push_str(&format!(
                "Nullable: {}\n\n",
                if nullable { "yes" } else { "no" }
            ));
            if let Some(cycle_path) = state.info.cyclic_rule_paths.get(&rule.name) {
                content.push_str(&format!("Cyclic: yes ({})\n", cycle_path));
            }

            // Show @pretty hints if any.
            for p in &state.info.pretties {
                if p.rule_name == rule.name {
                    content.push_str(&format!("\n@pretty: `{}`\n", p.hints.join(" ")));
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
                // Add FIRST set for references too.
                if let Some(first_label) = state.info.first_set_labels.get(&def.name) {
                    s.push_str(&format!("\n\n---\nFIRST: {}", first_label));
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

/// Check if the cursor is over the @no_collapse keyword or its rule name.
fn hover_no_collapse(state: &DocumentState, offset: usize) -> Option<Hover> {
    for nc in &state.info.no_collapses {
        // Check keyword span: "@no_collapse" is 13 chars.
        let kw_end = nc.span.0 + 13;
        if offset >= nc.span.0 && offset <= kw_end {
            // Look up rule definition for context.
            let rule_def = state
                .info
                .rule_index
                .get(&nc.rule_name)
                .map(|&i| &state.info.rules[i]);

            let mut content = String::from(
                "### `@no_collapse` — Span Preservation\n\n\
                 Prevents the parser from merging consecutive spans into a single `Span` \
                 for rule `",
            );
            content.push_str(&nc.rule_name);
            content.push_str("`.\n\n");

            content.push_str(
                "**Without** `@no_collapse`: repetitions (`*`, `+`) produce a single merged `Span`, \
                 and optionals (`?`) produce `Span` instead of `Option<Span>`.\n\n\
                 **With** `@no_collapse`: repetitions produce `Vec<Span>` and optionals produce \
                 `Option<Span>` — preserving individual element boundaries for formatting.\n\n",
            );

            content.push_str(
                "Use when `@pretty` directives need to format each repeated element independently \
                 (e.g. with `sep(\"...\")` or `split(\"...\")`).\n",
            );

            if let Some(def) = rule_def {
                content.push_str(&format!("\n---\n```bbnf\n{} = {}\n```", def.name, def.rhs_text));
            }

            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: Some(state.line_index.span_to_range(nc.span.0, kw_end)),
            });
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
            sep_str, rule_name, sep_str,
            sep_str, sep_str.trim_end(),
            sep_str,
            rule_name, all_hints.join(" ")
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
            delim, rule_name, delim,
            rule_name, all_hints.join(" ")
        );
    }

    if let Some(doc) = hint_documentation(hint) {
        format!(
            "### `{}` — `@pretty` Hint\n\n{}\n\n\
             Applied to rule `{}`.\n\n\
             ```bbnf\n@pretty {} {} ;\n```",
            hint, doc, rule_name,
            rule_name, all_hints.join(" ")
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
            } else if let Some(desc) = bbnf::generate::prettify::hints::hint_description(hint) {
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
