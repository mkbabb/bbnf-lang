use ls_types::*;

use super::lowercase_first;
use crate::directives::hints::{extract_sep_string, extract_split_delim, hint_documentation};
use crate::state::DocumentState;

/// Check if the cursor is over a @pretty hint keyword or rule name.
pub(super) fn hover_pretty(state: &DocumentState, offset: usize) -> Option<Hover> {
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
