use bbnf::generate::prettify::hints::hint_description;
use tower_lsp_server::ls_types::*;

use crate::analysis::{symbol_at_offset, SymbolAtOffset};
use crate::state::DocumentState;

pub fn hover(state: &DocumentState, position: Position) -> Option<Hover> {
    let offset = state.line_index.position_to_offset(position);

    // Check @pretty directive hover first.
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
            content.push_str(&format!("Nullable: {}\n\n", if nullable { "yes" } else { "no" }));
            if let Some(cycle_path) = state.info.cyclic_rule_paths.get(&rule.name) {
                content.push_str(&format!("Cyclic: yes ({})\n", cycle_path));
            }

            // Show @pretty hints if any.
            for p in &state.info.pretties {
                if p.rule_name == rule.name {
                    content.push_str(&format!(
                        "\n@pretty: `{}`\n",
                        p.hints.join(" ")
                    ));
                    break;
                }
            }

            Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: Some(state.line_index.span_to_range(rule.name_span.0, rule.name_span.1)),
            })
        }
        SymbolAtOffset::RuleReference { name, .. } => {
            // Look up the definition.
            let def = state.info.rule_index.get(&name).map(|&i| &state.info.rules[i]);

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

/// Check if the cursor is over a @pretty hint keyword or rule name.
fn hover_pretty(state: &DocumentState, offset: usize) -> Option<Hover> {
    for pretty in &state.info.pretties {
        // Check hint keywords.
        for (i, hint) in pretty.hints.iter().enumerate() {
            if let Some(&(start, end)) = pretty.hint_spans.get(i) {
                if offset >= start && offset <= end {
                    let desc = hint_description(hint)
                        .unwrap_or_else(|| panic!("Unknown @pretty hint encountered in hover: `{}`", hint));
                    let content = format!(
                        "`@pretty` hint: **{}**\n\n{}",
                        hint, desc
                    );
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

        // Check rule name in @pretty directive.
        let (rs, re) = pretty.rule_name_span;
        if offset >= rs && offset <= re {
            // Show the rule definition + applied hints.
            let def = state.info.rule_index.get(&pretty.rule_name)
                .map(|&i| &state.info.rules[i]);

            let content = if let Some(def) = def {
                format!(
                    "```bbnf\n{} = {}\n```\n\n@pretty hints: `{}`",
                    def.name,
                    def.rhs_text,
                    pretty.hints.join(" ")
                )
            } else {
                format!(
                    "`{}` — undefined rule\n\n@pretty hints: `{}`",
                    pretty.rule_name,
                    pretty.hints.join(" ")
                )
            };

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
