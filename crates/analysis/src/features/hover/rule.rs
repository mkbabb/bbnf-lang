use ls_types::*;

use crate::state::{DocumentState, RuleInfo};

/// Build hover content for a rule's LHS (definition site).
pub(super) fn build_rule_definition_hover(state: &DocumentState, rule: &RuleInfo) -> Hover {
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
        if let Some(ref ty) = ir_meta.projected_type {
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

    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: content,
        }),
        range: Some(
            state
                .line_index
                .span_to_range(rule.name_span.0, rule.name_span.1),
        ),
    }
}

/// Build hover content for a nonterminal reference site.
pub(super) fn build_rule_reference_hover(state: &DocumentState, name: &str) -> Hover {
    // Look up the definition.
    let def = state
        .info
        .rule_index
        .get(name)
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
            if let Some(ref ty) = ir_meta.projected_type {
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

    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: content,
        }),
        range: None,
    }
}
