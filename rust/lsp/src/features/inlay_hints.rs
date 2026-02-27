use tower_lsp_server::ls_types::*;

use crate::analysis::span_to_range;
use crate::state::DocumentState;

/// Produce inlay hints showing FIRST set and nullability info at rule definitions.
///
/// Suppresses trivial hints where the FIRST set is obvious:
/// - Rules with no nonterminal references and a single FIRST element (e.g., `div = "/"`)
/// - Rules that are a single nonterminal alias (e.g., `colorPercentage = percentage`)
pub fn inlay_hints(state: &DocumentState, range: Range) -> Vec<InlayHint> {
    let mut hints = Vec::new();

    for rule in &state.info.rules {
        let rule_range = span_to_range(&state.text, rule.name_span.0, rule.name_span.1);

        // Only produce hints for rules within the requested range.
        if rule_range.start.line < range.start.line || rule_range.start.line > range.end.line {
            continue;
        }

        // FIRST set label.
        if let Some(first_label) = state.info.first_set_labels.get(&rule.name) {
            let nullable = state.info.nullable_rules.contains(&rule.name);
            let ref_count = rule.references.len();

            // Suppress trivial hints:
            // 1. Pure terminal rule with a single FIRST char (e.g., `div = "/"`)
            // 2. Single nonterminal alias (e.g., `colorPercentage = percentage`)
            if !nullable {
                let first_count = first_label.matches('\'').count() / 2; // count quoted chars
                if first_count <= 1 && ref_count == 0 {
                    continue; // trivial terminal rule
                }
                if first_count <= 1 && ref_count == 1 {
                    continue; // single alias, just look at the referenced rule
                }
            }

            let label = if nullable {
                format!(" FIRST: {}  (nullable)", first_label)
            } else {
                format!(" FIRST: {}", first_label)
            };

            hints.push(InlayHint {
                position: Position::new(rule_range.start.line, rule_range.end.character),
                label: InlayHintLabel::String(label),
                kind: Some(InlayHintKind::TYPE),
                text_edits: None,
                tooltip: Some(InlayHintTooltip::String(format!(
                    "Characters that can begin a parse of `{}`",
                    rule.name
                ))),
                padding_left: Some(true),
                padding_right: None,
                data: None,
            });
        }
    }

    hints
}
