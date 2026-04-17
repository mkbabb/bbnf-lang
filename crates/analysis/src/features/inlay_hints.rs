use ls_types::*;

use crate::state::DocumentState;

/// True when `rhs_text` is the bare reference `ref_name` with no
/// structural decoration. Used by [`inlay_hints`] to distinguish
/// genuine alias rules from composite rules whose body happens to
/// contain a single nonterminal reference. The check is purely
/// textual — `RuleInfo::rhs_text` already comes from
/// `format_expression_short`, so leading / trailing whitespace is
/// the only nuisance to strip.
fn is_bare_ref_alias(rhs_text: &str, ref_name: &str) -> bool {
    rhs_text.trim() == ref_name
}

/// Produce inlay hints showing FIRST set and nullability info at rule definitions.
///
/// Suppresses trivial hints where the FIRST set is obvious:
/// - Rules with no nonterminal references and a single FIRST element (e.g., `div = "/"`)
/// - Rules that are a true single-nonterminal alias (e.g.,
///   `colorPercentage = percentage`) — the RHS is a bare reference
///   with no structural decoration (Seq / Repeat / Alt / literals).
pub fn inlay_hints(state: &DocumentState, range: Range) -> Vec<InlayHint> {
    let mut hints = Vec::new();

    for rule in &state.info.rules {
        let rule_range = state
            .line_index
            .span_to_range(rule.name_span.0, rule.name_span.1);

        // Only produce hints for rules within the requested range.
        if rule_range.start.line < range.start.line || rule_range.start.line > range.end.line {
            continue;
        }

        // FIRST set label.
        if let Some(first_label) = state.info.first_set_labels.get(&rule.name) {
            let nullable = state.info.nullable_rules.contains(&rule.name);
            let ref_count = rule.references.len();

            // Suppress trivial hints where FIRST is obvious from reading the rule:
            // - Pure terminal rules (no nonterminal refs): FIRST is just the first
            //   char of each literal/regex, which you can see directly
            // - True single-nonterminal alias (bare Ref RHS): the rule reads
            //   like `name = otherName ;` — FIRST is the same as the ref's
            //   FIRST. Composite rules with structural decoration around a
            //   single ref (e.g. `array = "[" , [ value , { "," , value } ] , "]"`,
            //   `object = "{" , [ pair , { "," , pair } ] , "}"`) carry
            //   load-bearing literal anchors that change the FIRST set;
            //   their hints stay so the developer can see the bracket /
            //   brace anchors at a glance.
            if !nullable {
                if ref_count == 0 {
                    continue; // pure terminal/regex rule — FIRST is obvious
                }
                if ref_count == 1 && is_bare_ref_alias(&rule.rhs_text, &rule.references[0].name) {
                    continue; // bare-ref alias
                }
            }

            // Truncate long FIRST set labels to keep inlay hints readable.
            const MAX_HINT_LEN: usize = 80;
            let truncated = if first_label.len() > MAX_HINT_LEN {
                let mut s = first_label[..MAX_HINT_LEN].to_string();
                // Don't cut in the middle of a char representation — find last comma.
                if let Some(comma) = s.rfind(',') {
                    s.truncate(comma);
                }
                format!("{}…}}", s.trim_end_matches([' ', ',']))
            } else {
                first_label.clone()
            };

            let label = if nullable {
                format!(" {}  ε", truncated)
            } else {
                format!(" {}", truncated)
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
