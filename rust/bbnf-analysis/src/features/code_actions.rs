use std::collections::HashMap;

use ls_types::*;

use crate::state::DocumentState;

pub fn code_actions(
    state: &DocumentState,
    uri: &Uri,
    range: Range,
) -> CodeActionResponse {
    let mut actions = Vec::new();

    // "Remove unused rule" for rules at cursor with UNNECESSARY tag.
    for diag in &state.info.diagnostics {
        if diag.message.starts_with("Unused rule:") {
            // Check if the diagnostic is in our range.
            if diag.range.start.line >= range.start.line
                && diag.range.end.line <= range.end.line
            {
                // Find the rule.
                let name = diag
                    .message
                    .strip_prefix("Unused rule: `")
                    .and_then(|s| s.strip_suffix('`'));
                if let Some(name) = name {
                    if let Some(&idx) = state.info.rule_index.get(name) {
                        let rule = &state.info.rules[idx];

                        // Extend full_span to include trailing whitespace/newline.
                        let mut delete_end = rule.full_span.1;
                        // Skip past the terminator (`;` or `.`) and any trailing whitespace.
                        while delete_end < state.text.len() {
                            let ch = state.text.as_bytes()[delete_end];
                            if ch == b';' || ch == b'.' {
                                delete_end += 1;
                                break;
                            }
                            delete_end += 1;
                        }
                        // Also consume trailing newline.
                        if delete_end < state.text.len()
                            && state.text.as_bytes()[delete_end] == b'\n'
                        {
                            delete_end += 1;
                        }

                        let mut changes = HashMap::new();
                        changes.insert(
                            uri.clone(),
                            vec![TextEdit {
                                range: state.line_index.span_to_range(rule.full_span.0, delete_end),
                                new_text: String::new(),
                            }],
                        );

                        actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                            title: format!("Remove unused rule `{}`", name),
                            kind: Some(CodeActionKind::QUICKFIX),
                            diagnostics: Some(vec![diag.clone()]),
                            edit: Some(WorkspaceEdit {
                                changes: Some(changes),
                                ..Default::default()
                            }),
                            ..Default::default()
                        }));
                    }
                }
            }
        }
    }

    // "Define undefined rule" for undefined nonterminal references at cursor.
    for diag in &state.info.diagnostics {
        if diag.message.starts_with("Undefined rule:")
            && diag.range.start.line >= range.start.line
            && diag.range.end.line <= range.end.line
        {
                let name = diag
                    .message
                    .strip_prefix("Undefined rule: `")
                    .and_then(|s| s.strip_suffix('`'));
                if let Some(name) = name {
                    // Insert a new rule at the end of the document.
                    let insert_text = format!("\n{} = ;\n", name);
                    let end_pos = state.line_index.offset_to_position(state.text.len());

                    let mut changes = HashMap::new();
                    changes.insert(
                        uri.clone(),
                        vec![TextEdit {
                            range: Range::new(end_pos, end_pos),
                            new_text: insert_text,
                        }],
                    );

                    actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                        title: format!("Define rule `{}`", name),
                        kind: Some(CodeActionKind::QUICKFIX),
                        diagnostics: Some(vec![diag.clone()]),
                        edit: Some(WorkspaceEdit {
                            changes: Some(changes),
                            ..Default::default()
                        }),
                        ..Default::default()
                    }));
                }
            }
        }

    actions
}
