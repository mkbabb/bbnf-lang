use std::collections::HashMap;

use ls_types::*;

use crate::analysis::{symbol_at_offset, SymbolAtOffset};
use crate::state::DocumentState;

pub fn prepare_rename(
    state: &DocumentState,
    position: Position,
) -> Option<PrepareRenameResponse> {
    let offset = state.line_index.position_to_offset(position);
    let symbol = symbol_at_offset(&state.info, offset)?;

    let (range, placeholder) = match &symbol {
        SymbolAtOffset::RuleDefinition(rule) => (
            state.line_index.span_to_range(rule.name_span.0, rule.name_span.1),
            rule.name.clone(),
        ),
        SymbolAtOffset::RuleReference { name, span } => (
            state.line_index.span_to_range(span.0, span.1),
            name.clone(),
        ),
    };

    Some(PrepareRenameResponse::RangeWithPlaceholder {
        range,
        placeholder,
    })
}

pub fn rename(
    state: &DocumentState,
    uri: &Uri,
    position: Position,
    new_name: &str,
) -> Option<WorkspaceEdit> {
    let offset = state.line_index.position_to_offset(position);
    let symbol = symbol_at_offset(&state.info, offset)?;

    let name = match &symbol {
        SymbolAtOffset::RuleDefinition(rule) => rule.name.clone(),
        SymbolAtOffset::RuleReference { name, .. } => name.clone(),
    };

    let mut edits = Vec::new();

    // Rename definition.
    if let Some(&idx) = state.info.rule_index.get(name.as_str()) {
        let rule = &state.info.rules[idx];
        edits.push(TextEdit {
            range: state.line_index.span_to_range(rule.name_span.0, rule.name_span.1),
            new_text: new_name.to_string(),
        });
    }

    // Rename all references.
    for rule in &state.info.rules {
        for refinfo in &rule.references {
            if refinfo.name == name {
                edits.push(TextEdit {
                    range: state.line_index.span_to_range(refinfo.span.0, refinfo.span.1),
                    new_text: new_name.to_string(),
                });
            }
        }
    }

    if edits.is_empty() {
        return None;
    }

    let mut changes = HashMap::new();
    changes.insert(uri.clone(), edits);
    Some(WorkspaceEdit {
        changes: Some(changes),
        ..Default::default()
    })
}
