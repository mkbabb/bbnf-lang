use tower_lsp_server::ls_types::*;

use crate::state::DocumentState;

/// BBNF keywords and operators for completion.
const BBNF_KEYWORDS: &[(&str, &str)] = &[
    ("epsilon", "Empty match (ε)"),
    ("ε", "Empty match"),
    ("@import", "Import rules from another grammar"),
    ("@recover", "Error recovery directive for a rule"),
];

pub fn completion(state: &DocumentState) -> CompletionResponse {
    let mut items = Vec::new();

    // All defined rule names.
    for rule in &state.info.rules {
        items.push(CompletionItem {
            label: rule.name.clone(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some(rule.rhs_text.clone()),
            ..Default::default()
        });
    }

    // BBNF keywords.
    for (keyword, detail) in BBNF_KEYWORDS {
        items.push(CompletionItem {
            label: keyword.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(detail.to_string()),
            ..Default::default()
        });
    }

    CompletionResponse::Array(items)
}
