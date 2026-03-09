use bbnf::generate::prettify::hints::HINT_DEFS;
use tower_lsp_server::ls_types::*;

use crate::state::DocumentState;

/// BBNF keywords and operators for completion.
const BBNF_KEYWORDS: &[(&str, &str)] = &[
    ("epsilon", "Empty match (ε)"),
    ("ε", "Empty match"),
    ("@import", "Import rules from another grammar"),
    ("@recover", "Error recovery directive for a rule"),
    ("@no_collapse", "Prevent span collapse for a rule (preserve Vec<Span>)"),
    ("@pretty", "Formatting hints for pretty-printer output"),
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

    // @pretty hint keywords.
    for hint_def in HINT_DEFS {
        items.push(CompletionItem {
            label: hint_def.name.to_string(),
            kind: Some(CompletionItemKind::ENUM_MEMBER),
            detail: Some(hint_def.description.to_string()),
            ..Default::default()
        });
    }

    // sep("...") hint with snippet.
    items.push(CompletionItem {
        label: "sep(\"...\")".to_string(),
        kind: Some(CompletionItemKind::ENUM_MEMBER),
        detail: Some("Custom separator string for Vec/tuple items".to_string()),
        insert_text: Some("sep(\"$1\")".to_string()),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        ..Default::default()
    });

    // split("...") hint with snippet.
    items.push(CompletionItem {
        label: "split(\"...\")".to_string(),
        kind: Some(CompletionItemKind::ENUM_MEMBER),
        detail: Some(
            "Split Span text on delimiter at format time (depth-aware, respects ()[] and quotes)"
                .to_string(),
        ),
        insert_text: Some("split(\"$1\")".to_string()),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        ..Default::default()
    });

    CompletionResponse::Array(items)
}
