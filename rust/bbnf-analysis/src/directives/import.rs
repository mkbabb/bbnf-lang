//! `@import` directive extraction, validation, and semantic token generation.

use crate::state::types::{ImportInfo, ImportedItem, SemanticTokenInfo, token_types};

/// Extract `ImportInfo` from a parsed grammar's import directives.
pub fn extract_imports(imports: &[bbnf::types::ImportDirective<'_>]) -> Vec<ImportInfo> {
    imports
        .iter()
        .map(|imp| ImportInfo {
            path: imp.path.to_string(),
            span: (imp.span.start, imp.span.end),
            items: imp.items.as_ref().map(|items| {
                items
                    .iter()
                    .map(|i| ImportedItem {
                        name: i.name.to_string(),
                        span: (i.span.start, i.span.end),
                    })
                    .collect()
            }),
        })
        .collect()
}

/// Generate semantic tokens for `@import` directives.
pub fn import_semantic_tokens(imports: &[ImportInfo]) -> Vec<SemanticTokenInfo> {
    let mut tokens = Vec::new();
    for imp in imports {
        // "@import" keyword (7 chars).
        tokens.push(SemanticTokenInfo {
            span: (imp.span.0, imp.span.0 + 7),
            token_type: token_types::KEYWORD,
        });
        // Selectively imported names as RULE_REFERENCE.
        if let Some(ref items) = imp.items {
            for item in items {
                tokens.push(SemanticTokenInfo {
                    span: item.span,
                    token_type: token_types::RULE_REFERENCE,
                });
            }
        }
    }
    tokens
}
