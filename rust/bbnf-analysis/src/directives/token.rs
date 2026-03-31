//! `@token` directive extraction, validation, and semantic token generation.

use std::collections::{HashMap, HashSet};

use crate::analysis::LineIndex;
use crate::state::types::{SemanticTokenInfo, TokenInfo, token_types};

use ls_types::*;

/// Extract `TokenInfo` from a parsed grammar's token directives.
///
/// Finds byte spans by searching source text for `@token <name>` patterns.
pub fn extract_tokens<S: AsRef<str>>(token_rules: &[S], src: &str) -> Vec<TokenInfo> {
    token_rules
        .iter()
        .filter_map(|name| {
            let name_str = name.as_ref();
            let needle = format!("@token {}", name_str);
            let dir_start = src.find(&needle)?;
            let kw_end = dir_start + needle.len();
            let dir_end = src[kw_end..]
                .find(';')
                .map(|off| kw_end + off + 1)
                .unwrap_or(kw_end);
            let name_start = dir_start + "@token ".len();
            Some(TokenInfo {
                rule_name: name_str.to_string(),
                span: (dir_start, dir_end),
                rule_name_span: (name_start, name_start + name_str.len()),
            })
        })
        .collect()
}

/// Validate `@token` directives and produce diagnostics + semantic tokens.
pub fn validate_tokens(
    tokens: &[TokenInfo],
    defined: &HashMap<&str, usize>,
    imported_names: &HashSet<&str>,
    line_index: &LineIndex,
) -> (Vec<Diagnostic>, Vec<SemanticTokenInfo>) {
    let mut diagnostics = Vec::new();
    let mut semantic_tokens = Vec::new();

    for tok in tokens {
        // Semantic token: KEYWORD for "@token" (6 chars).
        semantic_tokens.push(SemanticTokenInfo {
            span: (tok.span.0, tok.span.0 + 6),
            token_type: token_types::KEYWORD,
        });

        // Semantic token: RULE_REFERENCE for the rule name.
        semantic_tokens.push(SemanticTokenInfo {
            span: tok.rule_name_span,
            token_type: token_types::RULE_REFERENCE,
        });

        // Validate: warn if the target rule doesn't exist.
        if !defined.contains_key(tok.rule_name.as_str())
            && !imported_names.contains(tok.rule_name.as_str())
        {
            diagnostics.push(Diagnostic {
                range: line_index.span_to_range(tok.rule_name_span.0, tok.rule_name_span.1),
                severity: Some(DiagnosticSeverity::WARNING),
                source: Some("bbnf".into()),
                message: format!("`@token` targets undefined rule: `{}`", tok.rule_name),
                ..Default::default()
            });
        }
    }

    (diagnostics, semantic_tokens)
}
