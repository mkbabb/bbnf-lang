//! `@debug` directive extraction, validation, and semantic token generation.

use std::collections::{HashMap, HashSet};

use crate::analysis::LineIndex;
use crate::state::types::{DebugInfo, SemanticTokenInfo, token_types};

use ls_types::*;

/// Extract `DebugInfo` from a parsed grammar's debug directives.
///
/// Finds byte spans by searching source text for `@debug <name>` patterns.
pub fn extract_debugs<S: AsRef<str>>(debug_rules: &[S], src: &str) -> Vec<DebugInfo> {
    debug_rules
        .iter()
        .filter_map(|name| {
            let name_str = name.as_ref();
            let needle = format!("@debug {}", name_str);
            let dir_start = src.find(&needle)?;
            let kw_end = dir_start + needle.len();
            let dir_end = src[kw_end..]
                .find(';')
                .map(|off| kw_end + off + 1)
                .unwrap_or(kw_end);
            let name_start = dir_start + "@debug ".len();
            Some(DebugInfo {
                rule_name: name_str.to_string(),
                span: (dir_start, dir_end),
                rule_name_span: (name_start, name_start + name_str.len()),
            })
        })
        .collect()
}

/// Validate `@debug` directives and produce diagnostics + semantic tokens.
pub fn validate_debugs(
    debugs: &[DebugInfo],
    defined: &HashMap<&str, usize>,
    imported_names: &HashSet<&str>,
    line_index: &LineIndex,
) -> (Vec<Diagnostic>, Vec<SemanticTokenInfo>) {
    let mut diagnostics = Vec::new();
    let mut semantic_tokens = Vec::new();

    for dbg in debugs {
        // Semantic token: KEYWORD for "@debug" (6 chars).
        semantic_tokens.push(SemanticTokenInfo {
            span: (dbg.span.0, dbg.span.0 + 6),
            token_type: token_types::KEYWORD,
        });

        // Semantic token: RULE_REFERENCE for the rule name (unless "*").
        if dbg.rule_name != "*" {
            semantic_tokens.push(SemanticTokenInfo {
                span: dbg.rule_name_span,
                token_type: token_types::RULE_REFERENCE,
            });

            // Validate: warn if the target rule doesn't exist.
            if !defined.contains_key(dbg.rule_name.as_str())
                && !imported_names.contains(dbg.rule_name.as_str())
            {
                diagnostics.push(Diagnostic {
                    range: line_index.span_to_range(dbg.rule_name_span.0, dbg.rule_name_span.1),
                    severity: Some(DiagnosticSeverity::WARNING),
                    source: Some("bbnf".into()),
                    message: format!("`@debug` targets undefined rule: `{}`", dbg.rule_name),
                    ..Default::default()
                });
            }
        }
    }

    (diagnostics, semantic_tokens)
}
