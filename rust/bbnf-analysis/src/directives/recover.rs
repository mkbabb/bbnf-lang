//! `@recover` directive extraction, validation, and semantic token generation.

use std::collections::{HashMap, HashSet};

use crate::analysis::LineIndex;
use crate::state::types::{RecoverInfo, SemanticTokenInfo, token_types};

use ls_types::*;

/// Extract `RecoverInfo` from a parsed grammar's recover directives.
pub fn extract_recovers(recovers: &[bbnf::types::RecoverDirective<'_>]) -> Vec<RecoverInfo> {
    recovers
        .iter()
        .map(|rec| {
            let name_str = rec.rule_name.as_ref();
            let dir_src = rec.span.as_str();
            let name_start = dir_src
                .find(name_str)
                .map(|off| rec.span.start + off)
                .unwrap_or_else(|| {
                    panic!(
                        "could not resolve @recover rule-name span for `{}` within directive `{}`",
                        name_str, dir_src
                    )
                });
            // Extract sync expression text: everything between end of rule name and end of directive (minus trailing `;`).
            let after_name = &dir_src[name_start - rec.span.start + name_str.len()..];
            let sync_text = after_name.trim().trim_end_matches(';').trim().to_string();
            RecoverInfo {
                rule_name: name_str.to_string(),
                span: (rec.span.start, rec.span.end),
                rule_name_span: (name_start, name_start + name_str.len()),
                sync_expr_text: sync_text,
            }
        })
        .collect()
}

/// Validate `@recover` directives and produce diagnostics + semantic tokens.
pub fn validate_recovers(
    recovers: &[RecoverInfo],
    defined: &HashMap<&str, usize>,
    imported_names: &HashSet<&str>,
    line_index: &LineIndex,
) -> (Vec<Diagnostic>, Vec<SemanticTokenInfo>) {
    let mut diagnostics = Vec::new();
    let mut semantic_tokens = Vec::new();

    for rec in recovers {
        // Semantic token: KEYWORD for "@recover" (8 bytes).
        semantic_tokens.push(SemanticTokenInfo {
            span: (rec.span.0, rec.span.0 + 8),
            token_type: token_types::KEYWORD,
        });

        // Semantic token: RULE_REFERENCE for the rule name.
        semantic_tokens.push(SemanticTokenInfo {
            span: rec.rule_name_span,
            token_type: token_types::RULE_REFERENCE,
        });

        // Validate: warn if the target rule doesn't exist.
        if !defined.contains_key(rec.rule_name.as_str())
            && !imported_names.contains(rec.rule_name.as_str())
        {
            diagnostics.push(Diagnostic {
                range: line_index.span_to_range(rec.rule_name_span.0, rec.rule_name_span.1),
                severity: Some(DiagnosticSeverity::WARNING),
                source: Some("bbnf".into()),
                message: format!("`@recover` targets undefined rule: `{}`", rec.rule_name),
                ..Default::default()
            });
        }
    }

    (diagnostics, semantic_tokens)
}
