//! `@pretty` directive extraction, validation, and semantic token generation.
//!
//! Compartmentalised module so all `@pretty` LSP logic lives in one place.

use std::collections::{HashMap, HashSet};

use bbnf::generate::prettify::hints::{self, closest_hint, is_valid_hint};
use tower_lsp_server::ls_types::*;

use crate::analysis::LineIndex;

use super::types::{SemanticTokenInfo, token_types};

/// Owned `@pretty` directive info (no lifetimes).
#[derive(Debug, Clone)]
pub struct PrettyInfo {
    /// The targeted rule name.
    pub rule_name: String,
    /// The formatting hint keywords.
    pub hints: Vec<String>,
    /// Byte offset range of the entire directive.
    pub span: (usize, usize),
    /// Byte offset range of the rule name within the directive.
    pub rule_name_span: (usize, usize),
    /// Byte offset ranges of each hint keyword.
    pub hint_spans: Vec<(usize, usize)>,
}

/// Extract `PrettyInfo` from a parsed grammar.
///
/// Uses the source text + directive span to locate sub-spans for the rule
/// name and each hint keyword.
pub fn extract_pretties(
    pretties: &[bbnf::types::PrettyDirective<'_>],
    _src: &str,
) -> Vec<PrettyInfo> {
    pretties
        .iter()
        .map(|p| {
            let dir_src = p.span.as_str();
            let dir_start = p.span.start;

            // Find the rule name span within the directive source.
            let rule_name = p.rule_name.as_ref();
            let rule_name_offset = dir_src
                .find(rule_name)
                .map(|off| off + dir_start)
                .unwrap_or_else(|| {
                    panic!(
                        "could not resolve @pretty rule-name span for `{}` within directive `{}`",
                        rule_name, dir_src
                    )
                });
            let rule_name_span = (rule_name_offset, rule_name_offset + rule_name.len());

            // Find each hint span by searching after the rule name.
            let mut search_start = rule_name_offset + rule_name.len() - dir_start;
            let hint_spans: Vec<(usize, usize)> = p
                .hints
                .iter()
                .map(|hint| {
                    let hint_str = hint.as_ref();
                    let offset = dir_src[search_start..]
                        .find(hint_str)
                        .map(|off| off + search_start + dir_start)
                        .unwrap_or_else(|| {
                            panic!(
                                "could not resolve @pretty hint span for `{}` within directive `{}`",
                                hint_str, dir_src
                            )
                        });
                    search_start = offset - dir_start + hint_str.len();
                    (offset, offset + hint_str.len())
                })
                .collect();

            PrettyInfo {
                rule_name: rule_name.to_string(),
                hints: p.hints.iter().map(|h| h.to_string()).collect(),
                span: (p.span.start, p.span.end),
                rule_name_span,
                hint_spans,
            }
        })
        .collect()
}

/// Validate `@pretty` directives and produce diagnostics + semantic tokens.
///
/// Returns `(diagnostics, semantic_tokens, referenced_rule_names)`.
pub fn validate_pretties(
    pretties: &[PrettyInfo],
    defined: &HashMap<&str, usize>,
    imported_names: &HashSet<&str>,
    line_index: &LineIndex,
) -> (Vec<Diagnostic>, Vec<SemanticTokenInfo>) {
    let mut diagnostics = Vec::new();
    let mut semantic_tokens = Vec::new();

    for pretty in pretties {
        // Semantic token: KEYWORD for "@pretty".
        // "@pretty" is 7 chars, starts at the directive span start.
        semantic_tokens.push(SemanticTokenInfo {
            span: (pretty.span.0, pretty.span.0 + 7),
            token_type: token_types::KEYWORD,
        });

        // Semantic token: RULE_REFERENCE for the rule name.
        semantic_tokens.push(SemanticTokenInfo {
            span: pretty.rule_name_span,
            token_type: token_types::RULE_REFERENCE,
        });

        // Special case: `@pretty * <mode>` — meta-directive, skip rule validation.
        if pretty.rule_name == "*" {
            // Validate the mode keyword.
            for (i, hint) in pretty.hints.iter().enumerate() {
                let valid_modes = ["auto", "minimal", "off"];
                if !valid_modes.contains(&hint.as_str()) {
                    let span = pretty
                        .hint_spans
                        .get(i)
                        .copied()
                        .unwrap_or_else(|| {
                            panic!(
                                "missing @pretty mode hint span for `{}` at index {}",
                                hint, i
                            )
                        });
                    diagnostics.push(Diagnostic {
                        range: line_index.span_to_range(span.0, span.1),
                        severity: Some(DiagnosticSeverity::WARNING),
                        source: Some("bbnf".into()),
                        message: format!(
                            "Unknown heuristic mode `{}`. Valid modes: auto, minimal, off",
                            hint
                        ),
                        ..Default::default()
                    });
                }
                // Semantic token for mode keyword.
                if let Some(&span) = pretty.hint_spans.get(i) {
                    semantic_tokens.push(SemanticTokenInfo {
                        span,
                        token_type: token_types::KEYWORD,
                    });
                }
            }
            continue;
        }

        // Validate: warn if the target rule doesn't exist.
        if !defined.contains_key(pretty.rule_name.as_str())
            && !imported_names.contains(pretty.rule_name.as_str())
        {
            diagnostics.push(Diagnostic {
                range: line_index.span_to_range(
                    pretty.rule_name_span.0,
                    pretty.rule_name_span.1,
                ),
                severity: Some(DiagnosticSeverity::WARNING),
                source: Some("bbnf".into()),
                message: format!(
                    "`@pretty` targets undefined rule: `{}`",
                    pretty.rule_name
                ),
                ..Default::default()
            });
        }

        // Validate each hint keyword.
        for (i, hint) in pretty.hints.iter().enumerate() {
            let span = pretty
                .hint_spans
                .get(i)
                .copied()
                .unwrap_or_else(|| {
                    panic!(
                        "missing @pretty hint span for `{}` at index {}",
                        hint, i
                    )
                });

            if !is_valid_hint(hint) {
                let mut msg = format!("Unknown `@pretty` hint: `{}`", hint);
                if let Some(suggestion) = closest_hint(hint) {
                    msg.push_str(&format!(". Did you mean `{}`?", suggestion));
                } else {
                    let valid = hints::valid_hint_names();
                    msg.push_str(&format!(". Valid hints: {}", valid.join(", ")));
                }
                diagnostics.push(Diagnostic {
                    range: line_index.span_to_range(span.0, span.1),
                    severity: Some(DiagnosticSeverity::WARNING),
                    source: Some("bbnf".into()),
                    message: msg,
                    ..Default::default()
                });
            }

            // Semantic token for each hint keyword.
            semantic_tokens.push(SemanticTokenInfo {
                span,
                token_type: token_types::KEYWORD,
            });
        }
    }

    (diagnostics, semantic_tokens)
}
