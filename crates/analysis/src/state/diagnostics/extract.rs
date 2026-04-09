use std::collections::HashMap;

use bbnf::types::AST;
use ls_types::*;

use crate::analysis::LineIndex;

use super::super::ast_utils::{
    collect_references, collect_semantic_tokens, compute_expression_end, format_expression_short,
};
use super::super::types::{RuleInfo, SemanticTokenInfo, token_types};

/// Walk the parsed AST and extract rule info, a name→index map, semantic tokens,
/// and emit duplicate-rule diagnostics. Mirrors the original loop verbatim.
pub(super) fn extract_rules(
    ast: &AST<'_>,
    line_index: &LineIndex,
    diagnostics: &mut Vec<Diagnostic>,
    semantic_tokens: &mut Vec<SemanticTokenInfo>,
) -> (Vec<RuleInfo>, HashMap<String, usize>) {
    let mut rules: Vec<RuleInfo> = Vec::new();
    let mut rule_index: HashMap<String, usize> = HashMap::new();

    for (&name, entry) in ast.iter() {
        let name_str = name.to_string();
        let name_span = &entry.name_span;
        let name_byte_span = (name_span.start, name_span.end);
        let rhs = entry.rhs;

        // Compute full span (from LHS start to RHS end).
        let full_start = name_span.start;
        let full_end = compute_expression_end(rhs).unwrap_or_else(|| {
            panic!(
                "analyze_from_cache could not compute expression end for rule `{}`",
                name
            )
        });

        // Collect nonterminal references in RHS.
        let mut references = Vec::new();
        collect_references(rhs, &mut references);

        // Collect semantic tokens from RHS.
        collect_semantic_tokens(rhs, semantic_tokens);

        // Semantic token for rule definition (LHS).
        semantic_tokens.push(SemanticTokenInfo {
            span: name_byte_span,
            token_type: token_types::RULE_DEFINITION,
        });

        // Pretty-print RHS for hover.
        let rhs_text = format_expression_short(rhs);

        // Check for duplicate rule.
        if let Some(&existing_idx) = rule_index.get(&name_str) {
            let previous = &rules[existing_idx];
            diagnostics.push(Diagnostic {
                range: line_index.span_to_range(name_byte_span.0, name_byte_span.1),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("bbnf".into()),
                message: format!(
                    "Duplicate rule: `{}` (previous definition at bytes {}..{})",
                    name_str, previous.name_span.0, previous.name_span.1
                ),
                ..Default::default()
            });
        }

        let idx = rules.len();
        rule_index.insert(name_str.clone(), idx);

        rules.push(RuleInfo {
            name: name_str,
            name_span: name_byte_span,
            full_span: (full_start, full_end),
            rhs_text,
            references,
        });
    }

    (rules, rule_index)
}
