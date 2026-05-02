use std::collections::{HashMap, HashSet};

use bbnf::graph::{SccResult, find_aliases};
use bbnf::types::AST;
use ls_types::*;

use crate::analysis::LineIndex;

use super::super::ast_utils::{compute_reachable_rules, is_empty_rhs};
use super::super::types::RuleInfo;

/// Emit warnings for rules with empty bodies (AST-level, no FIRST sets needed).
/// Mirrors the original loop verbatim.
pub(super) fn detect_empty_bodies(
    ast: &AST<'_>,
    rules: &[RuleInfo],
    rule_index: &HashMap<String, usize>,
    line_index: &LineIndex,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for (&name, entry) in ast.iter() {
        if is_empty_rhs(entry.rhs) {
            if let Some(&idx) = rule_index.get(name) {
                let rule = &rules[idx];
                diagnostics.push(Diagnostic {
                    range: line_index.span_to_range(rule.name_span.0, rule.name_span.1),
                    severity: Some(DiagnosticSeverity::WARNING),
                    source: Some(crate::DIAGNOSTIC_SOURCE.into()),
                    message: format!("Rule `{}` has an empty body", name),
                    ..Default::default()
                });
            }
        }
    }
}

/// Detect rules whose RHS is just a nonterminal reference. Skips aliases of
/// imported rules (intentional re-exports). Mirrors the original loop verbatim.
pub(super) fn detect_aliases(
    ast: &AST<'_>,
    scc: &SccResult<'_>,
    rules: &[RuleInfo],
    rule_index: &HashMap<String, usize>,
    imported_names: &HashSet<&str>,
    line_index: &LineIndex,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let aliases = find_aliases(ast, &scc.cyclic_rules);
    for (&alias_name, &target_name) in &aliases {
        // Skip aliases of imported rules (intentional re-exports).
        if imported_names.contains(alias_name) {
            continue;
        }
        if let Some(&idx) = rule_index.get(alias_name) {
            let rule = &rules[idx];
            diagnostics.push(Diagnostic {
                range: line_index.span_to_range(rule.name_span.0, rule.name_span.1),
                severity: Some(DiagnosticSeverity::HINT),
                source: Some(crate::DIAGNOSTIC_SOURCE.into()),
                message: format!(
                    "Rule `{}` is an alias of `{}` -- consider using `{}` directly",
                    alias_name, target_name, target_name
                ),
                ..Default::default()
            });
        }
    }
}

/// Unreachable rule detection via BFS from root rules. Skips the first/last
/// rule (plausible entry points) and already-flagged unused rules. Mirrors the
/// original loop verbatim.
pub(super) fn detect_unreachable_rules(
    rules: &[RuleInfo],
    rule_index: &HashMap<String, usize>,
    referenced_names: &HashSet<&str>,
    line_index: &LineIndex,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let last_rule_idx = rules.len().saturating_sub(1);
    let reachable = compute_reachable_rules(rules, rule_index);
    for rule in rules {
        // Skip the first/last rule (entry points) and already-unused rules.
        let idx = rule_index.get(rule.name.as_str()).copied();
        if idx == Some(0) || idx == Some(last_rule_idx) {
            continue;
        }
        if !referenced_names.contains(rule.name.as_str()) {
            // Already flagged as unused -- no need to also flag as unreachable.
            continue;
        }
        if !reachable.contains(rule.name.as_str()) {
            diagnostics.push(Diagnostic {
                range: line_index.span_to_range(rule.name_span.0, rule.name_span.1),
                severity: Some(DiagnosticSeverity::HINT),
                source: Some(crate::DIAGNOSTIC_SOURCE.into()),
                message: format!("Rule `{}` is unreachable from the entry rule", rule.name),
                tags: Some(vec![DiagnosticTag::UNNECESSARY]),
                ..Default::default()
            });
        }
    }
}
