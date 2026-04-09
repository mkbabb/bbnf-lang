use std::collections::{HashMap, HashSet};

use ls_types::*;

use crate::analysis::LineIndex;

use super::super::types::{
    DebugInfo, ImportInfo, RecoverInfo, RuleInfo, TokenInfo,
};
use super::super::pretty::PrettyInfo;

/// Detect undefined nonterminals in rule references and populate the initial
/// `referenced_names` set. Mirrors the original loop verbatim.
pub(super) fn detect_undefined_and_collect_references<'a>(
    rules: &'a [RuleInfo],
    defined: &HashMap<&str, usize>,
    imported_names: &HashSet<&str>,
    line_index: &LineIndex,
    diagnostics: &mut Vec<Diagnostic>,
) -> HashSet<&'a str> {
    let mut referenced_names: HashSet<&'a str> = HashSet::new();

    for rule in rules {
        for refinfo in &rule.references {
            referenced_names.insert(&refinfo.name);
            if !defined.contains_key(refinfo.name.as_str())
                && !imported_names.contains(refinfo.name.as_str())
            {
                diagnostics.push(Diagnostic {
                    range: line_index.span_to_range(refinfo.span.0, refinfo.span.1),
                    severity: Some(DiagnosticSeverity::WARNING),
                    source: Some("bbnf".into()),
                    message: format!("Undefined rule: `{}`", refinfo.name),
                    ..Default::default()
                });
            }
        }
    }

    referenced_names
}

/// Add directive-referenced rule names into `referenced_names` before the unused
/// rule check. Mirrors the original block verbatim.
pub(super) fn add_directive_references<'a>(
    referenced_names: &mut HashSet<&'a str>,
    recover_infos: &'a [RecoverInfo],
    pretty_infos: &'a [PrettyInfo],
    token_infos: &'a [TokenInfo],
    debug_infos: &'a [DebugInfo],
) {
    for rec in recover_infos {
        referenced_names.insert(&rec.rule_name);
    }
    for p in pretty_infos {
        referenced_names.insert(&p.rule_name);
    }
    for tok in token_infos {
        referenced_names.insert(&tok.rule_name);
    }
    for dbg in debug_infos {
        if dbg.rule_name != "*" {
            referenced_names.insert(&dbg.rule_name);
        }
    }
}

/// Emit hints for rules that are neither referenced nor plausible entry points
/// (the first or last rule is always considered plausible). Mirrors the original
/// block verbatim.
pub(super) fn detect_unused_rules(
    rules: &[RuleInfo],
    rule_index: &HashMap<String, usize>,
    referenced_names: &HashSet<&str>,
    line_index: &LineIndex,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let last_rule_idx = rules.len().saturating_sub(1);
    for rule in rules {
        if !referenced_names.contains(rule.name.as_str()) && rules.len() > 1 {
            let idx = rule_index.get(rule.name.as_str()).copied();
            // Don't flag the first or last rule -- both are plausible entry points.
            if idx != Some(0) && idx != Some(last_rule_idx) {
                diagnostics.push(Diagnostic {
                    range: line_index.span_to_range(rule.name_span.0, rule.name_span.1),
                    severity: Some(DiagnosticSeverity::HINT),
                    source: Some("bbnf".into()),
                    message: format!("Unused rule: `{}`", rule.name),
                    tags: Some(vec![DiagnosticTag::UNNECESSARY]),
                    ..Default::default()
                });
            }
        }
    }
}

/// Build the set of names available via `@import` directives.
pub(super) fn build_imported_names(import_infos: &[ImportInfo]) -> HashSet<&str> {
    import_infos
        .iter()
        .filter_map(|imp| imp.items.as_ref())
        .flatten()
        .map(|item| item.name.as_str())
        .collect()
}

/// Build the defined-rules name→index map.
pub(super) fn build_defined_map(rules: &[RuleInfo]) -> HashMap<&str, usize> {
    rules
        .iter()
        .enumerate()
        .map(|(i, r)| (r.name.as_str(), i))
        .collect()
}
