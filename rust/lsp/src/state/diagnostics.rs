use std::collections::{HashMap, HashSet};

use bbnf::analysis::{
    calculate_ast_deps, compute_first_sets, find_aliases, find_first_set_conflicts,
    get_nonterminal_name, tarjan_scc,
};
use bbnf::types::{Expression, Token};

use tower_lsp_server::ls_types::*;

use crate::analysis::LineIndex;

use super::ast_utils::{
    build_cycle_path, collect_references, collect_semantic_tokens, compute_expression_end,
    compute_reachable_rules, format_charset, format_expression_short, is_empty_rhs,
};
use super::parsing::CachedParseResult;
use super::types::{
    DocumentInfo, ParseDiagnostics, RuleInfo, SemanticTokenInfo, token_types,
};

/// Analyze a BBNF document using pre-parsed AST and diagnostics from `parse_once()`.
/// This avoids double-parsing: the OwnedAst parses once, and we reuse its results here.
pub(crate) fn analyze_from_cache(
    text: &str,
    line_index: &LineIndex,
    cached: Option<&CachedParseResult<'_>>,
    parse_diag: &ParseDiagnostics,
) -> DocumentInfo {
    let mut rules = Vec::new();
    let mut diagnostics = Vec::new();
    let mut rule_index = HashMap::new();
    let mut semantic_tokens = Vec::new();

    // Handle parser panic.
    if let Some(msg) = &parse_diag.panic_message {
        let pos = Position::new(0, 0);
        diagnostics.push(Diagnostic {
            range: Range::new(pos, pos),
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some("bbnf".into()),
            message: format!("Parse error: {}", msg),
            ..Default::default()
        });
        return DocumentInfo {
            rules,
            diagnostics,
            rule_index,
            semantic_tokens,
            first_set_labels: HashMap::new(),
            nullable_rules: HashSet::new(),
            cyclic_rule_paths: HashMap::new(),
            imports: Vec::new(),
        };
    }

    let Some(parsed) = cached else {
        // Parse failure -- report error at furthest offset.
        let offset = parse_diag.furthest_offset.max(parse_diag.offset);
        let pos = line_index.offset_to_position(offset);
        diagnostics.push(Diagnostic {
            range: Range::new(pos, pos),
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some("bbnf".into()),
            message: format!(
                "Parse error at offset {} (line {}, col {})",
                offset,
                pos.line + 1,
                pos.character + 1
            ),
            ..Default::default()
        });
        return DocumentInfo {
            rules,
            diagnostics,
            rule_index,
            semantic_tokens,
            first_set_labels: HashMap::new(),
            nullable_rules: HashSet::new(),
            cyclic_rule_paths: HashMap::new(),
            imports: Vec::new(),
        };
    };

    // Check for incomplete parse (didn't consume all input).
    if parse_diag.offset < text.len() {
        let remaining = &text[parse_diag.offset..];
        if !remaining.trim().is_empty() {
            let pos = line_index.offset_to_position(parse_diag.offset);
            diagnostics.push(Diagnostic {
                range: Range::new(pos, pos),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("bbnf".into()),
                message: "Unexpected input after last rule".into(),
                ..Default::default()
            });
        }
    }

    let ast = &parsed.ast;
    let import_infos = parsed.imports.clone();

    // Check for empty AST on non-empty input -- likely a parse failure not caught above.
    if ast.is_empty() && !text.trim().is_empty() && import_infos.is_empty() {
        let furthest = parse_diag.furthest_offset.max(parse_diag.offset);
        let pos = line_index.offset_to_position(furthest.min(text.len()));
        diagnostics.push(Diagnostic {
            range: Range::new(Position::new(0, 0), pos),
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some("bbnf".into()),
            message: "Failed to parse any rules. Check syntax (each rule needs: name = expression ;)".into(),
            ..Default::default()
        });
        return DocumentInfo {
            rules,
            diagnostics,
            rule_index,
            semantic_tokens,
            first_set_labels: HashMap::new(),
            nullable_rules: HashSet::new(),
            cyclic_rule_paths: HashMap::new(),
            imports: import_infos,
        };
    }

    // Extract rule info from AST.
    for (lhs, rhs) in ast.iter() {
        if let Expression::Nonterminal(Token { value: name, span: name_span, .. }) = lhs {
            let name_str = name.to_string();
            let name_byte_span = (name_span.start, name_span.end);

            // Compute full span (from LHS start to RHS end).
            let full_start = name_span.start;
            let full_end = compute_expression_end(rhs).unwrap_or(name_span.end);

            // Collect nonterminal references in RHS.
            let mut references = Vec::new();
            collect_references(rhs, &mut references);

            // Collect semantic tokens from RHS.
            collect_semantic_tokens(rhs, &mut semantic_tokens);

            // Semantic token for rule definition (LHS).
            semantic_tokens.push(SemanticTokenInfo {
                span: name_byte_span,
                token_type: token_types::RULE_DEFINITION,
            });

            // Pretty-print RHS for hover.
            let rhs_text = format_expression_short(rhs);

            // Check for duplicate rule.
            if let Some(&_existing_idx) = rule_index.get(&name_str) {
                diagnostics.push(Diagnostic {
                    range: line_index.span_to_range(name_byte_span.0, name_byte_span.1),
                    severity: Some(DiagnosticSeverity::ERROR),
                    source: Some("bbnf".into()),
                    message: format!("Duplicate rule: `{}`", name_str),
                    related_information: None, // TODO: populate with correct URI from caller
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
    }

    // Diagnostics: undefined nonterminals and unused rules.
    let defined: HashMap<&str, usize> = rules
        .iter()
        .enumerate()
        .map(|(i, r)| (r.name.as_str(), i))
        .collect();

    // Build set of names available via @import directives.
    let imported_names: HashSet<&str> = import_infos
        .iter()
        .flat_map(|imp| {
            imp.items
                .as_ref()
                .map(|items| items.iter().map(|s| s.as_str()).collect::<Vec<_>>())
                .unwrap_or_default()
        })
        .collect();

    let mut referenced_names: std::collections::HashSet<&str> =
        std::collections::HashSet::new();

    for rule in &rules {
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

    for rule in &rules {
        if !referenced_names.contains(rule.name.as_str()) && rules.len() > 1 {
            // Don't flag the first rule -- it's typically the entry point.
            if rule_index.get(rule.name.as_str()) != Some(&0) {
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

    // Left recursion detection via dependency analysis.
    let deps = calculate_ast_deps(ast);
    let scc = tarjan_scc(&deps);

    // Build cycle path strings and emit enhanced cycle diagnostics.
    let mut cyclic_rule_paths = HashMap::new();
    for scc_group in &scc.sccs {
        // An SCC with >1 member means all members are cyclic.
        // An SCC with 1 member is cyclic only if it self-references (checked via cyclic_rules).
        let is_multi = scc_group.len() > 1;
        let cyclic_members: Vec<&str> = scc_group
            .iter()
            .filter_map(|e| {
                let name = get_nonterminal_name(e)?;
                if is_multi || scc.cyclic_rules.contains(*e) {
                    Some(name)
                } else {
                    None
                }
            })
            .collect();

        if cyclic_members.is_empty() {
            continue;
        }

        for &member in &cyclic_members {
            let path = if cyclic_members.len() == 1 {
                // Self-recursive rule.
                format!("{} \u{2192} {}", member, member)
            } else {
                // Multi-member SCC -- reconstruct a representative cycle path.
                build_cycle_path(member, &cyclic_members, &deps)
            };

            cyclic_rule_paths.insert(member.to_string(), path.clone());

            if let Some(&idx) = rule_index.get(member) {
                let rule = &rules[idx];
                diagnostics.push(Diagnostic {
                    range: line_index.span_to_range(rule.name_span.0, rule.name_span.1),
                    severity: Some(DiagnosticSeverity::INFORMATION),
                    source: Some("bbnf".into()),
                    message: format!(
                        "Rule `{}` participates in a cycle: {}",
                        member, path
                    ),
                    ..Default::default()
                });
            }
        }
    }

    // FIRST set computation for inlay hints (SCC-ordered, O(n+E)).
    let first_sets = compute_first_sets(ast, &deps, &scc);

    let mut first_set_labels = HashMap::new();
    let mut nullable_rules = HashSet::new();

    for (lhs, rhs) in ast.iter() {
        if let Expression::Nonterminal(Token { value: name, .. }) = lhs {
            let name_str = name.to_string();

            if let Some(cs) = first_sets.first.get(lhs) {
                first_set_labels.insert(name_str.clone(), format_charset(cs));
            }

            if first_sets.nullable.contains(lhs) {
                nullable_rules.insert(name_str.clone());
            }

            // Enhanced diagnostic: empty rule body detection.
            if is_empty_rhs(rhs) {
                if let Some(&idx) = rule_index.get(name.as_ref()) {
                    let rule = &rules[idx];
                    diagnostics.push(Diagnostic {
                        range: line_index.span_to_range(rule.name_span.0, rule.name_span.1),
                        severity: Some(DiagnosticSeverity::WARNING),
                        source: Some("bbnf".into()),
                        message: format!("Rule `{}` has an empty body", name),
                        ..Default::default()
                    });
                }
            }
        }
    }

    // FIRST set conflict detection for ambiguous alternations.
    let conflicts = find_first_set_conflicts(ast, &first_sets);
    for (rule_name, rule_conflicts) in &conflicts {
        if let Some(&idx) = rule_index.get(rule_name.as_str()) {
            let rule = &rules[idx];
            for conflict in rule_conflicts {
                let overlap_str = format_charset(&conflict.overlap);
                diagnostics.push(Diagnostic {
                    range: line_index.span_to_range(rule.name_span.0, rule.name_span.1),
                    severity: Some(DiagnosticSeverity::WARNING),
                    source: Some("bbnf".into()),
                    message: format!(
                        "Alternation in `{}` has ambiguous FIRST sets: branches {} and {} both start with {}",
                        rule_name,
                        conflict.branch_a + 1,
                        conflict.branch_b + 1,
                        overlap_str
                    ),
                    ..Default::default()
                });
            }
        }
    }

    // Alias detection: rules whose RHS is just a nonterminal reference.
    let aliases = find_aliases(ast, &scc.cyclic_rules);
    for (alias_lhs, target) in &aliases {
        if let (Some(alias_name), Some(target_name)) = (
            get_nonterminal_name(alias_lhs),
            get_nonterminal_name(target),
        ) {
            // Skip aliases of imported rules (intentional re-exports).
            if imported_names.contains(alias_name) {
                continue;
            }
            if let Some(&idx) = rule_index.get(alias_name) {
                let rule = &rules[idx];
                diagnostics.push(Diagnostic {
                    range: line_index.span_to_range(rule.name_span.0, rule.name_span.1),
                    severity: Some(DiagnosticSeverity::HINT),
                    source: Some("bbnf".into()),
                    message: format!(
                        "Rule `{}` is an alias of `{}` -- consider using `{}` directly",
                        alias_name, target_name, target_name
                    ),
                    ..Default::default()
                });
            }
        }
    }

    // Unreachable rule detection via BFS from root rules.
    let reachable = compute_reachable_rules(&rules, &rule_index);
    for rule in &rules {
        // Skip the first rule (entry point) and already-unused rules.
        if rule_index.get(rule.name.as_str()) == Some(&0) {
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
                source: Some("bbnf".into()),
                message: format!(
                    "Rule `{}` is unreachable from the entry rule",
                    rule.name
                ),
                tags: Some(vec![DiagnosticTag::UNNECESSARY]),
                ..Default::default()
            });
        }
    }

    // Sort semantic tokens by offset for encoding.
    semantic_tokens.sort_by_key(|t| t.span.0);

    DocumentInfo {
        rules,
        diagnostics,
        rule_index,
        semantic_tokens,
        first_set_labels,
        nullable_rules,
        cyclic_rule_paths,
        imports: import_infos,
    }
}

/// Public convenience wrapper: parses the text and analyzes in one step.
/// Used by tests and any code that doesn't need the cached AST.
#[cfg(test)]
pub fn analyze(text: &str, line_index: &LineIndex) -> DocumentInfo {
    let (cached, diag) = super::parsing::parse_once(text);
    analyze_from_cache(text, line_index, cached.as_ref(), &diag)
}
