use std::collections::{HashMap, HashSet};

use bbnf::analysis::{
    calculate_ast_deps, compute_first_sets, find_aliases, find_first_set_conflicts,
    get_nonterminal_name, tarjan_scc,
};
use bbnf::pipeline::{compile_ast, PipelineOptions};
use bbnf::types::{Expression, Token};

use super::types::IrRuleMeta;

use ls_types::*;

use crate::analysis::LineIndex;

use super::ast_utils::{
    build_cycle_path, collect_references, collect_semantic_tokens, compute_expression_end,
    compute_reachable_rules, format_charset, format_expression_short, is_empty_rhs,
};
use super::parsing::CachedParseResult;
use super::pretty;
use super::types::{
    DocumentInfo, ParseDiagnostics, RuleInfo, SemanticTokenInfo, token_types,
};

/// Analyze a BBNF document using pre-parsed AST and diagnostics from `parse_once()`.
/// This avoids double-parsing: the OwnedAst parses once, and we reuse its results here.
pub fn analyze_from_cache(
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
            recovers: Vec::new(),
            no_collapses: Vec::new(),
            pretties: Vec::new(),
            inlines: Vec::new(),
            debugs: Vec::new(),
            tokens: Vec::new(),
            ws_pattern: None,
            ir_meta: HashMap::new(),
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
            recovers: Vec::new(),
            no_collapses: Vec::new(),
            pretties: Vec::new(),
            inlines: Vec::new(),
            debugs: Vec::new(),
            tokens: Vec::new(),
            ws_pattern: None,
            ir_meta: HashMap::new(),
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
    let recover_infos = parsed.recovers.clone();
    let no_collapse_infos = parsed.no_collapses.clone();
    let pretty_infos = parsed.pretties.clone();
    let inline_infos = parsed.inlines.clone();
    let debug_infos = parsed.debugs.clone();
    let token_infos = parsed.tokens.clone();
    let ws_pattern_info = parsed.ws_pattern.clone();

    // Check for empty AST on non-empty input -- likely a parse failure not caught above.
    if ast.is_empty() && !text.trim().is_empty() && import_infos.is_empty() && recover_infos.is_empty() {
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
            recovers: recover_infos,
            no_collapses: no_collapse_infos,
            pretties: Vec::new(),
            inlines: Vec::new(),
            debugs: Vec::new(),
            tokens: Vec::new(),
            ws_pattern: None,
            ir_meta: HashMap::new(),
        };
    }

    // Extract rule info from AST.
    for (lhs, rhs) in ast.iter() {
        if let Expression::Nonterminal(Token { value: name, span: name_span, .. }) = lhs {
            let name_str = name.to_string();
            let name_byte_span = (name_span.start, name_span.end);

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
            collect_semantic_tokens(rhs, &mut semantic_tokens);

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
                        name_str,
                        previous.name_span.0,
                        previous.name_span.1
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
        .filter_map(|imp| imp.items.as_ref())
        .flatten()
        .map(|item| item.name.as_str())
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

    // Add directive-referenced rule names before the unused rule check.
    for rec in &recover_infos {
        referenced_names.insert(&rec.rule_name);
    }
    for nc in &no_collapse_infos {
        referenced_names.insert(&nc.rule_name);
    }
    for p in &pretty_infos {
        referenced_names.insert(&p.rule_name);
    }
    for inl in &inline_infos {
        referenced_names.insert(&inl.rule_name);
    }
    for tok in &token_infos {
        referenced_names.insert(&tok.rule_name);
    }
    for dbg in &debug_infos {
        if dbg.rule_name != "*" {
            referenced_names.insert(&dbg.rule_name);
        }
    }

    let last_rule_idx = rules.len().saturating_sub(1);
    for rule in &rules {
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

    // @import directive semantic tokens.
    for imp in &import_infos {
        // "@import" keyword (7 chars).
        semantic_tokens.push(SemanticTokenInfo {
            span: (imp.span.0, imp.span.0 + 7),
            token_type: token_types::KEYWORD,
        });
        // Selectively imported names as RULE_REFERENCE.
        if let Some(ref items) = imp.items {
            for item in items {
                semantic_tokens.push(SemanticTokenInfo {
                    span: item.span,
                    token_type: token_types::RULE_REFERENCE,
                });
            }
        }
    }

    // @recover directive validation and semantic tokens.
    for rec in &recover_infos {
        // Semantic token: KEYWORD for "@recover".
        // The "@recover" keyword is 8 bytes, starts at the directive span start.
        semantic_tokens.push(SemanticTokenInfo {
            span: (rec.span.0, rec.span.0 + 8), // "@recover" is 8 chars
            token_type: token_types::KEYWORD,
        });

        // Semantic token: RULE_REFERENCE for the rule name.
        semantic_tokens.push(SemanticTokenInfo {
            span: rec.rule_name_span,
            token_type: token_types::RULE_REFERENCE,
        });

        // Mark the rule name as referenced (for unused rule detection).
        referenced_names.insert(&rec.rule_name);

        // Validate: warn if the target rule doesn't exist.
        if !defined.contains_key(rec.rule_name.as_str())
            && !imported_names.contains(rec.rule_name.as_str())
        {
            diagnostics.push(Diagnostic {
                range: line_index.span_to_range(rec.rule_name_span.0, rec.rule_name_span.1),
                severity: Some(DiagnosticSeverity::WARNING),
                source: Some("bbnf".into()),
                message: format!(
                    "`@recover` targets undefined rule: `{}`",
                    rec.rule_name
                ),
                ..Default::default()
            });
        }
    }

    // @no_collapse directive validation and semantic tokens.
    for nc in &no_collapse_infos {
        // Semantic token: KEYWORD for "@no_collapse".
        // The "@no_collapse" keyword is 13 bytes, starts at the directive span start.
        semantic_tokens.push(SemanticTokenInfo {
            span: (nc.span.0, nc.span.0 + 13), // "@no_collapse" is 13 chars
            token_type: token_types::KEYWORD,
        });

        // Semantic token: RULE_REFERENCE for the rule name.
        semantic_tokens.push(SemanticTokenInfo {
            span: nc.rule_name_span,
            token_type: token_types::RULE_REFERENCE,
        });

        // Mark the rule name as referenced (for unused rule detection).
        referenced_names.insert(&nc.rule_name);

        // Validate: warn if the target rule doesn't exist.
        if !defined.contains_key(nc.rule_name.as_str())
            && !imported_names.contains(nc.rule_name.as_str())
        {
            diagnostics.push(Diagnostic {
                range: line_index.span_to_range(nc.rule_name_span.0, nc.rule_name_span.1),
                severity: Some(DiagnosticSeverity::WARNING),
                source: Some("bbnf".into()),
                message: format!(
                    "`@no_collapse` targets undefined rule: `{}`",
                    nc.rule_name
                ),
                ..Default::default()
            });
        }
    }

    // @pretty directive validation and semantic tokens.
    {
        let (pretty_diags, pretty_tokens) = pretty::validate_pretties(
            &pretty_infos,
            &defined,
            &imported_names,
            line_index,
        );
        diagnostics.extend(pretty_diags);
        semantic_tokens.extend(pretty_tokens);

        // Mark pretty directive rule names as referenced (for unused rule detection).
        for p in &pretty_infos {
            referenced_names.insert(&p.rule_name);
        }
    }

    // @inline directive validation and semantic tokens.
    for inl in &inline_infos {
        // Semantic token: KEYWORD for "@inline" (7 chars).
        semantic_tokens.push(SemanticTokenInfo {
            span: (inl.span.0, inl.span.0 + 7),
            token_type: token_types::KEYWORD,
        });

        // Semantic token: RULE_REFERENCE for the rule name.
        semantic_tokens.push(SemanticTokenInfo {
            span: inl.rule_name_span,
            token_type: token_types::RULE_REFERENCE,
        });

        // Mark the rule name as referenced (for unused rule detection).
        referenced_names.insert(&inl.rule_name);

        // Validate: warn if the target rule doesn't exist.
        if !defined.contains_key(inl.rule_name.as_str())
            && !imported_names.contains(inl.rule_name.as_str())
        {
            diagnostics.push(Diagnostic {
                range: line_index.span_to_range(inl.rule_name_span.0, inl.rule_name_span.1),
                severity: Some(DiagnosticSeverity::WARNING),
                source: Some("bbnf".into()),
                message: format!(
                    "`@inline` targets undefined rule: `{}`",
                    inl.rule_name
                ),
                ..Default::default()
            });
        }
    }

    // @debug directive validation and semantic tokens.
    for dbg in &debug_infos {
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

            // Mark the rule name as referenced (for unused rule detection).
            referenced_names.insert(&dbg.rule_name);

            // Validate: warn if the target rule doesn't exist.
            if !defined.contains_key(dbg.rule_name.as_str())
                && !imported_names.contains(dbg.rule_name.as_str())
            {
                diagnostics.push(Diagnostic {
                    range: line_index.span_to_range(dbg.rule_name_span.0, dbg.rule_name_span.1),
                    severity: Some(DiagnosticSeverity::WARNING),
                    source: Some("bbnf".into()),
                    message: format!(
                        "`@debug` targets undefined rule: `{}`",
                        dbg.rule_name
                    ),
                    ..Default::default()
                });
            }
        }
    }

    // @token directive validation and semantic tokens.
    for tok in &token_infos {
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

        // Mark the rule name as referenced (for unused rule detection).
        referenced_names.insert(&tok.rule_name);

        // Validate: warn if the target rule doesn't exist.
        if !defined.contains_key(tok.rule_name.as_str())
            && !imported_names.contains(tok.rule_name.as_str())
        {
            diagnostics.push(Diagnostic {
                range: line_index.span_to_range(tok.rule_name_span.0, tok.rule_name_span.1),
                severity: Some(DiagnosticSeverity::WARNING),
                source: Some("bbnf".into()),
                message: format!(
                    "`@token` targets undefined rule: `{}`",
                    tok.rule_name
                ),
                ..Default::default()
            });
        }
    }

    // @ws directive semantic tokens.
    if let Some(ws) = &ws_pattern_info {
        // Semantic token: KEYWORD for "@ws" (3 chars).
        semantic_tokens.push(SemanticTokenInfo {
            span: (ws.span.0, ws.span.0 + 3),
            token_type: token_types::KEYWORD,
        });
    }

    // Sort semantic tokens by offset for encoding.
    semantic_tokens.sort_by_key(|t| t.span.0);

    // IR pipeline: extract rich metadata (FOLLOW sets, dispatch, memo, types).
    let ir_meta = cached
        .map(|c| {
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| try_compile_ir(c)))
                .unwrap_or_default()
        })
        .unwrap_or_default();

    DocumentInfo {
        rules,
        diagnostics,
        rule_index,
        semantic_tokens,
        first_set_labels,
        nullable_rules,
        cyclic_rule_paths,
        imports: import_infos,
        recovers: recover_infos,
        no_collapses: no_collapse_infos,
        pretties: pretty_infos,
        inlines: inline_infos,
        debugs: debug_infos,
        tokens: token_infos,
        ws_pattern: ws_pattern_info,
        ir_meta,
    }
}

/// Public convenience wrapper: parses the text and analyzes in one step.
/// Used by tests, WASM, and any code that doesn't need the cached AST.
pub fn analyze(text: &str, line_index: &LineIndex) -> DocumentInfo {
    let (cached, diag) = super::parsing::parse_once(text);
    analyze_from_cache(text, line_index, cached.as_ref(), &diag)
}

// ─── IR Pipeline Integration ─────────────────────────────────────────────────

/// Run the IR pipeline on a cached parse result and extract per-rule metadata.
///
/// On failure (e.g., the grammar is incomplete or uses features not yet supported
/// by the IR lowering), returns an empty map — callers degrade gracefully.
fn try_compile_ir(
    cached: &CachedParseResult<'_>,
) -> HashMap<String, IrRuleMeta> {
    let ast = cached.ast.clone();

    // Reconstruct directive maps from the analysis-layer types.
    let recover_map: HashMap<String, Expression<'_>> = HashMap::new();

    let pretty_map: HashMap<String, Vec<String>> = cached
        .pretties
        .iter()
        .map(|p| (p.rule_name.clone(), p.hints.clone()))
        .collect();

    let no_collapse_set: HashSet<String> = cached
        .no_collapses
        .iter()
        .map(|nc| nc.rule_name.clone())
        .collect();

    let inline_set: HashSet<String> = cached
        .inlines
        .iter()
        .map(|inl| inl.rule_name.clone())
        .collect();
    let inline_ref = if inline_set.is_empty() { None } else { Some(&inline_set) };

    let token_set: HashSet<String> = cached
        .tokens
        .iter()
        .map(|tok| tok.rule_name.clone())
        .collect();
    let token_ref = if token_set.is_empty() { None } else { Some(&token_set) };

    let mut debug_set: HashSet<String> = HashSet::new();
    let mut debug_all = false;
    for dbg in &cached.debugs {
        if dbg.rule_name == "*" {
            debug_all = true;
        } else {
            debug_set.insert(dbg.rule_name.clone());
        }
    }
    let debug_ref = if debug_set.is_empty() { None } else { Some(&debug_set) };

    let ws_pattern = cached.ws_pattern.as_ref().map(|ws| ws.pattern.as_str());

    let options = PipelineOptions::default();

    let ir = match compile_ast(
        ast,
        &recover_map,
        &pretty_map,
        &no_collapse_set,
        &options,
        ws_pattern,
        inline_ref,
        token_ref,
        debug_ref,
        debug_all,
    ) {
        Ok(ir) => ir,
        Err(_) => return HashMap::new(),
    };

    // Build a lookup from RuleId → TypeDesc.
    let type_map: HashMap<u32, &bbnf_ir::TypeDesc> = ir
        .types
        .iter()
        .map(|(id, td)| (*id, td))
        .collect();

    let mut result = HashMap::new();
    for rule in &ir.rules {
        let name = ir.get_string(rule.name).to_string();

        let follow_set_label = ir.follow_sets.get(&rule.id).map(|cs| {
            format_charset_iter(cs.iter())
        });

        let inferred_type = type_map.get(&rule.id).map(|td| format_type_desc(td, &ir));

        result.insert(name, IrRuleMeta {
            follow_set_label,
            has_dispatch: rule.meta.dispatch.is_some(),
            memo_strategy: format!("{:?}", rule.meta.memo),
            span_eligible: rule.meta.span_eligible,
            has_sp_method: rule.meta.has_sp_method,
            inferred_type,
            force_inline: rule.meta.force_inline,
            is_transparent: rule.meta.is_transparent,
        });
    }

    result
}

/// Format a set of byte values for display (e.g., `{'a', 'b', 0x0a}`).
fn format_charset_iter(iter: impl IntoIterator<Item = u8>) -> String {
    let chars: Vec<u8> = iter.into_iter().collect();
    if chars.is_empty() {
        return "\u{2205}".into(); // ∅
    }
    let formatted: Vec<String> = chars
        .iter()
        .map(|&b| {
            if b.is_ascii_graphic() {
                format!("'{}'", b as char)
            } else {
                format!("0x{:02x}", b)
            }
        })
        .collect();
    format!("{{{}}}", formatted.join(", "))
}

/// Format a `TypeDesc` as a human-readable string.
fn format_type_desc(td: &bbnf_ir::TypeDesc, ir: &bbnf_ir::GrammarIR) -> String {
    match td {
        bbnf_ir::TypeDesc::Span => "Span".into(),
        bbnf_ir::TypeDesc::F64 => "f64".into(),
        bbnf_ir::TypeDesc::U32 => "u32".into(),
        bbnf_ir::TypeDesc::Option(inner) => format!("Option<{}>", format_type_desc(inner, ir)),
        bbnf_ir::TypeDesc::Vec(inner) => format!("Vec<{}>", format_type_desc(inner, ir)),
        bbnf_ir::TypeDesc::Tuple(items) => {
            let parts: Vec<_> = items.iter().map(|t| format_type_desc(t, ir)).collect();
            format!("({})", parts.join(", "))
        }
        bbnf_ir::TypeDesc::BoxedEnum => "Box<Enum>".into(),
        bbnf_ir::TypeDesc::Enum => "Enum".into(),
        bbnf_ir::TypeDesc::Named(id) => ir.get_string(*id).to_string(),
    }
}
