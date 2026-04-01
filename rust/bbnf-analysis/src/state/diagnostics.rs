use std::collections::{HashMap, HashSet};

use bbnf::analysis::{
    calculate_ast_deps, compute_first_sets, find_aliases, find_first_set_conflicts,
    get_nonterminal_name, tarjan_scc,
};
use bbnf::lower::DirectiveSet;
use bbnf::pipeline::{PipelineOptions, compile_ast};
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
use super::types::{DocumentInfo, ParseDiagnostics, RuleInfo, SemanticTokenInfo, token_types};

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

            pretties: Vec::new(),
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

            pretties: Vec::new(),
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
    let pretty_infos = parsed.pretties.clone();
    let debug_infos = parsed.debugs.clone();
    let token_infos = parsed.tokens.clone();
    let ws_pattern_info = parsed.ws_pattern.clone();

    // Check for empty AST on non-empty input -- likely a parse failure not caught above.
    if ast.is_empty()
        && !text.trim().is_empty()
        && import_infos.is_empty()
        && recover_infos.is_empty()
    {
        let furthest = parse_diag.furthest_offset.max(parse_diag.offset);
        let pos = line_index.offset_to_position(furthest.min(text.len()));
        diagnostics.push(Diagnostic {
            range: Range::new(Position::new(0, 0), pos),
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some("bbnf".into()),
            message:
                "Failed to parse any rules. Check syntax (each rule needs: name = expression ;)"
                    .into(),
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
            pretties: Vec::new(),
            debugs: Vec::new(),
            tokens: Vec::new(),
            ws_pattern: None,
            ir_meta: HashMap::new(),
        };
    }

    // Extract rule info from AST.
    for (lhs, rhs) in ast.iter() {
        if let Expression::Nonterminal(Token {
            value: name,
            span: name_span,
            ..
        }) = lhs
        {
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

    let mut referenced_names: std::collections::HashSet<&str> = std::collections::HashSet::new();

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
    for p in &pretty_infos {
        referenced_names.insert(&p.rule_name);
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
                    message: format!("Rule `{}` participates in a cycle: {}", member, path),
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
                message: format!("Rule `{}` is unreachable from the entry rule", rule.name),
                tags: Some(vec![DiagnosticTag::UNNECESSARY]),
                ..Default::default()
            });
        }
    }

    // ── Directive validation and semantic tokens ─────────────────────────────

    // @import directive semantic tokens.
    semantic_tokens.extend(crate::directives::import::import_semantic_tokens(
        &import_infos,
    ));

    // @recover directive validation and semantic tokens.
    {
        let (rec_diags, rec_tokens) = crate::directives::recover::validate_recovers(
            &recover_infos,
            &defined,
            &imported_names,
            line_index,
        );
        diagnostics.extend(rec_diags);
        semantic_tokens.extend(rec_tokens);

        // Mark recover directive rule names as referenced (for unused rule detection).
        for rec in &recover_infos {
            referenced_names.insert(&rec.rule_name);
        }
    }

    // @pretty directive validation and semantic tokens.
    {
        let (pretty_diags, pretty_tokens) =
            pretty::validate_pretties(&pretty_infos, &defined, &imported_names, line_index);
        diagnostics.extend(pretty_diags);
        semantic_tokens.extend(pretty_tokens);

        // Mark pretty directive rule names as referenced (for unused rule detection).
        for p in &pretty_infos {
            referenced_names.insert(&p.rule_name);
        }
    }

    // @debug directive validation and semantic tokens.
    {
        let (dbg_diags, dbg_tokens) = crate::directives::debug::validate_debugs(
            &debug_infos,
            &defined,
            &imported_names,
            line_index,
        );
        diagnostics.extend(dbg_diags);
        semantic_tokens.extend(dbg_tokens);

        // Mark debug directive rule names as referenced (for unused rule detection).
        for dbg in &debug_infos {
            if dbg.rule_name != "*" {
                referenced_names.insert(&dbg.rule_name);
            }
        }
    }

    // @token directive validation and semantic tokens.
    {
        let (tok_diags, tok_tokens) = crate::directives::token::validate_tokens(
            &token_infos,
            &defined,
            &imported_names,
            line_index,
        );
        diagnostics.extend(tok_diags);
        semantic_tokens.extend(tok_tokens);

        // Mark token directive rule names as referenced (for unused rule detection).
        for tok in &token_infos {
            referenced_names.insert(&tok.rule_name);
        }
    }

    // @ws directive semantic tokens.
    semantic_tokens.extend(crate::directives::ws::ws_semantic_tokens(
        ws_pattern_info.as_ref(),
    ));

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
        pretties: pretty_infos,
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
fn try_compile_ir(cached: &CachedParseResult<'_>) -> HashMap<String, IrRuleMeta> {
    let ast = cached.ast.clone();

    // Reconstruct directive maps from the analysis-layer types.
    let recover_map: HashMap<String, Expression<'_>> = HashMap::new();

    let pretty_map: HashMap<String, Vec<String>> = cached
        .pretties
        .iter()
        .map(|p| (p.rule_name.clone(), p.hints.clone()))
        .collect();

    let token_set: HashSet<String> = cached
        .tokens
        .iter()
        .map(|tok| tok.rule_name.clone())
        .collect();
    let token_ref = if token_set.is_empty() {
        None
    } else {
        Some(&token_set)
    };

    let mut debug_set: HashSet<String> = HashSet::new();
    let mut debug_all = false;
    for dbg in &cached.debugs {
        if dbg.rule_name == "*" {
            debug_all = true;
        } else {
            debug_set.insert(dbg.rule_name.clone());
        }
    }
    let debug_ref = if debug_set.is_empty() {
        None
    } else {
        Some(&debug_set)
    };

    let ws_pattern = cached.ws_pattern.as_ref().map(|ws| ws.pattern.as_str());

    let recovers_ref = if recover_map.is_empty() {
        None
    } else {
        Some(&recover_map)
    };
    let pretties_ref = if pretty_map.is_empty() {
        None
    } else {
        Some(&pretty_map)
    };

    let directives = DirectiveSet {
        recovers: recovers_ref,
        pretties: pretties_ref,
        ws_pattern,
        token_rules: token_ref,
        debug_rules: debug_ref,
        debug_all,
    };

    let options = PipelineOptions::default();

    let ir = match compile_ast(ast, &directives, &options) {
        Ok(ir) => ir,
        Err(_) => return HashMap::new(),
    };

    // Build a lookup from RuleId → TypeDesc.
    let type_map: HashMap<u32, &bbnf_ir::TypeDesc> =
        ir.types.iter().map(|(id, td)| (*id, td)).collect();

    let mut result = HashMap::new();
    for rule in &ir.rules {
        let name = ir.get_string(rule.name).to_string();

        let follow_set_label = ir
            .follow_sets
            .get(&rule.id)
            .map(|cs| format_charset_iter(cs.iter()));

        let inferred_type = type_map.get(&rule.id).map(|td| format_type_desc(td, &ir));

        result.insert(
            name,
            IrRuleMeta {
                follow_set_label,
                has_dispatch: rule.meta.dispatch.is_some(),
                memo_strategy: format!("{:?}", rule.meta.memo),
                span_eligible: rule.meta.span_eligible,
                has_sp_method: rule.meta.has_sp_method,
                inferred_type,
                is_transparent: rule.meta.is_transparent,
            },
        );
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
