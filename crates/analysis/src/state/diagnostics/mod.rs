//! Full diagnostic generation: orchestrates AST extraction, reference checks,
//! cycle detection, structural checks, directive validation, and the IR pipeline
//! integration that enriches `DocumentInfo` with per-rule metadata.
//!
//! Split across sub-modules:
//! - [`early`]: stub `DocumentInfo` builders for parse-panic / parse-failure
//!   / empty-AST early returns.
//! - [`extract`]: AST walk that produces `RuleInfo`, the name→index map, and
//!   initial semantic tokens.
//! - [`references`]: undefined-nonterminal + unused-rule detection.
//! - [`cycles`]: left-recursion / cycle-path detection via Tarjan SCC.
//! - [`structure`]: empty-body, alias, and unreachable-rule detection.
//! - [`directives`]: per-directive validation + semantic-token emission glue.
//! - [`ir_analysis`]: IR pipeline integration (FOLLOW sets, dispatch, memo,
//!   projected types, FIRST-set conflict detection).

mod cycles;
mod directives;
mod early;
mod extract;
mod ir_analysis;
mod references;
mod structure;

use bbnf::graph::calculate_ast_deps;
use ls_types::*;

use crate::analysis::LineIndex;

use super::parsing::CachedParseResult;
use super::types::{DocumentInfo, ParseDiagnostics};

/// Analyze a BBNF document using pre-parsed AST and diagnostics from `parse_once()`.
/// This avoids double-parsing: the OwnedAst parses once, and we reuse its results here.
pub fn analyze_from_cache(
    text: &str,
    line_index: &LineIndex,
    cached: Option<&CachedParseResult<'_>>,
    parse_diag: &ParseDiagnostics,
) -> DocumentInfo {
    // Handle parser panic.
    if let Some(msg) = &parse_diag.panic_message {
        return early::empty_doc_info_for_panic(msg);
    }

    let Some(parsed) = cached else {
        // Parse failure -- report error at furthest offset.
        return early::empty_doc_info_for_failure(line_index, parse_diag);
    };

    let mut diagnostics: Vec<Diagnostic> = Vec::new();
    let mut semantic_tokens = Vec::new();

    // Check for incomplete parse (didn't consume all input).
    if parse_diag.offset < text.len() {
        let remaining = &text[parse_diag.offset..];
        if !remaining.trim().is_empty() {
            let pos = line_index.offset_to_position(parse_diag.offset);
            diagnostics.push(Diagnostic {
                range: Range::new(pos, pos),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some(crate::DIAGNOSTIC_SOURCE.into()),
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
        return early::empty_doc_info_for_empty_ast(
            text,
            line_index,
            parse_diag,
            import_infos,
            recover_infos,
        );
    }

    // Extract rule info from AST (string-keyed: name → RuleEntry).
    let (rules, rule_index) =
        extract::extract_rules(ast, line_index, &mut diagnostics, &mut semantic_tokens);

    // Diagnostics: undefined nonterminals and unused rules.
    let defined = references::build_defined_map(&rules);
    let imported_names = references::build_imported_names(&import_infos);

    let mut referenced_names = references::detect_undefined_and_collect_references(
        &rules,
        &defined,
        &imported_names,
        line_index,
        &mut diagnostics,
    );

    // Add directive-referenced rule names before the unused rule check.
    references::add_directive_references(
        &mut referenced_names,
        &recover_infos,
        &pretty_infos,
        &token_infos,
        &debug_infos,
    );

    references::detect_unused_rules(
        &rules,
        &rule_index,
        &referenced_names,
        line_index,
        &mut diagnostics,
    );

    // Left recursion detection via dependency analysis.
    let deps = calculate_ast_deps(ast);
    let (cyclic_rule_paths, scc) =
        cycles::detect_cycles(&deps, &rules, &rule_index, line_index, &mut diagnostics);

    // Empty rule body detection (AST-level -- no FIRST sets needed).
    structure::detect_empty_bodies(ast, &rules, &rule_index, line_index, &mut diagnostics);

    // Alias detection: rules whose RHS is just a nonterminal reference.
    structure::detect_aliases(
        ast,
        &scc,
        &rules,
        &rule_index,
        &imported_names,
        line_index,
        &mut diagnostics,
    );

    // Unreachable rule detection via BFS from root rules.
    structure::detect_unreachable_rules(
        &rules,
        &rule_index,
        &referenced_names,
        line_index,
        &mut diagnostics,
    );

    // ── Directive validation and semantic tokens ─────────────────────────────
    directives::validate_directives(
        &import_infos,
        &recover_infos,
        &pretty_infos,
        &debug_infos,
        &token_infos,
        ws_pattern_info.as_ref(),
        &defined,
        &imported_names,
        line_index,
        &mut diagnostics,
        &mut semantic_tokens,
        &mut referenced_names,
    );

    // Sort semantic tokens by offset for encoding.
    semantic_tokens.sort_by_key(|t| t.span.0);

    // IR pipeline: extract rich metadata (FOLLOW sets, dispatch, memo, types, FIRST sets).
    let ir_analysis = cached
        .map(|c| {
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                ir_analysis::try_compile_ir(c)
            }))
            .unwrap_or_default()
        })
        .unwrap_or_default();

    // FIRST set conflict diagnostics from IR analysis.
    for (rule_name, conflicts) in &ir_analysis.first_set_conflicts {
        if let Some(&idx) = rule_index.get(rule_name.as_str()) {
            let rule = &rules[idx];
            for msg in conflicts {
                diagnostics.push(Diagnostic {
                    range: line_index.span_to_range(rule.name_span.0, rule.name_span.1),
                    severity: Some(DiagnosticSeverity::WARNING),
                    source: Some(crate::DIAGNOSTIC_SOURCE.into()),
                    message: format!("Alternation in `{rule_name}`: {msg}"),
                    ..Default::default()
                });
            }
        }
    }

    DocumentInfo {
        rules,
        diagnostics,
        rule_index,
        semantic_tokens,
        first_set_labels: ir_analysis.first_set_labels,
        nullable_rules: ir_analysis.nullable_rules,
        cyclic_rule_paths,
        imports: import_infos,
        recovers: recover_infos,
        pretties: pretty_infos,
        debugs: debug_infos,
        tokens: token_infos,
        ws_pattern: ws_pattern_info,
        ir_meta: ir_analysis.meta,
    }
}

/// Public convenience wrapper: parses the text and analyzes in one step.
/// Used by tests, WASM, and any code that doesn't need the cached AST.
pub fn analyze(text: &str, line_index: &LineIndex) -> DocumentInfo {
    let (cached, diag) = super::parsing::parse_once(text);
    analyze_from_cache(text, line_index, cached.as_ref(), &diag)
}
