//! BBNF grammar analysis: diagnostics, rule listing, FIRST sets, semantic tokens.

use bbnf_analysis::analysis::LineIndex;
use bbnf_analysis::state::diagnostics::analyze;
use ls_types::DiagnosticSeverity;
use serde::Serialize;
use wasm_bindgen::prelude::*;

use crate::to_js_value;

// ── Types ───────────────────────────────────────────────────────────────────

#[derive(Serialize)]
pub(crate) struct WasmDiagnostic {
    line: u32,
    character: u32,
    end_line: u32,
    end_character: u32,
    severity: u8, // 1=Error, 2=Warning, 3=Info, 4=Hint
    message: String,
}

#[derive(Serialize)]
pub(crate) struct WasmRule {
    name: String,
    rhs: String,
}

#[derive(Serialize)]
pub(crate) struct WasmSemanticToken {
    line: u32,
    start_char: u32,
    length: u32,
    token_type: u32,
}

#[derive(Serialize)]
pub(crate) struct WasmAnalysisResult {
    diagnostics: Vec<WasmDiagnostic>,
    rules: Vec<WasmRule>,
    first_sets: Vec<(String, String)>,
    nullable: Vec<String>,
    semantic_tokens: Vec<WasmSemanticToken>,
}

// ── Export ───────────────────────────────────────────────────────────────────

#[wasm_bindgen]
pub fn analyze_grammar(text: &str) -> JsValue {
    let line_index = LineIndex::new(text);
    let info = analyze(text, &line_index);

    let diagnostics: Vec<WasmDiagnostic> = info
        .diagnostics
        .iter()
        .map(|d| {
            let severity = match d.severity {
                Some(s) if s == DiagnosticSeverity::ERROR => 1,
                Some(s) if s == DiagnosticSeverity::WARNING => 2,
                Some(s) if s == DiagnosticSeverity::INFORMATION => 3,
                Some(s) if s == DiagnosticSeverity::HINT => 4,
                _ => 1,
            };
            WasmDiagnostic {
                line: d.range.start.line,
                character: d.range.start.character,
                end_line: d.range.end.line,
                end_character: d.range.end.character,
                severity,
                message: d.message.clone(),
            }
        })
        .collect();

    let rules: Vec<WasmRule> = info
        .rules
        .iter()
        .map(|r| WasmRule {
            name: r.name.clone(),
            rhs: r.rhs_text.clone(),
        })
        .collect();

    let first_sets: Vec<(String, String)> = info
        .first_set_labels
        .iter()
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();

    let nullable: Vec<String> = info.nullable_rules.iter().cloned().collect();

    let semantic_tokens: Vec<WasmSemanticToken> = info
        .semantic_tokens
        .iter()
        .map(|t| {
            let pos = line_index.offset_to_position(t.span.0);
            WasmSemanticToken {
                line: pos.line,
                start_char: pos.character,
                length: (t.span.1 - t.span.0) as u32,
                token_type: t.token_type,
            }
        })
        .collect();

    let result = WasmAnalysisResult {
        diagnostics,
        rules,
        first_sets,
        nullable,
        semantic_tokens,
    };

    to_js_value(&result)
}
