#![allow(non_snake_case)]

use bbnf_analysis::analysis::LineIndex;
use bbnf_analysis::state::diagnostics::analyze;
use bbnf_analysis::state::DocumentState;
use gorgeous::PrinterConfig;
use ls_types::DiagnosticSeverity;
use serde::Serialize;
use wasm_bindgen::prelude::*;

// ---------------------------------------------------------------------------
// Shared conversion types
// ---------------------------------------------------------------------------

#[derive(Serialize, Clone)]
struct WasmRange {
    start_line: u32,
    start_character: u32,
    end_line: u32,
    end_character: u32,
}

fn range_to_wasm(r: &ls_types::Range) -> WasmRange {
    WasmRange {
        start_line: r.start.line,
        start_character: r.start.character,
        end_line: r.end.line,
        end_character: r.end.character,
    }
}

// ---------------------------------------------------------------------------
// Gorgeous formatters (pre-built, compile-time codegen)
// ---------------------------------------------------------------------------

fn make_config(max_width: usize, indent: usize, use_tabs: bool) -> PrinterConfig {
    PrinterConfig {
        max_width,
        indent,
        use_tabs,
    }
}

#[wasm_bindgen]
pub fn format_json(
    input: &str,
    max_width: usize,
    indent: usize,
    use_tabs: bool,
) -> Option<String> {
    gorgeous::json::prettify_json(input, &make_config(max_width, indent, use_tabs))
}

#[wasm_bindgen]
pub fn format_css(
    input: &str,
    max_width: usize,
    indent: usize,
    use_tabs: bool,
) -> Option<String> {
    gorgeous::css::prettify_css(input, &make_config(max_width, indent, use_tabs))
}

#[wasm_bindgen]
pub fn format_bnf(
    input: &str,
    max_width: usize,
    indent: usize,
    use_tabs: bool,
) -> Option<String> {
    gorgeous::bnf::prettify_bnf(input, &make_config(max_width, indent, use_tabs))
}

#[wasm_bindgen]
pub fn format_ebnf(
    input: &str,
    max_width: usize,
    indent: usize,
    use_tabs: bool,
) -> Option<String> {
    gorgeous::ebnf::prettify_ebnf(input, &make_config(max_width, indent, use_tabs))
}

#[wasm_bindgen]
pub fn format_bbnf(
    input: &str,
    max_width: usize,
    indent: usize,
    use_tabs: bool,
) -> Option<String> {
    gorgeous::bbnf::prettify_bbnf(input, &make_config(max_width, indent, use_tabs))
}

// ---------------------------------------------------------------------------
// BBNF analysis (grammar diagnostics, hover, completion, semantic tokens)
// ---------------------------------------------------------------------------

#[derive(Serialize)]
struct WasmDiagnostic {
    line: u32,
    character: u32,
    end_line: u32,
    end_character: u32,
    severity: u8, // 1=Error, 2=Warning, 3=Info, 4=Hint
    message: String,
}

#[derive(Serialize)]
struct WasmRule {
    name: String,
    rhs: String,
}

#[derive(Serialize)]
struct WasmSemanticToken {
    line: u32,
    start_char: u32,
    length: u32,
    token_type: u32,
}

#[derive(Serialize)]
struct WasmAnalysisResult {
    diagnostics: Vec<WasmDiagnostic>,
    rules: Vec<WasmRule>,
    first_sets: Vec<(String, String)>,
    nullable: Vec<String>,
    semantic_tokens: Vec<WasmSemanticToken>,
}

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

    serde_wasm_bindgen::to_value(&result).unwrap()
}

// ---------------------------------------------------------------------------
// Hover
// ---------------------------------------------------------------------------

#[derive(Serialize)]
struct WasmHoverResult {
    contents: String,
}

#[wasm_bindgen]
pub fn hover_at_offset(text: &str, offset: usize) -> JsValue {
    let state = DocumentState::new(text.to_string());
    let position = state.line_index.offset_to_position(offset);

    let hover = bbnf_analysis::features::hover::hover(&state, position);

    match hover {
        Some(h) => {
            let contents = match h.contents {
                ls_types::HoverContents::Markup(m) => m.value,
                ls_types::HoverContents::Scalar(s) => match s {
                    ls_types::MarkedString::String(s) => s,
                    ls_types::MarkedString::LanguageString(ls) => ls.value,
                },
                ls_types::HoverContents::Array(arr) => arr
                    .into_iter()
                    .map(|s| match s {
                        ls_types::MarkedString::String(s) => s,
                        ls_types::MarkedString::LanguageString(ls) => ls.value,
                    })
                    .collect::<Vec<_>>()
                    .join("\n"),
            };
            serde_wasm_bindgen::to_value(&WasmHoverResult { contents }).unwrap()
        }
        None => JsValue::NULL,
    }
}

// ---------------------------------------------------------------------------
// Completions
// ---------------------------------------------------------------------------

fn completion_kind_to_u32(kind: Option<ls_types::CompletionItemKind>) -> u32 {
    match kind {
        Some(k) if k == ls_types::CompletionItemKind::FUNCTION => 3,
        Some(k) if k == ls_types::CompletionItemKind::KEYWORD => 14,
        Some(k) if k == ls_types::CompletionItemKind::ENUM_MEMBER => 20,
        _ => 0,
    }
}

#[derive(Serialize)]
struct WasmCompletionItem {
    label: String,
    kind: u32,
    detail: Option<String>,
}

#[wasm_bindgen]
pub fn completions(text: &str) -> JsValue {
    let state = DocumentState::new(text.to_string());
    let response = bbnf_analysis::features::completion::completion(&state);

    let items: Vec<WasmCompletionItem> = match response {
        ls_types::CompletionResponse::Array(items) => items
            .into_iter()
            .map(|i| WasmCompletionItem {
                label: i.label,
                kind: completion_kind_to_u32(i.kind),
                detail: i.detail,
            })
            .collect(),
        _ => Vec::new(),
    };

    serde_wasm_bindgen::to_value(&items).unwrap()
}

// ---------------------------------------------------------------------------
// Semantic tokens (delta-encoded)
// ---------------------------------------------------------------------------

#[derive(Serialize)]
struct WasmSemanticTokenDelta {
    delta_line: u32,
    delta_start: u32,
    length: u32,
    token_type: u32,
    token_modifiers: u32,
}

#[wasm_bindgen]
pub fn semantic_tokens_full(text: &str) -> JsValue {
    let state = DocumentState::new(text.to_string());
    let result = bbnf_analysis::features::semantic_tokens::semantic_tokens_full(&state);

    let tokens: Vec<WasmSemanticTokenDelta> = match result {
        ls_types::SemanticTokensResult::Tokens(tokens) => tokens
            .data
            .iter()
            .map(|t| WasmSemanticTokenDelta {
                delta_line: t.delta_line,
                delta_start: t.delta_start,
                length: t.length,
                token_type: t.token_type,
                token_modifiers: t.token_modifiers_bitset,
            })
            .collect(),
        _ => Vec::new(),
    };

    serde_wasm_bindgen::to_value(&tokens).unwrap()
}

// ---------------------------------------------------------------------------
// Inlay hints
// ---------------------------------------------------------------------------

#[derive(Serialize)]
struct WasmInlayHint {
    line: u32,
    character: u32,
    label: String,
    kind: u32, // 1=Type, 2=Parameter
    tooltip: Option<String>,
    padding_left: bool,
}

#[wasm_bindgen]
pub fn inlay_hints(text: &str, start_line: u32, end_line: u32) -> JsValue {
    let state = DocumentState::new(text.to_string());
    let range = ls_types::Range::new(
        ls_types::Position::new(start_line, 0),
        ls_types::Position::new(end_line, u32::MAX),
    );

    let hints = bbnf_analysis::features::inlay_hints::inlay_hints(&state, range);

    let wasm_hints: Vec<WasmInlayHint> = hints
        .into_iter()
        .map(|h| {
            let label = match h.label {
                ls_types::InlayHintLabel::String(s) => s,
                ls_types::InlayHintLabel::LabelParts(parts) => parts
                    .into_iter()
                    .map(|p| p.value)
                    .collect::<Vec<_>>()
                    .join(""),
            };
            let tooltip = h.tooltip.and_then(|t| match t {
                ls_types::InlayHintTooltip::String(s) => Some(s),
                ls_types::InlayHintTooltip::MarkupContent(m) => Some(m.value),
            });
            let kind = match h.kind {
                Some(ls_types::InlayHintKind::TYPE) => 1,
                Some(ls_types::InlayHintKind::PARAMETER) => 2,
                _ => 0,
            };
            WasmInlayHint {
                line: h.position.line,
                character: h.position.character,
                label,
                kind,
                tooltip,
                padding_left: h.padding_left.unwrap_or(false),
            }
        })
        .collect();

    serde_wasm_bindgen::to_value(&wasm_hints).unwrap()
}

// ---------------------------------------------------------------------------
// Go to definition
// ---------------------------------------------------------------------------

#[derive(Serialize)]
struct WasmDefinitionResult {
    range: WasmRange,
}

#[wasm_bindgen]
pub fn goto_definition(text: &str, offset: usize) -> JsValue {
    let state = DocumentState::new(text.to_string());
    let position = state.line_index.offset_to_position(offset);
    let uri: ls_types::Uri = "file:///dummy".parse().unwrap();

    let result =
        bbnf_analysis::features::goto_definition::goto_definition(&state, &uri, position);

    match result {
        Some(ls_types::GotoDefinitionResponse::Scalar(loc)) => {
            serde_wasm_bindgen::to_value(&WasmDefinitionResult {
                range: range_to_wasm(&loc.range),
            })
            .unwrap()
        }
        _ => JsValue::NULL,
    }
}

// ---------------------------------------------------------------------------
// Document symbols
// ---------------------------------------------------------------------------

#[derive(Serialize)]
struct WasmDocumentSymbol {
    name: String,
    detail: Option<String>,
    kind: u32,
    range: WasmRange,
    selection_range: WasmRange,
}

#[wasm_bindgen]
pub fn document_symbols(text: &str) -> JsValue {
    let state = DocumentState::new(text.to_string());
    let response = bbnf_analysis::features::document_symbols::document_symbols(&state);

    let symbols: Vec<WasmDocumentSymbol> = match response {
        ls_types::DocumentSymbolResponse::Nested(symbols) => symbols
            .into_iter()
            .map(|s| WasmDocumentSymbol {
                name: s.name,
                detail: s.detail,
                // All BBNF rules map to SymbolKind::FUNCTION (12)
                kind: 12,
                range: range_to_wasm(&s.range),
                selection_range: range_to_wasm(&s.selection_range),
            })
            .collect(),
        _ => Vec::new(),
    };

    serde_wasm_bindgen::to_value(&symbols).unwrap()
}

// ---------------------------------------------------------------------------
// Folding ranges
// ---------------------------------------------------------------------------

#[derive(Serialize)]
struct WasmFoldingRange {
    start_line: u32,
    start_character: Option<u32>,
    end_line: u32,
    end_character: Option<u32>,
    collapsed_text: Option<String>,
}

#[wasm_bindgen]
pub fn folding_ranges(text: &str) -> JsValue {
    let state = DocumentState::new(text.to_string());
    let ranges = bbnf_analysis::features::folding::folding_ranges(&state);

    let wasm_ranges: Vec<WasmFoldingRange> = ranges
        .into_iter()
        .map(|r| WasmFoldingRange {
            start_line: r.start_line,
            start_character: r.start_character,
            end_line: r.end_line,
            end_character: r.end_character,
            collapsed_text: r.collapsed_text,
        })
        .collect();

    serde_wasm_bindgen::to_value(&wasm_ranges).unwrap()
}

// ---------------------------------------------------------------------------
// Selection ranges
// ---------------------------------------------------------------------------

#[derive(Serialize)]
struct WasmSelectionRange {
    range: WasmRange,
    parent: Option<Box<WasmSelectionRange>>,
}

fn selection_range_to_wasm(sr: ls_types::SelectionRange) -> WasmSelectionRange {
    WasmSelectionRange {
        range: range_to_wasm(&sr.range),
        parent: sr.parent.map(|p| Box::new(selection_range_to_wasm(*p))),
    }
}

#[wasm_bindgen]
pub fn selection_ranges(text: &str, offsets: Vec<usize>) -> JsValue {
    let state = DocumentState::new(text.to_string());

    // Guard: the feature panics if there's no parsed AST
    if state.ast().is_none() {
        return serde_wasm_bindgen::to_value(&Vec::<WasmSelectionRange>::new()).unwrap();
    }

    let positions: Vec<ls_types::Position> = offsets
        .iter()
        .map(|&o| state.line_index.offset_to_position(o))
        .collect();

    let ranges =
        bbnf_analysis::features::selection_range::selection_ranges(&state, positions);

    let wasm_ranges: Vec<WasmSelectionRange> =
        ranges.into_iter().map(selection_range_to_wasm).collect();

    serde_wasm_bindgen::to_value(&wasm_ranges).unwrap()
}

// ---------------------------------------------------------------------------
// Code actions
// ---------------------------------------------------------------------------

#[derive(Serialize)]
struct WasmTextEdit {
    range: WasmRange,
    new_text: String,
}

#[derive(Serialize)]
struct WasmCodeAction {
    title: String,
    kind: Option<String>,
    edits: Vec<WasmTextEdit>,
}

#[wasm_bindgen]
pub fn code_actions(text: &str, start_offset: usize, end_offset: usize) -> JsValue {
    let state = DocumentState::new(text.to_string());
    let uri: ls_types::Uri = "file:///dummy".parse().unwrap();
    let range = ls_types::Range::new(
        state.line_index.offset_to_position(start_offset),
        state.line_index.offset_to_position(end_offset),
    );

    let response =
        bbnf_analysis::features::code_actions::code_actions(&state, &uri, range);

    let actions: Vec<WasmCodeAction> = response
        .into_iter()
        .filter_map(|item| match item {
            ls_types::CodeActionOrCommand::CodeAction(action) => {
                let edits = action
                    .edit
                    .and_then(|e| e.changes)
                    .into_iter()
                    .flat_map(|changes| {
                        changes.into_values().flat_map(|edits| {
                            edits.into_iter().map(|edit| WasmTextEdit {
                                range: range_to_wasm(&edit.range),
                                new_text: edit.new_text,
                            })
                        })
                    })
                    .collect();
                Some(WasmCodeAction {
                    title: action.title,
                    kind: action.kind.map(|k| k.as_str().to_string()),
                    edits,
                })
            }
            _ => None,
        })
        .collect();

    serde_wasm_bindgen::to_value(&actions).unwrap()
}

// ---------------------------------------------------------------------------
// Code lens
// ---------------------------------------------------------------------------

#[derive(Serialize)]
struct WasmCodeLens {
    range: WasmRange,
    title: String,
}

#[wasm_bindgen]
pub fn code_lens(text: &str) -> JsValue {
    let state = DocumentState::new(text.to_string());
    let lenses = bbnf_analysis::features::code_lens::code_lens(&state);

    let wasm_lenses: Vec<WasmCodeLens> = lenses
        .into_iter()
        .filter_map(|lens| {
            let title = lens.command.as_ref()?.title.clone();
            Some(WasmCodeLens {
                range: range_to_wasm(&lens.range),
                title,
            })
        })
        .collect();

    serde_wasm_bindgen::to_value(&wasm_lenses).unwrap()
}
