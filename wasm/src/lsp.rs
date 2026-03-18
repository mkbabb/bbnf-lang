//! LSP feature exports — full 17/17 parity: hover, completions, semantic tokens, inlay hints,
//! goto definition, document symbols, folding, selection ranges, code actions, code lens,
//! references, prepare rename, rename, document formatting, range formatting, on-type formatting,
//! full sync.

use std::cell::RefCell;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use bbnf_analysis::state::DocumentState;
use serde::Serialize;
use wasm_bindgen::prelude::*;

use crate::{WasmRange, range_to_wasm, to_js_value};

// ── Thread-local DocumentState cache ────────────────────────────────────────
//
// Every LSP feature call previously did `DocumentState::new(text.to_string())` —
// full re-parse + Tarjan SCC + FIRST sets + analysis. The playground calls hover,
// completions, semantic tokens, etc. sequentially on the same text, so this cache
// deduplicates 3-5 parses per keystroke down to 1.

thread_local! {
    static DOC_CACHE: RefCell<Option<(u64, DocumentState)>> = RefCell::new(None);
}

fn text_hash(text: &str) -> u64 {
    let mut h = DefaultHasher::new();
    text.hash(&mut h);
    h.finish()
}

fn with_state<R>(text: &str, f: impl FnOnce(&DocumentState) -> R) -> R {
    let hash = text_hash(text);
    DOC_CACHE.with(|cache| {
        let mut c = cache.borrow_mut();
        if c.as_ref().map_or(true, |(h, _)| *h != hash) {
            *c = Some((hash, DocumentState::new(text.to_string())));
        }
        f(&c.as_ref().unwrap().1)
    })
}

// ── Hover ───────────────────────────────────────────────────────────────────

#[derive(Serialize)]
struct WasmHoverResult {
    contents: String,
}

#[wasm_bindgen]
pub fn hover_at_offset(text: &str, offset: usize) -> JsValue {
    with_state(text, |state| {
        let position = state.line_index.offset_to_position(offset);
        let hover = bbnf_analysis::features::hover::hover(state, position);

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
                to_js_value(&WasmHoverResult { contents })
            }
            None => JsValue::NULL,
        }
    })
}

// ── Completions ─────────────────────────────────────────────────────────────

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
    with_state(text, |state| {
        let response = bbnf_analysis::features::completion::completion(state);

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

        to_js_value(&items)
    })
}

// ── Semantic tokens (delta-encoded) ─────────────────────────────────────────

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
    with_state(text, |state| {
        let result = bbnf_analysis::features::semantic_tokens::semantic_tokens_full(state);

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

        to_js_value(&tokens)
    })
}

// ── Inlay hints ─────────────────────────────────────────────────────────────

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
    with_state(text, |state| {
        let range = ls_types::Range::new(
            ls_types::Position::new(start_line, 0),
            ls_types::Position::new(end_line, u32::MAX),
        );

        let hints = bbnf_analysis::features::inlay_hints::inlay_hints(state, range);

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

        to_js_value(&wasm_hints)
    })
}

// ── Go to definition ────────────────────────────────────────────────────────

#[derive(Serialize)]
struct WasmDefinitionResult {
    range: WasmRange,
}

#[wasm_bindgen]
pub fn goto_definition(text: &str, offset: usize) -> JsValue {
    with_state(text, |state| {
        let position = state.line_index.offset_to_position(offset);
        let uri: ls_types::Uri = "file:///dummy".parse().unwrap();

        let result =
            bbnf_analysis::features::goto_definition::goto_definition(state, &uri, position);

        match result {
            Some(ls_types::GotoDefinitionResponse::Scalar(loc)) => {
                to_js_value(&WasmDefinitionResult {
                    range: range_to_wasm(&loc.range),
                })
            }
            _ => JsValue::NULL,
        }
    })
}

// ── Document symbols ────────────────────────────────────────────────────────

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
    with_state(text, |state| {
        let response = bbnf_analysis::features::document_symbols::document_symbols(state);

        let symbols: Vec<WasmDocumentSymbol> = match response {
            ls_types::DocumentSymbolResponse::Nested(symbols) => symbols
                .into_iter()
                .map(|s| WasmDocumentSymbol {
                    name: s.name,
                    detail: s.detail,
                    kind: 12,
                    range: range_to_wasm(&s.range),
                    selection_range: range_to_wasm(&s.selection_range),
                })
                .collect(),
            _ => Vec::new(),
        };

        to_js_value(&symbols)
    })
}

// ── Folding ranges ──────────────────────────────────────────────────────────

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
    with_state(text, |state| {
        let ranges = bbnf_analysis::features::folding::folding_ranges(state);

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

        to_js_value(&wasm_ranges)
    })
}

// ── Selection ranges ────────────────────────────────────────────────────────

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
    with_state(text, |state| {
        if state.ast().is_none() {
            return to_js_value(&Vec::<WasmSelectionRange>::new());
        }

        let positions: Vec<ls_types::Position> = offsets
            .iter()
            .map(|&o| state.line_index.offset_to_position(o))
            .collect();

        let ranges =
            bbnf_analysis::features::selection_range::selection_ranges(state, positions);

        let wasm_ranges: Vec<WasmSelectionRange> =
            ranges.into_iter().map(selection_range_to_wasm).collect();

        to_js_value(&wasm_ranges)
    })
}

// ── Code actions ────────────────────────────────────────────────────────────

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
    with_state(text, |state| {
        let uri: ls_types::Uri = "file:///dummy".parse().unwrap();
        let range = ls_types::Range::new(
            state.line_index.offset_to_position(start_offset),
            state.line_index.offset_to_position(end_offset),
        );

        let response =
            bbnf_analysis::features::code_actions::code_actions(state, &uri, range);

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

        to_js_value(&actions)
    })
}

// ── Code lens ───────────────────────────────────────────────────────────────

#[derive(Serialize)]
struct WasmCodeLens {
    range: WasmRange,
    title: String,
}

#[wasm_bindgen]
pub fn code_lens(text: &str) -> JsValue {
    with_state(text, |state| {
        let lenses = bbnf_analysis::features::code_lens::code_lens(state);

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

        to_js_value(&wasm_lenses)
    })
}

// ── References ─────────────────────────────────────────────────────────────

#[derive(Serialize)]
struct WasmLocation {
    range: WasmRange,
}

#[wasm_bindgen]
pub fn find_references(text: &str, offset: usize) -> JsValue {
    with_state(text, |state| {
        let position = state.line_index.offset_to_position(offset);
        let uri: ls_types::Uri = "file:///dummy".parse().unwrap();

        let result =
            bbnf_analysis::features::references::references(state, &uri, position, true);

        match result {
            Some(locations) => {
                let wasm_locs: Vec<WasmLocation> = locations
                    .into_iter()
                    .map(|loc| WasmLocation {
                        range: range_to_wasm(&loc.range),
                    })
                    .collect();
                to_js_value(&wasm_locs)
            }
            None => JsValue::NULL,
        }
    })
}

// ── Prepare rename ─────────────────────────────────────────────────────────

#[derive(Serialize)]
struct WasmPrepareRename {
    range: WasmRange,
    placeholder: String,
}

#[wasm_bindgen]
pub fn prepare_rename(text: &str, offset: usize) -> JsValue {
    with_state(text, |state| {
        let position = state.line_index.offset_to_position(offset);
        let result = bbnf_analysis::features::rename::prepare_rename(state, position);

        match result {
            Some(ls_types::PrepareRenameResponse::RangeWithPlaceholder { range, placeholder }) => {
                to_js_value(&WasmPrepareRename {
                    range: range_to_wasm(&range),
                    placeholder,
                })
            }
            _ => JsValue::NULL,
        }
    })
}

// ── Rename ─────────────────────────────────────────────────────────────────

#[wasm_bindgen]
pub fn rename_symbol(text: &str, offset: usize, new_name: &str) -> JsValue {
    with_state(text, |state| {
        let position = state.line_index.offset_to_position(offset);
        let uri: ls_types::Uri = "file:///dummy".parse().unwrap();

        let result =
            bbnf_analysis::features::rename::rename(state, &uri, position, new_name);

        match result {
            Some(edit) => {
                let edits: Vec<WasmTextEdit> = edit
                    .changes
                    .into_iter()
                    .flat_map(|changes| {
                        changes.into_values().flat_map(|edits| {
                            edits.into_iter().map(|e| WasmTextEdit {
                                range: range_to_wasm(&e.range),
                                new_text: e.new_text,
                            })
                        })
                    })
                    .collect();
                to_js_value(&edits)
            }
            None => JsValue::NULL,
        }
    })
}

// ── Document formatting ────────────────────────────────────────────────────

#[wasm_bindgen]
pub fn format_document(text: &str) -> JsValue {
    with_state(text, |state| {
        let result = bbnf_analysis::features::formatting::format_document(state);

        match result {
            Some(edits) => {
                let wasm_edits: Vec<WasmTextEdit> = edits
                    .into_iter()
                    .map(|e| WasmTextEdit {
                        range: range_to_wasm(&e.range),
                        new_text: e.new_text,
                    })
                    .collect();
                to_js_value(&wasm_edits)
            }
            None => JsValue::NULL,
        }
    })
}

// ── Range formatting ───────────────────────────────────────────────────────

#[wasm_bindgen]
pub fn format_range(text: &str, start_offset: usize, end_offset: usize) -> JsValue {
    with_state(text, |state| {
        let range = ls_types::Range::new(
            state.line_index.offset_to_position(start_offset),
            state.line_index.offset_to_position(end_offset),
        );

        let result = bbnf_analysis::features::formatting::format_range(state, range);

        match result {
            Some(edits) => {
                let wasm_edits: Vec<WasmTextEdit> = edits
                    .into_iter()
                    .map(|e| WasmTextEdit {
                        range: range_to_wasm(&e.range),
                        new_text: e.new_text,
                    })
                    .collect();
                to_js_value(&wasm_edits)
            }
            None => JsValue::NULL,
        }
    })
}

// ── On-type formatting ─────────────────────────────────────────────────────

#[wasm_bindgen]
pub fn on_type_format(text: &str, offset: usize) -> JsValue {
    with_state(text, |state| {
        let position = state.line_index.offset_to_position(offset);
        let result = bbnf_analysis::features::formatting::format_on_type(state, position);

        match result {
            Some(edits) => {
                let wasm_edits: Vec<WasmTextEdit> = edits
                    .into_iter()
                    .map(|e| WasmTextEdit {
                        range: range_to_wasm(&e.range),
                        new_text: e.new_text,
                    })
                    .collect();
                to_js_value(&wasm_edits)
            }
            None => JsValue::NULL,
        }
    })
}

// ── Full sync (stateless re-parse) ─────────────────────────────────────────
//
// Incremental sync requires persistent state across calls, which the stateless
// WASM export model doesn't support. Instead, `full_sync` performs a complete
// re-parse and returns diagnostics — the caller can invoke this after every edit.

#[derive(Serialize)]
struct WasmDiagnostic {
    range: WasmRange,
    severity: u32, // 1=Error, 2=Warning, 3=Information, 4=Hint
    message: String,
}

#[wasm_bindgen]
pub fn full_sync(text: &str) -> JsValue {
    with_state(text, |state| {
        let wasm_diags: Vec<WasmDiagnostic> = state
            .info
            .diagnostics
            .iter()
            .map(|d| WasmDiagnostic {
                range: range_to_wasm(&d.range),
                severity: d
                    .severity
                    .map(|s| match s {
                        ls_types::DiagnosticSeverity::ERROR => 1,
                        ls_types::DiagnosticSeverity::WARNING => 2,
                        ls_types::DiagnosticSeverity::INFORMATION => 3,
                        ls_types::DiagnosticSeverity::HINT => 4,
                        _ => 1,
                    })
                    .unwrap_or(1),
                message: d.message.clone(),
            })
            .collect();

        to_js_value(&wasm_diags)
    })
}

// ── Batch LSP export ────────────────────────────────────────────────────────
//
// Runs multiple LSP features on a single DocumentState, eliminating per-feature
// string copies across the JS↔WASM boundary. Combined with the thread-local cache,
// this reduces per-keystroke WASM calls from ~5 to 1.

#[derive(Serialize)]
struct WasmLspBatch {
    hover: Option<WasmHoverResult>,
    completions: Vec<WasmCompletionItem>,
    semantic_tokens: Vec<WasmSemanticTokenDelta>,
    inlay_hints: Vec<WasmInlayHint>,
    diagnostics: Vec<WasmDiagnostic>,
}

/// Run hover + completions + semantic tokens + inlay hints + diagnostics in one call.
#[wasm_bindgen]
pub fn lsp_batch(text: &str, offset: usize, start_line: u32, end_line: u32) -> JsValue {
    with_state(text, |state| {
        // Hover
        let position = state.line_index.offset_to_position(offset);
        let hover = bbnf_analysis::features::hover::hover(state, position).map(|h| {
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
            WasmHoverResult { contents }
        });

        // Completions
        let completions = match bbnf_analysis::features::completion::completion(state) {
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

        // Semantic tokens
        let semantic_tokens =
            match bbnf_analysis::features::semantic_tokens::semantic_tokens_full(state) {
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

        // Inlay hints
        let range = ls_types::Range::new(
            ls_types::Position::new(start_line, 0),
            ls_types::Position::new(end_line, u32::MAX),
        );
        let inlay_hints: Vec<WasmInlayHint> =
            bbnf_analysis::features::inlay_hints::inlay_hints(state, range)
                .into_iter()
                .map(|h| {
                    let label = match h.label {
                        ls_types::InlayHintLabel::String(s) => s,
                        ls_types::InlayHintLabel::LabelParts(parts) => {
                            parts.into_iter().map(|p| p.value).collect::<Vec<_>>().join("")
                        }
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

        // Diagnostics
        let diagnostics: Vec<WasmDiagnostic> = state
            .info
            .diagnostics
            .iter()
            .map(|d| WasmDiagnostic {
                range: range_to_wasm(&d.range),
                severity: d
                    .severity
                    .map(|s| match s {
                        ls_types::DiagnosticSeverity::ERROR => 1,
                        ls_types::DiagnosticSeverity::WARNING => 2,
                        ls_types::DiagnosticSeverity::INFORMATION => 3,
                        ls_types::DiagnosticSeverity::HINT => 4,
                        _ => 1,
                    })
                    .unwrap_or(1),
                message: d.message.clone(),
            })
            .collect();

        to_js_value(&WasmLspBatch {
            hover,
            completions,
            semantic_tokens,
            inlay_hints,
            diagnostics,
        })
    })
}
