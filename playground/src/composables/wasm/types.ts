import type * as monaco from "monaco-editor";

// ---------------------------------------------------------------------------
// Shared range type + converter
// ---------------------------------------------------------------------------

export interface WasmRange {
    start_line: number;
    start_character: number;
    end_line: number;
    end_character: number;
}

/** Convert a 0-indexed WASM range to a 1-indexed Monaco range. */
export function toMonacoRange(r: WasmRange): monaco.IRange {
    return {
        startLineNumber: r.start_line + 1,
        startColumn: r.start_character + 1,
        endLineNumber: r.end_line + 1,
        endColumn: r.end_character + 1,
    };
}

// ---------------------------------------------------------------------------
// Analysis types (existing)
// ---------------------------------------------------------------------------

export interface WasmDiagnostic {
    line: number;
    character: number;
    end_line: number;
    end_character: number;
    severity: number; // 1=Error, 2=Warning, 3=Info, 4=Hint
    message: string;
}

export interface WasmRule {
    name: string;
    rhs: string;
}

export interface WasmSemanticToken {
    line: number;
    start_char: number;
    length: number;
    token_type: number;
}

export interface WasmAnalysisResult {
    diagnostics: WasmDiagnostic[];
    rules: WasmRule[];
    first_sets: [string, string][];
    nullable: string[];
    semantic_tokens: WasmSemanticToken[];
}

export interface WasmHoverResult {
    contents: string;
}

export interface WasmCompletionItem {
    label: string;
    kind: number;
    detail: string | null;
}

// ---------------------------------------------------------------------------
// New LSP feature types
// ---------------------------------------------------------------------------

export interface WasmSemanticTokenDelta {
    delta_line: number;
    delta_start: number;
    length: number;
    token_type: number;
    token_modifiers: number;
}

export interface WasmInlayHint {
    line: number;
    character: number;
    label: string;
    kind: number; // 1=Type, 2=Parameter
    tooltip: string | null;
    padding_left: boolean;
}

export interface WasmDefinitionResult {
    range: WasmRange;
}

export interface WasmDocumentSymbol {
    name: string;
    detail: string | null;
    kind: number;
    range: WasmRange;
    selection_range: WasmRange;
}

export interface WasmFoldingRange {
    start_line: number;
    start_character: number | null;
    end_line: number;
    end_character: number | null;
    collapsed_text: string | null;
}

export interface WasmSelectionRange {
    range: WasmRange;
    parent: WasmSelectionRange | null;
}

export interface WasmTextEdit {
    range: WasmRange;
    new_text: string;
}

export interface WasmCodeAction {
    title: string;
    kind: string | null;
    edits: WasmTextEdit[];
}

export interface WasmCodeLens {
    range: WasmRange;
    title: string;
}
