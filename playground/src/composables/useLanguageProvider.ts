import * as monaco from "monaco-editor";
import { useWasm, ensureWasmLoaded, toMonacoRange } from "./wasm";
import type { WasmDiagnostic, WasmAnalysisResult, WasmSelectionRange } from "./wasm";

const SEVERITY_MAP: Record<number, monaco.MarkerSeverity> = {
    1: monaco.MarkerSeverity.Error,
    2: monaco.MarkerSeverity.Warning,
    3: monaco.MarkerSeverity.Info,
    4: monaco.MarkerSeverity.Hint,
};

// ---------------------------------------------------------------------------
// Semantic token legend — must match LSP server's semantic_token_legend()
// ---------------------------------------------------------------------------

const SEMANTIC_TOKEN_LEGEND: monaco.languages.SemanticTokensLegend = {
    tokenTypes: [
        "ruleDefinition",   // 0
        "ruleReference",    // 1
        "string",           // 2
        "regexp",           // 3
        "operator",         // 4
        "keyword",          // 5
        "comment",          // 6
    ],
    tokenModifiers: [
        "declaration",
        "definition",
    ],
};

// ---------------------------------------------------------------------------
// Symbol kind mapping (LSP SymbolKind u32 → Monaco)
// ---------------------------------------------------------------------------

const SYMBOL_KIND_MAP: Record<number, monaco.languages.SymbolKind> = {
    12: monaco.languages.SymbolKind.Function, // SymbolKind::FUNCTION = 12
};

// ---------------------------------------------------------------------------
// Individual provider registration functions
// ---------------------------------------------------------------------------

function registerHoverProvider(
    wasm: ReturnType<typeof useWasm>,
): monaco.IDisposable {
    return monaco.languages.registerHoverProvider("bbnf", {
        async provideHover(model, position) {
            const text = model.getValue();
            if (!text.includes("=") || !text.includes(";")) return null;

            const offset = model.getOffsetAt(position);
            const result = await wasm.hoverAtOffset(text, offset);
            if (!result) return null;

            return { contents: [{ value: result.contents }] };
        },
    });
}

function registerCompletionProvider(
    wasm: ReturnType<typeof useWasm>,
): monaco.IDisposable {
    return monaco.languages.registerCompletionItemProvider("bbnf", {
        triggerCharacters: ["@"],
        async provideCompletionItems(model, position) {
            const text = model.getValue();
            if (!text.includes("=") || !text.includes(";")) {
                return { suggestions: [] };
            }

            const items = await wasm.getCompletions(text);
            const word = model.getWordUntilPosition(position);
            const range = new monaco.Range(
                position.lineNumber,
                word.startColumn,
                position.lineNumber,
                word.endColumn,
            );

            return {
                suggestions: items.map((item) => ({
                    label: item.label,
                    kind: mapCompletionKind(item.kind),
                    detail: item.detail ?? undefined,
                    insertText: item.label,
                    range,
                })),
            };
        },
    });
}

function registerSemanticTokensProvider(
    wasm: ReturnType<typeof useWasm>,
): monaco.IDisposable {
    return monaco.languages.registerDocumentSemanticTokensProvider("bbnf", {
        getLegend() {
            return SEMANTIC_TOKEN_LEGEND;
        },
        async provideDocumentSemanticTokens(model) {
            const text = model.getValue();
            const tokens = await wasm.getSemanticTokens(text);

            // Monaco expects a flat Uint32Array: [deltaLine, deltaStart, length, tokenType, tokenModifiers, ...]
            const data = new Uint32Array(tokens.length * 5);
            for (let i = 0; i < tokens.length; i++) {
                const t = tokens[i]!;
                const offset = i * 5;
                data[offset] = t.delta_line;
                data[offset + 1] = t.delta_start;
                data[offset + 2] = t.length;
                data[offset + 3] = t.token_type;
                data[offset + 4] = t.token_modifiers;
            }

            return { data };
        },
        releaseDocumentSemanticTokens() {},
    });
}

function registerInlayHintsProvider(
    wasm: ReturnType<typeof useWasm>,
): monaco.IDisposable {
    return monaco.languages.registerInlayHintsProvider("bbnf", {
        async provideInlayHints(model, range) {
            const text = model.getValue();
            // Convert 1-indexed Monaco lines to 0-indexed LSP lines
            const startLine = range.startLineNumber - 1;
            const endLine = range.endLineNumber - 1;
            const hints = await wasm.getInlayHints(text, startLine, endLine);

            return {
                hints: hints.map((h) => ({
                    position: new monaco.Position(h.line + 1, h.character + 1),
                    label: h.label,
                    kind: h.kind === 1
                        ? monaco.languages.InlayHintKind.Type
                        : h.kind === 2
                          ? monaco.languages.InlayHintKind.Parameter
                          : monaco.languages.InlayHintKind.Type,
                    tooltip: h.tooltip ?? undefined,
                    paddingLeft: h.padding_left,
                })),
                dispose() {},
            };
        },
    });
}

function registerDefinitionProvider(
    wasm: ReturnType<typeof useWasm>,
): monaco.IDisposable {
    return monaco.languages.registerDefinitionProvider("bbnf", {
        async provideDefinition(model, position) {
            const text = model.getValue();
            const offset = model.getOffsetAt(position);
            const result = await wasm.gotoDefinition(text, offset);
            if (!result) return null;

            return {
                uri: model.uri,
                range: toMonacoRange(result.range),
            };
        },
    });
}

function registerDocumentSymbolProvider(
    wasm: ReturnType<typeof useWasm>,
): monaco.IDisposable {
    return monaco.languages.registerDocumentSymbolProvider("bbnf", {
        async provideDocumentSymbols(model) {
            const text = model.getValue();
            const symbols = await wasm.getDocumentSymbols(text);

            return symbols.map((s) => ({
                name: s.name,
                detail: s.detail ?? "",
                kind: SYMBOL_KIND_MAP[s.kind] ?? monaco.languages.SymbolKind.Function,
                range: toMonacoRange(s.range),
                selectionRange: toMonacoRange(s.selection_range),
                tags: [],
            }));
        },
    });
}

function registerFoldingRangeProvider(
    wasm: ReturnType<typeof useWasm>,
): monaco.IDisposable {
    return monaco.languages.registerFoldingRangeProvider("bbnf", {
        async provideFoldingRanges(model) {
            const text = model.getValue();
            const ranges = await wasm.getFoldingRanges(text);

            return ranges.map((r) => ({
                start: r.start_line + 1,
                end: r.end_line + 1,
                kind: monaco.languages.FoldingRangeKind.Region,
            }));
        },
    });
}

function registerSelectionRangeProvider(
    wasm: ReturnType<typeof useWasm>,
): monaco.IDisposable {
    return monaco.languages.registerSelectionRangeProvider("bbnf", {
        async provideSelectionRanges(model, positions) {
            const text = model.getValue();
            const offsets = positions.map((p) => model.getOffsetAt(p));
            const ranges = await wasm.getSelectionRanges(text, offsets);

            return ranges.map((sr) => convertSelectionRange(sr));
        },
    });
}

function convertSelectionRange(sr: WasmSelectionRange): monaco.languages.SelectionRange {
    return {
        range: toMonacoRange(sr.range),
        parent: sr.parent ? convertSelectionRange(sr.parent) : undefined,
    } as monaco.languages.SelectionRange;
}

function registerCodeActionProvider(
    wasm: ReturnType<typeof useWasm>,
): monaco.IDisposable {
    return monaco.languages.registerCodeActionProvider("bbnf", {
        async provideCodeActions(model, range) {
            const text = model.getValue();
            const startOffset = model.getOffsetAt({
                lineNumber: range.startLineNumber,
                column: range.startColumn,
            });
            const endOffset = model.getOffsetAt({
                lineNumber: range.endLineNumber,
                column: range.endColumn,
            });
            const actions = await wasm.getCodeActions(text, startOffset, endOffset);

            return {
                actions: actions.map((a) => ({
                    title: a.title,
                    kind: a.kind ?? "quickfix",
                    edit: {
                        edits: a.edits.map((edit) => ({
                            resource: model.uri,
                            textEdit: {
                                range: toMonacoRange(edit.range),
                                text: edit.new_text,
                            },
                            versionId: model.getVersionId(),
                        })),
                    },
                })),
                dispose() {},
            };
        },
    });
}

function registerCodeLensProvider(
    wasm: ReturnType<typeof useWasm>,
): monaco.IDisposable {
    return monaco.languages.registerCodeLensProvider("bbnf", {
        async provideCodeLenses(model) {
            const text = model.getValue();
            const lenses = await wasm.getCodeLens(text);

            return {
                lenses: lenses.map((lens) => ({
                    range: toMonacoRange(lens.range),
                    command: {
                        id: "editor.action.findReferences",
                        title: lens.title,
                    },
                })),
                dispose() {},
            };
        },
    });
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/**
 * Register all BBNF language providers for Monaco using WASM analysis.
 * Returns a disposable to clean up all registrations.
 */
export async function registerBBNFLanguageProvider() {
    await ensureWasmLoaded();
    const wasm = useWasm();

    const disposables: monaco.IDisposable[] = [
        registerHoverProvider(wasm),
        registerCompletionProvider(wasm),
        registerSemanticTokensProvider(wasm),
        registerInlayHintsProvider(wasm),
        registerDefinitionProvider(wasm),
        registerDocumentSymbolProvider(wasm),
        registerFoldingRangeProvider(wasm),
        registerSelectionRangeProvider(wasm),
        registerCodeActionProvider(wasm),
        registerCodeLensProvider(wasm),
    ];

    return {
        dispose() {
            for (const d of disposables) {
                d.dispose();
            }
        },
    };
}

/**
 * Update Monaco editor markers from WASM diagnostics.
 * Call this on grammar text change (debounced).
 */
export async function updateGrammarDiagnostics(
    model: monaco.editor.ITextModel,
) {
    const wasm = useWasm();
    const text = model.getValue();

    if (!text.trim()) {
        monaco.editor.setModelMarkers(model, "bbnf-wasm", []);
        return null;
    }

    try {
        const result = await wasm.analyzeGrammar(text);
        if (!result) return null;

        const markers: monaco.editor.IMarkerData[] = result.diagnostics.map(
            (d: WasmDiagnostic) => ({
                startLineNumber: d.line + 1,
                startColumn: d.character + 1,
                endLineNumber: d.end_line + 1,
                endColumn: d.end_character + 1,
                message: d.message,
                severity: SEVERITY_MAP[d.severity] ?? monaco.MarkerSeverity.Error,
                source: "bbnf-wasm",
            }),
        );

        monaco.editor.setModelMarkers(model, "bbnf-wasm", markers);
        return result;
    } catch {
        return null;
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function mapCompletionKind(kind: number): monaco.languages.CompletionItemKind {
    switch (kind) {
        case 3: return monaco.languages.CompletionItemKind.Function;
        case 14: return monaco.languages.CompletionItemKind.Keyword;
        case 20: return monaco.languages.CompletionItemKind.EnumMember;
        default: return monaco.languages.CompletionItemKind.Text;
    }
}
