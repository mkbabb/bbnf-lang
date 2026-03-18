import { ref } from "vue";
import { ensureWasmLoaded, getWasmModule } from "./loader";
import type {
    WasmAnalysisResult,
    WasmCompletionItem,
    WasmHoverResult,
    WasmSemanticTokenDelta,
    WasmInlayHint,
    WasmDefinitionResult,
    WasmDocumentSymbol,
    WasmFoldingRange,
    WasmSelectionRange,
    WasmCodeAction,
    WasmCodeLens,
    WasmLocation,
    WasmPrepareRename,
    WasmTextEdit,
    WasmParseResult,
    WasmParseCheckResult,
    WasmLspBatch,
} from "./types";

export type {
    WasmRange,
    WasmDiagnostic,
    WasmRule,
    WasmSemanticToken,
    WasmAnalysisResult,
    WasmHoverResult,
    WasmCompletionItem,
    WasmSemanticTokenDelta,
    WasmInlayHint,
    WasmDefinitionResult,
    WasmDocumentSymbol,
    WasmFoldingRange,
    WasmSelectionRange,
    WasmTextEdit,
    WasmCodeAction,
    WasmCodeLens,
    WasmLocation,
    WasmPrepareRename,
    WasmParseResult,
    WasmParseDiagnostic,
    WasmParseCheckResult,
    WasmLspBatch,
    WasmBatchDiagnostic,
} from "./types";

export { toMonacoRange } from "./types";
export { ensureWasmLoaded } from "./loader";

/** Built-in language identifiers that gorgeous can format. */
const BUILTIN_LANGUAGES = new Set(["json", "css", "bnf", "ebnf", "bbnf"]);

/**
 * Detect if the current grammar matches a built-in gorgeous formatter.
 * Returns the language name or null.
 */
export function detectBuiltinLanguage(entryRule: string): string | null {
    const lower = entryRule.toLowerCase();
    if (lower === "value" || lower === "json") return "json";
    if (lower === "stylesheet" || lower === "css") return "css";
    if (lower === "bnf_grammar") return "bnf";
    if (lower === "ebnf_grammar") return "ebnf";
    if (lower === "bbnf_grammar") return "bbnf";
    // "grammar" could be BNF, EBNF, or BBNF — let VM handle ambiguous grammars.
    return null;
}

export function useWasm() {
    const isLoaded = ref(false);
    const isLoading = ref(false);

    async function ensureLoaded() {
        if (isLoaded.value) return;
        isLoading.value = true;
        try {
            await ensureWasmLoaded();
            isLoaded.value = true;
        } finally {
            isLoading.value = false;
        }
    }

    // -----------------------------------------------------------------------
    // Formatting
    // -----------------------------------------------------------------------

    async function formatWithGorgeous(
        lang: string,
        input: string,
        maxWidth: number,
        indent: number,
        useTabs: boolean,
    ): Promise<string | null> {
        await ensureLoaded();
        const mod = getWasmModule();

        switch (lang) {
            case "json":
                return mod.format_json(input, maxWidth, indent, useTabs) ?? null;
            case "css":
                return mod.format_css(input, maxWidth, indent, useTabs) ?? null;
            case "bnf":
                return mod.format_bnf(input, maxWidth, indent, useTabs) ?? null;
            case "ebnf":
                return mod.format_ebnf(input, maxWidth, indent, useTabs) ?? null;
            case "bbnf":
                return mod.format_bbnf(input, maxWidth, indent, useTabs) ?? null;
            default:
                return null;
        }
    }

    // -----------------------------------------------------------------------
    // Existing analysis features
    // -----------------------------------------------------------------------

    async function analyzeGrammar(text: string): Promise<WasmAnalysisResult | null> {
        await ensureLoaded();
        return getWasmModule().analyze_grammar(text) as WasmAnalysisResult;
    }

    async function hoverAtOffset(text: string, offset: number): Promise<WasmHoverResult | null> {
        await ensureLoaded();
        return getWasmModule().hover_at_offset(text, offset) as WasmHoverResult | null;
    }

    async function getCompletions(text: string): Promise<WasmCompletionItem[]> {
        await ensureLoaded();
        return getWasmModule().completions(text) as WasmCompletionItem[];
    }

    // -----------------------------------------------------------------------
    // New LSP features
    // -----------------------------------------------------------------------

    async function getSemanticTokens(text: string): Promise<WasmSemanticTokenDelta[]> {
        await ensureLoaded();
        return getWasmModule().semantic_tokens_full(text) as WasmSemanticTokenDelta[];
    }

    async function getInlayHints(
        text: string,
        startLine: number,
        endLine: number,
    ): Promise<WasmInlayHint[]> {
        await ensureLoaded();
        return getWasmModule().inlay_hints(text, startLine, endLine) as WasmInlayHint[];
    }

    async function gotoDefinition(
        text: string,
        offset: number,
    ): Promise<WasmDefinitionResult | null> {
        await ensureLoaded();
        return getWasmModule().goto_definition(text, offset) as WasmDefinitionResult | null;
    }

    async function getDocumentSymbols(text: string): Promise<WasmDocumentSymbol[]> {
        await ensureLoaded();
        return getWasmModule().document_symbols(text) as WasmDocumentSymbol[];
    }

    async function getFoldingRanges(text: string): Promise<WasmFoldingRange[]> {
        await ensureLoaded();
        return getWasmModule().folding_ranges(text) as WasmFoldingRange[];
    }

    async function getSelectionRanges(
        text: string,
        offsets: number[],
    ): Promise<WasmSelectionRange[]> {
        await ensureLoaded();
        return getWasmModule().selection_ranges(text, offsets) as WasmSelectionRange[];
    }

    async function getCodeActions(
        text: string,
        startOffset: number,
        endOffset: number,
    ): Promise<WasmCodeAction[]> {
        await ensureLoaded();
        return getWasmModule().code_actions(text, startOffset, endOffset) as WasmCodeAction[];
    }

    async function getCodeLens(text: string): Promise<WasmCodeLens[]> {
        await ensureLoaded();
        return getWasmModule().code_lens(text) as WasmCodeLens[];
    }

    async function findReferences(text: string, offset: number): Promise<WasmLocation[]> {
        await ensureLoaded();
        const result = getWasmModule().find_references(text, offset);
        return result ? (result as WasmLocation[]) : [];
    }

    async function prepareRename(text: string, offset: number): Promise<WasmPrepareRename | null> {
        await ensureLoaded();
        return getWasmModule().prepare_rename(text, offset) as WasmPrepareRename | null;
    }

    async function renameSymbol(text: string, offset: number, newName: string): Promise<WasmTextEdit[]> {
        await ensureLoaded();
        const result = getWasmModule().rename_symbol(text, offset, newName);
        return result ? (result as WasmTextEdit[]) : [];
    }

    async function formatDocument(text: string): Promise<WasmTextEdit[]> {
        await ensureLoaded();
        const result = getWasmModule().format_document(text);
        return result ? (result as WasmTextEdit[]) : [];
    }

    async function formatRange(text: string, startOffset: number, endOffset: number): Promise<WasmTextEdit[]> {
        await ensureLoaded();
        const result = getWasmModule().format_range(text, startOffset, endOffset);
        return result ? (result as WasmTextEdit[]) : [];
    }

    async function onTypeFormat(text: string, offset: number): Promise<WasmTextEdit[]> {
        await ensureLoaded();
        const result = getWasmModule().on_type_format(text, offset);
        return result ? (result as WasmTextEdit[]) : [];
    }

    // -----------------------------------------------------------------------
    // Bytecode VM: compile, parse, format, free
    // -----------------------------------------------------------------------

    async function compileGrammar(grammar: string, entryRule?: string): Promise<number> {
        await ensureLoaded();
        return getWasmModule().compile_grammar(grammar, entryRule ?? undefined);
    }

    async function parseWithGrammar(handle: number, input: string): Promise<WasmParseResult> {
        await ensureLoaded();
        return getWasmModule().parse_with_grammar(handle, input) as WasmParseResult;
    }

    async function parseCheck(handle: number, input: string): Promise<WasmParseCheckResult> {
        await ensureLoaded();
        return getWasmModule().parse_check(handle, input) as WasmParseCheckResult;
    }

    async function lspBatch(
        text: string,
        offset: number,
        startLine: number,
        endLine: number,
    ): Promise<WasmLspBatch> {
        await ensureLoaded();
        return getWasmModule().lsp_batch(text, offset, startLine, endLine) as WasmLspBatch;
    }

    async function formatWithGrammar(
        handle: number,
        input: string,
        maxWidth: number,
        indent: number,
        useTabs: boolean,
    ): Promise<string | null> {
        await ensureLoaded();
        return getWasmModule().format_with_grammar(handle, input, maxWidth, indent, useTabs) ?? null;
    }

    function freeGrammar(handle: number): void {
        if (!isLoaded.value) return;
        getWasmModule().free_grammar(handle);
    }

    return {
        isLoaded,
        isLoading,
        ensureLoaded,
        formatWithGorgeous,
        analyzeGrammar,
        hoverAtOffset,
        getCompletions,
        getSemanticTokens,
        getInlayHints,
        gotoDefinition,
        getDocumentSymbols,
        getFoldingRanges,
        getSelectionRanges,
        getCodeActions,
        getCodeLens,
        findReferences,
        prepareRename,
        renameSymbol,
        formatDocument,
        formatRange,
        onTypeFormat,
        compileGrammar,
        parseWithGrammar,
        parseCheck,
        formatWithGrammar,
        freeGrammar,
        lspBatch,
    };
}
