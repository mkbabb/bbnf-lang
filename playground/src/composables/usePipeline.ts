import { ref, reactive, watch, onBeforeUnmount, type Ref } from "vue";
import { useDebounceFn } from "@vueuse/core";
import { useWasm, detectBuiltinLanguage } from "./wasm";

export interface PipelineError {
    message: string;
    source: "grammar" | "parse" | "format" | "import";
    /** 1-based line number in the source (if available). */
    line?: number;
    /** 1-based column (if available). */
    column?: number;
}

export interface Telemetry {
    parseMs: number;
    formatMs: number;
    totalMs: number;
    inputBytes: number;
}

export interface PipelineResult {
    grammarText: Ref<string>;
    inputText: Ref<string>;
    /** Optional entry rule override (e.g. "stylesheet" for CSS). */
    entryRuleOverride: Ref<string>;
    /** Entry rules discovered from the last successful grammar compile. */
    availableEntryRules: Ref<string[]>;
    printerConfig: { maxWidth: number; indent: number; useTabs: boolean };
    astJson: Ref<string>;
    formatted: Ref<string>;
    errors: Ref<PipelineError[]>;
    isProcessing: Ref<boolean>;
    /** Language hint for the formatted output editor. */
    formattedLanguage: Ref<string>;
    /** Indicates which formatter produced the output. */
    formattedBy: Ref<"gorgeous" | "">;
    /** Timing telemetry for the last pipeline run. */
    telemetry: Telemetry;
    /**
     * Set to true when the AST tab is visible. When false, the pipeline uses
     * `parse_check` (no tree serialization) for ~3-4x faster validation.
     */
    needsFullTree: Ref<boolean>;
}

/**
 * Try to extract a position (line, column) from an error message.
 * Many parser errors include "at position N" or "line N col N".
 */
function extractPosition(msg: string, source: string): { line?: number; column?: number } {
    // "at position N" — convert offset to line/col
    const posMatch = msg.match(/at position (\d+)/i);
    if (posMatch) {
        const offset = parseInt(posMatch[1]!, 10);
        const lines = source.slice(0, offset).split("\n");
        return { line: lines.length, column: (lines[lines.length - 1]?.length ?? 0) + 1 };
    }
    // "line N" or "line N, col N"
    const lineMatch = msg.match(/line (\d+)/i);
    if (lineMatch) {
        const line = parseInt(lineMatch[1]!, 10);
        const colMatch = msg.match(/col(?:umn)? (\d+)/i);
        return { line, column: colMatch ? parseInt(colMatch[1]!, 10) : 1 };
    }
    return {};
}

/**
 * Convert a byte offset into 1-based line/column using the source text.
 */
function offsetToLineCol(source: string, offset: number): { line: number; column: number } {
    const lines = source.slice(0, offset).split("\n");
    return { line: lines.length, column: (lines[lines.length - 1]?.length ?? 0) + 1 };
}

export function usePipeline(): PipelineResult {
    const grammarText = ref("");
    const inputText = ref("");
    const entryRuleOverride = ref("");
    const availableEntryRules = ref<string[]>([]);
    const printerConfig = reactive({ maxWidth: 80, indent: 2, useTabs: false });

    const astJson = ref("");
    const formatted = ref("");
    const errors = ref<PipelineError[]>([]);
    const isProcessing = ref(false);
    const formattedLanguage = ref("plaintext");
    const formattedBy = ref<"gorgeous" | "">("");
    const telemetry = reactive<Telemetry>({ parseMs: 0, formatMs: 0, totalMs: 0, inputBytes: 0 });
    const needsFullTree = ref(false);

    const wasm = useWasm();

    // Cache compiled grammar handle to avoid recompilation on input-only changes.
    let cachedGrammarText = "";
    let cachedGrammarHandle: number | null = null;
    let cachedEntryRule = "";

    async function runPipeline() {
        isProcessing.value = true;
        errors.value = [];

        try {
            // Step 1: Compile grammar (only if grammar text changed)
            if (grammarText.value !== cachedGrammarText) {
                // Free previous handle
                if (cachedGrammarHandle != null) {
                    wasm.freeGrammar(cachedGrammarHandle);
                    cachedGrammarHandle = null;
                }
                cachedGrammarText = grammarText.value;
                cachedEntryRule = "";

                if (!grammarText.value.trim()) {
                    availableEntryRules.value = [];
                    astJson.value = "";
                    formatted.value = "";
                    formattedBy.value = "";
                    return;
                }

                try {
                    // Analyze grammar first to discover entry rules
                    const analysis = await wasm.analyzeGrammar(grammarText.value);
                    if (analysis && analysis.rules.length > 0) {
                        const ruleNames = analysis.rules.map((r) => r.name);
                        availableEntryRules.value = ruleNames;

                        // Use explicit override if set, otherwise default to last rule
                        // (BBNF convention: last rule in source order is the entry point)
                        const lastRule = ruleNames[ruleNames.length - 1] ?? "";
                        cachedEntryRule =
                            entryRuleOverride.value && ruleNames.includes(entryRuleOverride.value)
                                ? entryRuleOverride.value
                                : lastRule;

                        // Heuristic: detect output language for syntax highlighting
                        formattedLanguage.value = detectLanguage(cachedEntryRule, ruleNames);
                    } else {
                        availableEntryRules.value = [];
                        cachedEntryRule = "";
                    }

                    // Compile grammar via WASM bytecode VM, passing the resolved entry rule
                    cachedGrammarHandle = await wasm.compileGrammar(grammarText.value, cachedEntryRule || undefined);
                } catch (e: any) {
                    const msg = e.message ?? String(e);
                    const pos = extractPosition(msg, grammarText.value);
                    availableEntryRules.value = [];
                    errors.value.push({ message: msg, source: "grammar", ...pos });
                    astJson.value = "";
                    formatted.value = "";
                    formattedBy.value = "";
                    return;
                }
            } else if (entryRuleOverride.value) {
                // Grammar unchanged but entry rule override changed — update cachedEntryRule
                if (availableEntryRules.value.includes(entryRuleOverride.value)) {
                    cachedEntryRule = entryRuleOverride.value;
                }
            }

            if (cachedGrammarHandle == null || !cachedEntryRule) {
                astJson.value = "";
                formatted.value = "";
                formattedBy.value = "";
                return;
            }

            if (!inputText.value.trim()) {
                astJson.value = "";
                formatted.value = "";
                formattedBy.value = "";
                return;
            }

            // Step 2: Parse input text with the compiled grammar via WASM
            //
            // Fast path: when the AST tab is not visible, use parse_check (no tree
            // serialization, ~3-4x faster). Full parse only when AST tab is active.
            try {
                const t0 = performance.now();

                if (!needsFullTree.value) {
                    // Fast path: validate only, skip tree serialization
                    const checkResult = await wasm.parseCheck(cachedGrammarHandle, inputText.value);
                    const t1 = performance.now();

                    if (!checkResult.success) {
                        const pos = offsetToLineCol(inputText.value, checkResult.offset);
                        errors.value.push({ message: "Input does not match grammar", source: "parse", ...pos });
                        astJson.value = "";
                        formatted.value = "";
                        formattedBy.value = "";
                        return;
                    }

                    // No AST to show in fast path
                    astJson.value = "";

                    // Still format output
                    try {
                        let fmtResult: string | null = null;

                        const builtinLang = detectBuiltinLanguage(cachedEntryRule);
                        if (builtinLang) {
                            fmtResult = await wasm.formatWithGorgeous(
                                builtinLang,
                                inputText.value,
                                printerConfig.maxWidth,
                                printerConfig.indent,
                                printerConfig.useTabs,
                            );
                        }

                        if (fmtResult == null && cachedGrammarHandle != null) {
                            fmtResult = await wasm.formatWithGrammar(
                                cachedGrammarHandle,
                                inputText.value,
                                printerConfig.maxWidth,
                                printerConfig.indent,
                                printerConfig.useTabs,
                            );
                        }

                        if (fmtResult != null) {
                            formatted.value = fmtResult;
                            formattedBy.value = "gorgeous";
                        } else {
                            formatted.value = "(no @pretty directives in grammar)";
                            formattedBy.value = "";
                        }
                    } catch (e: any) {
                        const msg = e.message ?? String(e);
                        errors.value.push({ message: msg, source: "format" });
                        formatted.value = "";
                        formattedBy.value = "";
                    }

                    const t2 = performance.now();
                    telemetry.parseMs = +(t1 - t0).toFixed(1);
                    telemetry.formatMs = +(t2 - t1).toFixed(1);
                    telemetry.totalMs = +(t2 - t0).toFixed(1);
                    telemetry.inputBytes = new TextEncoder().encode(inputText.value).byteLength;
                } else {
                    // Full path: parse with tree serialization for AST tab
                    const result = await wasm.parseWithGrammar(cachedGrammarHandle, inputText.value);
                    const t1 = performance.now();

                    if (!result.success) {
                        const diagMessages = result.diagnostics
                            .map((d) => {
                                const prefix = d.rule_name ? `[${d.rule_name}] ` : "";
                                return `${prefix}${d.expected}`;
                            })
                            .join("\n");

                        const msg = diagMessages || "Input does not match grammar";
                        const pos = offsetToLineCol(inputText.value, result.offset);
                        errors.value.push({ message: msg, source: "parse", ...pos });
                        astJson.value = "";
                        formatted.value = "";
                        formattedBy.value = "";
                        return;
                    }

                    astJson.value = JSON.stringify(result.value, null, 2);

                    // Step 3: Format output via WASM
                    try {
                        let fmtResult: string | null = null;

                        const builtinLang = detectBuiltinLanguage(cachedEntryRule);
                        if (builtinLang) {
                            fmtResult = await wasm.formatWithGorgeous(
                                builtinLang,
                                inputText.value,
                                printerConfig.maxWidth,
                                printerConfig.indent,
                                printerConfig.useTabs,
                            );
                        }

                        if (fmtResult == null && cachedGrammarHandle != null) {
                            fmtResult = await wasm.formatWithGrammar(
                                cachedGrammarHandle,
                                inputText.value,
                                printerConfig.maxWidth,
                                printerConfig.indent,
                                printerConfig.useTabs,
                            );
                        }

                        if (fmtResult != null) {
                            formatted.value = fmtResult;
                            formattedBy.value = "gorgeous";
                        } else {
                            formatted.value = "(no @pretty directives in grammar)";
                            formattedBy.value = "";
                        }
                    } catch (e: any) {
                        const msg = e.message ?? String(e);
                        errors.value.push({ message: msg, source: "format" });
                        formatted.value = "";
                        formattedBy.value = "";
                    }

                    const t2 = performance.now();
                    telemetry.parseMs = +(t1 - t0).toFixed(1);
                    telemetry.formatMs = +(t2 - t1).toFixed(1);
                    telemetry.totalMs = +(t2 - t0).toFixed(1);
                    telemetry.inputBytes = new TextEncoder().encode(inputText.value).byteLength;
                }
            } catch (e: any) {
                const msg = e.message ?? String(e);
                const pos = extractPosition(msg, inputText.value);
                errors.value.push({ message: msg, source: "parse", ...pos });
                astJson.value = "";
                formatted.value = "";
                formattedBy.value = "";
            }
        } catch (e: any) {
            errors.value.push({ message: e.message ?? String(e), source: "import" });
        } finally {
            isProcessing.value = false;
        }
    }

    // Free cached grammar handle on unmount to prevent leaks
    onBeforeUnmount(() => {
        if (cachedGrammarHandle != null) {
            wasm.freeGrammar(cachedGrammarHandle);
            cachedGrammarHandle = null;
        }
    });

    const debouncedRun = useDebounceFn(runPipeline, 300);

    watch(
        [
            grammarText,
            inputText,
            entryRuleOverride,
            needsFullTree,
            () => printerConfig.maxWidth,
            () => printerConfig.indent,
            () => printerConfig.useTabs,
        ],
        () => {
            debouncedRun();
        },
    );

    return {
        grammarText,
        inputText,
        entryRuleOverride,
        availableEntryRules,
        printerConfig,
        astJson,
        formatted,
        errors,
        isProcessing,
        formattedLanguage,
        formattedBy,
        telemetry,
        needsFullTree,
    };
}

/** Heuristic: detect the output language for syntax highlighting. */
function detectLanguage(entryRule: string, ruleNames: string[]): string {
    const lower = entryRule.toLowerCase();
    if (lower === "value" || lower === "json" || lower === "object") {
        if (ruleNames.some((r) => r === "object") && ruleNames.some((r) => r === "array") && ruleNames.some((r) => r === "string")) {
            return "json";
        }
    }
    if (lower === "stylesheet" || lower === "rule" || lower.includes("css")) return "css";
    if (lower === "program" || lower === "statement") return "javascript";
    if (lower === "grammar" || lower === "rule_def" || lower.includes("bbnf")) {
        if (ruleNames.some((r) => r === "identifier") && (ruleNames.some((r) => r === "expression") || ruleNames.some((r) => r === "alternation"))) {
            return "bbnf";
        }
    }
    return "plaintext";
}
