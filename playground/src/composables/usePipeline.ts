import { ref, reactive, watch, computed, type Ref } from "vue";
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

export interface PipelineResult {
    grammarText: Ref<string>;
    inputText: Ref<string>;
    /** Optional entry rule override (e.g. "stylesheet" for CSS). */
    entryRuleOverride: Ref<string>;
    printerConfig: { maxWidth: number; indent: number; useTabs: boolean };
    astJson: Ref<string>;
    formatted: Ref<string>;
    errors: Ref<PipelineError[]>;
    isProcessing: Ref<boolean>;
    /** Language hint for the formatted output editor. */
    formattedLanguage: Ref<string>;
    /** Indicates which formatter produced the output. */
    formattedBy: Ref<"interpreter" | "gorgeous" | "">;
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
 * Recursively unwrap `{ _branch: N, value: V }` wrappers from tagged alternations
 * so the AST JSON view shows clean values to the user.
 */
function stripBranchTags(val: unknown): unknown {
    if (val === null || val === undefined) return val;
    if (Array.isArray(val)) return val.map(stripBranchTags);
    if (typeof val === "object") {
        const obj = val as Record<string, unknown>;
        if ("_branch" in obj && "value" in obj && Object.keys(obj).length === 2) {
            return stripBranchTags(obj.value);
        }
        const out: Record<string, unknown> = {};
        for (const [k, v] of Object.entries(obj)) {
            out[k] = stripBranchTags(v);
        }
        return out;
    }
    return val;
}

export function usePipeline(): PipelineResult {
    const grammarText = ref("");
    const inputText = ref("");
    const entryRuleOverride = ref("");
    const printerConfig = reactive({ maxWidth: 80, indent: 2, useTabs: false });

    const astJson = ref("");
    const formatted = ref("");
    const errors = ref<PipelineError[]>([]);
    const isProcessing = ref(false);
    const formattedLanguage = ref("plaintext");
    const formattedBy = ref<"interpreter" | "gorgeous" | "">("");

    const wasm = useWasm();

    // Cache compiled grammar to avoid recompilation on input-only changes
    let cachedGrammarText = "";
    let cachedEntryOverride = "";
    let cachedNonterminals: Record<string, any> | null = null;
    let cachedAST: Map<string, any> | null = null;
    let cachedPretties: any[] = [];
    let cachedEntryRule = "";

    async function runPipeline() {
        isProcessing.value = true;
        errors.value = [];

        try {
            const { BBNFToASTWithImports, ASTToParser, analyzeGrammar, computeFirstSets, dedupGroups } =
                await import("@mkbabb/bbnf-lang");
            const { prettify } = await import("@mkbabb/bbnf-lang");

            // Step 1: Compile grammar (only if grammar text or entry rule changed)
            if (grammarText.value !== cachedGrammarText || entryRuleOverride.value !== cachedEntryOverride) {
                cachedGrammarText = grammarText.value;
                cachedEntryOverride = entryRuleOverride.value;
                cachedNonterminals = null;
                cachedAST = null;
                cachedPretties = [];
                cachedEntryRule = "";

                if (!grammarText.value.trim()) {
                    astJson.value = "";
                    formatted.value = "";
                    return;
                }

                try {
                    const result = BBNFToASTWithImports(grammarText.value);

                    if (result.length < 2 || !result[1]) {
                        errors.value.push({ message: "Failed to parse grammar", source: "grammar" });
                        astJson.value = "";
                        formatted.value = "";
                        return;
                    }

                    const parsed = result[1];
                    const ast = parsed.rules;
                    cachedPretties = parsed.pretties ?? [];

                    dedupGroups(ast);
                    const analysis = analyzeGrammar(ast);
                    const firstNullable = computeFirstSets(ast, analysis);
                    const nonterminals = ASTToParser(
                        ast,
                        analysis,
                        firstNullable,
                        parsed.recovers ?? [],
                        true, // tagAlternations for prettify
                    );

                    // Only auto-trim if the grammar doesn't handle whitespace itself.
                    // Grammars using ?w (optionalWhitespace) have explicit whitespace
                    // handling — global trim would double-skip and break parsing.
                    if (!hasExplicitWhitespace(ast)) {
                        for (const key of Object.keys(nonterminals)) {
                            nonterminals[key] = nonterminals[key].trim();
                        }
                    }

                    cachedNonterminals = nonterminals;
                    cachedAST = ast;

                    // Use explicit override if set, otherwise default to first rule
                    const firstKey = ast.keys().next().value;
                    cachedEntryRule = (entryRuleOverride.value && ast.has(entryRuleOverride.value))
                        ? entryRuleOverride.value
                        : (firstKey ?? "");

                    // Heuristic: detect output language for syntax highlighting
                    formattedLanguage.value = detectLanguage(cachedEntryRule, ast);
                } catch (e: any) {
                    const msg = e.message ?? String(e);
                    const pos = extractPosition(msg, grammarText.value);
                    errors.value.push({ message: msg, source: "grammar", ...pos });
                    astJson.value = "";
                    formatted.value = "";
                    return;
                }
            }

            if (!cachedNonterminals || !cachedEntryRule || !cachedAST) {
                astJson.value = "";
                formatted.value = "";
                return;
            }

            if (!inputText.value.trim()) {
                astJson.value = "";
                formatted.value = "";
                return;
            }

            // Step 2: Parse input text with the compiled grammar
            const entryParser = cachedNonterminals[cachedEntryRule];
            if (!entryParser) {
                errors.value.push({ message: `No parser for entry rule: "${cachedEntryRule}"`, source: "parse" });
                return;
            }

            try {
                const result = entryParser.parse(inputText.value);
                if (result === undefined || result === null) {
                    errors.value.push({ message: "Input does not match grammar", source: "parse" });
                    astJson.value = "";
                    formatted.value = "";
                    return;
                }

                astJson.value = JSON.stringify(stripBranchTags(result), null, 2);

                // Step 3: Format output
                // Try WASM gorgeous first for built-in languages (perfect output).
                const builtinLang = detectBuiltinLanguage(cachedEntryRule);
                let wasmFormatted = false;

                if (builtinLang) {
                    try {
                        const wasmOutput = await wasm.formatWithGorgeous(
                            builtinLang,
                            inputText.value,
                            printerConfig.maxWidth,
                            printerConfig.indent,
                            printerConfig.useTabs,
                        );
                        if (wasmOutput != null) {
                            formatted.value = wasmOutput;
                            formattedBy.value = "gorgeous";
                            wasmFormatted = true;
                        }
                    } catch {
                        // WASM failed — fall through to TS interpreter
                    }
                }

                // Fall back to TS prettify interpreter for custom grammars or WASM failures.
                if (!wasmFormatted) {
                    if (cachedPretties.length > 0) {
                        try {
                            const output = prettify(result, cachedEntryRule, cachedAST, cachedPretties, {
                                maxWidth: printerConfig.maxWidth,
                                indent: printerConfig.indent,
                                useTabs: printerConfig.useTabs,
                            });
                            formatted.value = output;
                            formattedBy.value = "interpreter";
                        } catch (e: any) {
                            const msg = e.message ?? String(e);
                            errors.value.push({ message: msg, source: "format" });
                            formatted.value = "";
                        }
                    } else {
                        formatted.value = "(no @pretty directives in grammar)";
                        formattedBy.value = "";
                    }
                }
            } catch (e: any) {
                const msg = e.message ?? String(e);
                const pos = extractPosition(msg, inputText.value);
                errors.value.push({ message: msg, source: "parse", ...pos });
                astJson.value = "";
                formatted.value = "";
            }
        } catch (e: any) {
            errors.value.push({ message: e.message ?? String(e), source: "import" });
        } finally {
            isProcessing.value = false;
        }
    }

    const debouncedRun = useDebounceFn(runPipeline, 300);

    watch(
        [
            grammarText,
            inputText,
            entryRuleOverride,
            () => printerConfig.maxWidth,
            () => printerConfig.indent,
            () => printerConfig.useTabs,
        ],
        () => {
            debouncedRun();
        },
    );

    return { grammarText, inputText, entryRuleOverride, printerConfig, astJson, formatted, errors, isProcessing, formattedLanguage, formattedBy };
}

/**
 * Check if any expression in the grammar AST uses the `?w` (optionalWhitespace)
 * operator, indicating the grammar handles whitespace explicitly.
 */
function hasExplicitWhitespace(ast: Map<string, any>): boolean {
    function walk(expr: any): boolean {
        if (!expr?.type) return false;
        if (expr.type === "optionalWhitespace") return true;
        if (Array.isArray(expr.value)) {
            return expr.value.some(walk);
        }
        if (expr.value && typeof expr.value === "object" && "type" in expr.value) {
            return walk(expr.value);
        }
        return false;
    }
    for (const rule of ast.values()) {
        if (walk(rule.expression)) return true;
    }
    return false;
}

/** Heuristic: detect the output language for syntax highlighting. */
function detectLanguage(entryRule: string, ast: Map<string, any>): string {
    const lower = entryRule.toLowerCase();
    if (lower === "value" || lower === "json" || lower === "object") {
        // Check if grammar has JSON-like rules
        if (ast.has("object") && ast.has("array") && ast.has("string")) return "json";
    }
    if (lower === "stylesheet" || lower === "rule" || lower.includes("css")) return "css";
    if (lower === "program" || lower === "statement") return "javascript";
    return "plaintext";
}
