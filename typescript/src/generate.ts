/* eslint-disable @typescript-eslint/no-explicit-any */
import {
    Parser,
    all,
    any,
    eof,
    regex,
    memoize,
    mergeMemos,
    string,
    dispatch,
    mergeErrorState,
    createParserContext,
} from "@mkbabb/parse-that";
import type { ParserState } from "@mkbabb/parse-that";
import type { Expression, Nonterminals, AST, RecoverDirective } from "./types.js";
import { removeAllLeftRecursion } from "./optimize.js";
import { analyzeGrammar, analyzeFirst, routes, dedupGroups } from "./analysis/index.js";
import type { AnalysisCache, FirstAnalysis } from "./analysis/index.js";
import { BBNFToAST, BBNFToASTWithImports } from "./parse.js";
import { loadModuleGraphSync, mergeModuleAST, mergeModuleRecovers } from "./imports.js";
import { resolve as resolveModuleId } from "./posix-path.js";

function escapeRegex(s: string): string {
    return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

/**
 * Find SCC entry points: cyclic rules referenced from outside their SCC.
 * Only these rules need full memoization — other cyclic rules in the same
 * SCC are reached through the entry point which already caches.
 */
function findSccEntryPoints(cache: AnalysisCache): Set<string> {
    const { depGraph, sccIndex, cyclicRules, sccs } = cache;
    const entryPoints = new Set<string>();

    // A cyclic rule is an entry point if any rule outside its SCC references it.
    for (const [src, deps] of depGraph) {
        const srcScc = sccIndex.get(src);
        for (const dep of deps) {
            if (!cyclicRules.has(dep)) continue;
            const depScc = sccIndex.get(dep);
            if (srcScc !== depScc) {
                entryPoints.add(dep);
            }
        }
    }

    // Ensure every cyclic SCC has at least one entry point.
    for (const scc of sccs) {
        if (scc.length <= 1) {
            // Single-member SCC: if cyclic (self-referencing), it's its own entry point.
            const name = scc[0];
            if (cyclicRules.has(name) && !entryPoints.has(name)) {
                entryPoints.add(name);
            }
            continue;
        }
        const hasEntry = scc.some((name) => entryPoints.has(name));
        if (!hasEntry) {
            entryPoints.add(scc[0]);
        }
    }

    return entryPoints;
}

export function ASTToParser(
    ast: AST,
    analysis?: AnalysisCache,
    first?: FirstAnalysis,
    recovers?: RecoverDirective[],
    tagAlternations = false,
    enableMemoization = false,
) {
    // Compute analysis if not provided
    const cache = analysis ?? analyzeGrammar(ast);
    const { cyclicRules, topoOrder } = cache;
    const firstFacts = first ?? analyzeFirst(ast);

    const nonterminals: Nonterminals = {};

    /**
     * Resolve a nonterminal to its terminal expression, following alias chains.
     * Returns the resolved expression, or null if it references a cyclic or
     * externally-overridden rule.
     */
    function resolveToTerminal(expr: Expression): Expression | null {
        if (!expr?.type) return null;
        if (expr.type === "literal" || expr.type === "regex") return expr;
        if (expr.type === "group") return resolveToTerminal(expr.value as Expression);
        if (expr.type === "nonterminal") {
            const rule = ast.get(expr.value as string);
            if (rule && !cyclicRules.has(expr.value as string)) {
                return resolveToTerminal(rule.expression);
            }
        }
        return null;
    }

    /**
     * Detect `literal >> regex/char* << literal` and compile to a single regex.
     * Handles both AST shapes:
     *   Shape A: next(literal_L, skip(many(charPattern), literal_R))
     *   Shape B: skip(next(literal_L, many(charPattern)), literal_R)
     */
    function tryWrapRegexCoalesce(expr: Expression): Parser<any> | null {
        let leftStr: string | null = null;
        let rightStr: string | null = null;
        let innerExpr: Expression | null = null;
        let quantifier: string | null = null;

        // Shape A: next(literal_L, skip(many(charPattern), literal_R))
        if (expr.type === "next") {
            const [left, right] = expr.value as [Expression, Expression];
            const resolvedLeft = resolveToTerminal(left);
            if (resolvedLeft?.type === "literal" && right.type === "skip") {
                const [middle, end] = right.value as [Expression, Expression];
                const resolvedEnd = resolveToTerminal(end);
                if (resolvedEnd?.type === "literal") {
                    leftStr = resolvedLeft.value as string;
                    rightStr = resolvedEnd.value as string;
                    if (middle.type === "many") {
                        innerExpr = middle.value as Expression;
                        quantifier = "*";
                    } else if (middle.type === "many1") {
                        innerExpr = middle.value as Expression;
                        quantifier = "+";
                    }
                }
            }
        }

        // Shape B: skip(next(literal_L, many(charPattern)), literal_R)
        if (expr.type === "skip" && !leftStr) {
            const [left, right] = expr.value as [Expression, Expression];
            const resolvedRight = resolveToTerminal(right);
            if (resolvedRight?.type === "literal" && left.type === "next") {
                const [start, middle] = left.value as [Expression, Expression];
                const resolvedStart = resolveToTerminal(start);
                if (resolvedStart?.type === "literal") {
                    leftStr = resolvedStart.value as string;
                    rightStr = resolvedRight.value as string;
                    if (middle.type === "many") {
                        innerExpr = middle.value as Expression;
                        quantifier = "*";
                    } else if (middle.type === "many1") {
                        innerExpr = middle.value as Expression;
                        quantifier = "+";
                    }
                }
            }
        }

        if (!leftStr || !rightStr || !innerExpr || !quantifier) return null;

        // Inner must resolve to a regex
        const resolved = resolveToTerminal(innerExpr);
        if (!resolved || resolved.type !== "regex") return null;

        const re = resolved.value as RegExp;
        const escapedLeft = escapeRegex(leftStr);
        const escapedRight = escapeRegex(rightStr);
        const combinedSource = `${escapedLeft}(${re.source})${quantifier}${escapedRight}`;
        try {
            return regex(new RegExp(combinedSource));
        } catch {
            return null;
        }
    }

    /**
     * Detect `(item << sep?)* ` and compile to `item.sepBy(sep)`.
     */
    function trySepByDetect(name: string, expr: Expression): Parser<any> | null {
        if (expr.type !== "many" && expr.type !== "many1") return null;

        const inner = expr.value as Expression;
        let unwrapped = inner;
        if (unwrapped.type === "group") unwrapped = unwrapped.value as Expression;

        if (unwrapped.type !== "skip") return null;
        const [item, sepOpt] = unwrapped.value as [Expression, Expression];

        if (sepOpt.type !== "optional") return null;
        const sep = sepOpt.value as Expression;

        const itemParser = generateParser(name, item);
        const sepParser = generateParser(name, sep);

        if (expr.type === "many") {
            return itemParser.sepBy(sepParser);
        } else {
            return itemParser.sepBy(sepParser, 1);
        }
    }

    /**
     * Phase 3.1: Detect `left >> middle << right` and compile to middle.wrap(left, right).
     * wrap() already inlines 2 function frames (index.ts), so this saves overhead.
     */
    function tryWrapDetect(name: string, expr: Expression): Parser<any> | null {
        // Unwrap Group nodes before pattern matching (same as Rust check_for_wrapped).
        if (expr.type === "group") {
            return tryWrapDetect(name, expr.value as Expression);
        }
        // Shape: skip(next(L, M), R) → M.wrap(L, R)
        if (expr.type === "skip") {
            const [left, right] = expr.value as [Expression, Expression];
            // Unwrap Group on the left side too.
            let unwrappedLeft = left;
            while (unwrappedLeft.type === "group") unwrappedLeft = unwrappedLeft.value as Expression;
            if (unwrappedLeft.type === "next") {
                const [l, m] = unwrappedLeft.value as [Expression, Expression];
                return generateParser(name, m).wrap(
                    generateParser(name, l),
                    generateParser(name, right),
                );
            }
        }
        return null;
    }

    /**
     * Phase 3.2: When all alternatives are string literals, compile to
     * dispatch() with char-based routing for O(1) lookup.
     */
    function tryAllLiteralsAlternation(name: string, alts: Expression[]): Parser<any> | null {
        if (alts.length < 2) return null;
        if (!alts.every((a) => {
            const resolved = resolveToTerminal(a);
            return resolved?.type === "literal";
        })) return null;

        const table: Record<string, Parser<any>> = {};
        const fallbackAlts: Parser<any>[] = [];

        for (const alt of alts) {
            const resolved = resolveToTerminal(alt)!;
            const lit = resolved.value as string;
            if (lit.length === 0) {
                // Empty literal can't be dispatched
                return null;
            }
            const firstChar = lit[0];
            if (table[firstChar]) {
                // Collision on first character — fall back to any()
                // Could group them, but for now just bail
                return null;
            }
            table[firstChar] = string(lit);
        }

        return dispatch(table);
    }

    function generateParser(name: string, expr: Expression): Parser<any> {
        // Try pattern recognition first
        const wrapResult = tryWrapRegexCoalesce(expr);
        if (wrapResult) return wrapResult;

        const wrapDetectResult = tryWrapDetect(name, expr);
        if (wrapDetectResult) return wrapDetectResult;

        const sepByResult = trySepByDetect(name, expr);
        if (sepByResult) return sepByResult;

        switch (expr.type) {
            case "literal":
                return string(expr.value as string);
            case "nonterminal": {
                const refName = expr.value as string;
                // Always use lazy — users may override nonterminals after
                // generation (e.g. nonterminals.S = regex(/\s*/)).
                const l = Parser.lazy(() => nonterminals[refName]);
                l.context.name = refName as any;
                return l;
            }

            case "epsilon":
                return eof().opt();

            case "group":
                return generateParser(name, expr.value as Expression);

            case "regex":
                return regex(expr.value as RegExp);

            case "optionalWhitespace":
                return generateParser(name, expr.value as any).trim();

            case "optional":
                return generateParser(name, expr.value as Expression).opt();
            case "many":
                return generateParser(name, expr.value as Expression).many();
            case "many1":
                return generateParser(name, expr.value as Expression).many(1);
            case "skip":
                return generateParser(
                    name,
                    (expr.value as [Expression, Expression])[0],
                ).skip(
                    generateParser(
                        name,
                        (expr.value as [Expression, Expression])[1],
                    ),
                );
            case "next":
                return generateParser(
                    name,
                    (expr.value as [Expression, Expression])[0],
                ).next(
                    generateParser(
                        name,
                        (expr.value as [Expression, Expression])[1],
                    ),
                );
            case "minus":
                return generateParser(
                    name,
                    (expr.value as [Expression, Expression])[0],
                ).minus(
                    generateParser(
                        name,
                        (expr.value as [Expression, Expression])[1],
                    ),
                );
            case "concatenation": {
                const parsers = (expr.value as Expression[]).map((x) =>
                    generateParser(name, x),
                );
                if (parsers.at(-1)?.context?.name === "eof") {
                    parsers.pop();
                }
                // Specialize: 2-element concatenation avoids the loop in all()
                // Must preserve all()'s undefined-skipping semantics.
                if (parsers.length === 2) {
                    const [p1, p2] = parsers;
                    const all2 = (state: ParserState<any>) => {
                        const savedOffset = state.offset;
                        p1.parser(state);
                        if (state.isError) {
                            state.offset = savedOffset;
                            return state;
                        }
                        const v1 = state.value;
                        p2.parser(state);
                        if (state.isError) {
                            state.offset = savedOffset;
                            state.isError = true;
                            return state;
                        }
                        const v2 = state.value;
                        if (v1 !== undefined) {
                            return v2 !== undefined
                                ? state.ok([v1, v2])
                                : state.ok([v1]);
                        }
                        return v2 !== undefined
                            ? state.ok([v2])
                            : state.ok([]);
                    };
                    return new Parser(
                        all2,
                        createParserContext("all", undefined, p1, p2),
                    );
                }
                return all(...parsers);
            }
            case "alternation": {
                const alts = expr.value as Expression[];

                // Phase 3.2: all-literals → dispatch table
                const litDispatch = tryAllLiteralsAlternation(name, alts);
                if (litDispatch) return litDispatch;

                let parsers = alts.map((x) => generateParser(name, x));

                if (tagAlternations) {
                    parsers = parsers.map((p, i) => p.map((v: any) => ({ _branch: i, value: v })));
                }

                // Route the ordered choice by the unit at the cursor (the one analysis, sound
                // on regex flags, escapes, lookarounds, non-ASCII units and end of input): each
                // route is the ordered sub-choice of the alternatives that can start there.
                const route = parsers.length >= 2 ? routes(alts.map((a) => firstFacts.info(a))) : null;
                if (route) {
                    const { tbl, na, eof: atEof } = route;
                    const groupParsers = route.groups.map((members) =>
                        members.length === 1 ? parsers[members[0]] : any(...members.map((i) => parsers[i])),
                    );
                    const dispatchParser = (state: ParserState<any>) => {
                        const src = state.src;
                        const offset = state.offset;
                        let g: number;
                        if (offset >= src.length) g = atEof;
                        else {
                            const c = src.charCodeAt(offset);
                            g = c < 128 ? tbl[c] : na;
                        }
                        if (g >= 0) return groupParsers[g].parser(state);
                        mergeErrorState(state as ParserState<unknown>);
                        return state.err(undefined);
                    };
                    return new Parser(
                        dispatchParser,
                        createParserContext("dispatch", undefined, ...parsers),
                    );
                }

                return any(...parsers);
            }
        }
    }

    // Compute SCC entry points and reference counts for memoization strategy.
    const sccEntryPoints = enableMemoization ? findSccEntryPoints(cache) : new Set<string>();
    const { refCounts } = cache;
    const SELECTIVE_THRESHOLD = 3;

    // Build rules in topological order (leaves first, from Tarjan's SCC).
    for (const name of topoOrder) {
        const rule = ast.get(name);
        if (!rule) continue;
        let parser = generateParser(name, rule.expression);

        // SCC-selective memoization (opt-in): entry points get full memo,
        // high-ref acyclic rules get lightweight mergeMemos.
        if (enableMemoization) {
            if (sccEntryPoints.has(name)) {
                parser = memoize(parser);
            } else if (
                !cyclicRules.has(name) &&
                (refCounts.get(name) ?? 0) > SELECTIVE_THRESHOLD
            ) {
                parser = mergeMemos(parser);
            }
        }

        nonterminals[name] = parser;
    }

    // Build any rules not in topoOrder
    for (const [name, rule] of ast) {
        if (!nonterminals[name]) {
            nonterminals[name] = generateParser(name, rule.expression);
        }
    }

    // Collapse alias chains: if a rule is just a nonterminal reference
    // (e.g. charger = chargerge; charge = charger; char = charge),
    // replace with the resolved parser to eliminate indirection.
    for (const [name, rule] of ast) {
        if (cyclicRules.has(name)) continue;
        let expr = rule.expression;
        // Unwrap groups
        while (expr.type === "group") expr = expr.value as Expression;
        if (expr.type === "nonterminal") {
            const target = expr.value as string;
            if (nonterminals[target]) {
                nonterminals[name] = nonterminals[target];
            }
        }
    }

    // Apply @recover wrapping: wrap annotated rules with .recover(syncParser, null).
    if (recovers && recovers.length > 0) {
        for (const recover of recovers) {
            const original = nonterminals[recover.ruleName];
            if (original) {
                const syncParser = generateParser(recover.ruleName + "$sync", recover.syncExpr);
                nonterminals[recover.ruleName] = original.recover(syncParser, null);
            }
        }
    }

    return nonterminals;
}

export function BBNFToParser(
    input: string,
    optimizeGraph: boolean = false,
    tagAlternations: boolean = false,
) {
    // Try import-aware parsing first to pick up @recover directives
    const importResult = BBNFToASTWithImports(input);
    let ast: AST;
    let recovers: RecoverDirective[] = [];

    if (importResult.length >= 2 && importResult[1]) {
        ast = importResult[1].rules;
        recovers = importResult[1].recovers ?? [];
    } else {
        const [, plainAst] = BBNFToAST(input);
        if (!plainAst) {
            throw new Error("Failed to parse BBNF grammar");
        }
        ast = plainAst;
    }

    dedupGroups(ast);

    const analysis = analyzeGrammar(ast);
    const finalAst = optimizeGraph ? removeAllLeftRecursion(ast, analysis) : ast;

    // Re-analyze if left recursion changed the AST
    const finalAnalysis = finalAst !== ast ? analyzeGrammar(finalAst) : analysis;
    const nonterminals = ASTToParser(finalAst, finalAnalysis, analyzeFirst(finalAst), recovers, tagAlternations, optimizeGraph);
    return [nonterminals, finalAst] as const;
}

/**
 * Parse a BBNF grammar from a file path, resolving `@import` directives.
 *
 * This is the preferred entry point for grammars that use imports.
 * It loads the full module graph, merges imported rules, then compiles
 * to executable parsers.
 *
 * @param entryPath - Path to the main `.bbnf` file.
 * @param readFileSync - The host's reader: module ID → text (a node host passes
 *   `(p) => fs.readFileSync(p, "utf8")`; no filesystem is assumed).
 * @param optimizeGraph - If true, apply left-recursion elimination.
 * @returns [nonterminals, ast] — the compiled parser map and final AST.
 */
export function BBNFToParserFromFile(
    entryPath: string,
    readFileSync: (path: string) => string,
    optimizeGraph: boolean = false,
    tagAlternations: boolean = false,
) {
    const entry = resolveModuleId(entryPath);
    const registry = loadModuleGraphSync(entry, readFileSync);

    if (registry.errors.length > 0) {
        const errorMessages = registry.errors
            .map((e) => {
                switch (e.type) {
                    case "FileNotFound":
                        return `File not found: ${e.path} (imported from ${e.importedFrom})`;
                    case "CircularImport":
                        return `Circular import: ${e.path} (chain: ${e.chain.join(" → ")})`;
                    case "MissingRule":
                        return `Rule '${e.ruleName}' not found in ${e.path}`;
                    case "NameConflict":
                        return `Name conflict: '${e.ruleName}' from both ${e.sourceA} and ${e.sourceB}`;
                    case "ParseError":
                        return `Parse error in ${e.path}: ${e.message}`;
                }
            })
            .join("\n");
        throw new Error(`Import resolution errors:\n${errorMessages}`);
    }

    const ast = mergeModuleAST(registry, entry);
    const recovers = mergeModuleRecovers(registry, entry);
    dedupGroups(ast);

    const analysis = analyzeGrammar(ast);
    const finalAst = optimizeGraph ? removeAllLeftRecursion(ast, analysis) : ast;

    const finalAnalysis = finalAst !== ast ? analyzeGrammar(finalAst) : analysis;
    const nonterminals = ASTToParser(finalAst, finalAnalysis, analyzeFirst(finalAst), recovers, tagAlternations, optimizeGraph);
    return [nonterminals, finalAst] as const;
}
