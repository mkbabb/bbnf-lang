/* eslint-disable @typescript-eslint/no-explicit-any */
import type { ProductionRule, AST, ParsedGrammar } from "./types.js";
import { BBNFGrammar } from "./grammar.js";

export function BBNFToAST(input: string) {
    const parser = new BBNFGrammar().grammar().eof();
    const parsed = parser.parse(input);

    if (!parsed) {
        return [parser] as const;
    }

    const ast = (parsed as ProductionRule[]).reduce(
        (acc, productionRule) => {
            return acc.set(productionRule.name.value, productionRule);
        },
        new Map<string, ProductionRule>(),
    ) as AST;

    return [parser, ast] as const;
}

/**
 * Parse a BBNF grammar that may contain `@import` directives.
 * Returns the parsed grammar with imports and rules separated.
 */
export function BBNFToASTWithImports(input: string) {
    const parser = new BBNFGrammar().grammarWithImports().eof();
    const parsed = parser.parse(input);

    if (!parsed) {
        return [parser] as const;
    }

    const grammar = parsed as ParsedGrammar;

    return [parser, {
        imports: grammar.imports,
        recovers: grammar.recovers ?? [],
        no_collapses: grammar.no_collapses ?? [],
        pretties: grammar.pretties ?? [],
        ws: grammar.ws ?? [],
        tokens: grammar.tokens ?? [],
        inlines: grammar.inlines ?? [],
        debugs: grammar.debugs ?? [],
        rules: grammar.rules,
    } as ParsedGrammar] as const;
}

/**
 * Parse multiple BBNF source files and merge their rules into a single AST.
 *
 * `files` is a Map from filename/path to source text.
 * Rules from later files override earlier ones with the same name.
 * Import directives in each file are resolved against the provided files map.
 */
export function BBNFToASTFromFiles(files: Map<string, string>): ParsedGrammar {
    const allImports: ParsedGrammar["imports"] = [];
    const allRecovers: ParsedGrammar["recovers"] = [];
    const allNoCollapses: ParsedGrammar["no_collapses"] = [];
    const allPretties: ParsedGrammar["pretties"] = [];
    const allWs: ParsedGrammar["ws"] = [];
    const allTokens: ParsedGrammar["tokens"] = [];
    const allInlines: ParsedGrammar["inlines"] = [];
    const allDebugs: ParsedGrammar["debugs"] = [];
    const mergedAST = new Map<string, ProductionRule>() as AST;

    for (const [filename, source] of files) {
        const result = BBNFToASTWithImports(source);
        if (result.length < 2 || !result[1]) {
            throw new Error(`Failed to parse grammar: ${filename}`);
        }
        const parsed = result[1];
        allImports.push(...parsed.imports);
        allRecovers.push(...(parsed.recovers ?? []));
        allNoCollapses.push(...(parsed.no_collapses ?? []));
        allPretties.push(...(parsed.pretties ?? []));
        allWs.push(...parsed.ws);
        allTokens.push(...parsed.tokens);
        allInlines.push(...parsed.inlines);
        allDebugs.push(...parsed.debugs);
        for (const [name, rule] of parsed.rules) {
            mergedAST.set(name, rule);
        }
    }

    return {
        imports: allImports,
        recovers: allRecovers,
        no_collapses: allNoCollapses,
        pretties: allPretties,
        ws: allWs,
        tokens: allTokens,
        inlines: allInlines,
        debugs: allDebugs,
        rules: mergedAST,
    };
}
