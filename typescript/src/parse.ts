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

    const { imports, recovers, pretties, rules } = parsed as { imports: any[]; recovers: any[]; pretties: any[]; rules: ProductionRule[] };

    const ast = rules.reduce(
        (acc, productionRule) => {
            return acc.set(productionRule.name.value, productionRule);
        },
        new Map<string, ProductionRule>(),
    ) as AST;

    return [parser, { imports, recovers: recovers ?? [], pretties: pretties ?? [], rules: ast } as ParsedGrammar] as const;
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
    const allPretties: ParsedGrammar["pretties"] = [];
    const mergedAST = new Map<string, ProductionRule>() as AST;

    for (const [filename, source] of files) {
        const result = BBNFToASTWithImports(source);
        if (result.length < 2 || !result[1]) {
            throw new Error(`Failed to parse grammar: ${filename}`);
        }
        const parsed = result[1];
        allImports.push(...parsed.imports);
        allRecovers.push(...(parsed.recovers ?? []));
        allPretties.push(...(parsed.pretties ?? []));
        for (const [name, rule] of parsed.rules) {
            mergedAST.set(name, rule);
        }
    }

    return { imports: allImports, recovers: allRecovers, pretties: allPretties, rules: mergedAST };
}
