// SERVED MODEL: claude-opus-5-5
//
// generate.ts — the parse-that FAÇADE over the one emitter: `BBNFToParser` / `BBNFToParserFromFile`
// compile a grammar at runtime (`compile()`, through `emit.ts`) and answer each rule as a parse-that
// 2.x `Parser` (`toParser`). There is no second interpreter: a rule's answer is the emitted code's.
// A rule is changed by giving it an ACTION (`options.actions`) or by the host SUPPLYING it
// (`options.host`, a parse-that `Parser`) at compile time; the returned table is read-only in effect
// (the emitted rules call each other directly, never through it).

import type { Parser } from "@mkbabb/parse-that";
import type { AST, Nonterminals, RecoverDirective } from "./types.js";
import { removeAllLeftRecursion } from "./optimize.js";
import { analyzeGrammar, dedupGroups } from "./analysis/index.js";
import { BBNFToAST, BBNFToASTWithImports } from "./parse.js";
import { loadModuleGraphSync, mergeModuleAST, mergeModuleRecovers } from "./imports.js";
import { resolve as resolveModuleId } from "./posix-path.js";
import { compile } from "./compile.js";
import type { Action, Compiled } from "./compile.js";
import { fromParser, toParser } from "./facade.js";

export type FacadeOptions = Readonly<{
    /** Apply left-recursion elimination to the grammar first. */
    optimizeGraph?: boolean;
    /** Actions by rule name (map · span · text). */
    actions?: Readonly<Record<string, Action>>;
    /** Rules the host supplies as parse-that parsers; a grammar rule of the same name is replaced. */
    host?: Readonly<Record<string, Parser<any>>>;
    /** Back-edge nesting limit (`0`: no depth fault). */
    maxDepth?: number;
}>;

/** Compiles `ast` and answers each rule (and each host rule) as a parse-that `Parser`. */
export function facade(ast: AST, recovers: readonly RecoverDirective[], options: FacadeOptions = {}): [Nonterminals, Compiled] {
    const hostParsers = options.host ?? {};
    const own: AST = new Map([...ast].filter(([name]) => !(name in hostParsers)));
    const host = Object.fromEntries(Object.entries(hostParsers).map(([n, p]) => [n, fromParser(p)]));
    const compiled = compile(own, { actions: options.actions, host, recovers, maxDepth: options.maxDepth });
    const nonterminals: Nonterminals = {};
    for (const name of own.keys()) nonterminals[name] = toParser(compiled, name);
    for (const [name, p] of Object.entries(hostParsers)) nonterminals[name] = p;
    return [nonterminals, compiled];
}

function prepare(ast: AST, optimizeGraph: boolean): AST {
    dedupGroups(ast);
    return optimizeGraph ? removeAllLeftRecursion(ast, analyzeGrammar(ast)) : ast;
}

/** A grammar's text → its rules as parse-that parsers, and the (final) AST. */
export function BBNFToParser(input: string, options: FacadeOptions = {}) {
    // Import-aware parsing first, to pick up @recover directives.
    const importResult = BBNFToASTWithImports(input);
    let ast: AST;
    let recovers: RecoverDirective[] = [];
    if (importResult.length >= 2 && importResult[1]) {
        ast = importResult[1].rules;
        recovers = importResult[1].recovers ?? [];
    } else {
        const [, plainAst] = BBNFToAST(input);
        if (!plainAst) throw new Error("Failed to parse BBNF grammar");
        ast = plainAst;
    }
    const finalAst = prepare(ast, options.optimizeGraph ?? false);
    const [nonterminals] = facade(finalAst, recovers, options);
    return [nonterminals, finalAst] as const;
}

/**
 * Parse a BBNF grammar from a module ID, resolving `@import` directives through the host's reader
 * (`(id) => text`; a node host passes `(p) => fs.readFileSync(p, "utf8")`; no filesystem is assumed).
 */
export function BBNFToParserFromFile(entryPath: string, readFileSync: (path: string) => string, options: FacadeOptions = {}) {
    const [ast, recovers] = loadGrammar(entryPath, readFileSync);
    const finalAst = prepare(ast, options.optimizeGraph ?? false);
    const [nonterminals] = facade(finalAst, recovers, options);
    return [nonterminals, finalAst] as const;
}

/** The merged AST and `@recover` directives of the module graph rooted at `entryPath`. */
export function loadGrammar(entryPath: string, readFileSync: (path: string) => string): [AST, RecoverDirective[]] {
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
    return [mergeModuleAST(registry, entry), mergeModuleRecovers(registry, entry)];
}
