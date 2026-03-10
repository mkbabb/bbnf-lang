/**
 * Runtime prettify interpreter — public API.
 *
 * Walks a BBNF Expression tree + parsed value simultaneously and produces
 * formatted output via pprint.
 */

import type { AST, PrettyDirective, PrettyHint } from "../types.js";
import { pprint, type PrinterConfig } from "@mkbabb/pprint";
import { expressionToDoc } from "./interpreter.js";

/**
 * Format a parsed value using the grammar's expression tree and @pretty
 * directives.
 *
 * @param value           The parsed value (produced by ASTToParser with tagAlternations=true).
 * @param ruleName        The entry rule name to start formatting from.
 * @param ast             The grammar AST (Map<string, ProductionRule>).
 * @param prettyDirectives  The @pretty directives from the parsed grammar.
 * @param config          Optional pprint configuration (maxWidth, indent, useTabs).
 * @returns               The formatted string.
 */
export function prettify(
    value: unknown,
    ruleName: string,
    ast: AST,
    prettyDirectives: PrettyDirective[],
    config?: PrinterConfig,
): string {
    const prettyMap = new Map<string, PrettyHint[]>();
    for (const dir of prettyDirectives) {
        prettyMap.set(dir.ruleName, dir.hints);
    }

    const rule = ast.get(ruleName);
    if (!rule) return String(value);

    const doc = expressionToDoc(
        rule.expression,
        value,
        ast,
        prettyMap,
        ruleName,
    );
    return pprint(doc, config);
}

export { expressionToDoc } from "./interpreter.js";
export type { ResolvedHints } from "./hints.js";
export { resolveHints, applySeparator, wrapDoc } from "./hints.js";
export { detectWrappedPattern, detectKeyValuePattern } from "./patterns.js";
export { inferHints } from "./heuristics.js";
