// SERVED MODEL: claude-opus-5-5
//
// bbnf-lang 0.1.4's first-character dispatch, kept ONLY as a test instrument (the stock-ASCII proof,
// `test/stock-ascii.test.ts`). `charset.ts`, `regex-first.ts`, `first-sets.ts` and `dispatch.ts` are
// `e91428ce1:typescript/src/analysis/` verbatim (the published 0.1.4 source), their `../types.js` and
// `./metadata.js` imports repointed. The package's one analysis (`src/analysis/first.ts`) replaced
// them; COHESION §0ck 1 rules that stock-ASCII routing is never shipped.
import type { AST, Expression } from "../../../src/types.js";
import { analyzeGrammar } from "../../../src/analysis/index.js";
import { computeFirstSets } from "./first-sets.js";
import { buildDispatchTable, buildPartialDispatchTable } from "./dispatch.js";

/**
 * Where 0.1.4 sent a non-ASCII first unit, per ordered choice: nowhere under a perfect dispatch table,
 * the fallback alternatives under a partial one, and `null` (every alternative, in order) when it
 * built no table. The value for `EmitOptions.nonAsciiRoute`.
 */
export function stockNonAsciiRoute(ast: AST): (alternatives: readonly Expression[]) => readonly number[] | null {
    const { firstSets, nullable } = computeFirstSets(ast, analyzeGrammar(ast));
    return (alternatives) => {
        const alts = [...alternatives];
        if (alts.length < 2) return null;
        if (buildDispatchTable(alts, firstSets, nullable)?.isPerfect) return [];
        return buildPartialDispatchTable(alts, firstSets, nullable)?.fallbackIndices ?? null;
    };
}
