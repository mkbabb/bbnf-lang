// SERVED MODEL: claude-opus-5-5
import { describe, it, expect } from "vitest";
import { readFileSync } from "node:fs";

import { grammarFromModules, loadModuleGraphSync } from "../src/imports.js";
import type { AST, Expression } from "../src/types.js";

/**
 * value.js's five CSS modules, each carrying the `@import` header its references
 * need, load through `@import` into the same rule table the published 0.1.4
 * built from their concatenation (value.js `src/css/bbnf/load.ts:42`), frozen in
 * `fixtures/value-js/ast-0.1.4.json` by `freeze-ast-0.1.4.mjs`.
 */
const DIR = new URL("./fixtures/value-js/", import.meta.url);
const MODULES = ["tokens", "math", "color", "value", "stylesheet", "css"];
const files: Record<string, string> = Object.fromEntries(
    MODULES.map((n) => [`/css/${n}.bbnf`, readFileSync(new URL(`${n}.bbnf`, DIR), "utf8")]),
);
const frozen = JSON.parse(readFileSync(new URL("ast-0.1.4.json", DIR), "utf8")) as {
    rules: number;
    ast: [string, unknown][];
};

const strip = (ast: AST): [string, unknown][] =>
    JSON.parse(
        JSON.stringify([...ast].map(([k, r]) => [k, r.expression]), (k, v) =>
            k === "range" || k === "comment" ? undefined : v instanceof RegExp ? `/${v.source}/${v.flags}` : v,
        ),
    );
const byName = (rows: [string, unknown][]) => [...rows].sort((a, b) => (a[0] < b[0] ? -1 : a[0] > b[0] ? 1 : 0));

function references(e: Expression, out: Set<string>): void {
    if (e.type === "nonterminal") out.add(e.value as string);
    const v = e.value as unknown;
    if (Array.isArray(v)) for (const x of v) if (x && typeof x === "object" && "type" in x) references(x as Expression, out);
    if (v && typeof v === "object" && !Array.isArray(v) && "type" in (v as object)) references(v as Expression, out);
}

describe("value.js's five modules through @import (T-2)", () => {
    it("merge to the 0.1.4 AST of their concatenation: 160 rules, deep-equal", () => {
        const ast = grammarFromModules(files, "/css/css.bbnf");
        expect(frozen.rules).toBe(160);
        expect(ast.size).toBe(160);
        expect(byName(strip(ast))).toEqual(byName(frozen.ast));
    });

    it("each module's own header makes every rule it references visible", () => {
        for (const n of MODULES.slice(0, 5)) {
            const id = `/css/${n}.bbnf`;
            const registry = loadModuleGraphSync(id, (p) => files[p]);
            expect(registry.errors, n).toEqual([]);
            const own = grammarFromModules(files, id);
            const refs = new Set<string>();
            for (const [, rule] of own) references(rule.expression, refs);
            const missing = [...refs].filter((r) => !own.has(r));
            expect(missing, n).toEqual([]);
        }
    });
});
