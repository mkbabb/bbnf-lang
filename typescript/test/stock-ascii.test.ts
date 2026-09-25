// SERVED MODEL: claude-opus-5-5
import { describe, it, expect } from "vitest";
import { mkdtempSync, readFileSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { emitGrammar, FAIL } from "../src/emit.js";
import { loadGrammar } from "../src/generate.js";
import { dedupGroups } from "../src/analysis/index.js";
import { stockNonAsciiRoute } from "./helpers/bbnf-0.1.4/index.js";

/**
 * The stock-ASCII proof (X.P.W7 `.e`, gate E-3). The oracle is the published 0.1.4's verdict (whole input
 * accepted or not) on every non-ASCII-bearing source of value.js's bench of record, for the 17 rules
 * value.js calls (`fixtures/value-js/verdicts-0.1.4.json`, frozen by its script). The emitted module with
 * 0.1.4's ASCII-only non-ASCII dispatch substituted reproduces it with 0 mismatches — so routing is the only
 * difference — and the module as shipped (the one analysis) differs from it exactly in the enumerated
 * F-b-4 rows (COHESION §0ck 1: accepted as spec-correct; stock-ASCII routing is never shipped).
 */
type Verdicts = { rules: string[]; sources: string[]; verdicts: Record<string, string> };
const V = JSON.parse(readFileSync(new URL("./fixtures/value-js/verdicts-0.1.4.json", import.meta.url), "utf8")) as Verdicts;

/** F-b-4 at the raw-grammar reading: rule → source indices (into `V.sources`) where the sound answer differs. */
const FB4: Record<string, number[]> = JSON.parse(readFileSync(new URL("./fixtures/value-js/F-b-4.json", import.meta.url), "utf8")).rows;

describe("routing: the one analysis vs bbnf-lang 0.1.4's ASCII-only dispatch (value.js's grammar)", async () => {
    const [ast] = loadGrammar(new URL("./fixtures/value-js/css.bbnf", import.meta.url).pathname, (p) => readFileSync(p, "utf8"));
    dedupGroups(ast);
    const dir = mkdtempSync(join(tmpdir(), "bbnf-stock-"));
    const load = async (name: string, nonAsciiRoute?: ReturnType<typeof stockNonAsciiRoute>) => {
        const file = join(dir, `${name}.mjs`);
        writeFileSync(file, emitGrammar(ast, { entries: V.rules, nonAsciiRoute }).module);
        return (await import(file)).createParser({}) as { entries: Record<string, (s: string) => unknown> };
    };
    const stock = await load("stock", stockNonAsciiRoute(ast));
    const sound = await load("sound");
    const verdicts = (p: typeof sound, rule: string) => V.sources.map((s) => (p.entries[rule](s) === FAIL ? "0" : "1")).join("");
    const differing = (a: string, b: string) => [...a].flatMap((x, i) => (x === b[i] ? [] : [i]));

    it(`stock-ASCII routing reproduces 0.1.4: 0 mismatches over ${V.sources.length} sources × ${V.rules.length} rules`, () => {
        for (const rule of V.rules) expect([rule, differing(verdicts(stock, rule), V.verdicts[rule])]).toEqual([rule, []]);
    });

    it("the shipped routing differs from 0.1.4 only in the enumerated F-b-4 rows", () => {
        for (const rule of V.rules) expect([rule, differing(verdicts(sound, rule), V.verdicts[rule])]).toEqual([rule, FB4[rule] ?? []]);
    });

});
