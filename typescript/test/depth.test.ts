// SERVED MODEL: claude-opus-5-5
import { describe, it, expect } from "vitest";
import { readFileSync } from "node:fs";

import { BBNFToAST } from "../src/parse.js";
import { compile } from "../src/compile.js";
import { backEdges, DEFAULT_MAX_DEPTH, FAIL } from "../src/emit.js";
import { loadGrammar } from "../src/generate.js";
import { dedupGroups } from "../src/analysis/index.js";

/** The nesting-depth fault (README "Nesting depth"): a counter on the back-edges alone; deep input is refused, never thrown. */
describe("the nesting-depth fault", () => {
    const [ast] = loadGrammar(new URL("./fixtures/value-js/css.bbnf", import.meta.url).pathname, (p) => readFileSync(p, "utf8"));
    dedupGroups(ast);
    const entries = ["valueTop", "keyframeSelector", "ruleList"];
    const p = compile(ast, { entries });
    const calc = (n: number) => "calc(".repeat(n) + "1" + ")".repeat(n);

    it("calc( nested 10,000 deep is refused (FAIL), and nothing throws", () => {
        expect(p.entries.valueTop(calc(10_000))).toBe(FAIL);
        expect(p.rules.valueTop(calc(10_000), 0)).toBe(-1);
        expect(p.entries.ruleList("@media x{".repeat(10_000) + "a{b:c}" + "}".repeat(10_000))).toBe(FAIL);
    });

    it(`the limit is ${DEFAULT_MAX_DEPTH} back-edges: calc( nests to exactly that depth, and a trip does not outlive its parse`, () => {
        expect(p.entries.valueTop(calc(DEFAULT_MAX_DEPTH))).not.toBe(FAIL);
        expect(p.entries.valueTop(calc(DEFAULT_MAX_DEPTH + 1))).toBe(FAIL);
        expect(p.entries.valueTop(calc(3))).not.toBe(FAIL);
        const shallow = compile(ast, { entries, maxDepth: 4 });
        expect(shallow.entries.valueTop(calc(4))).not.toBe(FAIL);
        expect(shallow.entries.valueTop(calc(5))).toBe(FAIL);
    });

    it("only the back-edges carry the counter; an entry that reaches none carries no reset or check", () => {
        const body = p.emission.body;
        const counted = body.match(/if \(D >= \d+\)/g)?.length ?? 0;
        expect(counted).toBeGreaterThan(0);
        expect(counted).toBeLessThanOrEqual(backEdges(ast).size * 2); // each marked edge, in value and recognize mode
        const entry = (name: string) => body.split("\n").find((l) => l.includes(`function e${entries.indexOf(name)}(s)`))!;
        expect(entry("valueTop")).toMatch(/D = 0;.*D <= \d+/);
        expect(entry("keyframeSelector")).not.toMatch(/\bD\b/);
        expect(compile(ast, { entries, maxDepth: 0 }).emission.body).not.toMatch(/\bD\b/);
    });

    it("backEdges marks one edge on every cycle", () => {
        const [, g] = BBNFToAST(`a = "(" , a , ")" | b ;\nb = "[" , c , "]" | "x" ;\nc = b ;\n`);
        expect([...backEdges(g!)].map((e) => e.replace("\u0000", "→")).sort()).toEqual(["a→a", "c→b"]);
    });
});
