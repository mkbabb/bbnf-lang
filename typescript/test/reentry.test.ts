// SERVED MODEL: claude-opus-5-5
import { describe, it, expect } from "vitest";
import { readFileSync } from "node:fs";

import { BBNFToAST } from "../src/parse.js";
import { compile, createAudit } from "../src/compile.js";
import { emitGrammar, FAIL } from "../src/emit.js";
import { grammarFromModules } from "../src/imports.js";
import { reenteredRules } from "../src/analysis/reentry.js";
import type { AST } from "../src/types.js";

/**
 * The refusal discipline (value.js X.P.W7 `.l3`, COHESION §0dq): a refusal builds no value it
 * discards (a `text` rule's value waits for its sequence to commit), and re-scans nothing a failed
 * alternative already classified (a re-entered rule keeps its last recognize answer).
 */
const DIR = new URL("./fixtures/value-js/", import.meta.url);
const MODULES = ["tokens", "math", "color", "value", "stylesheet", "css"];
const files: Record<string, string> = Object.fromEntries(
    MODULES.map((n) => [`/css/${n}.bbnf`, readFileSync(new URL(`${n}.bbnf`, DIR), "utf8")]),
);
const valueJs = (): AST => grammarFromModules(files, "/css/css.bbnf");

// The stylesheet layer's shape: a rule block is a prelude and a tail; an unclosed block is the
// prelude again, then "{" and the rest; anything else is the fault.
const SHEET = String.raw`
grp   = "(" , /[a-z ]*/ , ")" ;
pre   = ( /[^{;()]+/ | grp ) * ;
block = pre , ( ";" | "{" , /[^}]*/ , "}" ) ;
open  = pre , "{" , /[\s\S]*/ ;
rest  = /[\s\S]+/ ;
sheet = ( block ) * , ( open | rest ) ? ;
head  = pre << ";" ;
`;
const sheetAst = () => BBNFToAST(SHEET)[1]!;

type Profile = { slots: string[]; calls: number[]; actions: string[]; actionCalls: number[] };
function instrumented(ast: AST, actions: Record<string, { kind: "text" | "map"; fn: (...a: never[]) => unknown }>) {
    const actionKinds = Object.fromEntries(Object.entries(actions).map(([n, a]) => [n, a.kind]));
    const e = emitGrammar(ast, { actionKinds, instrument: true });
    const make = new Function("A", "H", "ACTION_KINDS", "FAIL", "AUDIT", e.body) as (...a: unknown[]) => {
        entries: Record<string, (s: string) => unknown>; profile: () => Profile; resetProfile: () => void;
    };
    return make(actions, {}, e.actionKinds, FAIL, undefined);
}

describe("reenteredRules", () => {
    it("names the prelude a failed rule block leaves for the unclosed-block arm", () => {
        // `grp` leads both sides too, but through `pre`, whose kept answer covers it.
        expect(reenteredRules(sheetAst())).toEqual(["pre"]);
    });

    it("classifies value.js's grammar", () => {
        expect(reenteredRules(valueJs())).toEqual(["preludeRun"]);
    });
});

describe("a refusal builds no value it discards", () => {
    const actions = {
        pre: { kind: "text" as const, fn: (t: string) => t.trim() },
        block: { kind: "map" as const, fn: (v: unknown[]) => v[0] },
    };

    it("runs a text rule's action only once its sequence has matched", () => {
        const p = instrumented(sheetAst(), actions);
        expect(p.entries.sheet("a (b) c")).toEqual([[], "a (b) c"]);
        let r = p.profile();
        expect(r.actionCalls[r.actions.indexOf("pre")]).toBe(0); // the prelude never committed
        p.resetProfile();
        expect(p.entries.sheet("a;b{c}")).toEqual([["a", "b"], undefined]);
        r = p.profile();
        expect(r.actionCalls[r.actions.indexOf("pre")]).toBe(2);
        expect(p.entries.head("x ;")).toBe("x");
        expect(p.entries.head("x {")).toBe(FAIL);
    });

    it("answers the positional values the grammar spells", () => {
        const q = compile(sheetAst(), { actions });
        const table: [string, unknown][] = [
            ["", [[], undefined]], ["a", [[], "a"]], ["a;", [["a"], undefined]], ["a;b{c}", [["a", "b"], undefined]],
            ["a (b", [[], "a (b"]], ["a (b) {x", [[], ["a (b)", "{", "x"]]], ["a{", [[], ["a", "{", undefined]]],
            ["{}", [[""], undefined]], ["a(b)(c);d", [["a(b)(c)"], "d"]],
        ];
        for (const [s, want] of table) expect([s, q.entries.sheet(s)]).toEqual([s, want]);
    });
});

describe("a refusal re-scans nothing it has already classified", () => {
    it("scans the prelude once when the rule block fails and the unclosed-block arm reads it", () => {
        const p = instrumented(sheetAst(), { pre: { kind: "text", fn: (t: string) => t } });
        expect(p.entries.sheet("a (b) (c) {x")).toEqual([[], ["a (b) (c) ", "{", "x"]]);
        const r = p.profile();
        // `grp` runs inside the prelude's scan: two groups, one scan (the kept answer serves `open`).
        expect(r.calls[r.slots.indexOf("grp/r")]).toBe(2);
        expect(r.calls[r.slots.indexOf("pre/r")]).toBe(2); // called twice, scanned once
    });

    it("starts every parse with no kept answer", () => {
        const q = compile(sheetAst(), {});
        expect(q.entries.sheet("a (b) {x")).not.toBe(FAIL);
        expect(q.entries.sheet("a (b) {x")).toEqual(q.entries.sheet("a (b) {x"));
        expect(q.entries.head("a (b) ;")).not.toBe(FAIL);
        expect(q.entries.head("a (b) {")).toBe(FAIL);
    });

    it("keeps only the grammar's answer: the audit build re-runs every kept answer (0 violations)", () => {
        const audit = createAudit();
        const q = compile(sheetAst(), { audit });
        const alphabet = ["a", " ", "(", ")", "{", "}", ";"];
        let n = 0;
        const walk = (s: string, depth: number): void => {
            q.entries.sheet(s); n++;
            if (depth > 0) for (const c of alphabet) walk(s + c, depth - 1);
        };
        walk("", 5);
        expect(n).toBeGreaterThan(19_000);
        expect(audit.samples).toEqual([]);
        expect(audit.violations).toBe(0);
        expect(audit.checks).toBeGreaterThan(1_000);
    });
});
