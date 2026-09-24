import { describe, it, expect } from "vitest";

import type { AST } from "../src/types.js";
import { regexFirst, analyzeFirst, routes } from "../src/analysis/index.js";
import type { Routes } from "../src/analysis/index.js";
import { rule, nonterminal, literal, alternation, concatenation, regexExpr, optional, epsilon } from "./helpers/ast-builders.js";

// ---------------------------------------------------------------------------
// regexFirst (sound: `first` is a superset, never `null`)
// ---------------------------------------------------------------------------

describe("regexFirst", () => {
    it("extracts chars from character class [abc]", () => {
        const cs = regexFirst(/[abc]/).first;
        expect(cs.has(97)).toBe(true); // a
        expect(cs.has(98)).toBe(true); // b
        expect(cs.has(99)).toBe(true); // c
        expect(cs.has(100)).toBe(false); // d
    });

    it("extracts chars from range [a-z]", () => {
        const cs = regexFirst(/[a-z]/).first;
        expect(cs.has(97)).toBe(true); // a
        expect(cs.has(122)).toBe(true); // z
        expect(cs.has(65)).toBe(false); // A
    });

    it("handles negated class [^a]", () => {
        const cs = regexFirst(/[^a]/).first;
        expect(cs.has(97)).toBe(false); // a is excluded
        expect(cs.has(98)).toBe(true); // b is included
    });

    it("handles alternation (a|b)", () => {
        const cs = regexFirst(/a|b/).first;
        expect(cs.has(97)).toBe(true); // a
        expect(cs.has(98)).toBe(true); // b
        expect(cs.has(99)).toBe(false); // c
    });

    it("handles escape sequences (\\d)", () => {
        const cs = regexFirst(/\d/).first;
        expect(cs.has(48)).toBe(true); // 0
        expect(cs.has(57)).toBe(true); // 9
        expect(cs.has(97)).toBe(false); // a
    });

    it("answers dot (.) soundly: every unit but the line terminators", () => {
        const cs = regexFirst(/./).first;
        expect(cs.has(97)).toBe(true);
        expect(cs.has(32)).toBe(true);
        expect(cs.has(10)).toBe(false); // \n
        expect(cs.has(13)).toBe(false); // \r
        expect(cs.nonAscii).toBe(true);
    });

    it("handles \\w escape", () => {
        const cs = regexFirst(/\w/).first;
        expect(cs.has(48)).toBe(true); // 0
        expect(cs.has(65)).toBe(true); // A
        expect(cs.has(97)).toBe(true); // a
        expect(cs.has(95)).toBe(true); // _
    });
});

// ---------------------------------------------------------------------------
// analyzeFirst (per-rule facts to a fixpoint)
// ---------------------------------------------------------------------------

describe("analyzeFirst", () => {
    it("literal has its first char in FIRST set", () => {
        const ast: AST = new Map([rule("r", literal("hello"))]);
        const { ruleInfo } = analyzeFirst(ast);
        const firstSets = new Map([...ruleInfo].map(([k, v]) => [k, v.first]));
        expect(firstSets.get("r")!.has(104)).toBe(true); // 'h'
    });

    it("alternation unions FIRST sets of all branches", () => {
        const ast: AST = new Map([
            rule("r", alternation([literal("abc"), literal("xyz")])),
        ]);
        const { ruleInfo } = analyzeFirst(ast);
        const firstSets = new Map([...ruleInfo].map(([k, v]) => [k, v.first]));
        expect(firstSets.get("r")!.has(97)).toBe(true); // 'a'
        expect(firstSets.get("r")!.has(120)).toBe(true); // 'x'
    });

    it("concatenation takes FIRST of first non-nullable element", () => {
        const ast: AST = new Map([
            rule("r", concatenation([literal("a"), literal("b")])),
        ]);
        const { ruleInfo } = analyzeFirst(ast);
        const firstSets = new Map([...ruleInfo].map(([k, v]) => [k, v.first]));
        expect(firstSets.get("r")!.has(97)).toBe(true); // 'a'
        expect(firstSets.get("r")!.has(98)).toBe(false); // 'b' is not reachable
    });

    it("nullable rule includes both FIRST chars when first elem is optional", () => {
        const ast: AST = new Map([
            rule("r", concatenation([optional(literal("a")), literal("b")])),
        ]);
        const { ruleInfo } = analyzeFirst(ast);
        const firstSets = new Map([...ruleInfo].map(([k, v]) => [k, v.first]));
        expect(firstSets.get("r")!.has(97)).toBe(true); // 'a' from optional
        expect(firstSets.get("r")!.has(98)).toBe(true); // 'b' since first is nullable
    });

    it("nonterminal reference inherits FIRST set", () => {
        const ast: AST = new Map([
            rule("a", nonterminal("b")),
            rule("b", literal("xyz")),
        ]);
        const { ruleInfo } = analyzeFirst(ast);
        const firstSets = new Map([...ruleInfo].map(([k, v]) => [k, v.first]));
        expect(firstSets.get("a")!.has(120)).toBe(true); // 'x' from b
    });

    it("cyclic rules converge to fixed point", () => {
        // a = "x" | b ; b = "y" | a ;
        const ast: AST = new Map([
            rule("a", alternation([literal("x"), nonterminal("b")])),
            rule("b", alternation([literal("y"), nonterminal("a")])),
        ]);
        const { ruleInfo } = analyzeFirst(ast);
        const firstSets = new Map([...ruleInfo].map(([k, v]) => [k, v.first]));
        // Both should have 'x' and 'y'
        expect(firstSets.get("a")!.has(120)).toBe(true); // 'x'
        expect(firstSets.get("a")!.has(121)).toBe(true); // 'y'
        expect(firstSets.get("b")!.has(120)).toBe(true); // 'x'
        expect(firstSets.get("b")!.has(121)).toBe(true); // 'y'
    });
});

// routes (0.1.4's buildDispatchTable, answered soundly)
// ---------------------------------------------------------------------------

const routed = (r: Routes, c: number): number[] => (r.tbl[c] < 0 ? [] : r.groups[r.tbl[c]]);
const routesOf = (alts: Parameters<typeof alternation>[0]) => {
    const ast: AST = new Map([rule("r", alternation(alts))]);
    const { info } = analyzeFirst(ast);
    return routes(alts.map((a) => info(a)));
};

describe("routes", () => {
    it("routes disjoint alternatives one to one", () => {
        const rt = routesOf([literal("a"), literal("b"), literal("c")]);
        expect(rt).not.toBeNull();
        expect(routed(rt!, 97)).toEqual([0]); // 'a' -> branch 0
        expect(routed(rt!, 98)).toEqual([1]); // 'b' -> branch 1
        expect(routed(rt!, 99)).toEqual([2]); // 'c' -> branch 2
    });

    it("returns null for overlapping alternatives (one route, the whole choice)", () => {
        // "ab" and "ac" both start with 'a'
        expect(routesOf([literal("ab"), literal("ac")])).toBeNull();
    });

    it("keeps a nullable alternative on every route, after the ones before it", () => {
        const rt = routesOf([literal("a"), epsilon()]);
        expect(rt).not.toBeNull();
        expect(routed(rt!, 97)).toEqual([0, 1]);
        expect(routed(rt!, 98)).toEqual([1]);
        expect(rt!.groups[rt!.eof]).toEqual([1]);
    });

    it("handles regex-based alternatives", () => {
        // /[0-9]/ and /[a-z]/ are disjoint
        const rt = routesOf([regexExpr(/[0-9]/), regexExpr(/[a-z]/)]);
        expect(rt).not.toBeNull();
        expect(routed(rt!, 48)).toEqual([0]); // '0' -> branch 0
        expect(routed(rt!, 97)).toEqual([1]); // 'a' -> branch 1
    });
});
