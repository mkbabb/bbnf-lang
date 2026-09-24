import { describe, it, expect } from "vitest";

import type { AST } from "../src/types.js";
import {
    findAliases,
    findTransparentAlternations,
    classifyAcyclicDeps,
    tarjanSCC,
    buildDepGraphs,
    computeRefCounts,
    analyzeGrammar,
    CharSet,
    analyzeFirst,
    routes,
    findFirstSetConflicts,
} from "../src/analysis/index.js";
import type { Routes } from "../src/analysis/index.js";
import { rule, nonterminal, literal, alternation, regexExpr, optional } from "./helpers/ast-builders.js";

// ---------------------------------------------------------------------------
// Analysis tests
// ---------------------------------------------------------------------------

describe("findAliases", () => {
    it("detects alias rule (bare nonterminal reference)", () => {
        const ast: AST = new Map([
            rule("alias", nonterminal("target")),
            rule("target", regexExpr(/x/)),
        ]);
        const { cyclicRules } = tarjanSCC(buildDepGraphs(ast).depGraph);
        const aliases = findAliases(ast, cyclicRules);

        expect(aliases.get("alias")).toBe("target");
    });

    it("excludes cyclic rules from aliases", () => {
        const ast: AST = new Map([
            rule("a", nonterminal("b")),
            rule("b", nonterminal("a")),
        ]);
        const { cyclicRules } = tarjanSCC(buildDepGraphs(ast).depGraph);
        const aliases = findAliases(ast, cyclicRules);

        expect(aliases.has("a")).toBe(false);
        expect(aliases.has("b")).toBe(false);
    });
});

describe("findTransparentAlternations", () => {
    it("detects cyclic pure-nonterminal alternation as transparent", () => {
        // root = root | a | b (self-recursive alternation of nonterminals)
        const ast: AST = new Map([
            rule(
                "root",
                alternation([
                    nonterminal("root"),
                    nonterminal("a"),
                    nonterminal("b"),
                ]),
            ),
            rule("a", regexExpr(/x/)),
            rule("b", regexExpr(/y/)),
        ]);
        const { cyclicRules } = tarjanSCC(buildDepGraphs(ast).depGraph);
        const transparent = findTransparentAlternations(ast, cyclicRules);

        expect(transparent.has("root")).toBe(true);
    });

    it("non-cyclic alternation is NOT transparent", () => {
        // value = a | b (not cyclic, so not transparent)
        const ast: AST = new Map([
            rule("value", alternation([nonterminal("a"), nonterminal("b")])),
            rule("a", regexExpr(/x/)),
            rule("b", regexExpr(/y/)),
        ]);
        const { cyclicRules } = tarjanSCC(buildDepGraphs(ast).depGraph);
        const transparent = findTransparentAlternations(ast, cyclicRules);

        expect(transparent.has("value")).toBe(false);
    });
});

describe("findFirstSetConflicts", () => {
    it("detects overlap when branches share first character", () => {
        // rule = "a" | "ab" | "b"  -- branches 0 and 1 both start with 'a'
        // (We need the overall FIRST set to have >1 char to avoid the short-circuit.)
        const ast: AST = new Map([
            rule("r", alternation([literal("a"), literal("ab"), literal("b")])),
        ]);
        const conflicts = findFirstSetConflicts(ast);

        expect(conflicts.has("r")).toBe(true);
        const rConflicts = conflicts.get("r")!;
        expect(rConflicts.length).toBeGreaterThan(0);
        // The overlap should contain 'a' (code 97).
        expect(rConflicts[0].overlap.has(97)).toBe(true);
    });

    it("no conflicts when branches have disjoint first characters", () => {
        // rule = "a" | "b"
        const ast: AST = new Map([
            rule("r", alternation([literal("a"), literal("b")])),
        ]);
        const conflicts = findFirstSetConflicts(ast);

        expect(conflicts.has("r")).toBe(false);
    });
});

describe("classifyAcyclicDeps", () => {
    it("linear chain is acyclic", () => {
        // a -> b -> c (no cycles)
        const depGraph = new Map<string, Set<string>>([
            ["a", new Set(["b"])],
            ["b", new Set(["c"])],
            ["c", new Set()],
        ]);
        const { acyclic, nonAcyclic } = classifyAcyclicDeps(depGraph);

        expect(acyclic.has("a")).toBe(true);
        expect(acyclic.has("b")).toBe(true);
        expect(acyclic.has("c")).toBe(true);
        expect(nonAcyclic.size).toBe(0);
    });

    it("self-referencing rule is non-acyclic", () => {
        // a -> a (self-loop)
        const depGraph = new Map<string, Set<string>>([
            ["a", new Set(["a"])],
        ]);
        const { acyclic, nonAcyclic } = classifyAcyclicDeps(depGraph);

        expect(nonAcyclic.has("a")).toBe(true);
        expect(acyclic.has("a")).toBe(false);
    });
});

// ---------------------------------------------------------------------------
// CharSet tests
// ---------------------------------------------------------------------------

describe("CharSet", () => {
    it("intersection returns only overlapping characters", () => {
        const a = new CharSet();
        a.add(65); // A
        a.add(66); // B
        a.add(67); // C

        const b = new CharSet();
        b.add(66); // B
        b.add(67); // C
        b.add(68); // D

        const inter = a.intersection(b);
        expect(inter.has(65)).toBe(false);
        expect(inter.has(66)).toBe(true);
        expect(inter.has(67)).toBe(true);
        expect(inter.has(68)).toBe(false);
    });

    it("intersection of disjoint sets is empty", () => {
        const a = new CharSet();
        a.add(65); // A
        a.add(66); // B

        const b = new CharSet();
        b.add(67); // C
        b.add(68); // D

        const inter = a.intersection(b);
        expect(inter.isEmpty()).toBe(true);
    });

    it("len returns correct popcount", () => {
        const cs = new CharSet();
        cs.add(48); // '0'
        cs.add(49); // '1'
        cs.add(50); // '2'
        cs.add(97); // 'a'
        cs.add(98); // 'b'

        expect(cs.len()).toBe(5);
    });

    it("[Symbol.iterator] yields correct character codes", () => {
        const cs = new CharSet();
        cs.add(65); // A
        cs.add(90); // Z
        cs.add(97); // a

        const codes = [...cs];
        expect(codes).toEqual([65, 90, 97]);
    });
});

// ---------------------------------------------------------------------------
// Extended analysis tests
// ---------------------------------------------------------------------------

describe("tarjanSCC (extended)", () => {
    it("detects 3+ member SCC", () => {
        // a -> b -> c -> a (cycle of 3)
        const depGraph = new Map<string, Set<string>>([
            ["a", new Set(["b"])],
            ["b", new Set(["c"])],
            ["c", new Set(["a"])],
        ]);
        const { sccs, cyclicRules } = tarjanSCC(depGraph);

        // All three should be in the same SCC
        expect(cyclicRules.has("a")).toBe(true);
        expect(cyclicRules.has("b")).toBe(true);
        expect(cyclicRules.has("c")).toBe(true);

        // Should be exactly one SCC containing all three
        const largeScc = sccs.find((scc) => scc.length === 3);
        expect(largeScc).toBeDefined();
    });
});

describe("computeRefCounts", () => {
    it("counts references accurately", () => {
        // a = b | c ; b = c ; c = /x/
        // c is referenced by both a and b (count 2), b by a (count 1)
        const ast: AST = new Map([
            rule("a", alternation([nonterminal("b"), nonterminal("c")])),
            rule("b", nonterminal("c")),
            rule("c", regexExpr(/x/)),
        ]);
        const counts = computeRefCounts(ast);

        expect(counts.get("b")).toBe(1); // referenced by a
        expect(counts.get("c")).toBe(2); // referenced by a and b
        expect(counts.get("a")).toBe(0); // not referenced by anyone
    });
});

describe("buildDepGraphs", () => {
    it("builds correct forward and reverse graphs", () => {
        const ast: AST = new Map([
            rule("a", alternation([nonterminal("b"), nonterminal("c")])),
            rule("b", nonterminal("c")),
            rule("c", regexExpr(/x/)),
        ]);
        const { depGraph, rdepGraph } = buildDepGraphs(ast);

        // Forward: a depends on b, c; b depends on c
        expect(depGraph.get("a")!.has("b")).toBe(true);
        expect(depGraph.get("a")!.has("c")).toBe(true);
        expect(depGraph.get("b")!.has("c")).toBe(true);
        expect(depGraph.get("c")!.size).toBe(0);

        // Reverse: c is depended on by a, b; b is depended on by a
        expect(rdepGraph.get("c")!.has("a")).toBe(true);
        expect(rdepGraph.get("c")!.has("b")).toBe(true);
        expect(rdepGraph.get("b")!.has("a")).toBe(true);
        expect(rdepGraph.get("a")!.size).toBe(0);
    });
});

// ---------------------------------------------------------------------------
// Dispatch table tests
// ---------------------------------------------------------------------------

/** The ordered sub-choice routed for unit `c` (`[]` = the choice fails there). */
const routed = (r: Routes, c: number): number[] => (r.tbl[c] < 0 ? [] : r.groups[r.tbl[c]]);
const routesOf = (alts: ReturnType<typeof literal>[]) => {
    const ast: AST = new Map([rule("r", alternation(alts))]);
    const { info } = analyzeFirst(ast);
    return routes(alts.map((a) => info(a)));
};

describe("routes: disjoint alternatives (0.1.4's perfect dispatch)", () => {
    it("routes each first unit to its one alternative", () => {
        // r = "a" | "b" | "c"
        const rt = routesOf([literal("a"), literal("b"), literal("c")]);
        expect(rt).not.toBeNull();
        expect(routed(rt!, 97)).toEqual([0]); // 'a' → 0
        expect(routed(rt!, 98)).toEqual([1]); // 'b' → 1
        expect(routed(rt!, 99)).toEqual([2]); // 'c' → 2
        expect(routed(rt!, 100)).toEqual([]); // 'd' → none
        expect(rt!.na).toBe(-1); // no alternative starts with a non-ASCII unit
        expect(rt!.eof).toBe(-1); // nor succeeds at end of input
    });

    it("returns null for overlapping first sets that leave one route (the whole choice)", () => {
        // r = "ab" | "ac" — both start with 'a'
        expect(routesOf([literal("ab"), literal("ac")])).toBeNull();
    });
});

describe("routes: colliding alternatives (0.1.4's partial dispatch)", () => {
    it("groups colliding alternatives in order, routes disjoint ones alone", () => {
        // r = "ab" | "ac" | "{" | "["
        const rt = routesOf([literal("ab"), literal("ac"), literal("{"), literal("[")]);
        expect(rt).not.toBeNull();
        expect(rt!.groups.length).toBeGreaterThanOrEqual(3);
        expect(routed(rt!, 123)).toEqual([2]); // '{'
        expect(routed(rt!, 91)).toEqual([3]); // '['
        expect(routed(rt!, 97)).toEqual([0, 1]); // 'a': "ab" then "ac", in order
    });

    it("returns null when all alternatives collide", () => {
        // r = "abc" | "axy" | "azz" — all start with 'a', one route
        expect(routesOf([literal("abc"), literal("axy"), literal("azz")])).toBeNull();
    });

    it("routes two disjoint alternatives (no minimum count)", () => {
        const rt = routesOf([literal("a"), literal("b")]);
        expect(routed(rt!, 97)).toEqual([0]);
        expect(routed(rt!, 98)).toEqual([1]);
    });

    it("keeps a nullable alternative in its ordered place on every route", () => {
        // r = "a" | "b" | "c"? | "{" — the optional can succeed anywhere, after "a"/"b" are tried
        const rt = routesOf([literal("a"), literal("b"), optional(literal("c")), literal("{")]);
        expect(rt).not.toBeNull();
        expect(routed(rt!, 97)).toEqual([0, 2]);
        expect(routed(rt!, 99)).toEqual([2]);
        expect(routed(rt!, 123)).toEqual([2, 3]); // "c"? (empty) wins before "{", as the plain choice
        expect(routed(rt!, 122)).toEqual([2]);
        expect(rt!.groups[rt!.na]).toEqual([2]);
        expect(rt!.groups[rt!.eof]).toEqual([2]);
    });
});

describe("analyzeGrammar", () => {
    it("returns complete analysis cache", () => {
        const ast: AST = new Map([
            rule("start", alternation([nonterminal("a"), nonterminal("b")])),
            rule("a", regexExpr(/x/)),
            rule("b", regexExpr(/y/)),
        ]);
        const analysis = analyzeGrammar(ast);

        expect(analysis.depGraph).toBeDefined();
        expect(analysis.rdepGraph).toBeDefined();
        expect(analysis.sccs).toBeDefined();
        expect(analysis.cyclicRules).toBeDefined();
        expect(analysis.topoOrder).toBeDefined();
        expect(analysis.refCounts).toBeDefined();
        expect(analysis.aliases).toBeDefined();
        expect(analysis.transparentAlternations).toBeDefined();
        expect(analysis.acyclicRules).toBeDefined();
        expect(analysis.nonAcyclicRules).toBeDefined();

        // topoOrder should contain all rules
        expect(analysis.topoOrder.length).toBe(3);
        // All rules should be acyclic (no cycles)
        expect(analysis.acyclicRules.has("start")).toBe(true);
        expect(analysis.acyclicRules.has("a")).toBe(true);
        expect(analysis.acyclicRules.has("b")).toBe(true);
    });
});
