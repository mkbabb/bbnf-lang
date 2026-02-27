import { describe, it, expect } from "vitest";

import type { AST, Expression, ProductionRule } from "../src/types.js";
import {
    findAliases,
    findTransparentAlternations,
    classifyAcyclicDeps,
    tarjanSCC,
    buildDepGraphs,
} from "../src/analysis.js";
import {
    CharSet,
    computeFirstSets,
    findFirstSetConflicts,
} from "../src/first-sets.js";
import { analyzeGrammar } from "../src/analysis.js";

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function rule(name: string, expression: Expression): [string, ProductionRule] {
    return [
        name,
        {
            name: { type: "nonterminal", value: name },
            expression,
            comment: { above: [], below: [] },
        } as ProductionRule,
    ];
}

function nonterminal(value: string): Expression {
    return { type: "nonterminal", value } as Expression;
}

function literal(value: string): Expression {
    return { type: "literal", value } as Expression;
}

function alternation(branches: Expression[]): Expression {
    return { type: "alternation", value: branches } as Expression;
}

function regexExpr(pattern: RegExp): Expression {
    return { type: "regex", value: pattern } as Expression;
}

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
        const analysis = analyzeGrammar(ast);
        const firstNullable = computeFirstSets(ast, analysis);
        const conflicts = findFirstSetConflicts(ast, firstNullable);

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
        const analysis = analyzeGrammar(ast);
        const firstNullable = computeFirstSets(ast, analysis);
        const conflicts = findFirstSetConflicts(ast, firstNullable);

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
