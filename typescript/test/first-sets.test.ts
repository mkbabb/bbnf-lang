import { describe, it, expect } from "vitest";

import type { AST, Expression, ProductionRule } from "../src/types.js";
import {
    CharSet,
    regexFirstChars,
    computeFirstSets,
    buildDispatchTable,
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

function concatenation(elems: Expression[]): Expression {
    return { type: "concatenation", value: elems } as Expression;
}

function regexExpr(pattern: RegExp): Expression {
    return { type: "regex", value: pattern } as Expression;
}

function optional(expr: Expression): Expression {
    return { type: "optional", value: expr } as Expression;
}

function epsilon(): Expression {
    return { type: "epsilon" } as Expression;
}

// ---------------------------------------------------------------------------
// regexFirstChars
// ---------------------------------------------------------------------------

describe("regexFirstChars", () => {
    it("extracts chars from character class [abc]", () => {
        const cs = regexFirstChars(/[abc]/);
        expect(cs).not.toBeNull();
        expect(cs!.has(97)).toBe(true); // a
        expect(cs!.has(98)).toBe(true); // b
        expect(cs!.has(99)).toBe(true); // c
        expect(cs!.has(100)).toBe(false); // d
    });

    it("extracts chars from range [a-z]", () => {
        const cs = regexFirstChars(/[a-z]/);
        expect(cs).not.toBeNull();
        expect(cs!.has(97)).toBe(true); // a
        expect(cs!.has(122)).toBe(true); // z
        expect(cs!.has(65)).toBe(false); // A
    });

    it("handles negated class [^a]", () => {
        const cs = regexFirstChars(/[^a]/);
        expect(cs).not.toBeNull();
        expect(cs!.has(97)).toBe(false); // a is excluded
        expect(cs!.has(98)).toBe(true); // b is included
    });

    it("handles alternation (a|b)", () => {
        const cs = regexFirstChars(/a|b/);
        expect(cs).not.toBeNull();
        expect(cs!.has(97)).toBe(true); // a
        expect(cs!.has(98)).toBe(true); // b
        expect(cs!.has(99)).toBe(false); // c
    });

    it("handles escape sequences (\\d)", () => {
        const cs = regexFirstChars(/\d/);
        expect(cs).not.toBeNull();
        expect(cs!.has(48)).toBe(true); // 0
        expect(cs!.has(57)).toBe(true); // 9
        expect(cs!.has(97)).toBe(false); // a
    });

    it("returns null for complex patterns like dot (.)", () => {
        const cs = regexFirstChars(/./);
        expect(cs).toBeNull();
    });

    it("handles \\w escape", () => {
        const cs = regexFirstChars(/\w/);
        expect(cs).not.toBeNull();
        expect(cs!.has(48)).toBe(true); // 0
        expect(cs!.has(65)).toBe(true); // A
        expect(cs!.has(97)).toBe(true); // a
        expect(cs!.has(95)).toBe(true); // _
    });
});

// ---------------------------------------------------------------------------
// computeFirstSets
// ---------------------------------------------------------------------------

describe("computeFirstSets", () => {
    it("literal has its first char in FIRST set", () => {
        const ast: AST = new Map([rule("r", literal("hello"))]);
        const analysis = analyzeGrammar(ast);
        const { firstSets } = computeFirstSets(ast, analysis);
        expect(firstSets.get("r")!.has(104)).toBe(true); // 'h'
    });

    it("alternation unions FIRST sets of all branches", () => {
        const ast: AST = new Map([
            rule("r", alternation([literal("abc"), literal("xyz")])),
        ]);
        const analysis = analyzeGrammar(ast);
        const { firstSets } = computeFirstSets(ast, analysis);
        expect(firstSets.get("r")!.has(97)).toBe(true); // 'a'
        expect(firstSets.get("r")!.has(120)).toBe(true); // 'x'
    });

    it("concatenation takes FIRST of first non-nullable element", () => {
        const ast: AST = new Map([
            rule("r", concatenation([literal("a"), literal("b")])),
        ]);
        const analysis = analyzeGrammar(ast);
        const { firstSets } = computeFirstSets(ast, analysis);
        expect(firstSets.get("r")!.has(97)).toBe(true); // 'a'
        expect(firstSets.get("r")!.has(98)).toBe(false); // 'b' is not reachable
    });

    it("nullable rule includes both FIRST chars when first elem is optional", () => {
        const ast: AST = new Map([
            rule("r", concatenation([optional(literal("a")), literal("b")])),
        ]);
        const analysis = analyzeGrammar(ast);
        const { firstSets, nullable } = computeFirstSets(ast, analysis);
        expect(firstSets.get("r")!.has(97)).toBe(true); // 'a' from optional
        expect(firstSets.get("r")!.has(98)).toBe(true); // 'b' since first is nullable
    });

    it("nonterminal reference inherits FIRST set", () => {
        const ast: AST = new Map([
            rule("a", nonterminal("b")),
            rule("b", literal("xyz")),
        ]);
        const analysis = analyzeGrammar(ast);
        const { firstSets } = computeFirstSets(ast, analysis);
        expect(firstSets.get("a")!.has(120)).toBe(true); // 'x' from b
    });

    it("cyclic rules converge to fixed point", () => {
        // a = "x" | b ; b = "y" | a ;
        const ast: AST = new Map([
            rule("a", alternation([literal("x"), nonterminal("b")])),
            rule("b", alternation([literal("y"), nonterminal("a")])),
        ]);
        const analysis = analyzeGrammar(ast);
        const { firstSets } = computeFirstSets(ast, analysis);
        // Both should have 'x' and 'y'
        expect(firstSets.get("a")!.has(120)).toBe(true); // 'x'
        expect(firstSets.get("a")!.has(121)).toBe(true); // 'y'
        expect(firstSets.get("b")!.has(120)).toBe(true); // 'x'
        expect(firstSets.get("b")!.has(121)).toBe(true); // 'y'
    });
});

// ---------------------------------------------------------------------------
// buildDispatchTable
// ---------------------------------------------------------------------------

describe("buildDispatchTable", () => {
    it("builds perfect table for disjoint alternatives", () => {
        const alts = [literal("a"), literal("b"), literal("c")];
        const ast: AST = new Map([rule("r", alternation(alts))]);
        const analysis = analyzeGrammar(ast);
        const { firstSets, nullable } = computeFirstSets(ast, analysis);

        const result = buildDispatchTable(alts, firstSets, nullable);
        expect(result).not.toBeNull();
        expect(result!.isPerfect).toBe(true);
        expect(result!.table[97]).toBe(0); // 'a' -> branch 0
        expect(result!.table[98]).toBe(1); // 'b' -> branch 1
        expect(result!.table[99]).toBe(2); // 'c' -> branch 2
    });

    it("returns null for overlapping alternatives", () => {
        // "ab" and "ac" both start with 'a'
        const alts = [literal("ab"), literal("ac")];
        const ast: AST = new Map([rule("r", alternation(alts))]);
        const analysis = analyzeGrammar(ast);
        const { firstSets, nullable } = computeFirstSets(ast, analysis);

        const result = buildDispatchTable(alts, firstSets, nullable);
        expect(result).toBeNull();
    });

    it("returns null when an alternative is nullable", () => {
        const alts = [literal("a"), epsilon()];
        const ast: AST = new Map([rule("r", alternation(alts))]);
        const analysis = analyzeGrammar(ast);
        const { firstSets, nullable } = computeFirstSets(ast, analysis);

        const result = buildDispatchTable(alts, firstSets, nullable);
        expect(result).toBeNull();
    });

    it("handles regex-based alternatives", () => {
        // /[0-9]/ and /[a-z]/ are disjoint
        const alts = [regexExpr(/[0-9]/), regexExpr(/[a-z]/)];
        const ast: AST = new Map([rule("r", alternation(alts))]);
        const analysis = analyzeGrammar(ast);
        const { firstSets, nullable } = computeFirstSets(ast, analysis);

        const result = buildDispatchTable(alts, firstSets, nullable);
        expect(result).not.toBeNull();
        expect(result!.isPerfect).toBe(true);
        expect(result!.table[48]).toBe(0); // '0' -> branch 0
        expect(result!.table[97]).toBe(1); // 'a' -> branch 1
    });
});
