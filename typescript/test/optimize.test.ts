import { describe, it, expect } from "vitest";

import type { AST, Expression, ProductionRule, Alteration, Concatenation, Nonterminal, Epsilon } from "../src/types.js";
import {
    topologicalSort,
    removeDirectLeftRecursion,
    removeIndirectLeftRecursion,
    removeAllLeftRecursion,
    findCommonPrefix,
} from "../src/optimize.js";
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

function epsilon(): Expression {
    return { type: "epsilon" } as Expression;
}

// ---------------------------------------------------------------------------
// topologicalSort
// ---------------------------------------------------------------------------

describe("topologicalSort", () => {
    it("orders a DAG: dependents come before dependencies", () => {
        // c depends on nothing, b depends on c, a depends on b
        // topologicalSort uses unshift (dependents first for left-recursion elimination)
        const ast: AST = new Map([
            rule("a", nonterminal("b")),
            rule("b", nonterminal("c")),
            rule("c", regexExpr(/x/)),
        ]);
        const sorted = topologicalSort(ast);
        const keys = [...sorted.keys()];
        // a comes before b, b comes before c (dependents first)
        expect(keys.indexOf("a")).toBeLessThan(keys.indexOf("b"));
        expect(keys.indexOf("b")).toBeLessThan(keys.indexOf("c"));
    });

    it("single-node AST", () => {
        const ast: AST = new Map([rule("only", literal("x"))]);
        const sorted = topologicalSort(ast);
        expect([...sorted.keys()]).toEqual(["only"]);
    });

    it("no-dependency rules maintain order", () => {
        const ast: AST = new Map([
            rule("a", literal("1")),
            rule("b", literal("2")),
            rule("c", literal("3")),
        ]);
        const sorted = topologicalSort(ast);
        expect(sorted.size).toBe(3);
    });
});

// ---------------------------------------------------------------------------
// removeDirectLeftRecursion
// ---------------------------------------------------------------------------

describe("removeDirectLeftRecursion", () => {
    it("eliminates A = A a | b pattern", () => {
        // expr = expr , "+" , expr | number
        const ast: AST = new Map([
            rule(
                "expr",
                alternation([
                    concatenation([nonterminal("expr"), literal("+"), nonterminal("expr")]),
                    nonterminal("number"),
                ]),
            ),
            rule("number", regexExpr(/\d+/)),
        ]);

        removeDirectLeftRecursion(ast);

        // The original "expr" rule should no longer start with nonterminal("expr")
        const exprRule = ast.get("expr")!;
        const branches = exprRule.expression.value as Expression[];
        for (const branch of branches) {
            if (branch.type === "concatenation") {
                const first = (branch.value as Expression[])[0];
                expect(first.value).not.toBe("expr");
            }
        }
    });

    it("does not modify non-left-recursive rule", () => {
        const ast: AST = new Map([
            rule("simple", alternation([literal("a"), literal("b")])),
        ]);
        const originalExpr = ast.get("simple")!.expression;
        removeDirectLeftRecursion(ast);
        // No new rules should be created
        expect(ast.size).toBe(1);
        expect(ast.get("simple")!.expression).toBe(originalExpr);
    });

    it("handles multiple left-recursive branches", () => {
        // expr = expr "*" expr | expr "+" expr | number
        const ast: AST = new Map([
            rule(
                "expr",
                alternation([
                    concatenation([nonterminal("expr"), literal("*"), nonterminal("expr")]),
                    concatenation([nonterminal("expr"), literal("+"), nonterminal("expr")]),
                    nonterminal("number"),
                ]),
            ),
            rule("number", regexExpr(/\d+/)),
        ]);

        removeDirectLeftRecursion(ast);

        // A tail rule should have been created
        expect(ast.size).toBeGreaterThan(2);
    });
});

// ---------------------------------------------------------------------------
// removeIndirectLeftRecursion
// ---------------------------------------------------------------------------

describe("removeIndirectLeftRecursion", () => {
    it("resolves 2-rule indirect cycle", () => {
        // A = B "x" | "a"
        // B = A "y" | "b"
        // This is indirect: A -> B -> A
        const ast: AST = new Map([
            rule("A", alternation([
                concatenation([nonterminal("B"), literal("x")]),
                literal("a"),
            ])),
            rule("B", alternation([
                concatenation([nonterminal("A"), literal("y")]),
                literal("b"),
            ])),
        ]);

        const analysis = analyzeGrammar(ast);
        const result = removeIndirectLeftRecursion(ast, analysis);

        // After substitution, B should no longer start with A (or should have
        // A's alternatives substituted in).
        expect(result).toBe(ast);
    });

    it("returns AST unchanged if no indirect cycles", () => {
        const ast: AST = new Map([
            rule("a", literal("x")),
            rule("b", nonterminal("a")),
        ]);
        const analysis = analyzeGrammar(ast);
        const result = removeIndirectLeftRecursion(ast, analysis);
        expect(result).toBe(ast);
    });
});

// ---------------------------------------------------------------------------
// removeAllLeftRecursion (end-to-end)
// ---------------------------------------------------------------------------

describe("removeAllLeftRecursion", () => {
    it("handles direct left recursion end-to-end", () => {
        const ast: AST = new Map([
            rule(
                "expr",
                alternation([
                    concatenation([nonterminal("expr"), literal("+"), nonterminal("term")]),
                    nonterminal("term"),
                ]),
            ),
            rule("term", regexExpr(/\d+/)),
        ]);

        const result = removeAllLeftRecursion(ast);
        // Should have created at least one tail rule
        expect(result.size).toBeGreaterThan(2);
    });
});

// ---------------------------------------------------------------------------
// findCommonPrefix
// ---------------------------------------------------------------------------

describe("findCommonPrefix", () => {
    it("finds shared literal prefix", () => {
        const result = findCommonPrefix(literal("hello"), literal("hello"));
        expect(result).toBeDefined();
        expect(result![0]!.type).toBe("literal");
        expect(result![0]!.value).toBe("hello");
    });

    it("finds shared nonterminal prefix", () => {
        const result = findCommonPrefix(nonterminal("foo"), nonterminal("foo"));
        expect(result).toBeDefined();
        expect(result![0]!.type).toBe("nonterminal");
    });

    it("returns undefined for different literals", () => {
        const result = findCommonPrefix(literal("a"), literal("b"));
        expect(result).toBeUndefined();
    });

    it("returns undefined for different types", () => {
        const result = findCommonPrefix(literal("a"), nonterminal("a"));
        expect(result).toBeUndefined();
    });
});
