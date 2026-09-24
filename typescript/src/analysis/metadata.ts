import type { AST, Expression } from "../types.js";
import { collectDependencies } from "./deps.js";

/**
 * Count how many times each nonterminal is referenced across the AST.
 */
export function computeRefCounts(ast: AST): Map<string, number> {
    const counts = new Map<string, number>();
    for (const [name] of ast) counts.set(name, 0);

    for (const [, rule] of ast) {
        const deps = new Set<string>();
        collectDependencies(rule.expression, deps);
        for (const dep of deps) {
            counts.set(dep, (counts.get(dep) ?? 0) + 1);
        }
    }
    return counts;
}

export interface AnalysisCache {
    depGraph: Map<string, Set<string>>;
    rdepGraph: Map<string, Set<string>>;
    sccs: string[][];
    sccIndex: Map<string, number>;
    cyclicRules: Set<string>;
    topoOrder: string[];
    refCounts: Map<string, number>;
    /** Alias map: alias rule name -> target rule name. */
    aliases: Map<string, string>;
    /** Rules that are pure alternations of nonterminals (transparent). */
    transparentAlternations: Set<string>;
    /** Rules whose transitive deps have no cycles or diamonds. */
    acyclicRules: Set<string>;
    /** Rules that are NOT acyclic (cyclic or diamond deps). */
    nonAcyclicRules: Set<string>;
}

/**
 * Find rules whose expression is a bare nonterminal reference (possibly
 * wrapped in group nodes). Returns a map from alias name -> target name.
 *
 * Cyclic rules are excluded: if A = B and B = A, neither is an alias.
 */
export function findAliases(ast: AST, cyclicRules: Set<string>): Map<string, string> {
    const aliases = new Map<string, string>();

    for (const [name, rule] of ast) {
        if (cyclicRules.has(name)) continue;

        let expr: Expression = rule.expression;
        // Unwrap groups
        while (expr.type === "group") expr = expr.value as Expression;

        if (expr.type === "nonterminal") {
            const target = expr.value as string;
            if (ast.has(target)) {
                aliases.set(name, target);
            }
        }
    }

    return aliases;
}

/**
 * Find rules that are pure alternations of nonterminals.
 *
 * A rule is "transparent" if:
 *   - its body is an Alternation
 *   - every branch is a Nonterminal
 *   - the rule is cyclic (non-acyclic rules benefit from transparency in codegen)
 */
export function findTransparentAlternations(
    ast: AST,
    cyclicRules: Set<string>,
): Set<string> {
    const transparent = new Set<string>();

    for (const [name, rule] of ast) {
        // Only cyclic rules benefit from transparency.
        if (!cyclicRules.has(name)) continue;

        const expr = rule.expression;
        if (expr.type !== "alternation") continue;

        const branches = expr.value as Expression[];
        if (branches.every((b) => b.type === "nonterminal")) {
            transparent.add(name);
        }
    }

    return transparent;
}
