import type { AST, Expression } from "./types.js";

/**
 * Recursively collect all nonterminal references from an expression.
 */
export function collectDependencies(expr: Expression, deps: Set<string>): void {
    if (!expr?.type) return;
    if (expr.type === "nonterminal") {
        deps.add(expr.value as string);
        return;
    }
    if (expr.value instanceof Array) {
        for (const child of expr.value) {
            collectDependencies(child as Expression, deps);
        }
    } else if (
        expr.value &&
        typeof expr.value === "object" &&
        "type" in (expr.value as object)
    ) {
        collectDependencies(expr.value as Expression, deps);
    }
}

/**
 * Build forward and reverse dependency graphs for the entire AST.
 */
export function buildDepGraphs(ast: AST) {
    const depGraph = new Map<string, Set<string>>();
    const rdepGraph = new Map<string, Set<string>>();

    for (const [name] of ast) {
        depGraph.set(name, new Set());
        rdepGraph.set(name, new Set());
    }

    for (const [name, rule] of ast) {
        const deps = new Set<string>();
        collectDependencies(rule.expression, deps);
        depGraph.set(name, deps);
        for (const dep of deps) {
            if (!rdepGraph.has(dep)) rdepGraph.set(dep, new Set());
            rdepGraph.get(dep)!.add(name);
        }
    }

    return { depGraph, rdepGraph };
}

export interface SCCResult {
    sccs: string[][];
    sccIndex: Map<string, number>;
    cyclicRules: Set<string>;
}

/**
 * Tarjan's SCC algorithm. Returns SCCs in reverse topological order
 * (leaf SCCs first — the order we want for bottom-up construction).
 */
export function tarjanSCC(depGraph: Map<string, Set<string>>): SCCResult {
    let index = 0;
    const stack: string[] = [];
    const onStack = new Set<string>();
    const indices = new Map<string, number>();
    const lowlinks = new Map<string, number>();
    const sccs: string[][] = [];

    function strongconnect(v: string) {
        indices.set(v, index);
        lowlinks.set(v, index);
        index++;
        stack.push(v);
        onStack.add(v);

        for (const w of depGraph.get(v) ?? []) {
            if (!depGraph.has(w)) continue; // skip external refs
            if (!indices.has(w)) {
                strongconnect(w);
                lowlinks.set(
                    v,
                    Math.min(lowlinks.get(v)!, lowlinks.get(w)!),
                );
            } else if (onStack.has(w)) {
                lowlinks.set(
                    v,
                    Math.min(lowlinks.get(v)!, indices.get(w)!),
                );
            }
        }

        if (lowlinks.get(v) === indices.get(v)) {
            const scc: string[] = [];
            let w: string;
            do {
                w = stack.pop()!;
                onStack.delete(w);
                scc.push(w);
            } while (w !== v);
            sccs.push(scc);
        }
    }

    for (const v of depGraph.keys()) {
        if (!indices.has(v)) strongconnect(v);
    }

    // Build sccIndex and cyclicRules
    const sccIndex = new Map<string, number>();
    const cyclicRules = new Set<string>();

    for (let i = 0; i < sccs.length; i++) {
        const scc = sccs[i];
        for (const name of scc) {
            sccIndex.set(name, i);
        }
        if (scc.length > 1) {
            for (const name of scc) cyclicRules.add(name);
        } else {
            // Self-referencing single-node SCC
            const name = scc[0];
            if (depGraph.get(name)?.has(name)) {
                cyclicRules.add(name);
            }
        }
    }

    return { sccs, sccIndex, cyclicRules };
}

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
    /** Alias map: alias rule name → target rule name. */
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
 * wrapped in group nodes). Returns a map from alias name → target name.
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

/**
 * Classify rules as acyclic or non-acyclic based on their transitive
 * dependency subgraph.
 *
 * A rule is "non-acyclic" if ANY of:
 *   1. It is in a cyclic SCC
 *   2. It transitively depends on a cyclic SCC
 *   3. Its dependency subgraph has diamond (convergent) paths
 *
 * Replicates the Rust `calculate_acyclic_deps_scc` semantics.
 */
export function classifyAcyclicDeps(
    depGraph: Map<string, Set<string>>,
): { acyclic: Set<string>; nonAcyclic: Set<string> } {
    const acyclic = new Set<string>();
    const nonAcyclic = new Set<string>();

    for (const name of depGraph.keys()) {
        const visited = new Set<string>();
        if (isAcyclicDfs(name, depGraph, visited)) {
            acyclic.add(name);
        } else {
            nonAcyclic.add(name);
        }
    }

    return { acyclic, nonAcyclic };
}

function isAcyclicDfs(
    name: string,
    depGraph: Map<string, Set<string>>,
    visited: Set<string>,
): boolean {
    if (visited.has(name)) return false; // cycle or diamond
    visited.add(name);

    const deps = depGraph.get(name);
    if (deps) {
        for (const dep of deps) {
            if (!depGraph.has(dep)) continue; // external ref
            if (!isAcyclicDfs(dep, depGraph, visited)) return false;
        }
    }
    return true;
}

/**
 * Full grammar analysis: dep graphs, Tarjan's SCC, topological order, ref counts,
 * aliases, transparent alternations, and acyclic dependency classification.
 */
export function analyzeGrammar(ast: AST): AnalysisCache {
    const { depGraph, rdepGraph } = buildDepGraphs(ast);
    const { sccs, sccIndex, cyclicRules } = tarjanSCC(depGraph);

    // Tarjan's yields SCCs in reverse topological order (leaves first).
    // Flatten to get rule-level topological order.
    const topoOrder: string[] = [];
    for (const scc of sccs) {
        for (const name of scc) {
            topoOrder.push(name);
        }
    }

    const refCounts = computeRefCounts(ast);
    const aliases = findAliases(ast, cyclicRules);
    const transparentAlternations = findTransparentAlternations(ast, cyclicRules);
    const { acyclic: acyclicRules, nonAcyclic: nonAcyclicRules } =
        classifyAcyclicDeps(depGraph);

    return {
        depGraph,
        rdepGraph,
        sccs,
        sccIndex,
        cyclicRules,
        topoOrder,
        refCounts,
        aliases,
        transparentAlternations,
        acyclicRules,
        nonAcyclicRules,
    };
}
