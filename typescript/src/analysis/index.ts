import type { AST } from "../types.js";
import type { AnalysisCache } from "./metadata.js";
import { buildDepGraphs } from "./deps.js";
import { tarjanSCC, classifyAcyclicDeps } from "./scc.js";
import { computeRefCounts, findAliases, findTransparentAlternations } from "./metadata.js";

export * from "./deps.js";
export * from "./scc.js";
export * from "./charset.js";
export * from "./first-sets.js";
export * from "./regex-first.js";
export * from "./dispatch.js";
export * from "./metadata.js";

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
