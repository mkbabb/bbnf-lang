export interface SCCResult {
    sccs: string[][];
    sccIndex: Map<string, number>;
    cyclicRules: Set<string>;
}

/**
 * Tarjan's SCC algorithm. Returns SCCs in reverse topological order
 * (leaf SCCs first -- the order we want for bottom-up construction).
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
