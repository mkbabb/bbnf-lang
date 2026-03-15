import type { AST, Expression } from "../types.js";
import type { FirstNullable } from "./first-sets.js";
import { CharSet } from "./charset.js";
import { exprFirstSet, exprIsNullable } from "./first-sets.js";

// --- FIRST set conflict detection ---

/**
 * A FIRST set conflict between two branches of an alternation.
 */
export interface FirstSetConflict {
    /** 0-based index of the first conflicting branch. */
    branchA: number;
    /** 0-based index of the second conflicting branch. */
    branchB: number;
    /** The overlapping characters. */
    overlap: CharSet;
}

/**
 * Find FIRST set conflicts in alternation rules.
 *
 * For each rule whose expression is an alternation, computes per-branch FIRST
 * sets and checks for pairwise overlap. Returns a map from rule name to the
 * list of conflicts found.
 */
export function findFirstSetConflicts(
    ast: AST,
    firstNullable: FirstNullable,
): Map<string, FirstSetConflict[]> {
    const conflicts = new Map<string, FirstSetConflict[]>();

    for (const [name, rule] of ast) {
        const expr = rule.expression;
        if (expr.type !== "alternation") continue;

        const branches = expr.value as Expression[];
        if (branches.length < 2) continue;

        // Short-circuit: if rule-level FIRST set has <=1 character, trivially no conflict.
        const ruleFirst = firstNullable.firstSets.get(name);
        if (ruleFirst && ruleFirst.len() <= 1) continue;

        // Compute per-branch FIRST sets.
        const branchFirsts = branches.map((branch) =>
            exprFirstSet(branch, firstNullable.firstSets, firstNullable.nullable, ast),
        );

        // Running union optimization: check against union of prior branches.
        const ruleConflicts: FirstSetConflict[] = [];
        const runningUnion = new CharSet();

        for (let i = 0; i < branchFirsts.length; i++) {
            if (i > 0 && branchFirsts[i].isDisjoint(runningUnion)) {
                // Disjoint with all prior branches -- no conflicts possible.
                runningUnion.union(branchFirsts[i]);
                continue;
            }

            for (let j = 0; j < i; j++) {
                const overlap = branchFirsts[i].intersection(branchFirsts[j]);
                if (!overlap.isEmpty()) {
                    ruleConflicts.push({
                        branchA: j,
                        branchB: i,
                        overlap,
                    });
                }
            }
            runningUnion.union(branchFirsts[i]);
        }

        if (ruleConflicts.length > 0) {
            conflicts.set(name, ruleConflicts);
        }
    }

    return conflicts;
}

// --- Dispatch table ---

export interface DispatchTable {
    table: Int8Array; // 128 entries: charCode -> alternative index, -1 = no match
    isPerfect: boolean; // true if all alternatives are covered disjointly
}

/**
 * Build a dispatch table for an alternation node. Returns null if any
 * alternative is nullable or has an empty/unknown FIRST set.
 */
export function buildDispatchTable(
    alternatives: Expression[],
    firstSets: Map<string, CharSet>,
    nullable: Map<string, boolean>,
): DispatchTable | null {
    const altFirstSets: CharSet[] = [];

    for (const alt of alternatives) {
        // Reject if any alternative is nullable
        if (exprIsNullable(alt, nullable, new Map())) return null;

        const cs = exprFirstSet(alt, firstSets, nullable, new Map());
        if (cs.isEmpty()) return null;
        altFirstSets.push(cs);
    }

    // Check pairwise disjointness
    for (let i = 0; i < altFirstSets.length; i++) {
        for (let j = i + 1; j < altFirstSets.length; j++) {
            if (!altFirstSets[i].isDisjoint(altFirstSets[j])) {
                return null;
            }
        }
    }

    // Build table
    const table = new Int8Array(128).fill(-1);
    for (let i = 0; i < altFirstSets.length; i++) {
        for (let ch = 0; ch < 128; ch++) {
            if (altFirstSets[i].has(ch)) {
                table[ch] = i;
            }
        }
    }

    return { table, isPerfect: true };
}

// --- Partial dispatch ---

/**
 * Partial dispatch result: groups of alternatives that share first-byte overlap,
 * plus a fallback group for nullable/empty-FIRST alternatives.
 */
export interface PartialDispatchTable {
    /** charCode → group index, -1 = no dispatchable match (try fallback). */
    table: Int8Array;
    /** Group index → alternative indices. Single-element = direct dispatch. */
    groups: number[][];
    /** Alternative indices not dispatchable (nullable or empty FIRST set). */
    fallbackIndices: number[];
}

/**
 * Build a partial dispatch table for alternations where perfect dispatch fails.
 *
 * Uses union-find to merge alternatives that share any first-byte overlap into
 * groups. Each group gets a dispatch slot — single-element groups dispatch O(1),
 * multi-element groups fall back to sequential trial within the group.
 *
 * Returns null when partial dispatch doesn't improve over sequential trial
 * (fewer than 2 dispatchable groups, or all alternatives collide).
 */
export function buildPartialDispatchTable(
    alternatives: Expression[],
    firstSets: Map<string, CharSet>,
    nullable: Map<string, boolean>,
): PartialDispatchTable | null {
    const n = alternatives.length;
    if (n < 3) return null; // Need at least 3 alternatives for partial to beat any()

    const altFirstSets: (CharSet | null)[] = [];
    const fallbackIndices: number[] = [];
    const dispatchableIndices: number[] = [];

    // Separate nullable/empty-FIRST alternatives from dispatchable ones.
    for (let i = 0; i < n; i++) {
        if (exprIsNullable(alternatives[i], nullable, new Map())) {
            altFirstSets.push(null);
            fallbackIndices.push(i);
        } else {
            const cs = exprFirstSet(alternatives[i], firstSets, nullable, new Map());
            if (cs.isEmpty()) {
                altFirstSets.push(null);
                fallbackIndices.push(i);
            } else {
                altFirstSets.push(cs);
                dispatchableIndices.push(i);
            }
        }
    }

    if (dispatchableIndices.length < 2) return null;

    // Union-find: merge alternatives that share any character.
    const parent = new Int32Array(n);
    for (let i = 0; i < n; i++) parent[i] = i;

    function find(x: number): number {
        while (parent[x] !== x) {
            parent[x] = parent[parent[x]];
            x = parent[x];
        }
        return x;
    }
    function merge(a: number, b: number) {
        parent[find(a)] = find(b);
    }

    for (let ch = 0; ch < 128; ch++) {
        let first = -1;
        for (const i of dispatchableIndices) {
            if (altFirstSets[i]!.has(ch)) {
                if (first >= 0) merge(first, i);
                else first = i;
            }
        }
    }

    // Collect groups from union-find components.
    const groupMap = new Map<number, number[]>();
    for (const i of dispatchableIndices) {
        const root = find(i);
        if (!groupMap.has(root)) groupMap.set(root, []);
        groupMap.get(root)!.push(i);
    }

    const groups = [...groupMap.values()];

    // Partial dispatch only helps when there are at least 2 distinct groups.
    if (groups.length <= 1) return null;

    // Build alt-to-group lookup.
    const altToGroup = new Int8Array(n).fill(-1);
    for (let g = 0; g < groups.length; g++) {
        for (const i of groups[g]) {
            altToGroup[i] = g;
        }
    }

    // Build table: charCode → group index.
    const table = new Int8Array(128).fill(-1);
    for (let ch = 0; ch < 128; ch++) {
        for (const i of dispatchableIndices) {
            if (altFirstSets[i]!.has(ch)) {
                table[ch] = altToGroup[i];
                break; // All alternatives sharing this char are in the same group.
            }
        }
    }

    return { table, groups, fallbackIndices };
}
