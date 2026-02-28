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
