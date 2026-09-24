// SERVED MODEL: claude-opus-5-5
//
// analysis/first.ts — the one grammar analysis every face compiles from: each node's SOUND
// first-unit set, whether it can succeed without consuming (`nullable`), and whether it can succeed
// at end of input (`eofOk`) — to a fixpoint over recursive rules. And the ROUTING of an ordered
// choice: for each first unit (ASCII, any non-ASCII unit, end of input), the ordered sub-choice of
// the alternatives that can start there.
// Soundness is the contract: `first` is a superset of the units a match can start with, `nullable`
// and `eofOk` are true whenever the empty match or the match at end of input is possible, so a
// routed choice answers exactly what the plain ordered choice answers.
// Provenance: value.js X.P.W7 research, route-ts-compiler `src/analysis.ts` (the ratified analysis),
// with a regex leaf's `eofOk` read under parse-that 2.x (F-p-EOF: a regex runs at end of input
// exactly as mid-input) and `?w` (optionalWhitespace) read as parse-that's `trim`.

import type { Expression, AST } from "../types.js";
import type { CharSet } from "./regex.js";
import { emptySet, fullSet, regexFirst, unionInto } from "./regex.js";

export type Info = { first: CharSet; nullable: boolean; eofOk: boolean };
export type FirstAnalysis = Readonly<{
    info: (e: Expression) => Info;
    nodeInfo: Map<Expression, Info>;
    ruleInfo: Map<string, Info>;
}>;

/** The analysis of `ast`: per-node facts (`info`) and per-rule facts (`ruleInfo`), settled. */
export function analyzeFirst(ast: AST): FirstAnalysis {
    const ruleInfo = new Map<string, Info>();
    for (const name of ast.keys()) ruleInfo.set(name, { first: emptySet(), nullable: false, eofOk: false });
    const nodeInfo = new Map<Expression, Info>();
    let settled = false; // after the fixpoint, a node's facts are final: answer them from the table
    function info(e: Expression): Info {
        if (settled) { const hit = nodeInfo.get(e); if (hit !== undefined) return hit; }
        const out: Info = { first: emptySet(), nullable: false, eofOk: false };
        switch (e.type) {
            case "literal": {
                const s = e.value as string;
                if (s.length === 0) { out.nullable = true; out.eofOk = true; break; }
                const c = s.charCodeAt(0);
                if (c < 128) out.first.add(c); else out.first.nonAscii = true;
                break;
            }
            case "regex": {
                const r = regexFirst(e.value as RegExp);
                unionInto(out.first, r.first);
                out.nullable = r.nullable;
                out.eofOk = r.nullable; // parse-that 2.x: at end of input only an empty match succeeds
                break;
            }
            case "nonterminal": {
                const r = ruleInfo.get(e.value as string);
                if (!r) {
                    // A rule the grammar does not define is the host's to supply: nothing is
                    // known of it, so it may start with any unit, match empty, or end the input.
                    unionInto(out.first, fullSet()); out.nullable = true; out.eofOk = true;
                    break;
                }
                unionInto(out.first, r.first); out.nullable = r.nullable; out.eofOk = r.eofOk;
                break;
            }
            case "group": return info(e.value as Expression);
            case "optionalWhitespace": {
                // parse-that `trim`: ASCII whitespace (9–13, 32) skipped before and after the inner
                // parser, which alone decides success.
                const r = info(e.value as unknown as Expression);
                unionInto(out.first, r.first);
                out.first.add(32); out.first.addRange(9, 13);
                out.nullable = r.nullable; out.eofOk = r.eofOk;
                break;
            }
            case "optional": case "many": {
                const r = info(e.value as Expression);
                unionInto(out.first, r.first); out.nullable = true; out.eofOk = true;
                break;
            }
            case "many1": {
                const r = info(e.value as Expression);
                unionInto(out.first, r.first); // a zero-width match never counts toward `+`
                break;
            }
            case "epsilon": out.nullable = true; out.eofOk = true; break;
            case "minus": return info((e.value as Expression[])[0]);
            case "skip": case "next": case "concatenation": {
                const parts = e.value as Expression[];
                out.nullable = true; out.eofOk = true;
                for (const p of parts) {
                    const r = info(p);
                    if (out.nullable) unionInto(out.first, r.first);
                    out.nullable &&= r.nullable;
                    out.eofOk &&= r.eofOk;
                }
                break;
            }
            case "alternation": {
                for (const a of e.value as Expression[]) {
                    const r = info(a);
                    unionInto(out.first, r.first); out.nullable ||= r.nullable; out.eofOk ||= r.eofOk;
                }
                break;
            }
            default: throw new Error(`unsupported BBNF node \`${(e as Expression).type}\``);
        }
        nodeInfo.set(e, out);
        return out;
    }
    for (let changed = true; changed;) {
        changed = false;
        for (const [name, rule] of ast) {
            const r = info(rule.expression), cur = ruleInfo.get(name)!;
            if (unionInto(cur.first, r.first)) changed = true;
            if (r.nullable && !cur.nullable) { cur.nullable = true; changed = true; }
            if (r.eofOk && !cur.eofOk) { cur.eofOk = true; changed = true; }
        }
    }
    settled = true;
    return { info, nodeInfo, ruleInfo };
}

export type Routes = Readonly<{ tbl: Int16Array; groups: number[][]; na: number; eof: number }>;

/**
 * The routing of an ordered choice by its first unit; `null` when every route is the whole choice
 * (routing would buy nothing). `tbl[c]` routes ASCII unit `c`, `na` every non-ASCII unit, `eof`
 * the end of input; a group is an ascending list of alternative indices, `-1` none.
 */
export function routes(infos: Info[]): Routes | null {
    const groupKey = new Map<string, number>();
    const groups: number[][] = [];
    const groupOf = (members: number[]): number => {
        if (members.length === 0) return -1;
        const key = members.join(",");
        let g = groupKey.get(key);
        if (g === undefined) { g = groups.length; groupKey.set(key, g); groups.push(members); }
        return g;
    };
    const tbl = new Int16Array(128);
    for (let c = 0; c < 128; c++) tbl[c] = groupOf(infos.flatMap((x, m) => (x.nullable || x.first.ascii[c] ? [m] : [])));
    const na = groupOf(infos.flatMap((x, m) => (x.nullable || x.first.nonAscii ? [m] : [])));
    const eof = groupOf(infos.flatMap((x, m) => (x.eofOk ? [m] : [])));
    if (groups.length === 1 && groups[0].length === infos.length) return null;
    return { tbl, groups, na, eof };
}

/** Two alternatives of one choice whose first-unit sets overlap (`overlap` = the shared units). */
export interface FirstSetConflict {
    branchA: number;
    branchB: number;
    overlap: CharSet;
}

/** Every pair of alternatives, per top-level choice rule, that can start on the same unit. */
export function findFirstSetConflicts(
    ast: AST,
    analysis: FirstAnalysis = analyzeFirst(ast),
): Map<string, FirstSetConflict[]> {
    const conflicts = new Map<string, FirstSetConflict[]>();
    for (const [name, rule] of ast) {
        if (rule.expression.type !== "alternation") continue;
        const firsts = (rule.expression.value as Expression[]).map((b) => analysis.info(b).first);
        const found: FirstSetConflict[] = [];
        for (let a = 0; a < firsts.length; a++) {
            for (let b = a + 1; b < firsts.length; b++) {
                const overlap = firsts[a].intersection(firsts[b]);
                if (!overlap.isEmpty()) found.push({ branchA: a, branchB: b, overlap });
            }
        }
        if (found.length > 0) conflicts.set(name, found);
    }
    return conflicts;
}
