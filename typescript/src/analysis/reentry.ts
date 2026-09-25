// SERVED MODEL: claude-opus-5-5
//
// reentry.ts — which rules a failing alternative leaves to be scanned again at the same offset,
// decided from the grammar's structure alone (value.js X.P.W7 `.l3`, COHESION §0dq: a refusal
// re-scans nothing it has already classified).
//
//   · The LEAD of an expression is the set of rules it calls at its own start offset: a reference;
//     the first element of a sequence (`,` `>>` `<<`); every arm of a choice; the body of `?` `*` `+`;
//     both sides of `a - b` (the excluded side is tried first, at the same offset). Closed over the
//     rule graph (a rule's lead includes its body's lead).
//   · A RETRY PAIR is two expressions the parser tries at the same offset one after the other, the
//     second because the first failed (or stopped): two arms of a choice; the body of a `?`/`*`/`+`
//     element of a sequence and the element after it (the last, failed iteration starts where the
//     next element does); `a - b`'s `b` and `a`.
//   · A rule is RE-ENTERED when it is a TEXT RUN (`bulk.ts`: an unbounded repetition over class runs,
//     the only scans whose length the grammar does not bound), it is in the lead of both sides of a
//     retry pair, and no other re-entered rule already leads to it (its caller's answer covers it).
//     A token rule (one regex leaf, a keyword, a number) is not kept: its first unit is tested before
//     it scans, it reads one token, and keeping its answer would cost a store on every call.
// The emitter keeps a re-entered rule's last recognize answer (end offset, keyed by the input and the
// offset; a parse that trips the depth fault never stores one), so the second side reads the first
// side's scan instead of repeating it. Rules are pure, so the answer at an offset is the answer.

import type { AST, Expression } from "../types.js";
import { textRunLeaves } from "./bulk.js";

const unwrap = (e: Expression): Expression => (e.type === "group" ? unwrap(e.value as Expression) : e);

/** The rules `e` calls directly at its own start offset. */
function leadRefs(e: Expression): string[] {
    const u = unwrap(e);
    switch (u.type) {
        case "nonterminal": return [u.value as string];
        case "concatenation": case "next": case "skip": return leadRefs((u.value as Expression[])[0]);
        case "alternation": return (u.value as Expression[]).flatMap(leadRefs);
        case "optional": case "many": case "many1": return leadRefs(u.value as Expression);
        case "minus": return (u.value as Expression[]).flatMap(leadRefs);
        default: return [];
    }
}

/** Every sub-expression of `e`, `e` included (pre-order). */
function* nodes(e: Expression): Generator<Expression> {
    yield e;
    const v = e.value as unknown;
    if (Array.isArray(v)) { for (const x of v) if (x && typeof x === "object" && "type" in x) yield* nodes(x as Expression); }
    else if (v && typeof v === "object" && "type" in (v as object)) yield* nodes(v as Expression);
}

/** The pairs of expressions tried at one offset, the second after the first failed or stopped. */
function retryPairs(e: Expression): [Expression, Expression][] {
    const out: [Expression, Expression][] = [];
    for (const n of nodes(e)) {
        if (n.type === "alternation") {
            const arms = n.value as Expression[];
            for (let j = 0; j < arms.length; j++) for (let k = j + 1; k < arms.length; k++) out.push([arms[j], arms[k]]);
        } else if (n.type === "concatenation") {
            const parts = (n.value as Expression[]).map(unwrap);
            for (let k = 0; k + 1 < parts.length; k++) {
                const p = parts[k];
                if (p.type === "optional" || p.type === "many" || p.type === "many1") out.push([p.value as Expression, parts[k + 1]]);
            }
        } else if (n.type === "minus") {
            const [a, b] = n.value as Expression[];
            out.push([b, a]);
        }
    }
    return out;
}

/** The re-entered rules of `ast` (see the header), in rule order. */
export function reenteredRules(ast: AST): readonly string[] {
    const closure = new Map<string, Set<string>>();
    const leadOf = (e: Expression): Set<string> => {
        const out = new Set<string>(), stack = leadRefs(e).filter((r) => ast.has(r));
        while (stack.length > 0) {
            const r = stack.pop()!;
            if (out.has(r)) continue;
            out.add(r);
            for (const d of leadRefs(ast.get(r)!.expression)) if (ast.has(d) && !out.has(d)) stack.push(d);
        }
        return out;
    };
    for (const [name, rule] of ast) closure.set(name, leadOf(rule.expression));
    const marked = new Set<string>();
    for (const [, rule] of ast) {
        for (const [x, y] of retryPairs(rule.expression)) {
            const lx = leadOf(x), ly = leadOf(y);
            for (const r of lx) if (ly.has(r) && textRunLeaves(ast.get(r)!.expression) !== null) marked.add(r);
        }
    }
    // A rule another re-entered rule leads to is covered by that rule's kept answer.
    const covered = (r: string) => [...marked].some((m) => m !== r && closure.get(m)!.has(r) && !closure.get(r)!.has(m));
    return [...ast.keys()].filter((r) => marked.has(r) && !covered(r));
}
