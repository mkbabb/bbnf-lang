// SERVED MODEL: claude-opus-5-5
//
// bulk.ts — where a class run scans bulk text and where it scans a token, decided from the grammar's
// structure alone (value.js X.P.W7 `.l2`, COHESION §0di). The emitter scans a class run in a
// bulk-text position with the regex engine (one sticky scan) and a class run in a token position with
// a code-unit loop. The decision takes no input and names no engine: one module runs the same
// everywhere; only the scan form of a leaf differs, and both forms match the same text.
//
//   · A TEXT RUN is a rule whose body is an unbounded repetition (`*`/`+`) over one class-run leaf or
//     over an ordered choice with at least one class-run arm (`[^()"']+`, `[\s;]+`): the rule scans
//     text between the opaque pieces its other arms name (strings, comments, bracketed groups).
//   · A BLOCK is a sequence (`,` `>>` `<<`) of exactly a literal, a reference to a text run, and a
//     literal: `"(" , textBody , ")"`, `"{" >> blockBody << "}"`.
//   · A text run is BULK when it spans a block, a prelude or a body:
//       – BODY: it is the middle of a block (it scans a bracketed body, and the block nests);
//       – PRELUDE / GAP: it is an element of a sequence (bare, `?`, `*`, `+`) beside an element that
//         derives a block without passing through a text run (the text before a block, or between
//         blocks: a rule's prelude, the gap between rules).
//     Every other class run is in a TOKEN position (a run inside one list item or one value).

import type { AST, Expression } from "../types.js";
import { singleClassRun } from "./regex.js";

const unwrap = (e: Expression): Expression => (e.type === "group" ? unwrap(e.value as Expression) : e);

/** A regex leaf that is a single-class run the emitter may scan (not an admit-everything class). */
function isClassRunLeaf(e: Expression): boolean {
    const u = unwrap(e);
    if (u.type !== "regex") return false;
    const run = singleClassRun(u.value as RegExp);
    return run !== null && !run.all;
}

/** The class-run leaves of a text run's body, or `null` when the body is not a text run. */
export function textRunLeaves(body: Expression): Expression[] | null {
    const b = unwrap(body);
    if (b.type !== "many" && b.type !== "many1") return null;
    const inner = unwrap(b.value as Expression);
    const arms = inner.type === "alternation" ? (inner.value as Expression[]) : [inner];
    const leaves = arms.map(unwrap).filter(isClassRunLeaf);
    return leaves.length > 0 ? leaves : null;
}

/** The elements of a sequence: a concatenation, `a >> b` and `a << b`, flattened; else `null`. */
function sequenceElements(e: Expression): Expression[] | null {
    const u = unwrap(e);
    if (u.type !== "concatenation" && u.type !== "next" && u.type !== "skip") return null;
    const out: Expression[] = [];
    for (const part of u.value as Expression[]) out.push(...(sequenceElements(part) ?? [unwrap(part)]));
    return out;
}

/** Every sub-expression of `e`, `e` included (pre-order). */
function* nodes(e: Expression): Generator<Expression> {
    yield e;
    const v = e.value as unknown;
    if (Array.isArray(v)) { for (const x of v) if (x && typeof x === "object" && "type" in x) yield* nodes(x as Expression); }
    else if (v && typeof v === "object" && "type" in (v as object)) yield* nodes(v as Expression);
}

/**
 * The rules whose class runs scan bulk text (block bodies, preludes, the gaps between blocks), from
 * the grammar's structure. Every other class run of the grammar is in a token position.
 */
export function bulkTextRuns(ast: AST): ReadonlySet<string> {
    const textRuns = new Set<string>();
    for (const [name, rule] of ast) if (textRunLeaves(rule.expression) !== null) textRuns.add(name);
    const runRef = (e: Expression): string | null => {
        let u = unwrap(e);
        while (u.type === "optional" || u.type === "many" || u.type === "many1") u = unwrap(u.value as Expression);
        return u.type === "nonterminal" && textRuns.has(u.value as string) ? (u.value as string) : null;
    };
    /** The middle text run of a block, or `null` when `e` is not a block. */
    const blockBody = (e: Expression): string | null => {
        const els = sequenceElements(e);
        if (els === null || els.length !== 3 || els[0].type !== "literal" || els[2].type !== "literal") return null;
        return els[1].type === "nonterminal" && textRuns.has(els[1].value as string) ? (els[1].value as string) : null;
    };
    // The rules that derive a block without passing through a text run: a least fixpoint over the
    // rule graph (a rule holding a block, then every rule referencing one of those, never through a
    // text run), so the answer does not depend on rule order.
    const refs = (e: Expression): string[] => [...nodes(e)].filter((n) => n.type === "nonterminal").map((n) => n.value as string);
    const holdsBlock = (e: Expression): boolean => [...nodes(e)].some((n) => blockBody(n) !== null);
    const derives = new Set<string>();
    for (const [name, rule] of ast) if (holdsBlock(rule.expression)) derives.add(name);
    for (let grew = true; grew;) {
        grew = false;
        for (const [name, rule] of ast) {
            if (derives.has(name)) continue;
            if (refs(rule.expression).some((r) => derives.has(r) && !textRuns.has(r))) { derives.add(name); grew = true; }
        }
    }
    const derivesBlock = (e: Expression): boolean => holdsBlock(e) || refs(e).some((r) => derives.has(r) && !textRuns.has(r));
    const bulk = new Set<string>();
    for (const [, rule] of ast) {
        for (const n of nodes(rule.expression)) {
            const body = blockBody(n);
            if (body !== null) bulk.add(body);
            const els = sequenceElements(n);
            if (els === null || n.type === "group") continue;
            els.forEach((el, j) => {
                const run = runRef(el);
                if (run !== null && els.some((other, k) => k !== j && derivesBlock(other))) bulk.add(run);
            });
        }
    }
    return new Set([...ast.keys()].filter((name) => bulk.has(name)));
}
