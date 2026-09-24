// SERVED MODEL: claude-opus-5-5
import { describe, it, expect } from "vitest";

import { BBNFToAST } from "../src/parse.js";
import { compile } from "../src/compile.js";
import { emitGrammar, FAIL } from "../src/emit.js";

/**
 * The `instrument` build flag (value.js X.P.W7 `.l`): an evidence build carries a per-rule profile
 * (calls, failures, inclusive time, discarded action values, action calls and time, and per entry its
 * calls, refusals and time); a shipping build carries none of its code.
 */
const GRAMMAR = `num = /\\d+/ ;
pair = num , "," , num ;
item = pair | num ;
list = item , ( ";" >> item ) * ;
`;
const ast = () => BBNFToAST(GRAMMAR)[1]!;
const actionKinds = { num: "map", pair: "map" } as const;
const actions = {
    num: { kind: "map" as const, fn: (t: string) => Number(t) },
    pair: { kind: "map" as const, fn: (v: unknown[]) => [v[0], v[2]] },
};

type Profile = {
    slots: string[]; calls: number[]; fails: number[]; time: number[]; discarded: number[];
    actions: string[]; actionCalls: number[]; actionTime: number[];
    entries: string[]; entryCalls: number[]; entryFails: number[]; entryTime: number[];
};
function instrumented() {
    const e = emitGrammar(ast(), { actionKinds, instrument: true });
    const make = new Function("A", "H", "ACTION_KINDS", "FAIL", "AUDIT", e.body) as (...a: unknown[]) => {
        entries: Record<string, (s: string) => unknown>; profile: () => Profile; resetProfile: () => void;
    };
    return make(actions, {}, e.actionKinds, FAIL, undefined);
}

describe("the instrument build flag", () => {
    it("is compiled out of a shipping build", () => {
        const { module } = emitGrammar(ast(), { actionKinds });
        for (const token of ["PC[", "NOW(", "ACT++", "DISC", "instrumented", "profile"]) expect(module).not.toContain(token);
    });

    it("answers exactly what the shipping build answers", () => {
        const p = instrumented(), q = compile(ast(), { actions });
        for (const s of ["1;2,3;4", "1,", "", "7", "1;;2"]) expect(p.entries.list(s)).toEqual(q.entries.list(s));
    });

    it("counts calls, failures and the action values a failing function discards", () => {
        const p = instrumented();
        expect(p.entries.item("5")).toBe(5);
        const r = p.profile();
        const at = (label: string) => r.slots.indexOf(label);
        // `item` tries `pair` first: `num` succeeds (its action runs), "," fails, so `pair` fails and
        // discards that one value; then `num` matches again.
        expect(r.calls[at("item/v")]).toBe(1);
        expect(r.calls[at("pair/v")]).toBe(1);
        expect(r.fails[at("pair/v")]).toBe(1);
        expect(r.discarded[at("pair/v")]).toBe(1);
        expect(r.calls[at("num/v")]).toBe(2);
        expect(r.actionCalls[r.actions.indexOf("num")]).toBe(2);
        expect(r.actionCalls[r.actions.indexOf("pair")]).toBe(0);
        expect(r.time.every((t) => t >= 0)).toBe(true);
        expect(p.entries.item("x")).toBe(FAIL);
        const e = p.profile(), item = e.entries.indexOf("item");
        expect([e.entryCalls[item], e.entryFails[item]]).toEqual([2, 1]);
        p.resetProfile();
        expect(p.profile().calls.every((c) => c === 0)).toBe(true);
    });
});
