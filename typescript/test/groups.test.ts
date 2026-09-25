// SERVED MODEL: claude-opus-5-5
import { describe, it, expect } from "vitest";

import { BBNFToAST } from "../src/parse.js";
import { compile, createAudit } from "../src/compile.js";
import { emitGrammar, FAIL } from "../src/emit.js";

/**
 * The `groups` action kind (value.js X.P.W7 `.k2`): a rule whose body is one regex leaf with capturing
 * groups hands its action what each group captured, so a numeric token reaches its action already
 * split into number and unit (no second regex over the same text).
 */
const GRAMMAR = `dim = /([+-]?\\d+(?:\\.\\d+)?)([a-z]+)?/i ;
wrapped = ( /(\\d+)-(\\d+)?/ ) ;
list = dim , ( "," >> dim ) * ;
`;
const ast = () => BBNFToAST(GRAMMAR)[1]!;
const actions = {
    dim: { kind: "groups" as const, fn: (n: string, unit: string | undefined) => ({ n: Number(n), unit }) },
    wrapped: { kind: "groups" as const, fn: (...groups: (string | undefined)[]) => groups },
};

describe("the groups action kind", () => {
    it("hands the action the leaf's capturing groups, in order; an unmatched group is undefined", () => {
        const p = compile(ast(), { actions });
        expect(p.entries.dim("12.5DEG")).toEqual({ n: 12.5, unit: "DEG" });
        expect(p.entries.dim("-3")).toEqual({ n: -3, unit: undefined });
        expect(p.entries.dim("deg")).toBe(FAIL);
        expect(p.entries.wrapped("4-")).toEqual(["4", undefined]);
        expect(p.entries.list("1px,2,3em")).toEqual([{ n: 1, unit: "px" }, [{ n: 2, unit: undefined }, { n: 3, unit: "em" }]]);
    });

    it("runs the leaf once, with exec, and no second test over the same text", () => {
        const { body } = emitGrammar(ast(), { actionKinds: { dim: "groups" } });
        const dim = body.slice(body.indexOf("function r0_dim_v"), body.indexOf("return o;", body.indexOf("function r0_dim_v")));
        expect(dim.match(/\.exec\(s\)/g)).toHaveLength(1);
        expect(dim).not.toMatch(/\.test\(s\)/);
        expect(dim).toMatch(/V = A0\(V\[1\], V\[2\]\)/);
    });

    it("the routing audit and the recognize mode agree with the value mode", () => {
        const audit = createAudit();
        const p = compile(ast(), { actions, audit });
        for (const s of ["1px,2,3em", "x", "7-", "-1.5e", ""]) {
            const end = p.rules.list(s, 0);
            expect(p.recognize!.list(s, 0)).toBe(end);
        }
        expect(audit.violations).toBe(0);
    });

    it("refuses a groups action on a body that is not one regex leaf with a capturing group", () => {
        const [, g] = BBNFToAST(`a = /x+/ ;\nb = a , "y" ;\n`);
        expect(() => emitGrammar(g!, { actionKinds: { a: "groups" } })).toThrow(/capturing group/);
        expect(() => emitGrammar(g!, { actionKinds: { b: "groups" } })).toThrow(/one regex leaf/);
    });
});
