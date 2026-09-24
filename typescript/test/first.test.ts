// SERVED MODEL: claude-opus-5-5
import { describe, it, expect } from "vitest";
import { readFileSync } from "node:fs";

import { regexFirst, analyzeFirst, routes } from "../src/analysis/index.js";
import { grammarFromModules } from "../src/imports.js";
import type { AST, Expression } from "../src/types.js";
import { rule, literal, alternation, concatenation, regexExpr, optional, nonterminal } from "./helpers/ast-builders.js";

/** The one analysis is sound on regex flags, escapes, lookarounds, non-ASCII units and EOF. */
describe("regexFirst reads the flags (F-b-3)", () => {
    it("/i admits both cases of every ASCII letter", () => {
        const { first } = regexFirst(/none/i);
        expect(first.has(110)).toBe(true); // n
        expect(first.has(78)).toBe(true); // N
        const cls = regexFirst(/[a-c]x/i).first;
        for (const c of "abcABC") expect(cls.has(c.charCodeAt(0)), c).toBe(true);
        expect(cls.has(100)).toBe(false);
    });
    it("without /i a letter is one case", () => {
        const { first } = regexFirst(/none/);
        expect(first.has(110)).toBe(true);
        expect(first.has(78)).toBe(false);
    });
    it("/iu: K and s fold with U+212A and U+017F, so the non-ASCII bit is set", () => {
        expect(regexFirst(/k/iu).first.nonAscii).toBe(true);
        expect(regexFirst(/k/i).first.nonAscii).toBe(false);
    });
});

describe("regexFirst reads escapes, lookarounds and non-ASCII units (F-b-4)", () => {
    it("\\u and \\x escapes are the unit they name", () => {
        expect(regexFirst(/\x41/).first.has(65)).toBe(true);
        expect(regexFirst(/A/).first.has(65)).toBe(true);
        expect(regexFirst(/é/).first.nonAscii).toBe(true);
        expect(regexFirst(/[\x80-\xff]/).first.nonAscii).toBe(true);
        expect(regexFirst(/\u{1F600}/u).first.nonAscii).toBe(true);
    });
    it("a lookaround is zero-width: the unit after it starts the match", () => {
        const { first, nullable } = regexFirst(/(?=a)[a-z]+/);
        expect(first.has(98)).toBe(true); // the class, read through the lookahead
        expect(nullable).toBe(false);
        expect(regexFirst(/(?!x)/).nullable).toBe(true);
        expect(regexFirst(/(?<=a)b/).first.has(98)).toBe(true);
    });
    it("a non-ASCII literal unit, \\s, \\S, \\W and a negated class set the non-ASCII bit", () => {
        expect(regexFirst(/é/).first.nonAscii).toBe(true);
        for (const re of [/\s/, /\S/, /\W/, /[^a]/, /./]) expect(regexFirst(re).first.nonAscii, String(re)).toBe(true);
        for (const re of [/\d/, /\w/, /[a-z]/]) expect(regexFirst(re).first.nonAscii, String(re)).toBe(false);
    });
    it("nullability: *, ?, {0,n} and anchors match empty; +, {1,} do not", () => {
        for (const re of [/a*/, /a?/, /a{0,3}/, /^/, /$/, /\b/]) expect(regexFirst(re).nullable, String(re)).toBe(true);
        for (const re of [/a+/, /a{1,}/, /[a-z]/]) expect(regexFirst(re).nullable, String(re)).toBe(false);
    });
});

describe("analyzeFirst: EOF route and the nullable/eofOk fixpoint", () => {
    it("a regex leaf succeeds at end of input exactly when it can match empty (parse-that 2.x)", () => {
        const ast: AST = new Map([rule("ws", regexExpr(/\s*/)), rule("id", regexExpr(/[a-z]+/))]);
        const { ruleInfo } = analyzeFirst(ast);
        expect(ruleInfo.get("ws")).toMatchObject({ nullable: true, eofOk: true });
        expect(ruleInfo.get("id")).toMatchObject({ nullable: false, eofOk: false });
    });
    it("nullable and eofOk propagate through recursion to a fixpoint", () => {
        // a = b , "x" | c ; b = c ; c = "y" ? ;
        const ast: AST = new Map([
            rule("a", alternation([concatenation([nonterminal("b"), literal("x")]), nonterminal("c")])),
            rule("b", nonterminal("c")),
            rule("c", optional(literal("y"))),
        ]);
        const { ruleInfo } = analyzeFirst(ast);
        expect(ruleInfo.get("b")).toMatchObject({ nullable: true, eofOk: true });
        expect(ruleInfo.get("a")).toMatchObject({ nullable: true, eofOk: true });
        const a = ruleInfo.get("a")!.first;
        expect([a.has(120), a.has(121)]).toEqual([true, true]); // 'x' through the nullable b
    });
    it("the EOF route keeps only the alternatives that can succeed at end of input", () => {
        const alts = [regexExpr(/[a-z]+/), regexExpr(/\s*/), literal("z")];
        const { info } = analyzeFirst(new Map([rule("r", alternation(alts))]));
        const rt = routes(alts.map((e) => info(e)))!;
        expect(rt.groups[rt.eof]).toEqual([1]);
        expect(rt.groups[rt.na]).toEqual([1]);
        expect(rt.groups[rt.tbl[122]]).toEqual([0, 1, 2]); // 'z': in the plain choice's order
    });
    it("a rule the grammar leaves to the host is unknown: any unit, empty, end of input", () => {
        const { info } = analyzeFirst(new Map([rule("r", nonterminal("hostRule"))]));
        const f = info(nonterminal("hostRule"));
        expect([f.nullable, f.eofOk, f.first.nonAscii, f.first.has(0), f.first.has(127)]).toEqual([true, true, true, true, true]);
    });
});

/** Every regex leaf of value.js's grammar (the T-2 fixture modules). */
function grammarRegexes(): RegExp[] {
    const dir = new URL("./fixtures/value-js/", import.meta.url);
    const files = Object.fromEntries(
        ["tokens", "math", "color", "value", "stylesheet", "css"].map((n) => [`/css/${n}.bbnf`, readFileSync(new URL(`${n}.bbnf`, dir), "utf8")]),
    );
    const out: RegExp[] = [];
    const walk = (e: Expression): void => {
        if (e.type === "regex") out.push(e.value as RegExp);
        const v = e.value as unknown;
        if (Array.isArray(v)) v.forEach((x) => x && typeof x === "object" && "type" in x && walk(x as Expression));
        else if (v && typeof v === "object" && "type" in (v as object)) walk(v as Expression);
    };
    for (const [, r] of grammarFromModules(files, "/css/css.bbnf")) walk(r.expression);
    return out;
}

describe("T-4: brute force over every value.js grammar regex", () => {
    const regexes = grammarRegexes();
    // Every ASCII unit and the non-ASCII units the analysis must bit: accented, dashes, NBSP,
    // BOM, line/paragraph separators, KELVIN SIGN, LONG S, an astral pair's lead unit.
    const probes = [...Array.from({ length: 128 }, (_, c) => String.fromCharCode(c)), "é", "—", " ", "﻿", " ", " ", "K", "ſ", "😀", "ß", "ü"];
    const tails = ["", "a", "A", "(", "0", "-", " x", "abc(", "1px", "%", ")", "e", "E", "x;", ":", "{", "}", '"a"', "*/", "/", "\\", "é", "rgba(1) none from to 50%)", "--x: 1;", "@media x", "#fff", ".5e3"];

    it("finds 110 regex leaves", () => {
        expect(regexes.length).toBe(110);
    });

    it("0 misses: every unit a match can start with is in `first`; every empty match is `nullable`", () => {
        const misses: string[] = [];
        for (const re of regexes) {
            const { first, nullable } = regexFirst(re);
            const sticky = new RegExp(re.source, re.flags.replace(/[gy]/g, "") + "y");
            for (const p of probes) {
                for (const t of tails) {
                    const input = p + t;
                    sticky.lastIndex = 0;
                    if (!sticky.test(input)) continue;
                    const code = input.charCodeAt(0);
                    if (sticky.lastIndex === 0) {
                        if (!nullable) misses.push(`${re} empty on ${JSON.stringify(input)}`);
                    } else if (!(code < 128 ? first.has(code) : first.nonAscii)) {
                        misses.push(`${re} starts with ${code} on ${JSON.stringify(input)}`);
                    }
                }
            }
            // At end of input (F-p-EOF): a match there is empty, so it must be nullable.
            sticky.lastIndex = 0;
            if (sticky.test("") && !nullable) misses.push(`${re} matches at EOF`);
        }
        expect(misses).toEqual([]);
    });
});
