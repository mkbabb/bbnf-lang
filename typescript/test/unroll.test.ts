// SERVED MODEL: claude-opus-5-5
import { describe, it, expect } from "vitest";

import { BBNFToAST } from "../src/parse.js";
import { compile, createAudit } from "../src/compile.js";
import { emitGrammar } from "../src/emit.js";
import { unrollEscapeRuns } from "../src/analysis/regex.js";

/**
 * The escape run, unrolled (value.js X.P.W7 `.l`): a string literal's body `(?:\\[\s\S]|[^"\\])*` is
 * emitted as `[^"\\]*(?:\\[\s\S][^"\\]*)*`, the same match with no per-iteration alternation (the
 * spelling JavaScriptCore backtracks through slowly when a quote is never closed).
 */
const STRING = String.raw`"(?:\\[\s\S]|[^"\\])*"|'(?:\\[\s\S]|[^'\\])*'`;

describe("unrollEscapeRuns", () => {
    it("unrolls both arm orders, and leaves every other spelling alone", () => {
        expect(unrollEscapeRuns(STRING)).toBe(String.raw`"[^"\\]*(?:\\[\s\S][^"\\]*)*"|'[^'\\]*(?:\\[\s\S][^'\\]*)*'`);
        expect(unrollEscapeRuns(String.raw`"(?:[^"\\]|\\[\s\S])*"`)).toBe(String.raw`"[^"\\]*(?:\\[\s\S][^"\\]*)*"`);
        for (const kept of [String.raw`"(?:\\[\s\S]|[^"\\])*?"`, String.raw`"(?:\\[\s\S]|[^"\\])+"`, String.raw`\(?:\\[\s\S]|[^"\\])*`, String.raw`[^()"']+`]) {
            expect(unrollEscapeRuns(kept)).toBe(kept);
        }
    });

    it("answers the same sticky match at every offset of every short input (exhaustive)", () => {
        const units = ['"', "'", "\\", "a", "\n"];
        const spelt = [STRING, String.raw`x(?:[^"\\]|\\[\s\S])*"?`, String.raw`(?:\\[\s\S]|[^;\\])*;`];
        let compared = 0;
        for (const src of spelt) {
            const a = new RegExp(src, "y"), b = new RegExp(unrollEscapeRuns(src), "y");
            expect(b.source).not.toBe(a.source);
            const inputs: string[] = [""];
            for (let len = 1; len <= 6; len++) for (let n = 0; n < units.length ** len; n++) {
                let s = "";
                for (let k = 0, m = n; k < len; k++, m = Math.floor(m / units.length)) s += units[m % units.length];
                inputs.push(s);
            }
            for (const s of inputs) for (let i = 0; i <= s.length; i++) {
                a.lastIndex = b.lastIndex = i;
                const x = a.test(s) ? a.lastIndex : -1, y = b.test(s) ? b.lastIndex : -1;
                if (x !== y) throw new Error(`/${src}/ at ${i} in ${JSON.stringify(s)}: ${x} vs ${y}`);
                compared++;
            }
        }
        expect(compared).toBeGreaterThan(50_000);
    });

    it("is what the emitter emits, and the audit build re-runs it as spelt with 0 disagreements", () => {
        const [, ast] = BBNFToAST(`str = /${STRING}/ ;\nlist = str , ( "," >> str ) * ;\n`);
        const { body } = emitGrammar(ast!, {});
        expect(body).toContain(JSON.stringify(unrollEscapeRuns(STRING)).slice(1, -1));
        expect(body).not.toContain(JSON.stringify(STRING).slice(1, -1));
        const audit = createAudit();
        const p = compile(ast!, { audit });
        for (const s of [`"a\\"b",'c'`, `"open`, `'x\\`, `"",''`, `"a\\\nb"`]) p.rules.list(s, 0);
        expect(audit.checks).toBeGreaterThan(0);
        expect(audit.violations).toBe(0);
    });
});
