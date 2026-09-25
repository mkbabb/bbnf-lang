// SERVED MODEL: claude-opus-5-5
import { describe, it, expect } from "vitest";
import { readFileSync } from "node:fs";

import { BBNFToParser, BBNFToParserFromFile } from "../src/generate.js";
import { encode } from "./conformance/grammar-cases.js";

/** VALUE-SEMANTICS.md, read against runtime compile(): every case answered exactly. */
type Case = { id?: string; law?: string; grammar: string; rule: string; input: string; expect: { ok: boolean; value?: unknown; end?: number } };
const read = (f: string) => JSON.parse(readFileSync(new URL(`./conformance/${f}`, import.meta.url), "utf8"));
/** The dated corrections beside a frozen corpus (grammars.addendum-*.json), keyed grammar·rule·input. */
const corrections = new Map<string, Case["expect"]>(
    (read("grammars.addendum-2026-09-23.json").corrections as Case[]).map((c) => [`${c.grammar}\u0000${c.rule}\u0000${c.input}`, c.expect]),
);
const load = (f: string) => (read(f).cases as Case[]).map((c) => ({ ...c, expect: corrections.get(`${c.grammar}\u0000${c.rule}\u0000${c.input}`) ?? c.expect }));
const answer = (st: { isError: boolean; value: unknown; offset: number }) =>
    st.isError ? { ok: false } : { ok: true, value: encode(st.value), end: st.offset };

describe("value semantics: the law corpus (conformance/semantics.json)", () => {
    for (const c of load("semantics.json")) {
        it(`${c.law}: ${c.id}`, () => {
            const [nt] = BBNFToParser(c.grammar);
            expect(answer(nt[c.rule].parseState(c.input))).toEqual(c.expect);
        });
    }
});

describe("value semantics: bbnf-lang's own grammars (conformance/grammars.json)", () => {
    const readFile = (p: string) => readFileSync(p, "utf8");
    const dir = new URL("./fixtures/grammar/", import.meta.url).pathname;
    for (const c of load("grammars.json")) {
        it(`${c.grammar} ${c.rule} ${JSON.stringify(c.input.slice(0, 32))}`, () => {
            const [nt] = BBNFToParserFromFile(`${dir}${c.grammar}`, readFile);
            expect(answer(nt[c.rule].parseState(c.input))).toEqual(c.expect);
        });
    }
});

/** E-1: the build-time module (written, imported) answers every corpus case exactly as runtime compile(). */
describe("value semantics: the emitted module = runtime compile() (both corpora)", async () => {
    const { mkdtempSync, writeFileSync } = await import("node:fs");
    const { tmpdir } = await import("node:os");
    const { join } = await import("node:path");
    const { BBNFToAST } = await import("../src/parse.js");
    const { compile } = await import("../src/compile.js");
    const { emitGrammar } = await import("../src/emit.js");
    const { loadGrammar } = await import("../src/generate.js");
    const dir = mkdtempSync(join(tmpdir(), "bbnf-conf-"));
    const readFile = (p: string) => readFileSync(p, "utf8");
    const gdir = new URL("./fixtures/grammar/", import.meta.url).pathname;
    const cases = [
        ...load("semantics.json").map((c) => ({ c, ast: BBNFToAST(c.grammar)[1]! })),
        ...load("grammars.json").map((c) => ({ c, ast: loadGrammar(`${gdir}${c.grammar}`, readFile)[0] })),
    ];
    let n = 0;
    for (const { c, ast } of cases) {
        it(`${c.grammar.slice(0, 40)} ${c.rule} ${JSON.stringify(c.input.slice(0, 24))}`, async () => {
            const runtime = compile(ast);
            const file = join(dir, `m${n++}.mjs`);
            writeFileSync(file, emitGrammar(ast).module);
            const m = await import(file);
            const emitted = m.createParser({});
            const a = runtime.rules[c.rule](c.input, 0), b = emitted.rules[c.rule](c.input, 0);
            expect(b).toBe(a);
            if (a >= 0) expect(encode(emitted.value())).toEqual(encode(runtime.value()));
            expect(emitted.entries[c.rule](c.input) === m.FAIL).toBe(runtime.entries[c.rule](c.input) === m.FAIL);
        });
    }
});
