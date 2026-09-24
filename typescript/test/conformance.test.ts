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
