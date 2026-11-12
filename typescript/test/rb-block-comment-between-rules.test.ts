/**
 * sk-v27 LW3.ORC2, SK/grammar/DIVERGENCE.tsv D-24 (R-b, ruling ORC-1a): the
 * TS front end reads a block comment between rules (before the first, between
 * two, after the last).
 *
 * `crates/gorgeous/tests/corpus/g3/bbnf/comments-factor.bbnf` (listed in
 * grammar/MANIFEST since LW3.ORC1-STR re-read it) opens with
 * `/* a block comment before the first rule *\/`. docs/bbnf/grammar-syntax.md:
 * "A BBNF file consists of **import directives** and **production rules** in
 * any order, interleaved with comments." The Rust bootstrap front end reads
 * the file; this front end (and the 0.3.0 package G1 pins) refuses it. Born
 * RED: it reads a block comment inside a rule and a line comment anywhere, and
 * refuses a block comment at the top level (`it.fails`: the assertion fails today, so the test passes; when the
 * front end is cured it fails, and the row and this marker go). Cure: the TS
 * backend over the IR, LW4.COMP-4.
 */
import { describe, expect, it } from "vitest";
import { readFileSync } from "node:fs";
import path from "node:path";

import { BBNFToASTWithImports } from "../src/parse";

const FILE = path.resolve(__dirname, "../../crates/gorgeous/tests/corpus/g3/bbnf/comments-factor.bbnf");

function accepts(src: string): boolean {
    try {
        const r = BBNFToASTWithImports(src);
        return r.length === 2 && r[1] !== undefined;
    } catch {
        return false;
    }
}

describe("D-24: a block comment between rules (born RED)", () => {
    it.fails("reads comments-factor.bbnf", () => {
        expect(accepts(readFileSync(FILE, "utf8"))).toBe(true);
    });

    for (const src of ["/* top */\na = \"x\" ;", "a = \"x\" ;\n/* between */\nb = \"y\" ;", "a = \"x\" ; /* after */"]) {
        it.fails(`reads ${JSON.stringify(src)}`, () => {
            expect(accepts(src)).toBe(true);
        });
    }

    it("reads the comment-bearing neighbours", () => {
        expect(accepts("// top\na = \"x\" ;")).toBe(true);
        expect(accepts("a = /* c */ \"x\" ;")).toBe(true);
        expect(accepts("a = \"x\" ;\nb = /* c */ \"y\" ;")).toBe(true);
    });
});
