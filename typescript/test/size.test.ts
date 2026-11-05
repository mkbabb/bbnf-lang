// SERVED MODEL: claude-opus-5-5
import { describe, it, expect } from "vitest";
import { readFileSync } from "node:fs";
import { gzipSync } from "node:zlib";

import { generate } from "../src/gen.js";

/**
 * bbnf-lang#2: the emitted module's size is pinned. The fixture is value.js's grammar with every
 * entry and action kind it uses, so a change that re-spells per-rule text (tables, action names, a
 * text rule's recognize twin) shows here. A pin moves DOWN with the emitter that shrinks it; a growth
 * fails until its cost is stated and the pin is raised in the same commit.
 */
const read = (id: string) => readFileSync(id, "utf8");
const gz = (s: string) => gzipSync(Buffer.from(s, "utf8")).length;

describe("emitted size (bbnf-lang#2)", () => {
    const g = generate({
        grammar: new URL("./fixtures/value-js/css.bbnf", import.meta.url).pathname, read,
        actions: { colorTop: { kind: "map" }, valueTop: { kind: "span" }, ident: { kind: "text" } },
        entries: ["colorTop", "valueTop", "ruleList"],
    });

    it("packs first-unit tables and interns action names (no 128-entry array literal, no A[\"name\"].fn)", () => {
        expect(g.js).not.toMatch(/new Uint8Array\(\[(?:[01],){127}[01]\]\)/);
        expect(g.js).not.toMatch(/= A\["[^"]+"\]\.fn;/);
    });

    it("stays self-contained: no import", () => {
        expect(g.js).not.toMatch(/^\s*import\b|\bfrom\s+["']|\brequire\(/m);
    });

    it("the value.js fixture module is within its pinned gzip size", () => {
        const bytes = { raw: Buffer.byteLength(g.js, "utf8"), gzip: gz(g.js) };
        console.log(`size: ${JSON.stringify(bytes)}`);
        expect(bytes.gzip).toBeLessThanOrEqual(PIN.gzip);
        expect(bytes.raw).toBeLessThanOrEqual(PIN.raw);
    });
});

// Measured 2026-11-04 (node zlib default level): base origin/master 2fe18f9c → 132,645 raw / 23,985 gz;
// this emitter → 112,516 / 23,638. Pinned at the measured raw and the gz rounded up to 50 B (zlib builds differ).
const PIN = { raw: 112516, gzip: 23650 };
