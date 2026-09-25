// SERVED MODEL: claude-opus-5-5
import { describe, it, expect } from "vitest";
import { readFileSync } from "node:fs";

import { BBNFToAST } from "../src/parse.js";
import { compile, createAudit } from "../src/compile.js";
import { emitGrammar } from "../src/emit.js";
import { grammarFromModules } from "../src/imports.js";
import { bulkTextRuns, textRunLeaves } from "../src/analysis/bulk.js";
import type { AST } from "../src/types.js";

/**
 * The bulk-text classification (value.js X.P.W7 `.l2`, COHESION §0di): a class run that spans a block
 * body, a prelude or the gap between blocks is scanned by the regex engine; a class run inside one list
 * item or one value keeps the code-unit loop. The decision is a property of the grammar alone.
 */
const DIR = new URL("./fixtures/value-js/", import.meta.url);
const MODULES = ["tokens", "math", "color", "value", "stylesheet", "css"];
const files: Record<string, string> = Object.fromEntries(
    MODULES.map((n) => [`/css/${n}.bbnf`, readFileSync(new URL(`${n}.bbnf`, DIR), "utf8")]),
);
const valueJs = (): AST => grammarFromModules(files, "/css/css.bbnf");
const textRuns = (ast: AST) => [...ast].filter(([, r]) => textRunLeaves(r.expression) !== null).map(([n]) => n);

const SHEET = String.raw`
q     = /"[^"]*"/ ;
body  = ( /[^{}"]+/ | q | "{" , body , "}" ) * ;
pre   = ( /[^{;"]+/ | q ) * ;
item  = ( /[^,"]+/ | q ) * ;
items = item , ( "," , item ) * ;
rule  = pre , ( ";" | "{" >> body << "}" ) ;
gap   = /[\s;]+/ * ;
sheet = gap , ( rule , gap ) * ;
word  = /[a-z]+/ ;
`;

describe("bulkTextRuns", () => {
    it("classifies value.js's grammar: preludeRun, blockBody, textBody, ruleGap (and balanced) are bulk text", () => {
        const ast = valueJs();
        const bulk = bulkTextRuns(ast);
        // Block bodies: the middle of a bracketed sequence.
        expect(bulk.has("blockBody")).toBe(true); // "{" >> blockBody << "}", "{" , blockBody , "}"
        expect(bulk.has("textBody")).toBe(true); // textGroup = "(" , textBody , ")"
        expect(bulk.has("balanced")).toBe(true); // "(" , balanced , ")" — the same shape as textBody
        // Prelude and gap: beside an element that derives a block (ruleBlock's blockTail).
        expect(bulk.has("preludeRun")).toBe(true); // ruleBlock = ( preludeRun , ( semiTail | blockTail ) ) - …
        expect(bulk.has("ruleGap")).toBe(true); // ruleList = ruleGap , ( ruleBlock , ruleGap ) * , …
        expect([...bulk].sort()).toEqual(["balanced", "blockBody", "preludeRun", "ruleGap", "textBody"]);
        // Token positions: a run inside one list item (its blocks come only through its own runs).
        const token = textRuns(ast).filter((n) => !bulk.has(n)).sort();
        expect(token).toEqual(["argRun", "colonRun", "commaRun", "semiRun", "spaceRun"]);
    });

    it("is a property of the grammar's structure, not of rule order", () => {
        const ast = valueJs();
        const reversed: AST = new Map([...ast].reverse());
        expect([...bulkTextRuns(reversed)].sort()).toEqual([...bulkTextRuns(ast)].sort());
        const [, small] = BBNFToAST(SHEET);
        expect([...bulkTextRuns(small!)].sort()).toEqual(["body", "gap", "pre"]);
        expect([...bulkTextRuns(new Map([...small!].reverse()))].sort()).toEqual(["body", "gap", "pre"]);
    });

    it("the emitter scans bulk runs and negated runs with one sticky regex; other token runs stay loops", () => {
        const [, ast] = BBNFToAST(SHEET);
        const { body } = emitGrammar(ast!, {});
        // Bulk text (body, pre, gap), and every negated class run (item's, uniformly: ESC-W7l-1 (b)).
        for (const cls of [`[^{}\\"]+`, `[^{;\\"]+`, `[\\\\s;]+`, `[^,\\"]+`]) expect(body).toContain(`new RegExp("${cls}", "y")`);
        // word: a token run over a positive class keeps the code-unit loop (its non-ASCII test is the class alone).
        expect(body).not.toContain(`new RegExp("[a-z]+", "y")`);
        expect(body).toContain(`new RegExp("[a-z]", "y")`);
    });

    it("answers what the grammar spells at every offset of every short sheet (audit, exhaustive)", () => {
        const [, ast] = BBNFToAST(SHEET);
        const audit = createAudit();
        const p = compile(ast!, { audit });
        const units = ["a", "{", "}", ";", '"', " ", "\n", "é"];
        let runs = 0;
        for (let len = 0; len <= 5; len++) for (let n = 0; n < units.length ** len; n++) {
            let s = "";
            for (let k = 0, m = n; k < len; k++, m = Math.floor(m / units.length)) s += units[m % units.length];
            for (let i = 0; i <= s.length; i++) { p.rules.sheet(s, i); p.rules.items(s, i); runs++; }
        }
        expect(runs).toBeGreaterThan(100_000);
        expect(audit.checks).toBeGreaterThan(100_000);
        expect(audit.samples).toEqual([]);
        expect(audit.violations).toBe(0);
    });
});
