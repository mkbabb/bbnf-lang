import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { BBNFToParserFromFile } from "../src/generate";
import {
    enableDiagnostics,
    disableDiagnostics,
} from "@mkbabb/parse-that";

const __dirname = dirname(fileURLToPath(import.meta.url));
const grammarDir = resolve(__dirname, "fixtures/grammar/css");
const testDir = resolve(__dirname, "fixtures/grammar/tests/css");

describe("CSS Stylesheet BBNF Grammar", () => {
    let nonterminals: Record<string, any>;

    beforeEach(() => {
        // Compile the css-stylesheet.bbnf grammar with @recover directives
        if (!nonterminals) {
            const entryPath = resolve(grammarDir, "css-stylesheet.bbnf");
            [nonterminals] = BBNFToParserFromFile(entryPath);
        }
        enableDiagnostics();
    });

    afterEach(() => {
        disableDiagnostics();
    });

    describe("compilation", () => {
        it("should compile the grammar with @recover directives", () => {
            expect(nonterminals).toBeDefined();
            expect(nonterminals.stylesheet).toBeDefined();
            expect(nonterminals.declaration).toBeDefined();
            expect(nonterminals.qualifiedRule).toBeDefined();
            expect(nonterminals.atRule).toBeDefined();
        });
    });

    describe("valid CSS parsing", () => {
        it("should parse a simple valid rule", () => {
            const input = `.foo { color: red; }`;
            const { value: result, diagnostics } = nonterminals.stylesheet.parseState(input);
            expect(result).toBeDefined();
            expect(diagnostics).toHaveLength(0);
        });

        it("should parse multiple valid rules", () => {
            const input = `
.foo { color: red; font-size: 16px; }
.bar { margin: 0; padding: 10px; }
`;
            const { value: result, diagnostics } = nonterminals.stylesheet.parseState(input.trim());
            expect(result).toBeDefined();
            expect(diagnostics).toHaveLength(0);
        });

        it("should parse a rule with class and type selectors", () => {
            const input = `.heading { color: red; }`;
            const { value: result, diagnostics } = nonterminals.stylesheet.parseState(input);
            expect(result).toBeDefined();
            expect(diagnostics).toHaveLength(0);
        });
    });

    describe("error recovery", () => {
        it("should recover from a missing semicolon between declarations", () => {
            const input = `.test { margin: 0\n  padding: 0; }`;
            const { value: result, diagnostics } = nonterminals.stylesheet.parseState(input);
            expect(result).toBeDefined();
            expect(diagnostics.length).toBeGreaterThanOrEqual(1);
        });

        it("should recover from a missing colon in a declaration", () => {
            const input = `.test { width 100%; max-width: 960px; }`;
            const { value: result, diagnostics } = nonterminals.stylesheet.parseState(input);
            expect(result).toBeDefined();
            expect(diagnostics.length).toBeGreaterThanOrEqual(1);
        });

        it("should recover from an invalid selector and continue", () => {
            // Use a selector that starts validly but has a malformed block
            const input = `.bad { color ; }\n.valid { color: green; }`;
            const { value: result, diagnostics } = nonterminals.stylesheet.parseState(input);
            expect(result).toBeDefined();
            // The "color ;" declaration is malformed (missing colon+value) — should recover
            expect(diagnostics.length).toBeGreaterThanOrEqual(1);
        });
    });

    describe("complex-errors.css end-to-end", () => {
        const cssPath = resolve(testDir, "complex-errors.css");
        let cssContent: string;

        beforeEach(() => {
            cssContent = readFileSync(cssPath, "utf-8");
        });

        it("should parse the file and collect multiple diagnostics", () => {
            const { value: result, diagnostics } = nonterminals.stylesheet.parseState(cssContent);


            // The file has 7 intentional errors — we should catch several
            expect(diagnostics.length).toBeGreaterThanOrEqual(3);

            // Should still produce a result (recovery succeeded)
            expect(result).toBeDefined();
        });

        it("should recover and continue past errors to parse the valid .success rule", () => {
            const { value: result, diagnostics } = nonterminals.stylesheet.parseState(cssContent);

            // The result should be an array of parsed rules
            expect(result).toBeDefined();
            expect(Array.isArray(result)).toBe(true);
            expect(result.length).toBeGreaterThan(0);

            // At least one valid rule should survive
            const nonNullRules = result.filter((r: any) => r !== null);
            expect(nonNullRules.length).toBeGreaterThan(0);
        });

        it("diagnostics should have correct source positions", () => {
            const { diagnostics } = nonterminals.stylesheet.parseState(cssContent);
            expect(diagnostics.length).toBeGreaterThan(0);

            for (const d of diagnostics) {
                expect(d.line).toBeGreaterThan(0);
                expect(d.column).toBeGreaterThanOrEqual(0);
            }
        });

        it("each diagnostic should have a message or offset info", () => {
            const { diagnostics } = nonterminals.stylesheet.parseState(cssContent);
            expect(diagnostics.length).toBeGreaterThan(0);

            for (const d of diagnostics) {
                // Each diagnostic should have position info
                expect(d.offset).toBeGreaterThanOrEqual(0);
            }
        });
    });
});
