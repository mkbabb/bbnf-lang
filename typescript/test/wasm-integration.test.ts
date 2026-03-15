/**
 * WASM bytecode VM integration tests.
 *
 * Tests the real WASM module end-to-end: compile grammar → parse input.
 * Requires: `cd wasm && wasm-pack build --target nodejs --out-dir pkg-node`
 */
import { describe, it, expect, beforeAll } from "vitest";
import { Grammar, initWasm, type WasmBindings } from "../src/wasm.js";
import { existsSync, readFileSync } from "fs";
import { resolve, dirname } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WASM_PKG_NODE = resolve(__dirname, "../../wasm/pkg-node");
const WASM_JS = resolve(WASM_PKG_NODE, "bbnf_wasm.js");

// Check at module load time whether the WASM build exists.
const wasmAvailable = existsSync(WASM_JS);

const JSON_GRAMMAR = `
null = "null" ;
bool = "true" | "false" ;
number = /-?(0|[1-9]\\d*)(\\.\\d+)?([eE][+-]?\\d+)?/ ;
comma = "," ?w ;
colon = ":" ?w ;
string = /"(?:[^"\\\\]|\\\\(?:["\\\\\\/bfnrt]|u[0-9a-fA-F]{4}))*"/ ;
array = "[" >> (( value << comma ? ) *)?w << "]" ;
pair = string, colon >> value ;
object = "{" >> (( pair << comma ? ) *)?w << "}" ;
value = object | array | string | number | bool | null ;
`;

describe.skipIf(!wasmAvailable)("WASM Integration", () => {
    beforeAll(async () => {
        const mod = await import(WASM_JS);
        initWasm(mod as unknown as WasmBindings);
    });

    it("compiles JSON grammar", () => {
        const g = Grammar.compile(JSON_GRAMMAR);
        expect(g.handle).toBeGreaterThan(0);
        expect(g.isFreed).toBe(false);
        g.free();
    });

    it("parses null", () => {
        const g = Grammar.compile(JSON_GRAMMAR);
        const result = g.parse("null");
        expect(result.success).toBe(true);
        expect(result.offset).toBe(4);
        g.free();
    });

    it("parses boolean", () => {
        const g = Grammar.compile(JSON_GRAMMAR);
        expect(g.parse("true").success).toBe(true);
        expect(g.parse("false").success).toBe(true);
        g.free();
    });

    it("parses number", () => {
        const g = Grammar.compile(JSON_GRAMMAR);
        const result = g.parse("42");
        expect(result.success).toBe(true);
        expect(result.offset).toBe(2);

        const sci = g.parse("3.14e10");
        expect(sci.success).toBe(true);
        expect(sci.offset).toBe(7);
        g.free();
    });

    it("parses string", () => {
        const g = Grammar.compile(JSON_GRAMMAR);
        const result = g.parse('"hello"');
        expect(result.success).toBe(true);
        expect(result.offset).toBe(7);
        g.free();
    });

    it("parses array", () => {
        const g = Grammar.compile(JSON_GRAMMAR);
        const result = g.parse("[1, 2, 3]");
        expect(result.success).toBe(true);
        expect(result.offset).toBe(9);
        g.free();
    });

    it("parses object", () => {
        const g = Grammar.compile(JSON_GRAMMAR);
        const result = g.parse('{"key": "value"}');
        expect(result.success).toBe(true);
        expect(result.offset).toBe(16);
        g.free();
    });

    it("parses nested JSON", () => {
        const g = Grammar.compile(JSON_GRAMMAR);
        const input = '{"a": [1, true, null], "b": {"c": "d"}}';
        const result = g.parse(input);
        expect(result.success).toBe(true);
        expect(result.offset).toBe(input.length);
        g.free();
    });

    it("returns parse tree with tagged values", () => {
        const g = Grammar.compile(JSON_GRAMMAR);
        const result = g.parse("42");
        expect(result.value).toBeDefined();
        // "value" is a transparent alternation (all nonterminal branches),
        // so it passes through — outermost tag is "number".
        expect(result.value!.type).toBe("Tagged");
        if (result.value!.type === "Tagged") {
            expect(result.value!.tag).toBe("number");
        }
        g.free();
    });

    it("handles parse failure", () => {
        const g = Grammar.compile(JSON_GRAMMAR);
        const result = g.parse("<<<invalid>>>");
        expect(result.success).toBe(false);
        g.free();
    });

    it("multiple grammars coexist", () => {
        const g1 = Grammar.compile(JSON_GRAMMAR);
        const g2 = Grammar.compile("digit = /[0-9]/ ;\nvalue = digit ;");
        expect(g1.handle).not.toBe(g2.handle);

        expect(g1.parse("42").success).toBe(true);
        expect(g2.parse("5").success).toBe(true);

        g1.free();
        g2.free();
    });

    it("free releases memory", () => {
        const g = Grammar.compile(JSON_GRAMMAR);
        g.free();
        expect(g.isFreed).toBe(true);
        expect(() => g.parse("null")).toThrow("Grammar has been freed");
    });
});
