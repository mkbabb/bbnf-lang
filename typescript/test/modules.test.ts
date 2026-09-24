// SERVED MODEL: claude-opus-5-5
import { describe, it, expect } from "vitest";
import { readFileSync, readdirSync } from "node:fs";

import { resolve, dirname, extname } from "../src/posix-path.js";
import { grammarFromModules, loadModuleGraphSync } from "../src/imports.js";

describe("posix-path (no node:path)", () => {
    it("resolves module IDs without a working directory", () => {
        expect(resolve("/css", "tokens.bbnf")).toBe("/css/tokens.bbnf");
        expect(resolve("/css/sub", "../tokens.bbnf")).toBe("/css/tokens.bbnf");
        expect(resolve("/css", "/abs/x.bbnf")).toBe("/abs/x.bbnf");
        expect(resolve("a/./b/../c")).toBe("a/c");
        expect(resolve("../x")).toBe("../x");
        expect(resolve("/..")).toBe("/");
    });
    it("dirname / extname", () => {
        expect(dirname("/css/tokens.bbnf")).toBe("/css");
        expect(dirname("/tokens.bbnf")).toBe("/");
        expect(dirname("tokens.bbnf")).toBe(".");
        expect(extname("/css/tokens.bbnf")).toBe(".bbnf");
        expect(extname("/css/tokens")).toBe("");
        expect(extname("/css/.hidden")).toBe("");
    });
});

describe("the @import loader reads only through the host's reader (F-b-1)", () => {
    it("no source module imports a node builtin", () => {
        for (const f of readdirSync(new URL("../src", import.meta.url), { recursive: true })) {
            if (!String(f).endsWith(".ts")) continue;
            const text = readFileSync(new URL(`../src/${f}`, import.meta.url), "utf8");
            expect(text, String(f)).not.toMatch(/^\s*(import|export)\b[^;]*from\s+"(node:)?(fs|path|os|url)"/m);
        }
    });

    it("merges a module graph from a files map", () => {
        const files: Record<string, string> = {
            "/g/base.bbnf": 'digit = /[0-9]/ ; word = /[a-z]+/ ;',
            "/g/main.bbnf": '@import "base" ;\nvalue = digit | word ;',
        };
        const ast = grammarFromModules(files, "/g/main.bbnf");
        expect([...ast.keys()].sort()).toEqual(["digit", "value", "word"]);
    });

    it("reports a missing module as an import error, never a filesystem read", () => {
        const files = { "/g/main.bbnf": '@import "absent" ;\nvalue = "x" ;' };
        const registry = loadModuleGraphSync("/g/main.bbnf", (id) => {
            const t = (files as Record<string, string>)[id];
            if (t === undefined) throw new Error(`no module ${id}`);
            return t;
        });
        expect(registry.errors.map((e) => [e.type, "path" in e ? e.path : undefined])).toEqual([["FileNotFound", "/g/absent.bbnf"]]);
        expect(() => grammarFromModules(files, "/g/main.bbnf")).toThrow(/absent\.bbnf/);
    });
});
