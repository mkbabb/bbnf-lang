import { describe, it, expect } from "vitest";

import {
    loadModuleGraphSync,
    mergeModuleAST,
    importedRuleNames,
} from "../src/imports.js";

/**
 * Helper: build a synchronous readFileSync from an in-memory file map.
 */
function memoryReader(files: Map<string, string>) {
    return (path: string) => {
        const content = files.get(path);
        if (content === undefined) throw new Error(`File not found: ${path}`);
        return content;
    };
}

describe("Module import graph", () => {
    it("glob import — all rules merged", () => {
        const files = new Map<string, string>();
        files.set(
            "/test/base.bbnf",
            'number = /[0-9]+/ ; text = /[a-z]+/ ;',
        );
        files.set(
            "/test/main.bbnf",
            '@import "base.bbnf" ; value = number | text ;',
        );

        const registry = loadModuleGraphSync("/test/main.bbnf", memoryReader(files));

        expect(registry.errors).toEqual([]);
        const names = importedRuleNames(registry, "/test/main.bbnf");
        expect(names.has("number")).toBe(true);
        expect(names.has("text")).toBe(true);
    });

    it("selective import — only named rules", () => {
        const files = new Map<string, string>();
        files.set(
            "/test/base.bbnf",
            'number = /[0-9]+/ ; text = /[a-z]+/ ; ident = /[a-zA-Z_]+/ ;',
        );
        files.set(
            "/test/main.bbnf",
            '@import { number, text } from "base.bbnf" ; value = number | text ;',
        );

        const registry = loadModuleGraphSync("/test/main.bbnf", memoryReader(files));

        expect(registry.errors).toEqual([]);
        const names = importedRuleNames(registry, "/test/main.bbnf");
        expect(names.has("number")).toBe(true);
        expect(names.has("text")).toBe(true);
        expect(names.has("ident")).toBe(false);
    });

    it("diamond dependency — no errors", () => {
        const files = new Map<string, string>();
        files.set("/test/d.bbnf", 'shared = /[0-9]+/ ;');
        files.set("/test/a.bbnf", '@import "d.bbnf" ; ruleA = shared ;');
        files.set("/test/b.bbnf", '@import "d.bbnf" ; ruleB = shared ;');
        files.set(
            "/test/main.bbnf",
            '@import "a.bbnf" ; @import "b.bbnf" ; top = ruleA | ruleB ;',
        );

        const registry = loadModuleGraphSync("/test/main.bbnf", memoryReader(files));

        // Diamond shapes should not produce circular-import errors.
        const circularErrors = registry.errors.filter(
            (e) => e.type === "CircularImport",
        );
        expect(circularErrors).toEqual([]);
    });

    it("circular import — allowed (both modules accessible)", () => {
        const files = new Map<string, string>();
        files.set(
            "/test/a.bbnf",
            '@import "b.bbnf" ; ruleA = /a/ ;',
        );
        files.set(
            "/test/b.bbnf",
            '@import "a.bbnf" ; ruleB = /b/ ;',
        );

        const registry = loadModuleGraphSync("/test/a.bbnf", memoryReader(files));

        // Cyclic imports are now allowed (Python-style partial-init).
        expect(registry.errors).toEqual([]);
        // Both modules should be loaded.
        expect(registry.modules.has("/test/a.bbnf")).toBe(true);
        expect(registry.modules.has("/test/b.bbnf")).toBe(true);
        // A can see B's rules.
        const namesA = importedRuleNames(registry, "/test/a.bbnf");
        expect(namesA.has("ruleB")).toBe(true);
    });

    it("missing file — FileNotFound error", () => {
        const files = new Map<string, string>();
        files.set(
            "/test/main.bbnf",
            '@import "nonexistent.bbnf" ; value = /x/ ;',
        );

        const registry = loadModuleGraphSync("/test/main.bbnf", memoryReader(files));

        const notFound = registry.errors.filter(
            (e) => e.type === "FileNotFound",
        );
        expect(notFound.length).toBeGreaterThan(0);
    });

    it("missing rule in selective import — MissingRule error", () => {
        const files = new Map<string, string>();
        files.set("/test/base.bbnf", 'number = /[0-9]+/ ;');
        files.set(
            "/test/main.bbnf",
            '@import { number, nonexistent } from "base.bbnf" ; value = number ;',
        );

        const registry = loadModuleGraphSync("/test/main.bbnf", memoryReader(files));

        const missingRule = registry.errors.filter(
            (e) => e.type === "MissingRule",
        );
        expect(missingRule.length).toBeGreaterThan(0);
        expect(missingRule[0].type === "MissingRule" && missingRule[0].ruleName).toBe(
            "nonexistent",
        );
    });

    it("non-transitive scope — A cannot see C's rules through B", () => {
        const files = new Map<string, string>();
        files.set("/test/c.bbnf", 'deep = /z/ ;');
        files.set("/test/b.bbnf", '@import "c.bbnf" ; mid = deep ;');
        files.set("/test/a.bbnf", '@import "b.bbnf" ; top = mid ;');

        const registry = loadModuleGraphSync("/test/a.bbnf", memoryReader(files));

        const namesA = importedRuleNames(registry, "/test/a.bbnf");
        // A can see B's local rule "mid".
        expect(namesA.has("mid")).toBe(true);
        // A cannot see C's rule "deep" — imports are non-transitive.
        expect(namesA.has("deep")).toBe(false);
    });

    it("three-way circular import — all modules accessible", () => {
        const files = new Map<string, string>();
        files.set("/test/a.bbnf", '@import "b.bbnf" ; ruleA = /a/ ;');
        files.set("/test/b.bbnf", '@import "c.bbnf" ; ruleB = /b/ ;');
        files.set("/test/c.bbnf", '@import "a.bbnf" ; ruleC = /c/ ;');

        const registry = loadModuleGraphSync("/test/a.bbnf", memoryReader(files));

        expect(registry.errors).toEqual([]);
        expect(registry.modules.has("/test/a.bbnf")).toBe(true);
        expect(registry.modules.has("/test/b.bbnf")).toBe(true);
        expect(registry.modules.has("/test/c.bbnf")).toBe(true);
    });

    it("self-import — no errors", () => {
        const files = new Map<string, string>();
        files.set("/test/self.bbnf", '@import "self.bbnf" ; ruleS = /s/ ;');

        const registry = loadModuleGraphSync("/test/self.bbnf", memoryReader(files));

        expect(registry.errors).toEqual([]);
        expect(registry.modules.has("/test/self.bbnf")).toBe(true);
    });

    it("selective import with transitive unfurling", () => {
        const files = new Map<string, string>();
        // base has: number, percentageUnit, percentage (depends on number + percentageUnit)
        files.set(
            "/test/base.bbnf",
            'number = /[0-9]+/ ; percentageUnit = "%" ; percentage = number , percentageUnit ;',
        );
        // main selectively imports only "percentage" — should auto-unfurl number + percentageUnit
        files.set(
            "/test/main.bbnf",
            '@import { percentage } from "base.bbnf" ; value = percentage ;',
        );

        const registry = loadModuleGraphSync("/test/main.bbnf", memoryReader(files));

        expect(registry.errors).toEqual([]);
        const names = importedRuleNames(registry, "/test/main.bbnf");
        expect(names.has("percentage")).toBe(true);
        expect(names.has("number")).toBe(true);
        expect(names.has("percentageUnit")).toBe(true);
    });

    it("deep chain unfurling — transitive deps through 3 levels", () => {
        const files = new Map<string, string>();
        files.set(
            "/test/base.bbnf",
            'digit = /[0-9]/ ; number = digit+ ; percentage = number , "%" ;',
        );
        files.set(
            "/test/main.bbnf",
            '@import { percentage } from "base.bbnf" ; value = percentage ;',
        );

        const registry = loadModuleGraphSync("/test/main.bbnf", memoryReader(files));

        expect(registry.errors).toEqual([]);
        const names = importedRuleNames(registry, "/test/main.bbnf");
        expect(names.has("percentage")).toBe(true);
        expect(names.has("number")).toBe(true);
        expect(names.has("digit")).toBe(true);
    });

    it("import after rules — arbitrary position", () => {
        const files = new Map<string, string>();
        files.set("/test/base.bbnf", 'number = /[0-9]+/ ;');
        files.set(
            "/test/main.bbnf",
            'ruleA = /a/ ; @import "base.bbnf" ; ruleB = number ;',
        );

        const registry = loadModuleGraphSync("/test/main.bbnf", memoryReader(files));

        expect(registry.errors).toEqual([]);
        const names = importedRuleNames(registry, "/test/main.bbnf");
        expect(names.has("number")).toBe(true);
        // Local rules should also be present
        const mod = registry.modules.get("/test/main.bbnf")!;
        expect(mod.localRuleNames).toContain("ruleA");
        expect(mod.localRuleNames).toContain("ruleB");
    });

    it("mergeModuleAST — imported and local rules present, local takes precedence", () => {
        const files = new Map<string, string>();
        files.set(
            "/test/base.bbnf",
            'number = /[0-9]+/ ; shared = "base_version" ;',
        );
        files.set(
            "/test/main.bbnf",
            '@import "base.bbnf" ; shared = "local_version" ; value = number | shared ;',
        );

        const registry = loadModuleGraphSync("/test/main.bbnf", memoryReader(files));

        expect(registry.errors).toEqual([]);

        const merged = mergeModuleAST(registry, "/test/main.bbnf");

        // Both imported ("number") and local ("value", "shared") rules present.
        expect(merged.has("number")).toBe(true);
        expect(merged.has("value")).toBe(true);
        expect(merged.has("shared")).toBe(true);

        // Local definition of "shared" should take precedence.
        const sharedRule = merged.get("shared")!;
        const expr = sharedRule.expression;
        expect(expr.type).toBe("literal");
        expect(expr.value).toBe("local_version");
    });
});
