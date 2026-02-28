import { test, expect, describe, it } from "vitest";
import fs from "fs";
import path from "path";
import { formatBBNF } from "../src/index.js";

const grammarDir = path.resolve(__dirname, "../../grammar/lang");

describe("Prettier BBNF", () => {
    it("should format json.bbnf", async () => {
        const input = fs.readFileSync(path.join(grammarDir, "json.bbnf"), "utf8");
        const formatted = await formatBBNF(input);
        expect(formatted).toBeTruthy();
        expect(formatted).toContain("value");
        expect(formatted).toContain("object");
    });

    it("should format csv.bbnf", async () => {
        const input = fs.readFileSync(path.join(grammarDir, "csv.bbnf"), "utf8");
        const formatted = await formatBBNF(input);
        expect(formatted).toBeTruthy();
    });

    it("should format math.bbnf", async () => {
        const input = fs.readFileSync(path.join(grammarDir, "math.bbnf"), "utf8");
        const formatted = await formatBBNF(input);
        expect(formatted).toBeTruthy();
    });

    it("should format bbnf.bbnf (self-describing)", async () => {
        const input = fs.readFileSync(path.join(grammarDir, "bbnf.bbnf"), "utf8");
        const formatted = await formatBBNF(input);
        expect(formatted).toBeTruthy();
    });

    it("should be idempotent", async () => {
        const input = fs.readFileSync(path.join(grammarDir, "json.bbnf"), "utf8");
        const first = await formatBBNF(input);
        const second = await formatBBNF(first);
        expect(first).toBe(second);
    });

    it("should throw on invalid grammar", async () => {
        await expect(formatBBNF("this is not valid")).rejects.toThrow();
    });
});
