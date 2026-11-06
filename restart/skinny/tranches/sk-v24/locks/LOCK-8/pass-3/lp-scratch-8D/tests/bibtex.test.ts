import { describe, it, expect } from "vitest";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { parseBibString, parseBibToMap } from "../src/bibtex/parser";

const SAMPLE_BIB = readFileSync(
    resolve(import.meta.dirname, "fixtures/sample.bib"),
    "utf-8",
);

describe("parseBibString", () => {
    it("parses all entries", () => {
        const entries = parseBibString(SAMPLE_BIB);
        expect(entries.length).toBe(3);
    });

    it("extracts citation keys", () => {
        const entries = parseBibString(SAMPLE_BIB);
        const keys = entries.map((e) => e.key);
        expect(keys).toContain("fourier1822");
        expect(keys).toContain("rudin1976");
        expect(keys).toContain("cooley1965");
    });

    it("extracts authors correctly", () => {
        const entries = parseBibString(SAMPLE_BIB);
        const fourier = entries.find((e) => e.key === "fourier1822")!;
        expect(fourier.author).toContain("Fourier");
    });

    it("computes short author for single author", () => {
        const entries = parseBibString(SAMPLE_BIB);
        const rudin = entries.find((e) => e.key === "rudin1976")!;
        expect(rudin.shortAuthor).toBe("Rudin");
    });

    it("computes short author with et al. for multiple authors", () => {
        const entries = parseBibString(SAMPLE_BIB);
        const cooley = entries.find((e) => e.key === "cooley1965")!;
        expect(cooley.shortAuthor).toContain("et al.");
    });

    it("extracts year", () => {
        const entries = parseBibString(SAMPLE_BIB);
        const fourier = entries.find((e) => e.key === "fourier1822")!;
        expect(fourier.year).toBe("1822");
    });

    it("cleans accents from fields", () => {
        const entries = parseBibString(SAMPLE_BIB);
        const fourier = entries.find((e) => e.key === "fourier1822")!;
        expect(fourier.title).toContain("Th");
        // The accent \' should be cleaned
        expect(fourier.title).not.toContain("\\");
    });

    it("extracts entry type", () => {
        const entries = parseBibString(SAMPLE_BIB);
        const rudin = entries.find((e) => e.key === "rudin1976")!;
        expect(rudin.type).toBe("book");
    });

    it("extracts additional fields", () => {
        const entries = parseBibString(SAMPLE_BIB);
        const rudin = entries.find((e) => e.key === "rudin1976")!;
        expect(rudin.fields.publisher).toBeDefined();
    });
});

describe("parseBibToMap", () => {
    it("returns a map keyed by citation key", () => {
        const map = parseBibToMap(SAMPLE_BIB);
        expect(map.size).toBe(3);
        expect(map.has("fourier1822")).toBe(true);
        expect(map.get("fourier1822")!.year).toBe("1822");
    });
});

describe("accent cleaning in bib entries", () => {
    it("cleans accented author names", () => {
        const bib = `@book{szego, author = {G\\'{a}bor Szeg\\H{o}}, title = {Test}, year = {1975}}`;
        const entries = parseBibString(bib);
        expect(entries).toHaveLength(1);
        expect(entries[0].author).toBe("Gábor Szego");
        expect(entries[0].shortAuthor).toBe("Szego");
    });

    it("cleans accented author from real bib file", async () => {
        const { existsSync } = await import("node:fs");
        const bibPath = resolve(import.meta.dirname, "../../fourier_analysis/paper/fourier_paper.bib");
        if (!existsSync(bibPath)) return;
        const bib = readFileSync(bibPath, "utf-8");
        const map = parseBibToMap(bib);
        const szego = map.get("szego_orthogonal");
        if (szego) {
            console.log("Szego author:", szego.author);
            console.log("Szego shortAuthor:", szego.shortAuthor);
            expect(szego.author).not.toContain("\\'");
            expect(szego.shortAuthor).not.toContain("\\'");
        }
    });
});
