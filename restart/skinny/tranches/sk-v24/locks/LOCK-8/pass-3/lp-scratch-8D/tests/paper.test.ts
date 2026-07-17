import { describe, it, expect } from "vitest";
import { readFileSync, existsSync } from "node:fs";
import { resolve } from "node:path";
import { parseLatex } from "../src/grammar/document";
import { transformDocument, validateOutput, cleanRawLatex } from "../src/transform/html";
import { parseBibToMap } from "../src/bibtex/parser";
import { createCompiledPaperMetadata } from "../src/compiled/metadata";

const PAPER_TEX = resolve(
    import.meta.dirname,
    "../../fourier-analysis/paper/fourier_paper.tex",
);
const PAPER_BIB = resolve(
    import.meta.dirname,
    "../../fourier-analysis/paper/fourier_paper.bib",
);
const PAPER_AUX = resolve(
    import.meta.dirname,
    "../../fourier-analysis/paper/fourier_paper.aux",
);
const PAPER_TOC = resolve(
    import.meta.dirname,
    "../../fourier-analysis/paper/fourier_paper.toc",
);
const PAPER_BBL = resolve(
    import.meta.dirname,
    "../../fourier-analysis/paper/fourier_paper.bbl",
);
const PAPER_LOG = resolve(
    import.meta.dirname,
    "../../fourier-analysis/paper/fourier_paper.log",
);

const hasPaper = existsSync(PAPER_TEX);

describe.skipIf(!hasPaper)("golden test: fourier_paper.tex", () => {
    it("parses without errors", () => {
        const source = readFileSync(PAPER_TEX, "utf-8");
        const ast = parseLatex(source);
        expect(ast.length).toBeGreaterThan(0);
    });

    it("produces section hierarchy", () => {
        const source = readFileSync(PAPER_TEX, "utf-8");
        const bibSource = readFileSync(PAPER_BIB, "utf-8");
        const bibEntries = parseBibToMap(bibSource);

        const ast = parseLatex(source);
        const { sections } = transformDocument(ast, {
            bibEntries,
            callouts: {
                applications: {
                    text: "Upload an image and watch epicycles trace its contour",
                    link: "/visualize",
                },
            },
        });

        // Should have multiple top-level sections (Introduction + chapters)
        expect(sections.length).toBeGreaterThanOrEqual(3);

        // Each section should have a title and id
        for (const section of sections) {
            expect(section.id).toBeTruthy();
            expect(section.title).toBeTruthy();
            expect(section.number).toBeTruthy();
        }
    });

    it("extracts theorems and definitions", () => {
        const source = readFileSync(PAPER_TEX, "utf-8");
        const ast = parseLatex(source);
        const { sections } = transformDocument(ast);

        // Count total theorems across all sections
        let totalTheorems = 0;
        function countTheorems(secs: typeof sections) {
            for (const s of secs) {
                totalTheorems += s.theorems?.length ?? 0;
                if (s.subsections) countTheorems(s.subsections);
            }
        }
        countTheorems(sections);

        // The paper has many theorems/definitions
        expect(totalTheorems).toBeGreaterThanOrEqual(10);
    });

    it("has no unresolved ?? references in output", () => {
        const source = readFileSync(PAPER_TEX, "utf-8");
        const bibSource = readFileSync(PAPER_BIB, "utf-8");
        const bibEntries = parseBibToMap(bibSource);

        const ast = parseLatex(source);
        const { sections } = transformDocument(ast, { bibEntries });

        const json = JSON.stringify(sections);
        expect(json).not.toContain("??");
    });

    it("passes validation with no unprocessed LaTeX patterns", () => {
        const source = readFileSync(PAPER_TEX, "utf-8");
        const bibSource = readFileSync(PAPER_BIB, "utf-8");
        const bibEntries = parseBibToMap(bibSource);

        const ast = parseLatex(source);
        const { sections } = transformDocument(ast, { bibEntries });
        const issues = validateOutput(sections);

        if (issues.length > 0) {
            const summary = issues
                .slice(0, 20)
                .map((i) => `  ${i.path}: ${i.pattern} — "${i.match}" in "…${i.text}…"`)
                .join("\n");
            console.warn(
                `Validation found ${issues.length} issues:\n${summary}`,
            );
        }

        // Zero tolerance for unprocessed LaTeX in output
        expect(issues).toEqual([]);
    });

    it("renders Karhunen-Loève correctly (accents + dashes in braceBalanced)", () => {
        const source = readFileSync(PAPER_TEX, "utf-8");
        const ast = parseLatex(source);
        const { sections } = transformDocument(ast);

        // Search all theorem names and section titles for Karhunen-Loève
        const json = JSON.stringify(sections);

        // Should NOT contain raw LaTeX accent
        expect(json).not.toContain("\\`{e}");
        expect(json).not.toContain("Lo\\`{e}ve");

        // Should NOT contain raw double-dash
        expect(json).not.toContain("Karhunen--Lo");

        // Should contain properly rendered form
        expect(json).toContain("Karhunen");
        expect(json).toContain("Loève");
    });

    it("produces label map with section IDs", () => {
        const source = readFileSync(PAPER_TEX, "utf-8");
        const bibSource = readFileSync(PAPER_BIB, "utf-8");
        const bibEntries = parseBibToMap(bibSource);

        const ast = parseLatex(source);
        const { labelMap } = transformDocument(ast, { bibEntries });

        // Should have labels
        const keys = Object.keys(labelMap);
        expect(keys.length).toBeGreaterThan(10);

        // Each label should have number, type, and sectionId
        for (const key of keys.slice(0, 10)) {
            const info = labelMap[key];
            expect(info.number).toBeTruthy();
            expect(info.type).toBeTruthy();
            expect(info.sectionId).toBeTruthy();
        }
    });

    it("generates clickable ref links", () => {
        const source = readFileSync(PAPER_TEX, "utf-8");
        const bibSource = readFileSync(PAPER_BIB, "utf-8");
        const bibEntries = parseBibToMap(bibSource);

        const ast = parseLatex(source);
        const { sections } = transformDocument(ast, { bibEntries });

        const json = JSON.stringify(sections);
        // Should contain paper-ref links
        expect(json).toContain('paper-ref');
        expect(json).toContain('data-ref');
    });

    it("preserves nested theorem and proof content structure from the compiled paper", () => {
        const source = readFileSync(PAPER_TEX, "utf-8");
        const bibSource = readFileSync(PAPER_BIB, "utf-8");
        const bibEntries = parseBibToMap(bibSource);
        const compiledMetadata = createCompiledPaperMetadata({
            texSource: source,
            auxSource: readFileSync(PAPER_AUX, "utf-8"),
            tocSource: readFileSync(PAPER_TOC, "utf-8"),
            bblSource: readFileSync(PAPER_BBL, "utf-8"),
            logSource: readFileSync(PAPER_LOG, "utf-8"),
        });

        const ast = parseLatex(source);
        const { sections } = transformDocument(ast, { bibEntries, compiledMetadata });

        const chapter5 = sections.find((section) => section.id === "interpreting-the-results");
        const infiniteVectors = chapter5?.content.find(
            (block) =>
                typeof block === "object" &&
                block !== null &&
                "theorem" in block &&
                block.theorem.number === "5.0.1",
        );
        expect(infiniteVectors).toBeDefined();
        if (typeof infiniteVectors === "object" && infiniteVectors && "theorem" in infiniteVectors) {
            const figureCount = infiniteVectors.theorem.content.filter(
                (block) => typeof block === "object" && block !== null && "figure" in block,
            ).length;
            const firstParagraph = infiniteVectors.theorem.content.find(
                (block) => typeof block === "string",
            );
            expect(figureCount).toBe(2);
            expect(firstParagraph).toContain("<br />");
        }

        const svdSection = sections
            .find((section) => section.id === "eigentheory-applied-svd-pca-and-compression")
            ?.subsections?.find((section) => section.id === "the-singular-value-decomposition");
        const proofBlock = svdSection?.content.find(
            (block) => typeof block === "object" && block !== null && "proof" in block,
        );
        expect(proofBlock).toBeDefined();
        if (typeof proofBlock === "object" && proofBlock && "proof" in proofBlock) {
            const paragraphs = proofBlock.proof.content.filter(
                (block) => typeof block === "string",
            );
            expect(paragraphs[0]).toContain("The natural starting point");
            expect(paragraphs[0]).toContain('data-ref="sec:pca"');
            expect(paragraphs.length).toBeGreaterThanOrEqual(4);
        }
    });
});

describe("cleanRawLatex", () => {
    it("handles accents in braced form", () => {
        expect(cleanRawLatex("Lo\\`{e}ve")).toBe("Loève");
        expect(cleanRawLatex("na\\\"ive")).toBe("naïve");
        expect(cleanRawLatex("Poincar\\'{e}")).toBe("Poincaré");
    });

    it("converts dashes", () => {
        expect(cleanRawLatex("Karhunen--Lo\\`{e}ve")).toBe("Karhunen\u2013Loève");
        expect(cleanRawLatex("em---dash")).toBe("em\u2014dash");
    });

    it("converts smart quotes", () => {
        expect(cleanRawLatex("``hello''")).toBe("\u201Chello\u201D");
    });

    it("strips residual commands (formatting handled by parser)", () => {
        // Formatting commands are now parsed by braceContent() into proper AST nodes.
        // cleanRawLatex only handles residual patterns — formatting commands in raw text
        // get their braces stripped and command names passed through.
        // This is a fallback scenario; in normal operation, \textit{...} is never a text node.
        const result = cleanRawLatex("\\textit{hello}");
        // Command name stripped by escaped-specials, braces stripped → "textithello"
        expect(result).toContain("hello");
    });

    it("preserves math segments", () => {
        expect(cleanRawLatex("the $x^2$ function")).toBe("the $x^2$ function");
    });

    it("strips braces after processing", () => {
        expect(cleanRawLatex("{grouped}")).toBe("grouped");
    });

    it("handles tildes as spaces", () => {
        expect(cleanRawLatex("Figure~1")).toBe("Figure 1");
    });

    it("resolves refs with label resolver", () => {
        const resolver = (key: string) =>
            key === "thm:foo" ? "2.3" : undefined;
        const result = cleanRawLatex(
            "Theorem~\\ref{thm:foo}",
            resolver,
        );
        expect(result).toContain("Theorem 2.3");
        expect(result).toContain('data-ref="thm:foo"');
    });

    it("handles \\eqref with label resolver", () => {
        const resolver = (key: string) =>
            key === "eq:bar" ? "1.5" : undefined;
        const result = cleanRawLatex("see \\eqref{eq:bar}", resolver);
        expect(result).toContain("(1.5)");
        expect(result).toContain('data-ref="eq:bar"');
    });

    it("preserves explicit line breaks in prose", () => {
        expect(cleanRawLatex("alpha\\\\beta")).toContain("<br />");
        expect(cleanRawLatex("alpha\\newline beta")).toContain("<br />");
    });
});
